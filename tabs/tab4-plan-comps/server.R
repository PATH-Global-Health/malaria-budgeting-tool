tab4Server <- function(input, output, session,
                       adm2_outline, adm1_outline,
                       shared) {
  ns <- session$ns

  # Adding instructions pop up
  observeEvent(input$show_instructions, {
    showModal(modalDialog(
      title = "Instructions détaillées pour l'utilisation du point de contrôle",
      easyClose = TRUE,
      size = "l",
      footer = modalButton("Fermer"),
      tagList(
        p("Cette section permet aux utilisateurs de comparer les budgets côte à côte. Les comparaisons sont effectuées à l'échelle nationale uniquement et aident les utilisateurs à comprendre les différences dans la combinaison d'interventions, les exigences budgétaires et l'évolution des coûts entre les différents plans."),
        p("Sélectionnez le budget principal, c'est à cela que seront comparés les budgets restants."),
        p("Sélectionnez un ou plusieurs budgets de comparaison."),
        p("Sélectionnez l'année qui vous intéresse et la devise.")
      )
    ))
  })

  # Track local data availability
  local_data_available <- reactiveVal(FALSE)

  observe({
    if (!is.null(shared$budget_results) &&
      is.data.frame(shared$budget_results) &&
      nrow(shared$budget_results) > 0) {
      local_data_available(TRUE)
    } else {
      local_data_available(FALSE)
    }
  })

  data_ready <- reactive({
    local_data_available()
  })

  # Available plan IDs
  available_plans <- reactive({
    req(shared$budget_results)
    shared$budget_results %>%
      select(plan_id) %>%
      distinct()
  })

  # Available years
  available_years <- reactive({
    req(shared$budget_results)
    sort(unique(shared$budget_results$year))
  })

  # Plan selection UI
  output$plan_bl_select_ui <- renderUI({
    plans <- available_plans()
    selectInput(ns("plan_bl_select"), "Sélectionnez le budget principal:",
      choices = c("", plans$plan_id),
      selected = ""
    )
  })

  output$remaining_plan_select <- renderUI({
    req(input$plan_bl_select)
    plans <- available_plans()
    remaining <- setdiff(plans$plan_id, input$plan_bl_select)
    checkboxGroupInput(
      ns("remaining_plan_select"),
      "Sélectionner des budgets de comparaison:",
      choices = remaining
    )
  })

  # Year selection UI
  output$year_select_ui <- renderUI({
    years <- available_years()
    choices <- if (length(years) > 1) c("", as.character(years), "Toutes les années") else c("", as.character(years))
    selectInput(
      ns("year_select"),
      "Sélectionnez les années d'intérêt:",
      choices = choices,
      selected = ""
    )
  })

  # Filter baseline data
  baseline_data <- reactive({
    req(shared$budget_results, input$plan_bl_select, input$year_select != "")
    data <- shared$budget_results %>%
      filter(plan_id == input$plan_bl_select)
    if (input$year_select != "" && input$year_select != "Toutes les années") {
      data <- data %>% filter(year == as.numeric(input$year_select))
    }
    return(data)
  })

  # Filter comparison data
  comparison_data <- reactive({
    req(
      shared$budget_results, input$remaining_plan_select,
      input$year_select != ""
    )

    data <-
      shared$budget_results %>%
      filter(plan_id %in% input$remaining_plan_select)

    if (input$year_select != "" && input$year_select != "Toutes les années") {
      data <- data %>% filter(year == as.numeric(input$year_select))
    }
    return(data)
  })

  output$page_description <- renderUI({
    # Generate the card content based on the state of the inputs
    content <- if (is.null(input$plan_bl_select) || input$plan_bl_select == "") {
      HTML("<p>Sélectionnez budget principal pour commencer les comparaisons.</p>")
    } else if (is.null(input$remaining_plan_select) || length(input$remaining_plan_select) == 0) {
      HTML(paste0(
        "<p>Plan principal défini comme: <b>",
        input$plan_bl_select,
        "</b>. Sélectionnez un ou plusieurs plans à comparer.</p>"
      ))
    } else {
      HTML(paste0(
        "<div style='font-size:14px;'>",
        "<b>Budget principal défini comme: </b> ", input$plan_bl_select, "<br><br>",
        "<b>Budget(s) de comparaison défini(s) comme: </b><br>",
        paste(input$remaining_plan_select, collapse = "<br>"),
        "</div>"
      ))
    }

    card(
      card_header("Informations de sélection"),
      card_body(content)
    )
  })

  #-BUDGET COMPARISON PLOTS------------------------------------------------------------------

  #-Reactive function for Cost Comparison Data----------------
  prepare_cost_data <- reactive({
    req(
      input$plan_bl_select, input$remaining_plan_select,
      input$year_select, input$currency_select
    )

    baseline <- baseline_data() %>%
      mutate(
        plan_label = plan_id
      )

    comparisons <- comparison_data() %>%
      mutate(
        cost_label = plan_id
      )

    bind_rows(
      baseline %>% rename(plan_label = plan_label),
      comparisons %>% rename(plan_label = cost_label)
    ) %>%
      filter(currency == input$currency_select) %>%
      select(plan_label, cost_element) %>%
      group_by(plan_label) %>%
      summarise(total_budget = sum(cost_element, na.rm = TRUE), .groups = "drop")
  })

  #-Prepare difference data
  prepare_diff_data <- reactive({
    req(
      input$plan_bl_select, input$remaining_plan_select,
      input$year_select, input$currency_select
    )

    currency_symbol <- if (input$currency_select == "USD") "$" else "FC"

    baseline_label <- input$plan_bl_select

    base <- baseline_data() %>%
      filter(currency == input$currency_select)

    comp <- comparison_data() %>%
      filter(currency == input$currency_select) %>%
      mutate(plan_label = plan_id)

    # Summarise total budgets
    base_sum <- base %>%
      summarise(total_budget = sum(cost_element, na.rm = TRUE)) %>%
      pull(total_budget)

    comp_sum <- comp %>%
      group_by(plan_label) %>%
      summarise(total_budget = sum(cost_element, na.rm = TRUE), .groups = "drop")


    req(length(base_sum) > 0)

    comp_sum %>%
      mutate(
        difference_millions = round((total_budget - base_sum) / 1e6),
        percent_change = round((difference_millions * 1e6 / base_sum) * 100),
        label = paste(plan_label, "vs", baseline_label),
        hover_text = paste0(
          "Différence: ", ifelse(difference_millions >= 0, "+", ""), currency_symbol,
          format(difference_millions, big.mark = ","), "M<br>",
          "Changement par rapport à la ligne de base: ", sprintf("%.0f%%", percent_change)
        )
      )
  })


  # UI components for cost data-------------------------------
  output$budget_comps <- renderUI({
    # plot card
    card(
      card_header("Comparaisons budgétaires"),
      full_screen = TRUE,
      min_height = 450,
      # style = "resize: vertical; overflow: auto; min-height: 300px; max-height: 800px;",
      card_body( # use card_body_fill instead of card_body
        layout_column_wrap(
          width = 1 / 2,
          (plotlyOutput(ns("budget_comp_chart"), height = "100%")),
          (plotlyOutput(ns("budget_diff_chart"), height = "100%"))
        )
      ),
      card_footer(paste(input$year_select))
    )
  })


  #-Plot one bar chart of total budget--------------------------
  output$budget_comp_chart <- renderPlotly({
    validate(
      need(input$plan_bl_select != "", "Sélectionnez un plan principal."),
      need(input$remaining_plan_select != "", "Sélectionnez des plans de comparaison."),
      need(input$year_select != "", "Sélectionnez une année."),
      need(input$currency_select != "", "Sélectionnez une devise.")
    )

    budget_barchart(
      prepare_cost_data(),
      currency_select = input$currency_select
    )
  })

  #-plot two cost difference plot--------------------------------
  output$budget_diff_chart <- renderPlotly({
    req(input$plan_bl_select != "",
        input$remaining_plan_select != "",
        input$year_select != "",
        input$currency_select != ""
        )


    budget_diff_chart(
      prepare_diff_data(),
      currency_select = input$currency_select
    )
  })

  # ------------------------------------------------------------------------------
  # Render the baseline budget table UI
  # ------------------------------------------------------------------------------

  output$budget_tables <- renderUI({
    card(
      card_header("Tableau récapitulatif du budget de base"),
      card_body(
        (DT::dataTableOutput(ns("budget_table"))) # Output table with loading spinner
      )
    )
  })

  # ------------------------------------------------------------------------------
  # Render the baseline budget data table (as a DataTable)
  # ------------------------------------------------------------------------------

  output$budget_table <- DT::renderDataTable({
    validate(
      need(input$plan_bl_select != "", "Sélectionnez un plan principal."),
      need(input$year_select != "", "Sélectionnez une année."),
      need(input$currency_select != "", "Sélectionnez une devise.")
    )

    create_budget_table(
      process_budget_data( # Process baseline plan data at national level
        spatial_scale = "National",
        adm1_select = NULL,
        adm2_select = NULL,
        currency_select = input$currency_select,
        data = baseline_data()
      ) |> mutate(total_cost = round(total_cost / 1e6, 0)), # Convert cost to millions
      currency_select = input$currency_select
    )
  })

  # ------------------------------------------------------------------------------
  # Render UI for comparison budget tables (one for each selected plan)
  # ------------------------------------------------------------------------------

  output$budget_tables_comp <- renderUI({
    req(input$plan_bl_select, input$remaining_plan_select, input$year_select, input$currency_select)

    # Create a card for each comparison plan
    lapply(input$remaining_plan_select, function(plan_id) {
      safe_id <- safe_id(plan_id) # Sanitize plan_id for use in outputId

      card(
        card_header(paste("Plan de comparaison:", plan_id)),
        card_body(
          withSpinner(DT::dataTableOutput(ns(paste0("budget_table_comp_", safe_id)))) # Each plan has its own table output
        )
      )
    }) |> tagList() # Combine all cards into a UI list
  })

  # ------------------------------------------------------------------------------
  # Main observer: Process all data and render comparison tables and cost plot
  # ------------------------------------------------------------------------------

  observe({
    req(input$plan_bl_select, input$year_select, input$currency_select) # Ensure all necessary inputs are provided

    # ---------------------------
    # Process baseline plan data
    # ---------------------------
    baseline_processed <- process_budget_data(
      spatial_scale = "National",
      adm1_select = NULL,
      adm2_select = NULL,
      currency_select = input$currency_select,
      data = baseline_data()
    ) |> mutate(total_cost = round(total_cost / 1e6, 0))

    # ---------------------------
    # Process all comparison plan data
    # ---------------------------
    comp_data <- comparison_data()
    all_comp_processed <- list() # Store all processed data

    purrr::walk(unique(comp_data$plan_id), function(plan_id) {
      safe_id <- safe_id(plan_id) # Sanitize for use in output ID
      plan_data <- comp_data %>% filter(plan_id == !!plan_id) # Filter data for this plan

      processed <- process_budget_data(
        spatial_scale = "National",
        adm1_select = NULL,
        adm2_select = NULL,
        currency_select = input$currency_select,
        data = plan_data
      ) |> mutate(total_cost = round(total_cost / 1e6, 0))

      all_comp_processed[[plan_id]] <<- processed # Store result

      # Dynamically render a table for this comparison plan
      output[[paste0("budget_table_comp_", safe_id)]] <- DT::renderDataTable({
        create_budget_table(
          processed,
          currency_select = input$currency_select,
          baseline_data = baseline_processed # Optionally compare against baseline
        )
      })
    })

    # ---------------------------
    # Combine all comparison data and render the cost comparison plot
    # ---------------------------
    combined_comp <- bind_rows(all_comp_processed, .id = "plan_id") # Add plan label to each row

    output$final_cost_plot <- renderPlotly({
      validate(
        need(input$plan_bl_select != "", "Sélectionnez un plan principal."),
        need(input$year_select != "", "Sélectionnez une année."),
        need(input$currency_select != "", "Sélectionnez une devise.")
      )

      generate_final_cost_plot(
        baseline_processed = baseline_processed,
        comparison_processed = combined_comp,
        currency_select = input$currency_select
      )
    })
  })

  # ------------------------------------------------------------------------------
  # Render UI for the final cost comparison plot
  # ------------------------------------------------------------------------------

  output$budget_item_plots <- renderUI({
    card(
      full_screen = TRUE,
      card_header("Comparaisons des coûts des articles"),
      card_body(
        class = "p-0",
        (plotlyOutput(ns("final_cost_plot")))
      ),
      card_footer(input$year_select)
    )
  })
}
