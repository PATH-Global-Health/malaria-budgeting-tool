tab4Server <- function(input, output, session,
                       lga_outline, state_outline, country_outline,
                       shared) {
  ns <- session$ns

  # Track data availability for this tab
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

  # Extract available plans and years from shared data
  available_plans <- reactive({
    req(shared$budget_results)
    shared$budget_results %>%
      select(source_scenario, source_cost) %>%
      distinct() %>%
      mutate(plan_label = paste0(source_scenario, " with ", source_cost))
  })

  available_years <- reactive({
    req(shared$budget_results)
    sort(unique(shared$budget_results$year))
  })

  # Parse plan inputs
  parse_plan <- function(x) strsplit(x, "\\|\\|\\|")[[1]]

  baseline_shortname <- reactive({
    req(input$plan_bl_select)
    parse_plan(input$plan_bl_select)[1]
  })

  baseline_costname <- reactive({
    req(input$plan_bl_select)
    parse_plan(input$plan_bl_select)[2]
  })

  comparison_details <- reactive({
    req(input$remaining_plan_select)
    lapply(input$remaining_plan_select, parse_plan)
  })

  comparison_labels <- reactive({
    req(comparison_details())
    purrr::map_chr(comparison_details(), ~ paste(.x[1], "with", .x[2]))
  })

  # UI for plan selection
  output$plan_bl_select_ui <- renderUI({
    plans <- available_plans()
    choices <- c("", setNames(
      paste0(plans$source_scenario, "|||", plans$source_cost),
      plans$plan_label
    ))
    selectInput(ns("plan_bl_select"), "Select the Baseline Plan:", choices = choices, selected = "")
  })

  output$remaining_plan_select <- renderUI({
    req(input$plan_bl_select)
    plans <- available_plans()
    all_choices <- paste0(plans$source_scenario, "|||", plans$source_cost)
    bl <- input$plan_bl_select
    remaining <- setdiff(all_choices, bl)
    display_labels <- plans$plan_label[match(remaining, all_choices)]
    checkboxGroupInput(
      ns("remaining_plan_select"),
      "Select plans to compare:",
      choices = setNames(remaining, display_labels)
    )
  })

  # UI for year selection
  output$year_select_ui <- renderUI({
    years <- available_years()
    if (length(years) > 1) {
      choices <- c("", as.character(years), "All Years")
    } else {
      choices <- c("", as.character(years))
    }
    selectInput(
      ns("year_select"),
      "Select Years of Interest:",
      choices = choices,
      selected = ""
    )
  })


  #-Data--------------------------------------------------------------------------------------
  baseline_data <- reactive({
    req(shared$budget_results)
    req(input$year_select)

    bl <-
      shared$budget_results %>%
      filter(
        scenario_name == baseline_shortname(),
        cost_name == baseline_costname()
      )

    # Filter by year if a specific year is selected
    if (input$year_select != "" && input$year_select != "All Years") {
      bl <- bl %>%
        filter(year == as.numeric(input$year_select))
    }

    return(bl)
  })

  comparison_data <- reactive({
    req(shared$budget_results)
    req(input$year_select)
    req(input$remaining_plan_select)
    req(comparison_details())

    # Create an empty data frame to store results
    result_df <- NULL

    # Process each selected comparison plan
    for (i in seq_along(comparison_details())) {
      plan_details <- comparison_details()[[i]]
      plan_label <- comparison_labels()[i]

      # Filter data for this specific plan
      plan_data <- shared$budget_results %>%
        filter(
          scenario_name == plan_details[1],  # Scenario name
          cost_name == plan_details[2]       # Cost name
        ) %>%
        # Add a column to identify which plan this data belongs to
        mutate(comparison_plan = plan_label)

      # Filter by year if a specific year is selected
      if (input$year_select != "" && input$year_select != "All Years") {
        plan_data <- plan_data %>%
          filter(year == as.numeric(input$year_select))
      }

      # Add to the result data frame
      if (is.null(result_df)) {
        result_df <- plan_data
      } else {
        result_df <- bind_rows(result_df, plan_data)
      }
    }

    return(result_df)
  })

  #-Adding plan description-----------------------------------------------------------------
  output$page_description <- renderUI({
    # 1) If user hasn't selected a baseline plan (or it's empty)
    if (is.null(input$plan_bl_select) || input$plan_bl_select == "") {
      return(HTML("<h4>Select a Baseline Plan to begin comparisons.</h4>"))
    }

    # 2) If baseline plan is selected but no remaining plan(s) selected
    if (is.null(input$remaining_plan_select) || length(input$remaining_plan_select) == 0) {
      return(HTML(paste0(
        "<h4>Baseline plan set as: <b>",
        input$plan_bl_select,
        "</b>. Select one or more plans for comparison.</h4>"
      )))
    }

    # 3) If both baseline plan and at least one remaining plan are selected
    HTML(
      paste0(
        # Wrap in a <div> with a smaller font size
        "<div style='font-size:14px;'>",

        "<b>Baseline plan set as:</b> ",
        input$plan_bl_select,
        "<br><br>",

        "<b>Comparison plan(s) set as:</b><br>",
        # Collapse each comparison plan with <br> so each is on its own line
        paste(input$remaining_plan_select, collapse = "<br>"),

        "<br><br>",
        "Comparison process initiated. See below for results.",
        "</div>"
      )
    )
  })


  #-Map boxes leaflet-------------------------------------------------------------------------
  output$baseline_map <- renderLeaflet({
    req(baseline_data())

    create_intervention_leaflet(
      lga_outline = lga_outline,
      state_outline = state_outline,
      country_outline = country_outline,
      intervention_mix_maps = baseline_data(),
      spatial_scale = "National",
      state_select = NULL,
      lga_select = NULL,
      center_lng = 9,
      center_lat = 4,
      zoom = 5.2
    )
  })

  #-Show Maps UI when Plans are selected----------------------------------------------------
  output$maps_ui <- renderUI({
    req(input$plan_bl_select != "")
    layout_column_wrap(
      width = 1/2,  # Two columns (50% each)

      # Baseline map card
      card(
        card_header(
          "Baseline Plan",
          tooltip(
            shiny::icon("info-circle"),
            "Map displays all interventions targeted aggregated over each year of the Plan"
          )
        ),
        full_screen = FALSE,
        style = "resize: vertical; overflow: auto; min-height: 300px; max-height: 800px;",
        card_body(
          class = "p-0",
          leafletOutput(session$ns("baseline_map"), height = "100%")
        )
      ),

      # Comparison maps card with dynamic tabs
      card(
        card_header(
          "Comparison Plans",
          tooltip(
            shiny::icon("info-circle"),
            "Map displays all interventions targeted aggregated over each year of the Plan"
          )
        ),
        full_screen = FALSE,
        style = "resize: vertical; overflow: auto; min-height: 300px; max-height: 800px;",
        card_body(
          class = "p-0",
          uiOutput(session$ns("comparison_tabs"))
        )
      )
    )
  })


  #-Dynamic UI: Create a tabset panel with a tab for each comparison plan----------------------
  #-Dynamic UI: Create a tabset panel with a tab for each comparison scenario_name-------------
  output$comparison_tabs <- renderUI({
    req(comparison_details())

    # Extract unique scenario_names only (drop cost_name distinctions)
    scenario_names <- unique(purrr::map_chr(comparison_details(), ~ .x[1]))

    # Build tab panels for each scenario
    tabs <- unname(lapply(scenario_names, function(plan) {
      safe_plan <- gsub(" ", "_", plan)  # sanitize for output ID
      tabPanel(
        title = plan,
        leafletOutput(session$ns(paste0("comparison_map_", safe_plan)))
      )
    }))

    # Return as tabsetPanel
    do.call(tabsetPanel, c(list(id = session$ns("comparison_tabset")), tabs))
  })

  #-Dynamic rendering of comparison maps (updated for new unified format)-----------------------
  observe({
    req(comparison_data())

    scenario_names <- unique(comparison_data()$scenario_name)

    purrr::walk(scenario_names, function(plan) {
      safe_id <- gsub(" ", "_", plan)

      plan_data <- comparison_data() %>%
        filter(scenario_name == plan)

      output[[paste0("comparison_map_", safe_id)]] <- renderLeaflet({
        create_intervention_leaflet(
          lga_outline = lga_outline,
          state_outline = state_outline,
          country_outline = country_outline,
          intervention_mix_maps = plan_data,
          spatial_scale = "National",
          state_select = NULL,
          lga_select = NULL,
          center_lng = 9,
          center_lat = 4,
          zoom = 5.2
        )
      })
    })
  })


  #-BUDGET COMPARISON PLOTS------------------------------------------------------------------

 #-Reactive function for Cost Comparison Data----------------
  prepare_cost_data <- reactive({
    req(input$plan_bl_select, input$remaining_plan_select,
        input$year_select, input$currency_select)

    baseline <- baseline_data() %>%
      mutate(
        plan_label = paste(baseline_shortname(), "with", baseline_costname())
      )

    comparisons <- comparison_data() %>%
      mutate(
        cost_label = paste(scenario_name, "with", cost_name)
      )

    bind_rows(
      baseline %>% rename(plan_label = plan_label),
      comparisons %>% rename(plan_label = cost_label)
    ) %>%
      filter(currency == input$currency_select) %>%
      select(plan_label, year, cost_element) %>%
      group_by(plan_label, year) %>%
      summarise(total_budget = sum(cost_element, na.rm = TRUE), .groups = "drop")
  })

  #-Prepare difference data
  prepare_diff_data <- reactive({
    req(input$plan_bl_select, input$remaining_plan_select,
        input$year_select, input$currency_select)

    currency_symbol <- if (input$currency_select == "USD") "$" else "₦"

    baseline_label <- paste(baseline_shortname(), "with", baseline_costname())

    base <- baseline_data() %>%
      filter(currency == input$currency_select)

    comp <- comparison_data() %>%
      filter(currency == input$currency_select) %>%
      mutate(plan_label = paste(scenario_name, "with", cost_name))

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
          "Difference: ", ifelse(difference_millions >= 0, "+", ""), currency_symbol,
          format(difference_millions, big.mark = ","), "M<br>",
          "Change from Baseline: ", sprintf("%.0f%%", percent_change)
        )
      )
  })


 # UI components for cost data-------------------------------
  output$budget_comps <- renderUI({
    req(input$plan_bl_select != "", input$remaining_plan_select != "",
        input$year_select != "", input$currency_select != "")
     # plot card
      card(
        card_header(
          "Budget comparisons",
          tooltip(
            shiny::icon("info-circle"),
            "If All Years selected data is summarised across each year of the plan."
          )
        ),
        full_screen = FALSE,
        style = "resize: vertical; overflow: auto; min-height: 300px; max-height: 800px;",
        card_body(
          class = "p-0",
          min_height = 400,
          layout_column_wrap(
            width = 1/2,
            plotlyOutput(session$ns("budget_comp_chart"), height = "100%"),
            plotlyOutput(session$ns("budget_diff_chart"), height = "100%"),

          )
        )
      )

  })


  #-Plot one bar chart of total budget--------------------------
  output$budget_comp_chart <- renderPlotly({

    req(input$plan_bl_select != "", input$remaining_plan_select != "",
        input$year_select != "", input$currency_select != "")

    budget_barchart(
      prepare_cost_data(),
      currency_select = input$currency_select
    )
  })

  #-plot two cost difference plot--------------------------------
  output$budget_diff_chart <- renderPlotly({

    req(input$plan_bl_select != "", input$remaining_plan_select != "",
        input$year_select != "", input$currency_select != "")

    budget_diff_chart(
      prepare_diff_data(),
      currency_select = input$currency_select
    )
  })

  #-BUIDGET TABLES BL----------------------------------------------------------------------------
  output$budget_tables <- renderUI({
    req(input$plan_bl_select != "")
    card(
      card_header(
        "Baseline Budget Summary Table",
        tooltip(
          shiny::icon("info-circle"),
          " "
        )
      ),
      card_body(
        DT::dataTableOutput(session$ns("budget_table"))
      )
    )
  })

  output$budget_table <- DT::renderDataTable({
    req(input$plan_bl_select, input$year_select, input$currency_select)
    # Your create_budget_table() function returns a DT datatable.
    create_budget_table(
      process_budget_data(
        spatial_scale = "National",
        state_select = NULL,
        lga_select = NULL,
        currency_select = input$currency_select,
        data = baseline_data()
      )  |>
        dplyr::mutate(total_cost = round(total_cost / 1e6, 0)),
      currency_select = input$currency_select
    )
  })

  #-BUIDGET TABLES----------------------------------------------------------------------------
  output$budget_tables_comp <- renderUI({
    req(input$plan_bl_select != "", comparison_labels())

    cards <- lapply(comparison_labels(), function(label) {
      safe_label <- gsub(" ", "_", label)
      card(
        card_header(
          paste("Comparison Plan:", label),
          tooltip(
            shiny::icon("info-circle"),
            "Rows highlighted in Green represent cost decreases, red indicates increases from the Baseline plan"
          )
        ),
        card_body(
          DT::dataTableOutput(session$ns(paste0("budget_table_comp_", safe_label)))
        )
      )
    })

    tagList(cards)
  })

  observe({
    req(input$plan_bl_select, input$year_select, input$currency_select, comparison_labels())

    # 1. Process baseline
    baseline_processed <- process_budget_data(
      spatial_scale = "National",
      state_select = NULL,
      lga_select = NULL,
      currency_select = input$currency_select,
      data = baseline_data()
    ) %>%
      dplyr::mutate(total_cost = round(total_cost / 1e6, 0))

    # 2. Process all comparison plans
    labels <- comparison_labels()
    comp_data <- comparison_data()

    all_comp_processed <- list()

    purrr::walk(labels, function(label) {
      safe_id <- gsub(" ", "_", label)

      plan_data <- comp_data %>%
        filter(paste(scenario_name, "with", cost_name) == label)

      processed_comp <- process_budget_data(
        spatial_scale = "National",
        state_select = NULL,
        lga_select = NULL,
        currency_select = input$currency_select,
        data = plan_data
      ) %>%
        dplyr::mutate(total_cost = round(total_cost / 1e6, 0))

      # Store for plot (optional if you only plot last one)
      all_comp_processed[[label]] <<- processed_comp

      output[[paste0("budget_table_comp_", safe_id)]] <- DT::renderDataTable({
        create_budget_table(
          processed_comp,
          currency_select = input$currency_select,
          baseline_data = baseline_processed
        )
      })
    })

    # 3. Combine all processed comparison data into one dataframe
    combined_comp <- dplyr::bind_rows(all_comp_processed, .id = "plan_label")

    # 4. Generate plot (outside purrr::walk!)
    output$final_cost_plot <- renderPlot({
      req(input$currency_select)
      generate_final_cost_plot(
        baseline_processed = baseline_processed,
        comparison_processed = combined_comp,
        currency_select = input$currency_select
      )
    })
  })

  #-BUDGET FIGURE------------------------------------------------------------------------------
  # output$final_cost_plot <- renderPlotly({
  #   req(input$plan_bl_select != "", comparison_shortnames(), input$year_select, input$currency_select)
  #
  #   generate_final_cost_plotly(
  #     currency_select = input$currency_select,
  #     year_select = input$year_select,
  #     spatial_scale = "National",
  #     baseline_plan = baseline_shortname(),
  #     comp_plans = comparison_shortnames(),
  #     full_data = shared$budget_results  # or your equivalent input data
  #   )
  # })

  #--- UI: Wrap in Card ---
  output$budget_item_plots <- renderUI({
    req(input$plan_bl_select != "", input$remaining_plan_select, input$year_select, input$currency_select)
    card(
      card_header(
        "Item Cost Comparisons",
        tooltip(
          shiny::icon("info-circle"),
          "Comparison of total costs across selected plans by different budget items"
        )
      ),
      card_body(
        plotOutput(session$ns("final_cost_plot"), height = "600px")
      )
    )
  })

  }
