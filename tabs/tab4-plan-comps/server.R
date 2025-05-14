tab4Server <-
  function(input, output,
           session, lga_outline,
           state_outline, country_outline, intervention_mix_maps) {

  #-check if data avaliable and add message if not-----------------------------------------------
   data_ready <- reactive({
      !is.null(intervention_mix_maps) && nrow(intervention_mix_maps) > 0
    })

   observe({
      if (!data_ready()) {
        output$page_description <- renderUI({
          card(
            card_header("Budgets Not Yet Generated"),
            card_body(
              tags$p("Budgets not yet generated - return to the 'Generate Budgets' tab to specificy budget generation"),
              tags$p("Once generated, return to this tab to view the generated budgets")
            )
          )
        })
      }
    })

  #-UI selection for remaining plans---------------------------------------------------------
  output$remaining_plan_select <- renderUI({
    # Make sure a baseline plan is selected
    req(input$plan_bl_select)

    # Compute the remaining plans
    available_plans <- setdiff(plan_labels, input$plan_bl_select)

    # Return a checkbox group input using the available plans
    checkboxGroupInput(
      session$ns("remaining_plan_select"),
      "Select plans to compare:",
      choices = available_plans,
      inline = FALSE
    )
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

  #-reactive elements for plans selected (short name for filters)----------------------------
  baseline_shortname <- reactive({
    req(input$plan_bl_select)
    # Extracts something like "Plan A" from a longer string:
    str_extract(input$plan_bl_select, "Plan\\s+[A-Z]")
  })

  comparison_shortnames <- reactive({
    req(input$remaining_plan_select)
    # For each selected plan, extract the "Plan A" part
    sapply(input$remaining_plan_select, function(x) str_extract(x, "Plan\\s+[A-Z]"))
  })

  #-Map boxes leaflet-------------------------------------------------------------------------
  output$baseline_map <- renderLeaflet({
    req(input$plan_bl_select)

    filtered_intervention_mix <-
      intervention_mix_maps |>
      filter(plan_shortname == baseline_shortname())


    create_intervention_leaflet(
      lga_outline = lga_outline,
      state_outline = state_outline,
      country_outline = country_outline,
      intervention_mix = filtered_intervention_mix,
      spatial_scale = "National",
      state_select =  NULL,
      lga_select =  NULL,
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
  output$comparison_tabs <- renderUI({
    req(input$remaining_plan_select)

    # Create a list of tabPanel elements for each comparison plan short name.
    # Use unname() so that the list is not named (which avoids tabsetPanel errors).
    tabs <- unname(lapply(comparison_shortnames(), function(plan) {
      # Create a safe ID by replacing spaces with underscores
      safe_plan <- gsub(" ", "_", plan)
      tabPanel(
        title = plan,
        leafletOutput(session$ns(paste0("comparison_map_", safe_plan)))
      )
    }))

    # Use do.call to pass the list of tabs as unnamed arguments
    do.call(tabsetPanel, c(list(id = session$ns("comparison_tabset")), tabs))
  })

  #-Dynamic rendering of comparison maps-------------------------------------------------------
  observe({
    req(comparison_shortnames())
    for (plan in comparison_shortnames()) {
      safe_plan <- gsub(" ", "_", plan)
      output[[paste0("comparison_map_", safe_plan)]] <- renderLeaflet({
        # Filter your intervention_mix_maps data using the current plan short name
        filtered_data <- intervention_mix_maps %>%
          filter(plan_shortname == plan)

        # Generate the Leaflet map using your custom function
        create_intervention_leaflet(
          lga_outline = lga_outline,
          state_outline = state_outline,
          country_outline = country_outline,
          intervention_mix = filtered_data,
          spatial_scale = "National",
          state_select =  NULL,
          lga_select =  NULL,
          center_lng = 9,
          center_lat = 4,
          zoom = 5.2
        )
      })
    }
  })

  #-BUDGET COMPARISON PLOTS------------------------------------------------------------------

  #-Reactive function for Cost Comparison Data----------------
  prepare_cost_data <- reactive({

    req(input$plan_bl_select, input$remaining_plan_select,
        input$year_select, input$currency_select)

    dat <-
      national_budget |>
      filter(
        currency == input$currency_select,
        plan_shortname %in% c(baseline_shortname(), comparison_shortnames())
      ) |>
      select(
        plan_shortname,
        plan_description,
        year,
        total_budget
      )

    if (input$year_select == "All Years") {
      dat <- dat |>
        group_by(
          plan_shortname,
          plan_description,
         ) |>
        dplyr::summarise(
          total_budget = sum(total_budget, na.rm = TRUE)
        )
    } else {
      dat <- dat |>
        dplyr::filter(year == input$year_select)
    }


  })


  #-Prepare difference data
  prepare_diff_data <- reactive({

    req(input$plan_bl_select, input$remaining_plan_select,
        input$year_select, input$currency_select)

    currency_symbol <- if (input$currency_select == "USD") "$" else "₦"

    dat <-
      national_budget |>
      filter(
        currency == input$currency_select,
        plan_shortname %in% c(baseline_shortname(), comparison_shortnames())
      ) |>
      select(
        plan_shortname,
        plan_description,
        year,
        total_budget
      )

    if (input$year_select == "All Years") {
      dat <- dat |>
        group_by(
          plan_shortname,
          plan_description,
        ) |>
        dplyr::summarise(
          total_budget = sum(total_budget, na.rm = TRUE)
        )
    } else {
      dat <- dat |>
        dplyr::filter(year == input$year_select)
    }

    baseline_cost <- dat$total_budget[dat$plan_shortname ==  baseline_shortname()]
    req(length(baseline_cost) > 0)

    dat <- dat %>%
      filter(plan_shortname %in% c(comparison_shortnames())) %>%
      mutate(
        difference_millions = round((total_budget - baseline_cost) / 1e6),
        percent_change = round((difference_millions * 1e6 / baseline_cost) * 100),
        label = paste(plan_shortname, " vs ", baseline_shortname()),
        hover_text = paste0("Difference: ", ifelse(difference_millions >= 0, "+", ""), currency_symbol,
                            format(difference_millions, big.mark = ","), "M<br>",
                            "Change from Baseline: ", sprintf("%.0f%%", percent_change))

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
            "If All Years selected data is summarised for each year of the plan."
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
        plan_select = baseline_shortname(),
        year_select = input$year_select,
        spatial_scale = "National",
        state_select = NULL,
        lga_select = NULL,
        currency_select = input$currency_select
      )  |>
        dplyr::mutate(total_cost = round(total_cost / 1e6, 0)),
      currency_select = input$currency_select
    )
  })

  #-BUIDGET TABLES----------------------------------------------------------------------------
  output$budget_tables_comp <- renderUI({
    req(input$plan_bl_select != "", comparison_shortnames())

    # Create a card for each comparison plan.
    cards <- lapply(comparison_shortnames(), function(plan) {
      safe_plan <- gsub(" ", "_", plan)
      card(
        card_header(
          paste("Comparison Plan:", plan),
          tooltip(
            shiny::icon("info-circle"),
            "Rows highlighted in Green represent changes from the Baseline plan"
          )
        ),
        card_body(
          DT::dataTableOutput(session$ns(paste0("budget_table_comp_", safe_plan)))
        )
      )
    })

    tagList(cards)
  })

  observe({
    req(input$plan_bl_select, input$year_select, input$currency_select, comparison_shortnames())

    # Get baseline data (used for highlighting differences)
    baseline_data <- process_budget_data(
      plan_select = baseline_shortname(),
      year_select = input$year_select,
      spatial_scale = "National",
      state_select = NULL,
      lga_select = NULL,
      currency_select = input$currency_select
    ) %>%
      dplyr::mutate(total_cost = round(total_cost / 1e6, 0))

    for (plan in comparison_shortnames()) {
      safe_plan <- gsub(" ", "_", plan)

      output[[paste0("budget_table_comp_", safe_plan)]] <- DT::renderDataTable({
        # Process data for the current comparison plan.
        comparison_data <- process_budget_data(
          plan_select = plan,
          year_select = input$year_select,
          spatial_scale = "National",
          state_select = NULL,
          lga_select = NULL,
          currency_select = input$currency_select
        ) %>%
          dplyr::mutate(total_cost = round(total_cost / 1e6, 0))

        # Use create_budget_table() with the baseline data.
        create_budget_table(
          comparison_data,
          currency_select = input$currency_select,
          baseline_data = baseline_data
        )
      })
    }
  })

  #-BUDGET FIGURE------------------------------------------------------------------------------
  output$final_cost_plot <- renderPlot({
    req(input$plan_bl_select != "", comparison_shortnames(), input$year_select, input$currency_select)

    generate_final_cost_plot(
      currency_select = input$currency_select,
      year_select = input$year_select,
      spatial_scale = "National",
      baseline_plan = baseline_shortname(),
      comp_plans = comparison_shortnames()
    )
  })

  output$budget_item_plots <- renderUI({
    req(input$plan_bl_select != "", comparison_shortnames(), input$year_select, input$currency_select)
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
