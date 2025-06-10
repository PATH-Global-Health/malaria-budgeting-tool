tab3Server <- function(input, output, session, lga_outline, state_outline, country_outline,
                       shared) {
  # Namespace for the session
  ns <- session$ns

  # Create a local reactiveVal to track data availability specifically for this tab
  local_data_available <- reactiveVal(FALSE)

  # Add reactive for scenario data
  scenario_data <- reactive({
    cache <- shared$scenario_uploads_cache
    if (is.null(cache) || !is.reactive(cache)) return(NULL)
    cache()
  })

  # Observe new uploads or changes from the shared state
  observeEvent(shared$refresh_trigger, {
    message("🔄 Refreshing data in tab3...")
    shared$reload_all_uploads()
  }, ignoreInit = FALSE)

  # Check data availability once at startup
  observe({
    # Only run this once when the module initializes
    if (!is.null(shared$budget_results) &&
        is.data.frame(shared$budget_results) &&
        nrow(shared$budget_results) > 0) {
      message("Tab3: Budget data availability detected. Rows: ", nrow(shared$budget_results))
      local_data_available(TRUE)
    } else {
      message("Tab3: No budget data available at startup")
      local_data_available(FALSE)
    }
  }, priority = 1000)

  # Log scenario data when available
  observe({
    req(scenario_data())
    message("📊 Scenario data is available in tab3. Rows: ", nrow(scenario_data()))

    # For debugging, log some basic info about the scenario data
    if (nrow(scenario_data()) > 0) {
      message("📊 Scenario data overview:")
      if ("scenario_name" %in% colnames(scenario_data())) {
        unique_scenarios <- unique(scenario_data()$scenario_name)
        message("Available scenarios: ", paste(unique_scenarios, collapse=", "))
      }
      if ("year" %in% colnames(scenario_data())) {
        years <- sort(unique(scenario_data()$year))
        message("Available years: ", paste(years, collapse=", "))
      }
    }
  }, priority = 900)

  # Only check data when the reload button is clicked
  observeEvent(input$reload_budget_data, {
    message("Tab3: Manual reload of budget data requested")
    shared$reload_budgets()

    # Update local availability after reload
    if (!is.null(shared$budget_results) &&
        is.data.frame(shared$budget_results) &&
        nrow(shared$budget_results) > 0) {
      message("Tab3: Budget data availability detected after reload. Rows: ", nrow(shared$budget_results))
      local_data_available(TRUE)
    } else {
      message("Tab3: No budget data available after reload")
      local_data_available(FALSE)
    }
  })

  # Use the local reactiveVal for data readiness checks
  data_ready <- reactive({
    return(local_data_available())
  })

  # Reactive expression to get unique plans from generated budgets
  available_plans <- reactive({
    req(local_data_available())
    req(shared$budget_results)

    # Extract unique combinations of source_scenario and source_cost
    plan_combinations <- shared$budget_results %>%
      select(source_scenario, source_cost) %>%
      distinct() %>%
      mutate(plan_label = paste0(source_scenario, " with ", source_cost))

    return(plan_combinations)
  })

  # Reactive expression to get unique years from generated budgets
  available_years <- reactive({
    req(local_data_available())
    req(shared$budget_results)

    # Extract unique years
    years <- sort(unique(shared$budget_results$year))

    return(years)
  })

  # Generate dynamic plan selection UI
  output$plan_select_ui <- renderUI({
    plans <- available_plans()

    if (is.null(plans) || nrow(plans) == 0) {
      return(selectInput(
        ns("plan_select"),
        "Select the Plan:",
        choices = c("No plans available - generate budgets first"),
        selected = NULL
      ))
    }

    # Create choices with labels and values
    choices <- c("", setNames(
      paste0(plans$source_scenario, "|||", plans$source_cost),
      plans$plan_label
    ))

    selectInput(
      ns("plan_select"),
      "Select the Plan:",
      choices = choices,
      selected = ""
    )
  })

  # year selection
  output$year_select_ui <- renderUI({
    years <- available_years()

    if (is.null(years) || length(years) == 0) {
      return(selectInput(
        ns("year_select"),
        "Select Years of Interest:",
        choices = c("No years available - generate budgets first"),
        selected = NULL
      ))
    }

    # Create choices - only include "All Years" if there's more than one year
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

  # Scenario data panel for debugging
  output$scenario_data_info <- renderUI({
    req(scenario_data())

    # Create a summary of the scenario data
    if (nrow(scenario_data()) > 0) {
      unique_scenarios <- unique(scenario_data()$scenario_name)
      scenario_years <- scenario_data() %>%
        group_by(scenario_name) %>%
        summarize(years = paste(sort(unique(year)), collapse=", "))

      html_content <- "<h4>Available Scenarios:</h4><ul>"
      for (i in 1:nrow(scenario_years)) {
        html_content <- paste0(html_content,
                               "<li><strong>", scenario_years$scenario_name[i],
                               "</strong>: Years (", scenario_years$years[i], ")</li>")
      }
      html_content <- paste0(html_content, "</ul>")

      return(HTML(html_content))
    } else {
      return(HTML("<p>No scenario data available</p>"))
    }
  })

  # Parse the selected plan to get scenario and cost
  selected_plan_details <- reactive({
    req(input$plan_select)

    if (input$plan_select == "") {
      return(NULL)
    }

    # Split the combined value to get scenario and cost
    split_value <- strsplit(input$plan_select, "\\|\\|\\|")[[1]]

    if (length(split_value) == 2) {
      return(list(
        scenario = split_value[1],
        cost = split_value[2]
      ))
    } else {
      return(NULL)
    }
  })

  # Filter the budget results based on user selections
  filtered_budget <- reactive({
    req(shared$budget_results)
    req(selected_plan_details())
    req(input$year_select)

    result <- shared$budget_results %>%
      filter(
        scenario_name == selected_plan_details()$scenario,
        cost_name == selected_plan_details()$cost
      )

    # Filter by year if a specific year is selected
    if (input$year_select != "" && input$year_select != "All Years") {
      result <- result %>%
        filter(year == as.numeric(input$year_select))
    }

    return(result)
  })

  observe({
    req(filtered_budget())
    message("Filtered budget data: ", nrow(filtered_budget()), " rows")
    # Can add more debug info here
  })

  #-Reactive: Filter LGA list based on selected state----------------------------------------
  lga_list <- reactive({
    req(input$state_select)
     unique(lga_outline$lga[lga_outline$state == input$state_select])
  })

  #-Generate UI for State Selection----------------------------------------------------------
  output$state_ui <- renderUI({
    req(input$spatial_scale %in% c("State", "LGA"))
    selectizeInput(
      session$ns("state_select"),
      "Select State:",
      choices = c("", unique(lga_outline$state)),
      selected = "",
      options = list(placeholder = "Type or select a state", allowEmptyOption = TRUE)
    )
  })

  #-Generate UI for LGA Selection-----------------------------------------------------------
  output$lga_ui <- renderUI({
    req(input$spatial_scale == "LGA", input$state_select)
    selectizeInput(
      session$ns("lga_select"),
      "Select LGA:",
      choices = c("", lga_list()),
      selected = "",
      options = list(placeholder = "Type or select an LGA", allowEmptyOption = TRUE)
    )
  })

  #-Adding plan description-----------------------------------------------------------------
  output$page_description <- renderUI({
    req(input$plan_select, input$year_select, input$spatial_scale)
    req(filtered_budget())  # Make sure filtered budget data is available

    # Check if scenario_description column exists and get scenario description
    if ("scenario_description" %in% colnames(filtered_budget())) {
      plan_desc <- filtered_budget() %>%
        pull(scenario_description) %>%
        unique()
    } else {
      # Fallback to scenario_name if description isn't available
      plan_desc <- selected_plan_details()$scenario
    }

    # Check if cost_description column exists and get cost description
    if ("cost_description" %in% colnames(filtered_budget())) {
      cost_desc <- filtered_budget() %>%
        pull(cost_description) %>%
        unique()
    } else {
      # Fallback to cost_name if description isn't available
      cost_desc <- selected_plan_details()$cost
    }

    if (length(plan_desc) == 0) plan_desc <- "No description available"
    if (length(cost_desc) == 0) cost_desc <- "No cost description available"

    # Build the description HTML
    description <- paste0("<h4>Displaying results for ", input$plan_select,
                          " at the ", input$spatial_scale, " level for year: ", input$year_select, "</h4>")

    if (input$spatial_scale == "State" && !is.null(input$state_select) && input$state_select != "") {
      description <- paste0(description, "<h4>State: ", input$state_select, "</h4>")
    }

    if (input$spatial_scale == "LGA" && !is.null(input$state_select) && input$state_select != "" &&
        !is.null(input$lga_select) && input$lga_select != "") {
      description <- paste0(description, "<h4>State: ", input$state_select, " | LGA: ", input$lga_select, "</h4>")
    }

    # Add both the plan description and cost description
    description <- paste0(description,
                          "<h5><strong>Plan Description:</strong> ", plan_desc, "</h5>",
                          "<h5><strong>Cost Description:</strong> ", cost_desc, "</h5>")

    HTML(description)
  })


  #-Generate the interactive map-----------------------------------------------------------
  output$interactive_map <- renderLeaflet({
    req(input$plan_select, input$spatial_scale)  # Wait until a spatial scale is chosen

    if (input$spatial_scale == "State") {
      req(input$plan_select, input$state_select)
      # Filter state_outline based on state_selected
    } else if (input$spatial_scale == "LGA") {
      req(input$plan_select, input$state_selected, input$lga_select)
      # Filter lga_outline based on state_selected and lga_selected
    }

    create_intervention_leaflet(
      lga_outline = lga_outline,
      state_outline = state_outline,
      country_outline = country_outline,
      intervention_mix = filtered_budget(),
      spatial_scale = input$spatial_scale,
      state_select = input$state_select,
      lga_select = input$lga_select,
      center_lng = 9,
      center_lat = 4,
      zoom = 5.2
    )
  })

  #-Generate the static maps tabbed-----------------------------------------------------
  # make maps
  observe({
    req(input$plan_select)

    years <- unique(filtered_budget()$year)
    if (length(years) == 0) return(NULL)

    # Create a tab for each available year plus one for "All Years".
    lapply(c("All Years", years), function(y) {
      output[[paste0("static_map_", y)]] <- renderPlot({
        create_static_map(
          lga_outline = lga_outline,
          state_outline = state_outline,
          filtered_data = filtered_budget(),
          plan_select = input$plan_select,
          spatial_scale = input$spatial_scale,
          state_select = input$state_select,
          lga_select = input$lga_select,
          year_value = y
        )
      })
    })
  })

  #-UI for multiple static maps, with tabs for each year--------------------------------
  output$static_map_tabs <- renderUI({
    req(input$plan_select)

    years <- unique(filtered_budget()$year)
    if (length(years) == 0) return(NULL)  # Prevent crash if no data
    # if (length(years >1)) years <- c(years, "All Years")

    navset_tab(
      !!!lapply(c(years), function(y) {
        nav_panel(
          title = paste(y),
          withSpinner(plotOutput(session$ns(paste0("static_map_", y)), height = "500px"))
        )
      })
    )
  })


  #-Show Maps UI when Plan & Year are selected------------------------------------------
  output$maps_ui <- renderUI({
    req(input$plan_select != "")

    layout_column_wrap(
      width = 1/2,  # Two columns, each taking 50% width

      # Interactive Map Card
      card(
        card_header(
          "Full Intervtion Mix Map",
          tooltip(
            shiny::icon("info-circle"),
            "Map displays all interventions targeted aggregated over each year of the Plan"
            )
          ),
        full_screen = TRUE,
        card_body(
          class = "p-0",
          withSpinner(leafletOutput(session$ns("interactive_map")))
        )
      ),

      # Static Map Card: Show either a **single plot** or **tabs**
      card(
        card_header(
          "Intervention Specific Maps",
          tooltip(
            shiny::icon("info-circle"),
            "Maps display interventions targeted and type by year. If All Years is selected data is aggregated from each year of the Plan"
            )
          ),
        full_screen = TRUE,
        card_body(
          class = "p-0",
          uiOutput(session$ns("static_map_tabs"))  # Always show tabs
        )
        )

    )
  })

  #-Ribbon Icons------------------------------------------------------------------------
  output$value_boxes <- renderUI({

    req(input$plan_select, input$year_select, input$spatial_scale, input$currency_select)
    # For spatial scales that require a state selection, ensure input$state_select exists
    if (input$spatial_scale %in% c("State", "LGA")) {
      req(input$state_select)
    }
    # For LGA level, require an LGA selection
    if (input$spatial_scale == "LGA") {
      req(input$lga_select)
    }

    create_icon_summaries(
      spatial_scale = input$spatial_scale,
      state_select = input$state_select,
      lga_select = input$lga_select,
      currency_select = input$currency_select,
      data = filtered_budget(),
      year_select = input$year_select
    )
  })

  #-Budget table summaries-------------------------------------------------------------
  output$budget_table_card <- renderUI({
    req(input$plan_select != "")
    card(
      card_header(
        "Budget Summary Table",
        tooltip(
          shiny::icon("info-circle"),
          "If State or LGA level is selected, data is calculated for malaria interventions only. Support services costs are not generated at these spatial levels."
        )
        ),
      card_body(
        withSpinner(DT::dataTableOutput(session$ns("budget_table")))
      )
    )
  })

  output$budget_table <- DT::renderDataTable({
    req(input$plan_select, input$year_select, input$spatial_scale, input$currency_select)
    # For spatial scales that require a state selection, ensure input$state_select exists
    if (input$spatial_scale %in% c("State", "LGA")) {
      req(input$state_select)
    }
    # For LGA level, require an LGA selection
    if (input$spatial_scale == "LGA") {
      req(input$lga_select)
    }
    # Your create_budget_table() function returns a DT datatable.
    create_budget_table(
      process_budget_data(
            spatial_scale = input$spatial_scale,
            state_select = input$state_select,
            lga_select = input$lga_select,
            currency_select = input$currency_select,
            data = filtered_budget()
      ),
      currency_select = input$currency_select
    )
  })

  #-Cost Plot elements------------------------------------------------------------------

   #-Donut chart--------------------
  output$donut_chart <- renderBillboarder({

    req(input$plan_select, input$year_select, input$spatial_scale, input$currency_select)
    # For spatial scales that require a state selection, ensure input$state_select exists
    if (input$spatial_scale %in% c("State", "LGA")) {
      req(input$state_select)
    }
    # For LGA level, require an LGA selection
    if (input$spatial_scale == "LGA") {
      req(input$lga_select)
    }

    donut_plot(
      process_budget_data(
        spatial_scale = input$spatial_scale,
        state_select = input$state_select,
        lga_select = input$lga_select,
        currency_select = input$currency_select,
        data = filtered_budget()
      )
    )
  })

  #-Treemap chart----------------------
  output$treemap_chart <- renderPlotly({

    req(input$plan_select, input$year_select, input$spatial_scale, input$currency_select)
    # For spatial scales that require a state selection, ensure input$state_select exists
    if (input$spatial_scale %in% c("State", "LGA")) {
      req(input$state_select)
    }
    # For LGA level, require an LGA selection
    if (input$spatial_scale == "LGA") {
      req(input$lga_select)
    }

    treemap_plot(
      process_budget_data(
        spatial_scale = input$spatial_scale,
        state_select = input$state_select,
        lga_select = input$lga_select,
        currency_select = input$currency_select,
        data = filtered_budget()
      ),
      currency_select = input$currency_select
    )
  })

  #-Stacked bar chart---------------------
  output$stacked_barchart <- renderPlotly({

    req(input$plan_select, input$year_select, input$spatial_scale, input$currency_select)
    # For spatial scales that require a state selection, ensure input$state_select exists
    if (input$spatial_scale %in% c("State", "LGA")) {
      req(input$state_select)
    }
    # For LGA level, require an LGA selection
    if (input$spatial_scale == "LGA") {
      req(input$lga_select)
    }

    stacked_plot(
      process_item_data(
        spatial_scale = input$spatial_scale,
        state_select = input$state_select,
        lga_select = input$lga_select,
        currency_select = input$currency_select,
        data = filtered_budget()
      ),
      currency_select = input$currency_select
    )
  })

  #-stacked % bar chart---------------------
  output$stacked_prop <-
    renderPlotly({
      req(input$plan_select, input$year_select, input$spatial_scale, input$currency_select)
      # For spatial scales that require a state selection, ensure input$state_select exists
      if (input$spatial_scale %in% c("State", "LGA")) {
        req(input$state_select)
      }
      # For LGA level, require an LGA selection
      if (input$spatial_scale == "LGA") {
        req(input$lga_select)
      }

      stacked_plot_prop(
        process_item_data(
          spatial_scale = input$spatial_scale,
          state_select = input$state_select,
          lga_select = input$lga_select,
          currency_select = input$currency_select,
          data = filtered_budget()
        ),
        currency_select = input$currency_select
      )
    })

  #-lolipop chart---------------------------
  output$lolipop_chart <- renderPlotly({

    req(input$plan_select, input$year_select, input$spatial_scale, input$currency_select)
    # For spatial scales that require a state selection, ensure input$state_select exists
    if (input$spatial_scale %in% c("State", "LGA")) {
      req(input$state_select)
    }
    # For LGA level, require an LGA selection
    if (input$spatial_scale == "LGA") {
      req(input$lga_select)
    }

    lolipop_plot(
      process_item_data(
        spatial_scale = input$spatial_scale,
        state_select = input$state_select,
        lga_select = input$lga_select,
        currency_select = input$currency_select,
        data = filtered_budget()
      ),
      currency_select = input$currency_select
    )
  })

  #-Map 1 - state total cost------------------
  output$state_total_map <- renderLeaflet({
    req(input$plan_select, input$year_select, input$spatial_scale, input$currency_select)
    if (input$spatial_scale %in% c("State", "LGA")) {
      req(input$state_select)
    }

    cost_dist_map(
      map_level = "State",
      currency_select = input$currency_select,
      map_type = "total",
      state_select = input$state_select,
      data = filtered_budget()
    )
  })

  #-Map 2 - state pp cost------------------
  output$state_pp_map <- renderLeaflet({
    req(input$plan_select, input$year_select, input$spatial_scale, input$currency_select)
    if (input$spatial_scale %in% c("State", "LGA")) {
      req(input$state_select)
    }

    cost_dist_map(
      map_level = "State",
      currency_select = input$currency_select,
      map_type = "per person",
      state_select = input$state_select,
      data = filtered_budget()
    )
  })

  #-Map 3 - lga total cost------------------
  output$lga_total_map <- renderLeaflet({
    req(input$plan_select, input$year_select, input$spatial_scale, input$currency_select)
    if (input$spatial_scale %in% c("State", "LGA")) {
      req(input$state_select)
    }
    if (input$spatial_scale == "LGA") {
      req(input$lga_select)
    }

    cost_dist_map(
      map_level = "LGA",
      currency_select = input$currency_select,
      map_type = "total",
      state_select = input$state_select,
      lga_select = input$lga_select,
      data = filtered_budget()
    )
  })

  #-Map 4 - lga pp cost------------------
  output$lga_pp_map <- renderLeaflet({
    req(input$plan_select, input$year_select, input$spatial_scale, input$currency_select)
    if (input$spatial_scale %in% c("State", "LGA")) {
      req(input$state_select)
    }
    if (input$spatial_scale == "LGA") {
      req(input$lga_select)
    }

    cost_dist_map(
      map_level = "LGA",
      currency_select = input$currency_select,
      map_type = "per person",
      state_select = input$state_select,
      lga_select = input$lga_select,
      data = filtered_budget()
    )
  })


# #-PLOT ELEMENTS-------------------------------------------------------------------------
  output$cost_charts <- renderUI({
    req(input$plan_select != "")

    layout_column_wrap(
      width = 1/3,  # 3 columns

      # Card 1: Proportional Cost Summaries with 2 tabs
      navset_card_tab(
        full_screen = TRUE,
        title = "Proportional Cost Summaries",
        nav_panel(
          "Plot 1",
          card_title("Proportion of Total Budget by Item"),
          card_body(
            class = "p-0",
            withSpinner(billboarderOutput(session$ns("donut_chart")))
            )

        ),
        nav_panel(
          "Plot 2",
          card_title("Treemap of Budget Items"),
          card_body(
            class = "p-0",
            withSpinner(plotlyOutput(session$ns("treemap_chart")))
          )

        ),
        nav_panel(
          shiny::icon("circle-info"),
          markdown("The proportion of each budget item's contribution to the
                   total budget is displayed using a donut chart - hover over
                   each section of the chart to see the proportional contribution of
                   that item (%).<br><br>The Treemap in panel Plot 2 displays
                   this information in another way, the size of the block for each
                   item is the relative contribution of that budget item to
                   the total budget.<br><br> When State or LGA level
                   information is displayed the Support Services costs are
                   not generated at this level only costs related to malaria
                   interventions are shown.")
        )
      ),

      # Card 2: Intervention Cost Breakdowns with 3 tabs
      navset_card_tab(
        full_screen = TRUE,
        title = "Intervention Cost Summaries",
        nav_panel(
          "Plot 1",
          card_title("Cost Breakdown by Category per Intervention"),
          card_body(
            class = "p-0",
            withSpinner(plotlyOutput(session$ns("stacked_barchart")))
          )
        ),
        nav_panel(
          "Plot 2",
          card_title("% Contribution by Category per Intervention"),
          card_body(
            class = "p-0",
            withSpinner(plotlyOutput(session$ns("stacked_prop")))
          )
        ),
        nav_panel(
          "Plot 3",
          card_title("Top 15 Specific Cost Components"),
          card_body(
            class = "p-0",
            withSpinner(plotlyOutput(session$ns("lolipop_chart")))
          )

        ),
        nav_panel(
          shiny::icon("circle-info"),
          markdown("Intervention specific total costs are shown spilt into data on the
                   cost category within each intervention (procurement, implementation
                   and support services).<br><br>The top 15 cost elements are then
                   displayed which highlight which specific line item of
                   the budget has the largest contribution to the overal
                   budget estimate.<br><br> When State or LGA level
                   information is displayed the Support Services costs are
                   not generated at this level only costs related to malaria
                   interventions are shown.")
        )
        ),

      # Card 3: Spatial Cost Summaries with conditional tabs
      navset_card_tab(
        full_screen = TRUE,
        title = "Spatial Cost Summaries",
        nav_panel(
          "Plot 1",
          card_title("State Level Total Costs"),
          full_screen = TRUE,
          card_body(
            class = "p-0",
            withSpinner(leafletOutput(session$ns("state_total_map")))
          )

        ),
        nav_panel(
          "Plot 2",
          card_title("State Level Cost per Person"),
          full_screen = TRUE,
          card_body(
            class = "p-0",
            withSpinner(leafletOutput(session$ns("state_pp_map")))
          )
        ),
        nav_panel(
          "Plot 3",
          card_title("LGA Level Total Costs"),
          full_screen = TRUE,
          card_body(
            class = "p-0",
            withSpinner(leafletOutput(session$ns("lga_total_map")))
          )

        ),
        nav_panel(
          "Plot 4",
          card_title("LGA Level Cost per Person"),
          full_screen = TRUE,
          card_body(
            class = "p-0",
            withSpinner(leafletOutput(session$ns("lga_pp_map")))
          )

        ),
        nav_panel(
          shiny::icon("circle-info"),
          markdown("State and LGA data on the total budget or total budget
                   per person associated with
                   the selected plan are displayed. Support Services costs are
                   not generated at the State and LGA level and data related only costs
                   associated with malaria interventions are shown.")
        )
      )
    )
  })



  #-Clear selections--------------------------------------------------------------------
  observeEvent(input$clear_inputs, {
    updateSelectInput(session, "plan_select", selected = "")
    updateSelectInput(session, "spatial_scale", selected = "")
    updateSelectInput(session, "year_select", selected = "")
    updateSelectInput(session, "currency_select", selected = "")
    updateSelectizeInput(session, "state_select", selected = "")
    updateSelectizeInput(session, "lga_select", selected = "")
  })
}
