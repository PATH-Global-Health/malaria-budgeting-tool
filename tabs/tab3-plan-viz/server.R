
tab3Server <- function(input, output, session, lga_outline, state_outline, country_outline, intervention_mix_maps, static_mix_maps) {

  #-Reactive: Filter intervention mix data based on selected plan--------------------------------
  filtered_intervention_mix <- reactive({
    req(input$plan_select)
    intervention_mix_maps[intervention_mix_maps$plan_shortname == input$plan_select, ]
  })

  #-Reactive: Filter static mix data for selected plan & year-------------------------------------
  filtered_static_mix <- reactive({
    req(input$plan_select)

    static_mix_maps |>
      filter(plan_shortname == input$plan_select)

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
    req(input$plan_select, input$year_select, input$spatial_scale)  # Ensure inputs are selected

    # Extract the plan description from the dataset
    plan_desc <-
      intervention_mix_maps |>
      filter(plan_shortname == input$plan_select) |>
      pull(plan_description) |>
      unique()  # Ensure we only get one unique description

    if (length(plan_desc) == 0) plan_desc <- "No description available"  # Default if missing

    # Start with the base description
    description <- paste0("<h4>Displaying results for ", input$plan_select,
                          " at the ", input$spatial_scale, " level for year: ", input$year_select, "</h4>")

    # If "State" is selected, include the state name
    if (input$spatial_scale == "State" && !is.null(input$state_select) && input$state_select != "") {
      description <- paste0(description, "<h4>State: ", input$state_select, "</h4>")
    }

    # If "LGA" is selected, include both state and LGA names
    if (input$spatial_scale == "LGA" && !is.null(input$state_select) && input$state_select != "" &&
        !is.null(input$lga_select) && input$lga_select != "") {
      description <- paste0(description, "<h4>State: ", input$state_select, " | LGA: ", input$lga_select, "</h4>")
    }

    # Add the plan description in a new row
    description <- paste0(description, "<h5><strong>Plan Description:</strong> ", plan_desc, "</h5>")

    # Render the HTML output
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
      intervention_mix = filtered_intervention_mix(),
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

    years <- unique(static_mix_maps$year)
    if (length(years) == 0) return(NULL)

    # Create a tab for each available year plus one for "All Years".
    lapply(c("All Years", years), function(y) {
      output[[paste0("static_map_", y)]] <- renderPlot({
        create_static_map(
          lga_outline = lga_outline,
          state_outline = state_outline,
          filtered_data = filtered_static_mix(),
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

    years <- unique(static_mix_maps$year)
    if (length(years) == 0) return(NULL)  # Prevent crash if no data

    navset_tab(
      !!!lapply(c(years, "All Years"), function(y) {
        nav_panel(
          title = paste(y),
          plotOutput(session$ns(paste0("static_map_", y)), height = "500px")
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
          leafletOutput(session$ns("interactive_map"))
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
      plan_select = input$plan_select,
      year_select = input$year_select,
      spatial_scale = input$spatial_scale,
      state_select = input$state_select,
      lga_select = input$lga_select,
      currency_select = input$currency_select
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
        DT::dataTableOutput(session$ns("budget_table"))
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
        plan_select = input$plan_select,
            year_select = input$year_select,
            spatial_scale = input$spatial_scale,
            state_select = input$state_select,
            lga_select = input$lga_select,
            currency_select = input$currency_select
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
        plan_select = input$plan_select,
        year_select = input$year_select,
        spatial_scale = input$spatial_scale,
        state_select = input$state_select,
        lga_select = input$lga_select,
        currency_select = input$currency_select
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
        plan_select = input$plan_select,
        year_select = input$year_select,
        spatial_scale = input$spatial_scale,
        state_select = input$state_select,
        lga_select = input$lga_select,
        currency_select = input$currency_select
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
        plan_select = input$plan_select,
        year_select = input$year_select,
        spatial_scale = input$spatial_scale,
        state_select = input$state_select,
        lga_select = input$lga_select,
        currency_select = input$currency_select
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
        plan_select = input$plan_select,
        year_select = input$year_select,
        spatial_scale = input$spatial_scale,
        state_select = input$state_select,
        lga_select = input$lga_select,
        currency_select = input$currency_select
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
      plan_select = input$plan_select,
      currency_select = input$currency_select,
      map_type = "total",
      year_select = input$year_select,
      state_select = input$state_select  # Pass selected state for highlighting
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
      plan_select = input$plan_select,
      currency_select = input$currency_select,
      map_type = "per person",
      year_select = input$year_select,
      state_select = input$state_select
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
      plan_select = input$plan_select,
      currency_select = input$currency_select,
      map_type = "total",
      year_select = input$year_select,
      state_select = input$state_select,  # Needed to filter the LGA shapefile correctly
      lga_select = input$lga_select         # Highlight the selected LGA
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
      plan_select = input$plan_select,
      currency_select = input$currency_select,
      map_type = "per person",
      year_select = input$year_select,
      state_select = input$state_select,
      lga_select = input$lga_select
    )
  })


#-PLOT ELEMENTS-------------------------------------------------------------------------
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
            billboarderOutput(session$ns("donut_chart"))
            )

        ),
        nav_panel(
          "Plot 2",
          card_title("Treemap of Budget Items"),
          card_body(
            class = "p-0",
            plotlyOutput(session$ns("treemap_chart"))
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
          card_title("Cost Breakdown by Category per Budget Item"),
          card_body(
            class = "p-0",
            plotlyOutput(session$ns("stacked_barchart"))
          )
        ),
        nav_panel(
          "Plot 2",
          card_title("Top 15 Specific Cost Components"),
          card_body(
            class = "p-0",
            plotlyOutput(session$ns("lolipop_chart"))
          )
        ),
        nav_panel(
          "Plot 3",
          card_title(" ")

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
            leafletOutput(session$ns("state_total_map"))
          )

        ),
        nav_panel(
          "Plot 2",
          card_title("State Level Cost per Person"),
          full_screen = TRUE,
          card_body(
            class = "p-0",
            leafletOutput(session$ns("state_pp_map"))
          )
        ),
        nav_panel(
          "Plot 3",
          card_title("LGA Level Total Costs"),
          full_screen = TRUE,
          card_body(
            class = "p-0",
            leafletOutput(session$ns("lga_total_map"))
          )

        ),
        nav_panel(
          "Plot 4",
          card_title("LGA Level Cost per Person"),
          full_screen = TRUE,
          card_body(
            class = "p-0",
            leafletOutput(session$ns("lga_pp_map"))
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
