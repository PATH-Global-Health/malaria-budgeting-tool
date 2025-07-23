tab3Server <- function(input, output, session, adm2_outline, adm1_outline, shared) {
  # Namespace for the session
  ns <- session$ns

  # Adding instructions pop up
  observeEvent(input$show_instructions, {
    showModal(modalDialog(
      title = "Instructions détaillées pour l'utilisation du point de contrôle",
      easyClose = TRUE,
      size = "l",
      footer = modalButton("Fermer"),
      tagList(
        p("Cette section permet aux utilisateurs de visualiser les résultats détaillés d'un plan budgétisé sélectionné. Elle comprend des cartes, des tableaux et des synthèses visuelles présentant la répartition spatiale, la répartition des coûts et le profil budgétaire global du plan d'intervention sélectionné."),
        tags$b("Étapes d'utilisation :"),
        tags$ol(
          tags$li("🧾 Sélectionnez les entrées en haut de la page (plan, échelle, année, devise)."),
          tags$li("🧭 Consultez la section Aperçu du plan."),
          tags$li("🗺️ Explorez les cartes : Mix d'intervention complet et Intervention spécifique."),
          tags$li("👥 Consultez les chiffres principaux de la population et du budget."),
          tags$li("📋 Consultez le tableau récapitulatif du budget."),
          tags$li("📈 Explorez les visualisations budgétaires par article, catégorie et géographie."),
          tags$li("🔍 Utilisez des filtres et des outils interactifs. Survolez et utilisez les icônes ℹ️ pour obtenir des explications.")
        ),
        p("📝 Conseil : utilisez cet onglet pour vérifier la couverture des interventions dans toutes les zones géographiques et évaluer la concentration budgétaire par intervention ou par région avant de comparer les plans.")
      )
    ))
  })

  # Create a local reactiveVal to track data availability specifically for this tab
  local_data_available <- reactiveVal(FALSE)

  observe({
    print(colnames(shared$budget_results))
  })

  # Add reactive for scenario data
  scenario_data <- reactive({
    cache <- shared$scenario_uploads_cache
    if (is.null(cache) || !is.reactive(cache)) {
      return(NULL)
    }
    cache()
  })

  # Observe new uploads or changes from the shared state
  observeEvent(shared$refresh_trigger,
    {
      message("🔄 Refreshing data in tab3...")
      shared$reload_all_uploads()
    },
    ignoreInit = FALSE
  )

  # Check data availability once at startup
  observe(
    {
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
    },
    priority = 1000
  )

  # Log scenario data when available
  observe(
    {
      req(scenario_data())
      message("📊 Scenario data is available in tab3. Rows: ", nrow(scenario_data()))

      # For debugging, log some basic info about the scenario data
      if (nrow(scenario_data()) > 0) {
        message("📊 Scenario data overview:")
        if ("scenario_name" %in% colnames(scenario_data())) {
          unique_scenarios <- unique(scenario_data()$scenario_name)
          message("Available scenarios: ", paste(unique_scenarios, collapse = ", "))
        }
        if ("year" %in% colnames(scenario_data())) {
          years <- sort(unique(scenario_data()$year))
          message("Available years: ", paste(years, collapse = ", "))
        }
      }
    },
    priority = 900
  )

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
    plan_combinations <-
      shared$budget_results %>%
      select(plan_id) %>%
      distinct()

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
        "Sélectionnez le budget:",
        choices = c("Aucun budget disponible – générez d’abord des budgets"),
        selected = NULL
      ))
    }

    # Create choices with labels and values
    choices <- c("", plans$plan_id)

    selectInput(
      ns("plan_select"),
      "Sélectionnez le budget:",
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
        "Sélectionnez les années d'intérêt:",
        choices = c("Aucune année disponible - générez d'abord les budgets"),
        selected = NULL
      ))
    }

    # Create choices - only include "All Years" if there's more than one year
    if (length(years) > 1) {
      choices <- c("", as.character(years), "Toutes les années")
    } else {
      choices <- c("", as.character(years))
    }

    selectInput(
      ns("year_select"),
      "Sélectionnez les années d'intérêt:",
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
        summarize(years = paste(sort(unique(year)), collapse = ", "))

      html_content <- "<h4>Scénarios disponibles:</h4><ul>"
      for (i in 1:nrow(scenario_years)) {
        html_content <- paste0(
          html_content,
          "<li><strong>", scenario_years$scenario_name[i],
          "</strong>: Années (", scenario_years$years[i], ")</li>"
        )
      }
      html_content <- paste0(html_content, "</ul>")

      return(HTML(html_content))
    } else {
      return(HTML("<p>Aucune donnée de scénario disponible</p>"))
    }
  })

  # Parse the selected plan
  selected_plan_id <- reactive({
    req(input$plan_select)
    if (input$plan_select == "") {
      return(NULL)
    }
    input$plan_select
  })

  # Filter the budget results based on user selections
  filtered_budget <- reactive({
    req(shared$budget_results)
    req(selected_plan_id())
    req(input$year_select)

    result <- shared$budget_results %>%
      filter(plan_id == selected_plan_id())

    if (input$year_select != "" && input$year_select != "Toutes les années") {
      result <- result %>% filter(year == as.numeric(input$year_select))
    }

    return(result)
  })

  observe({
    req(filtered_budget())
    message("Filtered budget data: ", nrow(filtered_budget()), " rows")
    # Can add more debug info here
  })

  # helper function for validation
  validate_plan_inputs <- function(input) {
    validate(
      need(input$plan_select != "", "Veuillez sélectionner un plan."),
      need(input$year_select != "", "Veuillez sélectionner une année."),
      need(input$spatial_scale != "", "Veuillez sélectionner une échelle spatiale.")
    )
  }


  #-Reactive: Filter LGA list based on selected state----------------------------------------
  adm2_list <- reactive({
    req(input$adm1_select)
    unique(adm2_outline$adm2[adm2_outline$adm1 == input$adm1_select])
  })

  #-Generate UI for State Selection----------------------------------------------------------
  output$adm1_ui <- renderUI({
    req(input$spatial_scale %in% c("Province", "Zone de santé"))
    selectizeInput(
      session$ns("adm1_select"),
      "Sélectionnez la province:",
      choices = c("", unique(adm2_outline$adm1)),
      selected = "",
      options = list(placeholder = "Tapez ou sélectionnez une province", allowEmptyOption = TRUE)
    )
  })

  #-Generate UI for LGA Selection-----------------------------------------------------------
  output$adm2_ui <- renderUI({
    req(input$spatial_scale == "Zone de santé", input$adm1_select)
    selectizeInput(
      session$ns("adm2_select"),
      "Sélectionnez la zone de santé:",
      choices = c("", adm2_list()),
      selected = "",
      options = list(placeholder = "Tapez ou sélectionnez une zone de santé", allowEmptyOption = TRUE)
    )
  })

  #-UI for budget envelope------------------------------------------------------------------
  output$budget_envelope_ui <- renderUI({
    req(input$year_select == "Toutes les années")

    tagList(
      div(
        style = "margin-bottom: 1rem;",
        card(
          card_header(tagList(icon("hand-holding-usd"), " Saisir le budget disponible")),
          card_body(
            numericInput(ns("available_budget"), "Budget disponible", value = NULL, min = 0),
            selectInput(ns("available_currency"), "Devise", choices = c("USD", "CDF"), selected = input$currency_select),
            helpText("⚠️ Disponible uniquement si «Toutes les années» est sélectionné.")
          ),
          class = "bg-light"
        )
      )
    )
  })


  #-Adding plan description-----------------------------------------------------------------
  output$page_description <- renderUI({
    validate_plan_inputs(input)

    req(filtered_budget()) # Make sure filtered budget data is available

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


    # Check if assumptions_changes column exists and get cost description
    if ("assumptions_changes" %in% colnames(filtered_budget())) {
      assum_desc <- filtered_budget() %>%
        pull(assumptions_changes) %>%
        unique()
    } else {
      # Fallback to cost_name if description isn't available
      assum_desc <- NULL
    }

    if (length(plan_desc) == 0) plan_desc <- "Aucune description disponible"
    if (length(cost_desc) == 0) cost_desc <- "Aucune description des coûts disponible"
    if (length(assum_desc) == 0) assum_desc <- "Aucune modification d'hypothèse disponible"

    # Build the description HTML
    description <- paste0(
      "<p><strong>Affichage des résultats pour ", input$plan_select,
      " au ", input$spatial_scale, " niveau pour l'année: ", input$year_select, "</strong></p>"
    )

    if (input$spatial_scale == "Province" && !is.null(input$adm1_select) && input$adm1_select != "") {
      description <- paste0(description, "<p><strong>Province:</strong> ", input$adm1_select, "</p>")
    }

    if (input$spatial_scale == "Zone de santé" && !is.null(input$adm1_select) && input$adm1_select != "" &&
      !is.null(input$adm2_select) && input$adm2_select != "") {
      description <- paste0(
        description, "<p><strong>Province:</strong> ", input$adm1_select,
        " | <strong>Zone de santé:</strong> ", input$adm2_select, "</p>"
      )
    }

    # Add plan + cost + assumptions
    description <- paste0(
      description,
      "<p><strong>Description du plan:</strong> ", plan_desc, "</p>",
      "<p><strong>Description des coûts:</strong> ", cost_desc, "</p>",
      "<p><strong>Description modification d'hypothèse:</strong> ", assum_desc, "</p>"
    )

    HTML(description)
  })


  #-Generate the interactive map-----------------------------------------------------------
  output$interactive_map <- renderLeaflet({
    req(input$plan_select, input$spatial_scale) # Wait until a spatial scale is chosen

    if (input$spatial_scale == "Province") {
      req(input$plan_select, input$adm1_select)
      # Filter adm1_outline based on adm1_selected
    } else if (input$spatial_scale == "Zone de santé") {
      req(input$plan_select, input$adm1_selected, input$adm2_select)
      # Filter adm2_outline based on adm1_selected and adm2_selected
    }

    create_intervention_leaflet(
      adm2_outline = adm2_outline,
      adm1_outline = adm1_outline,
      country_outline = NULL,
      intervention_mix = filtered_budget(),
      spatial_scale = input$spatial_scale,
      adm1_select = input$adm1_select,
      adm2_select = input$adm2_select
    )
  })

  #-Generate the static maps-----------------------------------------------------
  # Single static map plot
  output$static_map_plot <- renderPlot({
    req(input$plan_select)
    req(filtered_budget())

    # Decide whether to filter by year or aggregate
    static_data <- filtered_budget()

    year_label <- input$year_select
    if (!is.null(year_label) && year_label != "" && year_label != "all_years" && year_label != "Toutes les années") {
      static_data <- static_data %>% filter(year == as.numeric(year_label))
    } else {
      year_label <- "Toutes les années"
      # could add summarise step if you want to aggregate e.g. counts or means here
    }

    # Create the static map
    create_static_map(
      adm2_outline = adm2_outline,
      adm1_outline = adm1_outline,
      filtered_data = static_data,
      plan_select = input$plan_select,
      year_value = year_label,
      spatial_scale = input$spatial_scale,
      adm1_select = input$adm1_select,
      adm2_select = input$adm2_select
    )
  })


  #-Show Maps UI when Plan & Year are selected------------------------------------------
  output$maps_ui <- renderUI({
    req(input$plan_select != "")

    layout_column_wrap(
      # width = 1 / 2, # Two columns, each taking 50% width

      # INTERACTIVE MAP CARD
      card(
        full_screen = TRUE,
        card_header(
          "Carte complète des mix d'interventions",
          tooltip(
            shiny::icon("info-circle"),
            "La carte affiche toutes les interventions ciblées..."
          )
        ),
        card_body(
          class = "p-0 d-flex align-items-stretch", # ensure no padding and flexible growth
          leafletOutput(session$ns("interactive_map")) # no height specified!
        )
      ),

      # Static Map Card
      card(
        full_screen = TRUE,
        card_header(
          "Cartes spécifiques aux interventions",
          tooltip(
            shiny::icon("info-circle"),
            "Les cartes présentent les interventions ciblées et leur type par année..."
          )
        ),
        card_body(
          class = "p-0",
          withSpinner(plotOutput(session$ns("static_map_plot")))
        )
      )
    )
  })




  #-Ribbon Icons------------------------------------------------------------------------
  output$value_boxes <- renderUI({
    req(input$currency_select, input$plan_select, input$year_select, input$spatial_scale)

    create_icon_summaries(
      spatial_scale = input$spatial_scale,
      adm1_select = input$adm1_select,
      adm2_select = input$adm2_select,
      year_select = input$year_select,
      currency_select = input$currency_select,
      available_budget = input$available_budget,
      data = filtered_budget(),
      target_population = target_population,
      ns = session$ns # 👈 ensures namespacing works
    )
  })



  #-Budget table summaries-------------------------------------------------------------
  output$budget_table_card <- renderUI({
    req(input$plan_select != "")
    card(
      card_header(
        "Tableau récapitulatif du budget"
      ),
      card_body(
        withSpinner(DT::dataTableOutput(session$ns("budget_table")))
      )
    )
  })

  output$budget_table <- DT::renderDataTable({
    req(input$plan_select, input$year_select, input$spatial_scale, input$currency_select)
    # For spatial scales that require a state selection, ensure input$adm1_select exists
    if (input$spatial_scale %in% c("Province", "Zone de santé")) {
      req(input$adm1_select)
    }
    # For LGA level, require an LGA selection
    if (input$spatial_scale == "Zone de santé") {
      req(input$adm2_select)
    }
    # Your create_budget_table() function returns a DT datatable.
    create_budget_table(
      process_budget_data(
        spatial_scale = input$spatial_scale,
        adm1_select = input$adm1_select,
        adm2_select = input$adm2_select,
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
    # For spatial scales that require a state selection, ensure input$adm1_select exists
    if (input$spatial_scale %in% c("Province", "Zone de santé")) {
      req(input$adm1_select)
    }
    # For LGA level, require an LGA selection
    if (input$spatial_scale == "Zone de santé") {
      req(input$adm2_select)
    }

    donut_plot(
      process_budget_data(
        spatial_scale = input$spatial_scale,
        adm1_select = input$adm1_select,
        adm2_select = input$adm2_select,
        currency_select = input$currency_select,
        data = filtered_budget()
      )
    )
  })

  #-Treemap chart----------------------
  output$treemap_chart <- renderPlotly({
    req(input$plan_select, input$year_select, input$spatial_scale, input$currency_select)
    # For spatial scales that require a state selection, ensure input$adm1_select exists
    if (input$spatial_scale %in% c("Province", "Zone de santé")) {
      req(input$adm1_select)
    }
    # For LGA level, require an LGA selection
    if (input$spatial_scale == "Zone de santé") {
      req(input$adm2_select)
    }

    treemap_plot(
      process_budget_data(
        spatial_scale = input$spatial_scale,
        adm1_select = input$adm1_select,
        adm2_select = input$adm2_select,
        currency_select = input$currency_select,
        data = filtered_budget()
      ),
      currency_select = input$currency_select
    )
  })

  #-Stacked bar chart---------------------
  output$stacked_barchart <- renderPlotly({
    req(input$plan_select, input$year_select, input$spatial_scale, input$currency_select)
    # For spatial scales that require a state selection, ensure input$adm1_select exists
    if (input$spatial_scale %in% c("Province", "Zone de santé")) {
      req(input$adm1_select)
    }
    # For LGA level, require an LGA selection
    if (input$spatial_scale == "Zone de santé") {
      req(input$adm2_select)
    }

    stacked_plot(
      process_item_data(
        spatial_scale = input$spatial_scale,
        adm1_select = input$adm1_select,
        adm2_select = input$adm2_select,
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
      # For spatial scales that require a state selection, ensure input$adm1_select exists
      if (input$spatial_scale %in% c("Province", "Zone de santé")) {
        req(input$adm1_select)
      }
      # For LGA level, require an LGA selection
      if (input$spatial_scale == "Zone de santé") {
        req(input$adm2_select)
      }

      stacked_plot_prop(
        process_item_data(
          spatial_scale = input$spatial_scale,
          adm1_select = input$adm1_select,
          adm2_select = input$adm2_select,
          currency_select = input$currency_select,
          data = filtered_budget()
        ),
        currency_select = input$currency_select
      )
    })

  #-lolipop chart---------------------------
  output$lolipop_chart <- renderPlotly({
    req(input$plan_select, input$year_select, input$spatial_scale, input$currency_select)
    # For spatial scales that require a state selection, ensure input$adm1_select exists
    if (input$spatial_scale %in% c("Province", "Zone de santé")) {
      req(input$adm1_select)
    }
    # For LGA level, require an LGA selection
    if (input$spatial_scale == "Zone de santé") {
      req(input$adm2_select)
    }

    lolipop_plot(
      process_item_data(
        spatial_scale = input$spatial_scale,
        adm1_select = input$adm1_select,
        adm2_select = input$adm2_select,
        currency_select = input$currency_select,
        data = filtered_budget()
      ),
      currency_select = input$currency_select
    )
  })

  #-Map 1 - state total cost------------------
  output$adm1_total_map <- renderLeaflet({
    req(input$plan_select, input$year_select, input$spatial_scale, input$currency_select)
    if (input$spatial_scale %in% c("Province", "Zone de santé")) {
      req(input$adm1_select)
    }

    cost_dist_map(
      map_level = "Province",
      currency_select = input$currency_select,
      map_type = "total",
      adm1_select = input$adm1_select,
      data = filtered_budget()
    )
  })

  #-Map 2 - state pp cost------------------
  output$adm1_pp_map <- renderLeaflet({
    req(input$plan_select, input$year_select, input$spatial_scale, input$currency_select)
    if (input$spatial_scale %in% c("Province", "Zone de santé")) {
      req(input$adm1_select)
    }

    cost_dist_map(
      map_level = "Province",
      currency_select = input$currency_select,
      map_type = "per person",
      adm1_select = input$adm1_select,
      data = filtered_budget()
    )
  })

  #-Map 3 - lga total cost------------------
  output$adm2_total_map <- renderLeaflet({
    req(input$plan_select, input$year_select, input$spatial_scale, input$currency_select)
    if (input$spatial_scale %in% c("Province", "Zone de santé")) {
      req(input$adm1_select)
    }
    if (input$spatial_scale == "Zone de santé") {
      req(input$adm2_select)
    }

    cost_dist_map(
      map_level = "Zone de santé",
      currency_select = input$currency_select,
      map_type = "total",
      adm1_select = input$adm1_select,
      adm2_select = input$adm2_select,
      data = filtered_budget()
    )
  })

  #-Map 4 - lga pp cost------------------
  output$adm2_pp_map <- renderLeaflet({
    req(input$plan_select, input$year_select, input$spatial_scale, input$currency_select)
    if (input$spatial_scale %in% c("Province", "Zone de santé")) {
      req(input$adm1_select)
    }
    if (input$spatial_scale == "Zone de santé") {
      req(input$adm2_select)
    }

    cost_dist_map(
      map_level = "Zone de santé",
      currency_select = input$currency_select,
      map_type = "per person",
      adm1_select = input$adm1_select,
      adm2_select = input$adm2_select,
      data = filtered_budget()
    )
  })


  # #-PLOT ELEMENTS-------------------------------------------------------------------------
  output$cost_charts <- renderUI({
    req(input$plan_select != "")

    layout_column_wrap(
      width = 1 / 3, # 3 columns

      # Card 1: Proportional Cost Summaries with 2 tabs
      navset_card_tab(
        full_screen = TRUE,
        title = "Résumés des coûts proportionnels",
        nav_panel(
          "1",
          card_title("Proportion du budget total par poste"),
          card_body(
            class = "p-0",
            withSpinner(billboarderOutput(session$ns("donut_chart")))
          )
        ),
        # nav_panel(
        #   "Plot 2",
        #   card_title("Treemap of Budget Items"),
        #   card_body(
        #     class = "p-0",
        #     withSpinner(plotlyOutput(session$ns("treemap_chart")))
        #   )
        #
        # ),
        nav_panel(
          shiny::icon("circle-info"),
          markdown("La proportion de la contribution de chaque poste budgétaire au budget total est affichée à l'aide d'un graphique en anneau: survolez chaque section du graphique pour voir la contribution proportionnelle de ce poste (%).")
        )
      ),

      # Card 2: Intervention Cost Breakdowns with 3 tabs
      navset_card_tab(
        full_screen = TRUE,
        title = "Résumés des coûts d'intervention",
        nav_panel(
          "1",
          card_title("Répartition des coûts par catégorie et par intervention"),
          card_body(
            class = "p-0",
            withSpinner(plotlyOutput(session$ns("stacked_barchart")))
          )
        ),
        nav_panel(
          "2",
          card_title("% Contribution par catégorie par intervention"),
          card_body(
            class = "p-0",
            withSpinner(plotlyOutput(session$ns("stacked_prop")))
          )
        ),
        nav_panel(
          "3",
          card_title("Les 15 principaux éléments de coût spécifiques"),
          card_body(
            class = "p-0",
            withSpinner(plotlyOutput(session$ns("lolipop_chart")))
          )
        )
      ),

      # Card 3: Spatial Cost Summaries with conditional tabs
      navset_card_tab(
        full_screen = TRUE,
        title = "Résumés des coûts spatiaux",
        nav_panel(
          "1",
          card_title("Coûts totaux au niveau de la province"),
          full_screen = TRUE,
          card_body(
            class = "p-0",
            withSpinner(leafletOutput(session$ns("adm1_total_map")))
          )
        ),
        nav_panel(
          "2",
          card_title("Coût par personne au niveau de la province"),
          full_screen = TRUE,
          card_body(
            class = "p-0",
            withSpinner(leafletOutput(session$ns("adm1_pp_map")))
          )
        ),
        nav_panel(
          "3",
          card_title("Coûts totaux au niveau de la zone de santé"),
          full_screen = TRUE,
          card_body(
            class = "p-0",
            withSpinner(leafletOutput(session$ns("adm2_total_map")))
          )
        ),
        nav_panel(
          "4",
          card_title("Coût par personne dans la zone de santé"),
          full_screen = TRUE,
          card_body(
            class = "p-0",
            withSpinner(leafletOutput(session$ns("adm2_pp_map")))
          )
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
    updateSelectizeInput(session, "adm1_select", selected = "")
    updateSelectizeInput(session, "adm2_select", selected = "")
  })
}
