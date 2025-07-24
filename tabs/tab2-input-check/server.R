tab2Server <- function(input, output, session, shared) {
  ns <- session$ns

  observeEvent(shared$refresh_trigger,
    {
      message("🔄 Refreshing scenario data in tab2...")
      shared$reload_scenarios()
    },
    ignoreInit = FALSE
  )

  scenario_uploads <- reactive({
    shared$scenario_uploads_cache()
  })


  data_available <- reactive({
    df <- scenario_uploads()
    !is.null(df) && nrow(df) > 0
  })

  observe({
    if (!data_available()) {
      output$intervention_tabs <- renderUI({
        card(
          card_header("Aucune donnée téléchargée"),
          card_body(
            tags$p("Aucune donnée n'a été téléchargée dans l'application."),
            tags$p("Veuillez revenir à l’onglet «Saisie utilisateur» et télécharger vos scénarios d’intervention.")
          )
        )
      })
    }
  })

  observe({
    req(scenario_uploads())
    print("Scenarios loaded:")
    print(head(scenario_uploads()))
  })

  # input selection values
  output$plan_select_ui <- renderUI({
    req(scenario_uploads())

    plans <- unique(scenario_uploads()$scenario_name)
    selectInput(
      ns("plan_select"),
      "Sélectionnez le plan:",
      choices = c("", sort(plans)),
      selected = ""
    )
  })

  output$year_select_ui <- renderUI({
    req(scenario_uploads())

    years <- unique(scenario_uploads()$year)
    selectInput(
      ns("year_select"),
      "Sélectionnez les années d'intérêt:",
      choices = c("", sort(years), "Toutes les années"),
      selected = ""
    )
  })

  # Instructions
  # Adding instructions pop up
  observeEvent(input$show_instructions, {
    showModal(modalDialog(
      title = "Instructions détaillées pour l'utilisation du point de contrôle",
      easyClose = TRUE,
      size = "l",
      footer = modalButton("Fermer"),
      tagList(
        p("Une fois qu'un plan a été téléchargé, l'utilisateur peut passer à l'onglet « Vérifier le scénario ». Cette section permet aux utilisateurs de valider leurs plans d'intervention téléchargés et de signaler toute incohérence ou erreur logique."),
        tags$b("Étapes d'utilisation :"),
        tags$ul(
          tags$li("Sélectionnez un forfait précédemment téléchargé dans le menu déroulant."),
          tags$li("Sélectionnez l'année qui vous intéresse dans la liste déroulante."),
          tags$li("Les données rempliront une table en conséquence."),
          tags$li("Cliquez sur l'onglet d'une intervention pour afficher les informations de ciblage"),
          tags$li("Affichez les statistiques de couverture des titres affichées sous forme de cases récapitulatives colorées"),
          tags$ul(
            tags$li("🟩 Couverture complète – nombre de provinces où toutes les zones de santé bénéficient de l'intervention spécifique."),
            tags$li("🟧 Couverture partielle – nombre de provinces avec seulement certaines zones de santé ciblées, mais pas toutes"),
            tags$li("🟥 Aucune couverture – nombre de provinces sans zones de santé ciblées"),
          ),
          tags$li("•	Le tableau récapitule à l'échelle provinciale pour l'année sélectionnée, le nombre de zones de santé dans cette province, le nombre de bénéficiaires de l'intervention sélectionnée, le nombre de personnes ne recevant pas l'intervention et le pourcentage de couverture de cette intervention à l'échelle de la province.")
        ),
        p("Si l'utilisateur détecte des erreurs dans la spécification de la combinaison d'interventions, il peut revenir à l'onglet de téléchargement des données, supprimer la feuille de calcul incorrecte de l'application, corriger la version enregistrée localement et la télécharger à nouveau dans l'application.")
      )
    ))
  })

  # Create reactive filtered data based on user selections
  filtered_data <- reactive({
    req(data_available(), input$plan_select, input$year_select)

    year_filter <- if (input$year_select == "Toutes les années" || input$year_select == "") {
      unique(scenario_uploads()$year)
    } else {
      input$year_select
    }

    scenario_uploads() %>%
      filter(
        scenario_name == input$plan_select,
        year %in% year_filter
      ) %>%
      select(year, adm1, adm2, scenario_name, starts_with("code_"), -code_mii_urbain) %>%
      pivot_longer(
        cols = starts_with("code_"),
        names_to = "intervention",
        names_prefix = "code_",
        values_to = "included"
      ) %>%
      mutate(intervention = case_when(
        intervention == "prise_en_charge_public" ~ "Prise en charge public",
        intervention == "prise_en_charge_prive" ~ "Prise en charge prive",
        intervention == "Tpip" ~ "TPIp",
        intervention == "vacc" ~ "Vaccin",
        intervention == "mii_routine" ~ "Routine MII",
        intervention == "mii_campagne" ~ "Campagne MII",
        intervention == "cps" ~ "CPS",
        intervention == "cpp" ~ "CPP",
        intervention == "pier" ~ "Pulvérisation Intracommunautaire",
        intervention == "ggl" ~ "GGL",
        TRUE ~ intervention
      ))
  })

  observe({
    req(filtered_data())
    print(colnames(filtered_data()))
    print(head(filtered_data()))
  })

  # Get unique interventions from data
  interventions <- reactive({
    req(data_available(), input$plan_select)
    filtered_data() %>%
      filter(included == 1) |>
      filter(scenario_name == input$plan_select) %>%
      pull(intervention) %>%
      unique() %>%
      sort()
  })


  # Track the currently active intervention tab
  active_intervention <- reactiveVal()

  # Update active intervention when tab changes
  observeEvent(input$intervention_navset,
    {
      req(data_available())
      active_intervention(input$intervention_navset)
    },
    ignoreInit = TRUE
  )

  # Set default active intervention when interventions list changes
  observeEvent(interventions(), {
    req(data_available())
    if (length(interventions()) > 0 && is.null(active_intervention())) {
      active_intervention(interventions()[1])
    }
  })

  # Generate tabs for each intervention
  output$intervention_tabs <- renderUI({
    req(data_available(), interventions())

    if (length(interventions()) == 0) {
      return(card(
        card_header("Aucune donnée"),
        "Aucune intervention trouvée pour le forfait sélectionné. Veuillez sélectionner un autre forfait."
      ))
    }

    # Get year filter once for all interventions
    year_filter <- input$year_select

    # Create the tab panels with value boxes embedded in each
    intervention_panels <- lapply(interventions(), function(intervention) {
      # Get coverage data for this specific intervention
      coverage_data <- count_adm2_coverage(
        intervention = intervention,
        plan = input$plan_select,
        year_filter = year_filter,
        data = filtered_data()
      )

      # Calculate adm1 with full, partial, and no coverage for this specific intervention
      adm1_by_coverage <- coverage_data %>%
        group_by(Province) %>%
        summarize(
          min_coverage = min(`Coverage %`),
          max_coverage = max(`Coverage %`),
          avg_coverage = mean(`Coverage %`)
        ) %>%
        mutate(
          coverage_status = case_when(
            min_coverage == 100 ~ "full",
            max_coverage == 0 ~ "none",
            TRUE ~ "partial"
          )
        )

      # Count adm1 by coverage status
      summary_counts <- adm1_by_coverage %>%
        group_by(coverage_status) %>%
        summarise(count = n())

      # Convert to named list for easier access
      summary_list <- list(
        full_count = summary_counts %>% filter(coverage_status == "full") %>% pull(count) %>% as.integer(),
        partial_count = summary_counts %>% filter(coverage_status == "partial") %>% pull(count) %>% as.integer(),
        none_count = summary_counts %>% filter(coverage_status == "none") %>% pull(count) %>% as.integer()
      )

      # Handle NAs for adm1 that don't exist in a category
      if (length(summary_list$full_count) == 0) summary_list$full_count <- 0
      if (length(summary_list$partial_count) == 0) summary_list$partial_count <- 0
      if (length(summary_list$none_count) == 0) summary_list$none_count <- 0

      # Create nav panel with value boxes specific to this intervention
      nav_panel(
        title = intervention,
        # Value boxes for this specific intervention
        layout_column_wrap(
          width = 1 / 3,
          value_box("Provinces avec couverture complète", summary_list$full_count, showcase = bsicons::bs_icon("check-circle-fill"), theme = "teal"),
          value_box("Provinces avec couverture partielle", summary_list$partial_count, showcase = bsicons::bs_icon("dash-circle-fill"), theme = "yellow"),
          value_box("Provinces sans couverture", summary_list$none_count, showcase = bsicons::bs_icon("x-circle-fill"), theme = "red")
        ),
        DTOutput(ns(paste0("coverage_table_", make.names(intervention))))
      )
    })

    # Only create the card if we have intervention panels
    if (length(intervention_panels) > 0) {
      do.call(navset_card_tab, c(
        list(id = ns("intervention_navset")),
        intervention_panels
      ))
    } else {
      card(
        # card_header("Aucune intervention"),
        card_body("Aucune intervention trouvée pour les critères sélectionnés.")
      )
    }
  })

  # Generate tables for each intervention
  observe({
    req(data_available(), interventions(), input$plan_select)

    for (intervention in interventions()) {
      local({
        local_intervention <- intervention
        output_id <- paste0("coverage_table_", make.names(local_intervention))

        output[[output_id]] <- renderDT({
          req(input$plan_select)

          # Get year filter
          year_filter <- input$year_select

          # Get the data from the count_adm2_coverage function
          data_table <- count_adm2_coverage(
            intervention = local_intervention,
            plan = input$plan_select,
            year_filter = year_filter,
            data = filtered_data()
          )

          # Format the table using DT
          datatable(
            data_table,
            options = list(
              pageLength = 10,
              dom = "lfrtip",
              lengthMenu = list(c(10, 25, 50, -1), c("10", "25", "50", "All")),
              autoWidth = TRUE,
              scrollX = TRUE,
              searchHighlight = TRUE,
              search = list(regex = TRUE, caseInsensitive = TRUE),
              language = list(
                url = "https://cdn.datatables.net/plug-ins/1.13.4/i18n/fr-FR.json"
              )
            ),
            rownames = FALSE,
            colnames = c("Année", "Province", "Totale zone de sante", "Couverte", "Non couverte", "Couverture (%)"),
            filter = "top",
            class = "cell-border stripe"
          ) %>%
            formatStyle(
              "Coverage %",
              background = styleColorBar(
                range(0, 100),
                "#78c2ad"
              ),
              backgroundSize = "100% 90%",
              backgroundRepeat = "no-repeat",
              backgroundPosition = "center"
            ) %>%
            formatRound(
              c("Coverage %"),
              digits = 1
            )
        })
      })
    }
  })

  # Clear button logic
  observeEvent(input$clear_inputs, {
    updateSelectInput(session, "plan_select", selected = "")
    updateSelectInput(session, "year_select", selected = "")
  })
}
