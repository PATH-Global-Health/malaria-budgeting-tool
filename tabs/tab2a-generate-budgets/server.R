tab2aServer <- function(input, output, session, shared) {
  ns <- session$ns

  # # ----------------------------------------------------------------------------
  # # SAFELY INITIALIZE SHARED REFRESH TRIGGER
  # # ----------------------------------------------------------------------------
  # if (is.null(isolate(shared$refresh_trigger))) {
  #   shared$refresh_trigger <- reactiveVal(0)
  # }

  # ----------------------------------------------------------------------------
  # INSTRUCTIONS POP UP
  # ----------------------------------------------------------------------------
  observeEvent(input$show_instructions, {
    showModal(modalDialog(
      title = "Instructions détaillées pour Générer des budgets",
      easyClose = TRUE,
      size = "l",
      footer = modalButton("Fermer"),
      tagList(
        p("Cette section permet aux utilisateurs de générer des budgets d'intervention complets en combinant des plans précédemment téléchargés avec une feuille de coûts sélectionnée. L'outil calcule les budgets totaux en fonction de la combinaison d'interventions sélectionnée, de la population cible et des hypothèses budgétaires définies pour chaque intervention."),
        tags$b("Étapes d'utilisation :"),
        tags$ol(
          tags$li("📝 Sélectionnez un ou plusieurs plans d’intervention dans la liste déroulante."),
          tags$li("💵 Sélectionnez une feuille de coûts dans la liste déroulante."),
          tags$li("📐 Sélectionnez les hypothèses de quantification budgétaire pour chaque intervention."),
          tags$li("Sélectionnez l’hypothèse de tampon d’approvisionnement à utiliser."),
          tags$li("⚠️ Vérifiez et confirmez vos sélections. Dans le cas contraire, l'outil signalera les données manquantes."),
          tags$li("⚙️ Cliquez sur le bouton « Générer des budgets »."),
          tags$li("⏳ Attendez le message de confirmation. Les données budgétaires générées apparaîtront sous les onglets « Visualisation du plan » et « Comparaison du plan ».")
        ),
        p("📝 Conseil : Assurez-vous que votre feuille de coûts inclut les valeurs pour tous les types et sous-types d’intervention prévus pour une utilisation (par exemple, PBO par rapport aux moustiquaires standard, RTSS par rapport aux vaccins R21).")
      )
    ))
  })

  # ----------------------------------------------------------------------------
  # BUDGET ASSUMPTIONS POP UP
  # ----------------------------------------------------------------------------
  observeEvent(input$show_assumptions, {
    showModal(modalDialog(
      title = "Informations détaillées sur les hypothèses clés pour l'élaboration du budget",
      easyClose = TRUE,
      size = "l",
      footer = modalButton("Fermer"),
      tagList(
        p("Voici les principaux paramètres et hypothèses utilisés pour générer les budgets dans cet outil. Pour les ajuster, sélectionnez l'option « Ajuster » dans la liste déroulante « Hypothèses » de la matrice de sélection."),
        accordion(
          open = FALSE,
          accordion_panel(
            title = "Campagne MII",
            open = FALSE,
            tagList(
              tags$ul(
                tags$li("Personnes par MII : 1,8"),
                tags$li("Population cible : population totale"),
                tags$li("Couverture : 100 % de la population cible"),
                tags$li("MII par balle : 50"),
                tags$li("Tampon d'approvisionnement : 10%")
              )
            )
          ),
          accordion_panel(
            title = "Routine MII",
            open = FALSE,
            tagList(
              tags$ul(
                tags$li("Population cible : enfants de moins de 5 ans et femmes enceintes"),
                tags$li("Couverture : 30 % de la population cible"),
                tags$li("Tampon d'approvisionnement : 10%")
              )
            )
          ),
          accordion_panel(
            title = "TPIp",
            open = FALSE,
            tagList(
              tags$ul(
                tags$li("Population cible : femmes enceintes"),
                tags$li("Couverture : 80 % de participation à l'ANC"),
                tags$li("Points de contact : 3 par femme enceinte"),
                tags$li("Tampon d'approvisionnement : 10%")
              )
            )
          ),
          accordion_panel(
            title = "CPS",
            open = FALSE,
            tagList(
              tags$ul(
                tags$li("Population cible : enfants de 3 mois à 5 ans"),
                tags$li("Cycles : 4 cycles mensuels"),
                tags$li("Couverture : 100 % de la population cible"),
                tags$li("Répartition par âge : 18 % 3-11 mois, 77 % 12-59 mois"),
                tags$li("Tampon d'approvisionnement : 10%")
              )
            )
          ),
          accordion_panel(
            title = "CPP",
            open = FALSE,
            tagList(
              tags$ul(
                tags$li("Population cible : enfants de 0 à 2 ans"),
                tags$li("Couverture : 85 % grâce aux visites de vaccination"),
                tags$li("Points de contact : 4 par an"),
                tags$li("Facteur d'échelle nutritionnelle : 75 %"),
                tags$li("Tampon d'approvisionnement : 10%")
              )
            )
          ),
          accordion_panel(
            title = "Vaccin",
            open = FALSE,
            tagList(
              tags$ul(
                tags$li("Population cible : enfants de 5 mois à 36 mois"),
                tags$li("Couverture : 84 %"),
                tags$li("Doses : 4 doses par enfant"),
                tags$li("Tampon d'approvisionnement : 10%")
              )
            )
          ),
          accordion_panel(
            title = "Pulvérisation Intracommunautaire",
            open = FALSE,
            tagList(
              tags$ul(
                tags$li("Non quantifié actuellement dans cette version de l’outil — nécessite une consultation du programme.")
              )
            )
          ),
          accordion_panel(
            title = "Prise en charge public",
            open = FALSE,
            tagList(
              tags$ul(
                tags$li("Non quantifié actuellement dans cette version de l’outil — nécessite une consultation du programme.")
              )
            )
          )
        ),
        p(tags$em("Remarque : Toutes les hypothèses sont actuellement appliquées uniformément à toutes les zones géographiques. Des hypothèses propres à chaque province ou zone sanitaire pourront être intégrées dans les mises à jour futures.")),
        p(tags$em("Consultez l’onglet Méthodes pour une description complète des calculs et des hypothèses pour chaque intervention."))
      )
    ))
  })

  # ----------------------------------------------------------------------------
  # NULL OPERATOR AND SAFE ACCESSOR
  # ----------------------------------------------------------------------------
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  get_safe <- function(lst, i) {
    if (is.null(lst)) {
      return(NULL)
    }
    if (length(lst) >= i) lst[[i]] else NULL
  }

  # ----------------------------------------------------------------------------
  # INITIAL REACTIVE VALUES
  # ----------------------------------------------------------------------------
  row_count <- reactiveVal(1) # matrix row selections
  stored_selections <- reactiveVal(list( # Store selected values across UI redraws
    plans = list(),
    costs = list(),
    assumptions = list(),
    adjustments = list()
  ))
  matrix_selections <- reactiveVal(NULL) # Store matrix selections
  budget_results <- reactiveVal(NULL) # Store budget results
  budget_generating <- reactiveVal(FALSE) # Budget generation status
  incomplete_rows <- reactiveVal(integer(0)) # check if incomplete rows are trying to be processed

  # ----------------------------------------------------------------------------
  # BUDGET HISTORY
  # ----------------------------------------------------------------------------
  budget_history <- reactiveVal(data.frame( # Budget history table
    date_generated = character(), # Date/time budget was generated
    scenario = character(), # Scenario name
    cost = character(), # Cost name
    years = character(), # Years covered
    assumption_type = character(), # "baseline", "custom", or "none"
    assumptions_changes = character(), # Semicolon-separated changes or "Default"
    file_path = character(), # Path to the saved RDS file
    stringsAsFactors = FALSE
  ))

  observe({
    if (!dir.exists("generated")) dir.create("generated", showWarnings = FALSE) # On startup, load budget history if it exists
    history_path <- "generated/budget_history.rds"
    if (file.exists(history_path)) {
      history <- tryCatch(readRDS(history_path), error = function(e) NULL)
      if (!is.null(history)) budget_history(history)
    }
  })

  # ----------------------------------------------------------------------------
  # REFRESH DATA WHEN SHARED REFRESH TRIGGER FIRES
  # ----------------------------------------------------------------------------
  observeEvent(shared$refresh_trigger,
    {
      message("🔄 Refreshing data in tab2a...")
      shared$reload_all_uploads()
    },
    ignoreInit = FALSE
  )

  # ----------------------------------------------------------------------------
  # REACTIVE UPLOAD DATA SOURCES
  # ----------------------------------------------------------------------------
  scenario_data <- reactive({
    cache <- shared$scenario_uploads_cache
    if (is.null(cache) || !is.reactive(cache)) {
      return(NULL)
    }
    cache()
  })

  cost_data <- reactive({
    cache <- shared$cost_upload_cache
    if (is.null(cache) || !is.reactive(cache)) {
      return(NULL)
    }
    cache()
  })

  data_available <- reactive({
    !is.null(scenario_data()) && nrow(scenario_data()) > 0 &&
      !is.null(cost_data()) && nrow(cost_data()) > 0
  })

  # ----------------------------------------------------------------------------
  # MATRIX UI
  # ----------------------------------------------------------------------------
  output$matrix_ui <- renderUI({
    req(stored_selections())
    if (!data_available()) {
      return(
        div(
          style = "padding: 1em; color: #b30000;",
          tags$h5("⚠️ Données téléchargées sans frais/forfait"),
          tags$p("Revenez à l’onglet Saisie utilisateur pour spécifier cela.")
        )
      )
    }

    req(cost_data(), scenario_data(), row_count())
    num_rows <- row_count()
    if (num_rows <= 0) num_rows <- 1

    plan_choices <- c("Sélectionnez un plan", unique(scenario_data()$scenario_name))
    cost_choices <- c("Sélectionnez les coûts", unique(cost_data()$cost_name))

    assumption_vars <- c(
      "Campagne MII : personnes par moustiquaire",
      "Campagne MII : population cible",
      "Campagne MII : couverture de la population cible",
      "Campagne MII : moustiquaires par balle",
      "Campagne MII : marge de moustiquaires (%)",
      "Routine MII : population cible",
      "Routine MII : couverture de la population cible",
      "Routine MII : marge de moustiquaires (%)",
      "TPIp : fréquentation CPN",
      "TPIp : points de contact",
      "TPIp : marge pour l’approvisionnement en médicaments",
      "CPS : population cible",
      "CPS : couverture de la population cible",
      "CPS : cycles",
      "CPS : ciblage par âge",
      "CPS : marge pour l’approvisionnement en médicaments",
      "CPP : couverture",
      "CPP : points de contact",
      "CPP : facteur de mise à l’échelle nutritionnelle",
      "CPP : marge pour l’approvisionnement en médicaments",
      "Vaccination : couverture",
      "Vaccination : nombre de doses",
      "Vaccination : marge pour l’approvisionnement"
    )

    assumption_choices <- c("Sélectionnez une hypothèse", "Accepter la base de référence", "Faire des ajustements")

    stored <- stored_selections()

    table_header <- div(
      style = "display: table-row; font-weight: bold; background: #eaeaea;",
      div(style = "display: table-cell; width:8%;  border:1px solid #ccc; padding:5px;", "Spécification"),
      div(style = "display: table-cell; width:15%; border:1px solid #ccc; padding:5px;", "Plan"),
      div(style = "display: table-cell; width:12%; border:1px solid #ccc; padding:5px;", "Coûts"),
      div(style = "display: table-cell; width:15%; border:1px solid #ccc; padding:5px;", "Hypothèses"),
      div(style = "display: table-cell; width:25%; border:1px solid #ccc; padding:5px;", "Ajustements"),
      div(style = "display: table-cell; width:25%; border:1px solid #ccc; padding:5px;", "Résumé ajustements")
    )

    table_rows <- lapply(1:num_rows, function(i) {
      is_invalid <- i %in% incomplete_rows()
      row_style <- if (is_invalid) {
        "display: table-row; background-color: #ffe6e6;" # light red
      } else {
        "display: table-row;"
      }
      selected_plan <- get_safe(stored$plans, i) %||% "Sélectionnez un plan"
      selected_cost <- get_safe(stored$costs, i) %||% "Sélectionnez les coûts"
      selected_assumption <- get_safe(stored$assumptions, i) %||% "Sélectionnez une hypothèse"


      div(style = row_style, list(
        div(
          style = "display: table-cell; width:8%; border:1px solid #ccc; padding:5px; font-weight:bold;",
          tagList(
            HTML(paste0("Budget ", i, if (is_invalid) " ⚠️")),
            actionLink(
              ns(paste0("clear_row_", i)),
              icon("broom", style = "color: #888;"), # light grey
              style = "margin-left: 8px;",
              title = "Réinitialiser cette ligne"
            )
          )
        ),
        div(
          style = "display: table-cell; width:15%; border:1px solid #ccc; padding:5px;",
          selectInput(ns(paste0("row_plan_", i)), NULL, plan_choices, selected_plan, width = "100%")
        ),
        div(
          style = "display: table-cell; width:12%; border:1px solid #ccc; padding:5px;",
          selectInput(ns(paste0("row_cost_", i)), NULL, cost_choices, selected_cost, width = "100%")
        ),
        div(
          style = "display: table-cell; width:15%; border:1px solid #ccc; padding:5px;",
          selectInput(ns(paste0("row_assumption_", i)), NULL, assumption_choices, selected_assumption, width = "100%")
        ),
        div(
          style = "display: table-cell; width:25%; border:1px solid #ccc; padding:5px;",
          conditionalPanel(
            condition = sprintf("input['%s'] == 'Faire des ajustements'", ns(paste0("row_assumption_", i))),
            tagList(
              selectInput(ns(paste0("row_param_", i)), "Paramètre à ajuster", assumption_vars, width = "100%"),
              uiOutput(ns(paste0("dynamic_input_", i))),
              actionButton(ns(paste0("submit_adjust_", i)), "Ajouter l'ajustement")
            )
          )
        ),
        div(
          style = "display: table-cell; width:25%; border:1px solid #ccc; padding:5px;",
          uiOutput(ns(paste0("summary_inline_", i)))
        )
      ))
    })

    tagList(
      div(
        id = "matrix_container",
        style = "display: table; width:100%; border-collapse: collapse; margin-bottom:15px;",
        list(table_header, table_rows)
      ),
      div(
        style = "margin-top: 20px; color: #666; font-style: italic;",
        "Remarque: sélectionnez le plan, les coûts et les hypothèses pour chaque ligne."
      )
    )
  })


  # ----------------------------------------------------------------------------
  # TRACK SELECTIONS
  # ----------------------------------------------------------------------------
  observe({
    req(row_count())
    current <- stored_selections()
    for (i in 1:row_count()) {
      current$plans[[i]] <- input[[paste0("row_plan_", i)]] %||% get_safe(current$plans, i)
      current$costs[[i]] <- input[[paste0("row_cost_", i)]] %||% get_safe(current$costs, i)
      current$assumptions[[i]] <- input[[paste0("row_assumption_", i)]] %||% get_safe(current$assumptions, i)
    }
    stored_selections(current)
  })

  # ----------------------------------------------------------------------------
  # DYNAMIC INPUTS FOR ADJUSTMENTS
  # ----------------------------------------------------------------------------
  observe({
    lapply(1:row_count(), function(i) {
      output[[paste0("dynamic_input_", i)]] <- renderUI({
        param <- input[[paste0("row_param_", i)]]
        if (is.null(param) || param == "") {
          return(NULL)
        }
        switch(param,

               # Campagne MII
               "Campagne MII : personnes par moustiquaire" = numericInput(ns(paste0("adj_val_", i)), "Nouveau nombre de personnes par moustiquaire :", 1.8, min = 1, step = 0.1),
               "Campagne MII : population cible" = selectInput(ns(paste0("adj_val_", i)), "Nouvelle population cible :",
                                                                 choices = c("Population totale", "Enfants de moins de 5 ans", "Enfants de moins de 5 ans et femmes enceintes", "Enfants de moins de 10 ans"), selected = "Population totale"
               ),
               "Campagne MII : couverture de la population cible" = sliderInput(ns(paste0("adj_val_", i)), "Nouvelle couverture (%) :", 0, 100, 100),
               "Campagne MII : moustiquaires par balle" = numericInput(ns(paste0("adj_val_", i)), "Nouveau nombre de moustiquaires par balle :", 50, min = 1, step = 1),
               "Campagne MII : marge de moustiquaires (%)" = sliderInput(ns(paste0("adj_val_", i)), "Nouvelle marge (%) :", 0, 100, 10),

               # Routine MII
               "Routine MII : population cible" = selectInput(ns(paste0("adj_val_", i)), "Nouvelle population cible :",
                                                                   choices = c("Population totale", "Enfants de moins de 5 ans", "Enfants de moins de 5 ans et femmes enceintes", "Enfants de moins de 10 ans"), selected = "Enfants de moins de 5 ans et femmes enceintes"
               ),
               "Routine MII : couverture de la population cible" = sliderInput(ns(paste0("adj_val_", i)), "Nouvelle couverture (%) :", 0, 100, 30),
               "Routine MII : marge de moustiquaires (%)" = sliderInput(ns(paste0("adj_val_", i)), "Nouvelle marge (%) :", 0, 100, 10),

               # TPIp
               "TPIp : fréquentation CPN" = sliderInput(ns(paste0("adj_val_", i)), "Nouvelle couverture (%) :", 0, 100, 80),
               "TPIp : points de contact" = numericInput(ns(paste0("adj_val_", i)), "Nouveaux points de contact :", 3, min = 1),
               "TPIp : marge pour l’approvisionnement en médicaments" = sliderInput(ns(paste0("adj_val_", i)), "Nouvelle marge (%) :", 0, 100, 10),

               # CPS
               "CPS : population cible" = selectInput(ns(paste0("adj_val_", i)), "Nouvelle population cible :", choices = c("Enfants de 3 mois à 5 ans", "Enfants de 3 mois à 10 ans"), selected = "Enfants de 3 mois à 5 ans"),
               "CPS : couverture de la population cible" = sliderInput(ns(paste0("adj_val_", i)), "Nouvelle couverture (%) :", 0, 100, 100),
               "CPS : cycles" = numericInput(ns(paste0("adj_val_", i)), "Nouveaux cycles :", 4, min = 1),
               "CPS : ciblage par âge" = textInput(ns(paste0("adj_val_", i)), "Nouvelles proportions par âge :", "0.18,0.77"),
               "CPS : marge pour l’approvisionnement en médicaments" = sliderInput(ns(paste0("adj_val_", i)), "Nouvelle marge (%) :", 0, 100, 10),

               # CPP
               "CPP : couverture" = sliderInput(ns(paste0("adj_val_", i)), "Nouvelle couverture (%) :", 0, 100, 85),
               "CPP : points de contact" = numericInput(ns(paste0("adj_val_", i)), "Nouveaux points de contact :", 4, min = 1),
               "CPP : facteur de mise à l’échelle nutritionnelle" = sliderInput(ns(paste0("adj_val_", i)), "Nouveau facteur de mise à l’échelle nutritionnelle (%) :", 0, 100, 75),
               "CPP : marge pour l’approvisionnement en médicaments" = sliderInput(ns(paste0("adj_val_", i)), "Nouvelle marge (%) :", 0, 100, 10),

               # Vaccination
               "Vaccination : couverture" = sliderInput(ns(paste0("adj_val_", i)), "Nouvelle couverture (%) :", 0, 100, 84),
               "Vaccination : nombre de doses" = numericInput(ns(paste0("adj_val_", i)), "Nouveau nombre de doses :", 4, min = 1),
               "Vaccination : marge pour l’approvisionnement" = sliderInput(ns(paste0("adj_val_", i)), "Nouvelle marge (%) :", 0, 100, 10)
        )
      })
    })
  })

  # ----------------------------------------------------------------------------
  # ADD ADJUSTMENTS
  # ----------------------------------------------------------------------------
  observe({
    lapply(1:row_count(), function(i) {
      observeEvent(input[[paste0("submit_adjust_", i)]], {
        req(input[[paste0("row_param_", i)]], input[[paste0("adj_val_", i)]])
        current <- stored_selections()
        if (length(current$adjustments) < i || is.null(current$adjustments[[i]])) {
          current$adjustments[[i]] <- list()
        }

        param <- input[[paste0("row_param_", i)]]
        value <- input[[paste0("adj_val_", i)]]

        # Remove extra quotes if already present
        if (is.character(value)) {
          value <- gsub('"', "", value)
        }

        # Convert percent inputs to decimal
        percent_assumptions <- c(
          "Campagne MII : couverture de la population cible",
          "Routine MII : couverture de la population cible",
          "TPIp : fréquentation CPN",
          "CPS : couverture de la population cible",
          "CPP : couverture",
          "CPP : facteur de mise à l’échelle nutritionnelle",
          "Vaccination : couverture"
        )
        if (param %in% percent_assumptions) {
          value <- as.numeric(value) / 100
        }

        # Quote string values only
        if (is.character(value) && is.na(suppressWarnings(as.numeric(value)))) {
          value <- paste0('"', value, '"')
        }

        new_adj <- paste0(param, " = ", value)

        # Avoid duplicates
        if (!(new_adj %in% current$adjustments[[i]])) {
          current$adjustments[[i]] <- append(current$adjustments[[i]], list(new_adj))
          stored_selections(current)
        }
      })
    })
  })

  # ----------------------------------------------------------------------------
  # INLINE SUMMARY WITH INDIVIDUAL REMOVE LINKS
  # ----------------------------------------------------------------------------
  observe({
    lapply(1:row_count(), function(i) {
      output[[paste0("summary_inline_", i)]] <- renderUI({
        stored <- stored_selections()
        adj_list <- get_safe(stored$adjustments, i)
        assumption_type <- get_safe(stored$assumptions, i)

        if (!is.null(adj_list) && length(adj_list) > 0) {
          tagList(
            tags$b("Ajustements:"),
            tags$ul(
              lapply(seq_along(adj_list), function(j) {
                tags$li(
                  paste(adj_list[[j]]),
                  actionLink(ns(paste0("remove_adj_", i, "_", j)), icon("xmark", style = "color: #888;"), style = "margin-left:8px;"),
                  title = "réinitialiser la variable à la base de référence"
                )
              })
            )
          )
        } else if (assumption_type == "Accepter la base de référence") {
          div(
            style = "color: #666; font-style: italic;",
            "🛈 Aucun ajustement n’a été effectué — les hypothèses par défaut seront utilisées."
          )
        } else {
          NULL
        }
      })
    })
  })

  # ----------------------------------------------------------------------------
  # REMOVE INDIVIDUAL ADJUSTMENTS
  # ----------------------------------------------------------------------------
  observe({
    lapply(1:row_count(), function(i) {
      observe({
        adj_list <- get_safe(stored_selections()$adjustments, i)
        if (!is.null(adj_list)) {
          lapply(seq_along(adj_list), function(j) {
            observeEvent(input[[paste0("remove_adj_", i, "_", j)]],
              {
                current <- stored_selections()
                if (!is.null(current$adjustments[[i]]) && length(current$adjustments[[i]]) >= j) {
                  current$adjustments[[i]] <- current$adjustments[[i]][-j]
                  stored_selections(current)
                }
              },
              ignoreInit = TRUE
            )
          })
        }
      })
    })
  })

  # ----------------------------------------------------------------------------
  # ADD ROW BUTTON
  # ----------------------------------------------------------------------------
  observeEvent(input$add_row, {
    row_count(row_count() + 1)
  })

  # ----------------------------------------------------------------------------
  # REMOVE ROW BUTTON
  # ----------------------------------------------------------------------------
  observeEvent(input$remove_row, {
    req(row_count() > 1)
    row_count(row_count() - 1)
  })

  # ----------------------------------------------------------------------------
  # CLEAR ROW SELECTIONS
  # ----------------------------------------------------------------------------
  observe({
    lapply(1:row_count(), function(i) {
      observeEvent(input[[paste0("clear_row_", i)]], {
        current <- stored_selections()

        # Reset selections to NULL or default placeholders
        current$plans[[i]] <- "Sélectionnez un plan"
        current$costs[[i]] <- "Sélectionnez les coûts"
        current$assumptions[[i]] <- "Sélectionnez une hypothèse"
        current$adjustments[[i]] <- list()

        stored_selections(current)

        # Optionally also reset UI input values
        updateSelectInput(session, paste0("row_plan_", i), selected = "Sélectionnez un plan")
        updateSelectInput(session, paste0("row_cost_", i), selected = "Sélectionnez les coûts")
        updateSelectInput(session, paste0("row_assumption_", i), selected = "Sélectionnez une hypothèse")
      })
    })
  })

  # ----------------------------------------------------------------------------
  # DISABLE MATRIX UI WHEN GENERATING
  # ----------------------------------------------------------------------------
  observe({
    if (budget_generating()) {
      shinyjs::disable("matrix_container")
    } else {
      shinyjs::enable("matrix_container")
    }
  })



  # ----------------------------------------------------------------------------
  # GENERATE BUDGETS
  # ----------------------------------------------------------------------------
  observeEvent(input$generate_budgets, {
    # if lite version of tool turned on - set this message to show
    if (lite_mode) {
      showModal(modalDialog(
        title = "Fonctionnalité désactivée",
        "La génération de budget est désactivée dans cette version de démonstration de l'outil.
      Ce bouton est fourni à titre indicatif uniquement.",
        easyClose = TRUE
      ))
      return()
    }

    # ensure there is matrix data
    req(row_count() > 0)

    # Show loader
    budget_generating(TRUE)
    shinyjs::show("loading_container")

    on.exit(
      {
        budget_generating(FALSE)
        shinyjs::hide("loading_container")
      },
      add = TRUE
    )

    # Validate that all rows have valid selections
    invalid_rows <- vector()

    for (i in 1:row_count()) {
      plan <- input[[paste0("row_plan_", i)]]
      cost <- input[[paste0("row_cost_", i)]]
      assumption <- input[[paste0("row_assumption_", i)]]

      if (
        is.null(plan) || plan == "Sélectionnez un plan" ||
          is.null(cost) || cost == "Sélectionnez les coûts" ||
          is.null(assumption) || assumption == "Sélectionnez une hypothèse"
      ) {
        invalid_rows <- c(invalid_rows, i)
      }
    }

    # Save to reactiveVal
    incomplete_rows(invalid_rows)

    if (length(invalid_rows) > 0) {
      showModal(modalDialog(
        title = "Champs manquants",
        "Assurez-vous que toutes les sélections (plan, coûts, hypothèses) ont été complétées pour chaque ligne visible. Supprimez les lignes qui ne sont pas nécessaires.",
        easyClose = TRUE,
        footer = modalButton("Fermer")
      ))
      return()
    }

    # process matrix selections safely
    n_rows <- row_count() %||% 1
    if (!is.numeric(n_rows) || is.na(n_rows) || n_rows <= 0) {
      showModal(modalDialog("Aucune ligne valide n’est disponible pour générer un budget."))
      return()
    }

    matrix_data <- lapply(1:n_rows, function(i) {
      list(
        plan = input[[paste0("row_plan_", i)]],
        cost = input[[paste0("row_cost_", i)]],
        assumption_type = input[[paste0("row_assumption_", i)]],
        adjustments = get_safe(stored_selections()$adjustments, i) %||% list()
      )
    })

    matrix_selections(matrix_data)

    # Notify user and start progress bar
    showNotification("Début de la génération des budgets...", type = "message", duration = NULL)


    # Run through each budget generation process
    withProgress(message = "Génération en cours", detail = "Traitement des budgets...", value = 0, {
      results <- list()
      history_entries <- list()

      for (i in seq_along(matrix_data)) {
        incProgress(1 / length(matrix_data), detail = paste("Budget", i, "sur", length(matrix_data)))

        row <- matrix_data[[i]]
        scen <- scenario_data() |> filter(scenario_name == row$plan)
        cost <- cost_data() |> filter(cost_name == row$cost)

        # gnerate the budget
        tryCatch(
          {
            budget_df <- generate_budget(
              scen_data = scen,
              cost_data = cost,
              assumptions = row$adjustments
            )

            years <- sort(unique(budget_df$year))
            years_string <- paste(years, collapse = ", ")

            timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
            output_path <- paste0("generated/budget_", timestamp, "_", i, ".rds")
            budget_df$file_path <- output_path

            saveRDS(budget_df, output_path)

            # Add metadata for history
            entry <- data.frame(
              date_generated = format(Sys.time(), "%Y-%m-%d"),
              scenario = budget_df$scenario_name[1],
              cost = budget_df$cost_name[1],
              years = years_string,
              assumption_type = budget_df$assumption_type[1],
              assumptions_changes = budget_df$assumptions_changes[1],
              file_path = output_path,
              stringsAsFactors = FALSE
            )

            results[[i]] <- budget_df
            history_entries[[i]] <- entry
          },
          error = function(e) {
            showNotification(
              paste("Erreur lors de la génération du budget pour", row$plan, ":", e$message),
              type = "error",
              duration = NULL
            )
          }
        )
      }

      # Combine and save
      if (length(results) > 0) {
        combined_budgets <- bind_rows(results)
        new_history <- bind_rows(history_entries)

        budget_history(rbind(budget_history(), new_history))
        saveRDS(budget_history(), "generated/budget_history.rds")

        # Share for other tabs
        budget_results(combined_budgets)
        shared$budget_results <- combined_budgets
        shared$budget_results_available <- TRUE

        # Trigger reactive refresh
        shared$refresh_trigger <- shared$refresh_trigger + 1

        showNotification(
          paste(length(results), "budgets générés avec succès."),
          type = "message",
          duration = NULL
        )
      } else {
        showNotification("Aucun budget valide n’a été généré.", type = "warning")
      }
    })
  })


  # ----------------------------------------------------------------------------
  # RENDER BUDGET HISTORY TABLE
  # ----------------------------------------------------------------------------
  output$budget_history_table <- renderDT({
    df <- budget_history()
    df$date_generated <- format(as.Date(df$date_generated), "%Y-%m-%d")

    if (is.null(df) || nrow(df) == 0) {
      # Return an empty placeholder table with column names
      empty_df <- data.frame(
        date_generated = character(),
        scenario = character(),
        cost = character(),
        assumption_type = character(),
        assumptions_changes = character(),
        years = character(),
        actions = character(),
        stringsAsFactors = FALSE
      )

      return(datatable(
        empty_df,
        colnames = c(
          "Date", "Scénario", "Coût", "Type d'hypothèses",
          "Hypothèses modifiées", "Années", "Action"
        ),
        options = list(
          language = list(url = "//cdn.datatables.net/plug-ins/1.13.4/i18n/fr-FR.json")
        )
      ))
    }

    # Add delete button column
    df$actions <- paste0(
      '<button class="btn btn-warning btn-sm delete-budget-btn" data-path="',
      df$file_path, '" title="Supprimer ce budget">Supprimer</button>'
    )

    datatable(
      df[, c("date_generated", "scenario", "cost", "assumption_type", "assumptions_changes", "years", "actions")],
      colnames = c("Date", "Scénario", "Coût", "Type d'hypothèses", "Hypothèses modifiées", "Années", "Action"),
      escape = FALSE,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        columnDefs = list(list(targets = 6, orderable = FALSE)),
        language = list(url = "//cdn.datatables.net/plug-ins/1.13.4/i18n/fr-FR.json")
      ),
      callback = JS(
        sprintf(
          "table.on('click', '.delete-budget-btn', function() {
      var path = $(this).data('path');
      Shiny.setInputValue('%s', path, {priority: 'event'});
    });",
          ns("confirm_delete_path") # 👈 correctly namespace the input ID
        )
      )
    )
  })

  # ----------------------------------------------------------------------------
  # DELETE SELECTED BUDGET FILE WITH FEEDBACK
  # ----------------------------------------------------------------------------
  observeEvent(input$confirm_delete_path, {
    showModal(modalDialog(
      title = "Confirmer la suppression",
      "Voulez-vous vraiment supprimer ce budget ?",
      easyClose = FALSE,
      footer = tagList(
        modalButton("Annuler"),
        actionButton(ns("confirm_delete_yes"), "Oui, supprimer", class = "btn-warning")
      )
    ))
    shared$path_to_delete <- input$confirm_delete_path
  })

  # ----------------------------------------------------------------------------
  # FINAL DELETE
  # ----------------------------------------------------------------------------
  observeEvent(input$confirm_delete_yes, {
    removeModal()

    path <- shared$path_to_delete
    if (!file.exists(path)) {
      showNotification("❌ Fichier introuvable. Suppression échouée.", type = "error", duration = 5)
      return()
    }

    success <- tryCatch(
      {
        file.remove(path)
      },
      error = function(e) FALSE
    )

    if (isTRUE(success)) {
      # Remove from in-memory budget history and save
      hist <- budget_history()
      hist <- hist[hist$file_path != path, ]
      budget_history(hist)
      saveRDS(hist, "generated/budget_history.rds")

      # ✅ BONUS: Also remove from shared$budget_results
      if (!is.null(shared$budget_results)) {
        shared$budget_results <- shared$budget_results %>% filter(file_path != path)
      }

      # Update reactive flags
      shared$budget_results_available <- nrow(hist) > 0
      shared$refresh_trigger <- shared$refresh_trigger + 1

      showNotification("✅ Budget supprimé avec succès.", type = "message", duration = 4)
    } else {
      showNotification("❌ Échec de la suppression du fichier.", type = "error", duration = 5)
    }

    shared$path_to_delete <- NULL # Clear temp value
  })
}
