tab1aServer <- function(input, output, session,
                        template_file_path, SCENARIO_COLS, COST_COLS, TEMPLATE_ADMIN_DATA, shared) {
  # Add unified shared caches for scenario and cost uploads
  shared$scenario_uploads_cache <- reactiveVal(NULL)
  shared$cost_upload_cache <- reactiveVal(NULL)

  # Template file path check
  observe({
    if (!file.exists(template_file_path)) {
      warning(paste("Template file not found:", template_file_path))
    }
  })

  # Initialize reactive values
  refresh_trigger <- reactive({
    shared$refresh_trigger
  })

  # Helper functions
  calculate_file_hash <- function(file_path) {
    tryCatch(
      {
        sheets <- excel_sheets(file_path)
        content <- lapply(sheets, function(sheet) {
          data <- read_excel(file_path, sheet = sheet)
          paste(capture.output(data), collapse = "")
        })
        digest::digest(paste(content, collapse = ""), algo = "md5")
      },
      error = function(e) {
        NULL
      }
    )
  }

  # Function to check for duplicate files
  is_duplicate_file <- function(hash, type = "scenario") {
    if (is.null(hash)) {
      return(list(is_duplicate = FALSE))
    }
    db_file <- paste0(type, "_uploads.db")
    db <- dbConnect(SQLite(), db_file)
    on.exit(dbDisconnect(db))

    query <- "SELECT name FROM uploads WHERE file_hash = ? LIMIT 1"
    result <- dbGetQuery(db, query, list(hash))

    list(
      is_duplicate = nrow(result) > 0,
      existing_name = if (nrow(result) > 0) result$name[1] else NULL
    )
  }

  # Load all uploaded scenario metadata (from DB)
  load_all_uploaded_scenarios <- function() {
    db <- DBI::dbConnect(RSQLite::SQLite(), "scenario_uploads.db")
    uploads <- DBI::dbGetQuery(db, "SELECT * FROM uploads ORDER BY upload_date DESC")
    DBI::dbDisconnect(db)
    uploads
  }

  # Load the latest uploaded cost Excel file as a dataframe
  load_latest_uploaded_cost <- function() {
    db <- DBI::dbConnect(RSQLite::SQLite(), "cost_uploads.db")
    uploads <- DBI::dbGetQuery(db, "SELECT * FROM uploads ORDER BY upload_date DESC LIMIT 1")
    DBI::dbDisconnect(db)

    if (nrow(uploads) > 0) {
      file_path <- file.path("uploads/costs", uploads$filename[1])
      if (file.exists(file_path)) {
        readxl::read_excel(file_path)
      } else {
        NULL
      }
    } else {
      NULL
    }
  }


  # Scenario template download handler
  output$download_scenario_template <- downloadHandler(
    filename = function() {
      "scenario_template.xlsx"
    },
    content = function(file) {
      tryCatch(
        {
          message("Starting template download...")
          message(paste("Template path:", template_file_path))

          # Use openxlsx instead of openxlsx2
          wb <- openxlsx::loadWorkbook(template_file_path)

          # Get the name of all sheets
          original_sheets <- openxlsx::sheets(wb)
          template_sheet <- original_sheets[1]

          # Save reference to list-options-ignore sheet if it exists
          options_sheet_exists <- "list-options-ignore" %in% original_sheets

          years_to_use <- if (length(input$year_filter) > 0) input$year_filter else DEFAULT_YEARS
          message("Years to use: ", paste(years_to_use, collapse = ", "))

          # Clone the sheet for each selected year
          for (year in years_to_use) {
            year_str <- as.character(year)
            message("Creating sheet for year: ", year_str)

            # Clone the template sheet with the year as the name
            openxlsx::cloneWorksheet(wb, year_str, clonedSheet = template_sheet)

            # Verify the sheet was created
            if (year_str %in% openxlsx::sheets(wb)) {
              message("Successfully created sheet: ", year_str)
            } else {
              message("Failed to create sheet: ", year_str)
            }
          }

          # Remove the template sheet
          if (template_sheet %in% openxlsx::sheets(wb)) {
            message("Removing template sheet")
            openxlsx::removeWorksheet(wb, template_sheet)
          }

          # Get current sheets after removing template
          current_sheets <- openxlsx::sheets(wb)

          # Order we want: year sheets first, then list-options-ignore
          desired_order <- c()

          # Add year sheets in order
          for (year in years_to_use) {
            year_str <- as.character(year)
            if (year_str %in% current_sheets) {
              desired_order <- c(year_str)
            }
          }

          # Add list-options-ignore at the end if it exists
          if (options_sheet_exists) {
            desired_order <- c(desired_order, "liste-options-ignorer")
          }

          # Reorder the sheets using the setSheetOrder function
          # Reorder the sheets using the setSheetOrder function
          valid_order <- desired_order[desired_order %in% current_sheets]
          if (length(valid_order) == length(current_sheets)) {
            message("Setting sheet order: ", paste(valid_order, collapse = ", "))
            openxlsx::worksheetOrder(wb) <- match(valid_order, current_sheets)
          } else {
            message("⚠️ Could not set exact sheet order. Current sheets: ", paste(current_sheets, collapse = ", "))
          }

          # Save the workbook
          openxlsx::saveWorkbook(wb, file, overwrite = TRUE)

          # Verify final order
          final_sheets <- openxlsx::sheets(wb)
          message("Final sheet order: ", paste(final_sheets, collapse = ", "))
          message("Workbook saved successfully with ", length(final_sheets), " sheets")
        },
        error = function(e) {
          message(paste("Erreur:", e$message))
          showModal(modalDialog(
            title = "Erreur",
            sprintf("Failed to generate template: %s", e$message),
            easyClose = TRUE
          ))
        }
      )
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )


  # UI for scenario downloads
  output$scenario_download_ui <- renderUI({
    # Get list of files
    scenario_files <- list.files("uploads/scenarios", pattern = "\\.xlsx$")

    if (length(scenario_files) == 0) {
      return(p("Aucun scénario téléchargé disponible"))
    }

    # Remove .xlsx extension for display
    choices <- setNames(
      scenario_files,
      tools::file_path_sans_ext(scenario_files)
    )

    div(
      selectInput(
        session$ns("scenario_to_download"),
        "Sélectionnez un scénario à télécharger:",
        choices = choices,
        width = "100%"
      ),
      downloadButton(
        session$ns("download_selected_scenario"),
        "Télécharger le scénario sélectionné",
        class = "btn-primary"
      )
    )
  })

  # Download handler for selected scenario
  output$download_selected_scenario <- downloadHandler(
    filename = function() {
      input$scenario_to_download # Return the original filename
    },
    content = function(file) {
      file.copy(
        file.path("uploads/scenarios", input$scenario_to_download),
        file
      )
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )

  # Make sure the dropdown updates when new files are uploaded
  observeEvent(refresh_trigger(), {
    # Trigger a rebuild of the UI
    output$scenario_download_ui <- renderUI({
      # Get updated list of files
      scenario_files <- list.files("uploads/scenarios", pattern = "\\.xlsx$")

      if (length(scenario_files) == 0) {
        return(p("No uploaded scenarios available"))
      }

      # Remove .xlsx extension for display
      choices <- setNames(
        scenario_files,
        tools::file_path_sans_ext(scenario_files)
      )

      div(
        selectInput(
          session$ns("scenario_to_download"),
          "Sélectionnez un scénario à télécharger:",
          choices = choices,
          width = "100%"
        ),
        downloadButton(
          session$ns("download_selected_scenario"),
          "Télécharger le scénario sélectionné",
          class = "btn-primary"
        )
      )
    })
  })

  # Cost template download handler
  output$download_cost_template <- downloadHandler(
    filename = function() {
      "cost_template.xlsx"
    },
    content = function(file) {
      tryCatch(
        {
          print("Starting cost template download...")
          print(paste("Template path:", cost_template_file_path))

          # Load the original cost template with dropdowns
          wb <- openxlsx2::wb_load(cost_template_file_path)

          # Save workbook to user download
          openxlsx2::wb_save(wb, file)
          print("Saved cost workbook with dropdowns preserved")
        },
        error = function(e) {
          print(paste("Erreur:", e$message))
          showModal(modalDialog(
            title = "Erreur",
            sprintf("Failed to generate cost template: %s", e$message),
            easyClose = TRUE
          ))
        }
      )
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )

  # UI for cost downloads
  output$cost_download_ui <- renderUI({
    # Get list of files
    cost_files <- list.files("uploads/costs", pattern = "\\.xlsx$")

    if (length(cost_files) == 0) {
      return(p("Aucun fichier de coûts téléchargé disponible"))
    }

    # Remove .xlsx extension for display
    choices <- setNames(
      cost_files,
      tools::file_path_sans_ext(cost_files)
    )

    div(
      selectInput(
        session$ns("cost_to_download"),
        "Sélectionnez un fichier de coûts à télécharger:",
        choices = choices,
        width = "100%"
      ),
      downloadButton(
        session$ns("download_selected_cost"),
        "Télécharger le fichier de coûts sélectionné",
        class = "btn-primary"
      )
    )
  })

  # Download handler for selected cost file
  output$download_selected_cost <- downloadHandler(
    filename = function() {
      input$cost_to_download # Return the original filename
    },
    content = function(file) {
      file.copy(
        file.path("uploads/costs", input$cost_to_download),
        file
      )
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )

  # Make sure the dropdown updates when new files are uploaded
  observeEvent(refresh_trigger(), {
    # Trigger a rebuild of the UI
    output$cost_download_ui <- renderUI({
      # Get updated list of files
      cost_files <- list.files("uploads/costs", pattern = "\\.xlsx$")

      if (length(cost_files) == 0) {
        return(p("No uploaded cost files available"))
      }

      # Remove .xlsx extension for display
      choices <- setNames(
        cost_files,
        tools::file_path_sans_ext(cost_files)
      )

      div(
        selectInput(
          session$ns("cost_to_download"),
          "Sélectionnez un fichier de coûts à télécharger:",
          choices = choices,
          width = "100%"
        ),
        downloadButton(
          session$ns("download_selected_cost"),
          "Télécharger le fichier de coûts sélectionné",
          class = "btn-primary"
        )
      )
    })
  })

  # Scenario upload handler
  observeEvent(input$submit_scenario, {
    if (lite_mode) {
      showModal(modalDialog(
        title = "Feature Disabled",
        "Scenario uploading is disabled in this demonstration version.",
        easyClose = TRUE
      ))
      return()
    }

    tryCatch({
      all_sheet_names <- excel_sheets(input$scenario_file$datapath)

      # Keep only sheets with numeric names (i.e. years) for the validation as we have
      # the list data in here too now
      sheet_names <- all_sheet_names[grepl("^\\d{4}$", all_sheet_names)]

      # Validate each sheet
      all_sheets_valid <- TRUE

      for (sheet in sheet_names) {
        current_data <- read_excel(input$scenario_file$datapath, sheet = sheet)

        # Check column names
        if (!identical(colnames(current_data), SCENARIO_COLS)) {
          all_sheets_valid <- FALSE
          removeModal()
          showModal(modalDialog(
            title = "Erreur",
            HTML(sprintf(
              "Échec du téléchargement: noms des colonnes dans la feuille '%s' ne correspond pas au modèle.<br><br>
                 Attendue: %s<br>
                 Trouvée: %s",
              sheet,
              paste(SCENARIO_COLS, collapse = ", "),
              paste(colnames(current_data), collapse = ", ")
            )),
            easyClose = TRUE
          ))
          return()
        }

        # Check admin columns
        admin_cols <- grep("^adm", SCENARIO_COLS, value = TRUE)
        for (col in admin_cols) {
          if (!identical(
            sort(unique(current_data[[col]])),
            sort(unique(TEMPLATE_ADMIN_DATA[[col]]))
          )) {
            all_sheets_valid <- FALSE
            removeModal()
            showModal(modalDialog(
              title = "Erreur",
              HTML(sprintf(
                "Échec du téléchargement: valeurs dans la colonne administrative '%s' doit correspondre exactement au modèle.<br><br>
                   Attendue: %s<br>
                   Trouvée: %s",
                col,
                paste(sort(unique(TEMPLATE_ADMIN_DATA[[col]])), collapse = ", "),
                paste(sort(unique(current_data[[col]])), collapse = ", ")
              )),
              easyClose = TRUE
            ))
            return()
          }
        }

        # Check code columns
        code_cols <- grep("^code", SCENARIO_COLS, value = TRUE)
        for (col in code_cols) {
          invalid_values <- current_data[[col]][!is.na(current_data[[col]]) &
            !current_data[[col]] %in% c(0, 1)]
          if (length(invalid_values) > 0) {
            all_sheets_valid <- FALSE
            removeModal()
            showModal(modalDialog(
              title = "Erreur",
              HTML(sprintf(
                "Échec du téléchargement: la colonne '%s' ne peut contenir que les valeurs 0, 1 ou NA.<br><br>
                   Valeurs non valides trouvées: %s",
                col,
                paste(unique(invalid_values), collapse = ", ")
              )),
              easyClose = TRUE
            ))
            return()
          }
        }
      }

      if (!all_sheets_valid) {
        return()
      }

      # Create safe filename from scenario name
      safe_name <- gsub("[^[:alnum:]]", "_", input$scenario_name)
      new_filename <- paste0(safe_name, ".xlsx")
      file_path <- file.path("uploads/scenarios", new_filename)

      # Check if file already exists and append number if needed
      base_name <- safe_name
      counter <- 1
      while (file.exists(file_path)) {
        safe_name <- paste0(base_name, "_", counter)
        new_filename <- paste0(safe_name, ".xlsx")
        file_path <- file.path("uploads/scenarios", new_filename)
        counter <- counter + 1
      }

      # Calculate file hash and check for duplicates
      file_hash <- calculate_file_hash(input$scenario_file$datapath)
      duplicate_check <- is_duplicate_file(file_hash, "scenario")

      if (duplicate_check$is_duplicate) {
        removeModal()
        showModal(modalDialog(
          title = "Erreur",
          HTML(sprintf(
            "Échec du téléchargement: ce fichier est identique à un téléchargement précédent nommé '<strong>%s</strong>'.",
            duplicate_check$existing_name
          )),
          easyClose = TRUE
        ))
        return()
      }

      # Only copy the file after all validations are successful
      file.copy(input$scenario_file$datapath, file_path)

      # Save to database
      db <- dbConnect(SQLite(), "scenario_uploads.db")
      dbExecute(db,
        "INSERT INTO uploads (id, name, description, filename, file_hash, years, upload_date) VALUES (?, ?, ?, ?, ?, ?, datetime('now'))",
        params = list(
          safe_name,
          input$scenario_name,
          ifelse(is.null(input$scenario_description), "", input$scenario_description),
          new_filename,
          file_hash,
          paste(sheet_names, collapse = ",")
        )
      )
      dbDisconnect(db)

      # Reset inputs
      updateTextInput(session, session$ns("scenario_name"), value = "")
      updateTextAreaInput(session, session$ns("scenario_description"), value = "")

      shared$refresh_trigger <- shared$refresh_trigger + 1

      shared$scenario_uploads_cache(load_all_uploaded_scenarios())

      removeModal()
      showModal(modalDialog(
        title = "Succès",
        "Fichier de scénario téléchargé avec succès!",
        easyClose = TRUE,
        footer = tagList(
          modalButton("fermer"),
          tags$script("setTimeout(function() { resetScroll(); }, 100);")
        )
      ))
    }, error = function(e) {
      removeModal()
      showModal(modalDialog(
        title = "Erreur",
        sprintf("Échec du traitement du fichier Excel: %s", e$message),
        easyClose = TRUE
      ))
    }, finally = {
      removeModal()
    })
  })

  # Cost upload
  observeEvent(input$submit_cost, {
    if (lite_mode) {
      showModal(modalDialog(
        title = "Feature Disabled",
        "Cost uploading is disabled in this demonstration version.",
        easyClose = TRUE
      ))
      return()
    }

    tryCatch({
      current_data <- read_excel(input$cost_file$datapath)

      # Check column names
      if (!identical(colnames(current_data), COST_COLS)) {
        removeModal()
        showModal(modalDialog(
          title = "Erreur",
          HTML(sprintf(
            "Échec du téléchargement: les noms de colonnes ne correspondent pas au modèle.<br><br>
             Attendue: %s<br>
             Trouvée: %s",
            paste(COST_COLS, collapse = ", "),
            paste(colnames(current_data), collapse = ", ")
          )),
          easyClose = TRUE
        ))
        return()
      }

      # Create safe filename from cost name
      safe_name <- gsub("[^[:alnum:]]", "_", input$cost_name)
      new_filename <- paste0(safe_name, ".xlsx")
      file_path <- file.path("uploads/costs", new_filename)

      # Check if file already exists and append number if needed
      base_name <- safe_name
      counter <- 1
      while (file.exists(file_path)) {
        safe_name <- paste0(base_name, "_", counter)
        new_filename <- paste0(safe_name, ".xlsx")
        file_path <- file.path("uploads/costs", new_filename)
        counter <- counter + 1
      }

      # Calculate file hash and check for duplicates
      file_hash <- calculate_file_hash(input$cost_file$datapath)
      duplicate_check <- is_duplicate_file(file_hash, "cost")

      if (duplicate_check$is_duplicate) {
        removeModal()
        showModal(modalDialog(
          title = "Erreur",
          HTML(sprintf(
            "Échec du téléchargement: ce fichier est identique à un téléchargement précédent nommé '<strong>%s</strong>'.",
            duplicate_check$existing_name
          )),
          easyClose = TRUE
        ))
        return()
      }

      # Only copy the file after all validations are successful
      file.copy(input$cost_file$datapath, file_path)

      # Save to database
      db <- dbConnect(SQLite(), "cost_uploads.db")
      tryCatch({
        dbExecute(db,
          "INSERT INTO uploads (id, name, description, filename, file_hash, upload_date) VALUES (?, ?, ?, ?, ?, datetime('now'))",
          params = list(
            safe_name,
            input$cost_name,
            ifelse(is.null(input$cost_description), "", input$cost_description),
            new_filename,
            file_hash
          )
        )
      }, finally = {
        dbDisconnect(db)
      })

      # Reset inputs
      updateTextInput(session, session$ns("cost_name"), value = "")
      updateTextAreaInput(session, session$ns("cost_description"), value = "")

      shared$refresh_trigger <- shared$refresh_trigger + 1
      shared$cost_upload_cache(load_all_uploaded_cost())

      removeModal()
      showModal(modalDialog(
        title = "Succès",
        "Fichier de coûts téléchargé avec succès!",
        easyClose = TRUE,
        footer = tagList(
          modalButton("Fermer"),
          tags$script("setTimeout(function() { resetScroll(); }, 100);")
        )
      ))
    }, error = function(e) {
      removeModal()
      showModal(modalDialog(
        title = "Erreur",
        sprintf("Échec du traitement du fichier Excel: %s", e$message),
        easyClose = TRUE
      ))
    }, finally = {
      removeModal()
    })
  })

  # Render tables for both Scenario and Cost uploads
  output$scenario_uploads_table <- renderDT({
    refresh_trigger()

    db <- dbConnect(SQLite(), "scenario_uploads.db")
    uploads <- dbGetQuery(db, "
    SELECT id, name, description, filename, years, upload_date
    FROM uploads
    ORDER BY upload_date DESC
  ")
    dbDisconnect(db)

    if (nrow(uploads) > 0) {
      # Add a delete button column with unique IDs for each button
      uploads$actions <- paste0(
        '<button class="btn btn-warning btn-sm delete-btn" data-id="', uploads$id, '">Supprimer</button>'
      )

      datatable(
        uploads[, c("name", "description", "years", "upload_date", "actions")],
        options = list(
          pageLength = 5,
          scrollY = "200px",
          scrollCollapse = TRUE,
          columnDefs = list(
            list(targets = 4, orderable = FALSE) # Make the actions column not sortable
          ),
          language = list(
            url = "https://cdn.datatables.net/plug-ins/1.13.4/i18n/fr-FR.json"
          )
        ),
        colnames = c("Nom", "Description", "Années", "Date de téléchargement", "Actes"),
        selection = "none",
        escape = FALSE, # Important! Allows HTML in the table
        callback = JS("
        table.on('click', '.delete-btn', function() {
          var id = $(this).data('id');
          if (confirm('Êtes-vous sûr de vouloir supprimer définitivement ce scénario?')) {
            Shiny.setInputValue('tab1a-delete_scenario', id, {priority: 'event'});
          }
        });
      ")
      ) %>%
        formatDate("upload_date")
    } else {
      datatable(
        data.frame(
          Name = character(),
          Description = character(),
          Years = character(),
          "Date de téléchargement" = character(),
          Actions = character()
        ),
        options = list(
          pageLength = 5,
          scrollY = "200px",
          scrollCollapse = TRUE
        ),
        selection = "none",
        escape = FALSE
      )
    }
  })


  # Render table for cost uploads
  output$cost_uploads_table <- renderDT({
    refresh_trigger()

    db <- dbConnect(SQLite(), "cost_uploads.db")
    uploads <- dbGetQuery(db, "
    SELECT id, name, description, filename, upload_date
    FROM uploads
    ORDER BY upload_date DESC
  ")
    dbDisconnect(db)

    if (nrow(uploads) > 0) {
      # Add delete button
      uploads$actions <- paste0(
        '<button class="btn btn-warning btn-sm delete-cost-btn" data-id="', uploads$id, '">Supprimer</button>'
      )

      datatable(
        uploads[, c("name", "description", "upload_date", "actions")],
        options = list(
          pageLength = 5,
          scrollY = "200px",
          scrollCollapse = TRUE,
          columnDefs = list(
            list(targets = 3, orderable = FALSE) # 'Actions' column not sortable
          ),
          language = list(
            url = "https://cdn.datatables.net/plug-ins/1.13.4/i18n/fr-FR.json"
          )
        ),
        colnames = c("Nom", "Description", "Date de téléchargement", "Actes"),
        selection = "none",
        escape = FALSE,
        callback = JS("
        table.on('click', '.delete-cost-btn', function() {
          var id = $(this).data('id');
          if (confirm('Êtes-vous sûr de vouloir supprimer définitivement ce fichier de coûts? Cette action est irréversible.')) {
            Shiny.setInputValue('tab1a-delete_cost', id, {priority: 'event'});
          }
        });
      ")
      ) %>%
        formatDate("upload_date")
    } else {
      datatable(
        data.frame(
          Name = character(),
          Description = character(),
          "Date de téléchargement" = character(),
          Actions = character()
        ),
        options = list(
          pageLength = 5,
          scrollY = "200px",
          scrollCollapse = TRUE
        ),
        selection = "none",
        escape = FALSE
      )
    }
  })

  # Observe event delete data
  observeEvent(input$delete_scenario, {
    if (lite_mode) {
      showModal(modalDialog(
        title = "Feature Disabled",
        "Deleting scenarios is disabled in this demonstration version.",
        easyClose = TRUE
      ))
      return()
    }

    req(input$delete_scenario)

    print(paste("Triggered delete for scenario ID:", input$delete_scenario))

    scenario_id <- input$delete_scenario

    # Connect to DB and get the filename
    db <- dbConnect(SQLite(), "scenario_uploads.db")
    file_info <- dbGetQuery(db, "SELECT filename FROM uploads WHERE id = ?", list(scenario_id))

    if (nrow(file_info) > 0) {
      file_path <- file.path("uploads/scenarios", file_info$filename[1])

      # Delete the actual file
      if (file.exists(file_path)) {
        file.remove(file_path)
      }

      # Remove from the DB
      dbExecute(db, "DELETE FROM uploads WHERE id = ?", list(scenario_id))

      showModal(modalDialog(
        title = "Supprimé",
        "Le scénario sélectionné a été supprimé.",
        easyClose = TRUE
      ))

      # Trigger UI refresh
      shared$refresh_trigger <- shared$refresh_trigger + 1
    } else {
      showModal(modalDialog(
        title = "Erreur",
        "Scénario non trouvé ou déjà supprimé.",
        easyClose = TRUE
      ))
    }

    dbDisconnect(db)
  })

  # Delete Cost action
  observeEvent(input$delete_cost, {
    req(input$delete_cost)
    if (lite_mode) {
      showModal(modalDialog(
        title = "Feature Disabled",
        "Deleting cost sheets is disabled in this demonstration version.",
        easyClose = TRUE
      ))
      return()
    }
    cost_id <- input$delete_cost

    db <- dbConnect(SQLite(), "cost_uploads.db")
    file_info <- dbGetQuery(db, "SELECT filename FROM uploads WHERE id = ?", list(cost_id))

    if (nrow(file_info) == 1) {
      file_path <- file.path("uploads/costs", file_info$filename)

      if (file.exists(file_path)) {
        file.remove(file_path)
      }

      dbExecute(db, "DELETE FROM uploads WHERE id = ?", list(cost_id))

      showModal(modalDialog(
        title = "Supprimé",
        "Le fichier de coûts sélectionné a été définitivement supprimé.",
        easyClose = TRUE
      ))

      shared$refresh_trigger <- shared$refresh_trigger + 1
    } else {
      showModal(modalDialog(
        title = "Erreur",
        "Fichier de coûts non trouvé ou déjà supprimé.",
        easyClose = TRUE
      ))
    }

    dbDisconnect(db)
  })

  # Adding instructions pop up
  observeEvent(input$show_instructions, {
    showModal(modalDialog(
      title = "Instructions détaillées pour le téléchargement de modèles",
      easyClose = TRUE,
      size = "l",
      footer = modalButton("Fermer"),
      tagList(
        p("Cette section permet à l'utilisateur de télécharger des plans d'intervention contre le paludisme à des fins de budgétisation. Chaque nouvelle feuille de calcul représente un plan pour des années et des lieux spécifiques, détaillant les interventions prévues."),
        p("L'outil est conçu pour un contexte national spécifique et comprend les données prédéfinies suivantes nécessaires aux calculs budgétaires :"),
        tags$ul(
          tags$li("Limites administratives et noms"),
          tags$li("Masse continentale en km2"),
          tags$li("Indicateur du statut urbain de la plus petite unité spatiale utilisée pour la planification des interventions."),
          tags$li("Données sur la population cible"),
          tags$li("Nombre d'établissements de santé par type (primaire, secondaire, tertiaire) par unité spatiale"),
          tags$li("Données sur la prévalence des parasites chez les moins de 5 ans issues des enquêtes DHS"),
          tags$li("Données historiques sur la couverture des interventions")
        ),
        tags$b("Étapes d'utilisation – Aucune donnée précédente téléchargée :"),
        p("Suivez ces étapes si vous avez déjà utilisé l’outil – consultez la capture d’écran référencée pour vous référer entre les étapes."),
        tags$ol(
          tags$li("📆️ Sélectionnez les années de planification pour définir la portée de votre scénario"),
          tags$li("📄 Cliquez sur « Télécharger le modèle de scénario vide »"),
          tags$li("📊 Remplissez le modèle Excel : chaque feuille correspond à une année et chaque ligne représente la plus petite unité spatiale utilisée pour la planification des interventions. Chaque colonne détaille un type spécifique d'intervention contre le paludisme, précédé du mot « code_ ». Saisissez 1 pour les interventions prévues et 0 dans les autres cas. Dans la colonne « type_ » correspondante, sélectionnez le type d'intervention prévu pour chaque intervention. Par exemple, dans la colonne « type_vaccin », sélectionnez « R21 » ou « RTSS ». Si le type d'intervention n'est pas inclus dans la liste déroulante, vous pouvez le remplacer par du texte libre."),
          tags$li("💾 Une fois qu'un plan a été spécifié en indiquant quelles interventions doivent être ciblées, où chaque année l'utilisateur peut enregistrer une copie locale de ce fichier."),
          tags$li("📤 Revenez à l'application Web et téléchargez le fichier Excel complété à l'aide du formulaire, puis donnez au scénario un nom court : par exemple Plan A/Plan 1, etc. et une description : par exemple « Plan entièrement déployé »/« Plan de déploiement restreint du vaccin » - assurez-vous qu'il s'agit de descriptions informatives, car elles seront utiles lors de la comparaison des plans."),
          tags$li("📂 Appuyez sur le bouton « Soumettre le scénario » et la feuille de calcul sera téléchargée dans l’outil."),
          tags$li("📋 Les plans téléchargés apparaîtront dans un tableau récapitulatif avec des détails tels que le nom du plan, les années couvertes et la date de téléchargement."),
          tags$li("💵 Nous devons maintenant également spécifier les coûts unitaires qui seront essentiels pour que l’outil génère nos budgets."),
          tags$li("📄 Cliquez sur « Télécharger le modèle de coût vide »"),
          tags$li("📊 Remplissez le modèle Excel : Le modèle de coût unitaire permet à l'utilisateur d'avoir la flexibilité de spécifier les coûts unitaires exacts pertinents au contexte du pays. Le modèle comporte les colonnes suivantes :"),
          tags$ul(
            tags$li("resource_name : description longue des données de coût unitaire, par exemple « Coût d'achat du vaccin antipaludique par dose »"),
            tags$li("code_intervention : sélectionnez dans la liste déroulante le type d'intervention auquel le coût unitaire est lié, par exemple vaccin/campagne_itn/smc"),
            tags$li("type_intervention : sélectionnez dans la liste déroulante le type spécifique de coût unitaire d'intervention auquel se rapporte, par exemple r21/rtss/pyrethroid_net/PBO net"),
            tags$li("cost_class : sélectionnez dans la liste déroulante la classe de coût : par exemple, approvisionnement/distribution/opérationnel/support"),
            tags$li("unité : précisez l'unité exacte, par exemple par dose, par MII, par enfant, etc."),
            tags$li("local_currency_cost : coût spécifique en monnaie locale"),
            tags$li("usd_cost : Coût spécifique en USD"),
            tags$li("cost_year : année à laquelle le coût se rapporte"),
            tags$li("source : référencez la source de coût utilisée pour générer les données de coût unitaire")
          ),
          tags$li("Assurez-vous que pour chaque intervention réalisée, il existe des coûts unitaires pour l’intervention spécifique et le type d’intervention."),
          tags$li("💾 Une fois les données de coût unitaire spécifiées, l'utilisateur peut enregistrer une copie locale de ce fichier."),
          tags$li("📤 Revenez à l'application Web et téléchargez le fichier Excel complété à l'aide du formulaire et donnez un nom à la feuille de coûts : par exemple « Hypothèses de coûts 1 », etc. et une description : par exemple « Y compris une estimation de coût plus élevée de l'approvisionnement pour les filets PBO » - assurez-vous qu'il s'agit de descriptions informatives car elles seront utiles lors de la génération et de la comparaison des plans."),
          tags$li("📂 Appuyez sur le bouton « Soumettre la feuille de coûts » et la feuille de calcul sera téléchargée dans l’outil."),
          tags$li("📋 Les données de coût téléchargées apparaîtront dans un tableau récapitulatif avec des détails tels que le nom du coût, la description et la date de téléchargement.")
        ),
        tags$b("Étapes d’utilisation – Données précédentes téléchargées :"),
        p("Une fois qu'une feuille de calcul a été téléchargée dans l'outil, l'utilisateur est capable de télécharger un modèle basé sur un scénario spécifique téléchargé - cela peut faciliter le remplissage rapide d'un nouveau scénario sans avoir à répliquer chaque élément, mais assurez-vous de saisir un nouveau nom de scénario et une nouvelle description lors du nouveau téléchargement."),
        p("Une fois les deux ensembles de données téléchargés, l’utilisateur peut passer à l’étape suivante de l’application.")
      )
    ))
  })

  # # returning the data as a reactive list for future data processing
  # return(
  #   list(
  #     uploaded_scenarios = reactive({
  #       db <- DBI::dbConnect(RSQLite::SQLite(), "scenario_uploads.db")
  #       df <- dbReadTable(db, "uploads")
  #       dbDisconnect(db)
  #       df
  #     }),
  #     uploaded_costs = reactive({
  #       db <- DBI::dbConnect(RSQLite::SQLite(), "cost_uploads.db")
  #       df <- dbReadTable(db, "uploads")
  #       dbDisconnect(db)
  #       df
  #     })
  #
  #   )
  # )
}
