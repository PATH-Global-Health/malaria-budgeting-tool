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
        p("Cette section permet à l'utilisateur de spécifier des plans opérationnels de lutte contre le paludisme pour la budgétisation d'années et de lieux spécifiques, en détaillant les interventions qui doivent être mises en œuvre."),
        tags$b("1. Définir la combinaison d'intervention:"),
        p("Sélectionnez Années de planification pour définir l'étendue de votre scénario."),
        p("Cliquez sur « Télécharger le modèle de scénario vide »."),
        p("Remplissez le modèle Excel: "),
        tags$ul(
          tags$li("Chaque feuille correspond à une année qui a été spécifiée dans l'outil. "),
          tags$li("Chaque ligne représente la plus petite unité spatiale utilisée pour la planification de l'intervention (niveau de la zone de santé) avec les données adm0, amd1 et adm2 préspécifiées pour le pays d'intérêt (RDC)."),
          tags$li("Les colonnes « code_ » détaillent un type spécifique d'intervention antipaludique qui peut être dispensée, comme suit : 1 = Oui en cours de livraison OU 0/Blanc = Non non livré."),
          tags$li("Les colonnes « type_ » comportent des listes déroulantes permettant de sélectionner le type d'intervention spécifique délivré ")
        ),
        # Add image here
        div(
          style = "text-align: center; margin-top: 20px;",
          tags$a(
            href = "scenario-template-image.png", # path relative to www/
            target = "_blank", # open in new tab
            tags$img(
              src = "scenario-template-image.png",
              style = "max-width: 100%; height: auto; border: 1px solid #ccc; cursor: zoom-in;",
              alt = "Exemple de modèle de scénario"
            )
          ),
          tags$div(
            style = "font-style: italic; font-size: 90%; margin-top: 5px;",
            "Cliquez sur l'image pour l'agrandir"
          )
        ),
        p("Une fois qu'un plan a été spécifié en indiquant les interventions à cibler, où chaque année l'utilisateur peut sauvegarder une copie locale de ce fichier."),
        p("Revenez à l'application Web et téléchargez le fichier Excel complété à l'aide du bouton Télécharger."),
        p("Donnez au scénario un nom abrégé : par exemple Plan 1 BAU et une description : par exemple « Interventions simples - campagnes de masse, distribution de routine et CPP minimal » – assurez-vous qu'il s'agit de descriptions informatives, car elles seront utiles lors de la comparaison des plans."),
        p("Appuyez sur le bouton « Soumettre le scénario » et la feuille de calcul sera téléchargée dans l'outil. Les plans téléchargés apparaîtront dans un tableau récapitulatif avec les détails associés."),
        tags$b("2. Définition des coûts unitaires:"),
        p("Cliquez sur « Télécharger le modèle de coût vide »."),
        p("Assurez-vous que les en-têtes de colonne des colonnes A : J restent inchangés et que des colonnes supplémentaires peuvent être ajoutées selon les besoins de l'utilisateur."),
        p("Des feuilles supplémentaires pour le suivi des calculs de coûts unitaires, par exemple, peuvent également être ajoutées librement."),
        p("Les cellules ne contiennent pas de formules prédéfinies et c'est à l'utilisateur de saisir ou de calculer les données comme il l'entend."),
        p("Le modèle est prérempli avec certaines interventions, types d'intervention, classes de coûts et unités courants – ces lignes peuvent être modifiées et/ou supprimées selon les besoins de l'utilisateur, mais assurez-vous que pour chaque intervention réalisée dans le plan opérationnel, il y a des coûts unitaires pour l'intervention spécifique et le type d'intervention."),
        p("S'il le souhaite, l'utilisateur peut également ajouter des coûts unitaires spécifiques à l'emplacement. Pour ce faire, ajoutez des colonnes supplémentaires pour les spécifications 'adm1' et adm2' et assurez-vous que les noms sont cohérents entre celle-ci et la feuille de calcul du mix d'intervention, l'outil s'occupera du reste!"),
        p("Remplissez le modèle Excel : "),
        tags$ul(
          tags$li("« code_intervention » : Sélectionnez dans la liste déroulante l'intervention à laquelle les données de coût se rapportent."),
          tags$li("« type_intervention » : Sélectionnez dans la liste déroulante le type d'intervention spécifique auquel le coût se rapporte. Ces valeurs se rapportent à la colonne « type_ » du modèle précédent. Si l'utilisateur souhaite inclure des coûts fixes pour une intervention, cela est également spécifié dans cette colonne, par exemple Coûts fixes pour l'entreposage annuel de moustiquaires au cours d'une campagne."),
          tags$li("« cout_classe » : Sélectionnez la classe de coûts (Approvisionnement, Distribution, Opérationnel, Support, Autre) Si vous sélectionnez « autre », indiquez dans la colonne « cout_classe_autre » de quoi il s'agit."),
          tags$li("« description » : fournissez une brève description des composants du coût unitaire dans la colonne de description."),
          tags$li("« unite » : Sélectionnez dans la liste déroulante l'unité spécifique pour le coût, par exemple par filet, par enfant, par dose, par an , etc."),
          tags$li("« cout_monnaie_locale » : Valeur monétaire du coût unitaire spécifique en CDF"),
          tags$li("« taux_de_change » : taux de change à convertir de CDF en USD pour renseigner les valeurs de coût unitaire dans la  colonne « cout_usd » "),
          tags$li("« count_annee_pour_analyse » : cette valeur est l'année du plan opérationnel pour lequel le coût unitaire doit être utilisé pour calculer le budget. Si cette colonne est laissée vide, le même coût unitaire sera appliqué pour chaque année de livraison dans le plan spécifié."),
          tags$li("Pour faciliter la conversion des estimations de coût unitaire générées à partir de données historiques en valeurs monétaires actuelles, il existe les colonnes supplémentaires suivantes pour faciliter cette tâche : « cout_unitaire_d'origine » l' estimation du coût unitaire d'origine, « cout_unitaire_original_annee » l'année des données utilisées pour estimer le coût unitaire et enfin « facteur_d'inflation_initial_à_l'année_d'analyse » le facteur d'inflation à appliquer pour avoir des coûts unitaires en valeurs attendues actuelles et futures"),
          tags$li("Les colonnes « Notes » et « Source » peuvent être utilisées pour stocker des notes et des détails spécifiques sur la source de données utilisée pour générer les coûts unitaires. ")
        ),
        # Add image here
        div(
          style = "text-align: center; margin-top: 20px;",
          tags$a(
            href = "unit-template-image.png", # path relative to www/
            target = "_blank", # open in new tab
            tags$img(
              src = "unit-template-image.png",
              style = "max-width: 100%; height: auto; border: 1px solid #ccc; cursor: zoom-in;",
              alt = "Exemple de modèle de coûts unitaires"
            )
          ),
          tags$div(
            style = "font-style: italic; font-size: 90%; margin-top: 5px;",
            "Cliquez sur l'image pour l'agrandir"
          )
        ),
        p("Une fois que les données de coût unitaire ont été spécifiées, l'utilisateur peut enregistrer une copie locale de ce fichier."),
        p("Revenez à l'application Web et téléchargez le fichier Excel complété à l'aide du formulaire et donnez à la feuille de coûts un nom : par exemple « Coût 1 », etc. et une description : par exemple « Basé sur les données de coûts historiques » – assurez-vous qu'il s'agit de descriptions informatives car elles seront utiles lors de la génération et de la comparaison des plans."),
        p("Appuyez sur le bouton « Soumettre la feuille de coûts » et la feuille de calcul sera téléchargée dans l'outil."),
        p("Les données de coût téléchargées apparaîtront dans un tableau récapitulatif."),
        tags$b("3. Données précédentes téléchargées :"),
        p("Une fois qu'une feuille de calcul a été téléchargée dans l'outil, l'utilisateur est capable de télécharger un modèle basé sur un scénario spécifique téléchargé - cela peut faciliter le remplissage rapide d'un nouveau scénario sans avoir à répliquer chaque élément, mais assurez-vous de saisir un nouveau nom et une nouvelle description de scénario lors du rechargement.")
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
