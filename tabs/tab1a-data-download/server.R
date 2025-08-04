tab1aServer <- function(input, output, session,
                        template_file_path, SCENARIO_COLS, COST_COLS, TEMPLATE_ADMIN_DATA, shared) {
  #-DATA MANAGEMENT-------------------------------------------------------------

  # Reactive caches to store uploaded scenarios and cost data across the app
  shared$scenario_uploads_cache <- reactiveVal(NULL) # Stores all uploaded scenarios
  shared$cost_upload_cache <- reactiveVal(NULL) # Stores latest uploaded cost data

  # One-time check to ensure the template file exists when app runs
  observe({
    if (!file.exists(template_file_path)) {
      warning(paste("Template file not found:", template_file_path))
    }
  })

  # Trigger reactive refresh (e.g., after a new file is uploaded)
  refresh_trigger <- reactive({
    shared$refresh_trigger
  })

  # FUNCTION: Calculate a unique hash for a file based on its content
  # Used to identify and prevent duplicate uploads
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

  # FUNCTION: Check if a file with this hash has already been uploaded
  # Prevents duplicate submissions of the same scenario or cost file
  is_duplicate_file <- function(hash, type = "scenario") {
    if (is.null(hash)) {
      return(list(is_duplicate = FALSE)) # Cannot be duplicate if no hash
    }
    # Connect to the correct upload database
    db_file <- paste0(type, "_uploads.db")
    db <- dbConnect(SQLite(), db_file)
    on.exit(dbDisconnect(db)) # Ensure DB is closed after function finishes

    # Query for any record with the same hash
    query <- "SELECT name FROM uploads WHERE file_hash = ? LIMIT 1"
    result <- dbGetQuery(db, query, list(hash))

    # Return result as a list with duplication status and name (if applicable)
    list(
      is_duplicate = nrow(result) > 0,
      existing_name = if (nrow(result) > 0) result$name[1] else NULL
    )
  }

  # FUNCTION: Load all uploaded scenario metadata from the database
  # Returns a dataframe with all scenario uploads (most recent first)
  load_all_uploaded_scenarios <- function() {
    db <- DBI::dbConnect(RSQLite::SQLite(), "scenario_uploads.db")
    uploads <- DBI::dbGetQuery(db, "SELECT * FROM uploads ORDER BY upload_date DESC")
    DBI::dbDisconnect(db)
    uploads
  }

  # FUNNCTION: Load the most recently uploaded cost file as a dataframe
  # This is typically used to prefill or review the most current cost assumptions
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


  #-SCENARIO TEMPLATE DOWNLOAD HANDLER------------------------------------------
  output$download_scenario_template <- downloadHandler(

    # Define download file name
    filename = function() {
      "scenario_template.xlsx"
    },

    # Define content creation logic for the downloaded file
    content = function(file) {
      tryCatch(
        {
          message("Starting template download...")
          message(paste("Template path:", template_file_path))

          # Load the original workbook template
          wb <- openxlsx::loadWorkbook(template_file_path)

          # Identify sheet names in the original workbook
          original_sheets <- openxlsx::sheets(wb)
          template_sheet <- "modèle" # this sheet is used as the base to clone

          # Check whether the optional options sheet exists
          options_sheet_exists <- "liste-options-ignorer" %in% original_sheets

          # Determine which years to include (user-specified or default)
          years_to_use <- if (length(input$year_filter) > 0) input$year_filter else DEFAULT_YEARS
          message("Years to use: ", paste(years_to_use, collapse = ", "))

          # Clone the template sheet for each specified year
          for (year in years_to_use) {
            year_str <- as.character(year)
            message("Creating sheet for year: ", year_str)

            # Clone the template sheet with the year as the name
            openxlsx::cloneWorksheet(wb, year_str, clonedSheet = template_sheet)

            # Confirm the sheet was added
            if (year_str %in% openxlsx::sheets(wb)) {
              message("Successfully created sheet: ", year_str)
            } else {
              message("Failed to create sheet: ", year_str)
            }
          }

          # Remove the original template sheet after cloning
          if (template_sheet %in% openxlsx::sheets(wb)) {
            message("Removing template sheet")
            openxlsx::removeWorksheet(wb, template_sheet)
          }

          # Reorder sheets: years first, then options sheet if it exists
          current_sheets <- openxlsx::sheets(wb)
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

          # Set the sheet order if it's fully valid
          valid_order <- desired_order[desired_order %in% current_sheets]
          if (length(valid_order) == length(current_sheets)) {
            message("Setting sheet order: ", paste(valid_order, collapse = ", "))
            openxlsx::worksheetOrder(wb) <- match(valid_order, current_sheets)
          } else {
            message(
              "⚠️ Could not set exact sheet order. Current sheets: ",
              paste(current_sheets, collapse = ", ")
            )
          }

          # Save the modified workbook to the specified download path
          openxlsx::saveWorkbook(wb, file, overwrite = TRUE)

          # Log final state for debugging
          final_sheets <- openxlsx::sheets(wb)
          message("Final sheet order: ", paste(final_sheets, collapse = ", "))
          message(
            "Workbook saved successfully with ", length(final_sheets),
            " sheets"
          )
        },

        # Error handling: show modal dialog on failure
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
    # Define MIME type for Excel files
    contentType =
      "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )


  #-DOWNLOAD PREVIOUSLY UPLOADED SCENARIO TEMPLATE------------------------------
  output$scenario_download_ui <- renderUI({
    # List all uploaded .xlsx scenario files
    scenario_files <- list.files("uploads/scenarios", pattern = "\\.xlsx$")

    # If no files found, show fallback message
    if (length(scenario_files) == 0) {
      return(p("Aucun scénario téléchargé disponible"))
    }

    # Clean file names for user display (remove .xlsx extension)
    choices <- setNames(
      scenario_files,
      tools::file_path_sans_ext(scenario_files)
    )

    # Return UI elements: select dropdown + download button
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

  #-DOWNLOAD HANDLER: RETURNS SELECTED UPLOADED SCENARIO------------------------
  output$download_selected_scenario <- downloadHandler(

    # File name will be exactly as uploaded (preserve original name)
    filename = function() {
      input$scenario_to_download
    },
    # Copy selected file from uploads directory to output path
    content = function(file) {
      file.copy(
        file.path("uploads/scenarios", input$scenario_to_download),
        file
      )
    },
    # Define correct MIME type for Excel downloads
    contentType =
      "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )

  #-OBSERVER: RE-RENDERS THE SCENARIO SELECTION UI WHEN NEW FILES UPOLOADED-----
  observeEvent(refresh_trigger(), {
    # Update the dropdown UI dynamically
    output$scenario_download_ui <- renderUI({
      # Get latest list of uploaded scenarios
      scenario_files <- list.files("uploads/scenarios", pattern = "\\.xlsx$")

      # Return fallback if no files exist
      if (length(scenario_files) == 0) {
        return(p("No uploaded scenarios available"))
      }

      # Clean display names for dropdown
      choices <- setNames(
        scenario_files,
        tools::file_path_sans_ext(scenario_files)
      )

      # Updated UI with select and download
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


  #-COST TEMPLATE DOWNLOAD HANDLER----------------------------------------------
  output$download_cost_template <- downloadHandler(

    # File name used for download
    filename = function() {
      "cost_template.xlsx"
    },

    # Content logic to copy and return template
    content = function(file) {
      tryCatch(
        {
          print("Starting cost template download...")
          print(paste("Template path:", cost_template_file_path))

          # Load the cost template using openxlsx2 (preserves dropdowns)
          wb <- openxlsx2::wb_load(cost_template_file_path)

          # Save workbook to output file
          openxlsx2::wb_save(wb, file)
          print("Saved cost workbook with dropdowns preserved")
        },

        # Handle and report any errors
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
    # MIME type for Excel
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )

  #-UI: COST FILE DOWNLOAD DROPDOWN AND BUTTON----------------------------------
  output$cost_download_ui <- renderUI({
    # Step 1: List all uploaded cost files in the expected folder
    cost_files <- list.files("uploads/costs", pattern = "\\.xlsx$")

    # Step 2: If no cost files exist, show a fallback message
    if (length(cost_files) == 0) {
      return(p("Aucun fichier de coûts téléchargé disponible"))
    }

    # Step 3: Prepare named list of file choices (remove '.xlsx' from labels)
    choices <- setNames(
      cost_files,
      tools::file_path_sans_ext(cost_files)
    )

    # Step 4: Render the dropdown and download button UI
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

  #-DOWNLOAD HANDLER: ALLOW USER TO DOWNLOAD SELECTED COST FILE-----------------
  output$download_selected_cost <- downloadHandler(

    # Define the filename that will be used in the download
    filename = function() {
      input$cost_to_download # Return the original filename
    },
    # Define how to generate the downloadable content
    content = function(file) {
      # Copy the selected file from the uploads directory to the download location
      file.copy(
        file.path("uploads/costs", input$cost_to_download),
        file
      )
    },
    # Set the correct MIME type for Excel files
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )

  #-OBSERVER: UPDATE COST DOWNLOAD UI WHEN NEW FILES UPLOADED-------------------
  observeEvent(refresh_trigger(), {
    # Dynamically re-render the UI for cost file downloads
    output$cost_download_ui <- renderUI({
      # List the most recent uploaded cost files
      cost_files <- list.files("uploads/costs", pattern = "\\.xlsx$")

      # If no files are found, show fallback message
      if (length(cost_files) == 0) {
        return(p("No uploaded cost files available"))
      }

      # Clean the file names for display (remove '.xlsx' extension)
      choices <- setNames(
        cost_files,
        tools::file_path_sans_ext(cost_files)
      )

      # Return updated dropdown and download button UI
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

  #-SCENARIO UPLOAD HANDLER-----------------------------------------------------
  observeEvent(input$submit_scenario, {
    # Disable scenario upload in demo/lite mode
    if (lite_mode) {
      showModal(modalDialog(
        title = "Feature Disabled",
        "Scenario uploading is disabled in this demonstration version.",
        easyClose = TRUE
      ))
      return()
    }

    tryCatch({
      # STEP 1: Read all sheet names in the uploaded Excel file
      all_sheet_names <- excel_sheets(input$scenario_file$datapath)

      # Filter sheets to only include 4-digit numeric names (i.e., years)
      sheet_names <- all_sheet_names[grepl("^\\d{4}$", all_sheet_names)]

      all_sheets_valid <- TRUE

      # STEP 2: Validate each scenario sheet
      for (sheet in sheet_names) {
        current_data <- read_excel(input$scenario_file$datapath, sheet = sheet)

        # --- Validate column structure for scenario template
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

        # --- Validate admin column values match the template
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

        # --- Validate that 'code_' columns contain only 0, 1, or NA
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

      # Stop if any validation failed
      if (!all_sheets_valid) {
        return()
      }

      # STEP 3: Create safe filename and avoid duplicates
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

      # Check for duplicate file using hash
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

      # STEP 4: Save file and log metadata to the database
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

      # STEP 5: Reset inputs and update UI
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
      # Catch and display unexpected errors
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


  #-COST UPLOAD HANDLER---------------------------------------------------------
  observeEvent(input$submit_cost, {
    # Disable cost upload in demo/lite mode
    if (lite_mode) {
      showModal(modalDialog(
        title = "Feature Disabled",
        "Cost uploading is disabled in this demonstration version.",
        easyClose = TRUE
      ))
      return()
    }

    tryCatch({
      # STEP 1: Load uploaded cost data and check columns
      current_data <- read_excel(input$cost_file$datapath)

      # Check column names with the ones that must be there
      if (!identical(colnames(current_data), COST_COLS_MATCH)) {
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

      # STEP 2: Create a safe and unique file name
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

      # Check for duplicates using file hash
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

      # STEP 3: Save cost file and insert metadata in DB once checks successful
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

      # STEP 4: Reset UI and trigger refresh
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
      # Catch and display unexpected errors
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

  #-RENDER SCENARIO UPLOADS TABLE WITH DELETE BUTTONS---------------------------
  output$scenario_uploads_table <- renderDT({
    # Trigger reactivity to refresh table whenever new data is uploaded or deleted
    refresh_trigger()

    # STEP 1: Query scenario uploads from the database
    db <- dbConnect(SQLite(), "scenario_uploads.db")
    uploads <- dbGetQuery(db, "
    SELECT id, name, description, filename, years, upload_date
    FROM uploads
    ORDER BY upload_date DESC
  ")
    dbDisconnect(db)

    # STEP 2: If uploads exist, render a datatable with delete buttons
    if (nrow(uploads) > 0) {
      # Create a column of HTML delete buttons with data-id attributes
      uploads$actions <- paste0(
        '<button class="btn btn-warning btn-sm delete-btn" data-id="', uploads$id, '">Supprimer</button>'
      )

      # Render the table with the new 'actions' column
      datatable(
        uploads[, c("name", "description", "years", "upload_date", "actions")],
        options = list(
          pageLength = 5,
          scrollY = "200px",
          scrollCollapse = TRUE,
          columnDefs = list(
            list(targets = 4, orderable = FALSE) # Make the delete button column unsortable
          ),
          language = list(
            url = "https://cdn.datatables.net/plug-ins/1.13.4/i18n/fr-FR.json" # French translation
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
    }

    # STEP 3: If no uploads exist, return an empty table with headers
    else {
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


  #-RENDER COST UPLOADS TABLE WITH DELETE BUTTONS-------------------------------
  output$cost_uploads_table <- renderDT({
    # STEP 1: Trigger reactivity whenever uploads are updated
    refresh_trigger()

    # STEP 2: Query cost uploads from the local SQLite database
    db <- dbConnect(SQLite(), "cost_uploads.db")
    uploads <- dbGetQuery(db, "
    SELECT id, name, description, filename, upload_date
    FROM uploads
    ORDER BY upload_date DESC
  ")
    dbDisconnect(db)

    # STEP 3: If cost uploads exist, render table with delete buttons
    if (nrow(uploads) > 0) {
      # Add a column of HTML delete buttons with a unique data-id per row
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
            list(targets = 3, orderable = FALSE) # Prevent sorting on the delete button column
          ),
          language = list(
            url = "https://cdn.datatables.net/plug-ins/1.13.4/i18n/fr-FR.json" # Use French translations
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
      # STEP 4: If no cost uploads exist, render an empty placeholder table
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

  #-OBSERVER: SCENARIO DELETE---------------------------------------------------
  observeEvent(input$delete_scenario, {
    # Check: Prevent deletion in demonstration (lite) mode
    if (lite_mode) {
      showModal(modalDialog(
        title = "Feature Disabled",
        "Deleting scenarios is disabled in this demonstration version.",
        easyClose = TRUE
      ))
      return()
    }

    # Ensure input is present (required trigger)
    req(input$delete_scenario)
    print(paste("Triggered delete for scenario ID:", input$delete_scenario))
    scenario_id <- input$delete_scenario

    # STEP 1: Connect to the database and retrieve file info
    db <- dbConnect(SQLite(), "scenario_uploads.db")
    file_info <- dbGetQuery(db, "SELECT filename FROM uploads WHERE id = ?", list(scenario_id))

    # STEP 2: If file exists in the DB, attempt deletion
    if (nrow(file_info) > 0) {
      file_path <- file.path("uploads/scenarios", file_info$filename[1])

      # Delete physical file from disk
      if (file.exists(file_path)) {
        file.remove(file_path)
      }

      # Delete metadata entry from database
      dbExecute(db, "DELETE FROM uploads WHERE id = ?", list(scenario_id))

      # Notify user of success
      showModal(modalDialog(
        title = "Supprimé",
        "Le scénario sélectionné a été supprimé.",
        easyClose = TRUE
      ))

      # Trigger UI refresh
      shared$refresh_trigger <- shared$refresh_trigger + 1
    } else {
      # STEP 3: File not found in DB – show error
      showModal(modalDialog(
        title = "Erreur",
        "Scénario non trouvé ou déjà supprimé.",
        easyClose = TRUE
      ))
    }

    # Close DB connection
    dbDisconnect(db)
  })

  #-OBSERVER: COST DELETE---------------------------------------------------
  observeEvent(input$delete_cost, {
    # Ensure input is present (required trigger)
    req(input$delete_cost)

    # Check: Disable cost deletion in lite/demo mode
    if (lite_mode) {
      showModal(modalDialog(
        title = "Feature Disabled",
        "Deleting cost sheets is disabled in this demonstration version.",
        easyClose = TRUE
      ))
      return()
    }

    # STEP 1: Extract cost ID and query corresponding file from database
    cost_id <- input$delete_cost
    db <- dbConnect(SQLite(), "cost_uploads.db")
    file_info <- dbGetQuery(db, "SELECT filename FROM uploads WHERE id = ?", list(cost_id))

    # STEP 2: If file exists in DB, proceed with deletion
    if (nrow(file_info) == 1) {
      file_path <- file.path("uploads/costs", file_info$filename)

      # Delete physical Excel file from disk if it exists
      if (file.exists(file_path)) {
        file.remove(file_path)
      }

      # Delete corresponding entry from database
      dbExecute(db, "DELETE FROM uploads WHERE id = ?", list(cost_id))

      # Notify user of successful deletion
      showModal(modalDialog(
        title = "Supprimé",
        "Le fichier de coûts sélectionné a été définitivement supprimé.",
        easyClose = TRUE
      ))

      # Trigger a UI refresh
      shared$refresh_trigger <- shared$refresh_trigger + 1
    } else {
      # STEP 3: File not found – show error message
      showModal(modalDialog(
        title = "Erreur",
        "Fichier de coûts non trouvé ou déjà supprimé.",
        easyClose = TRUE
      ))
    }

    # STEP 4: Close the database connection
    dbDisconnect(db)
  })

  #-OBSERVER: Show Modal with SCENARIO Template Instructions--------------------
  observeEvent(input$show_instructions_sc, {
    # Open modal dialog with guidance for downloading and completing scenario templates
    showModal(modalDialog(
      title = "Instructions détaillées pour télécharger et compléter les modèles",
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
        div(
          style = "text-align: center; margin-top: 20px;",
          tags$a(
            href = "scenario-template-image.png",
            target = "_blank",
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
        tags$b("2. Données précédentes téléchargées :"),
        p("Une fois qu'une feuille de calcul a été téléchargée dans l'outil, l'utilisateur est capable de télécharger un modèle basé sur un scénario spécifique téléchargé - cela peut faciliter le remplissage rapide d'un nouveau scénario sans avoir à répliquer chaque élément, mais assurez-vous de saisir un nouveau nom et une nouvelle description de scénario lors du rechargement.")
      )
    ))
  })

  #-OBSERVER: Show Modal with COST Template Instructions--------------------
  observeEvent(input$show_instructions_uc, {
    # Open modal dialog with guidance for downloading and completing unit cost templates
    showModal(modalDialog(
      title = "Instructions détaillées pour télécharger et compléter les modèles",
      easyClose = TRUE,
      size = "l",
      footer = modalButton("Fermer"),
      tagList(
        tags$b("1. Définition des coûts unitaires:"),
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
        div(
          style = "text-align: center; margin-top: 20px;",
          tags$a(
            href = "unit-template-image.png",
            target = "_blank",
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
        tags$b("2. Données précédentes téléchargées :"),
        p("Une fois qu'une feuille de calcul a été téléchargée dans l'outil, l'utilisateur est capable de télécharger un modèle basé sur un scénario spécifique téléchargé - cela peut faciliter le remplissage rapide d'un nouveau scénario sans avoir à répliquer chaque élément, mais assurez-vous de saisir un nouveau nom et une nouvelle description de scénario lors du rechargement.")
      )
    ))
  })
}
