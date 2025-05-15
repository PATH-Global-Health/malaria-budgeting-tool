tab1aServer <- function(input, output, session,
  template_file_path, SCENARIO_COLS, COST_COLS, TEMPLATE_ADMIN_DATA, shared) {

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
      tryCatch({
        sheets <- excel_sheets(file_path)
        content <- lapply(sheets, function(sheet) {
          data <- read_excel(file_path, sheet = sheet)
          paste(capture.output(data), collapse = "")
        })
        digest::digest(paste(content, collapse = ""), algo = "md5")
      }, error = function(e) {
        NULL
      })
    }

    # Function to check for duplicate files
    is_duplicate_file <- function(hash, type = "scenario") {
      if (is.null(hash)) return(list(is_duplicate = FALSE))
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


    # Scenario template download handler
    output$download_scenario_template <- downloadHandler(
      filename = function() {
        "scenario_template.xlsx"
      },
      content = function(file) {
        tryCatch({
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
          message("Years to use: ", paste(years_to_use, collapse=", "))

          # Clone the sheet for each selected year
          for (year in years_to_use) {
            year_str <- as.character(year)
            message("Creating sheet for year: ", year_str)

            # Clone the template sheet with the year as the name
            openxlsx::cloneWorksheet(wb, year_str, clonedSheet = template_sheet)

            # Verify the sheet was created
            if(year_str %in% openxlsx::sheets(wb)) {
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
              desired_order <- c(desired_order, year_str)
            }
          }

          # Add list-options-ignore at the end if it exists
          if (options_sheet_exists) {
            desired_order <- c(desired_order, "list-options-ignore")
          }

          # Reorder the sheets using the setSheetOrder function
          if (length(current_sheets) > 0) {
            message("Setting sheet order: ", paste(desired_order, collapse=", "))
            openxlsx::worksheetOrder(wb) <- match(desired_order, current_sheets)
          }

          # Save the workbook
          openxlsx::saveWorkbook(wb, file, overwrite = TRUE)

          # Verify final order
          final_sheets <- openxlsx::sheets(wb)
          message("Final sheet order: ", paste(final_sheets, collapse=", "))
          message("Workbook saved successfully with ", length(final_sheets), " sheets")

        }, error = function(e) {
          message(paste("Error:", e$message))
          showModal(modalDialog(
            title = "Error",
            sprintf("Failed to generate template: %s", e$message),
            easyClose = TRUE
          ))
        })
      },
      contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
    )


    # UI for scenario downloads
   output$scenario_download_ui <- renderUI({
    # Get list of files
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
            "Select a scenario to download:",
            choices = choices,
            width = "100%"
        ),
        downloadButton(
            session$ns("download_selected_scenario"),
            "Download Selected Scenario",
            class = "btn-primary"
        )
    )
  })

  # Download handler for selected scenario
  output$download_selected_scenario <- downloadHandler(
    filename = function() {
        input$scenario_to_download  # Return the original filename
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
                "Select a scenario to download:",
                choices = choices,
                width = "100%"
            ),
            downloadButton(
                session$ns("download_selected_scenario"),
                "Download Selected Scenario",
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
      tryCatch({
        print("Starting cost template download...")
        print(paste("Template path:", cost_template_file_path))

        # Load the original cost template with dropdowns
        wb <- openxlsx2::wb_load(cost_template_file_path)

        # Save workbook to user download
        openxlsx2::wb_save(wb, file)
        print("Saved cost workbook with dropdowns preserved")

      }, error = function(e) {
        print(paste("Error:", e$message))
        showModal(modalDialog(
          title = "Error",
          sprintf("Failed to generate cost template: %s", e$message),
          easyClose = TRUE
        ))
      })
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )

    # UI for cost downloads
  output$cost_download_ui <- renderUI({
    # Get list of files
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
            "Select a cost file to download:",
            choices = choices,
            width = "100%"
        ),
        downloadButton(
            session$ns("download_selected_cost"),
            "Download Selected Cost File",
            class = "btn-primary"
        )
    )
  })

  # Download handler for selected cost file
  output$download_selected_cost <- downloadHandler(
    filename = function() {
        input$cost_to_download  # Return the original filename
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
            "Select a cost file to download:",
            choices = choices,
            width = "100%"
        ),
        downloadButton(
            session$ns("download_selected_cost"),
            "Download Selected Cost File",
            class = "btn-primary"
        )
        )
    })
  })

    # Scenario upload handler
    observeEvent(input$submit_scenario, {
      req(input$scenario_file, input$scenario_name)

      showModal(modalDialog(
        title = "Processing Scenario File",
        "Reading Excel file sheets...",
        footer = NULL,
        easyClose = TRUE
      ))

      tryCatch({
        all_sheet_names <- excel_sheets(input$scenario_file$datapath)

        # Keep only sheets with numeric names (i.e. years) for the validation as we have
        # the list data in here too now
        sheet_names <- all_sheet_names[grepl("^\\d{4}$", all_sheet_names)]

        # Validate each sheet
        all_sheets_valid <- TRUE

        for(sheet in sheet_names) {
          current_data <- read_excel(input$scenario_file$datapath, sheet = sheet)

          # Check column names
          if (!identical(colnames(current_data), SCENARIO_COLS)) {
            all_sheets_valid <- FALSE
            removeModal()
            showModal(modalDialog(
              title = "Error",
              HTML(sprintf(
                "Upload failed: Column names in sheet '%s' do not match the template.<br><br>
                 Expected: %s<br>
                 Found: %s",
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
          for(col in admin_cols) {
            if (!identical(sort(unique(current_data[[col]])),
                          sort(unique(TEMPLATE_ADMIN_DATA[[col]])))) {
              all_sheets_valid <- FALSE
              removeModal()
              showModal(modalDialog(
                title = "Error",
                HTML(sprintf(
                  "Upload failed: Values in administrative column '%s' must match the template exactly.<br><br>
                   Expected values: %s<br>
                   Found values: %s",
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
          for(col in code_cols) {
            invalid_values <- current_data[[col]][!is.na(current_data[[col]]) &
                                                !current_data[[col]] %in% c(0, 1)]
            if (length(invalid_values) > 0) {
              all_sheets_valid <- FALSE
              removeModal()
              showModal(modalDialog(
                title = "Error",
                HTML(sprintf(
                  "Upload failed: Column '%s' can only contain values 0, 1, or NA.<br><br>
                   Found invalid values: %s",
                  col,
                  paste(unique(invalid_values), collapse = ", ")
                )),
                easyClose = TRUE
              ))
              return()
            }
          }

          # Check plan columns
          plan_cols <- grep("^plan", SCENARIO_COLS, value = TRUE)
          for(col in plan_cols) {
            if (any(!is.na(current_data[[col]]))) {
              all_sheets_valid <- FALSE
              removeModal()
              showModal(modalDialog(
                title = "Error",
                HTML(sprintf(
                  "Upload failed: Column '%s' should be empty in the upload template. These values will be added by the application.",
                  col
                )),
                easyClose = TRUE
              ))
              return()
            }
          }
        }

        if (!all_sheets_valid) return()

        # Create safe filename from scenario name
        safe_name <- gsub("[^[:alnum:]]", "_", input$scenario_name)
        new_filename <- paste0(safe_name, ".xlsx")
        file_path <- file.path("uploads/scenarios", new_filename)

        # Check if file already exists and append number if needed
        base_name <- safe_name
        counter <- 1
        while(file.exists(file_path)) {
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
            title = "Error",
            HTML(sprintf(
              "Upload failed: This file is identical to a previous upload named '<strong>%s</strong>'.",
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
          ))
        dbDisconnect(db)

        # Reset inputs
        updateTextInput(session, session$ns("scenario_name"), value = "")
        updateTextAreaInput(session, session$ns("scenario_description"), value = "")

        shared$refresh_trigger <- shared$refresh_trigger + 1

        removeModal()
        showModal(modalDialog(
          title = "Success",
          "Scenario file uploaded successfully!",
          easyClose = TRUE,
          footer = tagList(
            modalButton("Close"),
            tags$script("setTimeout(function() { resetScroll(); }, 100);")
          )
        ))

      }, error = function(e) {
        removeModal()
        showModal(modalDialog(
          title = "Error",
          sprintf("Failed to process Excel file: %s", e$message),
          easyClose = TRUE
        ))
      }, finally = {
        removeModal()
      })
    })

  # Cost upload
  observeEvent(input$submit_cost, {
    req(input$cost_file, input$cost_name)

    showModal(modalDialog(
      title = "Processing Cost File",
      "Reading Excel file...",
      footer = NULL,
      easyClose = TRUE
    ))

    tryCatch({
      current_data <- read_excel(input$cost_file$datapath)

      # Check column names
      if (!identical(colnames(current_data), COST_COLS)) {
        removeModal()
        showModal(modalDialog(
          title = "Error",
          HTML(sprintf(
            "Upload failed: Column names do not match the template.<br><br>
             Expected: %s<br>
             Found: %s",
            paste(COST_COLS, collapse = ", "),
            paste(colnames(current_data), collapse = ", ")
          )),
          easyClose = TRUE
        ))
        return()
      }

      # # Check if resource column values match template
      # cost_template_data <- read_excel(cost_template_file_path)
      # expected_resources <- cost_template_data$resource[!is.na(cost_template_data$resource)]
      # uploaded_resources <- current_data$resource[!is.na(current_data$resource)]
      #
      # if (!identical(sort(unique(uploaded_resources)), sort(unique(expected_resources)))) {
      #   removeModal()
      #   showModal(modalDialog(
      #     title = "Error",
      #     HTML(sprintf(
      #       "Upload failed: Values in the 'resource' column must match the template exactly.<br><br>
      #        Please use the template file without modifying the resource names."
      #     )),
      #     easyClose = TRUE
      #   ))
      #   return()
      # }

      # Create safe filename from cost name
      safe_name <- gsub("[^[:alnum:]]", "_", input$cost_name)
      new_filename <- paste0(safe_name, ".xlsx")
      file_path <- file.path("uploads/costs", new_filename)

      # Check if file already exists and append number if needed
      base_name <- safe_name
      counter <- 1
      while(file.exists(file_path)) {
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
          title = "Error",
          HTML(sprintf(
            "Upload failed: This file is identical to a previous upload named '<strong>%s</strong>'.",
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
          ))
      }, finally = {
        dbDisconnect(db)
      })

      # Reset inputs
      updateTextInput(session, session$ns("cost_name"), value = "")
      updateTextAreaInput(session, session$ns("cost_description"), value = "")

      shared$refresh_trigger <- shared$refresh_trigger + 1

      removeModal()
      showModal(modalDialog(
        title = "Success",
        "Cost file uploaded successfully!",
        easyClose = TRUE,
        footer = tagList(
          modalButton("Close"),
          tags$script("setTimeout(function() { resetScroll(); }, 100);")
        )
      ))

    }, error = function(e) {
      removeModal()
      showModal(modalDialog(
        title = "Error",
        sprintf("Failed to process Excel file: %s", e$message),
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
        '<button class="btn btn-danger btn-sm delete-btn" data-id="', uploads$id, '">Delete</button>'
      )

      datatable(
        uploads[, c("name", "description", "years", "upload_date", "actions")],
        options = list(
          pageLength = 5,
          scrollY = "200px",
          scrollCollapse = TRUE,
          columnDefs = list(
            list(targets = 4, orderable = FALSE)  # Make the actions column not sortable
          )
        ),
        colnames = c("Name", "Description", "Years", "Upload Date", "Actions"),
        selection = "none",
        escape = FALSE,  # Important! Allows HTML in the table
        callback = JS("
        table.on('click', '.delete-btn', function() {
          var id = $(this).data('id');
          if (confirm('Are you sure you want to permanently delete this scenario?')) {
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
          "Upload Date" = character(),
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
        '<button class="btn btn-danger btn-sm delete-cost-btn" data-id="', uploads$id, '">Delete</button>'
      )

      datatable(
        uploads[, c("name", "description", "upload_date", "actions")],
        options = list(
          pageLength = 5,
          scrollY = "200px",
          scrollCollapse = TRUE,
          columnDefs = list(
            list(targets = 3, orderable = FALSE)  # 'Actions' column not sortable
          )
        ),
        colnames = c("Name", "Description", "Upload Date", "Actions"),
        selection = "none",
        escape = FALSE,
        callback = JS("
        table.on('click', '.delete-cost-btn', function() {
          var id = $(this).data('id');
          if (confirm('Are you sure you want to permanently delete this cost file? This action cannot be undone.')) {
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
          "Upload Date" = character(),
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
      title = "Deleted",
      "The selected scenario has been deleted.",
      easyClose = TRUE
    ))

    # Trigger UI refresh
    shared$refresh_trigger <- shared$refresh_trigger + 1
  } else {
    showModal(modalDialog(
      title = "Error",
      "Scenario not found or already deleted.",
      easyClose = TRUE
    ))
  }

  dbDisconnect(db)
})

# Delete Cost action
observeEvent(input$delete_cost, {
  req(input$delete_cost)
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
      title = "Deleted",
      "The selected cost file has been permanently deleted.",
      easyClose = TRUE
    ))

    shared$refresh_trigger <- shared$refresh_trigger + 1
  } else {
    showModal(modalDialog(
      title = "Error",
      "Cost file not found or already deleted.",
      easyClose = TRUE
    ))
  }

  dbDisconnect(db)
})

# Adding instructions pop up
observeEvent(input$show_instructions, {
  showModal(modalDialog(
    title = "Detailed Instructions for Uploading Templates",
    easyClose = TRUE,
    size = "l",
    footer = modalButton("Close"),
    tagList(
      tags$b("Steps to Use – No previous data uploaded:"),
      p("Follow these steps if you have yet used the tool – see the number referenced screen shot to reference between steps."),
      tags$ol(
        tags$li("📆️ Select planning years to define the scope of your scenario"),
        tags$li("📄 Click 'Download Empty Scenario Template'"),
        tags$li("📊 Fill out the Excel template: each sheet corresponds to a year, and each row represents the smallest spatial unit used for intervention planning. Each column details a specific type of malaria intervention preceded by the word ‘code_’ Enter 1 for planned interventions and 0 otherwise. In the corresponding ‘type_’ column for each intervention select the type of intervention that is planned for. For example, in the ‘type_vaccine’ column select from “R21” or “RTSS”. If the intervention type is not included in the drop down list you can overwrite with free text to include this."),
        tags$li("💾 Once a plan has been specified by indicating what interventions are to be targeted where each year the user can save a local copy of this file."),
        tags$li("📤 Return to the web application and upload the completed Excel file using the form, and give the scenario a short name: e.g. Plan A/ Plan 1 etc and a description: e.g. “Fully scaled up plan”/ “Restricted vaccine roll out plan” – make sure these are informative descriptions as it will be helpful when comparing plans."),
        tags$li("📂 Press “Submit Scenario” button and the spreadsheet will be uploaded in to the tool."),
        tags$li("📋 Uploaded plans will appear in a summary table with details like plan name, years covered, and upload date."),
        tags$li("💵 Now we also need to specify the unit costs that will be critical for the tool to generate our budgets."),
        tags$li("📄 Click 'Download Empty Cost Template'"),
        tags$li("📊 Fill out the Excel template: The Unit Cost template allows for the user to have flexibility in specifying the exact unit costs relevant to the country context the template has the following columns:"),
        tags$ul(
          tags$li("resource_name: Long name description of unit cost data e.g. “Procurement cost of malaria vaccine per dose”"),
          tags$li("code_intervention: Select from the drop down list the type of intervention the unit cost related to e.g. vaccine/ itn_campaign/ smc"),
          tags$li("type_intervention: Select from the drop down list the specific type of intervention unit cost relates to e.g. r21/ rtss/ pyrethroid_net/ PBO net"),
          tags$li("cost_class: Select from the drop down list the class of cost: e,g Procurement/ Distribution/ Operational/ Supportive"),
          tags$li("unit: specify the exact unit e.g. per dose, per ITN, per child etc"),
          tags$li("local_currency_cost: Specific cost in local currency"),
          tags$li("usd_cost: Specific cost in USD"),
          tags$li("cost_year: Year that cost relates to"),
          tags$li("source: Reference the cost source used to generate unit cost data")
        ),
        tags$li("Ensure that for each intervention being delivered there are unit costs for the specific intervention and intervention type."),
        tags$li("💾 Once unit cost data has been specified, the user can save a local copy of this file."),
        tags$li("📤 Return to the web application and upload the completed Excel file using the form and give the cost sheet a name: e.g. “Cost Assumptions 1” etc and a description: e.g. “Including a higher cost estimate of procurement for PBO nets” – make sure these are informative descriptions as it will be helpful when generating and comparing plans."),
        tags$li("📂 Press “Submit Cost Sheet” button and the spreadsheet will be uploaded in to the tool."),
        tags$li("📋 Uploaded cost data will appear in a summary table with details like cost name, description, and upload date.")
      ),
      tags$br(),
      tags$b("Steps to Use – Previous data uploaded:"),
      p("Once a spreadsheet has been uploaded into the tool the user is capable downloading a template based on a specific scenario uploaded – this can make it easier to quickly populate a new scenario without having to replicate every item but make sure to input a new scenario name and description when re-uploading."),
      p("Once both sets of data have been uploaded the user can move on to the next stage of the application.")
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
