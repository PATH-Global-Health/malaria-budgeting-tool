tab2aServer <- function(input, output, session, shared) {
  ns <- session$ns
  row_count <- reactiveVal(3)

  # Store matrix selections
  matrix_selections <- reactiveVal(NULL)

  # Store budget results
  budget_results <- reactiveVal(NULL)

  # Budget generation status
  budget_generating <- reactiveVal(FALSE)

  # Budget history table
  budget_history <- reactiveVal(data.frame(
    date_generated = character(),
    num_budgets = integer(),
    scenario_cost_combinations = character(),
    years_generated = character(),
    file_path = character(),
    stringsAsFactors = FALSE
  ))
  # On startup, load budget history if it exists
  observe({
    # Ensure the generated directory exists
    if (!dir.exists("generated")) {
      dir.create("generated", showWarnings = FALSE)
    }

    history_path <- "generated/budget_history.rds"
    if (file.exists(history_path)) {
      budget_history(readRDS(history_path))
    }
  }, priority = 1000)


  # Store selected values across UI redraws
  stored_selections <- reactiveVal(list(
    plans = list(),
    costs = list(),
    populations = list()
  ))

  observeEvent(shared$refresh_trigger, {
    message("🔄 Refreshing data in tab2a...")
    shared$reload_all_uploads()
  }, ignoreInit = FALSE)

  scenario_data <- reactive({
    cache <- shared$scenario_uploads_cache
    if (is.null(cache) || !is.reactive(cache)) return(NULL)
    cache()
  })

  cost_data <- reactive({
    cache <- shared$cost_upload_cache
    if (is.null(cache) || !is.reactive(cache)) return(NULL)
    cache()
  })

  observe({
    req(cost_data())
    message("💵 Cost data column names:")
    print(colnames(cost_data()))
    message("📋 Head of cost data:")
    print(head(cost_data()))
  })

  data_available <- reactive({
    !is.null(scenario_data()) && nrow(scenario_data()) > 0 &&
      !is.null(cost_data()) && nrow(cost_data()) > 0
  })

  # Reactive for cost titles to ensure they're only calculated once per render
  cost_titles_reactive <- reactive({
    req(cost_data())
    cost_data() %>%
      distinct(cost_name, cost_description) %>%
      mutate(label = paste0(cost_name, " (", cost_description, ")"))
  })

  # Observe and store all input changes for persistence
  observe({
    req(cost_data(), row_count() > 0)

    current_stored <- stored_selections()

    # Store plan selections
    for (i in 1:row_count()) {
      plan_id <- paste0("row_option_", i)
      if (!is.null(input[[plan_id]])) {
        current_stored$plans[[i]] <- input[[plan_id]]
      }

      # Store target population selections
      pop_id <- paste0("target_pop_", i)
      if (!is.null(input[[pop_id]])) {
        current_stored$populations[[i]] <- input[[pop_id]]
      }

      # Store cost checkbox selections
      cost_selection <- NULL
      if (!is.null(cost_titles_reactive())) {
        for (j in 1:nrow(cost_titles_reactive())) {
          checkbox_id <- paste0("cell_", i, "_", j)
          if (!is.null(input[[checkbox_id]]) && input[[checkbox_id]]) {
            cost_selection <- j

            # Ensure only one cost option is selected per row
            for (k in 1:nrow(cost_titles_reactive())) {
              if (k != j) {
                other_id <- paste0("cell_", i, "_", k)
                if (!is.null(input[[other_id]]) && input[[other_id]]) {
                  updateCheckboxInput(session, other_id, value = FALSE)
                }
              }
            }
          }
        }
      }
      current_stored$costs[[i]] <- cost_selection
    }

    stored_selections(current_stored)
  })

  output$matrix_ui <- renderUI({
    # First check if data is available
    if (!data_available()) {
      return(
        div(
          style = "padding: 1em; color: #b30000;",
          tags$h5("⚠️ No cost/Plan data uploaded"),
          tags$p("Return to the User Input tab to specify this.")
        )
      )
    }

    # Make sure we have all necessary data before proceeding
    req(cost_data(), scenario_data(), row_count())

    # Get number of rows and cost data safely
    num_rows <- row_count()
    if (num_rows <= 0) num_rows <- 1  # Safety check

    # Safely get cost titles
    cost_titles_df <- tryCatch({
      cost_titles_reactive()
    }, error = function(e) {
      message("Error getting cost titles: ", e$message)
      return(data.frame(cost_name = character(0),
                        cost_description = character(0),
                        label = character(0)))
    })

    # Safety check for cost titles
    if (nrow(cost_titles_df) == 0) {
      return(
        div(
          style = "padding: 1em; color: #b30000;",
          tags$h5("⚠️ No cost data available"),
          tags$p("Cost data appears to be empty. Please check your data files.")
        )
      )
    }

    cost_titles <- cost_titles_df$label

    # Safely get plan choices
    plan_choices <- tryCatch({
      c("Select a plan", unique(scenario_data()$scenario_name))
    }, error = function(e) {
      message("Error getting plan choices: ", e$message)
      return(c("Select a plan"))
    })

    # Get stored selections
    stored <- stored_selections()

    # Generate matrix inputs with safety checks
    matrix_inputs <- lapply(1:num_rows, function(i) {
      # Safely get stored plan
      selected_plan <- "Select a plan"
      if (!is.null(stored) && !is.null(stored$plans) && length(stored$plans) >= i) {
        if (!is.null(stored$plans[[i]])) {
          selected_plan <- stored$plans[[i]]
          # Make sure selected plan is in choices
          if (!selected_plan %in% plan_choices) {
            selected_plan <- "Select a plan"
          }
        }
      }

      row_dropdown <- selectInput(
        inputId = ns(paste0("row_option_", i)),
        label = NULL,
        choices = plan_choices,
        selected = selected_plan,
        width = "100%"
      )

      # Generate cost checkbox inputs with safety checks
      col_inputs <- lapply(seq_along(cost_titles), function(j) {
        # Default to unchecked
        is_checked <- FALSE

        # Safely check if this should be checked based on stored values
        if (!is.null(stored) && !is.null(stored$costs) && length(stored$costs) >= i) {
          if (!is.null(stored$costs[[i]]) && stored$costs[[i]] == j) {
            is_checked <- TRUE
          }
        }

        checkboxInput(ns(paste0("cell_", i, "_", j)), label = NULL, value = is_checked)
      })

      # Safely get stored population
      selected_pop <- "Whole population"
      if (!is.null(stored) && !is.null(stored$populations) && length(stored$populations) >= i) {
        if (!is.null(stored$populations[[i]])) {
          selected_pop <- stored$populations[[i]]
        }
      }

      population_dropdown <- selectInput(
        inputId = ns(paste0("target_pop_", i)),
        label = NULL,
        choices = c("Whole population", "Children u5", "Children u10", "Children u5 + Pregnant Women"),
        selected = selected_pop,
        width = "100%"
      )

      # Return the row UI with safety check for cost titles length
      tryCatch({
        tagList(
          div(
            style = "display: flex; align-items: center; margin-bottom: 10px;",
            div(style = "width: 150px;", row_dropdown),
            div(
              style = paste0("flex: 1; display: grid; grid-template-columns: repeat(", length(cost_titles) + 1, ", 1fr); gap: 10px;"),
              tagList(
                lapply(col_inputs, function(input) div(style = "text-align: center;", input)),
                div(style = "min-width: 180px;", population_dropdown)
              )
            )
          )
        )
      }, error = function(e) {
        message("Error generating row UI: ", e$message)
        # Return a simpler UI if there's an error
        div(
          style = "display: flex; align-items: center; margin-bottom: 10px;",
          div(style = "width: 150px;", row_dropdown),
          div(style = "color: red;", "Error rendering cost options"),
          div(style = "min-width: 180px;", population_dropdown)
        )
      })
    })

    # Generate header row with safety check
    header_row <- tryCatch({
      div(
        style = paste0("display: grid; margin-bottom: 10px; margin-left: 150px; grid-template-columns: repeat(", length(cost_titles) + 1, ", 1fr); gap: 10px;"),
        lapply(c(cost_titles, "ITN Target Population"), function(name) {
          div(style = "text-align: center; font-weight: bold;", HTML(name))
        })
      )
    }, error = function(e) {
      message("Error generating header row: ", e$message)
      # Return a simpler header if there's an error
      div(
        style = "margin-bottom: 10px; margin-left: 150px;",
        "Error rendering header"
      )
    })

    # Return the complete UI
    tagList(
      header_row,
      matrix_inputs,
      div(
        style = "margin-top: 20px; color: #666; font-style: italic;",
        "Note: Only one cost option can be selected per row."
      )
    )
  })

  # Add row to matrix
  observeEvent(input$add_row, {
    new_count <- row_count() + 1
    row_count(new_count)
  })

  # Process selection button
  observeEvent(input$process, {
    req(data_available())

    num_rows <- row_count()

    # Get cost information from the dataset
    cost_titles_df <- cost_titles_reactive()

    cost_titles <- cost_titles_df$label
    cost_names <- cost_titles_df$cost_name

    # Create a result data frame
    result_data <- data.frame(
      Plan = character(num_rows),
      Selected_Cost = character(num_rows),
      Target_Population = character(num_rows),
      stringsAsFactors = FALSE
    )

    for (i in 1:num_rows) {
      plan_id <- paste0("row_option_", i)
      pop_id <- paste0("target_pop_", i)

      # Get selected plan
      result_data$Plan[i] <- input[[plan_id]]

      # Get selected target population
      result_data$Target_Population[i] <- input[[pop_id]]

      # Get selected cost (the checked checkbox)
      for (j in 1:nrow(cost_titles_df)) {
        checkbox_id <- paste0("cell_", i, "_", j)
        if (!is.null(input[[checkbox_id]]) && input[[checkbox_id]]) {
          result_data$Selected_Cost[i] <- cost_names[j]
          break
        }
      }
    }

    # Update the reactive value
    matrix_selections(result_data)

    # Show the result table
    output$result_table <- renderDT({
      req(matrix_selections())
      datatable(
        matrix_selections(),
        options = list(dom = 't'),
        rownames = FALSE
      )
    })
  })

  # Budget generation
  # Process selection button
  observeEvent(input$process, {
    req(data_available())

    num_rows <- row_count()

    # Get cost information from the dataset
    cost_titles_df <- cost_titles_reactive()

    cost_titles <- cost_titles_df$label
    cost_names <- cost_titles_df$cost_name

    # Create a result data frame
    result_data <- data.frame(
      Plan = character(num_rows),
      Selected_Cost = character(num_rows),
      Target_Population = character(num_rows),
      stringsAsFactors = FALSE
    )

    for (i in 1:num_rows) {
      plan_id <- paste0("row_option_", i)
      pop_id <- paste0("target_pop_", i)

      # Get selected plan
      result_data$Plan[i] <- input[[plan_id]]

      # Get selected target population
      result_data$Target_Population[i] <- input[[pop_id]]

      # Get selected cost (the checked checkbox)
      for (j in 1:nrow(cost_titles_df)) {
        checkbox_id <- paste0("cell_", i, "_", j)
        if (!is.null(input[[checkbox_id]]) && input[[checkbox_id]]) {
          result_data$Selected_Cost[i] <- cost_names[j]
          break
        }
      }
    }

    # Update the reactive value
    matrix_selections(result_data)

    # Show the result table
    output$result_table <- renderDT({
      req(matrix_selections())
      datatable(
        matrix_selections(),
        options = list(dom = 't'),
        rownames = FALSE
      )
    })
  })

  # generate budgets
  observeEvent(input$generate_budgets, {
    req(matrix_selections(), data_available())

    # Show loading indicator
    budget_generating(TRUE)
    shinyjs::show("loading_container")

    # Process selections for budget generation
    budget_inputs <- process_selections_for_budget(
      matrix_selections(),
      scenario_data(),
      cost_data()
    )

    # Only continue if we have valid selections
    if (nrow(budget_inputs) == 0) {
      showNotification("No valid combinations to process", type = "warning")
      budget_generating(FALSE)
      shinyjs::hide("loading_container")
      return()
    }

    # Create a progress notification
    withProgress(
      message = "Generating budgets",
      detail = "This may take a while...",
      value = 0,
      {
        # Run generate_budget for each combination and collect results
        all_budgets <- list()
        budget_details <- list() # Store details for each budget

        for (i in 1:nrow(budget_inputs)) {
          # Update progress
          incProgress(
            1/nrow(budget_inputs),
            detail = paste("Processing budget", i, "of", nrow(budget_inputs))
          )

          # Extract data for this combination
          scen_data <- budget_inputs$scen_data[[i]]
          cost_option_data <- budget_inputs$cost_option_data[[i]]
          target_pop_type <- budget_inputs$Target_Population[i]

          # Generate the budget
          tryCatch({
            # Call the generate_budget function you provided
            budget_result <- generate_budget(scen_data, cost_option_data)

            # Extract years from the budget result
            years <- sort(unique(budget_result$year))
            years_string <- paste(years, collapse = ", ")

            # Add metadata for identification
            budget_result <- budget_result %>%
              mutate(
                source_scenario = budget_inputs$Plan[i],
                source_cost = budget_inputs$Selected_Cost[i],
                generation_date = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
              )

            # Add to results
            all_budgets[[i]] <- budget_result
            budget_details[[i]] <- list(
              scenario = budget_inputs$Plan[i],
              cost = budget_inputs$Selected_Cost[i],
              years = years_string,
              combination = paste(budget_inputs$Plan[i], "with", budget_inputs$Selected_Cost[i])
            )
          }, error = function(e) {
            showNotification(
              paste("Error generating budget for", budget_inputs$Plan[i], ":", e$message),
              type = "error",
              duration = 10
            )
          })
        }

        # Combine all budget results
        if (length(all_budgets) > 0) {
          combined_budgets <- bind_rows(all_budgets)

          # Extract details for budget history
          combinations <- sapply(budget_details, function(x) x$combination)
          all_years <- unique(unlist(lapply(budget_details, function(x) strsplit(x$years, ", ")[[1]])))
          all_years_string <- paste(sort(all_years), collapse = ", ")

          # Create formatted combinations for display
          formatted_combinations <- paste(combinations, collapse = " | ")

          # Save to file
          timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
          save_path <- file.path("generated", paste0("budgets_", timestamp, ".rds"))
          saveRDS(combined_budgets, save_path)

          # Update budget history with more detailed information
          current_history <- budget_history()
          new_entry <- data.frame(
            date_generated = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
            num_budgets = length(all_budgets),
            scenario_cost_combinations = formatted_combinations,
            years_generated = all_years_string,
            file_path = save_path,
            stringsAsFactors = FALSE
          )

          updated_history <- bind_rows(current_history, new_entry)
          budget_history(updated_history)

          # Save history to file
          saveRDS(updated_history, "generated/budget_history.rds")

          # Make budgets available to other tabs
          budget_results(combined_budgets)

          # IMPORTANT: Update these shared values
          shared$budget_results <- combined_budgets
          shared$budget_results_available <- TRUE

          # Add diagnostics
          message("Budget data shared: ", nrow(combined_budgets), " rows of data available")
          message("Budget data columns: ", paste(names(combined_budgets), collapse=", "))

          # Force a reactive invalidation to ensure tab3 updates
          shared$refresh_trigger <- shared$refresh_trigger + 1

          # Show success notification
          showNotification(
            paste("Successfully generated", length(all_budgets), "budgets"),
            type = "message"
          )
        }
      }
    )

    # Hide loading indicator
    budget_generating(FALSE)
    shinyjs::hide("loading_container")
  })

  # Render budget history table - Updated to display the new format
  output$budget_history_table <- renderDT({
    req(budget_history())

    # Format the table for display
    df <- budget_history()
    if ("file_path" %in% names(df)) {
      df <- df %>% select(-file_path)
    }

    # Format the table with better column names and organization
    datatable(
      df,
      colnames = c(
        "Date Generated" = "date_generated",
        "Number of Budgets" = "num_budgets",
        "Scenario-Cost Combinations" = "scenario_cost_combinations",
        "Years" = "years_generated"
      ),
      options = list(
        pageLength = 5,
        dom = 'ftip',
        order = list(list(0, 'desc')),
        columnDefs = list(
          list(className = 'dt-center', targets = c(0, 1, 3)),
          list(width = '45%', targets = 2)
        )
      ),
      rownames = FALSE
    )
  })

  # Return reactive values that might be needed by other modules
  return(list(
    budget_results = budget_results
  ))
}


