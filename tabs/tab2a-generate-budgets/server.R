tab2aServer <- function(input, output, session) {
  ns <- session$ns

  # Define column headers
  col_names <- c(
    "Cost assumption 1:<br>Current Costs",
    "Cost assumption 2:<br>Reduced commodity price",
    "ITN campaign<br>target population"
  )

  # Reactive value to track number of rows
  row_count <- reactiveVal(3)

  # Initialize the matrix UI
  output$matrix_ui <- renderUI({
    num_rows <- row_count()

    # Generate UI for each row
    matrix_inputs <- lapply(1:num_rows, function(i) {
      # Plan select
      row_dropdown <- selectInput(
        inputId = paste0("row_option_", i),
        label = NULL,
        choices = c("Select a plan", "Plan A", "Plan B", "Plan C"),
        selected = "Select a plan",
        width = "100%"
      )

      # Checkboxes
      col_inputs <- lapply(1:2, function(j) {
        input_id <- paste0("cell_", i, "_", j)
        checkboxInput(input_id, label = NULL, value = FALSE)
      })

      # Population group dropdown
      population_dropdown <- selectInput(
        inputId = paste0("target_pop_", i),
        label = NULL,
        choices = c(
          "Whole population",
          "Children u5",
          "Children u10",
          "Children u5 + Pregnant Women"
        ),
        selected = "Whole population",
        width = "100%"
      )

      # One row with plan select, two checkboxes, and one population dropdown
      tagList(
        div(
          style = "display: flex; align-items: center; margin-bottom: 10px;",
          div(style = "width: 150px;", row_dropdown),
          div(style = "flex: 1; display: flex; gap: 10px;",
              div(style = "flex: 1; text-align: center;", col_inputs[[1]]),
              div(style = "flex: 1; text-align: center;", col_inputs[[2]]),
              div(style = "flex: 2; min-width: 180px;", population_dropdown)
          )
        )
      )
    })

    # Header row
    header_row <- div(
      style = "display: flex; margin-bottom: 10px; margin-left: 150px; gap: 10px;",
      lapply(col_names, function(name) {
        div(style = "flex: 1; text-align: center; font-weight: bold;", HTML(name))
      })
    )

    # Combine header and rows
    tagList(header_row, matrix_inputs)
  })

  # Add row logic
  observeEvent(input$add_row, {
    row_count(row_count() + 1)
  })

  # Process logic
  observeEvent(input$process, {
    num_rows <- row_count()

    result_data <- data.frame(
      Plan = character(num_rows),
      `Unit Cost 1` = logical(num_rows),
      `Unit Cost 2` = logical(num_rows),
      `Target Population` = character(num_rows),
      stringsAsFactors = FALSE
    )

    for (i in 1:num_rows) {
      result_data$Plan[i] <- input[[paste0("row_option_", i)]]
      result_data$`Unit Cost 1`[i] <- input[[paste0("cell_", i, "_1")]]
      result_data$`Unit Cost 2`[i] <- input[[paste0("cell_", i, "_2")]]
      result_data$`Target Population`[i] <- input[[paste0("target_pop_", i)]]
    }

    # Render summary
    output$results <- renderPrint({
      cat("Selection Matrix Results:\n\n")
      print(result_data)

      cat("\nSummary:\n")
      cat("Total plans selected:", nrow(result_data), "\n")
      cat("Total Unit Cost 1 selections:", sum(result_data$`Unit Cost 1`), "\n")
      cat("Total Unit Cost 2 selections:", sum(result_data$`Unit Cost 2`), "\n")

      cat("\nTarget Population Breakdown:\n")
      print(table(result_data$`Target Population`))
    })

    # Render DT table
    output$result_table <- renderDT({
      display_data <- result_data
      display_data$`Unit Cost 1` <- ifelse(display_data$`Unit Cost 1`, "Yes", "No")
      display_data$`Unit Cost 2` <- ifelse(display_data$`Unit Cost 2`, "Yes", "No")

      datatable(
        display_data,
        options = list(pageLength = 15, dom = 't'),
        rownames = FALSE
      )
    })
  })
}
