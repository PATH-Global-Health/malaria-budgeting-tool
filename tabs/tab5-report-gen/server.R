tab5Server <- function(input, output, session) {
  ns <- session$ns

  #-Observe universal inputs and enable/disable download buttons-----------------------------
  observe({
    all_selected <- (
      length(input$plan_select) > 0 &&
        input$plan_select[1] != "" &&  # Check that the first selected value is not ""
        input$year_select != "" &&
        input$currency_select != ""
    )

    if (all_selected) {
      shinyjs::enable("download_report")
      shinyjs::enable("download_figures")
      shinyjs::enable("download_data")
    } else {
      shinyjs::disable("download_report")
      shinyjs::disable("download_figures")
      shinyjs::disable("download_data")
    }
  })

  #-Report Download------------------------------------------------------------------------
  output$download_report <- downloadHandler(
    filename = function() {
      # Collapse multiple plans into a single string for the filename
      plan_text <- paste(input$plan_select, collapse = "_")
      paste0(
        plan_text,
        "-report-summary-for-",
        input$year_select,
        "-date-generated-",
        Sys.Date(),
        ".docx"
      )
    },
    content = function(file) {
      # Define the path to the R Markdown template
      template_path <- file.path("global", "report-template.Rmd")

      if (!file.exists(template_path)) {
        stop("Template file not found: ", template_path)
      }

      temp <- tempdir()

      # Copy the template to a temporary file location
      tempReport <- file.path(temp, "report_template.Rmd")
      file.copy(template_path, tempReport, overwrite = TRUE)

      # Copy test figures to temporary file location
      file.copy(file.path("global", "test.png"), file.path(temp, "test.png"))
      file.copy(file.path("global", "test-2.png"), file.path(temp, "test-2.png"))

      # Determine the output format (currently fixed to Word)
      output_format <- "word_document"

      # Render the report using rmarkdown::render with passed parameters
      rmarkdown::render(
        tempReport,
        output_format = output_format,
        output_file = file,
        params = list(
          report_title   = input$report_title,
          authors_list   = input$authors_list,
          plan_select    = input$plan_select,
          year_select     = input$year_select,
          lga_outline    = lga_outline,
          state_outline  = state_outline,
          national_budget  = national_budget
        ),
        envir = new.env(parent = globalenv())
      )
    }
  )



  #-Figures Download Handler--------------------------------------------------------------
  output$download_figures <- downloadHandler(
    filename = function() {
      paste0("figures-", input$plan_select, "-", Sys.Date(), ".zip")
    },
    content = function(file) {
      withProgress(message = 'Generating Figures', value = 0, {
        # Step 1: Create a temporary directory
        incProgress(0.1, detail = "Creating temporary folder")
        tmp_dir <- tempfile("figures")
        dir.create(tmp_dir)

        # Step 2: Generate the plots based on current inputs
        # just using the static intervention map as an
        # example
        incProgress(0.2, detail = "Generating plots")
        plots <-
          generate_plots(
            input$plan_select,
            input$year_select,
            input$currency_select
            )

        # Step 3: Save each plot as a PNG file
        incProgress(0.2, detail = "Saving plots as PNG files")
        plot_files <- c()
        n <- length(plots)
        for (plot_name in names(plots)) {
          plot_file <- file.path(tmp_dir, paste0(plot_name, ".png"))
          png(filename = plot_file, width = 800, height = 600)
          print(plots[[plot_name]])
          dev.off()
          plot_files <- c(plot_files, plot_file)
          incProgress(0.2 / n, detail = paste("Saved", plot_name))
        }

        # Step 4: Zip the PNG files
        incProgress(0.2, detail = "Zipping files")
        utils::zip(zipfile = file, files = plot_files, flags = "-j")

        incProgress(0.1, detail = "Complete")
      })
    },
    contentType = "application/zip"
  )

  #-Data Download Handler------------------------------------------------------------------
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("data-", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      # Define the path to the existing file
      existing_file <- "data/nga-demo-data-pre-processed/budgets-generated/combined-plan-budgets.xlsx"

      # Copy the existing file to the requested download location
      file.copy(existing_file, file)
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )
}
