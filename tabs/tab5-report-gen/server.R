tab5Server <- function(input, output, session, shared) {
  ns <- session$ns

  # -- Extract reactive budget results --
  budget_data <- reactive({
    req(shared$budget_results)
    shared$budget_results
  })

  # -- UI Elements: Plan, Year, Currency selectors --
  output$plan_ui <- renderUI({
    req(budget_data())
    choices <- sort(unique(budget_data()$source_scenario))
    selectInput(ns("plan_select"), "Select Plan(s)", choices = choices, multiple = TRUE)
  })

  output$year_ui <- renderUI({
    req(budget_data())
    choices <- sort(unique(budget_data()$year))
    selectInput(ns("year_select"), "Select Year", choices = c(choices))
  })

  output$currency_ui <- renderUI({
    req(budget_data())
    choices <- sort(unique(budget_data()$currency))
    selectInput(ns("currency_select"), "Select Currency", choices = choices)
  })

  # -- Enable/disable buttons based on input validation --
  observe({
    all_selected <- !is.null(input$plan_select) &&
      length(input$plan_select) > 0 &&
      !is.null(input$year_select) &&
      input$year_select != "" &&
      !is.null(input$currency_select) &&
      input$currency_select != ""

    shinyjs::toggleState("download_report", condition = all_selected)
    shinyjs::toggleState("download_figures", condition = all_selected)
    shinyjs::toggleState("download_data", condition = all_selected)
  })

  # -- Report download handler --
  output$download_report <- downloadHandler(
    filename = function() {
      plan_text <- paste(input$plan_select, collapse = "_")
      paste0(plan_text, "-report-summary-", input$year_select, "-", Sys.Date(), ".docx")
    },
    content = function(file) {
      template_path <- file.path("global", "report-template.Rmd")
      req(file.exists(template_path))

      temp <- tempdir()
      temp_report <- file.path(temp, "report_template.Rmd")
      file.copy(template_path, temp_report, overwrite = TRUE)
      file.copy(file.path("global", "test.png"), file.path(temp, "test.png"))
      file.copy(file.path("global", "test-2.png"), file.path(temp, "test-2.png"))

      rmarkdown::render(
        temp_report,
        output_format = "word_document",
        output_file = file,
        params = list(
          report_title    = input$report_title,
          authors_list    = input$authors_list,
          plan_select     = input$plan_select,
          year_select     = input$year_select,
          currency_select = input$currency_select,
          lga_outline     = lga_outline,
          state_outline   = state_outline,
          national_budget = budget_data()
        ),
        envir = new.env(parent = globalenv())
      )
    }
  )

  # -- Figures download handler --
  output$download_figures <- downloadHandler(
    filename = function() {
      paste0("figures-", paste(input$plan_select, collapse = "_"), "-", Sys.Date(), ".zip")
    },
    content = function(file) {
      withProgress(message = 'Generating Figures', value = 0, {
        incProgress(0.1)
        tmp_dir <- tempfile("figures")
        dir.create(tmp_dir)

        plots <- generate_plots(
          plans = input$plan_select,
          year = input$year_select,
          currency = input$currency_select
        )

        plot_files <- c()
        for (plot_name in names(plots)) {
          plot_file <- file.path(tmp_dir, paste0(plot_name, ".png"))
          png(plot_file, width = 800, height = 600)
          print(plots[[plot_name]])
          dev.off()
          plot_files <- c(plot_files, plot_file)
        }

        utils::zip(zipfile = file, files = plot_files, flags = "-j")
      })
    },
    contentType = "application/zip"
  )

  # -- Raw budget data download handler --
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("budget-data-", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      data_to_write <- budget_data() |>
        filter(
          source_scenario %in% input$plan_select,
          currency == input$currency_select,
          if (input$year_select != "All Years") year == as.numeric(input$year_select) else TRUE
        )

      openxlsx::write.xlsx(data_to_write, file)
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )
}
