library(shiny)

ui <- fluidPage(
  titlePanel("Matrice de sélection"),
  p("Sélectionnez les options dans les listes déroulantes, sélectionnez une option de coût par ligne et cliquez sur le bouton << Sélection de processus >> pour confirmer vos choix."),
  uiOutput("matrix_ui")
)

server <- function(input, output, session) {
  ns <- NS("matrix")

  row_count <- reactiveVal(1) # fixed 1 row for test, change if needed

  plan_choices <- c("Sélectionnez un plan", "Plan 1 - BAU", "Plan 2 - Intensif")
  cost_choices <- c("Sélectionnez les coûts", "Coût 1", "Coût 2")
  assumption_choices <- c("Sélectionnez une hypothèse", "Accepter la ligne de base", "Faire des ajustements")
  assumption_vars <- c(
    "ITN campaign people per net", "ITN routine coverage",
    "IPTp doses", "IPTp coverage", "SMC cycles", "SMC age targeting",
    "PMC coverage", "Vaccination coverage", "Vaccination doses"
  )

  adjustment_store <- reactiveValues()

  # UI rendering
  output$matrix_ui <- renderUI({
    tagList(
      div(
        style = "display: table; width: 100%; border-collapse: collapse;",
        div(
          style = "display: table-row;",
          div(style = "display: table-cell; font-weight: bold; padding: 5px;", "Spécifications du Budget 1"),
          div(style = "display: table-cell; padding: 5px;", selectInput("row_plan_1", NULL, plan_choices)),
          div(style = "display: table-cell; padding: 5px;", selectInput("row_cost_1", NULL, cost_choices)),
          div(style = "display: table-cell; padding: 5px;", selectInput("row_assumption_1", NULL, assumption_choices)),

          # Show adjustment column only if selected
          if (!is.null(input$row_assumption_1) && input$row_assumption_1 == "Faire des ajustements") {
            div(
              style = "display: table-cell; padding: 5px;",
              tagList(
                selectInput("row_param_1", "Parameter to adjust", assumption_vars),
                uiOutput("dynamic_input_1"),
                actionButton("submit_adjust_1", "Add Adjustment"),
                uiOutput("summary_inline_1")
              )
            )
          }
        )
      ),
      tags$hr(),
      tags$p(style = "font-style: italic;", "Remarque: sélectionnez le plan, les coûts et les hypothèses pour chaque ligne.")
    )
  })

  # Dynamic input
  output$dynamic_input_1 <- renderUI({
    param <- input$row_param_1
    if (is.null(param)) {
      return(NULL)
    }
    switch(param,
      "ITN campaign people per net" = numericInput("adj_val_1", "New people per net:", 1.8, min = 1, step = 0.1),
      "ITN routine coverage" = sliderInput("adj_val_1", "New coverage %:", 0, 100, 30),
      "IPTp doses" = numericInput("adj_val_1", "New doses:", 3, min = 1),
      "IPTp coverage" = sliderInput("adj_val_1", "New coverage %:", 0, 100, 80),
      "SMC cycles" = numericInput("adj_val_1", "New cycles:", 4, min = 1),
      "SMC age targeting" = textInput("adj_val_1", "New age proportions:", "0.18,0.77"),
      "PMC coverage" = sliderInput("adj_val_1", "New coverage %:", 0, 100, 85),
      "Vaccination coverage" = sliderInput("adj_val_1", "New coverage %:", 0, 100, 84),
      "Vaccination doses" = numericInput("adj_val_1", "New doses:", 4, min = 1)
    )
  })

  # Add adjustment
  observeEvent(input$submit_adjust_1, {
    req(input$row_param_1, input$adj_val_1)
    message("DEBUG: Added adjustment for row 1 - ", input$row_param_1, " = ", input$adj_val_1)
    if (is.null(adjustment_store$row_1)) adjustment_store$row_1 <- list()
    adjustment_store$row_1 <- append(adjustment_store$row_1, list(paste0(input$row_param_1, " = ", input$adj_val_1)))
  })

  # Summary inline
  output$summary_inline_1 <- renderUI({
    if (!is.null(adjustment_store$row_1)) {
      tagList(
        tags$b("Adjustments:"),
        tags$ul(lapply(adjustment_store$row_1, function(x) tags$li(x)))
      )
    }
  })
}

shinyApp(ui, server)
