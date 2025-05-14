tab2aUI <- function(id) {
  ns <- NS(id)
  page_sidebar(
    title = "Budget Assumption selections",
    sidebar = sidebar(
      width = 300,
      h4("Make Your Selections"),
      p("Select options from the dropdowns, check the appropriate unit costs, and click the Process button to analyze the results."),
      hr(),
      actionButton(ns("add_row"), "Add Row", class = "btn-primary", width = "100%"),
      hr(),
      actionButton(ns("process"), "Process Selections", class = "btn-success", width = "100%")
    ),

    card(
      card_header("Selection Matrix"),
      card_body(
        uiOutput(ns("matrix_ui"))
      )
    ),

    card(
      card_header("Results"),
      card_body(
        verbatimTextOutput(ns("results")),
        DTOutput(ns("result_table"))
      )
    )
  )
}
