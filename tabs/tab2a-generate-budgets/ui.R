tab2aUI <- function(id) {
  ns <- NS(id)
  page_sidebar(
    title = "Budget Assumption selections",
    sidebar = sidebar(
      width = 300,
      h4("Make Your Selections"),
      p("Select options from the dropdowns, select one cost option per row, and click the Process Selection button to confirm your choices."),
      hr(),
      actionButton(ns("add_row"), "Add Row", class = "btn-primary", width = "100%"),
      hr(),
      actionButton(ns("process"), "Process Selection", class = "btn-primary", icon = icon("check-circle"), width = "100%")
    ),

    card(
      card_header("Selection Matrix"),
      card_body(
        useShinyjs(),
        div(id = ns("matrix_container"), uiOutput(ns("matrix_ui")))
      )
    ),

    card(
      card_header("Selection Results"),
      card_body(
        DTOutput(ns("result_table"))
      )
    ),

    card(
      card_header("Budget Generation"),
      card_body(
        p("Click the button below to generate budgets for all selected combinations of plans and costs:"),
        fluidRow(
          column(8,
                 actionButton(ns("generate_budgets"), "Generate Budgets",
                              class = "btn-success",
                              icon = icon("calculator"),
                              width = "100%")
          ),
          column(4,
                 # Loading indicator that shows during budget generation
                 div(
                   id = ns("loading_container"),
                   style = "display: none;",
                   span(
                     span(class = "fa fa-spinner fa-spin fa-2x"),
                     span(style = "margin-left: 10px;", "Generating budgets, please wait...")
                   )
                 )
          )
        )
      )
    ),

    card(
      card_header("Budget Generation History"),
      card_body(
        DTOutput(ns("budget_history_table"))
      )
    )
  )
}
