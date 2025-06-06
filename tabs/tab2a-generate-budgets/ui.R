tab2aUI <- function(id) {
  ns <- NS(id)
  page_sidebar(
    title = "Budget Assumption selections",

    div(
      style = "background-color: #fff3cd; border-left: 6px solid #ffa500; padding: 15px; margin-bottom: 20px;",
      strong("IMPORTANT: This is a demonstration version of the tool. "),
      "Data uploading functionality has been suspended. The values and outputs presented here are illustrative only, intended to showcase the tool's features. They should not be used for any decision-making or extrapolation.
      In addition, the data presented here is not representative of any real-world scenarios or costs.

      Our tool is in active development and therefore the version presented here is meant to be illustrative of the types of functionality that we are building out. We still have many features in progress and can't wait to share in the near future. Please reach out to hthompson@path.org with any suggestions or feedback you may have too we'd love to gain any insights from our community!",
    ),
    sidebar = sidebar(
      width = 300,
      h4("Make Your Selections"),
      p("Select options from the dropdowns, select one cost option per row, and click the Process Selection button to confirm your choices."),
      hr(),
      p("We are building in functionality to alter the quantification assumptions used in the tool and the ITN target population column here is an example of that. For each intervention we intend for the user to be able to accept the default implementation or make
        custom adjustments. For example we currently have assumed, 4 doses of malaria vaccine to be delivered with 10% procurement buffer and these aspects can be adjusted if desired in future versions."),


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
                 actionButton(
                   ns("generate_budgets"), "Generate Budgets",
                   class = "btn-success",
                   icon = icon("calculator"),
                   width = "100%",
                   `disabled` = if (lite_mode) NA else NULL
                 ),
                 if (lite_mode) {
                   tags$div(style = "color: #cc0000; font-size: 0.9em; margin-top: 5px;",
                            "⚠️ This feature is disabled in the demonstration version.")
                 }
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
