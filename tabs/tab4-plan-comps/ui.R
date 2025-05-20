tab4UI <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Plan Comparisons"),
    ("This tab allows the user to compare budget summaries directly for different plans. Start by selecting a 'Baseline' plan to compare against and filling in the remaining inputs. Budget comparisons are generated at the National Level only"),
    br(),
    ("Data values are examples generated using evolving methodology and meant for demonstration and not decision making."),

    card(
      card_header("User Inputs"),
      card_body(
        layout_column_wrap(
          width = 1/4,

          uiOutput(ns("plan_bl_select_ui")),
          uiOutput(ns("remaining_plan_select")),
          uiOutput(ns("year_select_ui")),

          selectInput(
            ns("currency_select"),
            "Select Currency:",
            choices = c("", "USD", "NGN"),
            selected = ""
          )
        ),
        br(),
        actionButton(
          ns("clear_inputs"),
          "Clear Selections",
          icon = icon("eraser"),
          class = "btn-danger"
        )
      )
    ),

    br(),
    uiOutput(ns("page_description")),
    uiOutput(ns("maps_ui")),
    uiOutput(ns("budget_comps")),
    uiOutput(ns("budget_tables")),
    uiOutput(ns("budget_tables_comp")),
    uiOutput(ns("budget_item_plots"))
  )
}
