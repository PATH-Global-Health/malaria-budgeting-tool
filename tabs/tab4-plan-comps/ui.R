tab4UI <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Plan Comparisons"),
    ("This tab allows the user to compare budget summaries directly for different plans. Start by selecting a 'Baseline' plan to compare against and filling in the remaining inputs. Budget comparisons are generated at the National Level only"),
    br(),
    ("Data values are examples generated using evolving methodology and meant for demonstration and not decision making."),

    div(
      style = "background-color: #fff3cd; border-left: 6px solid #ffa500; padding: 15px; margin-bottom: 20px;",
      strong("IMPORTANT: This is a demonstration version of the tool. "),
      "Our tool was initially developed for the Nigeria Context and we acknowledge their support in its development. Data uploading functionality has been suspended. The values and outputs presented here are illustrative only, intended to showcase the tool's features. They should not be used for any decision-making or extrapolation.
      In addition, the data presented here is not representative of any real-world scenarios or costs.

      Our tool is in active development and therefore the version presented here is meant to be illustrative of the types of functionality that we are building out. We still have many features in progress and can't wait to share in the near future. Please reach out to hthompson@path.org with any suggestions or feedback you may have too we'd love to gain any insights from our community!",

    ),

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
    withSpinner(uiOutput(ns("budget_comps"))),
    withSpinner(uiOutput(ns("budget_tables"))),
    withSpinner(uiOutput(ns("budget_tables_comp"))),
    withSpinner(uiOutput(ns("budget_item_plots")))
  )
}
