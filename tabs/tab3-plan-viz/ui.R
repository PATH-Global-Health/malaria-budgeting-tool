# UI of Tab 3
tab3UI <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Plan Visualization"),
    ("This tab provides key summaries of the generated budgets including tables and figures along with summary statistics. Outputs are populated once the user specifies each of the inputs below."),
    br(),
    ("Data values are examples generated using evolving methodology and meant for demonstration and not decision making."),

    div(
      style = "background-color: #fff3cd; border-left: 6px solid #ffa500; padding: 15px; margin-bottom: 20px;",
      strong("IMPORTANT: This is a demonstration version of the tool. "),
      "Our tool was initially developed for the Nigeria Context and we acknowledge their support in its development. Data uploading functionality has been suspended. The values and outputs presented here are illustrative only, intended to showcase the tool's features. They should not be used for any decision-making or extrapolation.
      In addition, the data presented here is not representative of any real-world scenarios or costs.

      Our tool is in active development and therefore the version presented here is meant to be illustrative of the types of functionality that we are building out. We still have many features in progress and can't wait to share in the near future. Please reach out to hthompson@path.org with any suggestions or feedback you may have too we'd love to gain any insights from our community!",

    ),

    # User input selections grouped into a card
    card(
      card_header("User Inputs"),
      card_body(
        layout_column_wrap(
          width = 1/4, # Wrap items into four columns

          actionButton(ns("reload_budget_data"), "Reload Budget Data",
                       icon = icon("sync"), class = "btn-info"),

          # Plan Selection - now using uiOutput for dynamic choices
          uiOutput(ns("plan_select_ui")),

          # Spatial Scale Selection
          selectInput(
            ns("spatial_scale"),
            "Select Spatial Scale:",
            choices = c("", "National", "State", "LGA"),
            selected = ""
          ),

          # Conditional UI for State Selection
          uiOutput(ns("state_ui")),

          # Conditional UI for LGA Selection
          uiOutput(ns("lga_ui")),

          # Year Selection - now using uiOutput for dynamic choices
          uiOutput(ns("year_select_ui")),

          # Currency Selection
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

    # Dynamic text output that appears after inputs are selected
    uiOutput(ns("page_description")),

    # Maps Display
    uiOutput(ns("maps_ui")),

    # Ribbon values
    withSpinner(uiOutput(ns("value_boxes"))),

    # Table summarising Elemental costs
    uiOutput(ns("budget_table_card")),

    # Additional Charts
    uiOutput(ns("cost_charts"))
  )
}
