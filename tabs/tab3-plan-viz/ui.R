# UI of Tab 3
tab3UI <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Plan Visualization"),
    h5("This tab provides key summaries of the generated budgets including tables and figures along with summary statistics. Outputs are populated once the user specifies each of the inputs below."),
    br(),
    h5("Data values are examples generated using evolving methodology and meant for demonstration and not decision making."),

    # User input selections grouped into a card
    card(
      card_header("User Inputs"),
      card_body(
        layout_column_wrap(
          width = 1/4, # Wrap items into four columns

          # Plan Selection
          selectInput(
            ns("plan_select"),
            "Select the Plan:",
            choices = c("", unique_plans),
            selected = ""
          ),

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

          # Year Selection
          selectInput(
            ns("year_select"),
            "Select Years of Interest:",
            choices = c("", years_to_select, "All Years"),
            selected = ""
          ),

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
    uiOutput(ns("value_boxes")),

    # Table summarising Elemental costs
    uiOutput(ns("budget_table_card")),

    # Additional Charts
    uiOutput(ns("cost_charts"))

  )
}
