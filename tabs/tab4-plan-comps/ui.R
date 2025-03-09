tab4UI <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Plan Comparisons"),
    h5("This tab allows the user to compare budget summaries directly for different plans. Start by selecting a 'Baseline' plan to compare against and filling in the remaining inputs. Budget comparisons are generated at the National Level only"),
    br(),
    h5("Data values are examples generated using evolving methodology and meant for demonstration and not decision making."),

    # User input selections grouped into a card
    card(
      card_header("User Inputs"),
      card_body(
        layout_column_wrap(

          # Plan Selection
          selectInput(
            ns("plan_bl_select"),
            "Select the Baselinne Plan:",
            choices = c("", plan_labels),
            selected = ""
          ),

          # Conditional UI for remaining selection
          uiOutput(ns("remaining_plan_select")),

          # Year Selection
          selectInput(
            ns("year_select"),
            "Select Years of Interest:",
            choices = c("", years_to_select,  "All Years"),
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

    # First show the map for each plan selected
    uiOutput(ns("maps_ui")),

    # Budget comparison plots
    uiOutput(ns("budget_comps")),

    # Budget Tables
    uiOutput(ns("budget_tables")),

    # Budget Tables comps
    uiOutput(ns("budget_tables_comp")),

    # Budget figure comp
    uiOutput(ns("budget_item_plots"))


  )
}
