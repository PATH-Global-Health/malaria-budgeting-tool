tab2UI <- function(id) {
  ns <- NS(id)

  fluidPage(
    titlePanel("Intervention Coverage by LGA"),
    h3("Plan and Intervention Analysis"),

    # User input selections grouped into a card
    card(
      card_header("User Inputs"),
      height = "auto",
      min_height = "250px", # Increase minimum height for the card
      card_body(
        layout_column_wrap(
          width = 1/3, # Wrap items into three columns

          # Plan Selection
          div(
            style = "position: relative; z-index: 1002;", # Higher z-index
            selectInput(
              ns("plan_select"),
              "Select the Plan:",
              choices = c("", unique_plans2),
              selected = "",
              multiple = FALSE
            )
          ),

          # Year Selection
          div(
            style = "position: relative; z-index: 1001;", # High z-index
            selectInput(
              ns("year_select"),
              "Select Years of Interest:",
              choices = c("", years_to_select, "All Years"),
              selected = ""
            )
          ),

          # Clear button
          div(
            style = "display: flex; align-items: flex-end;",
            actionButton(
              ns("clear_inputs"),
              "Clear Selections",
              icon = icon("eraser"),
              class = "btn-danger"
            )
          )
        )
      )
    ),

    # PMC and SMC warning area
    uiOutput(ns("smc_pmc_warning")),

    # Value boxes to show coverage statistics
    uiOutput(ns("coverage_summary")),

    # Output card with tabs for each intervention
    uiOutput(ns("intervention_tabs"))
  )
}
