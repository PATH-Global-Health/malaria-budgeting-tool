tab2UI <- function(id) {
  ns <- NS(id)

  fluidPage(
    titlePanel("Intervention Targeting Check Point"),

    # Instructions pop up
    actionButton(ns("show_instructions"), "📘 Detailed Instructions", class = "btn-info"),

    # User input selections grouped into a card
    card(
      card_header("User Inputs"),
      height = "auto",
      min_height = "250px", # Increase minimum height for the card
      card_body(
        layout_column_wrap(
          width = 1/3, # Wrap items into three columns

          # Dynamic Plan Selection
          uiOutput(ns("plan_select_ui")),

          # Dynamic Year Selection
          uiOutput(ns("year_select_ui")),

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
    shinycssloaders::withSpinner(uiOutput(ns("intervention_tabs")), type = 1)
  )
}


