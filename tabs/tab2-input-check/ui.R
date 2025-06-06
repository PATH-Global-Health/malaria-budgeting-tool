tab2UI <- function(id) {
  ns <- NS(id)

  fluidPage(
    titlePanel("Intervention Targeting Check Point"),

    div(
      style = "background-color: #fff3cd; border-left: 6px solid #ffa500; padding: 15px; margin-bottom: 20px;",
      strong("IMPORTANT: This is a demonstration version of the tool. "),
      "Data uploading functionality has been suspended. The values and outputs presented here are illustrative only, intended to showcase the tool's features. They should not be used for any decision-making or extrapolation.
      In addition, the data presented here is not representative of any real-world scenarios or costs.

      Our tool is in active development and therefore the version presented here is meant to be illustrative of the types of functionality that we are building out. We still have many features in progress and can't wait to share in the near future. Please reach out to hthompson@path.org with any suggestions or feedback you may have too we'd love to gain any insights from our community!",
    ),

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


