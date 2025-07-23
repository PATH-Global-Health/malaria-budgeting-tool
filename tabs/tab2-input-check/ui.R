tab2UI <- function(id) {
  ns <- NS(id)

  fluidPage(

    # warning text for demo
    div(
      style = "background-color: #fff3cd; border-left: 6px solid #ffa500; padding: 15px; margin-bottom: 20px;",
      strong(head_bold),
      main_text
    ),

    # Sidebar card with user inputs, instructions and then table to the rights
    card(
      min_height = 500,
      # sidebar layout with sidebar and card body
      layout_sidebar(

        # add the sidebar with input elements
        sidebar = sidebar(
          width = "400px",

          # Instructions pop up
          actionButton(ns("show_instructions"), "📘 Instructions détaillées", class = "btn-info"),

          # Dynamic Plan Selection
          uiOutput(ns("plan_select_ui")),

          # Dynamic Year Selection
          uiOutput(ns("year_select_ui")),

          # Clear button
          div(
            style = "display: flex; align-items: flex-end;",
            actionButton(
              ns("clear_inputs"),
              "Effacer les sélections",
              icon = icon("eraser"),
              class = "btn-secondary",
              width = "100%"
            )
          )
        ),

        # Value boxes to show coverage statistics
        uiOutput(ns("coverage_summary")),

        # Output card with tabs for each intervention
        shinycssloaders::withSpinner(uiOutput(ns("intervention_tabs")), type = 1)
      )
    ),
  )
}
