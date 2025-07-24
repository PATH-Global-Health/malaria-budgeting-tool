tab4UI <- function(id) {
  ns <- NS(id)

  fluidPage(
    div(
      style = "background-color: #fff3cd; border-left: 6px solid #ffa500; padding: 15px; margin-bottom: 20px;",
      strong(head_bold),
      main_text
    ),
    card(
      layout_sidebar(
        sidebar = sidebar(
          width = 300,
          # instructions pop up
          actionButton(ns("show_instructions"), "📘 Instructions détaillées", class = "btn-info"),
          uiOutput(ns("plan_bl_select_ui")),
          uiOutput(ns("remaining_plan_select")),
          uiOutput(ns("year_select_ui")),
          selectInput(
            ns("currency_select"),
            "Sélectionnez la devise:",
            choices = c("", "USD", "CDF"),
            selected = ""
          ),
          actionButton(
            ns("clear_inputs"),
            "Effacer les sélections",
            icon = icon("eraser"),
            class = "btn-secondary"
          )
        ),

        # main conetent
        uiOutput(ns("page_description")),
        # uiOutput(ns("maps_ui")),
        (uiOutput(ns("budget_comps"))),
        (uiOutput(ns("budget_item_plots"))),
        (uiOutput(ns("budget_tables"))),
        (uiOutput(ns("budget_tables_comp")))
      )
    )
  )
}
