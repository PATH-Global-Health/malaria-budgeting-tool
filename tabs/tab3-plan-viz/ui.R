tab3UI <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      style = "background-color: #fff3cd; border-left: 6px solid #ffa500; padding: 15px; margin-bottom: 20px;",
      strong(head_bold),
      main_text
    ),

    # 📦 Actual page layout with sidebar
    page_sidebar(
      # title = "Visualisations budgétaires",
      sidebar = sidebar(
        width = 300,
        # instructions pop up
        actionButton(ns("show_instructions"), "📘 Instructions détaillées", class = "btn-info"),
        actionButton(
          ns("reload_budget_data"),
          "Recharger les données budgétaires",
          icon = icon("sync"),
          class = "btn-info",
          title = "Cliquez pour forcer l'actualisation des données budgétaires si les scénarios nouvellement générés ne sont pas encore disponibles"
        ),
        uiOutput(ns("plan_select_ui")),
        selectInput(
          ns("spatial_scale"),
          "Sélectionner l'échelle spatiale:",
          choices = c("", "National", "Province", "Zone de santé"),
          selected = ""
        ),
        uiOutput(ns("adm1_ui")),
        uiOutput(ns("adm2_ui")),
        uiOutput(ns("year_select_ui")),
        selectInput(
          ns("currency_select"),
          "Sélectionnez la devise:",
          choices = c("", "USD", "CDF"),
          selected = ""
        ),
        uiOutput(ns("budget_envelope_ui")),
        actionButton(
          ns("clear_inputs"),
          "Effacer les sélections",
          icon = icon("eraser"),
          class = "btn-secondary"
        )
      ),
      # Main panel content
      uiOutput(ns("page_description")),
      uiOutput(ns("maps_ui")),
      withSpinner(uiOutput(ns("value_boxes"))),
      uiOutput(ns("budget_table_card")),
      uiOutput(ns("cost_charts"))
    )
  )
}
