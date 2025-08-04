tab3UI <- function(id) {
  #-NAMESPACING the module----------------------------
  ns <- NS(id)

  #-MAIN PAGE CONTENT---------------------------------
  fluidPage(

    # Yellow message banner at the top (alert-style)
    div(
      style = "background-color: #fff3cd; border-left: 6px solid #ffa500; padding: 15px; margin-bottom: 20px;",
      strong(head_bold),
      main_text
    ),

    #-SIDEBAR AND MAIN CONTENT-----------------------------
    card(

      # USER INPUT SELECTIONS
      layout_sidebar(
        sidebar = sidebar(
          width = 300,

          # instructions pop up
          actionButton(ns("show_instructions"), "📘 Instructions détaillées", class = "btn-info"),

          # force refresh button
          actionButton(
            ns("reload_budget_data"),
            "Recharger les données budgétaires",
            icon = icon("sync"),
            class = "btn-info",
            title = "Cliquez pour forcer l'actualisation des données budgétaires si les scénarios nouvellement générés ne sont pas encore disponibles"
          ),

          # budget selection dropdown
          uiOutput(ns("plan_select_ui")),

          # spatial scale drop down
          selectInput(
            ns("spatial_scale"),
            "Sélectionner l'échelle spatiale:",
            choices = c("", "National", "Province", "Zone de santé"),
            selected = ""
          ),

          # UI for province and health zone drop downs
          uiOutput(ns("adm1_ui")),
          uiOutput(ns("adm2_ui")),

          # Year selection dropdown
          uiOutput(ns("year_select_ui")),

          # Currency selection dropdown
          selectInput(
            ns("currency_select"),
            "Sélectionnez la devise:",
            choices = c("", "USD", "CDF"),
            selected = ""
          ),

          # Budget envelope selection dropdown
          uiOutput(ns("budget_envelope_ui")),

          # Clear selections
          actionButton(
            ns("clear_inputs"),
            "Effacer les sélections",
            icon = icon("eraser"),
            class = "btn-secondary"
          )
        ),
        # MAIN PANEL OUTPUT CONTENT CARDS
        (uiOutput(ns("page_description"))),
        (uiOutput(ns("maps_ui"))),
        (uiOutput(ns("value_boxes"))),
        (uiOutput(ns("budget_table_card"))),
        (uiOutput(ns("cost_charts")))
      )
    )
  )
}
