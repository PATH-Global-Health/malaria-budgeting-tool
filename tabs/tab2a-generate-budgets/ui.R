tab2aUI <- function(id) {
  ns <- NS(id)

  page_fluid(
    # warning message
    div(
      style = "background-color: #fff3cd; border-left: 6px solid #ffa500; padding: 15px; margin-bottom: 20px;",
      strong(head_bold),
      main_text
    ),

    # delete function handler
    tags$script(HTML("
  $(document).on('click', '.delete-budget-btn', function() {
    var filePath = $(this).data('path');
    var scen = $(this).data('scen');
    var cost = $(this).data('cost');
    Shiny.setInputValue('delete_budget_info', {
      path: filePath,
      scenario: scen,
      cost: cost
    }, {priority: 'event'});
  });
")),
    # titlePanel("Sélections des hypothèses budgétaires"),
    layout_columns(
      col_widths = c(2, 2, -6),
      # Instructions pop up
      actionButton(ns("show_instructions"), "📘 Instructions détaillées", class = "btn-info"),

      # Baseline assumptions pop up
      actionButton(ns("show_assumptions"), "📘 Budget assumptions", class = "btn-info")
    ),
    layout_columns(
      col_widths = c(7, 5), # Left column wider than right

      tagList( # use tagList to keep multiple siblings without wrapping them in a card
        card(
          card_header("Matrice de sélection"),
          p("Sélectionnez les données de plan et de coût à combiner..."),
          card_body(
            useShinyjs(),
            div(id = ns("matrix_container"), uiOutput(ns("matrix_ui"))),
            actionButton(ns("add_row"), "Ajouter une ligne", class = "btn-secondary", width = "100%"),
            actionButton(ns("remove_row"), "Supprimer la dernière ligne", class = "btn-secondary", width = "100%")
          )
        ),
        card(
          card_header("Génération de budget"),
          card_body(
            p("Cliquez sur le bouton ci-dessous pour générer des budgets..."),
            fluidRow(
              column(
                8,
                actionButton(
                  ns("generate_budgets"), "Générer des budgets",
                  class = "btn-success",
                  icon = icon("calculator"),
                  width = "100%",
                  `disabled` = if (lite_mode) NA else NULL
                ),
                if (lite_mode) {
                  tags$div(
                    style = "color: #cc0000; font-size: 0.9em; margin-top: 5px;",
                    "⚠️ Cette fonctionnalité est désactivée dans la version de démonstration."
                  )
                }
              ),
              column(
                4,
                div(
                  id = ns("loading_container"),
                  style = "display: none;",
                  span(
                    span(class = "fa fa-spinner fa-spin fa-2x"),
                    span(style = "margin-left: 10px;", "Génération des budgets, veuillez patienter...")
                  )
                )
              )
            )
          )
        )
      ),

      # --- RIGHT COLUMN ---
      card(
        card_header("Historique de la génération du budget"),
        card_body(
          DTOutput(ns("budget_history_table"))
        )
      )
    )
  )
}
