tab5UI <- function(id) {
  ns <- NS(id)
  fluidPage(
    div(
      style = "background-color: #fff3cd; border-left: 6px solid #ffa500; padding: 15px; margin-bottom: 20px;",
      strong(head_bold),
      main_text
    ),
    useShinyjs(),
    titlePanel("Téléchargements de rapports et de données"),
    ("Cet onglet permet aux utilisateurs de télécharger un rapport concernant les budgets générés et les méthodes utilisées pour les générer, ainsi que les éléments de chiffres individuels qui peuvent être vus dans l'outil et les données budgétaires brutes générées au format Excel."),
    br(),
    layout_column_wrap(
      width = 1 / 3,
      card(
        card_header("Génération de rapports méthodologiques"),
        card_body(
          p("Télécharger le rapport méthodologique ici."),
          downloadButton(ns("download_report"), "Télécharger le rapport")
        )
      ),
      card(
        card_header("Téléchargement des figures"),
        card_body(
          p("Téléchargez ici toutes les figures générées par l'outil."),
          downloadButton(ns("download_figures"), "Télécharger les figures")
        )
      ),
      card(
        card_header("Téléchargement de données"),
        card_body(
          p("Téléchargez les données budgétaires brutes au format Excel."),
          downloadButton(ns("download_data"), "Téléchargement de données")
        )
      )
    )
  )
}
