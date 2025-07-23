#-Packages----------------------------------------------------------------------
library(shiny)
library(bslib)
library(shinyjs)


#-Source UI and server functions for each tab-----------------------------------
source("global/source-ui-server-code.R")
source("global/global.R")
source("global/helpers.R")
source("global/figure-download.R")

# Disable scientific notation globally
options(scipen = 999)

# Warning message for demo tool
head_bold <- "IMPORTANT : Ceci est une version de démonstration de l’outil."
main_text <- "Les valeurs et les résultats présentés ici sont donnés à titre indicatif uniquement et visent à présenter les fonctionnalités de l'outil. Ils ne doivent pas servir à la prise de décision ni à l'extrapolation. De plus, les données présentées ici ne sont représentatives d'aucun scénario ni coût réel. Notre outil est en cours de développement; la version présentée ici est donc destinée à illustrer les fonctionnalités que nous développons. De nombreuses fonctionnalités sont encore en développement et nous avons hâte de les partager prochainement. N'hésitez pas à nous contacter à hthompson@path.org pour toute suggestion ou commentaire; nous serions ravis de recueillir les avis de notre communauté!"

#-Define UI---------------------------------------------------------------------
ui <- page_navbar(

  # Bold header of cards
  tags$style(HTML("
      .card-header {
      font-weight: bold;
    }
  ")),
  title = "Application budgétaire paludisme",
  theme = bs_theme(
    version = 5,
    bootswatch = "united",
    # bg = "rgb(100, 16, 59)", # set the foreground colour
    # fg = "rgb(0, 0, 0)", # set the background colour
    primary = "#007bc2", # set the primary colour
    secondary = "#62baff", # set the secondary colour
    success = "#3c4856",
    info = "#d6f4ff",
    warning = "#FFD100",
    danger = "#EF3340",
    base_font = font_google("Open Sans")
  ),
  navbar_options = navbar_options(
    position = "static-top",
    class = "bg-primary",
    theme = "dark",
    collapsible = TRUE,
  ),
  fillable = FALSE,
  useShinyjs(),
  tags$script(HTML("
    $(document).ready(function() {
      var img = $('<img>', {
        src: 'drc-circle-flag.png',
        alt: 'Logo',
        style: 'height: 40px; margin-left: auto; margin-right: 20px;'
      });
      $('.navbar-nav').parent().append(img);
    });
  ")),
  tags$style(HTML("
    .input-group-btn .btn-default {
      background-color: #62baff !important;
      color: white !important;
      border: none;
    }

    .input-group-btn .btn-default:hover {
      background-color: #4aa0d6 !important;
    }
  ")),

  # Tabs
  nav_panel("Aperçu", tab0UI("tab0")),
  nav_panel("Méthodes", tab6UI("tab6")),
  nav_panel("Entrées utilisateur", tab1aUI("tab1a")),
  nav_panel("Scénario de vérification", tab2UI("tab2")),
  nav_panel("Générer des budgets", tab2aUI("tab2a")),
  nav_panel("Visualisation des budgets", tab3UI("tab3")),
  nav_panel("Comparaisons des budgets", tab4UI("tab4")),
  nav_panel("Génération de rapports", tab5UI("tab5")),

  # Logo in Footer
  footer = div(
    style = "text-align:center; padding:10px;",
    img(src = "PATH_Logo_Color.png", height = "50px"),
    tags$span(
      " Application développée par PATH 2025, version de démonstration de l'outil 1 ",
      style = "margin-left:10px; font-size:14px; color:#555;"
    )
  )
)


#-Define Server-----------------------------------------------------------------
server <- function(input, output, session) {
  #-Shared reactive values for uploads and refresh-----
  shared <- reactiveValues(
    refresh_trigger = 0,
    budget_results = NULL,
    budget_results_available = FALSE
  )

  # Initialize shared upload caches and reloaders
  initSharedDataManager(shared)

  # Create an observer to load data initially
  observe(
    {
      # This runs once when the app starts
      shared$reload_scenarios()
      shared$reload_costs()

      # If you have the budget reload function
      if (exists("reload_budgets", shared)) {
        shared$reload_budgets()
      }
    },
    priority = 1000
  )

  #-Dynamically render content for each tab------------
  output$page_content <- renderUI({
    switch(input$sidebar_menu,
      "tab0" = tab0UI("tab0"),
      "tab1a" = tab1aUI("tab1a"),
      "tab2" = tab2UI("tab2"),
      "tab2a" = tab2aUI("tab2a"),
      "tab3" = tab3UI("tab3"),
      "tab4" = tab4UI("tab4"),
      "tab5" = tab5UI("tab5"),
      "tab6" = tab6UI("tab6")
    )
  })

  # =Call modules for each tab------------------------
  callModule(tab0Server, id = "tab0")
  callModule(tab1aServer, id = "tab1a", template_file_path, SCENARIO_COLS, COST_COLS, TEMPLATE_ADMIN_DATA, shared = shared)
  callModule(tab2Server, id = "tab2", shared = shared)
  callModule(tab2aServer, id = "tab2a", shared = shared)
  callModule(tab3Server, id = "tab3", adm2_outline, adm1_outline, shared)
  callModule(tab4Server, id = "tab4", adm2_outline, adm1_outline, shared)
  callModule(tab5Server, id = "tab5", shared)
  callModule(tab6Server, id = "tab6")
}

#-Run the App------------------------------------------
shinyApp(ui, server)
