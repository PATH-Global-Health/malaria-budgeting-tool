tab1aUI <- function(id) {
  ns <- NS(id) # Create namespace function for this module instance

  fluidPage(
    useShinyjs(), # Load shinyjs for enabling/disabling or manipulating elements with JS

    # ----------------------------------
    # Custom JavaScript in the <head>
    # for restting the scroll
    # ----------------------------------
    tags$head(
      tags$script("
        function resetScroll() {
          window.scrollTo(0, 0); // Defines a JS function to scroll to top (can be called from R)
        }
      "),
      tags$script(HTML("
        // Ensures after closing a Bootstrap modal, scrollbars work correctly
        $(document).on('hidden.bs.modal', function () {
          $('body').removeClass('modal-open');
          $('body').css('overflow', 'auto');
        });
      ")),
      tags$style(HTML("
      .card-header {
      font-weight: bold;
    }
      "))
    ),

    # -------------------------------
    # Yellow warning banner
    # -------------------------------
    div(
      style = "background-color: #fff3cd; border-left: 6px solid #ffa500; padding: 15px; margin-bottom: 20px;",
      strong(head_bold),
      main_text
    ),

    # -------------------------------
    # Scenario Template Section
    # -------------------------------
    card(
      # global card header
      card_header("Modèle de scénario"),
      # sidebar layout with sidebar and card body
      layout_sidebar(

        # add the sidebar with input elements
        sidebar = sidebar(
          width = "400px",

          # Instructions - scenarion template only
          actionButton(ns("show_instructions_sc"), "📘 Instructions détaillées", class = "btn-info"),

          # Select the years of the plan
          selectInput(ns("year_filter"), "Sélectionnez les années d'intérêt :",
            choices = DEFAULT_YEARS,
            selected = 2025:2027,
            multiple = TRUE
          ),
          # Download the scenario template
          downloadButton(ns("download_scenario_template"), "Télécharger le modèle de scénario vide", class = "btn-primary"),
          uiOutput(ns("scenario_download_ui")),
          hr(),
          fileInput(ns("scenario_file"), "Importer un fichier de scénario", accept = c(".xlsx", ".xls")) |>
            tagAppendAttributes(disabled = lite_mode),
          textInput(ns("scenario_name"), "Nom du scénario",
            placeholder = "Donnez un nom à ce scénario"
          ),
          textAreaInput(ns("scenario_description"), "Description",
            placeholder = "Ajoutez une description (facultatif)"
          ),
          actionButton(ns("submit_scenario"), "Soumettre le scénario", class = "btn-primary", disabled = if (lite_mode) NA else NULL)
        ),

        # include the card body with the plots
        card(
          card_header("Importations de scénarios précédents"),
          card_body(DTOutput(ns("scenario_uploads_table")))
        )
      )
    ),


    # -------------------------------
    # Cost Template Section
    # -------------------------------
    card(
      # global card header
      card_header("Modèle de coûts"),
      # sidebar layout with sidebar and card body
      layout_sidebar(

        # add the sidebar with input elements
        sidebar = sidebar(
          width = "400px",
          # Instructions - scenarion template only
          actionButton(ns("show_instructions_uc"), "📘 Instructions détaillées", class = "btn-info"),

          # Download template
          downloadButton(ns("download_cost_template"), "Télécharger le modèle de coûts vide", class = "btn-primary"),
          uiOutput(ns("cost_download_ui")),
          hr(),
          fileInput(ns("cost_file"), "Importer un fichier de coûts", accept = c(".xlsx", ".xls")) |>
            tagAppendAttributes(disabled = lite_mode),
          textInput(ns("cost_name"), "Nom de la feuille de coûts",
            placeholder = "Donnez un nom à cette feuille de coûts"
          ),
          textAreaInput(ns("cost_description"), "Description",
            placeholder = "Ajoutez une description (facultatif)"
          ),
          actionButton(ns("submit_cost"), "Soumettre la feuille de coûts", class = "btn-primary", disabled = if (lite_mode) NA else NULL)
        ),

        # include the card body with the plots
        card(
          card_header("Importations de coûts précédentes"),
          DTOutput(ns("cost_uploads_table"))
        )
      )
    ),

    # -------------------------------
    # JavaScript for deleting scenarios
    # -------------------------------
    tags$script(HTML("
      function deleteScenario(id) {
        if (confirm('Êtes-vous sûr de vouloir supprimer définitivement ce scénario ?')) {
          Shiny.setInputValue('delete_scenario', id);
          // Sends value back to server in input$delete_scenario
        }
      }
    "))
  )
}
