tab1aUI <- function(id) {
  ns <- NS(id)
  fluidPage(

    # Add the useShinyjs() function
    useShinyjs(),
    tags$head(
      # Keep your scroll-to-top function
      tags$script("
    function resetScroll() {
      window.scrollTo(0, 0);
    }
  "),

      # Add this script to fix scroll freezing after modals
      tags$script(HTML("
    $(document).on('hidden.bs.modal', function () {
      $('body').removeClass('modal-open');
      $('body').css('overflow', 'auto');
    });
  "))
    ),

    titlePanel("Data Download and Upload"),
    page_sidebar(
        sidebar = sidebar(
        width = "400px",
        card(
          "Instructions",
          p("This tool helps you manage both Scenario and Cost templates:"),
          tags$ul(
            tags$li("Scenario templates: Multiple sheets (by year) for intervention plans"),
            tags$li("Cost templates: Single sheet for unit cost information")
          ),
          p("To get started with either template:"),
          tags$ul(
            tags$li("Download an empty template to create a new file, or"),
            tags$li("Download an existing file from the tables below")
          )
        ),

        actionButton(ns("show_instructions"), "📘 Detailed Instructions", class = "btn-info"),

        # Scenario Template Section
        card(
          "Scenario Template",
          selectInput(ns("year_filter"), "Select years of interest:",
                     choices = DEFAULT_YEARS,
                     selected = 2025:2027,
                     multiple = TRUE),
          downloadButton(ns("download_scenario_template"), "Download Empty Scenario Template"),
          uiOutput(ns("scenario_download_ui")),
          hr(),
          fileInput(ns("scenario_file"), "Upload Scenario File",
                   accept = c(".xlsx", ".xls")),
          textInput(ns("scenario_name"), "Scenario Name",
                   placeholder = "Give this scenario a name"),
          textAreaInput(ns("scenario_description"), "Description",
                       placeholder = "Add a description (optional)"),
          actionButton(ns("submit_scenario"), "Submit Scenario", class = "btn-primary")
        ),

        # Cost Template Section
        card(
          "Cost Template",
          downloadButton(ns("download_cost_template"), "Download Empty Cost Template"),
          uiOutput(ns("cost_download_ui")),
          hr(),
          fileInput(ns("cost_file"), "Upload Cost File",
                   accept = c(".xlsx", ".xls")),
          textInput(ns("cost_name"), "Cost Sheet Name",
                   placeholder = "Give this cost sheet a name"),
          textAreaInput(ns("cost_description"), "Description",
                       placeholder = "Add a description (optional)"),
          actionButton(ns("submit_cost"), "Submit Cost Sheet", class = "btn-primary")
        )
      ),

      # Main panel content - stacked layout
      card(
        card_header("Previous Scenario Uploads"),
        DTOutput(ns("scenario_uploads_table"))

      ),
      card(
        card_header("Previous Cost Uploads"),
        DTOutput(ns("cost_uploads_table"))
      )
    ),

    # Add JavaScript for delete confirmation
    tags$script(HTML("
    function deleteScenario(id) {
      if (confirm('Are you sure you want to permanently delete this scenario?')) {
        Shiny.setInputValue('delete_scenario', id);
      }
    }
  "))
  )
}
