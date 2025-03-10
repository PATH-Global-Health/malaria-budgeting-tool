
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

#-Define UI---------------------------------------------------------------------
ui <- page_fluid(
  theme = bs_theme(version = 5, bootswatch = "minty"),
  useShinyjs(),

  # Add custom CSS for fixed full-height sidebar
  tags$head(
    tags$style(HTML("
      .sidebar-container {
        height: 100vh; /* Full height */
        position: fixed; /* Fix it to the side */
        top: 0;
        left: 0;
        width: 250px; /* Sidebar width */
        overflow-y: auto; /* Scrollable if content overflows */
        background-color: #f8f9fa; /* Light background color */
        border-right: 1px solid #ddd; /* Optional border */
        padding: 15px;
      }
      .main-content {
        margin-left: 250px; /* Adjust margin to match sidebar width */
        padding: 20px;
      }
      .logo-container {
        position: absolute;
        bottom: 10px;
        width: 100%;
        text-align: center;
      }
      .logo-container img {
        max-height: 50px;
      }
          /* Fix dropdown menus getting cut off */
      .dropdown-menu {
        max-height: 400px;
        overflow-y: auto;
      }
      
      /* Ensure dropdowns aren't constrained by parent containers */
      .dropdown-menu.show {
        position: absolute !important;
        transform: translate3d(0px, 38px, 0px) !important;
        top: 0px !important;
        left: 0px !important;
        will-change: transform !important;
      }
    "))
  ),

  #-Sidebar Navigation------------------------------------
  div(
    class = "sidebar-container",
    h3("Malaria CO-OP Tool"),
    tags$a(
      href = 'https://www.path.org/who-we-are/programs/malaria/malaria-control-and-elimination-partnership-in-africa-macepa/',
      icon("house"),
      title = "Go to PATH Malaria Page"
    ),
    br(),
    actionLink("info", label = "", icon = icon("info"), title = "Info"),
    br(),
    tags$a(
      href = "mailto:hthompson@path.org",
      icon("comments"),
      title = "Message Team"
    ),
    br(), br(),
    navset_pill_list(
      id = "sidebar_menu",
      widths = c(12, 1),
      nav_panel(title = tagList(icon("house"), "Overview"), value = "tab0"),
      nav_panel(title = tagList(icon("pen"), "User Inputs"), value = "tab1a"),
      nav_panel(title = tagList(icon("check"), "Check Scenario"), value = "tab2"),
      nav_panel(title = tagList(icon("chart-bar"), "Plan Visualization"), value = "tab3"),
      nav_panel(title = tagList(icon("table-columns"), "Plan Comparisons"), value = "tab4"),
      nav_panel(title = tagList(icon("file-pdf"), "Report Generation"), value = "tab5"),
      nav_panel(title = tagList(icon("book"), "Methods"), value = "tab6")
    ),
    # Add logo at the bottom
    div(
      style = "position: absolute; bottom: 0; width: 100%; text-align: left; padding: 10px;",
      img(src = "PATH_Logo_Color.png", height = "50px", alt = "Company Logo")
    )
  ),

  #-Main Content---------------------------------
  div(
    class = "main-content",
    uiOutput("page_content")
  )
)

#-Define Server-----------------------------------------------------------------
server <- function(input, output, session) {

   #-Dynamically render content for each tab------------
  output$page_content <- renderUI({
    switch(input$sidebar_menu,
           "tab0" = tab0UI("tab0"),
           "tab1a" = tab1aUI("tab1a"),
           # "tab1b" = tab1bUI("tab1b"),
           "tab2" = tab2UI("tab2"),
           "tab3" = tab3UI("tab3"),
           "tab4" = tab4UI("tab4"),
           "tab5" = tab5UI("tab5"),
           "tab6" = tab6UI("tab6")
    )
  })

  #=Call modules for each tab------------------------
  callModule(tab0Server, id = "tab0")
  callModule(tab1aServer, id = "tab1a", template_file_path, SCENARIO_COLS, COST_COLS, TEMPLATE_ADMIN_DATA)
  # tab1aServer("tab1a")
  # callModule(tab1bServer, id = "tab1b")
  callModule(tab2Server, id = "tab2", static_mix_maps)
  callModule(tab3Server, "tab3", lga_outline, state_outline, country_outline, intervention_mix_maps, static_mix_maps)
  callModule(tab4Server, id = "tab4", lga_outline, state_outline, country_outline, intervention_mix_maps)
  callModule(tab5Server, id = "tab5")
  callModule(tab6Server, id = "tab6")
}

#-Run the App------------------------------------------
shinyApp(ui, server)


