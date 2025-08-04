tab4UI <- function(id) {
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
    card(
      #-USER INPUTS----------------
      layout_sidebar(
        sidebar = sidebar(
          width = 300,

          # instructions pop up
          actionButton(ns("show_instructions"), "📘 Instructions détaillées", class = "btn-info"),

          # primary budget selection
          uiOutput(ns("plan_bl_select_ui")),

          # comparison plan selection
          uiOutput(ns("remaining_plan_select")),

          # year selection
          uiOutput(ns("year_select_ui")),

          # currency selection
          selectInput(
            ns("currency_select"),
            "Sélectionnez la devise:",
            choices = c("", "USD", "CDF"),
            selected = ""
          ),

          # clear selections
          actionButton(
            ns("clear_inputs"),
            "Effacer les sélections",
            icon = icon("eraser"),
            class = "btn-secondary"
          )
        ),

        #-APPLICATION OUTPUTS---------
        uiOutput(ns("page_description")),
        uiOutput(ns("budget_comps")),
        uiOutput(ns("budget_item_plots")),
        uiOutput(ns("budget_tables")),
        uiOutput(ns("budget_tables_comp"))
      )
    )
  )
}
