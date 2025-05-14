tab5UI <- function(id) {
  ns <- NS(id)
  fluidPage(
    useShinyjs(),
    titlePanel("Report and Data Downloads"),
    ("This tab allows users to download a report regarding the budgets generated and methods used to generate them, along with individual figure elements that can be seen in the tool and the raw budget data generated in excel format."),
    br(),
    ("Data values are examples generated using evolving methodology and meant for demonstration and not decision making."),
    br(),

    # Universal Settings Card
      card(
        card_header("Universal Settings"),
        card_body(
          selectInput(
            ns("plan_select"),
            "Select Plan",
            choices = c("", plan_labels),
            selected = "",
            multiple = TRUE
          ),
          selectInput(
            ns("year_select"),
            "Select Year",
            choices = c("",years_to_select, "All Years"),
            selected = ""
          ),
          selectInput(
            ns("currency_select"),
            "Select Currency",
            choices = c("", "USD", "NGN"),
            selected = ""
          ),
          br(),
          markdown(
            "The plan(s), year(s), and currency selected here will be used
            across all of the report, figure, and data download options.
            New settings can be selected, and each item will be produced
            individually on the click of each download button.(%).<br><br> All
            elements of the download are generated at the National level"
          )
        )
      )
    ,
    br(),

    # Download Options: Three cards in a row
    layout_column_wrap(
      width = 1/3,

      # Report Generation Card
      card(
        card_header("Report Generation"),
        card_body(
          textInput(ns("report_title"), "Report Title", value = ""),
          textInput(ns("authors_list"), "Authors List (separate by commas)", value = ""),
          br(),
          # Give the button an ID for enabling/disabling
          downloadButton(ns("download_report"), "Download Report")
        )
      ),

      # Figures Download Card
      card(
        card_header("Figures Download"),
        card_body(
          p("Download all figures associated with the budget here."),
          # Give the button an ID for enabling/disabling
          downloadButton(ns("download_figures"), "Download Figures")
        )
      ),

      # Data Download Card
      card(
        card_header("Data Download"),
        card_body(
          p("Download the raw budget data in Excel format."),
          # Give the button an ID for enabling/disabling
          downloadButton(ns("download_data"), "Download Data")
        )
      )
    )
  )
}
    #   # Card 2: Intervention Cost Breakdowns with 3 tabs
    #   navset_card_tab(
    #     full_screen = TRUE,
    #     title = "Figure",
    #     nav_panel(
    #       "Plot 1",
    #       card_title("Cost Breakdown by Category per Budget Item"),
    #       card_body(
    #         class = "p-0",
    #         plotlyOutput(session$ns("stacked_barchart"))
    #       )
    #     ),
    #     nav_panel(
    #       "Plot 2",
    #       card_title("Top 15 Specific Cost Components"),
    #       card_body(
    #         class = "p-0",
    #         plotlyOutput(session$ns("lolipop_chart"))
    #       )
    #     ),
    #     nav_panel(
    #       "Plot 3",
    #       card_title(" ")
    #
    #     ),
    #     nav_panel(
    #       shiny::icon("circle-info"),
    #       markdown("Intervention specific total costs are shown spilt into data on the
    #                cost category within each intervention (procurement, implementation
    #                and support services)(%).<br><br>The top 15 cost elements are then
    #                displayed which highlight which specific line item of
    #                the budget has the largest contribution to the overal
    #                budget estimate(%).<br><br> When State or LGA level
    #                information is displayed the Support Services costs are
    #                not generated at this level only costs related to malaria
    #                interventions are shown.")
    #     )
    #   ),
    #
    #   # Card 3: Spatial Cost Summaries with conditional tabs
    #   navset_card_tab(
    #     full_screen = TRUE,
    #     title = "Spatial Cost Summaries",
    #     nav_panel(
    #       "Plot 1",
    #       card_title("State Level Total Costs"),
    #       full_screen = TRUE,
    #       card_body(
    #         class = "p-0",
    #         leafletOutput(session$ns("state_total_map"))
    #       )
    #
    #     ),
    #     nav_panel(
    #       "Plot 2",
    #       card_title("State Level Cost per Person"),
    #       full_screen = TRUE,
    #       card_body(
    #         class = "p-0",
    #         leafletOutput(session$ns("state_pp_map"))
    #       )
    #     ),
    #     nav_panel(
    #       "Plot 3",
    #       card_title("LGA Level Total Costs"),
    #       full_screen = TRUE,
    #       card_body(
    #         class = "p-0",
    #         leafletOutput(session$ns("lga_total_map"))
    #       )
    #
    #     ),
    #     nav_panel(
    #       "Plot 4",
    #       card_title("LGA Level Cost per Person"),
    #       full_screen = TRUE,
    #       card_body(
    #         class = "p-0",
    #         leafletOutput(session$ns("lga_pp_map"))
    #       )
    #
    #     ),
    #     nav_panel(
    #       shiny::icon("circle-info"),
    #       markdown("State and LGA data on the total budget or total budget
    #                per person associated with
    #                the selected plan are displayed. Support Services costs are
    #                not generated at the State and LGA level and data related only costs
    #                associated with malaria interventions are shown.")
    #     )
    #   )
    # )

  #   # Universal Inputs: Global settings card (long skinny)
  #   card(
  #     card_header("Universal Settings"),
  #     card_body(
  #       selectInput(
  #         ns("plan_select"),
  #         "Select Plan",
  #         choices = c(plans_to_select),
  #         selected = ""
  #       ),
  #       selectInput(
  #         ns("year_select"),
  #         "Select Year",
  #         choices = c(years_to_select),
  #         selected = ""
  #       ),
  #       selectInput(
  #         ns("currency_select"),
  #         "Select Currency",
  #         choices = c("","USD","NGN"),
  #         selected = ""
  #       )
  #     )
  #   ),
  #   br(),
  #
  #   # Row containing the three download cards
  #   fluidRow(
  #     # Report Generation Card
  #     column(
  #       width = 4,
  #       card(
  #         card_header("Report Generation"),
  #         card_body(
  #           textInput(ns("report_title"), "Report Title", value = ""),
  #           textInput(ns("authors_list"), "Authors List (separate by commas)", value = ""),
  #           br(),
  #           downloadButton(ns("download_report"), "Download Report")
  #         )
  #       )
  #     ),
  #
  #     # Figures Download Card
  #     column(
  #       width = 4,
  #       card(
  #         card_header("Figures Download"),
  #         card_body(
  #           p("Download all figures here."),
  #           actionButton(ns("download_figures"), "Download Figures")
  #         )
  #       )
  #     ),
  #
  #     # Data Download Card
  #     column(
  #       width = 4,
  #       card(
  #         card_header("Data Download"),
  #         card_body(
  #           p("Download your data here."),
  #           actionButton(ns("download_data"), "Download Data")
  #         )
  #       )
  #     )
  #   )
  # )
# }


