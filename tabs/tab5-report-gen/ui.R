tab5UI <- function(id) {
  ns <- NS(id)
  fluidPage(
    useShinyjs(),
    titlePanel("Report and Data Downloads"),
    ("This tab allows users to download a report regarding the budgets generated and methods used to generate them, along with individual figure elements that can be seen in the tool and the raw budget data generated in excel format."),
    br(),
    ("Data values are examples generated using evolving methodology and meant for demonstration and not decision making."),
    br(),
    div(
      style = "background-color: #fff3cd; border-left: 6px solid #ffa500; padding: 15px; margin-bottom: 20px;",
      strong("IMPORTANT: This is a demonstration version of the tool. "),
      "Data uploading functionality has been suspended. The values and outputs presented here are illustrative only, intended to showcase the tool's features. They should not be used for any decision-making or extrapolation.
      In addition, the data presented here is not representative of any real-world scenarios or costs.

      Our tool is in active development and therefore the version presented here is meant to be illustrative of the types of functionality that we are building out. We still have many features in progress and can't wait to share in the near future. Please reach out to hthompson@path.org with any suggestions or feedback you may have too we'd love to gain any insights from our community!",
    ),
    # Universal Settings Card
    card(
      card_header("Universal Settings"),
      card_body(
        uiOutput(ns("plan_ui")),
        uiOutput(ns("year_ui")),
        uiOutput(ns("currency_ui")),
        br(),
        markdown("The plan(s), year(s), and currency selected here will be used across all of the report, figure, and data download options...")
      )
    ),
    br(),

    layout_column_wrap(
      width = 1/3,
      card(
        card_header("Report Generation"),
        card_body(
          textInput(ns("report_title"), "Report Title", value = ""),
          textInput(ns("authors_list"), "Authors List (separate by commas)", value = ""),
          br(),
          downloadButton(ns("download_report"), "Download Report")
        )
      ),
      card(
        card_header("Figures Download"),
        card_body(
          p("Download all figures associated with the budget here."),
          downloadButton(ns("download_figures"), "Download Figures")
        )
      ),
      card(
        card_header("Data Download"),
        card_body(
          p("Download the raw budget data in Excel format."),
          downloadButton(ns("download_data"), "Download Data")
        )
      )
    )
  )
}
