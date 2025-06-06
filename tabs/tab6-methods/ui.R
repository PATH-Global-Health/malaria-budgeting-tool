tab6UI <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Methods"),

    div(
      style = "background-color: #fff3cd; border-left: 6px solid #ffa500; padding: 15px; margin-bottom: 20px;",
      strong("IMPORTANT: This is a demonstration version of the tool. "),
      "Data uploading functionality has been suspended. The values and outputs presented here are illustrative only, intended to showcase the tool's features. They should not be used for any decision-making or extrapolation.
      In addition, the data presented here is not representative of any real-world scenarios or costs.

      Our tool is in active development and therefore the version presented here is meant to be illustrative of the types of functionality that we are building out. We still have many features in progress and can't wait to share in the near future. Please reach out to hthompson@path.org with any suggestions or feedback you may have too we'd love to gain any insights from our community!",
    ),

    hr(),

    p("Full description of underlying tool methodology being developed by PATH and partners will be available in the near future."),
  )
}
