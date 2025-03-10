
generate_plots <- function(plan, year, currency) {
  # Example: Filter your data based on the inputs (here we use dummy data)
  # In your real application, replace the dummy data with your actual data filtering and plotting code.

  library(ggplot2)

  mix_dat <-
    static_mix_maps |>
    filter(plan_shortname == "Plan A",
           year == 2025)

  # mixmap 1
  plot1 <-
    create_static_map(
      lga_outline = lga_outline,
      state_outline = state_outline,
      filtered_data = mix_dat,
      plan_select = "Plan A",
      spatial_scale = "National",
      state_select = NULL,
      lga_select = NULL,
      year_value = 2025
  )

  # total cost chart
  plot2 <-
    generate_final_cost_plot(
      currency_select = currency,
      year_select = year,
      spatial_scale = "National",
      baseline_plan = "Plan A",
      comp_plans = NULL
    )


  # Return a named list of plots
  list(mix_map = plot1, item_costs = plot2)
}
