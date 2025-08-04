#-HELPER FUNCTIONS FOR BUDGET GEN APPLICATION-----------------------------------



#-LEAFLET INTERVENTION MIX MAP--------------------------------------------------
# Generates an interactive Leaflet map displaying the mix of malaria interventions
# at the health zone (adm2) level, with optional highlighting for selected provinces
# or health zones. The map includes:
#   - Colored polygons for each health zone based on intervention mix
#   - Province (adm1) boundary outlines
#   - Labels showing province, zone, and intervention mix
#   - A legend for the intervention mixes
create_intervention_leaflet <- function(adm2_outline, adm1_outline,
                                        country_outline, intervention_mix_maps,
                                        spatial_scale, adm1_select,
                                        adm2_select,
                                        center_lng = 23.7, center_lat = -2.8,
                                        zoom = 4.5) {
  # Highlight option if adm1 or adm2 is selected
  if (spatial_scale == "Province") {
    single_highlight <- adm1_outline |> filter(adm1 == adm1_select)
  }
  if (spatial_scale == "Zone de santé") {
    single_highlight <- adm2_outline |> filter(adm1 == adm1_select, adm2 == adm2_select)
  }

  # join intervention data to the shape file
  map_data <-
    left_join(adm2_outline, intervention_mix_maps) |>
    group_by(adm1, adm2, geometry) |>
    summarise(
      mix_long = paste(unique(intervention_nice), collapse = " + "),
      .groups = "drop"
    ) |>
    distinct(adm1, adm2, mix_long, geometry)

  # set colour palette
  color_pal <- colorFactor(
    palette = "Paired",
    domain = map_data$mix_long
  )

  # build leaflet map
  map <-
    leaflet() |>
    addTiles() |>
    # Add plan data
    addPolygons(
      data = map_data,
      fillColor = ~ color_pal(mix_long),
      color = "grey",
      weight = 1,
      fillOpacity = 0.9,
      highlightOptions = highlightOptions(
        weight = 3,
        color = "black",
        fillOpacity = 1,
        bringToFront = TRUE
      ),
      label = ~ sprintf(
        "<strong>%s</strong><br>Province: %s<br>Mix d'intervention: %s",
        adm2, adm1, mix_long
      ) |> lapply(htmltools::HTML),
      labelOptions = labelOptions(
        direction = "auto",
        textsize = "10px",
        style = list("font-weight" = "normal", "padding" = "3px 8px"),
        sticky = TRUE
      )
    ) |>
    # Add adm1 boundaries
    addPolygons(
      data = adm1_outline,
      fillColor = "transparent",
      color = "black",
      weight = 2,
      options = pathOptions(interactive = FALSE)
    ) |>
    # Add legend (pass the values argument to color_pal)
    addLegend(
      pal = color_pal,
      values = map_data$mix_long,
      title = "Mix d'intervention:",
      position = "bottomright",
      opacity = 0.7
    )

  # Highlight selected adm1 or adm2
  if (spatial_scale %in% c("Province", "Zone de santé")) {
    map <- map |>
      addPolylines(
        data = single_highlight,
        color = "red",
        weight = 3,
        opacity = 1,
        group = "highlight"
      )
  }

  map
}

#-FACETED INTERVENTION MIX MAP--------------------------------------------------
# Generates a static faceted ggplot map showing the intervention type for each
# health zone (adm2), grouped by intervention. Highlights selected provinces or
# health zones if applicable, based on the selected spatial scale.
create_static_map <- function(adm2_outline, adm1_outline, filtered_data,
                              plan_select, year_value,
                              spatial_scale,
                              adm1_select,
                              adm2_select) {
  p <- NULL # Initialize plot variable

  # At national level - no selection hightlight
  if (spatial_scale == "National") {
    static_data <- adm2_outline |>
      left_join(filtered_data)

    p <- ggplot() +
      geom_sf(data = adm2_outline, fill = "grey90", col = "grey", alpha = 0.5) +
      geom_sf(data = static_data, aes(fill = type_intervention, col = type_intervention), alpha = 0.6) +
      geom_sf(data = adm1_outline, fill = NA, linewidth = 0.7, color = "black") +
      scale_fill_brewer(palette = "Spectral", direction = -1) +
      scale_color_brewer(palette = "Spectral", direction = -1) +
      theme_void(base_size = 14) +
      facet_wrap(vars(intervention_nice)) +
      theme(legend.position = "bottom") +
      labs(
        fill = "Type d'intervention (si spécifié)",
        col = "Type d'intervention (si spécifié)",
        title = paste0("Pour: ", year_value)
      )
  }

  # at adm1 - highlight adm1 selected
  else if (spatial_scale == "Province" & !is.null(adm1_select)) {
    adm1_highlight <- adm1_outline |> dplyr::filter(adm1 == adm1_select)

    static_data <- adm2_outline |>
      left_join(filtered_data)

    p <- ggplot() +
      geom_sf(data = adm2_outline, fill = "grey90", col = "grey", alpha = 0.5) +
      geom_sf(data = static_data, aes(fill = type_intervention, col = type_intervention), alpha = 0.6) +
      geom_sf(data = adm1_outline, fill = NA, linewidth = 0.7, color = "black") +
      geom_sf(data = adm1_highlight, fill = NA, linewidth = 1, color = "red") +
      scale_fill_brewer(palette = "Spectral", direction = -1) +
      scale_color_brewer(palette = "Spectral", direction = -1) +
      theme_void(base_size = 14) +
      facet_wrap(vars(intervention_nice)) +
      theme(legend.position = "bottom") +
      labs(
        fill = "Type d'intervention (si spécifié)",
        col = "Type d'intervention (si spécifié)",
        title = paste0("Pour: ", year_value)
      )
  }

  # at adm2 - highlight adm2 selected
  else if (spatial_scale == "Zone de santé" & !is.null(adm2_select)) {
    adm2_highlight <- adm2_outline |> dplyr::filter(adm1 == adm1_select, adm2 == adm2_select)
    static_data <- adm2_outline |>
      left_join(filtered_data)
    p <- ggplot() +
      geom_sf(data = adm2_outline, fill = "grey90", col = "grey", alpha = 0.5) +
      geom_sf(data = static_data, aes(fill = type_intervention, col = type_intervention), alpha = 0.6) +
      geom_sf(data = adm1_outline, fill = NA, linewidth = 0.7, color = "black") +
      geom_sf(data = adm2_highlight, fill = NA, linewidth = 1, color = "red") +
      scale_fill_brewer(palette = "Spectral", direction = -1) +
      scale_color_brewer(palette = "Spectral", direction = -1) +
      theme_void(base_size = 14) +
      facet_wrap(vars(intervention_nice)) +
      theme(legend.position = "bottom") +
      labs(
        fill = "Type d'intervention (si spécifié)",
        col = "Type d'intervention (si spécifié)",
        title = paste0("Pour: ", year_value)
      )
  }

  return(p) # Explicitly return the ggplot object
}

#-SUMMARY FUNCTION FOR POP AND TOTAL BUDGET----------------------------------------
# Summarizes budget and population data for a selected geographic and temporal scope.
# This helper function filters the dataset by spatial scale (National, Province, or
# Zone de santé), year, and currency, and returns:
#   - Total budget across all interventions
#   - Budget per person based on the total population
#   - Population summary for the selected area and year
summarise_budget_data <- function(data, pop_data, spatial_scale, adm1_select,
                                  adm2_select, year_select, currency_select) {
  # Filter budget to selected currency
  budget_filtered <- data |> filter(currency == currency_select)

  # adm1 level data selection
  if (spatial_scale == "Province") {
    budget_filtered <- budget_filtered |> filter(adm1 == adm1_select)
    pop_data <- pop_data |> filter(adm1 == adm1_select)
  }
  # adm2 level data selection
  else if (spatial_scale == "Zone de santé") {
    budget_filtered <- budget_filtered |> filter(adm1 == adm1_select, adm2 == adm2_select)
    pop_data <- pop_data |> filter(adm1 == adm1_select, adm2 == adm2_select)
  }

  # Filter budget and population data to the selected year
  if (year_select != "Toutes les années") {
    budget_filtered <- budget_filtered |> filter(year == as.numeric(year_select))
    pop_data <- pop_data |>
      filter(year == as.numeric(year_select)) |>
      summarise(across(starts_with("pop_"), ~ sum(.x, na.rm = TRUE)))
  }

  # If "all years" is selected, compute the mean population across all years
  else {
    pop_data <- pop_data |>
      group_by(year) |>
      summarise(across(starts_with("pop_"), ~ sum(.x, na.rm = TRUE))) |>
      summarise(across(everything(), mean, na.rm = TRUE))
  }

  # extract total budget across all interventions as a single value
  total_budget <- budget_filtered |>
    summarise(total_budget = sum(cost_element, na.rm = TRUE)) |>
    pull(total_budget)

  # Compute per-person budget
  total_budget_per_person <- total_budget / pop_data$pop_total


  # Return summary metrics as a list
  return(list(
    total_budget = total_budget, # Numeric total cost of all interventions
    total_budget_per_person = total_budget_per_person, # Cost per person based on total population
    pop_summary = pop_data # Population summary (either for one year or averaged)
  ))
}

#-POPULATION SUMMARY HELPER----------------------------------------------------------------
# This function filters and summarizes population data based on spatial and year selections.
# It returns the total population, population under age 5, and pregnant women either for a
# specific year or averaged across all years.
get_population_summary <- function(target_population,
                                   spatial_scale,
                                   adm1_select,
                                   adm2_select,
                                   year_select) {
  # Start with full dataset
  pop_filtered <- target_population

  # Filter to selected spatial scale
  if (spatial_scale == "Province") {
    # Filter by province (adm1)
    pop_filtered <- pop_filtered |> filter(adm1 == adm1_select)
  } else if (spatial_scale == "Zone de santé") {
    # Filter by province (adm1) and health zone (adm2)
    pop_filtered <- pop_filtered |> filter(adm1 == adm1_select, adm2 == adm2_select)
  }

  # If "all years" selected, average population over all years
  if (year_select == "Toutes les années") {
    pop_summary <- pop_filtered |>
      group_by(annee) |>
      summarise(
        pop_total = sum(pop_total, na.rm = TRUE), # Total population by year
        pop_0_5 = sum(pop_0_5, na.rm = TRUE), # Children under 5 by year
        pop_femme_enceinte = sum(pop_femme_enceinte, na.rm = TRUE), # Pregnant women by year
        .groups = "drop"
      ) |>
      summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) # Take mean across years
  } else {
    # Filter to selected year and summarise
    pop_summary <- pop_filtered |>
      filter(annee == as.numeric(year_select)) |>
      summarise(
        pop_total = sum(pop_total, na.rm = TRUE),
        pop_0_5 = sum(pop_0_5, na.rm = TRUE),
        pop_femme_enceinte = sum(pop_femme_enceinte, na.rm = TRUE)
      )
  }

  return(pop_summary)
}

#-VALUE BOX HELPER--------------------------------------------------------------
# Generates a set of summary icon cards (ribbon-style) for a selected spatial and
# temporal scope. These cards display:
#   - Total budget in the selected currency
#   - Cost per person (based on total population)
#   - Optional comparison to a user-provided available budget
# Returns:
# - A set of bslib::card UI components arranged in a `layout_column_wrap`
create_icon_summaries <- function(spatial_scale, adm1_select, year_select,
                                  adm2_select, currency_select, available_budget,
                                  data, target_population, ns = identity) {
  # Set currency symbol for formatting
  currency_symbol <- if (currency_select == "USD") "$" else "FC "

  # GGet population summary for the selected scope
  pop_data <- get_population_summary(
    target_population,
    spatial_scale,
    adm1_select,
    adm2_select,
    year_select
  )

  # Calculate total budget from filtered budget data
  cost_data <- data |>
    dplyr::filter(currency == currency_select) |>
    dplyr::summarise(total_budget = sum(cost_element, na.rm = TRUE))

  total_budget <- cost_data$total_budget
  cost_per_person <- total_budget / pop_data$pop_total

  # Conditions for showing optional budget comparison card
  show_budget_input <- year_select == "Toutes les années"
  show_budget_comparison <- !is.null(available_budget) && !is.na(available_budget) && show_budget_input

  # Generate summary cards
  layout_column_wrap(

    # 1. Total Budget Card
    card(
      card_header(tagList(icon("dollar-sign", class = "fa-2x"), " Budget total ", currency_select)),
      card_body(
        h4(paste0(currency_symbol, formatC(total_budget, format = "f", digits = 0, big.mark = ","))),
      ),
      class = "bg-info text-dark"
    ),

    # 2. Cost Per Person Card
    card(
      card_header(tagList(icon("calculator", class = "fa-2x"), " Coût par personne ", currency_select)),
      card_body(
        h4(paste0(currency_symbol, formatC(cost_per_person, format = "f", digits = 2, big.mark = ","))),
        p(paste(
          "Basé sur",
          formatC(pop_data$pop_total, format = "f", digits = 0, big.mark = ","),
          "personnes"
        ))
      ),
      class = "bg-info text-dark"
    ),

    # 3. Optional Budget Comparison Card (if all years selected and budget provided)
    if (show_budget_comparison) {
      budget_diff <- available_budget - total_budget
      over <- budget_diff < 0
      card(
        card_header(tagList(icon("balance-scale", class = "fa-2x"), " Comparaison avec le budget")),
        card_body(
          h4(paste0(
            if (over) "Au-dessus du budget de " else "Sous le budget de ",
            currency_symbol,
            formatC(abs(budget_diff), format = "f", digits = 0, big.mark = ",")
          )),
          p(paste("Budget disponible:", formatC(available_budget, format = "f", digits = 0, big.mark = ","), currency_select))
        ),
        # Conditional coloring: red if over budget, green if under
        style = if (over) "background-color:#dc3545; color:white;" else "background-color:#4CAF50; color:white;"
      )
    }
  )
}

#-TOTAL BUDGET PROCESSING-------------------------------------------------------
# Filters and aggregates budget data based on the selected spatial level, location,
# and currency. It summarizes the total cost and target population per intervention,
# while preserving metadata needed for display and comparison.
# Returns:
# - A summarized data frame grouped by plan and intervention, with total costs and
#   number of provinces (adm1) and health zones (adm2) targeted
process_budget_data <- function(spatial_scale,
                                adm1_select,
                                adm2_select,
                                currency_select,
                                data) {
  # 1) Filter data based on spatial level and selected currency
  data <- if (spatial_scale == "National") {
    data |>
      dplyr::filter(
        currency == currency_select
      )
  } else if (spatial_scale == "Province") {
    data |>
      dplyr::filter(
        adm1 == adm1_select,
        currency == currency_select
      )
  } else if (spatial_scale == "Zone de santé") {
    data |>
      dplyr::filter(
        adm1 == adm1_select,
        adm2 == adm2_select,
        currency == currency_select
      )
  } else {
    return(NULL)
  }

  # 2) Summarize total cost and targeted population per intervention
  data <-
    data |>
    group_by(
      plan_id,
      scenario_name, scenario_description,
      cost_name, cost_description,
      assumption_type, assumptions_changes,
      intervention_nice
    ) |>
    summarise(
      adm1_targeted = n_distinct(adm1), # Number of provinces covered
      adm2_targeted = n_distinct(paste(adm1, adm2, sep = "_")), # Number of health zones covered
      target_pop = round(sum(target_pop, na.rm = TRUE), 0), # Total target population
      total_cost = sum(cost_element, na.rm = TRUE), # Total cost of intervention
      .groups = "drop"
    )

  return(data)
}

#-PROCESS ITEM BASED BUDGET DATA--------------------------------------------------
# Filters and aggregates detailed cost data by cost class and unit type for a given
# intervention and plan. Unlike `process_budget_data()`, which summarizes total cost
# per intervention, this function provides a more granular view of cost components
# (e.g., commodities, delivery, fixed costs).
# Returns:
# - A grouped and summarized data frame with cost breakdowns by cost class and unit,
#   per intervention and plan
process_item_data <- function(spatial_scale,
                              adm1_select,
                              adm2_select,
                              currency_select,
                              data) {
  # 1) Select the data based on the input selections
  data <- if (spatial_scale == "National") {
    data |>
      dplyr::filter(
        currency == currency_select
      )
  } else if (spatial_scale == "Province") {
    data |>
      dplyr::filter(
        adm1 == adm1_select,
        currency == currency_select
      )
  } else if (spatial_scale == "Zone de santé") {
    data |>
      dplyr::filter(
        adm1 == adm1_select,
        adm2 == adm2_select,
        currency == currency_select
      )
  } else {
    return(NULL)
  }

  # 2) Aggregate cost by intervention, cost class, and unit type
  data <-
    data |>
    group_by(
      plan_id,
      scenario_name, scenario_description,
      cost_name, cost_description,
      assumption_type, assumptions_changes,
      intervention_nice,
      cost_class, unit
    ) |>
    summarise(cost_element = sum(cost_element, na.rm = TRUE))

  return(data)
}


#-BUDGET TABLE WITH FORMATTING----------------------------------------------------------
# Creates a formatted interactive DataTable summarizing budget information for
# each intervention in a selected plan. Optionally compares with a baseline plan
# to show differences in total cost, with conditional styling for visual cues.
#
# Returns:
# - A `DT::datatable` object with formatted columns, conditional highlighting,
#   and French labels
create_budget_table <- function(processed_data, currency_select, baseline_data = NULL) {
  # Remove grouping and drop the plan_id column for display
  processed_data <- processed_data |>
    ungroup() |>
    select(-plan_id)

  # Set display-friendly column labels (French)
  col_names <- c(
    "Plan" = "scenario_name",
    "Description du plan" = "scenario_description",
    "Données sur les coûts" = "cost_name",
    "Description des coûts" = "cost_description",
    "Type d'hypothèse" = "assumption_type",
    "Modifications des hypothèses" = "assumptions_changes",
    "Intervention" = "intervention_nice",
    "Provinces ciblées" = "adm1_targeted",
    "Zones de santé ciblées" = "adm2_targeted",
    "Population ciblée" = "target_pop",
    "Coût total" = "total_cost"
  )

  # Set currency symbol for display
  currency_symbol <- if (currency_select == "USD") "$" else "FC "

  # Comparison logic if a baseline is provided (FOR COMP PAGE)
  if (!is.null(baseline_data)) {
    # Extract total cost per intervention from baseline
    baseline_lookup <- baseline_data |>
      ungroup() |>
      dplyr::select(intervention_nice, total_cost) |>
      dplyr::rename(baseline_total = total_cost)

    # Join with current data and compute change label
    processed_data <- processed_data |>
      dplyr::left_join(baseline_lookup, by = "intervention_nice") |>
      mutate(
        `Différence vs primaire` = case_when(
          is.na(baseline_total) ~ "Nouvelle intervention",
          total_cost > baseline_total ~ "Augmenter",
          total_cost < baseline_total ~ "Diminuer",
          TRUE ~ " - "
        )
      )
  }

  # # Ensure clean diff labels (for consistent formatting)
  if (!is.null(baseline_data)) {
    processed_data <- processed_data |>
      mutate(`Différence vs primaire` = ifelse(is.na(`Différence vs primaire`), " - ", `Différence vs primaire`))
  }

  # Identify columns to hide (e.g., intermediate values)
  hidden_cols <- which(names(processed_data) %in% c("baseline_total", "diff_flag"))
  if (length(hidden_cols) == 0) hidden_cols <- NULL

  # Build the DT::datatable
  dt <- DT::datatable(
    processed_data,
    rownames = FALSE,
    colnames = col_names,
    options = list(
      scrollX = TRUE,
      pageLength = 20,
      columnDefs = list(
        list(targets = hidden_cols - 1, visible = FALSE)
      ),
      language = list(
        url = "https://cdn.datatables.net/plug-ins/1.13.4/i18n/fr-FR.json"
      )
    )
  ) |>
    # Set default font size
    DT::formatStyle(
      columns = 1:ncol(processed_data),
      fontSize = "14px"
    ) |>
    # Format total cost with currency
    DT::formatCurrency(
      columns = "Coût total", # match label!
      currency = currency_symbol,
      interval = 3,
      mark = ",",
      digits = 0
    ) |>
    # Format population as comma-separated numbers
    DT::formatCurrency(
      columns = c("Population ciblée"),
      currency = "",
      digits = 0,
      mark = ","
    )

  # Apply conditional formatting if differences are available
  if (!is.null(baseline_data) && "Différence vs primaire" %in% names(processed_data)) {
    dt <- dt |>
      DT::formatStyle(
        "Coût total",
        backgroundColor = DT::styleEqual(
          c("Nouvelle intervention", "Augmenter", "Diminuer"),
          c("#f88a73", "#f88a73", "lightgreen")
        ),
        valueColumns = "Différence vs primaire"
      )
  }

  dt
}

#-DONUT CHART-------------------------------------------------------------------
# Generates a simple interactive donut chart using the `billboarder` package,
# showing the distribution of total intervention costs.
#
# Returns:
# - A `billboarder` donut chart object with no title and tooltip enabled.
donut_plot <- function(data) {
  billboarder() |>
    # Create donut chart from intervention cost data
    bb_donutchart(
      data = data |>
        ungroup() |>
        arrange(desc(total_cost)) |>
        select(intervention_nice, total_cost)
    ) |>
    # Remove default title
    bb_title(text = " ", position = "left") |>
    # Hide legend for cleaner visual
    bb_legend(show = FALSE) |>
    # Enable tooltips on hover
    bb_tooltip()
}

#-STACKED BAR PLOT--------------------------------------------------------------
# Generates an interactive stacked bar plot using `plotly`, displaying the breakdown
# of total costs per intervention by cost category (e.g., commodities, delivery, fixed).
# Returns:
# - A `plotly` stacked bar chart object where:
#     - X-axis: interventions
#     - Y-axis: total cost
#     - Bar segments: cost classes
#
# Notes:
# - Colors are hardcoded for cost classes.
# - Interventions are reordered by total cost (highest first).
# - Tooltips show cost class and formatted cost.
stacked_plot <- function(data, currency_select) {
  # Set currency symbol for tooltip display
  currency_symbol <- if (currency_select == "USD") "$" else "FC "

  # Prepare data for stacked bar chart
  proc_impl_split <-
    data |>
    group_by(
      scenario_name, scenario_description,
      cost_name, cost_description, intervention_nice,
      cost_class
    ) |>
    summarise(cost_element = sum(cost_element, na.rm = TRUE)) |>
    group_by(intervention_nice) |>
    mutate(total_intervention_cost = sum(cost_element)) |>
    ungroup() |>
    mutate(intervention_nice = reorder(intervention_nice, -total_intervention_cost))

  # Define base palette
  base_colors <- c("#3779E5", "#181D31", "#81CF98", "#B2BABB")

  # Generate extended palette if needed
  cost_classes <- unique(proc_impl_split$cost_class)
  palette_colors <- colorRampPalette(base_colors)(length(cost_classes))

  # Create the stacked bar chart using plotly
  plot_ly(
    data = proc_impl_split,
    colors = palette_colors
  ) |>
    add_bars(
      x = ~intervention_nice,
      y = ~cost_element,
      color = ~cost_class,
      text = ~ paste0(
        cost_class, "<br>",
        "Coût: ", scales::dollar(cost_element, prefix = currency_symbol)
      ),
      hoverinfo = "text",
      textposition = "none",
      hoverlabel = list(namelength = -1)
    ) |>
    layout(
      barmode = "stack", # Stack bars by cost class
      xaxis = list(
        title = "",
        tickfont = list(size = 12)
      ),
      yaxis = list(
        title = paste0("Coût (", currency_select, ")"),
        tickfont = list(size = 12)
      ),
      legend = list(
        x = 0.8,
        y = 0.95,
        xanchor = "right",
        yanchor = "top",
        font = list(size = 12),
        bgcolor = "rgba(255,255,255,0.5)"
      ),
      font = list(size = 14)
    )
}


#-STACKED INTERVENTION PROPORTION PLOT-------------------------------------------
# Creates a proportional stacked bar chart using `plotly` to display the relative
# contribution of each cost class to the total cost per intervention.
#
# Returns:
# - A `plotly` stacked bar chart with:
#     - X-axis: interventions
#     - Y-axis: proportional cost breakdown
#     - Tooltip showing absolute cost and percentage
#     - Responsive color scale based on cost classes
stacked_plot_prop <- function(data, currency_select) {
  # Set currency symbol for display
  currency_symbol <- if (currency_select == "USD") "$" else "FC "

  # Prepare data with proportional cost per intervention
  proc_impl_split <-
    data |>
    group_by(
      scenario_name, scenario_description,
      cost_name, cost_description, intervention_nice,
      cost_class
    ) |>
    summarise(cost_element = sum(cost_element, na.rm = TRUE)) |>
    group_by(intervention_nice) |>
    mutate(
      total_intervention_cost = sum(cost_element),
      cost_prop = cost_element / total_intervention_cost
    ) |>
    ungroup() |>
    mutate(intervention_nice = reorder(intervention_nice, -total_intervention_cost))

  # Define base palette
  base_colors <- c("#3779E5", "#181D31", "#81CF98", "#B2BABB")

  # Generate extended palette if needed
  cost_classes <- unique(proc_impl_split$cost_class)
  palette_colors <- colorRampPalette(base_colors)(length(cost_classes))

  # Build plotly stacked proportional bar chart
  plot_ly(
    data = proc_impl_split,
    colors = palette_colors
  ) |>
    add_bars(
      x = ~intervention_nice,
      y = ~cost_prop,
      color = ~cost_class,
      text = ~ paste0(
        cost_class, "<br>",
        "Proportion: ", scales::percent(cost_prop, accuracy = 0.1), "<br>",
        "Coût: ", scales::dollar(cost_element, prefix = currency_symbol)
      ),
      hoverinfo = "text",
      textposition = "none",
      hoverlabel = list(namelength = -1)
    ) |>
    layout(
      barmode = "stack",
      xaxis = list(
        title = "",
        tickfont = list(size = 12)
      ),
      yaxis = list(
        title = "Proportion du coût total de l'intervention",
        tickformat = ".0%", # 0 decimal places
        tickfont = list(size = 12)
      ),
      legend = list(
        x = 0.8,
        y = 0.95,
        xanchor = "right",
        yanchor = "top",
        font = list(size = 12),
        bgcolor = "rgba(255,255,255,0.5)"
      ),
      font = list(size = 14)
    )
}

#-LOLIPOP CHART FOR INDIVIDUAL COST ELEMENT RANKING-----------------------------
# Creates a lollipop chart using `plotly` to display the top 15 cost elements
# across interventions and cost classes. Each point represents a cost component,
# with lines extending from the y-axis for visual emphasis.
#
# Returns:
# - A `plotly` interactive lollipop plot with color-coded cost classes
#   and a responsive legend.
lolipop_plot <- function(data, currency_select) {
  # Set currency symbol
  currency_symbol <- if (currency_select == "USD") "$" else "FC "

  # Prepare top 15 cost items
  top_costs <-
    data |>
    arrange(desc(cost_element)) |>
    head(15) |>
    mutate(
      label = paste0(intervention_nice, " ", cost_class, " coût ", ifelse("unit" %in% names(data), unit, ""))
    )

  # Define flexible color palette for cost classes
  base_colors <- c("#3779E5", "#181D31", "#81CF98", "#B2BABB")
  cost_classes <- unique(top_costs$cost_class)
  palette_colors <- colorRampPalette(base_colors)(length(cost_classes))

  plot_ly(colors = palette_colors) |>
    # Line segments (lollipop stems)
    add_segments(
      data = top_costs,
      x = 0,
      xend = ~cost_element,
      y = ~label,
      yend = ~label,
      line = list(color = "gray"),
      showlegend = FALSE
    ) |>
    # Marker circles (lollipop heads)
    add_markers(
      data = top_costs,
      x = ~cost_element,
      y = ~label,
      color = ~cost_class,
      colors = "Set1",
      marker = list(size = 12),
      text = ~ paste0(
        intervention_nice, "<br>",
        "Composante: ", cost_class, "<br>",
        "Coût: ", scales::dollar(cost_element, prefix = currency_symbol)
      ),
      hoverinfo = "text"
    ) |>
    # Layout customization
    layout(
      xaxis = list(
        title = paste0("Coût (", currency_select, ")"),
        tickfont = list(size = 12)
      ),
      yaxis = list(
        title = "",
        tickmode = "array",
        tickvals = top_costs$label,
        ticktext = top_costs$label,
        tickfont = list(size = 12),
        automargin = TRUE
      ),
      legend = list(
        title = list(text = "Catégorie de composant"),
        font = list(size = 12),
        orientation = "h",
        x = 0.5,
        xanchor = "center",
        y = -0.2
      )
    )
}

#-COST MAP FUNCTION-------------------------------------------------------------
# Creates a choropleth map using Leaflet showing either total cost or cost per person
# by province (adm1) or health zone (adm2). Areas are shaded based on cost values,
# and tooltips display contextual information.
#
# Returns:
# - A Leaflet map object with shaded polygons and tooltip values
cost_dist_map <- function(map_level,
                          map_type,
                          adm1_select = NULL,
                          adm2_select = NULL,
                          currency_select,
                          data) {
  # 1. Format population and budget data by spatial level
  if (map_level == "Province") {
    # Aggregate population by adm1
    pop_data <-
      target_population |>
      group_by(adm1) |>
      summarise(
        pop_total = sum(pop_total, na.rm = TRUE)
      )

    # Filter and summarize budget data
    data <-
      data |>
      ungroup() |>
      filter(currency == currency_select) |>
      dplyr::select(adm1, cost_element) |>
      group_by(adm1) |>
      dplyr::summarise(
        total_budget = sum(cost_element, na.rm = TRUE)
      ) |>
      left_join(pop_data) |>
      mutate(total_budget_per_person = total_budget / pop_total)
  } else if (map_level == "Zone de santé") {
    # Aggregate population by adm1 + adm2
    pop_data <-
      target_population |>
      group_by(adm1, adm2) |>
      summarise(
        pop_total = sum(pop_total, na.rm = TRUE)
      )

    # Filter and summarize budget data
    data <-
      data |>
      ungroup() |>
      filter(currency == currency_select) |>
      dplyr::select(adm1, adm2, cost_element) |>
      group_by(adm1, adm2) |>
      dplyr::summarise(
        total_budget = sum(cost_element, na.rm = TRUE)
      ) |>
      left_join(pop_data) |>
      mutate(total_budget_per_person = total_budget / pop_total)
  }

  # 2. Set legend title
  values <- if (map_type == "total") data$total_budget else data$total_budget_per_person
  title <- if (map_type == "total") "Coût total" else "Coût par personne"

  #  3. Join to the corresponding shapefile
  if (map_level == "Province") {
    data <-
      adm1_outline |>
      dplyr::left_join(data)
  } else if (map_level == "Zone de santé") {
    data <-
      adm2_outline |>
      dplyr::left_join(data)
  }

  # 4. Determine which values to map
  values <- if (map_type == "total") data$total_budget else data$total_budget_per_person

  # 5. Prepare labels for tooltip
  label_title <-
    if (map_level == "Province") {
      paste0(data$adm1, ":")
    } else {
      paste0(data$adm1, ", ", data$adm2, ":")
    }

  # 6. Create color palette for mapping
  pal <- colorNumeric(
    palette = "RdBu",
    domain = values,
    reverse = TRUE
  )

  # 7. Build leaflet map with polygons, labels, and legend
  map <- leaflet(data) |>
    addProviderTiles(providers$CartoDB.Positron) |>
    addPolygons(
      fillColor = ~ pal(values),
      weight = 2,
      opacity = 1,
      color = "grey",
      dashArray = "3",
      fillOpacity = 1,
      label = ~ paste0(
        label_title,
        format_cost_label(
          if (map_type == "total") total_budget else total_budget_per_person,
          currency_select,
          is_per_person = map_type == "per person"
        )
      )
    ) |>
    addLegend(
      position = "bottomright",
      pal = pal,
      values = values,
      title = paste(title, "(", currency_select, ")"),
      labFormat = labelFormat(
        prefix = if (currency_select == "USD") "$" else "FC "
      )
    )

  # 8. Optional highlight for selected adm1 or adm2
  if (map_level == "Province" && !is.null(adm1_select)) {
    adm1_hl <- adm1_outline |> dplyr::filter(adm1 == adm1_select)
    map <- map |>
      addPolylines(
        data = adm1_hl,
        color = "red",
        weight = 3,
        opacity = 1,
        group = "highlight"
      )
  }

  if (map_level == "Zone de santé" && !is.null(adm2_select)) {
    adm2_hl <- adm2_outline |> dplyr::filter(adm1 == adm1_select, adm2 == adm2_select)
    map <- map |>
      addPolylines(
        data = adm2_hl,
        color = "red",
        weight = 3,
        opacity = 1,
        group = "highlight"
      )
  }

  map
}

#-HELPER FOR COST SPATIAL MAPS FORMATTING----------------------------------------------
# Formats numeric cost values for display on maps or tooltips, with currency symbols
# and proper rounding. If the value is per person, it is rounded to 2 decimal places;
# otherwise, it is rounded to whole numbers with comma separators.
#
# Returns:
# - A formatted character string with appropriate currency symbol and formatting
format_cost_label <- function(value, currency_select, is_per_person = FALSE) {
  # Set currency symbol based on selection
  currency_symbol <- if (currency_select == "USD") "$" else "FC "

  # Format value based on whether it’s per-person or total
  if (is_per_person) {
    paste0(currency_symbol, round(value, 2))
  } else {
    paste0(currency_symbol, format(round(value), big.mark = ","))
  }
}

#-BUDGET COMPARISON TOTALS BARCHART---------------------------------------------
# Creates an interactive bar chart (via ggplotly) comparing total budgets across
# different plans. Budgets are displayed in millions, with color-coded bars and
# formatted tooltips showing plan name and cost.
# Returns:
# - A `plotly` interactive bar chart showing total budget per plan
budget_barchart <- function(data, currency_select) {
  # Set currency symbol
  currency_symbol <- if (currency_select == "USD") "$" else "FC "

  # Format data for plotting
  data <- data |>
    mutate(
      budget_millions = round(total_budget / 1e6),
      plan_label_wrapped = stringr::str_wrap(plan_label, width = 30),
      hover_text = paste0(
        "Plan: ", plan_label, "<br>",
        "Coût total: ", currency_symbol, format(budget_millions, big.mark = ","), "M"
      )
    )

  # Generate color palette based on number of unique plans
  unique_plans <- unique(data$plan_label_wrapped)
  palette_fun <- ggthemes::canva_pal("Fun and tropical")
  colors <- palette_fun(length(unique_plans))

  # Create ggplot object
  p <- ggplot(data, aes(x = plan_label_wrapped, y = budget_millions, fill = plan_label_wrapped, text = hover_text)) +
    geom_bar(stat = "identity", width = 0.6) +
    geom_text(aes(label = paste0(currency_symbol, format(budget_millions, big.mark = ","), "M")),
      vjust = +3, size = 4
    ) +
    theme_minimal() +
    labs(y = paste0("Coût total (", currency_select, " en millions)"), x = "") +
    theme(text = element_text(size = 12)) +
    scale_y_continuous(labels = scales::comma) +
    guides(fill = "none") +
    scale_fill_manual(values = colors)

  # Convert to interactive plotly chart
  ggplotly(p, tooltip = "text") |>
    layout(
      hoverlabel = list(bgcolor = "white"),
      xaxis = list(tickangle = -45)
    )
}

#-TOTAL BUDGET DIFFERENCE PLOT---------------------------------------------------
# Creates an interactive diverging bar (lollipop-style) chart to visualize the
# difference in total budget (in millions) between each comparison plan and a baseline.
# Positive changes are shown in red (above budget), and negative in green (savings).
#
# Returns:
# - A `plotly` interactive chart showing cost increases/decreases relative to a baseline
budget_diff_chart <- function(data, currency_select) {
  # Set appropriate currency symbol
  currency_symbol <- if (currency_select == "USD") "$" else "FC "

  # Wrap labels for better vertical display
  data <- data |> mutate(label_wrapped = stringr::str_wrap(label, width = 40))

  # Build static ggplot object
  p <-
    ggplot(data, aes(x = difference_millions, y = label_wrapped, text = hover_text)) +
    # Reference line at zero
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    # Diverging segments with conditional color based on budget difference
    geom_segment(aes(x = 0, xend = difference_millions, yend = label_wrapped),
      color = ifelse(data$difference_millions >= 0, "#f88a73", "lightgreen")
    ) +
    geom_point(size = 4) +
    theme_minimal() +
    labs(y = "", x = paste0("Changement de coût (", currency_select, " en millions)")) +
    theme(
      text = element_text(size = 12),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank()
    ) +
    scale_x_continuous(labels = scales::comma)

  # Convert to interactive plotly chart
  ggplotly(p, tooltip = "text") |>
    layout(hoverlabel = list(bgcolor = "white"))
}

#-NUMERICAL HELPER FUNCTION FOR BUDTGET SUMMARY---------------------------------------------
# Formats numeric cost values (assumed in millions) into a human-readable currency
# label for display in tooltips, text annotations, or summary outputs. Automatically
# switches to thousands (e.g., "k") if the value is less than 1 million to avoid
# displaying "0m" for small values.
# Returns:
# - A character string with:
#     - Values ≥ 1 million shown in millions with appropriate decimal precision and "m" suffix
#     - Values < 1 million shown in thousands with "k" suffix
#     - NA or 0 values shown as "0" prefixed by the currency symbol
#
# Examples:
# - format_cost_label_auto(154.2, "$") → "$154m"
# - format_cost_label_auto(0.743, "$") → "$743k"
# - format_cost_label_auto(0, "$") → "$0"
# - format_cost_label_auto(NA, "FC ") → "FC 0"
format_cost_label_auto <- function(value_millions, currency_symbol) {
  case_when(
    is.na(value_millions) ~ paste0(currency_symbol, "0"),
    value_millions >= 100 ~ paste0(currency_symbol, format(round(value_millions, 0), big.mark = ","), "m"),
    value_millions >= 10 ~ paste0(currency_symbol, format(round(value_millions, 1), big.mark = ","), "m"),
    value_millions >= 1 ~ paste0(currency_symbol, format(round(value_millions, 2), big.mark = ","), "m"),

    # If < 1 million, display in thousands
    value_millions > 0 ~ {
      value_thousands <- value_millions * 1000
      paste0(currency_symbol, format(round(value_thousands, 0), big.mark = ","), "k")
    },
    TRUE ~ paste0(currency_symbol, "0")
  )
}


#-HELPER FOR BUDGET COMP INTERVENTION PLOT--------------------------------------
# Processes and formats budget data for selected plans for final cost comparison plotting.
# Converts total costs to millions, formats cost labels for display, and computes
# differences from a baseline plan.
#
# Returns:
# - A list with:
#     - `data`: a data frame of processed and labeled plan cost data
#     - `currency_symbol`: symbol based on the selected currency
prepare_cost_plot_data <- function(plan_select, currency_select, spatial_scale, year_select, baseline_plan) {
  # Set appropriate currency symbol
  currency_symbol <- if (currency_select == "USD") "$" else "FC "

  # Process and combine data for each selected comparison budgtet
  dat <-
    purrr::map_df(plan_select, function(plan) {
      process_budget_data(
        spatial_scale = spatial_scale,
        adm1_select = NULL,
        adm2_select = NULL,
        currency_select = currency_select
      ) |>
        mutate(plan = plan)
    }) |>
    # Format total cost and display label
    mutate(
      total_cost = total_cost / 1e6,
      total_cost = ifelse(is.na(total_cost), 0, total_cost),
      tc_print = format_cost_label_millions(total_cost, currency_symbol)
    )

  # Extract baseline totals for each intervention
  baseline_data <- dat |>
    filter(plan == baseline_plan) |>
    select(title, baseline_total = total_cost)

  # Join and flag differences
  dat <- dat |>
    left_join(baseline_data, by = "title") |>
    mutate(is_different = total_cost != baseline_total)

  return(list(data = dat, currency_symbol = currency_symbol))
}

#-INTERVENTION TOTAL COST AND DIFFERNCE BY BUDGET-------------------------------
# Creates a grouped bar plot (with text labels and tooltips) comparing total intervention
# costs across a baseline plan and one or more comparison plans.
#
# Returns:
# - An interactive `plotly` bar chart with comparisons by intervention
generate_final_cost_plot <- function(baseline_processed, comparison_processed, currency_select) {
  # Set appropriate currency symbol
  currency_symbol <- if (currency_select == "USD") "$" else "FC "

  # Add label and type to distinguish baseline vs. comparisons
  baseline <- baseline_processed |>
    mutate(plan_label = plan_id) |>
    mutate(type = "baseline")

  comparisons <- comparison_processed |>
    mutate(plan_label = plan_id) |>
    mutate(type = "comparison")

  # Combine data
  dat <- bind_rows(baseline, comparisons)

  # Get baseline total per intervention for comparison
  baseline_totals <- baseline |>
    select(intervention_nice, baseline_total = total_cost)

  # Join and format
  dat <- dat |>
    left_join(baseline_totals, by = "intervention_nice") |>
    mutate(
      is_different = total_cost != baseline_total,
      total_cost = ifelse(is.na(total_cost), 0, total_cost),
      tc_print = format_cost_label_auto(total_cost, currency_symbol)
    ) |>
    mutate(intervention_nice = factor(intervention_nice, levels = unique(intervention_nice)))

  # Dynamic color palette based on number of plans
  palette_fun <- ggthemes::canva_pal("Fun and tropical")
  plan_colors <- palette_fun(length(unique(dat$plan_label)))
  names(plan_colors) <- unique(dat$plan_label)

  # Add outline and tooltip formatting
  dat <- dat |>
    mutate(
      outline_color = ifelse(type == "comparison" & is_different, "lightgreen", NA),
      outline_width = ifelse(type == "comparison" & is_different, 1.5, 0)
    ) |>
    mutate(
      tooltip_text = paste0(
        "Budget: ", plan_label, "<br>",
        "Intervention: ", intervention_nice, "<br>",
        "Coût: ", tc_print,
        ifelse(is.na(baseline_total), "<br><i>Nouvelle intervention</i>", "")
      )
    )

  #  Plot with ggplot and convert to plotly
  p <-
    ggplot(
      dat,
      aes(
        x = total_cost,
        y = reorder(intervention_nice, total_cost),
        group = plan_label
      )
    ) +
    geom_col(aes(fill = plan_label, group = plan_label, text = tooltip_text), position = "dodge") +
    geom_text(
      aes(
        label = tc_print,
        group = plan_label
      ),
      position = position_dodge(width = 0.9),
      hjust = -0.1, # Slightly outside the bar end
      vjust = 0.5, # Vertically centered
      size = 3.5
    ) +
    scale_fill_manual(values = plan_colors) +
    labs(
      x = paste0("Coût total (", currency_select, " en millions)"),
      y = NULL,
      fill = ""
    ) +
    theme_minimal() +
    theme(
      text = element_text(size = 12),
      legend.position = "none"
    ) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.2)), labels = scales::comma)

  # Convert to interactive plot
  ggplotly(p, tooltip = "text") |>
    layout(
      hoverlabel = list(
        bgcolor = "white",
        align = "left"
      )
    )
}



#-HELPER COVERAGE COUNT FUNCTION------------------------------------------------
# Calculates coverage statistics for a specific intervention within a given plan,
# summarizing how many health zones (adm2) within each province (adm1) received or
# did not receive the intervention, either across all years or for a specific year.
# Returns:
# - A summary data frame with one row per adm1 (or per adm1-year if year_filter is specific),
#   containing:
#     - Year: selected year or "Toutes les années"
#     - Province: name of the adm1 unit
#     - Total: number of unique adm2 units in that adm1
#     - Covered: number of adm2s where `included == 1`
#     - Uncovered: number of adm2s where `included == 0` or missing
#     - Coverage %: percentage of adm2s covered (rounded to 1 decimal place)
#
# Example usage:
#   count_adm2_coverage("SMC", "Plan A", "2025", scenario_data)
#
# Notes:
# - If no adm2s are covered in a province, `Covered` will be 0 and `Coverage %` will be 0.
# - If "Toutes les années" is selected, the count is collapsed across all years.
# - Works safely with missing coverage data (using `coalesce()`).
count_adm2_coverage <- function(intervention, plan, year_filter, data) {
  # ilter the data to selected intervention and plan
  filtered_data <- data |>
    filter(
      intervention == !!intervention,
      scenario_name == !!plan
    ) |>
    ungroup()

  # Subset: adm2s that are receiving the intervention (included == 1)
  receiving <- filtered_data |>
    filter(included == 1) |>
    distinct()

  # All distinct adm2s in the plan/intervention combination
  total_adm2s <- filtered_data |>
    distinct()

  # If summarizing across all years
  if (year_filter == "Toutes les années") {
    # Count total adm2s per province (adm1) across all years
    total_summary <- total_adm2s |>
      group_by(adm1) |>
      summarise(total = n_distinct(adm2), .groups = "drop")

    # Count receiving adm2s per province
    receiving_summary <- receiving |>
      group_by(adm1) |>
      summarise(receiving = n_distinct(adm2), .groups = "drop")

    # Join summaries and compute uncovered + coverage %
    summary_table <- total_summary |>
      left_join(receiving_summary, by = "adm1") |>
      mutate(
        receiving = coalesce(receiving, 0), # Fill missing with 0
        not = total - receiving, # Calculate uncovered
        coverage_pct = round(receiving / total * 100, 1), # Calculate %
        year = "Toutes les années"
      ) |>
      select(
        Year = year,
        Province = adm1,
        Total = total,
        Covered = receiving,
        Uncovered = not,
        `Coverage %` = coverage_pct
      ) |>
      arrange(Province)
  } else {
    # If summarizing for a specific year

    # Count total adm2s per province for the selected year
    total_summary <- total_adm2s |>
      filter(year == year_filter) |>
      group_by(year, adm1) |>
      summarise(total = n_distinct(adm2), .groups = "drop")

    # Count receiving adm2s for the selected year
    receiving_summary <- receiving |>
      filter(year == year_filter) |>
      group_by(year, adm1) |>
      summarise(receiving = n_distinct(adm2), .groups = "drop")

    # Join summaries and compute uncovered + coverage %
    summary_table <- total_summary |>
      left_join(receiving_summary, by = c("year", "adm1")) |>
      mutate(
        receiving = coalesce(receiving, 0), # Fill missing with 0
        not = total - receiving, # Calculate uncovered
        coverage_pct = round(receiving / total * 100, 1) # Calculate %
      ) |>
      select(
        Year = year,
        Province = adm1,
        Total = total,
        Covered = receiving,
        Uncovered = not,
        `Coverage %` = coverage_pct
      ) |>
      arrange(Year, Province)
  }

  # Return final formatted summary table
  return(summary_table)
}


#-DATA MANAGEMENT Load all uploaded SCENARIO files-----------------------------------------------------
# Loads and combines all uploaded scenario Excel files stored in a SQLite database
# and a specified folder. Each Excel file may contain multiple sheets representing
# different years. Metadata such as scenario name and description is extracted from
# the database and added to the resulting dataset.
# Arguments:
# - db_path: character string specifying the path to the SQLite database
#            that stores metadata about uploaded scenario files.
#            Default: "scenario_uploads.db"
# - folder: character string specifying the folder where the scenario Excel files
#           are stored. Default: "uploads/scenarios"
#
# Returns:
# - A data frame combining all uploaded scenario sheets across years and scenarios.
#   The result includes the following added columns:
#     - `year`: numeric year (from sheet name)
#     - `scenario_name`: name of the scenario (from database)
#     - `scenario_description`: description of the scenario (from database)
#
# Behavior:
# - If the database contains no uploads, returns NULL.
# - If a listed file does not exist or cannot be read, it is skipped.
# - If a sheet is missing or unreadable, it is skipped.
#
# Dependencies:
# - Requires the following packages: DBI, RSQLite, readxl, purrr
load_all_uploaded_scenarios <- function(db_path = "scenario_uploads.db", folder = "uploads/scenarios") {
  # 1. Connect to the SQLite database and fetch metadata about uploaded files
  db <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  uploads <- DBI::dbGetQuery(db, "SELECT name, description, filename, years FROM uploads")
  DBI::dbDisconnect(db)

  # 2. Return NULL early if no uploads are found in the database
  if (nrow(uploads) == 0) {
    return(NULL)
  }

  # 3. Loop over each uploaded scenario file (one row per scenario)
  purrr::map_dfr(seq_len(nrow(uploads)), function(i) {
    row <- uploads[i, ]

    # Build full file path to the Excel file
    file_path <- file.path(folder, row$filename)
    # Skip if file is missing
    if (!file.exists(file_path)) {
      return(NULL)
    }

    # Split comma-separated year list into individual sheet names
    year_sheets <- unlist(strsplit(row$years, ","))

    # 4. Loop over each year/sheet in the file
    purrr::map_dfr(year_sheets, function(y) {
      #  Try to read the sheet; fail silently and skip if error
      df <- tryCatch(readxl::read_excel(file_path, sheet = y), error = function(e) NULL)

      if (is.null(df)) {
        return(NULL)
      }

      # Add metadata columns for tracking
      df$year <- as.integer(y)
      df$scenario_name <- row$name
      df$scenario_description <- row$description
      df
    })
  })
}

#-DATA MANAGEMENT Load all uploaded COST files-----------------------------------------------------
# Loads and combines all uploaded cost Excel files stored in a SQLite database
# and a specified folder. Each file contains cost data for a specific named
# cost set, and metadata (name + description) is stored in the database.
#
# Arguments:
# - db_path: character string specifying the path to the SQLite database
#            containing metadata about uploaded cost files.
#            Default: "cost_uploads.db"
# - folder: character string specifying the folder where the Excel files
#           are physically stored. Default: "uploads/costs"
#
# Returns:
# - A combined data frame of all uploaded cost files with two added columns:
#     - `cost_name`: name of the cost set (from database)
#     - `cost_description`: description of the cost set (from database)
#
# Behavior:
# - If no uploads are recorded in the database, returns NULL.
# - If a file listed in the database is missing or cannot be read, it is skipped.
load_all_uploaded_costs <- function(db_path = "cost_uploads.db", folder = "uploads/costs") {
  # 1. Connect to SQLite database and retrieve uploaded cost metadata
  db <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  uploads <- DBI::dbGetQuery(db, "SELECT name, description, filename FROM uploads")
  DBI::dbDisconnect(db)

  # 2. Exit early if no uploads are found
  if (nrow(uploads) == 0) {
    return(NULL)
  }

  # 3. Loop through each uploaded cost file and read the data
  purrr::map_dfr(seq_len(nrow(uploads)), function(i) {
    row <- uploads[i, ]

    # Construct full file path to the Excel file
    file_path <- file.path(folder, row$filename)

    # Skip this file if it does not exist
    if (!file.exists(file_path)) {
      return(NULL)
    }

    # Try to read the Excel file; skip silently if it fails
    df <- tryCatch(readxl::read_excel(file_path), error = function(e) NULL)
    if (is.null(df)) {
      return(NULL)
    }

    # Add metadata from the database as columns in the dataframe
    df$cost_name <- row$name
    df$cost_description <- row$description
    df
  })
}

#-DATA MANAGEMENT Load all GENERATED BUDGET DATA files-----------------------------------------------------
# Loads and combines all previously generated budget `.rds` files, using
# metadata stored in a history file ("budget_history.rds").
#
# Returns:
# - A combined dataframe of all successfully loaded budget files.
#   Returns NULL if the history file is missing, empty, or no valid budget
#   files can be found.
#
# Workflow:
# - Looks for a metadata file: "generated/budget_history.rds"
# - Each row in the history file should contain a `file_path` to a saved .rds file
# - Tries to load each listed file; logs warnings for any that are missing
# - Combines the loaded data frames into a single result
load_budget_data <- function() {
  # set expected location of budget history
  history_path <- "generated/budget_history.rds"

  # 1. Check if the history file exists
  if (!file.exists(history_path)) {
    message("No budget history file found")
    return(NULL)
  }

  # 2. Read the metadata from the history file
  history <- readRDS(history_path)

  # 3. Validate that metadata is not empty and contains file paths
  if (nrow(history) == 0 || !"file_path" %in% names(history)) {
    message("Budget history is empty or missing 'file_path'")
    return(NULL)
  }

  # 4. Try loading each saved budget file listed in the metadata
  all_budgets <- lapply(history$file_path, function(path) {
    if (file.exists(path)) {
      readRDS(path)
    } else {
      message("Missing budget file: ", path)
      NULL # Preserve structure if file is missing
    }
  })

  # 5. Remove any NULL results from failed loads
  all_budgets <- all_budgets[!sapply(all_budgets, is.null)]

  # 6. If none of the files could be loaded, return NULL
  if (length(all_budgets) == 0) {
    message("No valid budget files found")
    return(NULL)
  }

  # 7. Combine all loaded budget data frames into one
  combined <- dplyr::bind_rows(all_budgets)

  return(combined)
}

#-DATA MANAGEMENT SHARED DATA MANAGER ACROSS TABS-------------------------------
# Initializes and manages shared reactive caches and data stores for uploaded
# scenarios, costs, and generated budgets within a Shiny app.
#
# This function sets up:
# - Reactive caches for uploaded scenario and cost files
# - Data frame storage for loaded budget results
# - Reload helper functions for each dataset
# - A unified reload_all_uploads() function to refresh everything at once
#
# Parameters:
# - shared: A list-like object (typically `reactiveValues()` or standard list)
#   used to store shared reactive values and functions across server modules
#
# Behavior:
# - Defines reactiveVal caches for scenarios and costs
# - Initializes placeholders for budget data
# - Attaches helper functions to reload each data type from disk
# - Reload functions should be called from a reactive observer (not inside this function)
initSharedDataManager <- function(shared) {
  # Reactive caches for scenario and cost uploads
  shared$scenario_uploads_cache <- reactiveVal(NULL) # Cache for uploaded scenarios
  shared$cost_upload_cache <- reactiveVal(NULL) # Cache for uploaded costs

  # Non-reactive cache for budget results
  shared$budget_results <- NULL # Will hold the loaded budget data
  shared$budget_results_available <- FALSE # Flag for availability

  # Function: Reload scenario files from disk
  shared$reload_scenarios <- function() {
    shared$scenario_uploads_cache(load_all_uploaded_scenarios())
  }

  # Function: Reload cost files from disk
  shared$reload_costs <- function() {
    shared$cost_upload_cache(load_all_uploaded_costs())
  }

  # Function: Reload generated budget data from disk
  shared$reload_budgets <- function() {
    # Load the most recent budget
    budget_data <- load_budget_data()

    # Set the shared values if data was loaded
    if (!is.null(budget_data) && nrow(budget_data) > 0) {
      shared$budget_results <- budget_data
      shared$budget_results_available <- TRUE
      message("Loaded ", nrow(budget_data), " rows of budget data")
    } else {
      shared$budget_results <- NULL
      shared$budget_results_available <- FALSE
      message("No budget data loaded")
    }
  }

  # Function: Reload all uploads at once
  shared$reload_all_uploads <- function() {
    shared$reload_scenarios()
    shared$reload_costs()
    shared$reload_budgets()
  }

  # NOTE:
  # Do not call shared$reload_all_uploads() here directly.
  # Trigger it in a reactive context (e.g., observeEvent) to avoid blocking app startup.
}


#-BUDGET GENERATION - process_selections_for_budget(): Pass assumptions---------
# Processes a user-defined selection matrix for budget generation by filtering
# out incomplete rows and attaching the relevant scenario, cost, and assumption
# data needed for downstream computation.
#
# Returns:
# - A tibble with one row per valid user selection, containing:
#   - `scen_data`: list-column with filtered scenario data
#   - `cost_option_data`: list-column with filtered cost data
#   - `assumptions`: list-column of assumption strings
#   - `plan_name`, `cost_name`: character names of selections
#   - `assumptions_used`: string summarizing applied assumptions
#   - `ready_for_processing`: logical flag (always TRUE)
#
# This structure is designed for use in budget quantification pipelines where
# each row can be passed into a `generate_budget()`-style function.
process_selections_for_budget <- function(selections, scenario_data, cost_data) {
  selections |>
    # Filter out incomplete rows (e.g., where plan or cost not selected)
    filter(
      Plan != "Sélectionnez un plan",
      Selected_Cost != "Sélectionnez les coûts"
    ) |>
    # Process each selection row independently
    rowwise() |>
    mutate(
      # Attach matching scenario data as a list column
      scen_data = list(scenario_data |> filter(scenario_name == Plan)),
      # Attach matching cost data as a list column
      cost_option_data = list(cost_data |> filter(cost_name == Selected_Cost)),
      # Include adjustments as assumptions (list-column)
      assumptions = list(Adjustments),
      # Store raw names for traceability
      plan_name = Plan,
      cost_name = Selected_Cost,
      # Collapse assumptions into a readable string (or fallback text)
      assumptions_used = if (length(Adjustments) > 0) {
        paste(unlist(Adjustments), collapse = "; ")
      } else {
        "base de référence"
      },
      # Flag this row as ready for downstream processing
      ready_for_processing = TRUE
    ) |>
    # Ensure rowwise context is removed
    ungroup()
}

#-BUDGET GENERATION FULL LOGIC------------------------------------------------------
# Main function that quantifies intervention requirements and costs
# based on scenario data, cost assumptions, and optional user-defined adjustments.
#
#   This function generates a detailed budget dataframe for malaria intervention
#   planning by quantifying product and service needs and applying unit costs.
#   It accepts scenario data, cost data, and optional user-defined assumptions
#   to compute total quantities, unit and delivery costs, and resulting cost
#   elements across multiple interventions (e.g., ITNs, IPTp, SMC, PMC, vaccination).
#   The output includes detailed costing at the health zone (adm2) level
#
# Arguments:
#   scen_data    : A dataframe containing the intervention implementation scenario,
#                  including variables such as `adm1`, `adm2`, `year`,
#                  `scenario_name`, `scenario_description`, and intervention codes
#                  (e.g., `code_smc`, `code_iptp`, etc.).
#
#   cost_data    : A dataframe of unit cost and delivery cost information,
#                  including variables like `cout_classe`, `cout_monnaie_locale`,
#                  `cout_usd`, `code_intervention`, and `cout_annee_pour_analyse`.
#
#   assumptions  : A character vector of optional user-specified assumption changes
#                  (e.g., "CPS : population cible = Enfants de moins de 10 ans").
#                  If no assumptions are provided, default values are used.
#
# Returns:
#   A dataframe containing the detailed calculated budget, including:
#     - Spatial and temporal coverage (adm1, adm2, year)
#     - Scenario and cost metadata
#     - Quantified population and product requirements
#     - Unit costs (local and USD), cost categories, and delivery units
#     - Calculated cost elements in both currencies
#     - Intervention type and readable labels
#     - Fixed costs (e.g., trainings) included at national level
#     - Assumption summaries and a unique `plan_id` for traceability
#
# Notes:
#   - This function handles both standard and adjusted cost assumptions.
#   - All quantities and costs are calculated at the health zone (adm2) level,
#     except for fixed costs, which apply nationally.
#   - Assumptions are parsed from strings using the format `Key = Value`.
#   - Appropriate population columns are selected based on user assumptions.
#   - Final output is suitable for display, download, and comparison in a
#     Shiny dashboard or reporting tool.
generate_budget <- function(scen_data, cost_data, assumptions) {
  # Print a summary of the interventions and number of adm1s/adm2s being targeted
  # in the console
  cat("Costing scenario being generated for the following mix of interventions:")
  print(
    scen_data |>
      select(adm1, adm2, year, scenario_name, scenario_description, starts_with("code_")) |>
      pivot_longer(
        cols = starts_with("code_"),
        names_to = "intervention",
        names_prefix = "code_",
        values_to = "included"
      ) |>
      filter(included == 1) |>
      group_by(intervention, year) |>
      summarise(
        adm1_targeted = n_distinct(adm1),
        adm2_targeted = n_distinct(paste(adm1, adm2, sep = "_"))
      )
  )
  cat(scen_data$scenario_description[1])

  # Update cost data to account for used or missing cost year values
  cost_data <-
    cost_data |>
    filter(!is.na(cout_monnaie_locale)) |>
    rename(
      cost_class = cout_classe,
      unit = unite,
      local_cost = cout_monnaie_locale,
      usd_cost = cout_usd,
      cost_year = cout_annee_pour_analyse
    ) |>
    mutate(
      cost_year = as.integer(cost_year)
    )

  cost_data_expanded <- scen_data |>
    distinct(year) |>
    crossing(cost_data) |>
    mutate(
      cost_year = if_else(is.na(cost_year), year, cost_year)
    ) |>
    filter(cost_year == year)

  # extract assumption variables
  assumptions <- unlist(assumptions)

  # Target population data already loaded / passed
  target_population <- target_population

  # Convert assumptions like "SMC buffer = 1.1" to a named list
  if (!is.null(assumptions) && length(assumptions) > 0) {
    assumption_list <- purrr::map_chr(assumptions, ~.x) |>
      set_names(purrr::map_chr(strsplit(assumptions, " = "), 1)) |>
      purrr::map(~ eval(parse(text = strsplit(.x, " = ")[[1]][2])))
  } else {
    assumption_list <- list()
  }

  get_assumption <- function(varname, default) {
    if (!is.null(assumption_list[[varname]])) assumption_list[[varname]] else default
  }

  # Apply assumptions - either dedualts or pulled through the updated versions
  itn_campaign_divisor <- get_assumption("Campagne MII : personnes par moustiquaire", 1.8)
  itn_campaign_bale_size <- get_assumption("Campagne MII : moustiquaires par balle", 50)
  itn_campaign_buffer <- get_assumption("Campagne MII : marge de moustiquaires (%)", 1.1)
  itn_campaign_coverage <- get_assumption("Campagne MII : couverture de la population cible", 1)

  itn_routine_coverage <- get_assumption("Routine MII : couverture de la population cible", 0.3)
  itn_routine_buffer <- get_assumption("Routine MII : marge de moustiquaires (%)", 1.1)

  iptp_anc_coverage <- get_assumption("TPIp : fréquentation CPN", 0.8)
  iptp_doses_per_pw <- get_assumption("TPIp : points de contact", 3)
  iptp_buffer <- get_assumption("TPIp : marge pour l’approvisionnement en médicaments", 1.1)

  smc_age_string <- get_assumption("CPS : ciblage par âge", "0.18,0.77")
  smc_split <- as.numeric(strsplit(smc_age_string, ",")[[1]])
  smc_pop_prop_3_11 <- smc_split[1]
  smc_pop_prop_12_59 <- smc_split[2]
  smc_coverage <- get_assumption("CPS : couverture de la population cible", 1)
  smc_monthly_rounds <- get_assumption("CPS : cycles", 4)
  smc_buffer <- get_assumption("CPS : marge pour l’approvisionnement en médicaments", 1.1)

  pmc_coverage <- get_assumption("CPP : couverture", 0.85)
  pmc_touchpoints <- get_assumption("CPP : points de contact", 4)
  pmc_tablet_factor <- get_assumption("CPP : facteur de mise à l’échelle nutritionnelle", 0.75)
  pmc_buffer <- get_assumption("CPP : marge pour l’approvisionnement en médicaments", 1.1)

  vacc_coverage <- get_assumption("Vaccination : couverture", 0.84)
  vacc_doses_per_child <- get_assumption("Vaccination : nombre de doses", 4)
  vacc_wastage <- get_assumption("Vaccination : marge pour l’approvisionnement", 1.1)

  # Extract specific population assumptions due to named list nature
  get_pop_column <- function(varname, default_col) {
    pop_assumption <- assumption_list[[varname]]
    if (is.null(pop_assumption)) {
      return(default_col)
    }

    mapping <- list(
      "Population totale" = "pop_total",
      "Enfants de moins de 5 ans" = "pop_0_5",
      "Enfants de moins de 5 ans et femmes enceintes" = c("pop_0_5", "pop_femme_enceinte"),
      "Enfants de moins de 10 ans" = c("pop_0_5", "pop_5_10"),
      "Enfants de 3 mois à 5 ans" = "pop_0_5",
      "Enfants de 3 mois à 10 ans" = c("pop_0_5", "pop_5_10"),
      "Enfants 0-1" = "pop_0_1",
      "Enfants 1-2" = "pop_1_2",
      "Enfants 5-10" = "pop_5_10",
      "Enfants 5-36 mois" = "pop_vaccine_5_36_mois",
      "Femmes enceintes" = "pop_femme_enceinte",
      "Population urbaine" = "pop_urbain"
    )

    mapped_col <- mapping[[pop_assumption]]
    if (!is.null(mapped_col)) {
      return(mapped_col)
    } else {
      warning(paste("Hypothèse de population cible non reconnue :", pop_assumption))
      return(default_col)
    }
  }

  # Extract specific population columns
  itn_campaign_pop_col <- get_pop_column("Campagne MII : population cible", "pop_total")
  itn_routine_pop_col <- get_pop_column("Routine MII : population cible", c("pop_0_5", "pop_femme_enceinte"))
  smc_pop_col <- get_pop_column("CPS : population cible", "pop_0_5")

  # QUNATIFICATION LOGIC

  # ITN CAMPAIGNS
  itn_campaign_quantifications <-
    scen_data |>
    select(
      adm1, adm2, year, contains("mii_campagne"),
      scenario_name, scenario_description
    ) |>
    filter(
      code_mii_campagne == 1
    ) |>
    left_join(
      target_population |>
        select(
          adm1, adm2,
          year = annee, all_of(itn_campaign_pop_col)
        )
    ) |>
    rowwise() |>
    mutate(
      target_pop = sum(c_across(all_of(itn_campaign_pop_col)), na.rm = TRUE)
    ) |>
    select(-all_of(itn_campaign_pop_col)) |>
    ungroup() |>
    mutate(
      quant_nets = ((target_pop * itn_campaign_coverage) / itn_campaign_divisor) * itn_campaign_buffer,
      quant_bales = quant_nets / itn_campaign_bale_size,
      target_pop = target_pop * itn_campaign_coverage
    ) |>
    mutate(
      code_intervention = "mii_campagne"
    ) |>
    mutate(
      type_intervention = type_mii_campagne
    ) |>
    pivot_longer(
      cols = starts_with("quant"),
      names_to = "unit",
      values_to = "quantity",
      names_prefix = "quant_"
    ) |>
    select(
      adm1, adm2, year,
      scenario_name, scenario_description,
      code_intervention, type_intervention,
      target_pop, unit, quantity
    ) |>
    mutate(
      unit = case_when(
        unit == "nets" ~ "par MII",
        unit == "bales" ~ "per bale"
      )
    )

  # ITN ROUTINE
  itn_routine_quantifications <-
    scen_data |>
    select(
      adm1, adm2, year, contains("mii_routine"),
      scenario_name, scenario_description
    ) |>
    filter(
      code_mii_routine == 1
    ) |>
    left_join(
      target_population |>
        select(
          adm1, adm2,
          year = annee, all_of(itn_routine_pop_col)
        )
    ) |>
    rowwise() |>
    mutate(
      target_pop = sum(c_across(all_of(itn_routine_pop_col)), na.rm = TRUE)
    ) |>
    ungroup() |>
    mutate(
      quant_nets = (target_pop * itn_routine_coverage) * itn_routine_buffer
    ) |>
    select(-all_of(itn_routine_pop_col)) |>
    mutate(
      code_intervention = "mii_routine"
    ) |>
    mutate(
      type_intervention = type_mii_routine
    ) |>
    pivot_longer(
      cols = starts_with("quant"),
      names_to = "unit",
      values_to = "quantity",
      names_prefix = "quant_"
    ) |>
    select(
      adm1, adm2, year,
      scenario_name, scenario_description,
      code_intervention, type_intervention,
      target_pop, unit, quantity
    ) |>
    mutate(
      unit = case_when(
        unit == "nets" ~ "par MII",
        unit == "bales" ~ "per bale"
      )
    )


  # IPTp
  iptp_quantifications <-
    scen_data |>
    select(
      adm1, adm2, year, contains("Tpip"),
      scenario_name, scenario_description
    ) |>
    filter(
      code_Tpip == 1
    ) |>
    left_join(
      target_population |>
        select(
          adm1, adm2,
          year = annee, pop_femme_enceinte
        )
    ) |>
    mutate(
      quant_sp_doses = ((pop_femme_enceinte * iptp_anc_coverage) * iptp_doses_per_pw) * iptp_buffer,
    ) |>
    mutate(
      target_pop = pop_femme_enceinte
    ) |>
    mutate(
      code_intervention = "Tpip"
    ) |>
    mutate(
      type_intervention = type_Tpip
    ) |>
    pivot_longer(
      cols = starts_with("quant"),
      names_to = "unit",
      values_to = "quantity",
      names_prefix = "quant_"
    ) |>
    select(
      adm1, adm2, year,
      scenario_name, scenario_description,
      code_intervention, type_intervention,
      target_pop, unit, quantity
    ) |>
    mutate(
      unit = case_when(
        unit == "sp_doses" ~ "per SP"
      )
    )


  # SMC
  # check if expanded age range is included
  include_5_10 <- any(grepl("10", smc_pop_col))

  smc_quantification <-
    scen_data |>
    select(
      adm1, adm2, year, contains("cps"),
      scenario_name, scenario_description
    ) |>
    filter(
      code_cps == 1
    ) |>
    left_join(
      target_population |>
        select(
          adm1, adm2,
          year = annee, all_of(smc_pop_col)
        )
    ) |>
    mutate(
      quant_smc_spaq_3_11_months = ((pop_0_5 * smc_pop_prop_3_11) * smc_coverage) * smc_monthly_rounds * smc_buffer,
      quant_smc_spaq_12_59_months = ((pop_0_5 * smc_pop_prop_12_59) * smc_coverage) * smc_monthly_rounds * smc_buffer,
      quant_smc_spaq_5_10_years = if (include_5_10) {
        (pop_5_10 * smc_coverage) * smc_monthly_rounds * smc_buffer
      } else {
        0
      },
      quant_smc_child = if (include_5_10) {
        ((pop_0_5 * (smc_pop_prop_3_11 + smc_pop_prop_12_59)) + pop_5_10) * smc_coverage
      } else {
        (pop_0_5 * (smc_pop_prop_3_11 + smc_pop_prop_12_59)) * smc_coverage
      }
    ) |>
    mutate(
      target_pop = quant_smc_child
    ) |>
    mutate(
      code_intervention = "cps"
    ) |>
    mutate(
      type_intervention = type_cps
    ) |>
    pivot_longer(
      cols = starts_with("quant"),
      names_to = "unit",
      values_to = "quantity",
      names_prefix = "quant_smc_"
    ) |>
    select(
      adm1, adm2, year,
      scenario_name, scenario_description,
      code_intervention, type_intervention,
      target_pop, unit, quantity
    ) |>
    mutate(
      unit = case_when(
        unit == "spaq_3_11_months" ~ "per SPAQ pack 3-11 month olds",
        unit == "spaq_12_59_months" ~ "per SPAQ pack 12-59 month olds",
        unit == "spaq_5_10_years" ~ "per SPAQ pack 12-59 month olds",
        unit == "child" ~ "par enfant",
      )
    )

  # PMC
  pmc_quantification <-
    scen_data |>
    select(
      adm1, adm2, year, contains("cpp"),
      scenario_name, scenario_description
    ) |>
    filter(
      code_cpp == 1
    ) |>
    left_join(
      target_population |>
        select(
          adm1, adm2,
          year = annee, pop_0_1, pop_1_2
        )
    ) |>
    mutate(
      quant_pmc_sp_0_1_years = pop_0_1 * pmc_coverage * pmc_touchpoints * pmc_tablet_factor * pmc_buffer,
      quant_pmc_sp_1_2_years = pop_1_2 * pmc_coverage * pmc_touchpoints * 2 * pmc_tablet_factor * pmc_buffer,
      quant_pmc_sp_total = quant_pmc_sp_0_1_years + quant_pmc_sp_1_2_years,
      quant_pmc_child = pop_0_1 * pmc_coverage + pop_1_2 * pmc_coverage
    ) |>
    select(-quant_pmc_sp_0_1_years, -quant_pmc_sp_1_2_years) |>
    mutate(
      target_pop = quant_pmc_child
    ) |>
    mutate(
      code_intervention = "cpp"
    ) |>
    mutate(
      type_intervention = type_cpp
    ) |>
    pivot_longer(
      cols = starts_with("quant"),
      names_to = "unit",
      values_to = "quantity",
      names_prefix = "quant_pmc_"
    ) |>
    select(
      adm1, adm2, year,
      scenario_name, scenario_description,
      code_intervention, type_intervention,
      target_pop, unit, quantity
    ) |>
    mutate(
      unit = case_when(
        unit == "sp_total" ~ "per SP",
        unit == "child" ~ "par enfant",
      )
    )


  # VACCINE
  vacc_quantification <-
    scen_data |>
    select(
      adm1, adm2, year, contains("vacc"),
      scenario_name, scenario_description
    ) |>
    filter(
      code_vacc == 1
    ) |>
    left_join(
      target_population |>
        select(
          adm1, adm2,
          year = annee, pop_vaccine_5_36_mois
        )
    ) |>
    mutate(
      quant_vacc_doses = pop_vaccine_5_36_mois * vacc_coverage * vacc_wastage * vacc_doses_per_child,
      quant_vacc_child = pop_vaccine_5_36_mois * vacc_coverage,
    ) |>
    mutate(
      target_pop = quant_vacc_child
    ) |>
    mutate(
      code_intervention = "vacc"
    ) |>
    mutate(
      type_intervention = type_vacc
    ) |>
    pivot_longer(
      cols = starts_with("quant"),
      names_to = "unit",
      values_to = "quantity",
      names_prefix = "quant_vacc_"
    ) |>
    select(
      adm1, adm2, year,
      scenario_name, scenario_description,
      code_intervention, type_intervention,
      target_pop, unit, quantity
    ) |>
    mutate(
      unit = case_when(
        unit == "doses" ~ "par dose",
        unit == "child" ~ "par enfant",
      )
    )

  # To handle instances where no intervention targeted
  safe_quantification <- function(df) {
    if (nrow(df) == 0) {
      tibble(
        adm1 = character(), adm2 = character(), year = integer(),
        scenario_name = character(), scenario_description = character(),
        code_intervention = character(), type_intervention = character(),
        target_pop = numeric(), unit = character(), quantity = numeric()
      )
    } else {
      df
    }
  }


  # CASE MANAGEMENT / IRS / MDA ETC
  # NEED TO WAIT UNTIL METHODS OF QUANTIFICATION ARE SHARED

  # COMBINE QUANTIFICATION DATA INTO ONE DATA FRAME
  budget <-
    bind_rows(
      safe_quantification(itn_campaign_quantifications),
      safe_quantification(itn_routine_quantifications),
      safe_quantification(iptp_quantifications),
      safe_quantification(smc_quantification),
      safe_quantification(pmc_quantification),
      safe_quantification(vacc_quantification)
    ) |>
    # join with the cost data
    left_join(
      cost_data_expanded |> select(-year),
      by = c("code_intervention", "type_intervention", "unit", "year" = "cost_year")
    ) |>
    # pivot longer cost columns
    pivot_longer(
      cols = ends_with("_cost"),
      names_to = "currency",
      values_to = "unit_cost"
    ) |>
    # mutate
    mutate(
      cost_element = quantity * unit_cost,
      currency = case_when(
        currency == "usd_cost" ~ "USD",
        TRUE ~ "CDF"
      )
    ) |>
    mutate(
      intervention_nice = case_when(
        code_intervention == "prise_en_charge_public" ~ "Prise en charge public",
        code_intervention == "prise_en_charge_prive" ~ "Prise en charge prive",
        code_intervention == "Tpip" ~ "TPIp",
        code_intervention == "vacc" ~ "Vaccin",
        code_intervention == "mii_routine" ~ "Routine MII",
        code_intervention == "mii_campagne" ~ "Campagne MII",
        code_intervention == "cps" ~ "CPS",
        code_intervention == "cpp" ~ "CPP",
        code_intervention == "pier" ~ "Pulvérisation Intracommunautaire",
        code_intervention == "ggl" ~ "GGL",
        TRUE ~ code_intervention
      )
    ) |>
    select(
      adm1, adm2, year,
      scenario_name, scenario_description,
      cost_name, cost_description,
      code_intervention, type_intervention,
      target_pop, unit, quantity,
      cost_class, currency, unit_cost, cost_element,
      intervention_nice
    )

  # Add fixed cost values into the budget
  fixed_budget <-
    cost_data_expanded |>
    filter(type_intervention == "Cout fixe") |>
    pivot_longer(
      cols = ends_with("_cost"),
      names_to = "currency",
      values_to = "unit_cost"
    ) |>
    mutate(
      currency = if_else(currency == "usd_cost", "USD", "CDF"),
      adm1 = NA_character_,
      adm2 = NA_character_,
      scenario_name = unique(scen_data$scenario_name)[1],
      scenario_description = unique(scen_data$scenario_description)[1],
      cost_name = cost_data$cost_name[1],
      cost_description = cost_data$cost_description[1],
      target_pop = NA_real_,
      quantity = 1,
      cost_element = unit_cost * quantity
    ) |>
    mutate(
      intervention_nice = case_when(
        code_intervention == "prise_en_charge_public" ~ "Prise en charge public",
        code_intervention == "prise_en_charge_prive" ~ "Prise en charge prive",
        code_intervention == "Tpip" ~ "TPIp",
        code_intervention == "vacc" ~ "Vaccin",
        code_intervention == "mii_routine" ~ "Routine MII",
        code_intervention == "mii_campagne" ~ "Campagne MII",
        code_intervention == "cps" ~ "CPS",
        code_intervention == "cpp" ~ "CPP",
        code_intervention == "pier" ~ "Pulvérisation Intracommunautaire",
        code_intervention == "ggl" ~ "GGL",
        TRUE ~ code_intervention
      )
    ) |>
    select(
      adm1, adm2, year,
      scenario_name, scenario_description,
      cost_name, cost_description,
      code_intervention, type_intervention,
      target_pop, unit, quantity,
      cost_class, currency, unit_cost, cost_element,
      intervention_nice
    )

  # Accounting for fixed costs
  # Combine with the main budget
  budget_final <- bind_rows(budget, fixed_budget)

  # Add assumptions summary string
  assumption_summary <- if (length(assumption_list) > 0) {
    paste(names(assumption_list), unlist(assumption_list), sep = " = ", collapse = "; ")
  } else {
    "valeurs par défaut"
  }

  budget_final <- budget_final |>
    mutate(
      assumptions_changes = assumption_summary,
      assumption_type = ifelse(assumption_summary == "valeurs par défaut", "hypothèses de base", "hypothèses ajustées")
    ) |>
    mutate(
      plan_id = paste0(
        scenario_name, " avec ", cost_name, " avec ", assumption_type
      )
    ) |>
    mutate(
      plan_id = case_when(
        assumption_type == "hypothèses ajustées" ~ paste0(plan_id, " (", assumptions_changes, ")"),
        TRUE ~ plan_id
      )
    )

  return(budget_final)
}

#-BUDGET GEN HELPER - ASSUMPTIONS HANDLING--------------------------------------
#   This utility function sanitizes a character label to produce a "safe" ID
#   that can be used in HTML or Shiny input/output element names. It ensures
#   that the resulting string contains only alphanumeric characters and
#   underscores, making it compatible with Shiny's UI element IDs.
safe_id <- function(label) {
  cleaned <- gsub("[^a-zA-Z0-9_]", "_", label)
  cleaned <- gsub("_+", "_", cleaned)
  cleaned <- gsub("^_|_$", "", cleaned)
  return(cleaned)
}
