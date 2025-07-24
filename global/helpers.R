#-------------------------------------------------------------------------------
# Helper functions to call into server code to keep streamlined
#
#-------------------------------------------------------------------------------

#-Leaflet intervention mix map--------------------------------------------------
create_intervention_leaflet <- function(adm2_outline, adm1_outline,
                                        country_outline, intervention_mix_maps,
                                        spatial_scale, adm1_select,
                                        adm2_select,
                                        center_lng = 23.7, center_lat = -2.8,
                                        zoom = 4.5) {
  # Highlight option if adm1 or adm2 is selected
  if (spatial_scale == "Province") {
    single_highlight <- adm1_outline %>% filter(adm1 == adm1_select)
  }
  if (spatial_scale == "Zone de santé") {
    single_highlight <- adm2_outline %>% filter(adm1 == adm1_select, adm2 == adm2_select)
  }

  map_data <-
    left_join(adm2_outline, intervention_mix_maps) |>
    group_by(adm1, adm2, geometry) %>%
    summarise(
      mix_long = paste(unique(intervention_nice), collapse = " + "),
      .groups = "drop"
    ) %>%
    distinct(adm1, adm2, mix_long, geometry)

  color_pal <- colorFactor(
    palette = "Paired", # or use another palette like "Paired", "Dark2"
    domain = map_data$mix_long
  )

  map <-
    leaflet() |>
    # setView(lng = center_lng, lat = center_lat, zoom = zoom) |>
    addTiles() |>
    # Add adm2 polygons with color based on intervention_summary
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
      ) %>% lapply(htmltools::HTML),
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

#-Static Facet intervention maps------------------------------------------------
create_static_map <- function(adm2_outline, adm1_outline, filtered_data,
                              plan_select, year_value,
                              spatial_scale,
                              adm1_select,
                              adm2_select) {
  p <- NULL # Initialize plot variable

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
  } else if (spatial_scale == "Province" & !is.null(adm1_select)) {
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
  } else if (spatial_scale == "Zone de santé" & !is.null(adm2_select)) {
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

#-summary helper for pop and budget data----------------------------------------
summarise_budget_data <- function(data, pop_data, spatial_scale, adm1_select,
                                  adm2_select, year_select, currency_select) {
  # Filter budget
  budget_filtered <- data |> filter(currency == currency_select)

  if (spatial_scale == "Province") {
    budget_filtered <- budget_filtered |> filter(adm1 == adm1_select)
    pop_data <- pop_data |> filter(adm1 == adm1_select)
  } else if (spatial_scale == "Zone de santé") {
    budget_filtered <- budget_filtered |> filter(adm1 == adm1_select, adm2 == adm2_select)
    pop_data <- pop_data |> filter(adm1 == adm1_select, adm2 == adm2_select)
  }

  if (year_select != "Toutes les années") {
    budget_filtered <- budget_filtered |> filter(year == as.numeric(year_select))
    pop_data <- pop_data |>
      filter(year == as.numeric(year_select)) |>
      summarise(across(starts_with("pop_"), ~ sum(.x, na.rm = TRUE)))
  } else {
    pop_data <- pop_data |>
      group_by(year) |>
      summarise(across(starts_with("pop_"), ~ sum(.x, na.rm = TRUE))) |>
      summarise(across(everything(), mean, na.rm = TRUE))
  }

  total_budget <- budget_filtered |>
    summarise(total_budget = sum(cost_element, na.rm = TRUE)) |>
    pull(total_budget)
  total_budget_per_person <- total_budget / pop_data$pop_total

  return(list(
    total_budget = total_budget,
    total_budget_per_person = total_budget_per_person,
    pop_summary = pop_data
  ))
}

#-Population summary data---------------------------------------------------------------------------------------
get_population_summary <- function(target_population, spatial_scale, adm1_select, adm2_select, year_select) {
  pop_filtered <- target_population

  if (spatial_scale == "Province") {
    pop_filtered <- pop_filtered %>% filter(adm1 == adm1_select)
  } else if (spatial_scale == "Zone de santé") {
    pop_filtered <- pop_filtered %>% filter(adm1 == adm1_select, adm2 == adm2_select)
  }

  if (year_select == "Toutes les années") {
    pop_summary <- pop_filtered %>%
      group_by(annee) %>%
      summarise(
        pop_total = sum(pop_total, na.rm = TRUE),
        pop_0_5 = sum(pop_0_5, na.rm = TRUE),
        pop_femme_enceinte = sum(pop_femme_enceinte, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      summarise(across(everything(), ~ mean(.x, na.rm = TRUE)))
  } else {
    pop_summary <- pop_filtered %>%
      filter(annee == as.numeric(year_select)) %>%
      summarise(
        pop_total = sum(pop_total, na.rm = TRUE),
        pop_0_5 = sum(pop_0_5, na.rm = TRUE),
        pop_femme_enceinte = sum(pop_femme_enceinte, na.rm = TRUE)
      )
  }

  return(pop_summary)
}

#-Ribbon icons function---------------------------------------------------------
create_icon_summaries <- function(spatial_scale, adm1_select, year_select,
                                  adm2_select, currency_select, available_budget,
                                  data, target_population, ns = identity) {
  currency_symbol <- if (currency_select == "USD") "$" else "FC "

  # Get population data
  pop_data <- get_population_summary(target_population, spatial_scale, adm1_select, adm2_select, year_select)

  # Calculate cost summary
  cost_data <- data %>%
    dplyr::filter(currency == currency_select) %>%
    dplyr::summarise(total_budget = sum(cost_element, na.rm = TRUE))

  total_budget <- cost_data$total_budget
  cost_per_person <- total_budget / pop_data$pop_total

  # Conditions
  show_budget_input <- year_select == "Toutes les années"
  show_budget_comparison <- !is.null(available_budget) && !is.na(available_budget) && show_budget_input

  # Cards
  layout_column_wrap(

    # 4. Budget Total
    card(
      card_header(tagList(icon("dollar-sign", class = "fa-2x"), " Budget total ", currency_select)),
      card_body(
        h4(paste0(currency_symbol, formatC(total_budget, format = "f", digits = 0, big.mark = ","))),
      ),
      class = "bg-info text-dark"
    ),

    # 5. Cost Per Person
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

    # 7. Budget Comparison (only after input)
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
        style = if (over) "background-color:#dc3545; color:white;" else "background-color:#4CAF50; color:white;"
      )
    }
  )
}

#-TOTAL COST PROCESSING-------------------------------------------------------------------
process_budget_data <- function(spatial_scale,
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

  # 2) Calculate total costs per intervention
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
      adm1_targeted = n_distinct(adm1),
      adm2_targeted = n_distinct(paste(adm1, adm2, sep = "_")),
      target_pop = round(sum(target_pop, na.rm = TRUE), 0),
      total_cost = sum(cost_element, na.rm = TRUE)
    )

  return(data)
}

#-Process individual item data------------------------------------------------
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

  # 2) Calculate total costs per intervention
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
create_budget_table <- function(processed_data, currency_select, baseline_data = NULL) {
  processed_data <- processed_data |>
    ungroup() |>
    select(-plan_id)

  # Display names for columns
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

  currency_symbol <- if (currency_select == "USD") "$" else "FC "

  # Add comparison columns if baseline is provided
  if (!is.null(baseline_data)) {
    baseline_lookup <- baseline_data %>%
      ungroup() %>%
      dplyr::select(intervention_nice, total_cost) %>%
      dplyr::rename(baseline_total = total_cost)

    processed_data <- processed_data %>%
      dplyr::left_join(baseline_lookup, by = "intervention_nice") %>%
      mutate(
        `Différence vs primaire` = case_when(
          is.na(baseline_total) ~ "Nouvelle intervention",
          total_cost > baseline_total ~ "Augmenter",
          total_cost < baseline_total ~ "Diminuer",
          TRUE ~ " - "
        )
      )
  }

  # Ensure diff_type is not all NA (needed for styling)
  if (!is.null(baseline_data)) {
    processed_data <- processed_data %>%
      mutate(`Différence vs primaire` = ifelse(is.na(`Différence vs primaire`), " - ", `Différence vs primaire`))
  }

  # Hide only columns that won’t be used in styling
  hidden_cols <- which(names(processed_data) %in% c("baseline_total", "diff_flag"))
  if (length(hidden_cols) == 0) hidden_cols <- NULL

  # Build datatable
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
  ) %>%
    DT::formatStyle(
      columns = 1:ncol(processed_data),
      fontSize = "14px"
    ) %>%
    DT::formatCurrency(
      columns = "Coût total", # match label!
      currency = currency_symbol,
      interval = 3,
      mark = ",",
      digits = 0
    ) %>%
    DT::formatCurrency(
      columns = c("Population ciblée"),
      currency = "",
      digits = 0,
      mark = ","
    )

  # Only apply highlighting if diff_type exists
  if (!is.null(baseline_data) && "Différence vs primaire" %in% names(processed_data)) {
    dt <- dt %>%
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

#-DONUT CHART-------------------------------------------------------------
donut_plot <- function(data) {
  billboarder() |>
    bb_donutchart(
      data = data |>
        ungroup() |>
        arrange(desc(total_cost)) |>
        select(intervention_nice, total_cost)
    ) |>
    bb_title(text = " ", position = "left") |>
    bb_legend(show = FALSE) |>
    bb_tooltip()
}

#-TREE MAP--------------------------------------------------------------
treemap_plot <- function(data, currency_select) {
  currency_symbol <- if (currency_select == "USD") "$" else "FC "

  intervention_totals <-
    data |>
    arrange(desc(total_cost))

  plot_ly(
    data = intervention_totals,
    type = "treemap",
    labels = ~intervention_nice,
    parents = "",
    values = ~ round(total_cost, 0),
    textinfo = "label+value",
    hovertemplate = paste(
      "<b>%{label}</b><br>",
      "Coût total: ", currency_symbol, "%{value:,.0f}<br>",
      "<extra></extra>"
    )
  )
}

#-STACKED BAR PLOT----------------------------------------------------
stacked_plot <- function(data, currency_select) {
  currency_symbol <- if (currency_select == "USD") "$" else "FC "

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

  plot_ly(
    data = proc_impl_split,
    colors = c("#3779E5", "#181D31", "#81CF98", "#B2BABB")
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
      textposition = "none", # Don't show as a label on bar
      hoverlabel = list(namelength = -1) # Don't truncate labels
    ) |>
    layout(
      barmode = "stack",
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

#-stacked proportional plot-----------------------------------------
stacked_plot_prop <- function(data, currency_select) {
  currency_symbol <- if (currency_select == "USD") "$" else "FC "

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

  plot_ly(
    data = proc_impl_split,
    colors = c("#3779E5", "#181D31", "#81CF98", "#B2BABB")
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

#-lolipop plot for specific elements---------------------------------------------
lolipop_plot <- function(data, currency_select) {
  currency_symbol <- if (currency_select == "USD") "$" else "FC "

  # Get top costs and include intervention information
  top_costs <-
    data |>
    arrange(desc(cost_element)) |>
    head(15) |>
    mutate(
      label = paste0(intervention_nice, " ", cost_class, " coût ", ifelse("unit" %in% names(data), unit, ""))
    )

  plot_ly(colors = c("#3779E5", "#181D31", "#81CF98", "#B2BABB")) |>
    add_segments(
      data = top_costs,
      x = 0,
      xend = ~cost_element,
      y = ~label,
      yend = ~label,
      line = list(color = "gray"),
      showlegend = FALSE
    ) |>
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

#-COST MAP FUNCTION------------------------------------------------------------
cost_dist_map <- function(map_level,
                          map_type,
                          adm1_select = NULL,
                          adm2_select = NULL,
                          currency_select,
                          data) {
  # Format dataset based on map_level
  if (map_level == "Province") {
    pop_data <-
      target_population |>
      group_by(adm1) |>
      summarise(
        pop_total = sum(pop_total, na.rm = TRUE)
      )

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
    pop_data <-
      target_population |>
      group_by(adm1, adm2) |>
      summarise(
        pop_total = sum(pop_total, na.rm = TRUE)
      )

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

  # Determine which values to map and the legend title
  values <- if (map_type == "total") data$total_budget else data$total_budget_per_person
  title <- if (map_type == "total") "Coût total" else "Coût par personne"

  # Join with shapefiles
  if (map_level == "Province") {
    data <-
      adm1_outline |>
      dplyr::left_join(data)
  } else if (map_level == "Zone de santé") {
    data <-
      adm2_outline |>
      dplyr::left_join(data)
  }

  # Create label for each feature
  label_title <-
    if (map_level == "Province") paste0(data$adm1, ":") else paste0(data$adm1, ", ", data$adm2, ":")

  # Create color palette
  pal <- colorNumeric(
    palette = "RdBu",
    domain = values,
    reverse = TRUE
  )

  # Build base map
  map <- leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
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
    ) %>%
    addLegend(
      position = "bottomright",
      pal = pal,
      values = values,
      title = paste(title, "(", currency_select, ")"),
      labFormat = labelFormat(
        prefix = if (currency_select == "USD") "$" else "FC "
      )
    )

  # Add highlight if a adm1 is selected and the map is at adm1 level
  if (map_level == "Province" && !is.null(adm1_select)) {
    adm1_hl <- adm1_outline %>% dplyr::filter(adm1 == adm1_select)
    map <- map %>%
      addPolylines(
        data = adm1_hl,
        color = "red",
        weight = 3,
        opacity = 1,
        group = "highlight"
      )
  }

  # Add highlight if an adm2 is selected and the map is at adm2 level
  if (map_level == "Zone de santé" && !is.null(adm2_select)) {
    adm2_hl <- adm2_outline %>% dplyr::filter(adm1 == adm1_select, adm2 == adm2_select)
    map <- map %>%
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

format_cost_label <- function(value, currency_select, is_per_person = FALSE) {
  currency_symbol <- if (currency_select == "USD") "$" else "FC "

  if (is_per_person) {
    paste0(currency_symbol, round(value, 2))
  } else {
    paste0(currency_symbol, format(round(value), big.mark = ","))
  }
}

#-Budget comparison plot-------------------------------------------------------------------------
# Cost Comparison Plot
budget_barchart <- function(data, currency_select) {
  currency_symbol <- if (currency_select == "USD") "$" else "FC "

  data <- data |>
    mutate(
      budget_millions = round(total_budget / 1e6),
      plan_label_wrapped = stringr::str_wrap(plan_label, width = 30),
      hover_text = paste0(
        "Plan: ", plan_label, "<br>",
        "Coût total: ", currency_symbol, format(budget_millions, big.mark = ","), "M"
      )
    )

  unique_plans <- unique(data$plan_label_wrapped)
  palette_fun <- ggthemes::canva_pal("Fun and tropical")
  colors <- palette_fun(length(unique_plans))

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

  ggplotly(p, tooltip = "text") %>%
    layout(
      hoverlabel = list(bgcolor = "white"),
      xaxis = list(tickangle = -45)
    )
}

# cost difference plot-----------------------------------------------------------------------
budget_diff_chart <- function(data, currency_select) {
  currency_symbol <- if (currency_select == "USD") "$" else "FC "

  data <- data |> mutate(label_wrapped = stringr::str_wrap(label, width = 40))

  p <- ggplot(data, aes(x = difference_millions, y = label_wrapped, text = hover_text)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
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

  ggplotly(p, tooltip = "text") %>%
    layout(hoverlabel = list(bgcolor = "white"))
}

#-final cost plot-------------------------------------------------------------------------
#--- Helper: Process and format data for plotting ----
prepare_cost_plot_data <- function(plan_select, currency_select, spatial_scale, year_select, baseline_plan) {
  currency_symbol <- if (currency_select == "USD") "$" else "FC "

  dat <-
    purrr::map_df(plan_select, function(plan) {
      process_budget_data(
        spatial_scale = spatial_scale,
        adm1_select = NULL,
        adm2_select = NULL,
        currency_select = currency_select
      ) %>%
        mutate(plan = plan)
    }) %>%
    mutate(
      total_cost = total_cost / 1e6,
      tc_print = case_when(
        currency_select == "CDF" ~ paste0(currency_symbol, format(round(total_cost, 0), big.mark = ","), "m"),
        currency_select == "USD" & total_cost > 60 ~ paste0(currency_symbol, format(round(total_cost, 0), big.mark = ","), "m"),
        currency_select == "USD" & total_cost <= 60 & total_cost > 1 ~ paste0(currency_symbol, format(round(total_cost, 1), big.mark = ","), "m"),
        currency_select == "USD" & total_cost <= 1 ~ paste0(currency_symbol, format(round(total_cost, 2), big.mark = ","), "m"),
        is.na(total_cost) ~ paste0(currency_symbol, "0m")
      ),
      total_cost = ifelse(is.na(total_cost), 0, total_cost)
    )

  baseline_data <- dat %>%
    filter(plan == baseline_plan) %>%
    select(title, baseline_total = total_cost)

  dat <- dat %>%
    left_join(baseline_data, by = "title") %>%
    mutate(is_different = total_cost != baseline_total)

  return(list(data = dat, currency_symbol = currency_symbol))
}

#--- Main Plot Function (returns plotly object) ----
generate_final_cost_plot <- function(baseline_processed, comparison_processed, currency_select) {
  currency_symbol <- if (currency_select == "USD") "$" else "FC "

  # Add label column
  baseline <- baseline_processed %>%
    mutate(plan_label = plan_id) %>%
    mutate(type = "baseline")

  comparisons <- comparison_processed %>%
    mutate(plan_label = plan_id) %>%
    mutate(type = "comparison")

  dat <- bind_rows(baseline, comparisons)

  # Get baseline totals for each intervention
  baseline_totals <- baseline %>%
    select(intervention_nice, baseline_total = total_cost)

  dat <- dat %>%
    left_join(baseline_totals, by = "intervention_nice") %>%
    mutate(
      is_different = total_cost != baseline_total,
      tc_print = case_when(
        currency_select == "CDF" ~ paste0(currency_symbol, format(round(total_cost, 0), big.mark = ","), "m"),
        currency_select == "USD" & total_cost > 60 ~ paste0(currency_symbol, format(round(total_cost, 0), big.mark = ","), "m"),
        currency_select == "USD" & total_cost <= 60 & total_cost > 1 ~ paste0(currency_symbol, format(round(total_cost, 1), big.mark = ","), "m"),
        currency_select == "USD" & total_cost <= 1 ~ paste0(currency_symbol, format(round(total_cost, 2), big.mark = ","), "m"),
        is.na(total_cost) ~ paste0(currency_symbol, "0m")
      ),
      total_cost = ifelse(is.na(total_cost), 0, total_cost)
    )

  dat <- dat %>%
    mutate(intervention_nice = factor(intervention_nice, levels = unique(intervention_nice)))

  # Set palette
  palette_fun <- ggthemes::canva_pal("Fun and tropical")
  plan_colors <- palette_fun(length(unique(dat$plan_label)))
  names(plan_colors) <- unique(dat$plan_label)

  dat <- dat %>%
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

  # Plot
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

  ggplotly(p, tooltip = "text") %>%
    layout(
      hoverlabel = list(
        bgcolor = "white",
        align = "left"
      )
    )
}


# Scenario check functions ----------

# Function to count adm2s receiving and not receiving specific intervention by adm1 and year
count_adm2_coverage <- function(intervention, plan, year_filter, data) {
  # Filter down to intervention and plan
  filtered_data <- data |>
    filter(
      intervention == !!intervention,
      scenario_name == !!plan
    ) |>
    ungroup()

  receiving <- filtered_data |>
    filter(included == 1) |>
    distinct()

  total_adm2s <- filtered_data |>
    distinct()

  if (year_filter == "Toutes les années") {
    # Summarise across all years
    total_summary <- total_adm2s |>
      group_by(adm1) |>
      summarise(total = n_distinct(adm2), .groups = "drop")

    receiving_summary <- receiving |>
      group_by(adm1) |>
      summarise(receiving = n_distinct(adm2), .groups = "drop")

    summary_table <- total_summary |>
      left_join(receiving_summary, by = "adm1") |>
      mutate(
        receiving = coalesce(receiving, 0),
        not = total - receiving,
        coverage_pct = round(receiving / total * 100, 1),
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
    # Normal per-year summary
    total_summary <- total_adm2s |>
      filter(year == year_filter) |>
      group_by(year, adm1) |>
      summarise(total = n_distinct(adm2), .groups = "drop")

    receiving_summary <- receiving |>
      filter(year == year_filter) |>
      group_by(year, adm1) |>
      summarise(receiving = n_distinct(adm2), .groups = "drop")

    summary_table <- total_summary |>
      left_join(receiving_summary, by = c("year", "adm1")) |>
      mutate(
        receiving = coalesce(receiving, 0),
        not = total - receiving,
        coverage_pct = round(receiving / total * 100, 1)
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

  return(summary_table)
}


# smc_pmc_check <- function(plan, year_filter = NULL) {
#   # Which adm2s are receiving PMC each year
#   pmc_tmp <- static_mix_maps |>
#     st_drop_geometry() |>
#     filter(plan_shortname == !!plan,
#            intervention == "PMC")
#
#   # Apply year filter if provided
#   if (!is.null(year_filter) && length(year_filter) > 0 && !all(is.na(year_filter))) {
#     pmc_tmp <- pmc_tmp |>
#       filter(year %in% year_filter)
#   }
#
#   pmc_tmp <- pmc_tmp |>
#     distinct(year, adm1, adm2, intervention)
#
#   # Which adm2s are receiving SMC each year
#   smc_tmp <- static_mix_maps |>
#     st_drop_geometry() |>
#     filter(plan_shortname == !!plan,
#            intervention == "SMC")
#
#   # Apply year filter if provided
#   if (!is.null(year_filter) && length(year_filter) > 0 && !all(is.na(year_filter))) {
#     smc_tmp <- smc_tmp |>
#       filter(year %in% year_filter)
#   }
#
#   smc_tmp <- smc_tmp |>
#     distinct(year, adm1, adm2, intervention)
#
#   # Complete join by year, adm1, and adm2
#   smc_pmc <- inner_join(pmc_tmp, smc_tmp, by = c("year", "adm1", "adm2")) |>
#     select(year, adm1, adm2)
#
#   # If none return message within "None.", otherwise return all SMC and PMC data
#   if (nrow(smc_pmc) == 0) {
#     NULL
#   } else {
#     smc_pmc
#   }
# }

#-data management
# Load all uploaded scenario files
load_all_uploaded_scenarios <- function(db_path = "scenario_uploads.db", folder = "uploads/scenarios") {
  db <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  uploads <- DBI::dbGetQuery(db, "SELECT name, description, filename, years FROM uploads")
  DBI::dbDisconnect(db)

  if (nrow(uploads) == 0) {
    return(NULL)
  }

  purrr::map_dfr(seq_len(nrow(uploads)), function(i) {
    row <- uploads[i, ]
    file_path <- file.path(folder, row$filename)
    if (!file.exists(file_path)) {
      return(NULL)
    }

    year_sheets <- unlist(strsplit(row$years, ","))
    purrr::map_dfr(year_sheets, function(y) {
      df <- tryCatch(readxl::read_excel(file_path, sheet = y), error = function(e) NULL)
      if (is.null(df)) {
        return(NULL)
      }
      df$year <- as.integer(y)
      df$scenario_name <- row$name
      df$scenario_description <- row$description
      df
    })
  })
}

# Load uploaded cost file
load_all_uploaded_costs <- function(db_path = "cost_uploads.db", folder = "uploads/costs") {
  db <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  uploads <- DBI::dbGetQuery(db, "SELECT name, description, filename FROM uploads")
  DBI::dbDisconnect(db)

  if (nrow(uploads) == 0) {
    return(NULL)
  }

  purrr::map_dfr(seq_len(nrow(uploads)), function(i) {
    row <- uploads[i, ]
    file_path <- file.path(folder, row$filename)
    if (!file.exists(file_path)) {
      return(NULL)
    }

    df <- tryCatch(readxl::read_excel(file_path), error = function(e) NULL)
    if (is.null(df)) {
      return(NULL)
    }

    df$cost_name <- row$name
    df$cost_description <- row$description
    df
  })
}

# load budget data
load_budget_data <- function() {
  history_path <- "generated/budget_history.rds"

  # Check if the history file exists
  if (!file.exists(history_path)) {
    message("No budget history file found")
    return(NULL)
  }

  # Load the history metadata
  history <- readRDS(history_path)

  # If empty, return
  if (nrow(history) == 0 || !"file_path" %in% names(history)) {
    message("Budget history is empty or missing 'file_path'")
    return(NULL)
  }

  # Loop through each path and load budget files
  all_budgets <- lapply(history$file_path, function(path) {
    if (file.exists(path)) {
      readRDS(path)
    } else {
      message("Missing budget file: ", path)
      NULL
    }
  })

  # Filter out failed/missing loads
  all_budgets <- all_budgets[!sapply(all_budgets, is.null)]

  # Combine into one dataframe
  if (length(all_budgets) == 0) {
    message("No valid budget files found")
    return(NULL)
  }

  combined <- dplyr::bind_rows(all_budgets)

  return(combined)
}

# Initializes shared caches and provides reload functions
# Initializes shared caches and provides reload functions
# Update your initSharedDataManager function
# Update your initSharedDataManager function
initSharedDataManager <- function(shared) {
  shared$scenario_uploads_cache <- reactiveVal(NULL)
  shared$cost_upload_cache <- reactiveVal(NULL)
  shared$budget_results <- NULL
  shared$budget_results_available <- FALSE

  shared$reload_scenarios <- function() {
    shared$scenario_uploads_cache(load_all_uploaded_scenarios())
  }

  shared$reload_costs <- function() {
    shared$cost_upload_cache(load_all_uploaded_costs())
  }

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

  shared$reload_all_uploads <- function() {
    shared$reload_scenarios()
    shared$reload_costs()
    shared$reload_budgets()
  }

  # DO NOT call reload_all_uploads() directly here
  # Instead, create an observer to execute it
}

# ----------------------------------------------------------------------------
# process_selections_for_budget(): Pass assumptions
# ----------------------------------------------------------------------------
process_selections_for_budget <- function(selections, scenario_data, cost_data) {
  selections |>
    filter(
      Plan != "Sélectionnez un plan",
      Selected_Cost != "Sélectionnez les coûts"
    ) |>
    rowwise() |>
    mutate(
      scen_data = list(scenario_data |> filter(scenario_name == Plan)),
      cost_option_data = list(cost_data |> filter(cost_name == Selected_Cost)),
      assumptions = list(Adjustments),
      plan_name = Plan,
      cost_name = Selected_Cost,
      assumptions_used = if (length(Adjustments) > 0) {
        paste(unlist(Adjustments), collapse = "; ")
      } else {
        "base de référence"
      },
      ready_for_processing = TRUE
    ) |>
    ungroup()
}

#-Generate budget function------------------------------------------------------
generate_budget <- function(scen_data, cost_data, assumptions) {
  #-SUMMARY------------------------------------------------------------------------------
  # Print a summary of the interventions and number of adm1s/adm2s being targeted
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

  #-Update cost data for processing------------------------------------------------------
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

  #-Assumption variables------------------------------------------------------------------
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

  # Extraire la colonne de population cible selon l'hypothèse
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

  # Extraire les colonnes de population spécifiques
  itn_campaign_pop_col <- get_pop_column("Campagne MII : population cible", "pop_total")
  itn_routine_pop_col <- get_pop_column("Routine MII : population cible", c("pop_0_5", "pop_femme_enceinte"))
  smc_pop_col <- get_pop_column("CPS : population cible", "pop_0_5")

  #-Generate quantifications---------------------------------------------------------------

  #-ITN CAMPAIGNS--------------------------------
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

  #-ITN ROUTINE-------------------------------
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


  #-IPTp-------------------------------------
  ## Assumptions - three doses
  ## of SP (in blister packs of 3 pills)
  ## targeted ANC attendence at 80%
  ## coverage with a 10% buffer stock
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


  #-SMC-----------------------------------------
  ## Since SMC is delivered in two dosage groups:
  ## 3 to <12 months and >12 to 59 months,
  ## the number of packets for each age group needs to be
  ##  quantified and then multiplied by 4, to cover every
  ##  cycle.  In addition, a buffer stock between
  ##  10 -20% should be included to accommodate for loss,
  ##  re-dosing and treatment of children from neighbouring locations.
  ##  Calculation:
  ##    A. Total number of children under 5.
  ##    B. Number of children 3 to <12 months = (18% of A)
  ##    C. 10% buffer stock for children 3 to <12 months = (10% of B)
  ##    D. Total number of packets for children 3 to <12 months needed for one cycle of SMC = (B + C)
  ##    E. Total number of packets for children 3 to <12 months needed for one round of SMC = (4 x D)
  ##    F. Number of children >12 to 59 months = (77% of A)
  ##    G. 10% buffer stock for children >12 to 59 months = (10% of F)
  ##    H. Total number of packets for children >12 to 59 months needed for one cycle of SMC = (F + G)
  ##    I. Total number of packets for children >12 to 59 months needed for one round of SMC = (4 x H)

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

  #-PMC-----------------------------------------------------------
  # Assumptions
  # Antigen coverage rate = 85% (since immunization is being
  # used as the contact point).
  # children 0-1 take 1 tab
  # children 1-2 take 2 tab
  # Since one in four children/infants in Nigeria is underweight),
  # 25% of children <1 year will take half instead of one tablet,
  #  while 25% of children 1-2 years will take one instead of 2 tablets.
  #  A factor of 0.75% was therefore used to quantify the required SP
  #  for each age group.
  #  There will be 4 touch points within a calendar year for PMC
  #  With a 10% buffer added
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


  #-Vaccine----------------------------------------------------
  ## Assumptions
  ## 84% coverage
  ## 10% wasatge
  ## 4 doses per child
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

  # to handle instances where no intervention targeted
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


  #-CASE MANAGEMENT-----------------------------------------------------------------
  # NEED TO WAIT UNTIL METHODS OF QUANTIFICATION ARE SHARED

  #-Combine into one dataframe-----------------------------------------------------
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

#-safe ID handler
safe_id <- function(label) {
  label %>%
    gsub("[^a-zA-Z0-9_]", "_", .) %>%
    gsub("_+", "_", .) %>%
    gsub("^_|_$", "", .)
}
