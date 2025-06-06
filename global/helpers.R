#-------------------------------------------------------------------------------
# Helper functions to call into server code to keep streamlined
#
#-------------------------------------------------------------------------------

#-Leaflet intervention mix map--------------------------------------------------
create_intervention_leaflet <- function(lga_outline, state_outline,
                                        country_outline, intervention_mix_maps,
                                        spatial_scale, state_select,
                                        lga_select,
                                        center_lng = 9, center_lat = 4,
                                        zoom = 5.2) {

  # Highlight option if State or LGA is selected
  if (spatial_scale == "State") {
     single_highlight <- state_outline %>% filter(state == state_select)
  }
  if (spatial_scale == "LGA") {
     single_highlight <- lga_outline %>% filter(state == state_select, lga == lga_select)
  }

  map_data <-
    left_join(lga_outline, intervention_mix_maps, by=c("state" = "adm1", "lga" = "adm2")) |>
    group_by(state, lga, geometry) %>%
    summarise(
      mix_long = paste(unique(intervention_nice), collapse = " + "),
      .groups = "drop"
    ) %>%
    distinct(state, lga, mix_long, geometry)

  color_pal <- colorFactor(
    palette = "Paired", # or use another palette like "Paired", "Dark2"
    domain = map_data$mix_long
  )

  map <- leaflet()  |>
    addTiles() |>
    # Add LGA polygons with color based on intervention_summary
    addPolygons(
      data = map_data,
      fillColor = ~color_pal(mix_long),
      color = "grey",
      weight = 1,
      fillOpacity = 0.9,
      highlightOptions = highlightOptions(
        weight = 3,
        color = "black",
        fillOpacity = 1,
        bringToFront = TRUE
      ),
      label = ~sprintf(
        "<strong>%s</strong><br>State: %s<br>Intervention mix: %s",
        lga, state, mix_long
      ) %>% lapply(htmltools::HTML),
      labelOptions = labelOptions(
        direction = "auto",
        textsize = "10px",
        style = list("font-weight" = "normal", "padding" = "3px 8px"),
        sticky = TRUE
      )
    )  |>
    # Add state boundaries
    addPolygons(
      data = state_outline,
      fillColor = "transparent",
      color = "black",
      weight = 2,
      options = pathOptions(interactive = FALSE)
    )  |>
    # Add national boundaries
    addPolygons(
      data = country_outline,
      fillColor = "transparent",
      color = "black",
      weight = 2,
      options = pathOptions(interactive = FALSE)
    )  |>
    # Add legend (pass the values argument to color_pal)
    addLegend(
      pal = color_pal,
      values = map_data$mix_long,
      title = "Intervention Mix",
      position = "bottomright",
      opacity = 0.7
    )  |>
    setView(lng = center_lng, lat = center_lat, zoom = zoom)


  if (spatial_scale %in% c("State", "LGA")) {

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
create_static_map <- function(lga_outline, state_outline, filtered_data,
                              plan_select, year_value,
                              spatial_scale,
                              state_select,
                              lga_select) {

   p <- NULL  # Initialize plot variable



  if (spatial_scale == "National") {
    static_data <- lga_outline |>
      left_join(filtered_data, by=c("state" = "adm1", "lga" = "adm2"))

    p <- ggplot() +
      geom_sf(data = lga_outline, fill = "grey90", col = "grey", alpha = 0.5) +
      geom_sf(data = static_data, aes(fill = type_intervention, col = type_intervention), alpha = 0.6) +
      geom_sf(data = state_outline, fill = NA, linewidth = 0.7, color = "black") +
      scale_fill_brewer(palette = "Spectral", direction = -1) +
      scale_color_brewer(palette = "Spectral", direction = -1) +
      theme_void(base_size = 14) +
      facet_wrap(vars(intervention_nice)) +
      theme(legend.position = "bottom") +
      labs(
        fill = "Intervention Class (if specified)",
        col = "Intervention Class (if specified)",
        title = paste0("Intervention mix for: ", plan_select, " - ", year_value)
      )
  } else if (spatial_scale == "State" & !is.null(state_select)) {
    state_highlight <- state_outline |> dplyr::filter(state == state_select)
    static_data <- filtered_data
    p <- ggplot() +
      geom_sf(data = lga_outline, fill = "grey90", col = "grey", alpha = 0.5) +
      geom_sf(data = static_data, aes(fill = type_intervention, col = type_intervention), alpha = 0.6) +
      geom_sf(data = state_outline, fill = NA, linewidth = 0.7, color = "black") +
      geom_sf(data = state_highlight, fill = NA, linewidth = 1, color = "red") +
      scale_fill_brewer(palette = "Spectral", direction = -1) +
      scale_color_brewer(palette = "Spectral", direction = -1) +
      theme_void(base_size = 14) +
      facet_wrap(vars(intervention_nice)) +
      theme(legend.position = "bottom") +
      labs(
        fill = "Intervention Class (if specified)",
        col = "Intervention Class (if specified)",
        title = paste0("Intervention mix for: ", plan_select, " - ", year_value)
      )
  } else if (spatial_scale == "LGA" & !is.null(lga_select)) {
    lga_highlight <- lga_outline |> dplyr::filter(state == state_select, lga == lga_select)
    static_data <- filtered_data
    p <- ggplot() +
      geom_sf(data = lga_outline, fill = "grey90", col = "grey", alpha = 0.5) +
      geom_sf(data = static_data, aes(fill = type_intervention, col = type_intervention), alpha = 0.6) +
      geom_sf(data = state_outline, fill = NA, linewidth = 0.7, color = "black") +
      geom_sf(data = lga_highlight, fill = NA, linewidth = 1, color = "red") +
      scale_fill_brewer(palette = "Spectral", direction = -1) +
      scale_color_brewer(palette = "Spectral", direction = -1) +
      theme_void(base_size = 14) +
      facet_wrap(vars(intervention_nice)) +
      theme(legend.position = "bottom") +
      labs(
        fill = "Intervention Class (if specified)",
        col = "Intervention Class (if specified)",
        title = paste0("Intervention mix for: ", plan_select, " - ", year_value)
      )
  }

  return(p)  # Explicitly return the ggplot object
}

#-Ribbon icons function---------------------------------------------------------
#Create Icon Summary Cards
create_icon_summaries <- function(spatial_scale, state_select, year_select,
                                  lga_select, currency_select, data) {

    # Select the data based on the spatial scale
    if (spatial_scale == "National") {
      data <- data |>
        dplyr::filter(
          currency == currency_select
        )
      pop_data <-
        target_population |>
        summarise(
          pop_total = sum(pop_total, na.rm = TRUE),
          pop_0_5 = sum(pop_0_5, na.rm = TRUE),
          pop_pw = sum(pop_pw, na.rm = TRUE)
        )

    } else if (spatial_scale == "State") {
      data <- data |>
        dplyr::filter(
          adm1 == state_select,
          currency == currency_select
        ) |>
        group_by(adm1)

      pop_data <-
        target_population |>
        dplyr::filter(
          adm1 == state_select) |>
        summarise(
          pop_total = sum(pop_total, na.rm = TRUE),
          pop_0_5 = sum(pop_0_5, na.rm = TRUE),
          pop_pw = sum(pop_pw, na.rm = TRUE)
        )
    } else if (spatial_scale == "LGA") {
      data <- data |>
        dplyr::filter(
          adm1 == state_select,
          adm2 == lga_select,
          currency == currency_select
        ) |>
        group_by(adm1, adm2)

      pop_data <-
        target_population |>
        dplyr::filter(
          adm1 == state_select,
          adm2 == lga_select) |>
        summarise(
          pop_total = sum(pop_total, na.rm = TRUE),
          pop_0_5 = sum(pop_0_5, na.rm = TRUE),
          pop_pw = sum(pop_pw, na.rm = TRUE)
        )
    } else {
      data <- NULL
    }

    # Filter by year or aggregate over all years
    if (year_select == "All Years") {
      data <- data |>
        dplyr::select(cost_element) |>
        dplyr::summarise(
          total_budget = sum(cost_element, na.rm = TRUE)) |>
        bind_cols(pop_data) |>
        mutate( total_budget_per_person = total_budget / pop_total)

    } else {
      data <- data |>
        dplyr::select(cost_element) |>
        dplyr::summarise(
          total_budget = sum(cost_element, na.rm = TRUE)) |>
        bind_cols(pop_data) |>
        mutate( total_budget_per_person = total_budget / pop_total)
    }

    # Set the currency symbol based on the currency_select value
    currency_symbol <- if (currency_select == "USD") "$" else "₦"

    # Create the UI: Using layout_column_wrap to space 5 boxes evenly across the width
    layout_column_wrap(
      width = 1/5,
      # Total Population
      card(
        card_header(
          tagList(
            icon("users", class = "fa-2x"),
            " Total Population"
          )
        ),
        card_body(
          h3(formatC(data$pop_total, format = "f", digits = 0, big.mark = ","), style = "font-weight: bold;"),
          p("People")
        ),
        class = "bg-info text-white"
      ),
      # Population u5
      card(
        card_header(
          tagList(
            icon("child", class = "fa-2x"),
            " Population u5"
          )
        ),
        card_body(
          h3(formatC(data$pop_0_5,
                     format = "d",
                     big.mark = ","),
             style = "font-weight: bold;"),
          p("Under 5")
        ),
        class = "bg-success text-white"
      ),
      # Pregnant Women (pop_pw)
      card(
        card_header(
          tagList(
            icon("female", class = "fa-2x"),
            " Pregnant Women"
          )
        ),
        card_body(
          h3(formatC(data$pop_pw,
                     format = "d", big.mark = ","), style = "font-weight: bold;"),
          p("Women")
        ),
        class = "bg-warning text-white"
      ),
      # Total Budget
      card(
        card_header(
          tagList(
            icon("dollar-sign", class = "fa-2x"),
            " Total Budget"
          )
        ),
        card_body(
          h4(paste0(currency_symbol,
                    formatC(as.numeric(data$total_budget),
                            format = "f", digits = 0, big.mark = ","))),
          p(currency_select)
        ),
        class = "bg-primary text-white"
      ),
      # Cost Per Person
      card(
        card_header(
          tagList(
            icon("calculator", class = "fa-2x"),
            " Cost Per Person"
          )
        ),
        card_body(
          h4(paste0(currency_symbol,
                    formatC(as.numeric(data$total_budget_per_person),
                            format = "f", digits = 2, big.mark = ","))),
          p(currency_select)
        ),
        class = "bg-danger text-white"
      )
    )
  }


#-TOTAL COST PROCESSING-------------------------------------------------------------------
process_budget_data <- function(spatial_scale,
                                state_select,
                                lga_select,
                                currency_select,
                                data) {

  # 1) Select the data based on the input selections
  data <- if (spatial_scale == "National") {
    data |>
      dplyr::filter(
        currency == currency_select
      )
  } else if (spatial_scale == "State") {
    data |>
      dplyr::filter(
        adm1 == state_select,
        currency == currency_select
      )
  } else if (spatial_scale == "LGA") {
    data |>
      dplyr::filter(
        adm1 == state_select,
        adm2 == lga_select,
        currency == currency_select
      )
  } else {
    return(NULL)
  }

# 2) Calculate total costs per intervention
data <-
  data |>
  group_by(scenario_name, scenario_description,
           cost_name, cost_description, intervention_nice) |>
  summarise(states_targeted = n_distinct(adm1),
            lgas_targeted = n_distinct(paste(adm1, adm2, sep = "_")),
            total_cost = sum(cost_element, na.rm = TRUE) )

  return(data)
}

#-Process individual item data------------------------------------------------
process_item_data <- function(spatial_scale,
                                state_select,
                                lga_select,
                                currency_select,
                              data) {

  # 1) Select the data based on the input selections
  data <- if (spatial_scale == "National") {
    data |>
      dplyr::filter(
        currency == currency_select
      )
  } else if (spatial_scale == "State") {
    data |>
      dplyr::filter(
        adm1 == state_select,
        currency == currency_select
      )
  } else if (spatial_scale == "LGA") {
    data |>
      dplyr::filter(
        adm1 == state_select,
        adm2 == lga_select,
        currency == currency_select
      )
  } else {
    return(NULL)
  }

  # 2) Calculate total costs per intervention
  data <-
    data |>
    group_by(scenario_name, scenario_description,
             cost_name, cost_description, intervention_nice,
             cost_class, unit) |>
    summarise(cost_element = sum(cost_element, na.rm = TRUE))

  return(data)
}


#-BUDGET TABLE WITH FORMATTING----------------------------------------------------------
create_budget_table <- function(processed_data, currency_select, baseline_data = NULL) {
  # Display names for columns
  col_names <- c(
    "Plan" = "scenario_name",
    "Plan description" = "scenario_description",
    "Cost data" = "cost_name",
    "Cost description" = "cost_description",
    "Intervention" = "intervention_nice",
    "States Targeted" = "states_targeted",
    "LGAs Targeted" = "lgas_targeted",
    "Total Cost" = "total_cost"
  )

  currency_symbol <- if (currency_select == "USD") "$" else "₦"

  # Add comparison columns if baseline is provided
  if (!is.null(baseline_data)) {
    baseline_lookup <- baseline_data %>%
      ungroup() %>%
      dplyr::select(intervention_nice, total_cost) %>%
      dplyr::rename(baseline_total = total_cost)

    processed_data <- processed_data %>%
      dplyr::left_join(baseline_lookup, by = "intervention_nice") %>%
      dplyr::mutate(
        diff_flag = total_cost != baseline_total,
        diff_type = case_when(
          is.na(baseline_total) ~ NA_character_,
          total_cost > baseline_total ~ "increase",
          total_cost < baseline_total ~ "decrease",
          TRUE ~ NA_character_
        )
      )
  }

  # Ensure diff_type is not all NA (needed for styling)
  if (!is.null(baseline_data)) {
    processed_data <- processed_data %>%
      mutate(diff_type = ifelse(is.na(diff_type), "none", diff_type))
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
      )
    )
  ) %>%
    DT::formatStyle(
      columns = 1:ncol(processed_data),
      fontSize = '14px'
    ) %>%
    DT::formatCurrency(
      columns = "Total Cost",  # match label!
      currency = currency_symbol,
      interval = 3,
      mark = ",",
      digits = 0
    )

  # Only apply highlighting if diff_type exists
  if (!is.null(baseline_data) && "diff_type" %in% names(processed_data)) {
    dt <- dt %>%
      DT::formatStyle(
        "Total Cost",  # formatted label
        backgroundColor = DT::styleEqual(
          c("increase", "decrease"),
          c("#f88a73", "lightgreen")
        ),
        valueColumns = "diff_type"
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

  currency_symbol <- if(currency_select == "USD") "$" else "₦"

  intervention_totals <-
    data  |>
    arrange(desc(total_cost))

  plot_ly(
    data = intervention_totals,
    type = "treemap",
    labels = ~intervention_nice,
    parents = "",
    values = ~ round(total_cost,0),
    textinfo = "label+value",
    hovertemplate = paste(
      "<b>%{label}</b><br>",
      "Total Cost: ", currency_symbol, "%{value:,.0f}<br>",
      "<extra></extra>"
    )
  )
}

#-STACKED BAR PLOT----------------------------------------------------
stacked_plot <- function(data, currency_select) {

  currency_symbol <- if(currency_select == "USD") "$" else "₦"

  proc_impl_split <-
    data  |>
    group_by(scenario_name, scenario_description,
             cost_name, cost_description, intervention_nice,
             cost_class) |>
    summarise(cost_element = sum(cost_element, na.rm = TRUE)) |>
    group_by(intervention_nice) |>
    mutate(total_intervention_cost = sum(cost_element)) |>
    ungroup() |>
    mutate(intervention_nice = reorder(intervention_nice, -total_intervention_cost))

  plot_ly(
    data = proc_impl_split,
    colors = c("#3779E5", "#181D31",  "#81CF98")
  ) |>
    add_bars(
      x = ~intervention_nice,
      y = ~cost_element,
      color = ~cost_class,
      text = ~paste0(cost_class, "<br>",
                     "Cost: ", scales::dollar(cost_element, prefix = currency_symbol)),
      hoverinfo = "text",
      textposition = "none", # Don't show as a label on bar
      hoverlabel = list(namelength = -1) # Don't truncate labels
    ) |>
    layout(
      barmode = 'stack',
      xaxis = list(
        title = "",
        tickfont = list(size = 12)
      ),
      yaxis = list(
        title = paste0("Cost (", currency_select, ")"),
        tickfont = list(size = 12)
      ),
      legend = list(
        x = 0.8,
        y = 0.95,
        xanchor = "right",
        yanchor = "top",
        font = list(size = 12),
        bgcolor = 'rgba(255,255,255,0.5)'
      ),
      font = list(size = 14)
    )
}

#-stacked proportional plot-----------------------------------------
stacked_plot_prop <- function(data, currency_select) {

  currency_symbol <- if (currency_select == "USD") "$" else "₦"

  proc_impl_split <-
    data |>
    group_by(scenario_name, scenario_description,
             cost_name, cost_description, intervention_nice,
             cost_class) |>
    summarise(cost_element = sum(cost_element, na.rm = TRUE)) |>
    group_by(intervention_nice) |>
    mutate(total_intervention_cost = sum(cost_element),
           cost_prop = cost_element / total_intervention_cost) |>
    ungroup() |>
    mutate(intervention_nice = reorder(intervention_nice, -total_intervention_cost))

  plot_ly(
    data = proc_impl_split,
    colors = c("#3779E5", "#181D31", "#81CF98")
  ) |>
    add_bars(
      x = ~intervention_nice,
      y = ~cost_prop,
      color = ~cost_class,
      text = ~paste0(cost_class, "<br>",
                     "Proportion: ", scales::percent(cost_prop, accuracy = 0.1), "<br>",
                     "Cost: ", scales::dollar(cost_element, prefix = currency_symbol)),
      hoverinfo = "text",
      textposition = "none",
      hoverlabel = list(namelength = -1)
    ) |>
    layout(
      barmode = 'stack',
      xaxis = list(
        title = "",
        tickfont = list(size = 12)
      ),
      yaxis = list(
        title = "Proportion of Total Intervention Cost",
        tickformat = "%",
        tickfont = list(size = 12)
      ),
      legend = list(
        x = 0.8,
        y = 0.95,
        xanchor = "right",
        yanchor = "top",
        font = list(size = 12),
        bgcolor = 'rgba(255,255,255,0.5)'
      ),
      font = list(size = 14)
    )
}

#-lolipop plot for specific elements---------------------------------------------
lolipop_plot <- function(data, currency_select){

  currency_symbol <- if(currency_select == "USD") "$" else "₦"

  # Get top costs and include intervention information
  top_costs <-
    data |>
    arrange(desc(cost_element)) |>
    head(15) |>
    mutate(
      label = paste0(intervention_nice, " ", cost_class, " cost", ifelse("unit" %in% names(data), unit, ""))
    )

  plot_ly(colors = c("#3779E5", "#181D31", "#81CF98")) |>
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
      text = ~paste0(
        intervention_nice, "<br>",
        "Component: ", cost_class, "<br>",
        "Cost: ", scales::dollar(cost_element, prefix = currency_symbol)
      ),
      hoverinfo = "text"
    ) |>
    layout(
      xaxis = list(
        title = paste0("Cost (", currency_select, ")"),
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
        title = list(text = "Component Category"),
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
                          state_select = NULL,
                          lga_select = NULL,
                          currency_select,
                          data) {

  # Format dataset based on map_level
    if(map_level == "State"){

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
          total_budget = sum(cost_element, na.rm = TRUE)) |>
        left_join(pop_data) |>
        mutate( total_budget_per_person = total_budget / pop_total)

  } else if(map_level == "LGA"){
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
        total_budget = sum(cost_element, na.rm = TRUE)) |>
      left_join(pop_data) |>
      mutate( total_budget_per_person = total_budget / pop_total)
  }

  # Determine which values to map and the legend title
  values <- if(map_type  == "total") data$total_budget else data$total_budget_per_person
  title <- if(map_type == "total") "Total Cost" else "Cost per Person"

  # Join with shapefiles
  if(map_level == "State"){
    data <-
      state_outline |>
      dplyr::left_join(data, by = c("state" = "adm1"))
  } else if(map_level == "LGA"){
    data <-
      lga_outline |>
      dplyr::left_join(data, by = c("state" = "adm1",
                                    "lga" = "adm2"))
  }

  # Create label for each feature
  label_title <-
    if(map_level == "State") paste0(data$state, ":") else paste0(data$state, ", ", data$lga, ":")

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
      fillColor = ~pal(values),
      weight = 2,
      opacity = 1,
      color = "grey",
      dashArray = "3",
      fillOpacity = 1,
      label = ~paste0(
        label_title,
        format_cost_label(
          if(map_type == "total") total_budget else total_budget_per_person,
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
        prefix = if(currency_select == "USD") "$" else "₦"
      )
    )

  # Add highlight if a state is selected and the map is at State level
  if(map_level == "State" && !is.null(state_select)) {
    state_hl <- state_outline %>% dplyr::filter(state == state_select)
    map <- map %>%
      addPolylines(
        data = state_hl,
        color = "red",
        weight = 3,
        opacity = 1,
        group = "highlight"
      )

  }

  # Add highlight if an LGA is selected and the map is at LGA level
  if(map_level == "LGA" && !is.null(lga_select)) {
    lga_hl <- lga_outline %>% dplyr::filter(state == state_select, lga == lga_select)
    map <- map %>%
      addPolylines(
        data = lga_hl,
        color = "red",
        weight = 3,
        opacity = 1,
        group = "highlight"
      )
  }

  map
}

format_cost_label <- function(value, currency_select, is_per_person = FALSE) {
  currency_symbol <- if(currency_select == "USD") "$" else "₦"

  if(is_per_person) {
    paste0(currency_symbol, round(value, 2))
  } else {
    paste0(currency_symbol, format(round(value), big.mark = ","))
  }
}

#-Budget comparison plot-------------------------------------------------------------------------
# Cost Comparison Plot
budget_barchart <- function(data, currency_select) {

  currency_symbol <- if (currency_select == "USD") "$" else "₦"

  data <- data |>
    mutate(
      budget_millions = round(total_budget / 1e6),
      hover_text = paste0("Plan: ", plan_label, "<br>",
                          "Total Cost: ", currency_symbol, format(budget_millions, big.mark = ","), "M")
    )

  # Generate palette safely
  # palette_fun <- ggthemes::canva_pal("Fun and tropical")
  # colors <- palette_fun(length(unique(data$plan_labels)))

  unique_plans <- unique(data$plan_label)
  palette_fun <- ggthemes::canva_pal("Fun and tropical")
  colors <- palette_fun(length(unique_plans))

  p <- ggplot(data, aes(x = plan_label, y = budget_millions, fill = plan_label, text = hover_text)) +
    geom_bar(stat = "identity", width = 0.6) +
    geom_text(aes(label = paste0(currency_symbol, format(budget_millions, big.mark = ","), "M")),
              vjust = -0.5, size = 4) +
    theme_minimal() +
    labs(title = paste("Total Cost (", currency_select, " in Millions)"), x = "", y="") +
    theme(text = element_text(size = 12)) +
    scale_y_continuous(labels = scales::comma) +
    guides(fill = "none") +
    scale_fill_manual(values = colors)

  ggplotly(p, tooltip = "text") %>%
    layout(
      hoverlabel = list(bgcolor = "white"),
      xaxis = list(tickangle = -45)  # rotates labels to avoid overlap
    )
}

# cost difference plot-----------------------------------------------------------------------
budget_diff_chart <- function(data, currency_select){


  currency_symbol <- if (currency_select == "USD") "$" else "₦"

  p <-
    ggplot(data, aes(x = difference_millions, y = label, text = hover_text)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    geom_segment(aes(x = 0, xend = difference_millions, y = label, yend = label),
                 color = ifelse(data$difference_millions >= 0, "#ED7D31", "#4472C4")) +
    geom_point(size = 4) +
    theme_minimal() +
    labs(title = paste("Change in Cost (", currency_select, " in Millions)"), y = "", x=" ") +
    theme(text = element_text(size = 12),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank()) +
    scale_x_continuous(labels = scales::comma)

  ggplotly(p, tooltip = "text") %>%
    layout(hoverlabel = list(bgcolor = "white"))

}

#-final cost plot-------------------------------------------------------------------------
#--- Helper: Process and format data for plotting ----
prepare_cost_plot_data <- function(plan_select, currency_select, spatial_scale, year_select, baseline_plan) {
  currency_symbol <- if (currency_select == "USD") "$" else "₦"

  dat <- purrr::map_df(plan_select, function(plan) {
    process_budget_data(
      plan_select = plan,
      year_select = year_select,
      spatial_scale = spatial_scale,
      state_select = NULL,
      lga_select = NULL,
      currency_select = currency_select
    ) %>%
      mutate(plan = plan)
  }) %>%
    mutate(
      total_cost = total_cost / 1e6,
      tc_print = case_when(
        currency_select == "NGN" ~ paste0(currency_symbol, format(round(total_cost, 0), big.mark = ","), "m"),
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
  currency_symbol <- if (currency_select == "USD") "$" else "₦"

  # Add label column
  baseline <- baseline_processed %>%
    mutate(plan_label = paste(unique(scenario_name), "with", unique(cost_name))) %>%
    mutate(type = "baseline")

  comparisons <- comparison_processed %>%
    mutate(plan_label = paste(scenario_name, "with", cost_name)) %>%
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
        currency_select == "NGN" ~ paste0(currency_symbol, format(round(total_cost, 0), big.mark = ","), "m"),
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
    )

  # Plot
  p <- ggplot(dat, aes(x = total_cost, y = reorder(intervention_nice, total_cost))) +
    geom_col(aes(fill = plan_label), width = 0.7) +
    geom_col(
      data = dat %>% filter(type == "comparison"),
      aes(
        x = total_cost,
        y = reorder(intervention_nice, total_cost)
      ),
      fill = NA,
      color = dat %>% filter(type == "comparison") %>% pull(outline_color),
      linewidth = dat %>% filter(type == "comparison") %>% pull(outline_width)
    ) +
    geom_text(aes(label = tc_print, x = total_cost + max(total_cost) * 0.02), size = 10, hjust = 0) +
    scale_fill_manual(values = plan_colors) +
    facet_wrap(~plan_label, scales = "free_y") +
    theme_bw(base_size = 18) +
    labs(
      x = paste0("Total Cost (", currency_symbol, " Millions)"),
      y = NULL,
      fill = "Plan"
    ) +
    theme(
      strip.text = element_text(face = "bold"),
      legend.position = "none"
    ) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.2)))

  return(p)
}

# Scenario check functions ----------

# Function to count LGAs receiving and not receiving specific intervention by state and year
count_lga_coverage <- function(intervention, plan, year_filter = NULL,
                               data ) {
  # Get LGAs receiving intervention
  receiving <-
    data |>
    filter(intervention == !!intervention,
           scenario_name == !!plan) |>
    filter(included == 1)

  # Apply year filter if provided
  if (!is.null(year_filter)) {
    receiving <- receiving |>
      filter(year %in% year_filter)
  }

  receiving <- receiving |>
    distinct(year, state, lga)

  # Get total LGAs per state
  total_lgas <-
    data |>
    filter(intervention == !!intervention,
           scenario_name == !!plan)

  # Apply year filter if provided
  if (!is.null(year_filter)) {
    total_lgas <- total_lgas |>
      filter(year %in% year_filter)
  }

  total_lgas <-
    total_lgas |>
    distinct(year, state, lga)

  # Calculate receiving and not receiving counts
  total_lgas |>
    group_by(year, state) |>
    summarise(total = n_distinct(lga)) |>
    left_join(
      receiving |>
        group_by(year, state) |>
        summarise(receiving = n_distinct(lga)),
      by = c("year", "state")
    ) |>
    mutate(
      receiving = coalesce(receiving, 0),
      not = total - receiving,
      coverage_pct = round(receiving / total * 100, 1)
    ) |>
    select(Year = year,
           State = state,
           Total = total,
           Covered = receiving,
           Uncovered = not,
           `Coverage %` = coverage_pct) |>
    arrange(Year, State) |>
    ungroup()
}

# smc_pmc_check <- function(plan, year_filter = NULL) {
#   # Which LGAs are receiving PMC each year
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
#     distinct(year, state, lga, intervention)
#
#   # Which LGAs are receiving SMC each year
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
#     distinct(year, state, lga, intervention)
#
#   # Complete join by year, state, and lga
#   smc_pmc <- inner_join(pmc_tmp, smc_tmp, by = c("year", "state", "lga")) |>
#     select(year, state, lga)
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

  if (nrow(uploads) == 0) return(NULL)

  purrr::map_dfr(seq_len(nrow(uploads)), function(i) {
    row <- uploads[i, ]
    file_path <- file.path(folder, row$filename)
    if (!file.exists(file_path)) return(NULL)

    year_sheets <- unlist(strsplit(row$years, ","))
    purrr::map_dfr(year_sheets, function(y) {
      df <- tryCatch(readxl::read_excel(file_path, sheet = y), error = function(e) NULL)
      if (is.null(df)) return(NULL)
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

  if (nrow(uploads) == 0) return(NULL)

  purrr::map_dfr(seq_len(nrow(uploads)), function(i) {
    row <- uploads[i, ]
    file_path <- file.path(folder, row$filename)
    if (!file.exists(file_path)) return(NULL)

    df <- tryCatch(readxl::read_excel(file_path), error = function(e) NULL)
    if (is.null(df)) return(NULL)

    df$cost_name <- row$name
    df$cost_description <- row$description
    df
  })
}

# load budget data
load_most_recent_budget <- function() {
  # Check if the history file exists
  history_path <- "generated/budget_history.rds"
  if (!file.exists(history_path)) {
    message("No budget history file found")
    return(NULL)
  }

  # Load the budget history
  history <- readRDS(history_path)

  # If no history, return NULL
  if (nrow(history) == 0) {
    message("Budget history is empty")
    return(NULL)
  }

  # Get the most recent budget file path
  most_recent <- history[nrow(history), "file_path"]

  # Check if the file exists
  if (!file.exists(most_recent)) {
    message("Most recent budget file not found: ", most_recent)
    return(NULL)
  }

  # Load the budget data
  message("Loading most recent budget from: ", most_recent)
  budget_data <- readRDS(most_recent)

  return(budget_data)
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
    budget_data <- load_most_recent_budget()

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

#-Process selections for budget------------------------------------------------
process_selections_for_budget <- function(selections, scenario_data, cost_data) {
  # Extract relevant info from selections dataframe
  selections %>%
    filter(
      Plan != "Select a plan" &
        !is.na(Selected_Cost) &
        !is.na(Target_Population)
    ) %>%
    rowwise() %>%
    mutate(
      # Get scenario data for this plan
      scen_data = list(scenario_data %>% filter(scenario_name == Plan)),
      # Get cost data for this cost option
      cost_option_data = list(cost_data %>% filter(cost_name == Selected_Cost)),
      # Mark for processing
      ready_for_processing = TRUE
    )
}
#-Generate budget function------------------------------------------------------
generate_budget <- function(scen_data, cost_data){


  #-SUMMARY------------------------------------------------------------------------------
  # Print a summary of the interventions and number of states/LGAs being targeted
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
      summarise(states_targeted = n_distinct(adm1),
                lgas_targeted = n_distinct(paste(adm1, adm2, sep = "_"))
      )
  )
  cat(scen_data$scenario_description[1])


  #-Add target population data------------------------------------------------------------
  target_population <-
    readxl::read_xlsx(
      "data/nga-demo-data-pre-processed/data-needs-not-user-defined.xlsx",
      sheet = "population"
    )

  #-Generate quantifications---------------------------------------------------------------

  #-ITN CAMPAIGNS--------------------------------
  ## Assumptions - net quant is
  ## pop / 1.8 and  that 50
  ## nets make up a bale
  itn_campaign_data <-
    scen_data |>
    select(
      adm1, adm2, year, contains("itn_campaign"),
      scenario_name, scenario_description
    ) |>
    filter(
      code_itn_campaign == 1
    ) |>
    left_join(
      target_population |>
        select(
          adm1, adm2, pop_total
        )
    ) |>
    mutate(
      quant_nets = pop_total / 1.8
    ) |>
    mutate(
      quant_bales = quant_nets / 50
    ) |>
    rename(
      target_pop = pop_total
    ) |>
    mutate(
      code_intervention = "itn_campaign"
    ) |>
    mutate(
      type_intervention = type_itn_campaign
    ) |>
    pivot_longer(
      cols=starts_with("quant"),
      names_to = "unit",
      values_to = "quantity",
      names_prefix = "quant_"
    ) |>
    select (
      adm1, adm2, year,
      scenario_name, scenario_description,
      code_intervention, type_intervention,
      target_pop, unit, quantity
    ) |>
    mutate(
      unit = case_when(
        unit == "nets" ~ "per ITN",
        unit == "bales" ~ "per bale"
      )
    )

  #-ITN ROUTINE-------------------------------
  ## Assumptions - nets needed are
  ## 30% of pw and u5 pop
  itn_routine_quantifications <-
    scen_data |>
    select(
      adm1, adm2, year, contains("itn_routine"),
      scenario_name, scenario_description
    ) |>
    filter(
      code_itn_routine == 1
    ) |>
    left_join(
      target_population |>
        select(
          adm1, adm2, pop_pw, pop_0_5)
    ) |>
    mutate(
      target_pop = pop_pw + pop_0_5
    ) |>
    mutate(
      quant_nets = target_pop * 0.3
    ) |>
    select(-pop_pw) |>
    select(-pop_0_5)|>
    mutate(
      code_intervention = "itn_routine"
    ) |>
    mutate(
      type_intervention = type_itn_routine
    ) |>
    pivot_longer(
      cols=starts_with("quant"),
      names_to = "unit",
      values_to = "quantity",
      names_prefix = "quant_"
    ) |>
    select (
      adm1, adm2, year,
      scenario_name, scenario_description,
      code_intervention, type_intervention,
      target_pop, unit, quantity
    ) |>
    mutate(
      unit = case_when(
        unit == "nets" ~ "per ITN",
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
      adm1, adm2, year, contains("iptp"),
      scenario_name, scenario_description
    ) |>
    filter(
      code_iptp == 1
    ) |>
    left_join(
      target_population |>
        select(
          adm1, adm2, pop_pw
        )
    ) |>
    mutate(
      quant_sp_doses = pop_pw * 0.8 * 3 * (1.1)
    ) |>
    mutate(
      target_pop = pop_pw
    ) |>
    mutate(
      code_intervention = "iptp"
    ) |>
    mutate(
      type_intervention = type_iptp
    ) |>
    pivot_longer(
      cols=starts_with("quant"),
      names_to = "unit",
      values_to = "quantity",
      names_prefix = "quant_"
    ) |>
    select (
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
  smc_monthly_rounds <- 4    #smc given over 4 months
  smc_pop_prop_3_11 <- 0.18  # 18% of the under 5 population is 3-11 months
  smc_pop_prop_12_59 <- 0.77 # 77% of the under 5 population is 12-59 months
  buffer = 1.1 # includes 10% buffer

  smc_quantification <-
    scen_data |>
    select(
      adm1, adm2, year, contains("smc"),
      scenario_name, scenario_description
    ) |>
    filter(
      code_smc == 1
    ) |>
    left_join(
      target_population |>
        select(
          adm1, adm2, pop_0_5
        )
    ) |>
    mutate(
      quant_smc_spaq_3_11_months = pop_0_5 *  smc_pop_prop_3_11 * smc_monthly_rounds * buffer,
      quant_smc_spaq_12_59_months = pop_0_5 *  smc_pop_prop_12_59 * smc_monthly_rounds * buffer,
      quant_smc_child = pop_0_5 * (smc_pop_prop_3_11 + smc_pop_prop_12_59)
    ) |>
    mutate(
      target_pop = quant_smc_child
    ) |>
    mutate(
      code_intervention = "smc"
    ) |>
    mutate(
      type_intervention = type_smc
    ) |>
    pivot_longer(
      cols=starts_with("quant"),
      names_to = "unit",
      values_to = "quantity",
      names_prefix = "quant_smc_"
    ) |>
    select (
      adm1, adm2, year,
      scenario_name, scenario_description,
      code_intervention, type_intervention,
      target_pop, unit, quantity
    ) |>
    mutate(
      unit = case_when(
        unit == "spaq_3_11_months" ~ "per SPAQ pack 3-11 month olds",
        unit == "spaq_12_59_months" ~ "per SPAQ pack 12-59 month olds",
        unit == "child" ~ "per child",
      )
    )



  #-PMC-----------------------------------------------------------
  ## Assumptions
  ## Antigen coverage rate = 85% (since immunization is being
  ## used as the contact point).
  ## children 0-1 take 1 tab
  ## children 1-2 take 2 tab
  ## Since one in four children/infants in Nigeria is underweight),
  ## 25% of children <1 year will take half instead of one tablet,
  ##  while 25% of children 1-2 years will take one instead of 2 tablets.
  ##  A factor of 0.75% was therefore used to quantify the required SP
  ##  for each age group.
  ##  There will be 4 touch points within a calendar year for PMC
  ##  With a 10% buffer added
  pmc_quantification <-
    scen_data |>
    select(
      adm1, adm2, year, contains("pmc"),
      scenario_name, scenario_description
    ) |>
    filter(
      code_pmc == 1
    ) |>
    left_join(
      target_population |>
        select(
          adm1, adm2, pop_0_1, pop_1_2
        )
    ) |>
    mutate(
      quant_pmc_sp_0_1_years = pop_0_1 * 0.85 * 4 * 0.75 * 1.1,
      quant_pmc_sp_1_2_years = pop_1_2 * 0.85 * 4 * 2 * 0.75 * 1.1,
      quant_pmc_sp_total = quant_pmc_sp_0_1_years + quant_pmc_sp_1_2_years,
      quant_pmc_child =  pop_0_1 * 0.85 + pop_1_2 * 0.85
    )  |>
    select(-quant_pmc_sp_0_1_years, -quant_pmc_sp_1_2_years) |>
    mutate(
      target_pop = quant_pmc_child
    ) |>
    mutate(
      code_intervention = "pmc"
    ) |>
    mutate(
      type_intervention = type_pmc
    ) |>
    pivot_longer(
      cols=starts_with("quant"),
      names_to = "unit",
      values_to = "quantity",
      names_prefix = "quant_pmc_"
    ) |>
    select (
      adm1, adm2, year,
      scenario_name, scenario_description,
      code_intervention, type_intervention,
      target_pop, unit, quantity
    ) |>
    mutate(
      unit = case_when(
        unit == "sp_total" ~ "per SP",
        unit == "child" ~ "per child",
      )
    )



  #-Vaccine----------------------------------------------------
  ## Assumptions
  ## 84% coverage
  ## 7% wasatge
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
          adm1, adm2, pop_vaccine_5_36_months
        )
    ) |>
    mutate(
      quant_vacc_doses = pop_vaccine_5_36_months * 0.84 * 1.07 * 4,
      quant_vacc_child = pop_vaccine_5_36_months * 0.84
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
      cols=starts_with("quant"),
      names_to = "unit",
      values_to = "quantity",
      names_prefix = "quant_vacc_"
    ) |>
    select (
      adm1, adm2, year,
      scenario_name, scenario_description,
      code_intervention, type_intervention,
      target_pop, unit, quantity
    ) |>
    mutate(
      unit = case_when(
        unit == "doses" ~ "per dose",
        unit == "child" ~ "per child",
      )
    )



  #-CASE MANAGEMENT-----------------------------------------------------------------
  # Still using this data as need to discuss quantification methods with program
  case_management_quantification <-
    read.csv(
      "data/nga-demo-data-pre-processed/cm-quant-data.csv"
    ) |>
    mutate(scenario_name =  scen_data$scenario_name,
           scenario_description = scen_data$scenario_description,
           year = scen_data$year,
           code_intervention = "cm_public") |>
    # pivot longer
    pivot_longer(
      cols = starts_with("cm"),
      names_to = "unit",
      values_to = "quantity",
      names_prefix = "cm_"
    ) |>
    # create a type_intervention column that takes the value of the unit column
    mutate(
      type_intervention = case_when(
        unit == "rdt_kit_quantity" ~ "RDT kits",
        unit == "act_packs_quantity" ~ "AL",
        unit == "iv_artesunate_quantity" ~ "Artesunate injections",
        unit == "ras_quantity" ~ "RAS"
      )
    ) |>
    mutate(
      unit = case_when(
        unit == "rdt_kit_quantity" ~ "per RDT kit",
        unit == "act_packs_quantity" ~ "per AL",
        unit == "iv_artesunate_quantity" ~ "per 60mg powder",
        unit == "ras_quantity" ~ "per RAS"
      )
    )|>
    select(
      adm1, adm2, year,
      scenario_name, scenario_description,
      code_intervention, type_intervention,
      unit, quantity
    )

  #-Combine into one dataframe-----------------------------------------------------
  budget <-
    bind_rows(itn_campaign_data,
              itn_routine_quantifications,
              iptp_quantifications,
              smc_quantification,
              pmc_quantification,
              vacc_quantification,
              case_management_quantification) |>
    # join with the cost data
    left_join(
      cost_data,
      by = c("code_intervention", "type_intervention", "unit")
    ) |>
    filter(
      !is.na(cost_class)
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
        currency == "ngn_cost" ~ "NGN"
      )
    ) |>
    mutate(
      intervention_nice = case_when(
        code_intervention == "cm_public" ~ "Case Management Public",
        code_intervention == "cm_private" ~ "Case Management Private",
        code_intervention == "iptp" ~ "IPTp",
        code_intervention == "vacc" ~ "Vaccine",
        code_intervention == "itn_routine" ~ "ITN Routine",
        code_intervention == "itn_campaign" ~ "ITN Campaign",
        code_intervention == "smc" ~ "SMC",
        code_intervention == "pmc" ~ "PMC",
        code_intervention == "irs" ~ "IRS",
        code_intervention == "lsm" ~ "LSM",
        TRUE ~ code_intervention)
    )


  return(budget)

}
