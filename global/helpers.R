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

  # Define color palette based on unique values in intervention_summary
  color_pal <- colorFactor(
    palette = colorRampPalette(RColorBrewer::brewer.pal(12, "Paired"))(length(unique(intervention_mix_maps$intervention_summary))),
    domain = unique(intervention_mix_maps$intervention_summary)
  )

  # Highlight option if State or LGA is selected
  if (spatial_scale == "State") {
     single_highlight <- state_outline %>% filter(state == state_select)
  }
  if (spatial_scale == "LGA") {
     single_highlight <- lga_outline %>% filter(state == state_select, lga == lga_select)
  }

  map <- leaflet()  |>
    addTiles() |>
    # Add LGA polygons with color based on intervention_summary
    addPolygons(
      data = intervention_mix_maps,
      fillColor = ~color_pal(intervention_mix_maps$intervention_summary),
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
        lga, state, intervention_summary
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
      values = intervention_mix_maps$intervention_summary,
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
    static_data <- filtered_data
    p <- ggplot() +
      geom_sf(data = lga_outline, fill = "grey90", col = "grey", alpha = 0.5) +
      geom_sf(data = static_data, aes(fill = intervention_type, col = intervention_type), alpha = 0.6) +
      geom_sf(data = state_outline, fill = NA, linewidth = 0.7, color = "black") +
      scale_fill_brewer(palette = "Spectral", direction = -1) +
      scale_color_brewer(palette = "Spectral", direction = -1) +
      theme_void(base_size = 14) +
      facet_wrap(vars(intervention)) +
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
      geom_sf(data = static_data, aes(fill = intervention_type, col = intervention_type), alpha = 0.6) +
      geom_sf(data = state_outline, fill = NA, linewidth = 0.7, color = "black") +
      geom_sf(data = state_highlight, fill = NA, linewidth = 1, color = "red") +
      scale_fill_brewer(palette = "Spectral", direction = -1) +
      scale_color_brewer(palette = "Spectral", direction = -1) +
      theme_void(base_size = 14) +
      facet_wrap(vars(intervention)) +
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
      geom_sf(data = static_data, aes(fill = intervention_type, col = intervention_type), alpha = 0.6) +
      geom_sf(data = state_outline, fill = NA, linewidth = 0.7, color = "black") +
      geom_sf(data = lga_highlight, fill = NA, linewidth = 1, color = "red") +
      scale_fill_brewer(palette = "Spectral", direction = -1) +
      scale_color_brewer(palette = "Spectral", direction = -1) +
      theme_void(base_size = 14) +
      facet_wrap(vars(intervention)) +
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
create_icon_summaries <- function(plan_select, year_select,
                                    spatial_scale,
                                    state_select,
                                    lga_select, currency_select) {

    # Select the data based on the spatial scale
    if (spatial_scale == "National") {
      data <- national_budget |>
        dplyr::filter(
          plan_shortname == plan_select,
          currency == currency_select
        )
    } else if (spatial_scale == "State") {
      data <- state_budget |>
        dplyr::filter(
          adm1 == state_select,
          plan_shortname == plan_select,
          currency == currency_select
        )
    } else if (spatial_scale == "LGA") {
      data <- lga_budget |>
        dplyr::filter(
          adm1 == state_select,
          adm2 == lga_select,
          plan_shortname == plan_select,
          currency == currency_select
        )
    } else {
      data <- NULL
    }

    # Filter by year or aggregate over all years
    if (year_select == "All Years") {
      data <- data |>
        dplyr::select(pop_total, pop_0_5, pop_pw,
                      total_budget, total_budget_per_person) |>
        dplyr::summarise(
          pop_total = mean(pop_total, na.rm = TRUE),
          pop_0_5 = mean(pop_0_5, na.rm = TRUE),
          pop_pw = mean(pop_pw, na.rm = TRUE),
          total_budget = sum(total_budget, na.rm = TRUE),
          total_budget_per_person = sum(total_budget_per_person, na.rm = TRUE)
        )
    } else {
      data <- data |>
        dplyr::filter(year == year_select)
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
          h3(formatC(data$pop_total, format = "d", big.mark = ","), style = "font-weight: bold;"),
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
process_budget_data <- function(plan_select, year_select,
                                spatial_scale,
                                state_select,
                                lga_select,
                                currency_select) {

  # 1) Select the data based on the input selections
  data <- if (spatial_scale == "National") {
    national_budget |>
      dplyr::filter(
        plan_shortname == plan_select,
        currency == currency_select
      )
  } else if (spatial_scale == "State") {
    state_budget |>
      dplyr::filter(
        adm1 == state_select,
        plan_shortname == plan_select,
        currency == currency_select
      )
  } else if (spatial_scale == "LGA") {
    lga_budget |>
      dplyr::filter(
        adm1 == state_select,
        adm2 == lga_select,
        plan_shortname == plan_select,
        currency == currency_select
      )
  } else {
    return(NULL)
  }

  # 2) Select relevant columns
  data <- data |>
    dplyr::select(
      dplyr::any_of(c(
        "adm1", "adm2", "plan_shortname", "plan_description",
        "year", "currency"
      )),
      dplyr::starts_with("code"),
      dplyr::contains("total_cost")
    )

  # 3) Pivot longer to get columns in the desired format
  data <- data |>
    tidyr::pivot_longer(
      cols = tidyselect::matches("^code_|_total_cost$"),
      names_to = "temp",
      values_to = "value"
    ) |>
    dplyr::mutate(
      intervention = dplyr::case_when(
        stringr::str_starts(temp, "code_") ~ stringr::str_remove(temp, "^code_"),
        stringr::str_ends(temp, "_total_cost") ~ stringr::str_remove(temp, "_total_cost")
      ),
      measure = dplyr::case_when(
        stringr::str_starts(temp, "code_") ~ "LGAs_targeted",
        stringr::str_ends(temp, "_total_cost") ~ "total_cost"
      )
    ) |>
    dplyr::select(-temp) |>
    tidyr::pivot_wider(
      names_from = measure,
      values_from = value
    )

  # 4) Rename interventions to friendly names
  data <- data |>
    dplyr::mutate(
      title = dplyr::case_when(
        intervention == "cb" ~ "Capacity building",
        intervention == "cm_private" ~ "Case Management - Private Sector",
        intervention == "cm_public" ~ "Case Management - Public Sector",
        intervention == "ento_surveillance" ~ "Entomological Surveillance",
        intervention == "gc" ~ "Governance & Coordination",
        intervention == "gf_wd" ~ "Global Fund Warehouse & Distribution",
        intervention == "iptp" ~ "IPTp",
        intervention == "irs" ~ "IRS",
        intervention == "itn_campaign" ~ "ITN Campaign",
        intervention == "itn_routine" ~ "ITN Routine",
        intervention == "lsm" ~ "LSM",
        intervention == "me" ~ "Monitoring & Evaluation",
        intervention == "pmc" ~ "PMC",
        intervention == "rm" ~ "Resource Mobilization",
        intervention == "smc" ~ "SMC",
        intervention == "ss_sbc" ~ "Social Behaviour Change",
        intervention == "vacc" ~ "Vaccine",
        TRUE ~ intervention
      )
    )

  # 5) Handle "All Years" vs single year
  if (year_select == "All Years") {
    data <- data |>
      dplyr::group_by(
        plan_shortname,
        plan_description,
        currency,
        intervention,
        title
      ) |>
      dplyr::summarise(
        total_cost = sum(total_cost, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        item_class = dplyr::case_when(
          intervention %in% c(
            "cb", "ento_surveillance", "gc", "gf_wd",
            "me", "rm", "ss_sbc"
          ) ~ "Support Services",
          TRUE ~ "Malaria Intervention"
        )
      )
  } else {
    data <- data |>
      dplyr::filter(year == year_select) |>
      dplyr::mutate(
        item_class = dplyr::case_when(
          intervention %in% c(
            "cb", "ento_surveillance", "gc", "gf_wd",
            "me", "rm", "ss_sbc"
          ) ~ "Support Services",
          TRUE ~ "Malaria Intervention"
        )
      )
  }

  # 6) Final dataset with custom column order
  data <- data |>
    dplyr::select(
      plan_shortname,
      item_class,
      title,
      total_cost
    )

  return(data)
}

#-Process individual item data------------------------------------------------
process_item_data <- function(plan_select, year_select,
                                spatial_scale,
                                state_select,
                                lga_select,
                                currency_select) {

  # 1) Select the data based on the input selections
  data <-  if (spatial_scale == "National") {
    national_budget |>
      dplyr::filter(
        plan_shortname == plan_select,
        currency == currency_select
      ) |>
    rename(ss_sbc_cost = ss_sbc_total_cost,
           rm_cost = rm_total_cost)
  } else if (spatial_scale == "State") {
    state_budget |>
      dplyr::filter(
        adm1 == state_select,
        plan_shortname == plan_select,
        currency == currency_select
      )
  } else if (spatial_scale == "LGA") {
    lga_budget |>
      dplyr::filter(
        adm1 == state_select,
        adm2 == lga_select,
        plan_shortname == plan_select,
        currency == currency_select
      )
  } else {
    return(NULL)
  }

  # 2) Select relevant columns
  data <-
    data |>
    rename(cm_private_cost = cm_private_total_cost) |>
    dplyr::select(
      -dplyr::starts_with("code"),
      -dplyr::contains("total_cost"),
      -dplyr::contains("pop"),
      -dplyr::contains("quant"),
      -dplyr::contains("landmass"),
      -dplyr::contains("total"),
      dplyr::any_of(c(
        "adm1", "adm2", "plan_shortname", "plan_description",
        "year", "currency"
      ))

    )

  # 3) Pivot longer to get columns in the desired format
  data <-
    data |>
    tidyr::pivot_longer(
      cols = contains("cost"),
      names_to = "line_item",
      values_to = "cost"
    ) |>
    mutate(
      category = case_when(
        grepl("procurement", line_item) ~ "Procurement",
        grepl("distribution|campaign|operational|eqa|storage", line_item) ~ "Implementation",
        TRUE ~ "Support"
      )
    )


  # 4) Rename interventions to friendly names
  data <-
    data |>
    dplyr::mutate(
      title = dplyr::case_when(
        stringr::str_starts(line_item, "cb") ~ "Capacity building",
        stringr::str_starts(line_item, "cm_private") ~ "Case Management - Private Sector",
        stringr::str_starts(line_item, "cm_") ~ "Case Management - Public Sector",
        stringr::str_starts(line_item, "ento_surveillance") ~ "Entomological Surveillance",
        stringr::str_starts(line_item, "gc") ~ "Governance & Coordination",
        stringr::str_starts(line_item, "gf_wd") ~ "Global Fund Warehouse & Distribution",
        stringr::str_starts(line_item, "iptp") ~ "IPTp",
        stringr::str_starts(line_item, "irs") ~ "IRS",
        stringr::str_starts(line_item, "itn_campaign") ~ "ITN Campaign",
        stringr::str_starts(line_item, "itn_routine") ~ "ITN Routine",
        stringr::str_starts(line_item, "lsm") ~ "LSM",
        stringr::str_starts(line_item, "me") ~ "Monitoring & Evaluation",
        stringr::str_starts(line_item, "pmc") ~ "PMC",
        stringr::str_starts(line_item, "rm") ~ "Resource Mobilization",
        stringr::str_starts(line_item, "smc") ~ "SMC",
        stringr::str_starts(line_item, "ss_sbc") ~ "Social Behaviour Change",
        stringr::str_starts(line_item, "vacc") ~ "Vaccine",
        TRUE ~ line_item
      )
    )

  # 5) Handle "All Years" vs single year
  if (year_select == "All Years") {
    data <- data |>
      dplyr::group_by(
        plan_shortname,
        plan_description,
        currency,
        line_item,
        title,
        category
      ) |>
      dplyr::summarise(
        cost = sum(cost, na.rm = TRUE),
        .groups = "drop"
      )
  } else {
    data <- data |>
      dplyr::filter(year == year_select)
  }

  return(data)
}


#-BUDGET TABLE WITH FORMATTING----------------------------------------------------------
create_budget_table <- function(processed_data, currency_select, baseline_data = NULL) {
  # Define display column names.
  col_names <- c(
    "Plan" = "plan_shortname",
    "Category" = "item_class",
    "Budget Item" = "title",
    "Total Cost" = "total_cost"
  )

  # Determine currency symbol.
  currency_symbol <- if (currency_select == "USD") "$" else "₦"

  # If baseline data is provided, join and compute a flag for differences.
  if (!is.null(baseline_data)) {
    # Use common keys for joining. Adjust these keys as necessary.
    baseline_lookup <- baseline_data %>%
      dplyr::select(item_class, title, total_cost) %>%
      dplyr::rename(baseline_total = total_cost)

    processed_data <- processed_data %>%
      dplyr::left_join(baseline_lookup, by = c("item_class", "title")) %>%
      dplyr::mutate(diff_flag = total_cost != baseline_total)
  }

  # Identify columns to hide
  hidden_cols <- which(names(processed_data) %in% c("baseline_total", "diff_flag")) - 1

  # Build the datatable.
  dt <- DT::datatable(
    processed_data,
    options = list(
      pageLength = 20,
      scrollX = TRUE,
      columnDefs = list(
        list(targets = hidden_cols, visible = FALSE)  # Hide specified columns
      )
    ),
    rownames = FALSE,
    colnames = col_names
  ) %>%
    DT::formatStyle(
      columns = 1:ncol(processed_data),
      fontSize = '14px'
    ) %>%
    DT::formatCurrency(
      columns = "Total Cost",
      currency = currency_symbol,
      interval = 3,
      mark = ",",
      digits = 0
    )

  # If baseline data was provided, highlight rows flagged as different.
  if (!is.null(baseline_data)) {
    dt <- dt %>% DT::formatStyle(
      'diff_flag',
      target = 'row',
      backgroundColor = DT::styleEqual(c(TRUE), c('lightgreen'))
    )
  }

  dt
}

#-DONUT CHART-------------------------------------------------------------
donut_plot <- function(data) {
  billboarder() |>
    bb_donutchart(
      data = data |>
        arrange(desc(total_cost)) |>
        select(title, total_cost)
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
    labels = ~title,
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
    group_by(title, category) |>
    summarise(cost = sum(cost), .groups = 'drop') |>
    group_by(title) |>
    mutate(total_intervention_cost = sum(cost)) |>
    ungroup() |>
    mutate(title = reorder(title, -total_intervention_cost))

  plot_ly(data = proc_impl_split,
          colors = c("#181D31", "#3779E5", "#81CF98", "#DADEE2", "#4E5E77")) |>
    add_bars(
      x = ~title,
      y = ~cost,
      color = ~category,
      text = ~scales::dollar(cost, prefix = currency_symbol),
      hoverinfo = "text"
    )  |>
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
        x = 0.8,       # Position relative to plot width (0 = left, 1 = right)
        y = 0.95,      # Position relative to plot height (0 = bottom, 1 = top)
        xanchor = "right",  # Anchor point for the legend's x position
        yanchor = "top",    # Anchor point for the legend's y position
        font = list(size = 12),
        bgcolor = 'rgba(255,255,255,0.5)'  # Optional: semi-transparent background
      ),
      font = list(size = 14)
    )
}

#-lolipop plot for specific elements---------------------------------------------
lolipop_plot <- function(data, currency_select){

  currency_symbol <- if(currency_select == "USD") "$" else "₦"

  # Get top costs and include intervention information
  top_costs <-
    data  |>
    arrange(desc(cost))  |>
    head(15) |>
    mutate(
      line_item = str_replace_all(line_item, "_", " "),
      line_item = str_replace(line_item, "cm " , "CM "),
      line_item = str_replace(line_item, "itn " , "ITN "),
      line_item = str_replace(line_item, "lsm " , "LSM "),
      line_item = str_replace(line_item, "vacc " , "Vaccine "),
      line_item = str_replace(line_item, "act " , "ACT "),
      line_item = str_replace(line_item, "smc " , "SMC "),
      line_item = str_replace(line_item, "rdt " , "RDT "),
      line_item = str_replace(line_item, "pmc " , "PMC "),
      line_item = str_replace(line_item, "spaq " , "SPAQ "),
      line_item = str_replace(line_item, "sp " , "SP "),
      line_item = str_replace(line_item, "iptp " , "IPTp ")
        )


  to_bold <- function(text) {
    # Split the text into words
    words <- strsplit(text, " ", fixed = TRUE)[[1]]
    if (length(words) == 0) return(text)

    # Helper function to convert a single character
    bold_letter <- function(ch) {
      # Convert uppercase letters
      if (grepl("[A-Z]", ch)) {
        return(intToUtf8(utf8ToInt(ch) - utf8ToInt("A") + 0x1D400))
      } else if (grepl("[a-z]", ch)) {
        return(intToUtf8(utf8ToInt(ch) - utf8ToInt("a") + 0x1D41A))
      } else {
        return(ch)
      }
    }

    # Convert the first word to bold
    bold_first <- paste(sapply(strsplit(words[1], "")[[1]], bold_letter), collapse = "")
    words[1] <- bold_first
    paste(words, collapse = " ")
  }


  # highlight intervention bits in bold
  unique_labels <- unique(top_costs$line_item)
  modified_labels <- sapply(unique_labels, to_bold)


  plot_ly(colors = c("#181D31", "#3779E5", "#81CF98", "#DADEE2", "#4E5E77"))  |>
    add_segments(
      data = top_costs,
      x = 0,
      xend = ~cost,
      y = ~line_item,
      yend = ~line_item,
      line = list(color = "gray"),
      showlegend = FALSE
    ) %>%
    add_markers(
      data = top_costs,
      x = ~cost,
      y = ~line_item,
      color = ~category,
      colors = "Set1",
      marker = list(size = 12),
      text = ~paste0(
        title, "<br>",
        "Cost: ", scales::dollar(cost, prefix = currency_symbol)
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
        tickvals = unique_labels,
        ticktext = modified_labels,
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
                          plan_select,
                          currency_select,
                          map_type,
                          year_select,
                          state_select = NULL,
                          lga_select = NULL) {
  # Format dataset based on map_level
  if(map_level == "State"){
    data <-
      state_budget |>
      dplyr::select(plan_shortname, adm0, adm1, total_budget, currency, year, total_budget_per_person) |>
      dplyr::filter(currency == currency_select,
                    plan_shortname == plan_select)
  } else if(map_level == "LGA"){
    data <-
      lga_budget |>
      dplyr::select(plan_shortname, adm0, adm1, adm2, currency, year, total_budget, total_budget_per_person) |>
      dplyr::filter(currency == currency_select,
                    plan_shortname == plan_select)
  }

  # Account for years selected
  if(year_select == 'All Years'){
    # Define the default grouping columns
    group_cols <- c("plan_shortname", "adm0", "adm1")

    # If adm2 exists in the dataset, add it to the grouping
    if("adm2" %in% names(data)){
      group_cols <- c(group_cols, "adm2")
    }

    data <- data |>
      dplyr::group_by(across(all_of(group_cols))) |>
      dplyr::summarise(
        total_budget = sum(total_budget, na.rm = TRUE),
        total_budget_per_person = sum(total_budget_per_person, na.rm = TRUE),
        .groups = "drop"
      )
  }  else {
    data <-
      data |>
      dplyr::filter(year == year_select)
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
      hover_text = paste0("Plan: ", plan_shortname, "<br>",
                          "Total Cost: ", currency_symbol, format(budget_millions, big.mark = ","), "M")
    )

  # Generate palette safely
  palette_fun <- ggthemes::canva_pal("Fun and tropical")
  colors <- palette_fun(length(unique(data$plan_shortname)))

  p <- ggplot(data, aes(x = plan_shortname, y = budget_millions, fill = plan_shortname, text = hover_text)) +
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
    layout(hoverlabel = list(bgcolor = "white"))
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
generate_final_cost_plot <- function(currency_select, year_select, spatial_scale = "National",
                                     baseline_plan, comp_plans) {


  # all plans
  plan_select <- c(baseline_plan, comp_plans)


  # Determine currency symbol
  currency_symbol <- if (currency_select == "USD") "$" else "₦"

  # Process data for selected plans using the same function as the tables
  dat <- purrr::map_df(plan_select, function(plan) {
    process_budget_data(
      plan_select = plan,
      year_select = year_select,
      spatial_scale = spatial_scale,
      state_select = NULL,
      lga_select = NULL,
      currency_select = currency_select
    ) %>%
      mutate(plan = plan)  # Ensure plan name is included
  })

  # Convert to millions for consistency with tables
  dat <- dat %>%
    mutate(
      # title = factor(title, levels = rev(title)),  # Order interventions
      # plan = str_to_title(plan),  # Capitalize plan names
      total_cost = total_cost / 1e6,  # Convert to millions
      tc_print = case_when(
        currency_select == "NGN"  ~ paste0(currency_symbol, format(round(total_cost, 0), big.mark = ","), "m"),
        currency_select == "USD" & total_cost > 60 ~ paste0(currency_symbol, format(round(total_cost, 0), big.mark = ","), "m"),
        currency_select == "USD" & total_cost <= 60 & total_cost > 1 ~ paste0(currency_symbol, format(round(total_cost, 1), big.mark = ","), "m"),
        currency_select == "USD" & total_cost <= 1 ~ paste0(currency_symbol, format(round(total_cost, 2), big.mark = ","), "m"),
        is.na(total_cost) ~ paste0(currency_symbol, "0m")
      ),
      total_cost = ifelse(is.na(total_cost), 0, total_cost)
    )

  # Extract baseline data (Scenario 1) for comparison
  baseline_data <- dat %>%
    filter(plan == baseline_plan) %>%
    select(title, total_cost) %>%
    rename(baseline_total = total_cost)

  # Merge baseline comparison data
  dat <- dat %>%
    left_join(baseline_data, by = "title") %>%
    mutate(is_different = total_cost != baseline_total)

  # Generate ggplot
  ggplot(dat) +
    geom_col(aes(x = total_cost, y = title, fill = plan)) +
    geom_col(data = dat %>% filter(is_different, plan != baseline_plan),
             aes(x = total_cost, y = title),
             fill = NA,
             color = "lightgreen",
             size = 2) +
    facet_wrap(vars(plan), ncol = 4, scales = "fixed") +
    scale_fill_manual(values = ggthemes::canva_pal("Fun and tropical")(length(unique(dat$plan_shortname))))+
    labs(
      y = "",
      x = paste0("Total Cost (in ", currency_symbol, " Million)")
    ) +
    geom_text(aes(x = total_cost + 10, y = title, label = tc_print),
              size = 6, hjust = 0) +
    theme_bw(16) +
    theme(
      strip.text = element_text(size = 14, face = "bold"),
      axis.text.y = element_text(size = 14),
      axis.text.x = element_text(size = 14),
      legend.position = "none"
    )
}

# Scenario check functions ----------

# Function to count LGAs receiving and not receiving specific intervention by state and year
count_lga_coverage <- function(intervention, plan, year_filter = NULL) {
  # Get LGAs receiving intervention
  receiving <- static_mix_maps |>
    st_drop_geometry() |>
    filter(intervention == !!intervention,
           plan_shortname == !!plan)

  # Apply year filter if provided
  if (!is.null(year_filter)) {
    receiving <- receiving |>
      filter(year %in% year_filter)
  }

  receiving <- receiving |>
    distinct(year, state, lga)

  # Get total LGAs per state
  total_lgas <- static_mix_maps |>
    filter(plan_shortname == !!plan) |>
    st_drop_geometry()

  # Apply year filter if provided
  if (!is.null(year_filter)) {
    total_lgas <- total_lgas |>
      filter(year %in% year_filter)
  }

  total_lgas <- total_lgas |>
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

smc_pmc_check <- function(plan, year_filter = NULL) {
  # Which LGAs are receiving PMC each year
  pmc_tmp <- static_mix_maps |>
    st_drop_geometry() |>
    filter(plan_shortname == !!plan,
           intervention == "PMC")

  # Apply year filter if provided
  if (!is.null(year_filter) && length(year_filter) > 0 && !all(is.na(year_filter))) {
    pmc_tmp <- pmc_tmp |>
      filter(year %in% year_filter)
  }

  pmc_tmp <- pmc_tmp |>
    distinct(year, state, lga, intervention)

  # Which LGAs are receiving SMC each year
  smc_tmp <- static_mix_maps |>
    st_drop_geometry() |>
    filter(plan_shortname == !!plan,
           intervention == "SMC")

  # Apply year filter if provided
  if (!is.null(year_filter) && length(year_filter) > 0 && !all(is.na(year_filter))) {
    smc_tmp <- smc_tmp |>
      filter(year %in% year_filter)
  }

  smc_tmp <- smc_tmp |>
    distinct(year, state, lga, intervention)

  # Complete join by year, state, and lga
  smc_pmc <- inner_join(pmc_tmp, smc_tmp, by = c("year", "state", "lga")) |>
    select(year, state, lga)

  # If none return message within "None.", otherwise return all SMC and PMC data
  if (nrow(smc_pmc) == 0) {
    NULL
  } else {
    smc_pmc
  }
}
