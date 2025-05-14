tab2Server <- function(input, output, session, static_mix_maps) {
  ns <- session$ns

  # Check if static_mix_maps exists and contains data
  data_available <- reactive({
    exists("static_mix_maps", inherits = FALSE) &&
      !is.null(static_mix_maps) &&
      nrow(static_mix_maps) > 0
  })

  # Show message if no data is available
  observe({
    if (!data_available()) {
      output$intervention_tabs <- renderUI({
        card(
          card_header("No Data Uploaded"),
          card_body(
            tags$p("No data has been uploaded into the application."),
            tags$p("Please return to the 'User Input' tab and upload your intervention scenarios.")
          )
        )
      })
    }
  })

  # Get unique interventions from data
  interventions <- reactive({
    req(data_available(), input$plan_select)
    static_mix_maps %>%
      filter(plan_shortname == input$plan_select) %>%
      pull(intervention) %>%
      unique() %>%
      sort()
  })

  # Track the currently active intervention tab
  active_intervention <- reactiveVal()

  # Update active intervention when tab changes
  observeEvent(input$intervention_navset, {
    req(data_available())
    active_intervention(input$intervention_navset)
  }, ignoreInit = TRUE)

  # Set default active intervention when interventions list changes
  observeEvent(interventions(), {
    req(data_available())
    if(length(interventions()) > 0 && is.null(active_intervention())) {
      active_intervention(interventions()[1])
    }
  })

  # Create reactive filtered data based on user selections
  filtered_data <- reactive({
    req(data_available(), input$plan_select)

    year_filter <- if (input$year_select == "All Years" || input$year_select == "") {
      unique(static_mix_maps$year)
    } else {
      input$year_select
    }

    static_mix_maps %>%
      filter(
        plan_shortname == input$plan_select,
        year %in% year_filter
      )
  })

  # Check for SMC and PMC overlap
  smc_pmc_overlap <- reactive({
    req(data_available(), input$plan_select)

    year_filter <- if (input$year_select == "All Years" || input$year_select == "") {
      unique(static_mix_maps$year)
    } else {
      input$year_select
    }

    # Which LGAs are receiving PMC each year
    pmc_tmp <- static_mix_maps %>%
      st_drop_geometry() %>%
      filter(plan_shortname == input$plan_select,
             intervention == "PMC")

    # Apply year filter if provided
    if (!is.null(year_filter) && length(year_filter) > 0) {
      pmc_tmp <- pmc_tmp %>%
        filter(year %in% year_filter)
    }

    pmc_tmp <- pmc_tmp %>%
      distinct(year, state, lga, intervention)

    # Which LGAs are receiving SMC each year
    smc_tmp <- static_mix_maps %>%
      st_drop_geometry() %>%
      filter(plan_shortname == input$plan_select,
             intervention == "SMC")

    # Apply year filter if provided
    if (!is.null(year_filter) && length(year_filter) > 0) {
      smc_tmp <- smc_tmp %>%
        filter(year %in% year_filter)
    }

    smc_tmp <- smc_tmp %>%
      distinct(year, state, lga, intervention)

    # Complete join by year, state, and lga
    smc_pmc <- inner_join(pmc_tmp, smc_tmp, by = c("year", "state", "lga")) %>%
      select(year, state, lga)

    # If none, return NULL, otherwise return overlap data
    if (nrow(smc_pmc) == 0) {
      NULL
    } else {
      smc_pmc
    }
  })

  # Generate warning if SMC and PMC overlap
  output$smc_pmc_warning <- renderUI({
    req(data_available())
    overlap_data <- smc_pmc_overlap()

    if (!is.null(overlap_data) && nrow(overlap_data) > 0) {
      # Create a formatted table of the overlapping areas
      overlap_html <- overlap_data %>%
        arrange(year, state, lga) %>%
        mutate(
          display_text = paste0(year, ": ", state, " - ", lga)
        ) %>%
        pull(display_text) %>%
        paste(collapse = ", ")

      # Display warning message with the overlapping areas
      card(
        card_header(
          class = "bg-warning text-white",
          tags$div(
            class = "d-flex align-items-center",
            bsicons::bs_icon("exclamation-triangle-fill", size = "1.5rem", class = "me-2"),
            "Warning: PMC and SMC Overlap Detected"
          )
        ),
        card_body(
          tags$div(
            tags$p("The following LGAs have both PMC and SMC interventions scheduled for the same year. This may be unintended and should be reviewed:"),
            tags$div(style = "max-height: 150px; overflow-y: auto;",
                     tags$p(HTML(overlap_html))
            )
          )
        )
      )
    } else {
      # No overlap, don't display anything
      NULL
    }
  })

  # Generate tabs for each intervention
  output$intervention_tabs <- renderUI({
    req(data_available(), interventions())

    if(length(interventions()) == 0) {
      return(card(
        card_header("No Data"),
        "No interventions found for the selected plan. Please select a different plan."
      ))
    }

    # Get year filter once for all interventions
    year_filter <- if (input$year_select == "All Years" || input$year_select == "") {
      unique(static_mix_maps$year)
    } else {
      input$year_select
    }

    # Create the tab panels with value boxes embedded in each
    intervention_panels <- lapply(interventions(), function(intervention) {
      # Get coverage data for this specific intervention
      coverage_data <- count_lga_coverage(
        intervention = intervention,
        plan = input$plan_select,
        year_filter = year_filter
      )

      # Calculate states with full, partial, and no coverage for this specific intervention
      states_by_coverage <- coverage_data %>%
        group_by(State) %>%
        summarize(
          min_coverage = min(`Coverage %`),
          max_coverage = max(`Coverage %`),
          avg_coverage = mean(`Coverage %`)
        ) %>%
        mutate(
          coverage_status = case_when(
            min_coverage == 100 ~ "full",
            max_coverage == 0 ~ "none",
            TRUE ~ "partial"
          )
        )

      # Count states by coverage status
      summary_counts <- states_by_coverage %>%
        group_by(coverage_status) %>%
        summarise(count = n())

      # Convert to named list for easier access
      summary_list <- list(
        full_count = summary_counts %>% filter(coverage_status == "full") %>% pull(count) %>% as.integer(),
        partial_count = summary_counts %>% filter(coverage_status == "partial") %>% pull(count) %>% as.integer(),
        none_count = summary_counts %>% filter(coverage_status == "none") %>% pull(count) %>% as.integer()
      )

      # Handle NAs for states that don't exist in a category
      if(length(summary_list$full_count) == 0) summary_list$full_count <- 0
      if(length(summary_list$partial_count) == 0) summary_list$partial_count <- 0
      if(length(summary_list$none_count) == 0) summary_list$none_count <- 0

      # Create nav panel with value boxes specific to this intervention
      nav_panel(
        title = intervention,
        # Value boxes for this specific intervention
        layout_column_wrap(
          width = 1/3,
          value_box("States with Full Coverage", summary_list$full_count, showcase = bsicons::bs_icon("check-circle-fill"), theme = "success"),
          value_box("States with Partial Coverage", summary_list$partial_count, showcase = bsicons::bs_icon("dash-circle-fill"), theme = "warning"),
          value_box("States with No Coverage", summary_list$none_count, showcase = bsicons::bs_icon("x-circle-fill"), theme = "danger")
        ),
        DTOutput(ns(paste0("coverage_table_", make.names(intervention))))
      )
    })

    # Only create the card if we have intervention panels
    if (length(intervention_panels) > 0) {
      card(
        card_header(paste0("LGA Coverage for Plan: ", input$plan_select)),
        card_body(
          do.call(navset_card_tab, c(
            list(id = ns("intervention_navset")),
            intervention_panels
          ))
        )
      )
    } else {
      card(
        card_header("No Interventions"),
        card_body("No interventions found for the selected criteria.")
      )
    }
  })

  # Generate tables for each intervention
  observe({
    req(data_available(), interventions(), input$plan_select)

    for(intervention in interventions()) {
      local({
        local_intervention <- intervention
        output_id <- paste0("coverage_table_", make.names(local_intervention))

        output[[output_id]] <- renderDT({
          req(input$plan_select)

          # Get year filter
          year_filter <- if (input$year_select == "All Years" || input$year_select == "") {
            unique(static_mix_maps$year)
          } else {
            input$year_select
          }

          # Get the data from the count_lga_coverage function
          data_table <- count_lga_coverage(
            intervention = local_intervention,
            plan = input$plan_select,
            year_filter = year_filter
          )

          # Format the table using DT
          datatable(
            data_table,
            options = list(
              pageLength = 10,
              dom = 'lfrtip',
              lengthMenu = list(c(10, 25, 50, -1), c('10', '25', '50', 'All')),
              autoWidth = TRUE,
              scrollX = TRUE,
              searchHighlight = TRUE,
              search = list(regex = TRUE, caseInsensitive = TRUE)
            ),
            rownames = FALSE,
            filter = 'top',
            class = 'cell-border stripe'
          ) %>%
            formatStyle(
              'Coverage %',
              background = styleColorBar(
                range(0, 100),
                '#78c2ad'
              ),
              backgroundSize = '100% 90%',
              backgroundRepeat = 'no-repeat',
              backgroundPosition = 'center'
            ) %>%
            formatRound(
              c('Coverage %'),
              digits = 1
            )
        })
      })
    }
  })

  # Clear button logic
  observeEvent(input$clear_inputs, {
    updateSelectInput(session, "plan_select", selected = "")
    updateSelectInput(session, "year_select", selected = "")
  })
}
