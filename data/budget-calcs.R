
#-------------------------------------------------------------------------------
# Generate budget function
#


scen_data <- read_excel("uploads/scenarios/Plan_1___BAU.xlsx",
                         sheet = "2026") |>
  mutate(year = 2026,
         scenario_name = "Plan 1 BAU",
         scenario_description = "BAU old scenario")

cost_data <- read_excel("uploads/costs/Cost_1.xlsx") |>
  mutate(cost_name = "Cost 1",
         cost_description = "Historic") |> select(-cost_year, -notes, -source)


generate_budget(scen_data, cost_data)

generate_budget <- function(scen_data, cost_data){

  #-SUMMARY------------------------------------------------------------------------------
  # Print a summary of the interventions and number of states/LGAs being targeted
  cat("Costing scenario being generated for the following mix of interventions:")
  print(
    scen_data |>
      select(adm1, adm2, year, starts_with("code_")) |>
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
    rename(
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
      cost_element = quantity * unit_cost
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
      ) |>
    left_join(target_population |>
                select(adm1, adm2, pop_total),
              by = c("adm1", "adm2"))


 return(budget)

}




#   #-MAKE intervention mix map------------------------------------------------------
#   mix_map <-
#     scen_data |>
#     crossing(year = c(2025, 2026, 2027)) |>
#     select(-code_itn_urban) |>
#     # Pivot the data from wide to long format
#     pivot_longer(cols = starts_with("code_"),
#                  names_to = "intervention",
#                  values_to = "value")  |>
#     select(-starts_with("type_")) |>
#     # Filter to keep only rows where the intervention is set to 1
#     filter(value == 1)  |>
#     mutate(intervention = str_remove(intervention, "code_")) |>
#     # create a new column for nicely coded intervention names
#     mutate(
#       intervention_mix_to_show = str_to_upper(intervention)) |>
#     mutate(
#       intervention_mix_to_show = case_when(
#         intervention == "cm_public" ~ "CM",
#         intervention == "iptp" ~ "IPTp",
#         intervention == "vacc" ~ "Vaccine",
#         intervention == "itn_routine" ~ "ITN Routine",
#
#         intervention == "itn_campaign" ~ "ITN Campaign",
#
#         TRUE ~ intervention_mix_to_show)
#     ) |>
#     group_by(adm1, adm2, scenario_name, scenario_description, year)  |>
#     # Concatenate interventions with "+" separator
#     mutate(intervention_summary = paste(intervention_mix_to_show, collapse = " + ")) |>
#     # remove case management private from the mix
#     mutate(intervention_summary =str_remove_all(intervention_summary, "\\s*\\+ CM_PRIVATE$")) |>
#     mutate(intervention_summary =str_remove_all(intervention_summary, "CM_PRIVATE\\s*\\+\\s*")) |>
#     # reduce down
#     select(-intervention, -value, -intervention_mix_to_show) |>
#     distinct()
#
#   mix_shp <-
#     sf::st_read("data/nga-demo-data-pre-processed/shapefiles/lga_shapefile_simp.shp") |>
#     left_join(mix_map, by = c("state" = "adm1",
#                               "lga" = "adm2"))
#
#   static_plot_mix_map <-
#     scen_data |>
#     crossing(year = c(2025, 2026, 2027)) |>
#     select(-code_itn_urban) |>
#     pivot_longer(
#       cols = starts_with("code_"),  # Selects all 'code_*' columns
#       names_to = "intervention",  # New column for intervention names
#       values_to = "code"  # Values from 'code_*' columns
#     ) %>%
#     filter(code == 1) %>%  # Keep only rows where code == 1
#     mutate(
#       intervention_type = case_when(
#         intervention == "code_cm_public" ~ NA,
#         intervention == "code_iptp" ~ type_iptp,
#         intervention == "code_smc" ~ type_smc,
#         intervention == "code_pmc" ~ type_pmc,
#         intervention == "code_vacc" ~ type_vacc,
#         intervention == "code_irs" ~ type_irs,
#         intervention == "code_itn_campaign" ~ type_itn_campaign,
#         intervention == "code_itn_routine" ~ type_itn_routine,
#         intervention == "code_lsm" ~ type_lsm,
#         TRUE ~ NA_character_  # Assigns NA if no matching type column exists
#       )
#     ) %>%
#     mutate(intervention = str_remove(intervention, "code_")) |>
#     # create a new column for nicely coded intervention names
#     mutate(
#       intervention = case_when(
#         intervention == "cm_public" ~ "Case Management Public",
#         intervention == "cm_private" ~ "Case Management Private",
#         intervention == "iptp" ~ "IPTp",
#         intervention == "vacc" ~ "Vaccine",
#         intervention == "itn_routine" ~ "ITN Routine",
#         intervention == "itn_campaign" ~ "ITN Campaign",
#         intervention == "smc" ~ "SMC",
#         intervention == "pmc" ~ "PMC",
#         intervention == "irs" ~ "IRS",
#         intervention == "lsm" ~ "LSM",
#         TRUE ~ intervention)
#     ) |>
#     select(adm0, adm1, adm2, scenario_name, scenario_description, intervention, intervention_type, year)
#
#   static_shp <-
#     sf::st_read("data/nga-demo-data-pre-processed/shapefiles/lga_shapefile_simp.shp") |>
#     left_join(static_plot_mix_map, by = c("state" = "adm1",
#                                           "lga" = "adm2"))
#
#
#   #-FORMAT AND SAVE DATA------------------------------------------------------------
#
#   # save intervention mix map
#   sf::st_write(mix_shp,
#                paste0("data/nga-demo-data-pre-processed/shapefiles/",
#                       data$scenario_name[1],
#                       "-interactive-map.shp"),
#                delete_dsn = TRUE)  # Overwrites existing file
#
#   sf::st_write(static_shp,
#                paste0("data/nga-demo-data-pre-processed/shapefiles/",
#                       data$scenario_name[1],
#                       "-static-map.shp"),
#                delete_dsn = TRUE)  # Overwrites existing file
#
#   # save raw budgets into single sheet
#
#   # Create a new workbook
#   wb <- openxlsx::createWorkbook()
#
#   # Add each dataframe as a sheet
#   openxlsx::addWorksheet(wb, "LGA")
#   openxlsx::writeData(wb, "LGA", lga_mix)
#
#   openxlsx::addWorksheet(wb, "State")
#   openxlsx::writeData(wb, "State", state_mix)
#
#   openxlsx::addWorksheet(wb, "National")
#   openxlsx::writeData(wb, "National", national_mix)
#
#   # Save workbook
#   openxlsx::saveWorkbook(wb, paste0("data/nga-demo-data-pre-processed/budgets-generated/"
#                                     ,data$scenario_name[1], "-budgets.xlsx"), overwrite = TRUE)
#
#
# }
#
#
