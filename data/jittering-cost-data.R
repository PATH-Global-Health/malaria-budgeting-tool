library(dplyr)

# 1. Load real data (but in a secure location; not checked into public repo):
df_real <- readxl::read_xlsx("uploads/costs/Cost_1.xlsx")

# 2. Perturb:
set.seed(2025)
generate_jitter_factor <- function(n, log_sd = 0.2) exp(rnorm(n, 0, log_sd))
demo_exch_rate <- 1000

df_demo <- df_real %>%
  rowwise() %>%
  mutate(
    log_sd = case_when(
      cost_class == "Procurement" ~ 0.15,
      cost_class == "Distribution" ~ 0.20,
      cost_class == "Operational"  ~ 0.25,
      cost_class == "Support"      ~ 0.30,
      TRUE                         ~ 0.20
    ),
    jitter = generate_jitter_factor(1, log_sd),
    usd_cost_demo = round(usd_cost * jitter, 2),
    ngn_cost_demo = round((usd_cost_demo * demo_exch_rate) / 100) * 100,
    cost_year = 2026,
    notes = paste0(coalesce(notes, ""), " [Demo: values perturbed ±", round((jitter-1)*100, 1), "%]")
  ) %>%
  ungroup() %>%
  select(-log_sd, -jitter)

# 3. Write out to edit
readr::write_csv(df_demo, "data/demo_costs1.csv")

# second cost data set
