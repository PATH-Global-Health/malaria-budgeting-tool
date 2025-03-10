#-------------------------------------------------------------------------------
# Global script for reading in data for the demo version of the tool
#
#-------------------------------------------------------------------------------
library(DT)
library(RSQLite)
library(digest)
library(readxl)
library(openxlsx)
library(tidyverse)
library(viridisLite)
library(glue)
library(billboarder)
library(treemap)
library(RColorBrewer)
library(htmlwidgets)
library(readxl)
library(leaflet)
library(leaflet.extras2)
library(sf)
library(DT)
library(plotly)
library(billboarder)
library(ggthemes)
library(shinyjs)
library(writexl)
library(webshot2)
library(htmlwidgets)

#-read in usable data-----------------------------------------------------------

# Read template file and store column names and admin data
template_file_path <- "www/scenario-template.xlsx"
cost_template_file_path <- "www/cost-template.xlsx"

SCENARIO_COLS <- colnames(read_excel(template_file_path))
COST_COLS <- colnames(read_excel(cost_template_file_path))

# Function to get admin data from template
get_template_admin_data <- function() {
  template_data <- read_excel(template_file_path)
  admin_cols <- grep("^adm", colnames(template_data), value = TRUE)
  template_data[admin_cols]
}

# Store template admin data
TEMPLATE_ADMIN_DATA <- get_template_admin_data()

# Default years range (2020-2030)
DEFAULT_YEARS <- as.character(2025:2032)

# Function to sync database with actual files
sync_database <- function(type = "scenario") {
  # Set paths and create directories if they don't exist
  upload_dir <- file.path("uploads", paste0(type, "s"))
  dir.create(upload_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Database file path
  db_file <- paste0(type, "_uploads.db")
  
  # Connect to database
  db <- dbConnect(SQLite(), db_file)
  
  # Create table if it doesn't exist
  if(type == "scenario") {
    dbExecute(db, "
      CREATE TABLE IF NOT EXISTS uploads (
        id TEXT PRIMARY KEY,
        name TEXT,
        description TEXT,
        filename TEXT,
        file_hash TEXT,
        years TEXT,
        upload_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP
      )
    ")
  } else {
    dbExecute(db, "
      CREATE TABLE IF NOT EXISTS uploads (
        id TEXT PRIMARY KEY,
        name TEXT,
        description TEXT,
        filename TEXT,
        file_hash TEXT,
        upload_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP
      )
    ")
  }
  
  # Get list of actual files
  existing_files <- list.files(upload_dir, pattern = "\\.xlsx$")
  
  # Get database records
  db_files <- dbGetQuery(db, "SELECT filename FROM uploads")$filename
  
  # Remove records for files that no longer exist
  missing_files <- setdiff(db_files, existing_files)
  if(length(missing_files) > 0) {
    placeholders <- paste(rep("?", length(missing_files)), collapse = ",")
    dbExecute(db, sprintf("DELETE FROM uploads WHERE filename IN (%s)", placeholders), 
             missing_files)
  }
  
  dbDisconnect(db)
}

# Add this to your global.R or run it once to set up the database
if (!file.exists("scenario_uploads.db")) {
       db <- dbConnect(SQLite(), "scenario_uploads.db")
       dbExecute(db, "
         CREATE TABLE uploads (
           id TEXT PRIMARY KEY,
           name TEXT,
           description TEXT,
           filename TEXT,
           file_hash TEXT,
           years TEXT,
           upload_date DATETIME DEFAULT CURRENT_TIMESTAMP
         )
       ")
       dbDisconnect(db)
     }

# Create necessary directories
dir.create("uploads", showWarnings = FALSE)
dir.create("uploads/scenarios", showWarnings = FALSE)
dir.create("uploads/costs", showWarnings = FALSE)
dir.create("www", showWarnings = FALSE)

# Sync databases on app startup
sync_database("scenario")
sync_database("cost")

# intervention mix maps
intervention_mix_maps <-
  st_read("data/nga-demo-data-pre-processed/shapefiles/combined_interactive_map.shp") |>
  select(-year) |>
  distinct() |>
  rename(intervention_summary = intrvn_,
         plan_shortname = pln_shr,
         plan_description = pln_dsc)

# static intervention maps
static_mix_maps <-
  st_read("data/nga-demo-data-pre-processed/shapefiles/combined_static_map.shp") |>
  rename(intervention = intrvnt,
         intervention_type = intrvn_,
         plan_shortname = pln_shr,
         plan_description = pln_dsc) |>
  mutate(intervention_type =
           case_when(is.na(intervention_type) ~ "NA" ,
                     TRUE ~ intervention_type))

# Shape files
country_outline <- sf::st_read("data/nga-demo-data-pre-processed/shapefiles/country_shapefile.shp")
state_outline   <- sf::st_read("data/nga-demo-data-pre-processed/shapefiles/state_shapefile_simp.shp")
lga_outline     <- sf::st_read("data/nga-demo-data-pre-processed/shapefiles/lga_shapefile_simp.shp")

state_outline$state[which(state_outline$state == "Akwa-Ibom")] <- "Akwa Ibom"

# plan budget data
national_budget <- read_xlsx("data/nga-demo-data-pre-processed/budgets-generated/combined-plan-budgets.xlsx",
                             sheet = "National") |>
  rename(irs_total_cost = irs_campaign_total_cost,
         cm_private_total_cost = cm_private) |>
  rename(gf_wd_malaria_long_haul_distribution_cost = gf_wd_malaria_long_haul_distribution,
         gf_wd_malaria_warehouse_axial_cost = gf_wd_malaria_warehouse_axial,
         gf_wd_malaria_warehouse_wib_cost = gf_wd_malaria_warehouse_wib)

state_budget <- read_xlsx("data/nga-demo-data-pre-processed/budgets-generated/combined-plan-budgets.xlsx",
                          sheet = "State")|>
  rename(irs_total_cost = irs_campaign_total_cost,
         cm_private_total_cost = cm_private)

lga_budget <- read_xlsx("data/nga-demo-data-pre-processed/budgets-generated/combined-plan-budgets.xlsx",
                          sheet = "LGA")|>
  rename(irs_total_cost = irs_campaign_total_cost,
         cm_private_total_cost = cm_private)

# Extract unique plans and their descriptions, excluding 'baseline'
unique_plans <- unique(national_budget$plan_shortname)
plan_descriptions <- unique(national_budget$plan_description)

# Combine plan and plan_description for checkbox labels
plan_labels <- paste(unique_plans, "-", plan_descriptions)

# values to use as the year input selection values
years_to_select <- unique(national_budget$year)
plans_to_select <- plan_labels

# Make a copy of "Plan A" called this "Plan Z", then add SMC to Aba North LGA in Abia State in 2025. 
plan_z <- static_mix_maps |> 
  filter(plan_shortname == "Plan A",) |> 
  mutate(
    plan_shortname = "Plan Z", 
    plan_description = "Test SMC/PMC warning", 
    plan_name = "Plan Z") |> 
  add_row(
    plan_shortname = "Plan Z",
    plan_description = "Test SMC/PMC warning", 
    year = 2025,
    state = "Abia",
    lga = "Aba North",
    intervention = "SMC")
#  Add plan Z to static mix maps
static_mix_maps <- bind_rows(static_mix_maps, plan_z)

# Extract unique plans and their descriptions, excluding 'baseline'
unique_plans2 <- unique(static_mix_maps$plan_shortname)

