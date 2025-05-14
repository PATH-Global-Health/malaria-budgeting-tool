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
library(purrr)
# library(openxlsx2)

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

# Define the folder path
scenario_folder <- "uploads/scenarios"

# Get all .xlsx file paths
scenario_files <- list.files(scenario_folder, pattern = "\\.xlsx$", full.names = TRUE)

# Function to read all sheets from a single file and add 'year' column
read_sheets_with_year <- function(file_path) {
  sheet_names <- excel_sheets(file_path)

  map_dfr(sheet_names, function(sheet) {
    read_excel(file_path, sheet = sheet) %>%
      mutate(year = as.numeric(sheet))  # assumes the sheet name is the year
  })
}

# Read and combine all files/sheets
scenario_data <- map_dfr(scenario_files, read_sheets_with_year)













# create empty elements that will get filled by the tool
intervention_mix_maps <- NULL
static_mix_maps <- NULL

# Shape files
country_outline <- sf::st_read("data/nga-demo-data-pre-processed/shapefiles/country_shapefile.shp")
state_outline   <- sf::st_read("data/nga-demo-data-pre-processed/shapefiles/state_shapefile_simp.shp")
lga_outline     <- sf::st_read("data/nga-demo-data-pre-processed/shapefiles/lga_shapefile_simp.shp")
state_outline$state[which(state_outline$state == "Akwa-Ibom")] <- "Akwa Ibom"

# plan budget data
national_budget <- NULL
state_budget    <- NULL
lga_budget      <- NULL

# Extract unique plans and their descriptions
unique_plans      <- NULL
plan_descriptions <- NULL
plan_labels       <- NULL

# values to use as the year input selection values
years_to_select <- NULL
plans_to_select <- NULL

# Extract unique plans and their descriptions, excluding 'baseline'
unique_plans2 <- NULL

