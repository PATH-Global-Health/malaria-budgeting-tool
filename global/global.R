#-GLOBAL VARIABLES AND LIBRARIES------------------------------------------------
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
library(shinycssloaders)

#-UNIVERSAL WARNING LABEL DEMO INSTANCE-----------------------------------------
head_bold <- "IMPORTANT : Ceci est une version de démonstration de l’outil."
main_text <- "Les valeurs et les résultats présentés ici sont donnés à titre indicatif uniquement et visent à présenter les fonctionnalités de l'outil. Ils ne doivent pas servir à la prise de décision ni à l'extrapolation. De plus, les données présentées ici ne sont représentatives d'aucun scénario ni coût réel. Notre outil est en cours de développement; la version présentée ici est donc destinée à illustrer les fonctionnalités que nous développons. De nombreuses fonctionnalités sont encore en développement et nous avons hâte de les partager prochainement. N'hésitez pas à nous contacter à hthompson@path.org pour toute suggestion ou commentaire; nous serions ravis de recueillir les avis de notre communauté!"

#-LITE VERSION TOGGLE-----------------------------------------------------------
# define app as lite mode to suspend upload and delete ability
lite_mode <- FALSE

#-SCENARIO AND COST TEMPLATES---------------------------------------------------

# Define file paths for empty templates
template_file_path <- "www/scenario-template-empty_Francais.xlsx"
cost_template_file_path <- "www/cost-template-empty_Francais.xlsx"

# Store column names from scenario and cost templates
SCENARIO_COLS <- colnames(read_excel(template_file_path, sheet = "modèle"))
COST_COLS <- colnames(read_excel(cost_template_file_path, sheet = "couts_unitaires_data"))

# for cost template select the values that must be kept to ensure down
# stream functionality remains and that the user can add addiditional sheets
# to help with their own processing
COST_COLS_MATCH <- c(
  "code_intervention", "type_intervention",
  "cout_classe", "cout_classe_autre", "description",
  "unite", "cout_monnaie_locale", "cout_usd",
  "cout_annee_pour_analyse"
)

# Function: Extract only admin columns from scenario template
get_template_admin_data <- function() {
  template_data <- read_excel(template_file_path)
  admin_cols <- grep("^adm", colnames(template_data), value = TRUE)
  template_data[admin_cols]
}

# Store extracted admin data
TEMPLATE_ADMIN_DATA <- get_template_admin_data()

# Set default years to: current year + 1, +2, +3
DEFAULT_YEARS <- as.character((as.integer(format(Sys.Date(), "%Y")) + 1:3))


#-DATABASE SYNC AND INITIALIZATION----------------------------------------------

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
  if (type == "scenario") {
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

  # Remove any database records for files that no longer exist
  missing_files <- setdiff(db_files, existing_files)
  if (length(missing_files) > 0) {
    placeholders <- paste(rep("?", length(missing_files)), collapse = ",")
    dbExecute(
      db, sprintf("DELETE FROM uploads WHERE filename IN (%s)", placeholders),
      missing_files
    )
  }

  dbDisconnect(db)
}

# Create initial empty scenario database if it doesn't exist
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

#-DIRECTORY STRUCTURE AND INITIAL SYNC------------------------------------------

# Create required folders for file uploads
dir.create("uploads", showWarnings = FALSE)
dir.create("uploads/scenarios", showWarnings = FALSE)
dir.create("uploads/costs", showWarnings = FALSE)
dir.create("www", showWarnings = FALSE)

# Sync databases with uploaded files at startup
sync_database("scenario")
sync_database("cost")

#-SCENARIO DATA IMPORT----------------------------------------------------------
# Define path to scenario uploads
scenario_folder <- "uploads/scenarios"

# Get list of all uploaded Excel scenario files
scenario_files <- list.files(scenario_folder,
  pattern = "\\.xlsx$",
  full.names = TRUE
)

# Function to read all sheets from a single file and add 'year' column
read_sheets_with_year <- function(file_path) {
  sheet_names <- excel_sheets(file_path)

  map_dfr(sheet_names, function(sheet) {
    read_excel(file_path, sheet = sheet) %>%
      mutate(year = as.numeric(sheet)) # assumes the sheet name is the year
  })
}

# Read and combine all scenario files and their sheets
scenario_data <- map_dfr(scenario_files, read_sheets_with_year)

#-TARGET POPULATION DATA IMPORT-------------------------------------------------
# Load predefined target population data
target_population <-
  readxl::read_xlsx(
    "www/data-needs-not-user-defined-empty_Francais.xlsx",
    sheet = "population"
  )

#-SHAPEFILE IMPORTS-------------------------------------------------------------

## Previously used code to read and simplify raw shapefiles (commented out)
# country_outline <- NULL
# adm1_outline <- sf::st_read("data/shapefiles/drc-admin1-clean-id.shp") |>
#   select(adm1 = province, geometry) |>
#   st_simplify(dTolerance = 2000, preserveTopology = TRUE)
#
# adm2_outline <- sf::st_read("data/shapefiles/drc-admin2-clean-id.shp") |>
#   select(adm1 = provinc, adm2 = hlth_zn, geometry) |>
#   st_simplify(dTolerance = 2000, preserveTopology = TRUE)
#
# st_write(adm1_outline, "data/shapefiles/drc-admin1-clean-id-simp.shp")
# st_write(adm2_outline, "data/shapefiles/drc-admin2-clean-id-simp.shp")

# Load simplified shapefiles for faster use in app
adm1_outline <- sf::st_read("data/shapefiles/drc-admin1-clean-id-simp.shp")
adm2_outline <- sf::st_read("data/shapefiles/drc-admin2-clean-id-simp.shp")
