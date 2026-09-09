# ============================================================
# Early neonatal weight loss in historical Swiss maternity records
# Master script: run complete modular analysis
# ============================================================

# Run this file from the project root directory.
# Required raw data file:
# data_raw/laus_cleaned_2024-09-06.csv

source("R/01_setup_packages_paths.R")
source("R/02_import_clean_cohort.R")
source("R/03_variables_outcomes.R")
source("R/04_descriptive_tables.R")
source("R/05_main_models.R")
source("R/06_sensitivity_models.R")
source("R/09_reviewer_revision.R")
source("R/07_figures.R")
source("R/10_reviewer_supplement.R")
source("R/08_export_outputs.R")

message("Complete analysis finished. Outputs are in the outputs/ folder.")
