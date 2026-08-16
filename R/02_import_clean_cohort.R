# ============================================================
# 02_import_clean_cohort.R
# Import raw data and define initial eligible cohort
# ============================================================

if (!file.exists(path_raw_data)) {
  stop(
    "Raw data file not found: ", path_raw_data, "\n",
    "Please place laus_cleaned_2024-09-06.csv in the data_raw/ folder."
  )
}


df <- read_delim(
  path_raw_data,
  delim = ";",
  show_col_types = FALSE
) %>%
  clean_names()

glimpse(df)
dim(df)

# Initial analytical population
n_original <- nrow(df)

# ------------------------------------------------------------
# Flowchart exclusion counts
# Sequential exclusion counts before NWL trajectory cleaning
# ------------------------------------------------------------

n_original <- nrow(df)

n_stillbirth <- sum(
  !is.na(df$stillbirth) &
    df$stillbirth != 0
)

n_missing_livebirth_status <- sum(
  is.na(df$stillbirth)
)

n_missing_birthweight <- sum(
  df$stillbirth == 0 &
    (is.na(df$birthweight) | df$birthweight <= 0),
  na.rm = TRUE
)

n_no_postnatal_weight <- sum(
  df$stillbirth == 0 &
    !is.na(df$birthweight) &
    df$birthweight > 0 &
    is.na(df$weight_a) &
    is.na(df$weight_b),
  na.rm = TRUE
)

df_analysis <- df %>%
  dplyr::filter(
    stillbirth == 0,
    !is.na(birthweight),
    birthweight > 0,
    !is.na(weight_a) | !is.na(weight_b)
  )

n_eligible_raw <- nrow(df_analysis)
n_eligible <- n_eligible_raw

flowchart_exclusions <- tibble::tibble(
  Step = c(
    "Initial records",
    "Excluded: stillbirths",
    "Excluded: missing live-birth status",
    "Excluded: missing or invalid birthweight",
    "Excluded: no postnatal weight measurement",
    "Live births with birthweight and at least one postnatal weight"
  ),
  N = c(
    n_original,
    n_stillbirth,
    n_missing_livebirth_status,
    n_missing_birthweight,
    n_no_postnatal_weight,
    n_eligible_raw
  ),
  Percent_of_initial = round(N / n_original * 100, 1)
)

print(flowchart_exclusions)

print(paste("Raw eligible records before NWL trajectory cleaning:", n_eligible_raw))

# Check that the flowchart is additive
print(
  n_original -
    n_stillbirth -
    n_missing_livebirth_status -
    n_missing_birthweight -
    n_no_postnatal_weight
)

# 
# Step                                                               N Percent_of_initial
# <chr>                                                          <int>              <dbl>
#   1 Initial records                                                13033              100  
# 2 Excluded: stillbirths                                            536                4.1
# 3 Excluded: missing live-birth status                                1                0  
# 4 Excluded: missing or invalid birthweight                           8                0.1
# 5 Excluded: no postnatal weight measurement                        352                2.7
# 6 Live births with birthweight and at least one postnatal weight 12136               93.1
