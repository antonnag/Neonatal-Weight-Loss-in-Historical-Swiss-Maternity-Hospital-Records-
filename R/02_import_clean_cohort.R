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
# ------------------------------------------------------------

n_stillbirth <- sum(df$stillbirth != 0, na.rm = TRUE)

n_missing_birthweight <- sum(
  is.na(df$birthweight),
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

flowchart_exclusions <- tibble::tibble(
  Step = c(
    "Initial records",
    "Excluded: stillbirths",
    "Excluded: missing",
    "Excluded: no postnatal weight measurement",
    "Eligible singleton live births"
  ),
  N = c(
    n_original,
    n_stillbirth,
    n_missing_birthweight,
    n_no_postnatal_weight,
    n_original - n_stillbirth -
      n_missing_birthweight -
      n_no_postnatal_weight
  ),
  Percent_of_initial = round(N / n_original * 100, 1)
)

print(flowchart_exclusions)


df_analysis <- df %>%
  filter(
    stillbirth == 0,
    !is.na(birthweight),
    birthweight > 0,
    !is.na(weight_a) | !is.na(weight_b)
  )

n_eligible <- nrow(df_analysis)



