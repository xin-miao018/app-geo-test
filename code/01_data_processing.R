# =============================================================================
# Data Processing — Clean Amplitude Exports for GeoLift
# =============================================================================

library(tidyverse)

# --- Cleaning Function --------------------------------------------------------

clean_amplitude_export <- function(filepath) {
  raw <- read.csv(filepath, skip = 7, header = TRUE, check.names = FALSE,
                  stringsAsFactors = FALSE, quote = "\"")

  colnames(raw) <- str_trim(colnames(raw), side = "both")
  colnames(raw) <- str_remove_all(colnames(raw), "^\t+")

  names(raw)[1] <- "location"

  raw <- raw %>%
    mutate(across(everything(), ~ str_trim(str_remove_all(.x, "^\t+"), side = "both")))

  raw <- raw %>%
    filter(location != "(none)", location != "")

  # Drop last column (partial week with severely depressed counts)
  raw <- raw[, -ncol(raw)]

  date_cols <- colnames(raw)[-1]

  raw <- raw %>%
    mutate(across(all_of(date_cols), as.numeric))

  long <- raw %>%
    pivot_longer(cols = all_of(date_cols), names_to = "date", values_to = "Y") %>%
    mutate(
      location = tolower(location),
      location = str_replace_all(location, " ", "_"),
      date = format(as.Date(date), "%m/%d/%Y")
    ) %>%
    arrange(location, date)

  return(long)
}

# --- Process Both Files -------------------------------------------------------

installs <- clean_amplitude_export("input/Weekly Install by Geo.csv")
trials   <- clean_amplitude_export("input/Weekly Trials by Geo.csv")

# --- Validation ---------------------------------------------------------------

cat("=== INSTALLS ===\n")
cat("  Geos:", n_distinct(installs$location), "\n")
cat("  Time periods:", n_distinct(installs$date), "\n")
cat("  Total rows:", nrow(installs), "\n")
cat("  NAs:", sum(is.na(installs$Y)), "\n\n")

cat("=== TRIALS ===\n")
cat("  Geos:", n_distinct(trials$location), "\n")
cat("  Time periods:", n_distinct(trials$date), "\n")
cat("  Total rows:", nrow(trials), "\n")
cat("  NAs:", sum(is.na(trials$Y)), "\n\n")

# Volume summary
cat("=== INSTALLS — Volume by Geo (Top 20) ===\n")
installs %>%
  group_by(location) %>%
  summarise(total = sum(Y, na.rm = TRUE), mean_weekly = round(mean(Y, na.rm = TRUE), 1)) %>%
  arrange(desc(total)) %>%
  print(n = 20)

cat("\n=== TRIALS — Volume by Geo (Top 20) ===\n")
trials %>%
  group_by(location) %>%
  summarise(total = sum(Y, na.rm = TRUE), mean_weekly = round(mean(Y, na.rm = TRUE), 1)) %>%
  arrange(desc(total)) %>%
  print(n = 20)

# Warn about low-volume trial geos
low_vol <- trials %>%
  group_by(location) %>%
  summarise(mean_weekly = mean(Y, na.rm = TRUE)) %>%
  filter(mean_weekly < 3)

if (nrow(low_vol) > 0) {
  cat("\n⚠ WARNING: The following geos have very low trial volume (mean < 3/week).\n")
  cat("  GeoLift may have limited power for these locations:\n")
  print(low_vol)
}

# --- Save Outputs -------------------------------------------------------------

dir.create("output", showWarnings = FALSE)
write.csv(installs, "output/cleaned_installs.csv", row.names = FALSE)
write.csv(trials, "output/cleaned_trials.csv", row.names = FALSE)

cat("\n✓ Saved output/cleaned_installs.csv\n")
cat("✓ Saved output/cleaned_trials.csv\n")
