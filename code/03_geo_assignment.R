# =============================================================================
# Geo Assignment — Map Test/Control Markets
# =============================================================================
# Run this after reviewing market selection results from 02_geolift_market_selection.R
# Set the SELECTED_ID below to pull markets directly from best_markets CSV.

library(tidyverse)

# =============================================================================
# PARAMETERS — Edit these
# =============================================================================

SELECTED_ID <- 1                # ID from best_markets CSV
METRIC      <- "installs"       # "installs" or "trials"

# =============================================================================
# Load selection and generate assignment
# =============================================================================

best_markets <- read.csv(
  paste0("output/results/best_markets_", METRIC, ".csv"),
  stringsAsFactors = FALSE
)

chosen <- best_markets %>% filter(ID == SELECTED_ID)

if (nrow(chosen) == 0) {
  stop("ID ", SELECTED_ID, " not found in best_markets_", METRIC, ".csv")
}

test_markets <- unlist(strsplit(chosen$location[1], ", "))

cat("Selected ID:", SELECTED_ID, "\n")
cat("Duration:", chosen$duration[1], "weeks\n")
cat("Effect Size:", chosen$EffectSize[1], "\n")
cat("Power:", chosen$Power[1], "\n")
cat("Avg MDE:", chosen$Average_MDE[1], "\n")
cat("L2 Imbalance:", chosen$AvgScaledL2Imbalance[1], "\n\n")

all_geos <- sort(unique(
  read.csv(paste0("output/cleaned_", METRIC, ".csv"), stringsAsFactors = FALSE)$location
))

geo_assignment <- data.frame(
  geo   = all_geos,
  group = ifelse(all_geos %in% test_markets, "test", "control"),
  stringsAsFactors = FALSE
)

write.csv(geo_assignment, "output/geo_assignment.csv", row.names = FALSE)

cat("Geo assignment saved to output/geo_assignment.csv\n\n")
cat("Test markets (", sum(geo_assignment$group == "test"), "):\n")
cat(" ", paste(geo_assignment$geo[geo_assignment$group == "test"], collapse = ", "), "\n\n")
cat("Control markets (", sum(geo_assignment$group == "control"), "):\n")
cat(" ", paste(geo_assignment$geo[geo_assignment$group == "control"], collapse = ", "), "\n")
