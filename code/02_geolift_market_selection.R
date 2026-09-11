# =============================================================================
# GeoLift Market Selection — US App Test
# =============================================================================

library(GeoLift)
library(tidyverse)

# =============================================================================
# HYPERPARAMETERS — Edit these before running
# =============================================================================

TREATMENT_PERIODS  <- c(4, 8, 12)         # test durations in weeks
N_TEST_MARKETS     <- c(6, 8, 10)          # number of treatment geos to evaluate
EFFECT_SIZE        <- seq(0, 0.20, 0.05)  # lift range for power curves
CPIC               <- 20                   # cost per incremental conversion
BUDGET             <- NULL                 # total budget (NULL = unconstrained)
ALPHA              <- 0.1                  # significance level
SIDE_OF_TEST       <- "one_sided"          # "one_sided" or "two_sided"
MODEL              <- "Ridge"              # "Ridge", "LASSO", "None"
FIXED_EFFECTS      <- TRUE                 # include fixed effects
RUN_INSTALLS       <- TRUE                 # run analysis on installs metric
RUN_TRIALS         <- FALSE                # run analysis on trials metric

# =============================================================================
# 1. Setup
# =============================================================================

dir.create("output/results", showWarnings = FALSE, recursive = TRUE)
dir.create("output/plots", showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# 2. Load Data & Run Market Selection
# =============================================================================

run_market_selection <- function(filepath, metric_name) {
  cat("\n###############################################################\n")
  cat("# MARKET SELECTION:", toupper(metric_name), "\n")
  cat("###############################################################\n\n")

  raw <- read.csv(filepath, stringsAsFactors = FALSE)

  geo_data <- GeoDataRead(
    data        = raw,
    date_id     = "date",
    location_id = "location",
    Y_id        = "Y",
    format      = "mm/dd/yyyy"
  )

  cat("GeoLift data summary:\n")
  print(summary(geo_data))

  # --- Exploratory Plot ---
  geo_plot <- GeoPlot(
    geo_data,
    Y_id        = "Y",
    time_id     = "time",
    location_id = "location"
  )
  ggsave(paste0("output/plots/01_geo_timeseries_", metric_name, ".png"),
         geo_plot, width = 14, height = 8)
  cat("Saved: output/plots/01_geo_timeseries_", metric_name, ".png\n")

  # --- Market Selection ---
  cat("\n--- Running Market Selection ---\n")
  cat("  Treatment periods:", paste(TREATMENT_PERIODS, collapse = ", "), "weeks\n")
  cat("  N test markets:", paste(N_TEST_MARKETS, collapse = ", "), "\n")
  cat("  Model:", MODEL, "\n")
  cat("  CPIC:", CPIC, "\n\n")

  market_selection <- GeoLiftMarketSelection(
    data              = geo_data,
    treatment_periods = TREATMENT_PERIODS,
    N                 = N_TEST_MARKETS,
    effect_size       = EFFECT_SIZE,
    cpic              = CPIC,
    budget            = BUDGET,
    side_of_test      = SIDE_OF_TEST,
    alpha             = ALPHA,
    model             = MODEL,
    fixed_effects     = FIXED_EFFECTS,
    parallel          = TRUE,
    parallel_setup    = "sequential"
  )

  # --- Save Results ---
  saveRDS(market_selection,
          paste0("output/results/market_selection_", metric_name, ".rds"))
  write.csv(market_selection$BestMarkets,
            paste0("output/results/best_markets_", metric_name, ".csv"),
            row.names = FALSE)

  # --- Print Top 10 ---
  cat("\n=== TOP 10 MARKET SELECTIONS —", toupper(metric_name), "===\n")
  top <- market_selection$BestMarkets %>% head(10)
  print(top %>% select(any_of(c("ID", "location", "duration", "EffectSize",
                                 "Power", "Average_MDE",
                                 "AvgScaledL2Imbalance", "Investment",
                                 "ProportionTotal_Y", "Holdout", "rank"))))

  # --- Lift Plots for Top 6 ---
  for (id in 1:min(6, nrow(market_selection$BestMarkets))) {
    tryCatch({
      g <- plot(market_selection, type = "Lift", market_ID = id)
      ggsave(paste0("output/plots/02_market_selection_", metric_name, "_id", id, ".png"),
             g, width = 10, height = 7)
    }, error = function(e) {
      cat("  Plot ID", id, "skipped:", conditionMessage(e), "\n")
    })
  }

  # --- Power Curves for Top 3 Selections ---
  cat("\n--- Running Power Analysis for Top 3 Selections ---\n")
  for (i in 1:min(3, nrow(market_selection$BestMarkets))) {
    locs <- unlist(strsplit(market_selection$BestMarkets$location[i], ", "))
    dur  <- market_selection$BestMarkets$duration[i]

    cat("  Selection", i, ":", paste(locs, collapse = ", "), "(", dur, "weeks)\n")

    tryCatch({
      p <- GeoLiftPower(
        data             = geo_data,
        locations        = locs,
        effect_size      = EFFECT_SIZE,
        treatment_periods = dur,
        cpic             = CPIC,
        side_of_test     = SIDE_OF_TEST,
        model            = MODEL
      )
      g <- plot(p)
      ggsave(paste0("output/plots/03_power_", metric_name, "_sel", i, "_", dur, "wk.png"),
             g, width = 10, height = 7)
    }, error = function(e) {
      cat("    Power plot skipped:", conditionMessage(e), "\n")
    })
  }

  cat("\n", toupper(metric_name), "market selection complete.\n")
  return(market_selection)
}

# =============================================================================
# 3. Run
# =============================================================================

if (RUN_INSTALLS) {
  ms_installs <- run_market_selection("output/cleaned_installs.csv", "installs")
}

if (RUN_TRIALS) {
  ms_trials <- run_market_selection("output/cleaned_trials.csv", "trials")
}

cat("\n\n===============================================================\n")
cat("MARKET SELECTION COMPLETE\n")
cat("===============================================================\n")
cat("Outputs:\n")
cat("  output/results/best_markets_*.csv    — ranked market selections\n")
cat("  output/results/market_selection_*.rds — full R objects\n")
cat("  output/plots/                         — timeseries, lift, power plots\n")
cat("\nReview results, then run 03_geo_assignment.R with your chosen markets.\n")
