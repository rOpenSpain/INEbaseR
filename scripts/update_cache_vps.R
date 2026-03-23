# Full cache regeneration for VPS monthly cron
# Run from repo root: Rscript scripts/update_cache_vps.R
# Deletes all SERIEOPERATION-*.rds and recaches every operation from the API.

setwd("/opt/INEbaseR")

library(jsonlite)
source("R/resources.R")
source("R/operations.R")
source("R/series.R")
source("R/cache.R")

# Override get_cache_directory_path so the script works without
# the package being installed (find.package would fail otherwise)
get_cache_directory_path <- function(package = "INEbaseR", path = "extdata") {
  file.path("/opt/INEbaseR/inst", path)
}

log_msg <- function(msg) {
  cat(paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", msg, "\n"))
}

extdata_path <- "inst/extdata"

# Delete all existing SERIEOPERATION-*.rds to force full recache
existing <- list.files(extdata_path, pattern = "^SERIEOPERATION-.*\\.rds$", full.names = TRUE)
if (length(existing) > 0) {
  file.remove(existing)
  log_msg(paste("Deleted", length(existing), "existing SERIEOPERATION-*.rds files"))
}

# Fetch list of all operations from the API
log_msg("Fetching list of all operations...")
operations <- get_operations_all()
log_msg(paste("Found", nrow(operations), "operations to cache"))

cat("\n")

for (i in seq_len(nrow(operations))) {

  op_id   <- operations$Id[i]
  op_name <- operations$Nombre[i]
  rds_file <- file.path(extdata_path, paste0("SERIEOPERATION-", op_id, ".rds"))

  log_msg(paste0("[", i, "/", nrow(operations), "] Operation ", op_id, ": ", op_name))
  t_start <- proc.time()

  data <- get_series_operation_api(
    operation  = op_id,
    det        = 2,
    tip        = "M",
    pagination = TRUE
  )

  t_elapsed <- (proc.time() - t_start)[["elapsed"]]

  if (is.null(data) || NROW(data) == 0) {
    log_msg(paste0("  SKIPPED: no data returned (", round(t_elapsed, 1), "s)"))
    cat("\n")
    next
  }

  saveRDS(data, file = rds_file, compress = TRUE)
  log_msg(paste0("  Saved: ", NROW(data), " rows in ", round(t_elapsed, 1), "s"))
  cat("\n")

}

log_msg("=== Cache update complete ===")
