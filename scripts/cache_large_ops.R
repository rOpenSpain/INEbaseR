# Cache large operations: 426, 432, 436, 437, 438, 450, 455, 465
# Run from repo root: Rscript scripts/cache_large_ops.R

setwd("C:/Users/admin/Documents/GitHub/INEbaseR")

library(jsonlite)
source("R/resources.R")
source("R/operations.R")
source("R/series.R")
source("R/cache.R")

# Override get_cache_directory_path so the script works without
# the package being installed (find.package would fail otherwise)
get_cache_directory_path <- function(package = "INEbaseR", path = "extdata") {
  file.path("C:/Users/admin/Documents/GitHub/INEbaseR/inst", path)
}

ops_pending <- c(426, 432, 436, 437, 438, 450, 455, 465)

extdata_path <- "inst/extdata"

cat("=== Caching", length(ops_pending), "large operations ===\n\n")

for (op_id in ops_pending) {

  rds_file <- file.path(extdata_path, paste0("SERIEOPERATION-", op_id, ".rds"))

  if (file.exists(rds_file)) {
    cat("--- Operation", op_id, "already cached, skipping ---\n")
    next
  }

  cat("--- Operation", op_id, "---\n")
  t_start <- proc.time()

  data <- get_series_operation_api(
    operation = op_id,
    det = 2,
    tip = "M",
    pagination = TRUE
  )

  t_elapsed <- (proc.time() - t_start)[["elapsed"]]

  if (is.null(data) || NROW(data) == 0) {
    cat("  SKIPPED: no data returned for operation", op_id, "\n\n")
    next
  }

  saveRDS(data, file = rds_file, compress = TRUE)
  cat(sprintf("  Saved: %d rows in %.1f seconds\n", NROW(data), t_elapsed))
  cat(sprintf("  Cols: %s\n\n", paste(names(data), collapse = ", ")))
}

cat("=== Done ===\n")
