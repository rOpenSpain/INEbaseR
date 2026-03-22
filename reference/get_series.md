# Get series

This function returns data or metadata from an operation, table or a
serie

## Usage

``` r
get_series(
  code = NULL,
  resource = "metadata",
  help = FALSE,
  ioe = FALSE,
  det = 0,
  tip = NULL,
  lang = "ES",
  date_start = NULL,
  date_end = NULL,
  nlast = NULL,
  classification = NULL,
  verbose = FALSE,
  benchmark = FALSE,
  geographical_granularity = NULL,
  temporal_granularity = NULL
)
```

## Arguments

- code:

  (string) serie, operation or table identificator

- resource:

  (string) resource to access, by default `resource = "metadata"` to get
  serie metadata. Possible values are
  `metadata, operation, values, table, metadataoperation, data, by_granularity, by_common_parameters or nlast`

- help:

  (boolean) type any value for `resource` param and type `help = TRUE`
  to see params available for this `resource`.

- ioe:

  (boolean) `TRUE` if code is in format `IO30138`, and `FALSE` by
  default

- det:

  (int) `det = 2` to see two levels of depth, specifically to access the
  `PubFechaAct` object, `det = 0` by default

- tip:

  (string) `tip = M` to obtain the metadata (crossing variables-values)
  of the series

- lang:

  (string) language used to obtain information

- date_start:

  (string) start date in format `YYYY-MM-DD`

- date_end:

  (string) end date in format `YYYY-MM-DD`

- nlast:

  (int) last `n` serie values

- classification:

  (string) serie classification, if `classification = NULL` this value
  will be auto-detected

- verbose:

  (boolean) to show more information about this process,
  `verbose = FALSE` by default

- benchmark:

  (boolean) used to measure the performance of the system,
  `benchmark = FALSE` by default.

- geographical_granularity:

  (string) geographical granularity

- temporal_granularity:

  (string) temporal granularity

## Examples

``` r
get_series("IPC206449")
get_series(resource = "metadata", help = TRUE)
get_series("IPC", resource = "operation")
get_series("IPC206449", resource = "values")
get_series(22350, resource = "table")
get_series("IPC251541", resource = "nlast")
get_series("IPC206449", resource = "data", nlast = 5)
get_series("IPC", resource = "by_granularity", geographical_granularity = "CCAA", verbose = TRUE)
get_series("IPC251539", resource = "by_common_parameters", verbose = TRUE)
```
