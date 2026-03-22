# Update cache

This function allows to update cache data

## Usage

``` r
update_cache(
  code = NULL,
  resource = "series",
  help = FALSE,
  n = 0,
  page = 1,
  pagination = TRUE,
  page_start = NULL,
  page_end = NULL,
  benchmark = FALSE,
  force = FALSE,
  ignore_series = NULL,
  tip = "M",
  det = 2,
  lang = "ES"
)
```

## Arguments

- code:

  (string) operation identificator

- resource:

  (string) resource to access, by default `resource = "all"` to get
  serie metadata. Possible values are `all or series`

- help:

  (boolean) type any value for `resource` param and type `help = TRUE`
  to see params available for this `resource`.

- n:

  (int) number of operation to update starting from first operation
  getted from `get_operations_all()` function.

- page:

  (int) `page = 1` to obtain data of an specific page (to use this,
  `pagination = FALSE`).

- pagination:

  (boolean) `TRUE` to obtain data page by page and `FALSE` by default.

- page_start:

  (int) `page_start = 1` start page range to obtain data (to use this,
  `pagination = TRUE`).

- page_end:

  (int) `page_end = 2` end page range to obtain data (to use this,
  `pagination = TRUE`).

- benchmark:

  (boolean) used to measure the performance of the system,
  `benchmark = FALSE` by default.

- force:

  (boolean) to force to update all cache data, `force = FALSE` by
  default.

- ignore_series:

  (list) list of operation identificators to ignore. More slow series to
  cache are: 16, 49, 330 and 334

- tip:

  (string) `tip = M` to obtain the metadata (crossing variables-values)
  of the series.

- det:

  (int) `det = 2` to see two levels of depth, specifically to access the
  `PubFechaAct` object, `det = 0` by default

- lang:

  (string) language used to obtain information

## Examples

``` r
update_cache(resource = "series")
update_cache(resource = "series", help = TRUE)
```
