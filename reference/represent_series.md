# Represent series

This function allows representing series data in maps or charts

## Usage

``` r
represent_series(
  code = NULL,
  resource = "maps",
  help = FALSE,
  nlast = 0,
  date_start = NULL,
  date_end = NULL,
  classification = NULL,
  map_scale = 60,
  verbose = FALSE,
  benchmark = FALSE,
  det = 0,
  type = NA,
  lang = "ES"
)
```

## Arguments

- code:

  (string) serie identificator

- resource:

  (string) resource to access, by default `resource = "all"` to get
  serie metadata. Possible values are `maps, plot or highcharts`

- help:

  (boolean) type any value for `resource` param and type `help = TRUE`
  to see params available for this `resource`.

- nlast:

  (int) last `n` serie data, if `nult = 0` this value will be
  auto-calculated

- date_start:

  (date) start date in format (string) `YYYY-MM-DD`

- date_end:

  (date) end date in format (string) `YYYY-MM-DD`

- classification:

  (string) serie classification, if `classification = NULL` this value
  will be auto-detected

- map_scale:

  (int) refers to the relationship or ratio between distance on a map
  and the corresponding distance on the ground. For example, on a
  `1:1000000` scale map, 1cm on the map equals 1km on the ground.
  Possible values are: `1, 3, 10, 20 or 60`, and it's only for PROV or
  CCAA geographical granularity, `map_scale = 60` by default and
  `map_scale = NULL` for high detailed map.

- verbose:

  (boolean) show more information during the process

- benchmark:

  (boolean) used to measure the performance of the system,
  `benchmark = FALSE` by default.

- det:

  (int) `det = 2` to see two levels of depth, specifically to access the
  `PubFechaAct` object, `det = 0` by default

- type:

  (string) what type of plot should be drawn, `type = "p"` (for points)
  by default. See `type` in
  [`plot`](https://rdrr.io/r/graphics/plot.default.html)

- lang:

  (string) language used to obtain information

## Examples

``` r
represent_series("IPC251521", resource = "maps")
represent_series(resource = "maps", help = TRUE)
represent_series("IPC251521", resource = "maps", map_scale = NULL)
represent_series("IPC206449", resource = "plot", nlast = 5)
represent_series("IPC206449", resource = "highcharts", nlast = 5)
```
