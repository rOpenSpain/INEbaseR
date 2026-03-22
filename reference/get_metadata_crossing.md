# Get metadata crossing

This function returns data or metadata by metadata crossing

## Usage

``` r
get_metadata_crossing(
  code = NULL,
  resource = "series",
  help = FALSE,
  query = NULL,
  p = 1,
  det = 0,
  tip = NULL,
  nlast = 1,
  lang = "ES"
)
```

## Arguments

- code:

  (string) operation identificator

- resource:

  (string) resource to access, by default `resource = "metadata"` to get
  serie metadata. Possible values are `series or data`

- help:

  (boolean) type any value for `resource` param and type `help = TRUE`
  to see params available for this `resource`.

- query:

  (string) string separated by `AND` with syntax `variable = value`
  using natural language

- p:

  (int) periodicity, `p = 1` by default

- det:

  (int) `det = 2` to see two levels of depth, specifically to access the
  `PubFechaAct` object, `det = 0` by default

- tip:

  (string) `tip = M` to obtain the metadata (crossing variables-values)
  of the series

- nlast:

  last `n` values

- lang:

  (string) language used to obtain information

## Examples

``` r
get_metadata_crossing(resource = "series", help = TRUE)
get_metadata_crossing("IPC", resource = "series", query = "Provincias = Madrid AND Tipo de dato = Variacion mensual AND Grupos ECOICOP = NULL")
get_metadata_crossing("IPC", resource = "data", query = "Provincias = Madrid AND Tipo de dato = Variacion mensual AND Grupos ECOICOP = NULL", nlast = 5)
```
