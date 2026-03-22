# Get operations

This function returns information about operations

## Usage

``` r
get_operations(
  code = NULL,
  resource = "all",
  help = FALSE,
  ioe = FALSE,
  geographical_granularity = NULL,
  temporal_granularity = NULL,
  verbose = TRUE,
  lang = "ES"
)
```

## Arguments

- code:

  (string) operation identificator

- resource:

  (string) resource to access, by default `resource = "metadata"` to get
  serie metadata. Possible values are
  `all, metadata, variables_values or by_granularity`

- help:

  (boolean) type any value for `resource` param and type `help = TRUE`
  to see params available for this `resource`.

- ioe:

  (boolean) `TRUE` if code is in format `IO30138`, and `FALSE` by
  default

- geographical_granularity:

  (string) geographical granularity

- temporal_granularity:

  (string) temporal granularity

- verbose:

  (boolean) show more information during the process

- lang:

  (string) language used to obtain information

## Examples

``` r
get_operations()
get_operations(resource = "all")
get_operations(resource = "all", help = TRUE)
get_operations("IPC", resource = "metadata")
get_operations("IPC", resource = "variables_values")
get_operations(resource = "by_granularity", geographical_granularity = "PROV")
```
