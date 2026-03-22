# Get publications

This function returns all available publications from an operation or a
date

## Usage

``` r
get_publications(
  code = NULL,
  resource = "all",
  help = FALSE,
  det = 0,
  ioe = FALSE,
  lang = "ES"
)
```

## Arguments

- code:

  (string) operation or publication indentificator

- resource:

  (string) resource to access, by default `resource = "all"` to get
  serie metadata. Possible values are `all, operation or date`

- help:

  (boolean) type any value for `resource` param and type `help = TRUE`
  to see params available for this `resource`.

- det:

  (int) `det = 2` to see two levels of depth, specifically to access the
  `PubFechaAct` object, `det = 0` by default

- ioe:

  (boolean) `TRUE` if code is in format `IO30138`, and `FALSE` by
  default

- lang:

  (string) language used to obtain information

## Examples

``` r
get_publications()
get_publications(resource = "all", help = TRUE)
get_publications("IPC", resource = "operation")
get_publications(8, resource = "date")
```
