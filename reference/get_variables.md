# Get variables

This function returns all or operations variables

## Usage

``` r
get_variables(
  code = NULL,
  resource = "all",
  help = FALSE,
  ioe = FALSE,
  lang = "ES"
)
```

## Arguments

- code:

  (string) operation identifier

- resource:

  (string) resource to access, by default `resource = "metadata"` to get
  serie metadata. Possible values are `all or operation`

- help:

  (boolean) type any value for `resource` param and type `help = TRUE`
  to see params available for this `resource`.

- ioe:

  (boolean) `TRUE` if code is in format `IO30138`, and `FALSE` by
  default

- lang:

  (string) language used to obtain information

## Examples

``` r
get_variables()
get_variables(resource = "all", help = TRUE)
get_variables("IPC", resource = "operation")
```
