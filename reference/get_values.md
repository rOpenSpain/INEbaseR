# Get values

This function returns values from a variable or variable operation

## Usage

``` r
get_values(
  code = NULL,
  resource = "all",
  operation = NULL,
  help = FALSE,
  det = 0,
  ioe = FALSE,
  lang = "ES"
)
```

## Arguments

- code:

  (int) variable indentificator

- resource:

  (string) resource to access, by default `resource = "all"` to get
  serie metadata. Possible values are `all or variable_operation`

- operation:

  (string) operation indentificator

- help:

  (boolean) type any value for `resource` param and type `help = TRUE`
  to see params available for this `resource`.

- det:

  (int) `det = 1` to see the detail of the variable to which it belongs,
  `det = 0` by default

- ioe:

  (boolean) `TRUE` if code is in format `IO30138`, and `FALSE` by
  default

- lang:

  (string) language used to obtain information

## Examples

``` r
get_values(115)
get_values(resource = "all", help = TRUE)
get_values(762, operation = "IPC", resource = "variable_operation")
```
