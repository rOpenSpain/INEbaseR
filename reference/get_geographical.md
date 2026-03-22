# Get geographical

This function returns geographical information

## Usage

``` r
get_geographical(
  code = NULL,
  resource = "variable",
  all = FALSE,
  verbose = FALSE,
  natcode = NULL,
  geocode = NULL,
  exponential_notation = FALSE,
  help = FALSE
)
```

## Arguments

- code:

  (string) serie id

- resource:

  (string) resource to access, by default `resource = "variable"` to get
  serie metadata. Possible values are
  `variable, natcode or natcode_to_geocode`

- all:

  (boolean) if `all = TRUE` you will get all natcodes

- verbose:

  (boolean) show more information during the process

- natcode:

  (int) geographical code from INE

- geocode:

  (string) geographical code from Eurostat

- exponential_notation:

  (boolean) to show or not exponential notation. `e.g. e+10`

- help:

  (boolean) type any value for `resource` param and type `help = TRUE`
  to see params available for this `resource`.

## Examples

``` r
get_geographical("IPC251522")
get_geographical(resource = "variable", help = TRUE)
get_geographical("IPC251522", resource = "natcode")
get_geographical(natcode = 34050000000, resource = "natcode_to_geocode")
```
