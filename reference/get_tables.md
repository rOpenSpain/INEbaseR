# Get tables

This function returns data / metadata from tables

## Usage

``` r
get_tables(
  code = NULL,
  resource = "operation",
  help = FALSE,
  grp = NULL,
  geo = 0,
  nlast = 0,
  det = 0,
  tip = NULL,
  ioe = FALSE,
  lang = "ES"
)
```

## Arguments

- code:

  operation (string/int) or table (int) identificator

- resource:

  (string) resource to access, by default `resource = "operation"` to
  get tables of an operation. Possible values are
  `operation, group, group_values or data`

- help:

  (boolean) type any value for `resource` param and type `help = TRUE`
  to see params available for this `resource`

- grp:

  (int) group identification

- geo:

  (int) use `geo = 1` to access only tables with geographic content,
  `geo = 0` by default

- nlast:

  last `n` values

- det:

  (int) use `det = 2` to see two levels of depth, specifically to access
  the `PubFechaAct` object, `det = 0` by default

- tip:

  (string) use `tip = "A"` to view as friendly, specifically the view
  the field `ultima_modificacion` or use `tip = "AM"` to obtain the
  metadata (crossing variables-values) of the series and a friendly
  output.

- ioe:

  (boolean) use `ioe = TRUE` if code is in format `IO30138`, and `FALSE`
  by default

- lang:

  (string) language used to obtain information

## Examples

``` r
get_tables("IPC")
get_tables(resource = "operation", help = TRUE)
get_tables(25, resource = "operation")
get_tables(22350, resource = "group")
get_tables(22350, grp = 81497, resource = "group_values")
get_tables(22350, nlast = 5, resource = "data")
```
