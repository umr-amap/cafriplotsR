# Query builder for plot

Allow building progressively a SQL query to filter plots following
different criteria using the builder pattern

## Methods

- `new(connection)`:

  Initialize the builder with a database connection.

  - `connection`: A DBI connection object to the database

- `filter_country(country, interactive = FALSE)`:

  Filter plots by country name(s).

  - `country`: Character vector of country name(s)

  - `interactive`: Logical. If TRUE, uses .link_table for fuzzy matching

- `filter_plot_name(plot_name, interactive = FALSE)`:

  Filter plots by plot name(s).

  - `plot_name`: Character vector of plot name(s)

  - `interactive`: Logical. If TRUE, uses .link_table for fuzzy matching

- `filter_method(method, interactive = FALSE)`:

  Filter plots by method(s).

  - `method`: Character vector of method name(s)

  - `interactive`: Logical. If TRUE, uses .link_table for fuzzy matching

- `filter_locality(locality_name)`:

  Filter plots by locality name(s).

  - `locality_name`: Character vector of locality name(s)

- `build(operator = "AND")`:

  Build the final SQL query.

  - `operator`: Character. Join operator between conditions ("AND" or
    "OR"). Default is "AND"

  Returns a SQL query object

- `build_with_or()`:

  Build the SQL query with OR operator between conditions. Returns a SQL
  query object

- `add_custom_condition(condition, wrap_parentheses = TRUE)`:

  Add a custom SQL condition.

  - `condition`: Character. Raw SQL condition string

  - `wrap_parentheses`: Logical. If TRUE, wraps condition in parentheses

- `print_conditions()`:

  Display current filter conditions (for debugging).

## Methods

### Public methods

- [`PlotFilterBuilder$new()`](#method-PlotFilterBuilder-initialize)

- [`PlotFilterBuilder$filter_country()`](#method-PlotFilterBuilder-filter_country)

- [`PlotFilterBuilder$filter_plot_name()`](#method-PlotFilterBuilder-filter_plot_name)

- [`PlotFilterBuilder$filter_method()`](#method-PlotFilterBuilder-filter_method)

- [`PlotFilterBuilder$filter_locality()`](#method-PlotFilterBuilder-filter_locality)

- [`PlotFilterBuilder$build()`](#method-PlotFilterBuilder-build)

- [`PlotFilterBuilder$build_with_or()`](#method-PlotFilterBuilder-build_with_or)

- [`PlotFilterBuilder$add_custom_condition()`](#method-PlotFilterBuilder-add_custom_condition)

- [`PlotFilterBuilder$print_conditions()`](#method-PlotFilterBuilder-print_conditions)

- [`PlotFilterBuilder$clone()`](#method-PlotFilterBuilder-clone)

------------------------------------------------------------------------

### `PlotFilterBuilder$new()`

#### Usage

    PlotFilterBuilder$new(connection)

------------------------------------------------------------------------

### `PlotFilterBuilder$filter_country()`

#### Usage

    PlotFilterBuilder$filter_country(country, interactive = FALSE)

------------------------------------------------------------------------

### `PlotFilterBuilder$filter_plot_name()`

#### Usage

    PlotFilterBuilder$filter_plot_name(
      plot_name,
      interactive = FALSE,
      exact_match = FALSE
    )

------------------------------------------------------------------------

### `PlotFilterBuilder$filter_method()`

#### Usage

    PlotFilterBuilder$filter_method(method, interactive = FALSE)

------------------------------------------------------------------------

### `PlotFilterBuilder$filter_locality()`

#### Usage

    PlotFilterBuilder$filter_locality(locality_name)

------------------------------------------------------------------------

### `PlotFilterBuilder$build()`

#### Usage

    PlotFilterBuilder$build(operator = "AND")

------------------------------------------------------------------------

### `PlotFilterBuilder$build_with_or()`

#### Usage

    PlotFilterBuilder$build_with_or()

------------------------------------------------------------------------

### `PlotFilterBuilder$add_custom_condition()`

#### Usage

    PlotFilterBuilder$add_custom_condition(condition, wrap_parentheses = TRUE)

------------------------------------------------------------------------

### `PlotFilterBuilder$print_conditions()`

#### Usage

    PlotFilterBuilder$print_conditions()

------------------------------------------------------------------------

### `PlotFilterBuilder$clone()`

The objects of this class are cloneable with this method.

#### Usage

    PlotFilterBuilder$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
con <- call.mydb()
query <- PlotFilterBuilder$new(con)$
  filter_country("Gabon")$
  filter_method("transect")$
  build()
} # }
```
