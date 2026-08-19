# Internal function

Compute

## Usage

``` r
.link_table(
  data_stand,
  column_searched,
  column_name,
  id_field,
  id_table_name,
  db_connection = NULL,
  table_name,
  keep_columns = NULL,
  keep_original_value = FALSE,
  field_label = NULL
)
```

## Arguments

- data_stand:

  tibble

- column_searched:

  string vector

- column_name:

  name of the column that will store the id

- id_field:

  string name of the column of the output that will contain the id

- id_table_name:

  string name of the database table that contain the id

- db_connection:

  PqConnection connection of the database

- table_name:

  string name of the table in the database to search into

- keep_columns:

  string vector of the columns in the database table to keep in the
  output, by default NULL

## Value

vector

## Author

Gilles Dauby, <gilles.dauby@ird.fr>

## Examples

``` r
# .link_table(data_stand = data_stand, column_searched = "method", column_name = "method", id_field = "id_method", db_connection = mydb, table_name = "methodslist")

```
