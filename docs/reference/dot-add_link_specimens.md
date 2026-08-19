# Add Link Between Specimen and Individual

Creates links between herbarium specimens and individual trees in the
database. Includes validation to ensure foreign key references exist.

## Usage

``` r
.add_link_specimens(
  new_data,
  col_names_select = NULL,
  col_names_corresp = c("id_specimen", "id_n", "id_linktype"),
  launch_adding_data = FALSE,
  validate = TRUE,
  con = NULL
)
```

## Arguments

- new_data:

  Tibble with columns: id_specimen, id_n, and either id_linktype or type

- col_names_select:

  Character vector of column names in new_data to use. If NULL, uses all
  columns of new_data.

- col_names_corresp:

  Character vector of target column names. Default: c("id_specimen",
  "id_n", "id_linktype")

- launch_adding_data:

  Logical. If TRUE, links are actually added to database. Default FALSE
  for safety.

- validate:

  Logical. If TRUE, validates FK references before adding. Default TRUE.

- con:

  Database connection. If NULL, calls call.mydb()

## Value

Invisibly returns the data that was (or would be) added

## Details

Link types: - type_individual (id_linktype=1): Specimen collected from
this specific individual - referenced_individual (id_linktype=2):
Specimen represents same species but from different individual

The function: 1. Renames columns to standard names 2. Checks for
duplicate links (same id_specimen + id_n already in database) 3.
Validates FK references if validate=TRUE 4. Sets audit columns
(created_by, created_at) 5. Adds links if launch_adding_data=TRUE

## Author

Gilles Dauby, <gilles.dauby@ird.fr>
