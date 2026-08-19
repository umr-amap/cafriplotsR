# Query All Specimen Links for Individuals

Returns ALL specimen links for specified individuals, including link
type information and specimen details. Unlike merge_individuals_taxa()
which returns only the primary link, this returns all links.

## Usage

``` r
query_all_specimen_links(
  id_ind = NULL,
  id_specimen = NULL,
  include_specimen_info = TRUE,
  include_linktype_info = TRUE,
  con = NULL
)
```

## Arguments

- id_ind:

  Integer vector of individual IDs. If NULL, returns all links.

- id_specimen:

  Integer vector of specimen IDs. If NULL, ignored.

- include_specimen_info:

  Logical. If TRUE, joins specimen details.

- include_linktype_info:

  Logical. If TRUE, joins link type details.

- con:

  Database connection. If NULL, calls call.mydb()

## Value

Tibble with link information
