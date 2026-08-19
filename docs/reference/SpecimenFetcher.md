# Specimen Fetcher

R6 class for fetching specimen data from database

## Methods

### Public methods

- [`SpecimenFetcher$new()`](#method-SpecimenFetcher-initialize)

- [`SpecimenFetcher$fetch_by_ids()`](#method-SpecimenFetcher-fetch_by_ids)

- [`SpecimenFetcher$fetch_by_collector_and_number()`](#method-SpecimenFetcher-fetch_by_collector_and_number)

- [`SpecimenFetcher$fetch_with_filter()`](#method-SpecimenFetcher-fetch_with_filter)

- [`SpecimenFetcher$clone()`](#method-SpecimenFetcher-clone)

------------------------------------------------------------------------

### `SpecimenFetcher$new()`

#### Usage

    SpecimenFetcher$new(connection)

------------------------------------------------------------------------

### `SpecimenFetcher$fetch_by_ids()`

#### Usage

    SpecimenFetcher$fetch_by_ids(specimen_ids)

------------------------------------------------------------------------

### `SpecimenFetcher$fetch_by_collector_and_number()`

#### Usage

    SpecimenFetcher$fetch_by_collector_and_number(id_table_colnam, colnbr)

------------------------------------------------------------------------

### `SpecimenFetcher$fetch_with_filter()`

#### Usage

    SpecimenFetcher$fetch_with_filter(query)

------------------------------------------------------------------------

### `SpecimenFetcher$clone()`

The objects of this class are cloneable with this method.

#### Usage

    SpecimenFetcher$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
