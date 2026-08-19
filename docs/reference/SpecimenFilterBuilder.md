# Specimen Filter Builder

R6 class for building SQL queries to filter specimens with a fluent API

## Methods

### Public methods

- [`SpecimenFilterBuilder$new()`](#method-SpecimenFilterBuilder-initialize)

- [`SpecimenFilterBuilder$filter_collector()`](#method-SpecimenFilterBuilder-filter_collector)

- [`SpecimenFilterBuilder$filter_number()`](#method-SpecimenFilterBuilder-filter_number)

- [`SpecimenFilterBuilder$filter_taxonomy()`](#method-SpecimenFilterBuilder-filter_taxonomy)

- [`SpecimenFilterBuilder$filter_by_ids()`](#method-SpecimenFilterBuilder-filter_by_ids)

- [`SpecimenFilterBuilder$build()`](#method-SpecimenFilterBuilder-build)

- [`SpecimenFilterBuilder$add_custom_condition()`](#method-SpecimenFilterBuilder-add_custom_condition)

- [`SpecimenFilterBuilder$print_conditions()`](#method-SpecimenFilterBuilder-print_conditions)

- [`SpecimenFilterBuilder$clone()`](#method-SpecimenFilterBuilder-clone)

------------------------------------------------------------------------

### `SpecimenFilterBuilder$new()`

#### Usage

    SpecimenFilterBuilder$new(connection)

------------------------------------------------------------------------

### `SpecimenFilterBuilder$filter_collector()`

#### Usage

    SpecimenFilterBuilder$filter_collector(
      collector = NULL,
      id_colnam = NULL,
      interactive = FALSE
    )

------------------------------------------------------------------------

### `SpecimenFilterBuilder$filter_number()`

#### Usage

    SpecimenFilterBuilder$filter_number(
      number = NULL,
      number_min = NULL,
      number_max = NULL
    )

------------------------------------------------------------------------

### `SpecimenFilterBuilder$filter_taxonomy()`

#### Usage

    SpecimenFilterBuilder$filter_taxonomy(
      genus = NULL,
      species = NULL,
      family = NULL,
      idtax_n = NULL
    )

------------------------------------------------------------------------

### `SpecimenFilterBuilder$filter_by_ids()`

#### Usage

    SpecimenFilterBuilder$filter_by_ids(specimen_ids)

------------------------------------------------------------------------

### `SpecimenFilterBuilder$build()`

#### Usage

    SpecimenFilterBuilder$build(operator = "AND")

------------------------------------------------------------------------

### `SpecimenFilterBuilder$add_custom_condition()`

#### Usage

    SpecimenFilterBuilder$add_custom_condition(condition, wrap_parentheses = TRUE)

------------------------------------------------------------------------

### `SpecimenFilterBuilder$print_conditions()`

#### Usage

    SpecimenFilterBuilder$print_conditions()

------------------------------------------------------------------------

### `SpecimenFilterBuilder$clone()`

The objects of this class are cloneable with this method.

#### Usage

    SpecimenFilterBuilder$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
