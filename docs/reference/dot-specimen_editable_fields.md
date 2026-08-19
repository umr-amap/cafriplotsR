# Editable (non-identification) fields of the specimens table

Named character vector mapping the specimen columns that
[`update_specimen_fields`](https://umr-amap.github.io/cafriplotsR/reference/update_specimen_fields.md)
is allowed to write to their expected R type (`"character"`, `"numeric"`
or `"integer"`).

Identification fields (`idtax_n`, `detby`, `detd`, `detm`, `dety`,
`detvalue`) are deliberately excluded: they are handled by
[`update_ident_specimens`](https://umr-amap.github.io/cafriplotsR/reference/update_ident_specimens.md).

## Usage

``` r
.specimen_editable_fields()
```

## Value

Named character vector, names are specimen column names.
