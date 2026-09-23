# Validate a backbone argument

`"internal"` is accepted without touching the database. Any other code
must be a backbone offered to users (see \[list_backbones()\]).

## Usage

``` r
.validate_backbone(backbone = "internal", con_taxa = NULL)
```

## Arguments

- backbone:

  Character scalar.

- con_taxa:

  Connection or pool to the taxa database, used only for a backbone
  other than `"internal"`.

## Value

`backbone`, unchanged.
