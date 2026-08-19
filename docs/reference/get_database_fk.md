# Get database foreign keys

Get a figure showing main tables and primary and foreign keys

## Usage

``` r
get_database_fk(con)
```

## Arguments

- con:

  A database connection. Optional; if \`NULL\`, will call
  \`call.mydb()\`.

## Value

A database structure diagram showing foreign key relationships between
the specified tables.
