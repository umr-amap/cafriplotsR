# Refuse a connection that cannot answer a hierarchy question

\`table_taxa.id_parent\` was added to the \*\*taxa\*\* database
(\`rainbio\`) by \`inst/migrations/taxa_hierarchy.R\`. The main database
carries its own \`table_taxa\` without that column, so a connection from
\`call.mydb()\` reaches a table of the right name and the wrong shape -
and every function in this file then fails several queries in with a raw
PostgreSQL "column child.id_parent does not exist".

## Usage

``` r
.require_taxa_hierarchy(con, caller)
```

## Arguments

- con:

  A plain DBI connection.

- caller:

  Name of the calling function, used in the message.

## Value

\`TRUE\`, invisibly. Aborts otherwise.

## Details

Checking up front costs one \`dbListFields()\` and says which of the two
things went wrong.
