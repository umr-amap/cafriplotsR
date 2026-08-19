# Link Executor Module - Server

Link Executor Module - Server

## Usage

``` r
mod_link_executor_server(id, validated_links, con, i18n)
```

## Arguments

- id:

  Module namespace ID

- validated_links:

  Reactive containing validated links from Step 5

- con:

  Reactive database connection pool

- i18n:

  Reactive returning translator object

## Value

Reactive list containing execution results
