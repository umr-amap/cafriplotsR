# Setup credentials storage in environment variables

Helper function to configure credentials in .Renviron file. WARNING:
Credentials will be stored in plain text. Only use on secure personal
computers.

## Usage

``` r
setup_db_credentials(user = NULL, pass = NULL)
```

## Arguments

- user:

  Username for database

- pass:

  Password for database
