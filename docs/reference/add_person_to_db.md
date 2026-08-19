# Add a person to table_colnam using secure function

Adds a new person to table_colnam using the secure add_person()
PostgreSQL function. This works even if the user doesn't have direct
INSERT permission on table_colnam.

\*\*Requires\*\*: Database administrator must first run
\`setup_add_person_function()\`

## Usage

``` r
add_person_to_db(
  con,
  first_name,
  last_name,
  nationality = NULL,
  institute = NULL,
  contact = NULL
)
```

## Arguments

- con:

  Database connection

- first_name:

  Character. Person's first name (required)

- last_name:

  Character. Person's last name (required)

- nationality:

  Character. Person's nationality (optional)

- institute:

  Character. Person's institute/organization (optional)

- contact:

  Character. Contact email or phone (optional)

## Value

Integer. The ID of the newly created (or existing) person

## Examples

``` r
if (FALSE) { # \dontrun{
con <- call.mydb()

# Add a new person
id <- add_person_to_db(con, "John", "Doe",
                       nationality = "USA",
                       institute = "University XYZ")

# Person already exists - returns existing ID
id2 <- add_person_to_db(con, "John", "Doe")  # Returns same ID
} # }
```
