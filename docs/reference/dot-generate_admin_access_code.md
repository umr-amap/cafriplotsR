# Generate Admin Access Code

Generates R code for admin to grant row-level security access. Uses
plot_names (which user knows) instead of plot_ids (which user can't
see).

## Usage

``` r
.generate_admin_access_code(username, plot_ids, plot_names)
```

## Arguments

- username:

  Username who imported the plots

- plot_names:

  Vector of plot names

## Value

Character string with R code
