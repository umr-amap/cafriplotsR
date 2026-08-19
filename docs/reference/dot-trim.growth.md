# Identify potential errors for estimating growth

Add a column of individuals to be excluded because of potential errors
Adapted from http://ctfs.si.edu/Public/CTFSRPackage/

## Usage

``` r
.trim.growth(
  censuses,
  slope = 0.006214,
  intercept = 0.9036,
  err.limit = 4,
  maxgrow = 75,
  mindbh = 100
)
```

## Arguments

- censuses:

  tibble with first census

- slope:

  numeric see
  http://ctfs.si.edu/Public/CTFSRPackage/index.php/web/topics/growth~slash~growth.r/trim.growth

- intercept:

  numeric see
  http://ctfs.si.edu/Public/CTFSRPackage/index.php/web/topics/growth~slash~growth.r/trim.growth

- err.limit:

  integer any measure of second diameter higher than err.limit standard
  deviation below the first measure will be excluded

- maxgrow:

  numeric any growth in mm/year higher than maxgrow will be excluded

- mindbh:

  numeric minimum diameter in mm for excluding measures

## Value

tibble joining both census and with a added column indicating logical
whether inidividual should be excluded

## Author

Gilles Dauby, <gilles.dauby@ird.fr>
