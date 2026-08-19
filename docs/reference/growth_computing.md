# Growth computing for multiple census

Growth computing for multiple census

## Usage

``` r
growth_computing(
  dataset,
  metadata,
  mindbh = NULL,
  err.limit = 4,
  maxgrow = 75,
  method = "I",
  export_ind_growth = TRUE
)
```

## Arguments

- dataset:

  tibble, ouput of query_plots

- metadata:

  tibble, ouput of query_plots

- mindbh:

  numeric see
  http://ctfs.si.edu/Public/CTFSRPackage/index.php/web/topics/growth~slash~growth.r/trim.growth

- err.limit:

  integer any measure of second diameter higher than err.limit standard
  deviation below the first measure will be excluded

- maxgrow:

  numeric any growth in mm/year higher than maxgrow will be excluded

- method:

  string either 'I' or 'E'

- export_ind_growth:

  whether growth per individuals should be exported

## Value

tibble

## Details

growthrate is in

## Author

Gilles Dauby, <gilles.dauby@ird.fr>
