# CEOdata <img src='man/figures/logo.png' align="right" height="139" />

Easy and convenient access to the datasets / microdata of the ["Centre d'Estudis d'Opinió"](https://ceo.gencat.cat/ca/inici/), the Catalan institution for polling and public opinion. The package uses the data stored in the open data platform of the Generalitat de Catalunya and returns it in a tidy format (tibble).

The main function, `CEOdata()` can return either:
1. An accumulated microdata series (identified by `series`), or
2. A single study dataset (identified by `reo`)

## Installation

To install from GitHub (development version), run:
```
install.packages("remotes")
remotes::install_github("ceopinio/CEOdata")
```

## Basic usage

### 1. Accumulated microdata series

List available accumulated series:
```
CEOaccumulated_meta()
```

Load the default accumulated series ("BOP_presencial")
```
d <- CEOdata()
```

Load a specific accumulated series
```
d_longi <- CEOdata(series = "Longitudinal")
```

### 2. Individual studies (REO)

You can inspect the catalogue of individual studies using:
```
CEOmeta()
```
And load the microdata of a specific study by REO code:
```
CEOdata(reo = "1145")
```
By default, SPSS labelled variables are converted into R factors.
To keep the original haven-labelled format:
```
CEOdata(reo = "1145", raw = TRUE)
```

## Vignettes
To access the package vignettes, please use:
```
vignette('using_CEOdata')
vignette('working_with_survey_data_using_the_CEOdata_package')
```

Report bugs, request improvements, ask questions or provide ideas at the [issues page](https://github.com/ceopinio/CEOdata/issues/).
