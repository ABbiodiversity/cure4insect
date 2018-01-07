# cure4insect: Custom Reporting for Intactness and Sector Effects

Install:

```R
devtools::install_github("ABbiodiversity/cure4insect")
```

Examples:

```R
library(cure4insect)

## the workflow with 1 species

load_common_data()
Spp <- "Ovenbird"
ID <- c("182_362", "182_363", "182_364", "182_365", "182_366", "182_367",
    "182_368", "182_369", "182_370", "182_371", "182_372")
subset_common_data(id=ID, species=Spp)
load_species_data("Ovenbird")
x <- calculate_results()
x
flatten_results(x)

## wrapper function
## species="all" runs all species
## species="mites" runs all mite species
## sender=you@example.org will send an email with the results attached
z <- custom_report(id=ID, 
    species=c("AlderFlycatcher", "Achillea.millefolium"),
    address=NULL)
z
```

