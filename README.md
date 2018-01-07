# cure4insect: Custom Reporting for Intactness and Sector Effects

Install:

```R
devtools::install_github("ABbiodiversity/cure4insect")
```

Example:

```R
load_common_data()
SPP <- "Ovenbird"
PIX <- c("182_362", "182_363", "182_364", "182_365", "182_366", "182_367",
    "182_368", "182_369", "182_370", "182_371", "182_372")
subset_common_data(id=PIX, species=SPP)
load_species_data("Ovenbird")
x <- calculate_results()
x
flatten_results(x)

load_species_data("Ovenbird", boot=FALSE)
calculate_results()

SPP <- c("AlderFlycatcher", "Achillea.millefolium")
subset_common_data(id=PIX, species=SPP)
custom_report(address="psolymos@gmail.com")
```

