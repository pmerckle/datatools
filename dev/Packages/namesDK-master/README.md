
<!-- README.md is generated from README.Rmd. Please edit that file -->
namesDK
=======

Gender classifier for danish names
----------------------------------

Description

The function uses the official danish lists of male, female and unisex first names to classify names as either male, female or unisex. If a name can not be determined as either one of those three it will return as NA.

Examples

``` r
library(namesDK)
gender("Lars Løkke Rasmussen")
#> [[1]]
#> [1] "male"
gender(c("Helle Thorning Smidt", "Lars Løkke Rasmussen", "Traktor Troels"))
#> [[1]]
#> [1] "female"
#> 
#> [[2]]
#> [1] "male"
#> 
#> [[3]]
#> [1] NA
```
