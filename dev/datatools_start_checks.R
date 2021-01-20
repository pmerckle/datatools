#
# datatools
# getting started
#



# Install packages and developing tools ----

install.packages(c("devtools", "roxygen2", "testthat", "knitr"))

# Install and check developing tools
library(devtools)
# has_devel() # Doit retourner TRUE (ou rien ?) si l'installation est correcte ?




# Test package ----

# Check package
check()

# General test


.libPaths("C:/Users/mercklep/Documents/R/win-library/3.6")
library(devtools)
devtools::install_github("pmerckle/datatools")

library(datatools)
soundex("Mercklé")
gender_unique("Pierre")
gender("Armando")
gender(c("Jacques", "Bernadette", "Nicolas", "Carla", "François", "Julie", "Emmanuel", "Brigitte"))
gender("Camille", year_max = 1950)
gender("Camille", year_min = 1950)
year("Théoxane")

# Help pages
package?datatools
?unaccent
?gender_unique

# Functions
unaccent("Jérémie")
gender("Henry")
gender(c("Patrick", "Michelle"))
is_female("Marcelle")
year("Théo")
year(c("Anouk", "Lilia"))
origin(c("Lars", "Ahmed", "Férouze", "Merike", "Merja"))

# Create new vignette
devtools::use_vignette("firstnamer")


# SANDBOX ----

library(tidyverse)
fn_fr %>%
  filter(year == 1993 & sex == "2") %>%
  arrange(desc(count)) %>%
  slice(1:10)


