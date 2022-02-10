#
# datatools
# getting started
#



# Install packages and developing tools ----
# install.packages(c("devtools", "roxygen2", "testthat", "knitr"))

# Install and check developing tools
library(devtools)
library(roxygen2)
# has_devel() # Doit retourner TRUE ou "Your system is ready to build packages" si l'installation est correcte.

# Test package ----

# Document and build
document(setwd("~/Dropbox/Data/datatools")) # Creates help pages and updates NAMESPACE

# Check package
check()
rm(list = c("fn_fr", "fn_or", "gender", "n", "unaccent"))



# Download online version
devtools::install_github("pmerckle/datatools")
library(datatools)

# Test functions

# firstnames
gender("Henry")
gender(c("Patrick", "Michelle"))
gender("Ana Lisa")
gender(c("Jacques", "Bernadette", "Nicolas", "Carla", "François", "Julie", "Emmanuel", "Brigitte"))
gender("Camille", year_max = 1950)
gender("Camille", year_min = 1950)
is_female("Martin")
is_female("Marcelle")
is_male("Zorglub")
is_female("Zorglub")
year("Théo")
year(c("Anouk", "Lilia"))
year("Nathalie")

# misc

# soundex
soundex("Berlin")

# str
unaccent("Jérémie")

# pcs
cs_household("35", "64")
gs_household("35", "64")



# Help pages
?datatools
?gender
?soundex
?unaccent




# SANDBOX ----

# Test chisq.stars() modifications

library(questionr)
data(hdv2003)

devtools::install_github("pmerckle/datatools", force = TRUE)
library(datatools)

tableau <- table(hdv2003$sexe, hdv2003$relig)
chisq.test(tableau)
chisq.stars(hdv2003$sexe, hdv2003$relig)


# Old

library(tidyverse)
library(questionr)
load("data-raw/fn_fr.RData")
temp <- table(fn_fr$year, fn_fr$sex)

devtools::install_github("pmerckle/datatools", force = TRUE)
library(datatools)

?cs_household
cs_household("32", "61")

?gs_household
gs_household(55, 12)

install.packages("tibble")



