# La fonction gender() : un test rapide Man vs Machine
# Vendredi Quanti, 12/02/2021


# Packages ----

install_library <- function(packages) {
  for (i in packages){
    if(!is.element(i, .packages(all.available = TRUE))) install.packages(i, dep = TRUE)
    library(i,character.only = TRUE)
  }
}

install_library("questionr")
install_library("tidyverse")
install_library("devtools")
install_library("knitr")
install_library("kableExtra")


# Package datatools

library(devtools)
install_github("pmerckle/datatools", force = TRUE)
library(datatools)


# > Usages simples ----

gender("Henry")
gender("Paul-Marie")
gender("Ana Lisa")
gender("Jérôme")

gender(c("Jacques", "Bernadette", "Nicolas", "Carla", "François", "Julie", "Emmanuel", "Brigitte"))

gender("Camille")
gender("Camille", freq = TRUE)
gender("Camille", year_max = 1950)
gender("Camille", year_min = 1950)

is_female("Martin")
is_female("Marcelle")
is_male("Camille", year_max = 1950)
is_male("Zorglub")


# > Données ----

# > Importer les deux fichiers de qualification
load("data_total_2020.RData")
df <- data_total
# load("data_total_2021.RData")
# data_21 <- data_total
rm(list = "data_total")

# Assembler les deux fichiers
df <- df %>%
  select(
    Prénom,
    Corps,
    Date.de.naissance,
    Sexe.1,
    Avis.1
  ) %>%
  rename(sexe = Sexe.1)

# gender()
debut <- Sys.time()
temp <- gender(df$Prénom, year_min = 1960) %>% as.character %>% unlist
fin <- Sys.time()
vitesse <- length(temp) / as.numeric(fin - debut)
print(paste(round(vitesse), "prénoms par seconde"))

df$gender <- temp
df$gender[df$gender == "female"] <- "FEMME"
df$gender[df$gender == "male"] <- "HOMME"
df$gender[df$gender == "logical(0)"] <- NA
# Réordonnancement de df$gender
df$gender <- factor(df$gender,
  levels = c("FEMME", "HOMME")
)

# Croisement
table(df$sexe, df$gender, useNA = "always") %>% addmargins

# Codés différemment
df %>% filter(sexe != gender) %>%
  select(Prénom, sexe, gender) %>%
  kable(caption = "Prénoms non-codés par l'humain") %>%
  kable_classic

# Non-codés par l'humain
df %>% filter(is.na(sexe)) %>%
  select(Prénom, sexe, gender) %>%
  kable(caption = "Prénoms codés différemment par l'humain et par la fonction") %>%
  kable_classic

# Non-codés par la fonction
df %>% filter(is.na(gender)) %>%
  select(Prénom, sexe) %>%
  kable(caption = "Prénoms non-codés par la fonction gender()") %>%
  kable_classic

# Tableau croisé sexe avec la durée
tableau <- table(df$Avis.1, df$sexe)
tableau %>% cprop
chisq.test(tableau)

# Tableau croisé gender avec la durée
tableau <- table(df$Avis.1, df$gender)
tableau %>% cprop
chisq.test(tableau)

```
