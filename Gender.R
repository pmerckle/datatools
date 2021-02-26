# La fonction gender() : un test rapide Human vs Machine
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


# Package datatools ----

library(devtools)
install_github("pmerckle/datatools", force = TRUE)
library(datatools)


# Usages simples ----

gender("Henry")
gender("Paul-Marie")
gender("Ana Lisa")

gender(c("Jacques", "Bernadette", "Nicolas", "Carla", "François", "Julie", "Emmanuel", "Brigitte"))

gender("Camille")
gender("Camille", freq = TRUE)
gender("Camille", year_max = 1950)
gender("Camille", year_min = 1950)

is_female("Martin")
is_female("Marcelle")
is_male("Camille", year_max = 1950)
is_male("Zorglub")


# Human vs Machine ----

# Importer les deux fichiers de qualification
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
    Discipline.1,
    Date.de.naissance,
    Sexe.1,
    Avis.1
  ) %>%
  rename(Human = Sexe.1)
df$Prénom <- iconv(df$Prénom, to="ASCII//TRANSLIT//IGNORE")

# gender()
debut <- Sys.time()
temp <- gender(df$Prénom, year_min = 1960) %>% as.character %>% unlist
fin <- Sys.time()
vitesse <- length(temp) / as.numeric(fin - debut)
print(paste(round(vitesse), "prénoms par seconde"))

df$Machine <- temp
df$Machine[df$Machine == "female"] <- "FEMME"
df$Machine[df$Machine == "male"] <- "HOMME"
df$Machine[df$Machine == "logical(0)"] <- NA
# Réordonnancement de df$Machine
df$Machine <- factor(df$Machine,
  levels = c("FEMME", "HOMME")
)

# Croisement
table(df$Human, df$Machine, useNA = "always") %>% addmargins %>% kable %>% kable_classic

# Codés différemment
df %>% filter(Human != Machine) %>%
  select(Prénom, Human, Machine) %>%
  kable(caption = "Prénoms codés différemment") %>%
  kable_classic

# Non-codés par l'humain
df %>% filter(is.na(Human)) %>%
  select(Prénom, Human, Machine) %>%
  kable(caption = "Prénoms non-codés par l'être humain") %>%
  kable_classic

# Non-codés par la fonction
df %>% filter(is.na(Machine)) %>%
  select(Prénom, Human, Machine) %>%
  kable(caption = "Prénoms non-codés par la machine") %>%
  kable_classic

# Tableau croisé sexe avec l'avis
tableau <- table(df$Avis.1, df$Human)
tableau %>% cprop %>% round(1) %>%
  kable(caption = "Avis en fonction du sexe codé par l'être humain") %>%
  kable_classic
chisq.test(tableau)

# Tableau croisé gender avec l'avis
tableau <- table(df$Avis.1, df$Machine)
tableau %>% cprop %>% round(1) %>%
  kable(caption = "Avis en fonction du sexe codé par la machine") %>%
  kable_classic
chisq.test(tableau)


