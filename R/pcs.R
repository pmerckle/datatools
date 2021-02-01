# pcs
#
# Functions for socio-economic classification
#



# csind_to_gsind() ----

#' Coding Individual CS to Individual GS
#'
#' # Création de la variable de regroupement gsind à un chiffre à partir de la variables cs à deux chiffres.
#' @param csind 2-digit PCS socio-economic classification category
#' @value 1-digit individual socio-economic classification category
#' @details
#'Création de la variable de regroupement gsind à un chiffre à partir de la variables cs à deux chiffres, par la fusion des GS individuels 1 et 2 et le déplacement de la CS 23.
#' Attention : la fonction permet de coder à partir de la CS détaillée, ainsi qu'à partir de la CS de niveau intermédiaire. Cette dernière comporte les codes  10, 32, 36, 41, 51, 61, 66, 73, 76, 81 et 82, à la place des catégories détaillées correspondantes.
# ' Cette fonction n'est pas utilisée directement, elle est mobilisée dans les fonctions csmenage() et gsmenage()
#' @seealso \code{\link[datatools]{gsind_to_household}}, \code{\link[datatools]{cs_household}} \code{\link[datatools]{gs_household}}

csind_to_gsind <- function(csind) {
  csind <- as.character(csind)
  gsind <- NA
  gsind[csind %in% c("23", "3", "30", "31", "32", "33", "34", "35", "36", "37", "38", "74")] <- "1"
  gsind[csind %in% c("4", "40", "41", "42", "43", "44", "45", "46", "47", "48", "73", "75")] <- "2"
  gsind[(substr(csind, 1 , 1) %in% c("1", "2") | csind %in% c("71", "72")) & csind != "23"] <- "3"
  gsind[csind %in% c("5", "50", "51", "52", "53", "54", "55", "56", "77")] <- "4"
  gsind[csind %in% c("6", "60", "61", "62", "63", "64", "65", "66", "67", "68", "69", "76", "78")] <- "5"
  gsind[is.na(gsind)] <- "6"
  return(gsind)
}



# gsind_to_household ----

#' Coding Individual GS to Household CS or GS
#'
#' # Création de la variable csmenage à partir des 2 variables gsind à 1 chiffre de la personne de référence et du conjoint.
#' @param gsind1 2-digit PCS socio-economic classification categories of first household member
#' @param gsind2 2-digit PCS socio-economic classification categories of second household member (if any)
#' @value Household socio-economic classification category
#' @details
#' Cette fonction n'est pas utilisée directement, elle est mobilisée dans les fonctions cs_household() et gs_household()
#' @seealso \code{\link[datatools]{gsind_to_house}}, \code{\link[datatools]{cs_household}} \code{\link[datatools]{gs_household}}

gsind_to_cshouse <- function(gsind1, gsind2) {
  g1_g2 <- paste0(gsind1, gsind2)
  res <- NA
  res[g1_g2 == "11"] <-"I-A"
  res[g1_g2 == "12"] <-"I-B"
  res[g1_g2 == "21"] <-"I-B"
  res[g1_g2 == "13"] <-"II-B"
  res[g1_g2 == "14"] <-"II-B"
  res[g1_g2 == "15"] <-"II-B"
  res[g1_g2 == "31"] <-"II-B"
  res[g1_g2 == "51"] <-"II-B"
  res[g1_g2 == "41"] <-"II-B"
  res[g1_g2 == "22"] <-"II-A"
  res[g1_g2 == "32"] <-"II-A"
  res[g1_g2 == "23"] <-"II-A"
  res[g1_g2 == "16"] <-"II-C"
  res[g1_g2 == "61"] <-"II-C"
  res[g1_g2 == "42"] <-"III-B"
  res[g1_g2 == "24"] <-"III-B"
  res[g1_g2 == "25"] <-"III-B"
  res[g1_g2 == "52"] <-"III-B"
  res[g1_g2 == "44"] <-"III-A"
  res[g1_g2 == "26"] <-"III-C"
  res[g1_g2 == "62"] <-"III-C"
  res[g1_g2 == "34"] <-"IV-A"
  res[g1_g2 == "35"] <-"IV-A"
  res[g1_g2 == "43"] <-"IV-A"
  res[g1_g2 == "53"] <-"IV-A"
  res[g1_g2 == "33"] <-"IV-A"
  res[g1_g2 == "36"] <-"IV-B"
  res[g1_g2 == "63"] <-"IV-B"
  res[g1_g2 == "45"] <-"V-A"
  res[g1_g2 == "54"] <-"V-A"
  res[g1_g2 == "55"] <-"V-B"
  res[g1_g2 == "64"] <-"VI-A"
  res[g1_g2 == "46"] <-"VI-A"
  res[g1_g2 == "56"] <-"VI-B"
  res[g1_g2 == "65"] <-"VI-B"
  res[g1_g2 == "66"] <-"VII"
  return(res)
}



# cs_household() ----


#' Household Socio-Economic Classification from French individual PCS of the household member
#'
#' Computes the detailed socio-economic class of a household from the individual socio-economic classes of its members
#' @param csind1 2-digit PCS socio-economic classification categories of first household member
#' @param csind2 2-digit PCS socio-economic classification categories of second household member (if any)
#' @value 1 latin number +  1-letter detailed socio-economic classification category
#' @details
#' The function computes the French detailed socio-economic class of a household by following the rules indicated in CNIS, 2019, p. 42 sq. The resulting classification distinguishes 7 groups (coded with latin numerals), and 16 subgroups with a distinction between homogamous and heterogamous households (coded with arabic numerals):
#'
#'   | Code  | Category
#'   |-------|-------------------------------------------------------------------------|
#'   | I     | Ménages à dominante cadre                                               |
#'   | I-A   | Cadre avec cadre                                                        |
#'   | I-B   | Cadre avec profession intermédiaire                                     |
#'   | II    | Ménages à dominante intermédiaire ou cadre                              |
#'   | II-A  | Cadre avec employé ou ouvrier                                           |
#'   | II-B  | Cadre avec inactif ou sans conjoint                                     |
#'   | II-C  | Profession intermédiaire ou cadre avec petit indépendant                |
#'   | II-D  | Profession intermédiaire avec profession intermédiaire                  |
#'   | III   | Ménages à dominante employée ou intermédiaire                           |
#'   | III-A | Profession intermédiaire avec employé ou ouvrier                        |
#'   | III-B | Profession intermédiaire avec inactif ou sans conjoint                  |
#'   | III-C | Employé avec employé                                                    |
#'   | IV    | Ménages à dominante petit indépendant                                   |
#'   | IV-A  | Petit indépendant avec petit indépendant, avec inactif ou sans conjoint |
#'   | IV-B  | Petit indépendant avec employé ou ouvrier                               |
#'   | V     | Ménages à dominante ouvrière                                            |
#'   | V-A   | Ouvrier avec employé                                                    |
#'   | V-B   | Ouvrier avec ouvrier                                                    |
#'   | VI    | Ménages mono-actifs d’employé ou d’ouvrier                              |
#'   | VI-A  | Employé avec inactif ou sans conjoint                                   |
#'   | VI-B  | Ouvrier avec inactif ou sans conjoint                                   |
#'   | VII   | Ménages inactifs                                                        |
#'   | VII-A | Inactif avec inactif ou sans conjoint                                   |
#'
#' @md
#' Note1: the "Ménages inactifs" category (VII) excludes retirees who have worked but includes the unemployed who have never worked; the "cadre" category refers in this classification to the group of managers and senior professionals and, for the working population, to the category of managers of enterprises with more than 10 employees. The retired inactive persons are classified with the "petits indépendants", i.e. farmers, craftsmen and tradesmen).
#' Note2: the function allows coding from the detailed as well as from the intermediate CS classification. The latter includes codes 10, 32, 36, 41, 51, 61, 66, 73, 76, 81 and 82, instead of the corresponding detailed categories.
#' @seealso \code{\link[datatools]{gs_household}} for a synthetic classification in only 7 groups.
#' @example
#' cs_household("32", "61")
#' @export

cs_household <- function(csind1, csind2) {
  gsind1<- csind_to_gsind(csind1)
  gsind2<- csind_to_gsind(csind2)
  res <- gsind_to_cshouse(gsind1, gsind2)
  return(res)
}

# gs_household() ----

#' Household Socio-Economic Classification from French individual PCS of the household member
#'
#' Computes the synthetic socio-economic class of a household from the individual socio-economic classes of its members
#' @param csind1 2-digit PCS socio-economic classification categories of first household member
#' @param csind2 2-digit PCS socio-economic classification categories of second household member (if any)
#' @value 1 latin number synthetic socio-economic classification category
#' @details
#' The function computes the French synthetic socio-economic class of a household by following the rules indicated in CNIS, 2019, p. 42 sq. The resulting classification distinguishes 7 groups, coded with latin numerals):
#'
#'   | Code  | Category
#'   |-------|-------------------------------------------------------------------------|
#'   | I     | Ménages à dominante cadre                                               |
#'   | II    | Ménages à dominante intermédiaire ou cadre                              |
#'   | III   | Ménages à dominante employée ou intermédiaire                           |
#'   | IV    | Ménages à dominante petit indépendant                                   |
#'   | V     | Ménages à dominante ouvrière                                            |
#'   | VI    | Ménages mono-actifs d’employé ou d’ouvrier                              |
#'   | VII   | Ménages inactifs                                                        |
#'
#' @md
#' Note1: the "Ménages inactifs" category (VII) excludes retirees who have worked but includes the unemployed who have never worked; the "cadre" category refers in this classification to the group of managers and senior professionals and, for the working population, to the category of managers of enterprises with more than 10 employees. The retired inactive persons are classified with the "petits indépendants", i.e. farmers, craftsmen and tradesmen).
#' Note2: the function allows coding from the detailed as well as from the intermediate CS classification. The latter includes codes 10, 32, 36, 41, 51, 61, 66, 73, 76, 81 and 82, instead of the corresponding detailed categories.
#' @seealso \code{\link[datatools]{gs_household}} for a detailed classification in 16 subgroups.
#' @example
#' gs_household(55, 12)
#' @export

# csind1 <- 33
# csind2 <- 33
gs_household <- function(csind1, csind2) {
  gsind1 <- csind_to_gsind(csind1)
  gsind2 <- csind_to_gsind(csind2)
  res <- gsind_to_cshousehold(gsind1, gsind2)
  res <- gsub(pattern = "-[ABC]", replacement = "", res)
  res <- gsub(pattern = "-[ABC]", replacement = "", res)
  return(gsmenage)
}





