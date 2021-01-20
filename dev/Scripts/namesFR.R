## namesFR

# Autres sources possibles
# http://opendata.paris.fr/explore/dataset/liste_des_prenoms_2004_a_2012/download/?format=csv
# http://www.mediafire.com/download/gzu4gfdbg68ufed/prenoms.tsv
# http://csu.hypotheses.org/files/2016/05/genre-pr%C3%A9noms-coulmont-hammou.xls

# Prénoms français source : Lexique
source <- read.csv("https://www.data.gouv.fr/s/resources/liste-de-prenoms/20141127-154433/Prenoms.csv", header=TRUE, sep=";", quote="", stringsAsFactors=FALSE)
namebase <- source
names(namebase) <- c("firstname", "gender", "language", "frequency")
namebase$firstname <- iconv(namebase$firstname, to='ASCII//TRANSLIT')
namebase$firstname <- gsub(" \\([0-9]\\)", "", namebase$firstname)
namebase <- namebase[!duplicated(namebase$firstname, fromLast=TRUE), ]
namebase$gender[namebase$gender==""] <- NA
namebase$gender <- factor(namebase$gender)
levels(namebase$gender) <- c("Female", "Female", "Male", "Male")
namebase$gender <- as.character(namebase$gender)
namebase.lexique <- namebase[, c("firstname", "gender")]

# Prénoms français source : Paris Open Data
source <- read.csv("http://opendata.paris.fr/explore/dataset/liste_des_prenoms_2004_a_2012/download/?format=csv", header=TRUE, sep=";", quote="", stringsAsFactors=FALSE, fileEncoding="UTF-8")
namebase <- source
names(namebase) <- c("firstname", "frequency", "gender", "year")
namebase$firstname <- iconv(namebase$firstname, to='ASCII//TRANSLIT')
namebase <- namebase[!duplicated(namebase$firstname, fromLast=TRUE), ]
namebase <- namebase[namebase$firstname!="", ]
namebase$gender[namebase$gender==""] <- NA
namebase$gender <- as.character(factor(namebase$gender, labels=c("Female", "Male", "Unisex")))
namebase$gender <- as.character(namebase$gender)
namebase <- namebase[order(namebase$firstname), ]
namebase$firstname <- stringr::str_to_lower(namebase$firstname)
namebase.paris <- namebase[, c("firstname", "gender")]

# Fusionner les deux bases
namebase <- rbind(namebase.lexique, namebase.paris)
namebase <- namebase[!duplicated(namebase$firstname, fromLast=TRUE), ]

name <- "Pierre"

# Fonction gender
gender <- function(names){
  gender <- lapply(names, function(name){
    name <- iconv(name, to='ASCII//TRANSLIT')
    firstname <- unlist(stringr::str_split(name, "[-, ]"))
    firstname <- stringr::str_to_lower(firstname[1])
    gender <- namebase$gender[which(namebase$firstname==firstname)]
    if(length(gender) == 0) gender <- NA
    return(gender)
  })
  return(unlist(gender))
}

# Fonction language
language <- function(names){
  language <- lapply(names, function(name){
    name <- iconv(name, to='ASCII//TRANSLIT')
    firstname <- unlist(stringr::str_split(name, " "))
    firstname <- stringr::str_to_lower(firstname[1])
    language <- namebase$language[which(namebase$firstname==firstname)]
    if(length(language) == 0) language <- NA
    return(language)
  })
  return(unlist(language))
}

# Fonction frequency
frequency <- function(names){
  frequency <- lapply(names, function(name){
    name <- iconv(name, to='ASCII//TRANSLIT')
    firstname <- unlist(stringr::str_split(name, " "))
    firstname <- stringr::str_to_lower(firstname[1])
    frequency <- namebase$frequency[which(namebase$firstname==firstname)]
    if(length(frequency) == 0) frequency <- NA
    return(frequency)
  })
  return(unlist(frequency))
}



# Test
prenoms <- c("Pierre", "Annick", "Noé", "Lilia", "Anouk", "Siméon", "Robinson", "Léonie")
gender(prenoms)
language(prenoms)
frequency(prenoms)

# Prénoms du meilleur ami dans l'enquête ados
sexesV1 <- gender(panel.rec$E128V1)
freq(sexesV1[panel.rec$E128V1!=""])
sexesV2 <- gender(panel.rec$E128V2)
freq(sexesV2[panel.rec$E128V2!=""])
sexesV3 <- gender(panel.rec$E128V3)
freq(sexesV3[panel.rec$E128V3!=""])
sexesV4 <- gender(panel.rec$E128V4)
freq(sexesV4[panel.rec$E128V4!=""])

library(xlsx)
export <- data.frame()
export <- rbind(export, cbind(ident=panel.rec$ident[is.na(sexesV1) & panel.rec$E128V1!=""], firstname=panel.rec$E128V1[is.na(sexesV1) & panel.rec$E128V1!=""], gender="", correc="", vague="V1"))
export <- rbind(export, cbind(ident=panel.rec$ident[is.na(sexesV2) & panel.rec$E128V2!=""], firstname=panel.rec$E128V2[is.na(sexesV2) & panel.rec$E128V2!=""], gender="", correc="", vague="V2"))
export <- rbind(export, cbind(ident=panel.rec$ident[is.na(sexesV3) & panel.rec$E128V3!=""], firstname=panel.rec$E128V3[is.na(sexesV3) & panel.rec$E128V3!=""], gender="", correc="", vague="V3"))
export <- rbind(export, cbind(ident=panel.rec$ident[is.na(sexesV4) & panel.rec$E128V4!=""], firstname=panel.rec$E128V4[is.na(sexesV4) & panel.rec$E128V4!=""], gender="", correc="", vague="V4"))
export$firstname <- as.character(export$firstname)
write.xlsx2(export[order(export$firstname), ], file="ownfirstnames_source.xlsx")


