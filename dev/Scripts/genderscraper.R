## genderscraper

# install.packages("xlsx")
library(xlsx)
# install.packages("rvest")
library(rvest)

# Répertoire de travail
setwd("~/Dropbox/Data/namesFR")
# Charger le fichier Excel
ownfirstnames <- read.xlsx2("ownfirstnames_source.xlsx", sheetIndex=1, stringsAsFactors=FALSE)

# Scraper les genres sur prenoms.com
i <- 1
name <- "MILLAN"
for (i in 1:nrow(ownfirstnames)) {
  print(i)
  if (ownfirstnames$gender[i]=="") {
    print(ownfirstnames$firstname[i])
    name <- unlist(stringr::str_split(ownfirstnames$firstname[i], "[-, ]"))[1]
    if (ownfirstnames$correc[i]!="") name <- unlist(stringr::str_split(ownfirstnames$correc[i], "[-, ]"))[1]
    url <- paste0("http://www.prenoms.com/prenom/", name, ".html")
    page <- read_html(url)
    gender <- paste(html_text(html_nodes(page, "span.Feminin")), html_text(html_nodes(page, "span.Masculin")))
    if (length(gender)==0) gender <- ""
    if (gender=="  masculin") gender <- "Male"
    if (gender==" fÃ©minin ") gender <- "Female"
    if (gender==" fÃ©minin  masculin") gender <- "Unisex"
    print(gender)
    ownfirstnames$gender[i] <- gender
  }
}
write.xlsx2(ownfirstnames, "ownfirstnames_result.xlsx")
