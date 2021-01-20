## code to prepare internal datasets goes here



# Libraries ----


library(dplyr)
library(readxl)
library(magrittr)
library(stringr)



# Data ----



# > SOUNDEX ----

# Import DM rules
dm_rules <- read_excel("data-raw/daitch-mokotoff_encoding_table_modif.xlsx") %>%
  arrange(desc(pattern)) %>% # order by decreasing alphabetical order to apply IE before EJ
  arrange(desc(nchar(pattern))) # Order by decreasing length of patterns, to make sure that longer patterns are searched and replaced before shorter ones

# Backup
save(dm_rules, file = "data-raw/dm_rules.RData")


# > FRANCE : Fichier des prénoms ----

# Load and unzip data file 2017
fn_fr <- read.table("data-raw/nat2017.txt", stringsAsFactors = FALSE, encoding = "UTF-8")

# Load and unzip data file 2019
# temp <- tempfile()
# download.file("https://www.insee.fr/fr/statistiques/fichier/2540004/nat2019_csv.zip",temp)
# fn_fr <- read.table(unz(temp, "nat2019.csv"), header = TRUE, sep = ";", stringsAsFactors = FALSE, encoding = "UTF-8")
# unlink(temp)

# Clean data
names(fn_fr) <- c("sex", "fn", "year", "count")

# Remove bad data
fn_fr <- fn_fr[fn_fr$sex %in% c("1", "2"), ]

# Variable class
fn_fr$sex <- factor(fn_fr$sex)
fn_fr$year <- as.integer(fn_fr$year)
fn_fr$count <- as.integer(fn_fr$count)

# Clean encoding
fn_fr$firstname <- str_replace_all(fn_fr$fn, c(
  "Ã‚" = "A",
  "Ã€" = "A",
  "Ã„" = "A",
  "Ã†" = "AE",
  "Ã‡" = "C",
  "Ãˆ" = "C",
  "Ã‹" = "E",
  "ÃŠ" = "E",
  "Ã‰" = "E",
  "ÃŽ" = "I",
  "Ã\u008f" = "I",
  "Ã”" = "O",
  "Ã–" = "O",
  "Ãœ" = "OE",
  "Ã›" = "U",
  "Ã™" = "U"
))

# Backup
save(fn_fr, file = "data-raw/fn_fr.RData")


# > INTERNATIONAL : behindthename.com ----

# Get number of pages to scrape from homepage
url <- "https://www.behindthename.com/names"
# page <- url %>% read_html %>% html_nodes(xpath = '//*[@id="div_pagination"]/div/a')
# page <- url %>% read_html %>% html_nodes(css = '#div_pagination > div > a')
# n <- page[length(page)-1] %>% html_text %>% as.integer
n <- 80
# Scrape pages
df <- NULL
for (i in 1:n) {
  print(i)
  page <- paste0(url, "/", i) %>% read_html
  name <- page %>% html_nodes("span.listname") %>% html_text
  gender <- page %>% html_nodes("span.listgender") %>% html_text
  usage <- page %>% html_nodes("span.listusage") %>% html_text
  df <- rbind(df, cbind(name, gender, usage))
}
fn_or <- as.data.frame(df)
names(fn_or) <- c("fn", "sex", "origin")

# Backup
save(fn_or, file = "data-raw/fn_or.RData")



# > Load and save data to R/sysdata.rda ----

# Load data
load("data-raw/dm_rules.RData")
load("data-raw/fn_fr.RData")
load("data-raw/fn_or.RData")

# Save data
usethis::use_data(dm_rules, fn_fr, fn_or, internal = TRUE, overwrite = TRUE)

# Check
check()

