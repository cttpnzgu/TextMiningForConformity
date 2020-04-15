# 1 http://fadn.gov.ru/press_centr/news-agency?year=2019
# 2 http://komitet2-4.km.duma.gov.ru/Novosti-Komiteta 
# 3 https://minkavkaz.gov.ru/ 
# 4 https://sovetnational.ru/information-support/news/ 
# 5 http://region.council.gov.ru/ 
library(tidyverse)
library(rvest)
library(stringr)

#####
# 2
rm(list = ls())
alllinks <- NULL
alltitles <- NULL
allpostdates <- NULL
for (nom_year in 2009:2019) {
 #nom_year <- 2012
print(nom_year)
webpage <- read_html(paste0("http://komitet2-4.km.duma.gov.ru/Novosti-Komiteta?filter[year]=", nom_year))

webpage %>%
  html_nodes(xpath = "//*[@id='effect']//div//div//div//div//i//a") %>% 
  html_attr("href") -> links

webpage %>%
  html_nodes(xpath = "//*[@id='effect']//div//div//div//div//i//a") %>% 
  html_text() -> titles

webpage %>%
  html_nodes(xpath = "//*[@id='effect']//div//div//div//div//b") %>% 
  html_text() -> postdate

alllinks <- append(alllinks, links)
alltitles <- append(alltitles, titles)
allpostdates <- append(allpostdates, postdate)
}
alllinks <- sapply(alllinks, function(x) paste0("http://komitet2-4.km.duma.gov.ru",x))
names(alllinks) <- NULL

text.article <- lapply(alllinks,
                       function(url){
                         url %>% read_html() %>% 
                           html_nodes(xpath = "//*[@id='effect']//div//div[4]") %>% 
                           html_text()})

text.article <- gsub("\r\n", " ", text.article)
text.article <- gsub("\t", " ", text.article)
text.article <- gsub("\\\\t", " ", text.article)
text.article <- gsub("\\n", " ", text.article)
text.article <- gsub("\\\\r\\\\n", " ", text.article)
text.article <- gsub("c\\(", " ", text.article)
text.article <- gsub("  ", " ", text.article)

text.article <- str_trim(string = text.article, side = "both")
df <- data.frame(text = text.article, stringsAsFactors = FALSE)
df <- data.frame(link = alllinks, title = alltitles, postdate = allpostdates, text = text.article, stringsAsFactors = FALSE)
saveRDS(object = df, file = "df-komitet2-4.RDS")

# text79 <- df$text[79]
# text79 <- str_sub(string = gsub("c\\(", "", text79), start = 1, end = 100)
# gsub('\\\"\"', "", text79, )
# 
# df$text[79]
# df$link[79]

#####
# 4 https://sovetnational.ru/information-support/news/?year=2019
rm(list = ls())
alllinks <- NULL
alltitles <- NULL
allpostdates <- NULL
#nom_year <- 2015
for (nom_year in 2015:2019) {
# https://sovetnational.ru/information-support/news/?cur_cc=3&year=2015&curPos=0
curpos <- 0
l <- 1
while (l>0) {
    webpage <- read_html(paste0("https://sovetnational.ru/information-support/news/?cur_cc=3&year=",
                                nom_year,
                                "&curPos=",
                                curpos))
    webpage %>%
      html_nodes(xpath = "//html//body//div[3]//div//div[2]//div//div[2]//a") %>% 
      html_attr("href") -> links   
    
    webpage %>%
      html_nodes(xpath = "//html//body//div[3]//div//div[2]//div//div[2]//a") %>% 
      html_text() -> titles
    
    webpage %>%
      html_nodes(xpath = "//html//body//div[3]//div//div[2]//div//div[2]//div[1]") %>% 
      html_text() -> postdate
    
    alllinks <- append(alllinks, links)
    alltitles <- append(alltitles, titles)
    allpostdates <- append(allpostdates, postdate)
    curpos <- curpos+10
    l <- length(links)
} }
allpostdates <- gsub("\\r\\n ", "", allpostdates)
allpostdates <- str_trim(string = allpostdates, side = "both")
alllinks <- sapply(alllinks, function(x) paste0("https://sovetnational.ru",x))

text.article <- lapply(alllinks,
                       function(url){
                         url %>% read_html() %>% 
                           html_nodes(xpath = "//html//body//div//div//div//div[1]/div[3]") %>% 
                           html_text()})

text.article <- gsub("\\r\\n", "", text.article)
text.article <- str_trim(string = text.article, side = "both")

df <- data.frame(link = alllinks, title = alltitles, postdate = allpostdates, text = text.article, stringsAsFactors = FALSE)
saveRDS(object = df, file = "df-sovetnational.ru.RDS")
