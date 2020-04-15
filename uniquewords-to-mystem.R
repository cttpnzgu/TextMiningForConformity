rm(list = ls())

mystem <- function(doc) {
  library(stringr)
  sdoc <- system('./mystem -nl -e utf-8 ', intern=T, input=doc)
  # При получении нескольких вариантов mystem разделяет их
  # вертикальной чертой. Удалим черту и варианты.
  sdoc <- str_replace(sdoc, '\\|.*$', '')
  # Если mystem сомневается в результате, он добавляет знак вопроса. Удаляем.
  sdoc <- str_replace(sdoc, '\\?', '')
  sdoc <- paste(sdoc, collapse=" ")
  attributes(sdoc) <- attributes(doc)
  sdoc
}
uniquewords <- readRDS("~/rdata/reva/uniquewords.RDS")
mystemuniquewords <- uniquewords
for (w in 1:length(uniquewords)) {
  mystemuniquewords[w] <- mystem(uniquewords[w])
  print(paste(w, uniquewords[w], mystemuniquewords[w]))
  
}
#saveRDS(object = mystemuniquewords, "mystemuniquewords.RDS")
unique_mystemuniquewords <- unique(mystemuniquewords)
      