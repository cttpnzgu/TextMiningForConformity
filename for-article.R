library(tidyverse)
library(lsa)
library(ggplot2)
library(plotly)
library(INBOtheme)
library(xtable)
library(dplyr)
library(knitr)
library(plyr)
library(Cairo)
library(lubridate)

rm(list = ls())
df.cosine.original <- readRDS("df.cosine.original.RDS")
df.cosine.original$feature <- "original"
df.cosine.expert <- readRDS("df.cosine.expert.RDS")
df.cosine.expert$feature <- "expert"
df <- rbind(df.cosine.original, df.cosine.expert)

df0 <- readRDS("df0-all-news-2.RDS")
df0 <- df0[!duplicated(df0$link),]
df <- left_join(x = df, y = df0, by = "link")

df$similarity[is.na(df$similarity)] <- 0

df %>% group_by(feature,item) %>% 
  mutate(q1 = quantile(similarity,probs=0.25),
       q3 = quantile(similarity,probs=0.75),
       iqr = IQR(similarity),
       outlier = if_else((q1-1.5*iqr)>similarity | (q3+1.5*iqr)<similarity, TRUE, FALSE)) %>% ungroup()-> df

library(openxlsx)
write.xlsx(x = df, file = "df.xlsx")
# for LATEX 
# Количество аномально похожих новостей на тексты направлений стратегии на сайтах 
table(df %>% filter(outlier, feature=="expert") %>% select(item, site)) %>% xtable()
    table(df %>% filter(outlier, feature=="expert") %>% select(item, site)) -> table.1

# tbl <-ftable(table.1,row.vars=c(1,2))
# xftbl <- xtableFtable(tbl, method = "compact")
# print.xtableFtable(xftbl, booktabs = T) 
#cbind(table.1, apply(table.1, 1, sum))
#table(df %>% filter(outlier) %>% select(item, site)) %>% kable()

#1
#pdf("article/figures/Rplot01.pdf", width = 3.5, height = 3.5) 
#CairoPDF(file = "article/figures/Rplot01.pdf", width = 3.5, height = 3.5)
# !!! select portrait 3.5 x 3
df %>% filter(feature=="expert") %>% ggplot(aes(x = item,
                                                                    y = similarity,
                                                                    label = site))+
   geom_boxplot(notch = TRUE, position = position_dodge(width = 0.9), width = 0.7, 
                alpha = 1, shape = 2, outlier.colour = "red", outlier.shape = 19, outlier.size = 1, outlier.alpha = 0.5)+
  #geom_jitter(size = 0.1, width = 0.2, alpha = 0.5)+
  facet_wrap(~site, nrow = 3)+
  theme_elsevier(base_size = 10, base_family = "")
#dev.off() 

# 2
#pdf("Rplot02.pdf") 
#df$newx = str_wrap(df$x, width = 10)
levels(factor(df$site))
#library(scales)
#cols <- hue_pal()(3)
df %>% filter(outlier) %>% transform(site = replace(site, str_detect(site, "komitet2-4.km.duma.gov.ru"), "komitet2-4.km. \n duma.gov.ru")) %>% 
  transform(site=str_wrap(site, 10)) %>%  ggplot(aes(x = item,
                                      fill=similarity))+
  geom_dotplot()+
    facet_grid(feature~site)+
    theme_elsevier(base_size = 10, base_family = "")+
  theme(legend.position = "none", axis.text.y=element_blank())
  #theme(legend.position = "bottom",  legend.text=element_text(size=10), axis.text.y=element_blank())
 #dev.off() 

  
# 3
#pdf("Rplot03.pdf") 
df %>% filter(outlier) %>% filter(feature=="expert") %>% ggplot(aes(x = item,
                  y = similarity,
                  label = site))+
  # geom_boxplot(position = position_dodge(width = 0.9), width = 0.7, 
  #              alpha = 1, shape = 2)+
    geom_jitter(width = 0.25, alpha = 0.7, size = 1)+
  facet_wrap(~site, nrow = 3)+
  theme_elsevier(base_size = 10, base_family = "")
#dev.off() 

# 4 
#pdf("Rplot04.pdf") 
df %>% filter(outlier) %>% filter(feature=="expert") %>% transform(postdate=as.Date(postdate)) %>%
  ggplot(aes(x = postdate,
             y = similarity))+
  geom_point(alpha = 0.7, size = 1)+
  stat_smooth()+
 facet_wrap(~site, ncol = 1)+
  scale_x_date(date_breaks="1 month", date_labels="%m-%Y")+
   theme_elsevier(base_size = 10, base_family = "") +
  theme(axis.text.x = element_text(angle = 90, size=8))
#dev.off() 

# 5
df %>% filter(outlier) %>% filter(feature=="expert") %>%  transform(site = replace(site, str_detect(site, "komitet2-4.km.duma.gov.ru"), "komitet2-4.km. \n duma.gov.ru")) %>% 
  transform(site=str_wrap(site, 10)) %>% mutate(month = factor(month(postdate, label = FALSE, abbr = TRUE))) %>% 
ggplot(aes(x = month,
           fill = site,
           label = item))+
  geom_dotplot()+
  facet_wrap(~item, ncol = 4)+
  theme_elsevier(base_size = 10, base_family = "")+
 theme(legend.position = "bottom",  legend.text=element_text(size=7), axis.text.y=element_blank())
#theme(legend.position = "bottom", axis.text.y=element_blank())

#############
df %>% filter(outlier) %>% filter(feature=="expert") %>%  transform(site = replace(site, str_detect(site, "komitet2-4.km.duma.gov.ru"), "komitet2-4.km. \n duma.gov.ru")) %>% 
  transform(site=str_wrap(site, 10)) %>% mutate(month = factor(month(postdate, label = FALSE, abbr = TRUE))) %>% 
  ggplot(aes(x = month,
             fill = site,
             colour = site,
             label = item))+
  geom_dotplot(dotsize = 1.5)+
  facet_wrap(~item, ncol = 3)+
  theme_elsevier(base_size = 10, base_family = "")+
  theme(legend.position = "bottom",  legend.text=element_text(size=7), axis.text.y=element_blank())

df %>% filter(outlier) %>% filter(feature=="expert") %>%  transform(site = replace(site, str_detect(site, "komitet2-4.km.duma.gov.ru"), "komitet2-4.km. \n duma.gov.ru")) %>% 
  transform(site=str_wrap(site, 10)) %>% mutate(month = factor(month(postdate, label = FALSE, abbr = TRUE))) %>% 
  ggplot(aes(x = month,
             fill = site,
             label = item))+
  geom_dotplot()+
  facet_grid(item~.)

# Таблица по месяцам
df %>% filter(outlier) %>% filter(feature=="expert") %>% mutate(month = factor(month(postdate, label = FALSE, abbr = TRUE))) %>% select(item, month) %>% table() %>% xtable()

df %>% filter(outlier) %>% filter(feature=="expert") %>% mutate(month = factor(month(postdate, label = FALSE, abbr = TRUE))) %>% 
  ggplot(aes(x = month,
             y = similarity,
             fill = site,
             label = item))+
  geom_dotplot()+
  theme_elsevier(base_size = 18, base_family = "")

df %>% filter(outlier) %>% filter(feature=="expert") %>% mutate(month = factor(month(postdate, label = FALSE, abbr = TRUE))) %>% 
  ggplot(aes(x = month,
             y = item))+
  geom_jitter(aes(size = similarity), width = 0.1, alpha = 0.7)+
  facet_wrap(~site)+
  theme_elsevier(base_size = 18, base_family = "")


df %>% filter(outlier) %>% filter(feature=="expert") %>% mutate(month = factor(month(postdate, label = FALSE, abbr = TRUE))) %>% 
  ggplot(aes(x = item,
             y = month))+
  geom_jitter(aes(size = similarity), width = 0.1, alpha = 0.7)+
  facet_wrap(~site)+
  theme_elsevier(base_size = 18, base_family = "")

#tidy_fadn_tf_idf$month <- month(tidy_fadn_tf_idf$dates)

# df %>% ggplot(aes(x = item,
#                        y = similarity,
#                        colour=feature,
#                        label = site))+
#   geom_boxplot(position = position_dodge(width = 0.7), width = 0.5, 
#                alpha = 0.5)


 df %>% filter(outlier) %>% ggplot(aes(x = similarity, fill = feature))+
  geom_density(position = 'identity', alpha = 0.3)+
  facet_grid(~item)
print(g)

g <- df %>% ggplot(aes(x = similarity, fill = feature))+
  geom_histogram(position = 'identity', alpha = 0.5, bins = 30)+
  facet_grid(~item)
print(g)
##########
g <- df %>% ggplot(aes(x = factor(item),
                       y = similarity,
                       colour=feature,
                       label = site))+
  geom_boxplot(position = position_dodge(width = 0.7), width = 0.5, 
               alpha = 0.5)+
  facet_wrap(~site)
print(g)
ggplotly(g)

g <- df %>% ggplot(aes(x = item,
                       y = similarity,
                       colour=feature))+
  geom_jitter()+
  facet_wrap(~site)
ggplotly(g)

g <- df %>% filter(outlier) %>% ggplot(aes(x = similarity, fill = feature))+
  geom_density(position = 'identity', alpha = 0.3)+
  facet_grid(site~item)
ggplotly(g)

  g <- df %>% filter(outlier) %>% ggplot(aes(x = similarity, fill = feature))+
    geom_density(position = 'identity', alpha = 0.3, adjust = 1)+
    facet_grid(item~site)
ggplotly(g)
###########
# 6
library(ggTimeSeries)
library(lubridate)
library(tidyverse)
df %>% filter(outlier) %>% filter(feature=="expert") %>% transform(postdate=date(postdate)) -> df.calendar

ggplot_calendar_heatmap(dtDateValue = df.calendar, cDateColumnName = "postdate", cValueColumnName = "similarity")

  ggplot(aes(x = month,
             
             fill = site,
             label = item))+
  geom_dotplot()+
  facet_wrap(~item, ncol = 3)+
  theme_elsevier(base_size = 14, base_family = "")+theme(legend.position = "bottom",  legend.text=element_text(size=12))