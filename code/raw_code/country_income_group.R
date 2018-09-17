library(readxl)
wb <- read_excel("./data/raw_data/country_classification.xls", "wb")
crosswalk <- read_excel("./data/raw_data/country_classification.xls", "crosswalk")

country_codes <- wb %>% 
    left_join(crosswalk, by = "code", `copy`=TRUE)


user_ctry <- users %>% 
    left_join(country_codes, by = "country_cd", `copy`=TRUE)


jdf <- prop.table(table(user_ctry$income_group))

par(mar=c(1,1,1,1))
library(RColorBrewer)
coul <- brewer.pal(5, "OrRd") 
pie(jdf, col = coul , main="Share of learners from different country income groups") 


## Average cost of programs in data science
# https://www.bestmastersdegrees.com/top/affordable-online-masters-statistics-data-analytics
mean(c(49064, 45988, 34500, 27898, 19832, 19346, 18810, 17219, 16278, 15840, 14076, 
       11688, 11357, 11304, 10532, 10224, 10124, 9900, 9743, 9506, 9172, 9116, 9062, 
       8656, 8568, 8514, 8222, 7637, 7488, 7200))
