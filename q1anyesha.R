test_dockets <- read.table("test_dockets.csv", header = TRUE,sep = ",")
district <- read.table("districts.csv", header = TRUE,sep = ",")


library(dplyr)
library(tidyverse)
library(ggplot2)
library(cluster)
library(factoextra)

head(test_dockets)
str(test_dockets)

head(district)
str(district)

cases_per_district <- as.data.frame(table(test_dockets$district))

test_dockets_pop <-left_join(test_dockets, district, by=c("district" = "district_number"))

filpd <- test_dockets_pop %>% group_by(district) %>% summarise(count = n())
filpd <- na.omit(filpd) %>% mutate(district_number = district)
filpdd <- full_join(filpd, district, by = "district_number")
filpdd <- filpdd %>% mutate(filing_rate = count / census_2010_population) %>% arrange(desc(filing_rate))


test_dockets_pop2 <-left_join(test_dockets_pop, filpdd, by=c("district" = "district_number"))



plot(filpdd$district, filpdd$filing_rate)

drop <- c("count","district_number", "district_name", "state", "census_2010_population")
simple = filpdd[,!(names(filpdd) %in% drop)]

distance <- get_dist(simple)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))


k2 <- kmeans(simple, centers = 2)
str(k2)

fviz_cluster(k2, data = simple)



simple %>%
  as_tibble() %>%
  mutate(cluster = k2$cluster,
         state = row.names(district)) %>%
  ggplot(aes(district, filing_rate, color = factor(cluster), label = district)) +
  geom_text()

test_dockets_pop2_sub= dplyr::select_if(test_dockets_pop2, is.numeric)



corr = cor(test_dockets_pop2_sub)
corr
library(corrplot)
corrplot(corr, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)