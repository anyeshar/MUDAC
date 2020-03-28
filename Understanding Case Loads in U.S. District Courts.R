test_dockets <- read.csv(file.choose(), header=TRUE)
head(test_dockets)

districts_data <- read.csv(file.choose(), header=TRUE)
head(districts_data)

library(dplyr)
library(tidyverse)
library(ggplot2)
test_dockets_pop <-left_join(test_dockets, districts_data, by=c("district" = "district_number"))

# Understanding Case Loads in U.S. District Courts

Rate_per_district <- test_dockets_pop %>% 
  group_by(district) %>% 
  distinct() %>% 
  mutate(rate_per_district = census_2010_population/100000) %>% 
  select(district, rate_per_district) %>% 
  arrange(district)

Rate_per_district %>% 
  ggplot(aes(x=district))+ geom_bar(aes(fill=factor(district))) + geom_text(stat='count', aes(label=..count..))

Rate_per_district %>% 
  ggplot(aes(x=district)) + geom_histogram(aes(fill=factor(district))) + geom_text(stat='count', aes(label=..count..))

Rate_per_district %>% 
  ggplot(aes(x=district, y=rate_per_district)) + geom_point(aes(fill=factor(district))) + geom_text(stat='count', aes(label=..count..))
