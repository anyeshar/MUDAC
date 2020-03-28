test_dockets <- read.csv(file.choose(), header=TRUE)
head(test_dockets)

districts_data <- read.csv(file.choose(), header=TRUE)
head(districts_data)

library(dplyr)
library(tidyverse)
left_join(test_dockets, districts_data, by=c("district" = "district_number"))
