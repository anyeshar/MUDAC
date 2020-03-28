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
  ggplot(aes(x=district, y=rate_per_district)) + geom_point(aes(color=factor(district))) +
  geom_text(aes(label=district)) + labs(x= "District Number", y = "Filing Rate (Filings per 100,000 people)", 
                                        title = "Scatter plot of Filing Rates for each District") +
  theme(legend.position = "none")

r_per_dis_data <- read.csv(file.choose(), header = TRUE)
head(r_per_dis_data)

model.districts <- lm(X~district_number, data=r_per_dis_data)
library(CARS)
anova(model.districts)

# H0: The districts all have the same filing rate
# H1: At least one district has a different filing rate
# F value = 4.2053
# p-value = 0.05137
# P-Value is significant at alpha = 0.10.
# Conclusion: We have moderately weak evidence that at least one of the districts has a 
# different filing rate than another district.

factor(r_per_dis_data$district_number)
##Pairwise Comparisons
##Fit model with aov() including only 1st parent employment 
model.distP <- aov(district_number~X, data=r_per_dis_data)
install.packages("car")
library(car)
resid(model.distP)

#residual plot
ggplot(data=r_per_dis_data, aes(x=X, y=resid(model.distP))) + geom_point(aes(color=district_number))

# There is no obvious pattern in the residual plot, so assumption of equal variances is okay.
# Based on the normal quantile plot, normaility assumption is reasonably met since there is no 
# obvious curvature in the plot and the points on the plot are
# all within the confidence bounds. 

# Since we only have moderately weak evidence of districts having different filing
# rates (significant at alpha=0.10), there are no pairwise comparisons significant at the 
# alpha = 0.05. If we wanted to separate districts as high or low, district numbers 45, 47, 48,
# 52, and 64 would be classified as high. These are the districts with filing rates greater than 40. 
# The rest of the districts are all closer together and it does not necessarily make sense to 
# classify any of them as having low filing rates. Districts 68 and 69 have the lowest filing rates
# and are the furthest from the bulk of the lower data, so we will classify them as low.

install.packages("CARS")

##cluster into high and low


