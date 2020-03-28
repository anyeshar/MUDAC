test_dockets <- read.csv(file.choose(), header=TRUE)
head(test_dockets)

districts_data <- read.csv(file.choose(), header=TRUE)
head(districts_data)

library(dplyr)
library(tidyverse)
library(ggplot2)
test_dockets_pop <-left_join(test_dockets, districts_data, by=c("district" = "district_number"))

# Understanding Case Loads in U.S. District Courts

Rate_per_district <-test_dockets_pop %>% 
  group_by(district) %>% 
  mutate(count = n(), filing_rate = (count /(census_2010_population))) %>% 
  select(district, filing_rate, count)

Rate_per_district %>% 
  ggplot(aes(x=district, y=filing_rate)) + geom_point(aes(color=factor(district))) +
  geom_text(aes(label=district)) + labs(x= "District Number", y = "Filing Rate", 
                                        title = "Scatter plot of Filing Rates for each District") +
  theme(legend.position = "none")

write.csv(Rate_per_district, "r_per_d2.csv")

# Rate_per_district <- test_dockets_pop %>% 
#   group_by(district) %>% 
#   distinct() %>% 
#   mutate(rate_per_district = census_2010_population/100000) %>% 
#   select(district, rate_per_district) %>% 
#   arrange(district)
# 
# 
# Rate_per_district %>% 
#   ggplot(aes(x=district, y=rate_per_district)) + geom_point(aes(color=factor(district))) +
#   geom_text(aes(label=district)) + labs(x= "District Number", y = "Filing Rate (Filings per 100,000 people)", 
#                                         title = "Scatter plot of Filing Rates for each District") +
#   theme(legend.position = "none")

r_per_dis_data <- read.csv(file.choose(), header = TRUE)
head(r_per_dis_data)

model.districts <- lm(filing_rate~district, data=r_per_dis_data)
library(CARS)
anova(model.districts)

# H0: The districts all have the same filing rate
# H1: At least one district has a different filing rate
# F value = 2.475
# p-value = 0.1288
# P-Value is not significant at alpha = 0.05 or 0.10.
# Conclusion: We do not have evidence that at least one of the districts has a 
# different filing rate than another district.

factor(r_per_dis_data$district)
##Pairwise Comparisons
##Fit model with aov() including only 1st parent employment 
model.distP <- aov(district~filing_rate, data=r_per_dis_data)
install.packages("car")
library(car)
resid(model.distP)

#residual plot
ggplot(data=r_per_dis_data, aes(x=filing_rate, y=resid(model.distP))) + 
  geom_point(aes(color=district)) + geom_line(y=0) + labs(title="Residual Plot of Filing Rate")

# There is no obvious pattern in the residual plot, so assumption of equal variances is okay.
# Based on the normal quantile plot, normaility assumption is reasonably met since there is no 
# obvious curvature in the plot and the points on the plot are
# all within the confidence bounds. 

# Since we do not have evidence of different districts having significantly different filing rates,
# it would not be appropriate to conduct a pairwise comparisons test. 
# If we wanted to separate districts as high or low, district numbers 50, 51, 52, 55, 60
# would be classified as high. These are the districts with filing rates greater than 0.0002. 
# The rest of the districts are all closer together and it does not necessarily make sense to 
# classify any of them as having low filing rates. Districts 58 and 68 have the lowest filing rates
# and are the furthest from the bulk of the lower data, so we will classify them as low.


