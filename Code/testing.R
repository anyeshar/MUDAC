#test
library(tidyverse)
districts<-read_csv("~/Documents/MUDAC/Data/districts.csv")
districts[districts==-8]<-NA

district_fips_code<-read_csv("~/Documents/MUDAC/Data/district_fips_code.csv")
district_fips_code[district_fips_code==-8]<-NA

train_dockets<-read_csv("~/Documents/MUDAC/Data/train_dockets.csv")
train_dockets$outcome<-factor(train_dockets$outcome)
levels(train_dockets$outcome)[5]<-"Other"
train_dockets$outcome<-factor(train_dockets$outcome, levels=c("Dismissed", "Summary Judgment", "Settled", "Other"))
train_dockets[train_dockets==-8]<-NA

train_other_motions<-read_csv("~/Documents/MUDAC/Data/train_other_motions.csv")
train_other_motions[train_other_motions==-8]<-NA
train_other_motions$motion_type<-factor(train_other_motions$motion_type)
levels(train_other_motions$motion_type)[2:4]<-"Other Non-Terminating Motions"
levels(train_other_motions$motion_type)[4:5]<-"Other Non-Terminating Motions"

train_terminating_motions<-read.csv("~/Documents/MUDAC/Data/train_terminating_motions.csv")
train_terminating_motions[train_terminating_motions==-8]<-NA
levels(train_terminating_motions$motion_type)[1:2]<-"Other Terminating Motions"
levels(train_terminating_motions$motion_type)[3]<-"Other Terminating Motions"
levels(train_terminating_motions$motion_type)[4:5]<-"Other Terminating Motions"

test_terminating_motions<-read_csv("~/Documents/MUDAC/Data/test_terminating_motions.csv")
test_terminating_motions[test_terminating_motions==-8]<-NA
test_terminating_motions$motion_type<-factor(test_terminating_motions$motion_type)
levels(test_terminating_motions$motion_type)[1:2]<-"Other Terminating Motions"
levels(test_terminating_motions$motion_type)[3]<-"Other Terminating Motions"
levels(test_terminating_motions$motion_type)[4:5]<-"Other Terminating Motions"

test_dockets<-read_csv("~/Documents/MUDAC/Data/test_dockets.csv")
test_dockets[test_dockets==-8]<-NA
str(test_dockets)

test_other_motions<-read_csv("~/Documents/MUDAC/Data/test_other_motions.csv")
test_other_motions[test_other_motions==-8]<-NA
test_other_motions$motion_type<-factor(test_other_motions$motion_type)
levels(test_other_motions$motion_type)[2:4]<-"Other Non-Terminating Motions"
levels(test_other_motions$motion_type)[4:5]<-"Other Non-Terminating Motions"

###Investigate any relationships that may exist between the outcome of a case and the various terminating motions that are made######

# train_joined<-
#   train_terminating_motions %>% 
#   group_by(mudac_id, motion_type) %>% 
#   summarise(total_motion=n()) %>% 
#   pivot_wider(values_from=total_motion,
#               names_from=motion_type) %>% left_join(train_dockets, by="mudac_id")

train_terminating_motions %>% 
  group_by(mudac_id, motion_type) %>% select(mudac_id, motion_type) %>% 
  summarise(total_motion=n()) %>% filter(total_motion>=1) %>%
  left_join(train_dockets, by="mudac_id") %>% 
  ggplot(aes(x=outcome))+
    geom_bar(aes(fill=outcome))+
    facet_wrap(~motion_type)+
    coord_flip()+labs(title="Outcomes Related to Various Terminating Motions Filed")


summary_judgment<-train_terminating_motions %>% 
  group_by(mudac_id, motion_type) %>% select(mudac_id, motion_type) %>% 
  summarise(total_motion=n()) %>% filter(total_motion>=1) %>%
  left_join(train_dockets, by="mudac_id") %>% select(mudac_id, motion_type, total_motion, outcome) %>% 
  group_by(motion_type, outcome) %>% filter(motion_type=="Motion for Summary Judgment") %>% summarise(outcome_total=n())
# chi square goodness of fit
##Conduct goodness of fit test
#Set the probability values from the model in the same 
#color order as the data
modelp<- c(0.25, 0.25, 0.25, 0.25)
summary_judgment.goodtest<- chisq.test(summary_judgment[-5, 3], p = modelp)
summary_judgment.goodtest
#Get expected values for each category
summary_judgment.goodtest$expected
#Contribution from each category to test statistic
(summary_judgment.goodtest$residuals)^2 

dismiss<-train_terminating_motions %>% 
  group_by(mudac_id, motion_type) %>% select(mudac_id, motion_type) %>% 
  summarise(total_motion=n()) %>% filter(total_motion>=1) %>%
  left_join(train_dockets, by="mudac_id") %>% select(mudac_id, motion_type, total_motion, outcome) %>% 
  group_by(motion_type, outcome) %>% filter(motion_type=="Motion to Dismiss") %>% summarise(outcome_total=n())
# chi square goodness of fit
##Conduct goodness of fit test
#Set the probability values from the model in the same 
#color order as the data
modelp<- c(0.25, 0.25, 0.25, 0.25)
dismiss.goodtest<- chisq.test(dismiss[-5, 3], p = modelp)
dismiss.goodtest
#Get expected values for each category
dismiss.goodtest$expected
#Contribution from each category to test statistic
(dismiss.goodtest$residuals)^2

other_terminating_motions<-train_terminating_motions %>% 
  group_by(mudac_id, motion_type) %>% select(mudac_id, motion_type) %>% 
  summarise(total_motion=n()) %>% filter(total_motion>=1) %>%
  left_join(train_dockets, by="mudac_id") %>% select(mudac_id, motion_type, total_motion, outcome) %>% 
  group_by(motion_type, outcome) %>% filter(motion_type=="Other Terminating Motions") %>% summarise(outcome_total=n())
# chi square goodness of fit
##Conduct goodness of fit test
#Set the probability values from the model in the same 
#color order as the data
modelp<- c(0.25, 0.25, 0.25, 0.25)
other_terminating_motions.goodtest<- chisq.test(other_terminating_motions[-5, 3], p = modelp)
other_terminating_motions.goodtest
#Get expected values for each category
other_terminating_motions.goodtest$expected
#Contribution from each category to test statistic
(other_terminating_motions.goodtest$residuals)^2

###Investigate any relationships that may exist between the outcome of a case and the various non-terminating motions that are made#####
train_other_motions %>% 
  group_by(mudac_id, motion_type) %>% 
  select(mudac_id, motion_type) %>% 
  summarise(total_motion=n()) %>% 
  filter(total_motion>=1) %>% left_join(train_dockets, by = "mudac_id") %>% 
  select(mudac_id, motion_type, outcome) %>% 
  ggplot(aes(x=outcome))+
  geom_bar(aes(fill=outcome))+
  facet_wrap(~motion_type)+
  coord_flip()+labs(title="Outcomes Related to Various Non-Terminating Motions Filed")

summary_judgment<-test_terminating_motions %>% 
  group_by(mudac_id, motion_type) %>% select(mudac_id, motion_type) %>% 
  summarise(total_motion=n()) %>% filter(total_motion>=1) %>%
  left_join(train_dockets, by="mudac_id") %>% select(mudac_id, motion_type, total_motion, outcome) %>% 
  group_by(motion_type, outcome) %>% filter(motion_type=="Motion for Summary Judgment") %>% summarise(outcome_total=n())
# chi square goodness of fit
##Conduct goodness of fit test
#Set the probability values from the model in the same 
#color order as the data
modelp<- c(0.25, 0.25, 0.25, 0.25)
summary_judgment.goodtest<- chisq.test(summary_judgment[-5, 3], p = modelp)
summary_judgment.goodtest
#Get expected values for each category
summary_judgment.goodtest$expected
#Contribution from each category to test statistic
(summary_judgment.goodtest$residuals)^2 

dismiss<-test_terminating_motions %>% 
  group_by(mudac_id, motion_type) %>% select(mudac_id, motion_type) %>% 
  summarise(total_motion=n()) %>% filter(total_motion>=1) %>%
  left_join(train_dockets, by="mudac_id") %>% select(mudac_id, motion_type, total_motion, outcome) %>% 
  group_by(motion_type, outcome) %>% filter(motion_type=="Motion to Dismiss") %>% summarise(outcome_total=n())
# chi square goodness of fit
##Conduct goodness of fit test
#Set the probability values from the model in the same 
#color order as the data
modelp<- c(0.25, 0.25, 0.25, 0.25)
dismiss.goodtest<- chisq.test(dismiss[-5, 3], p = modelp)
dismiss.goodtest
#Get expected values for each category
dismiss.goodtest$expected
#Contribution from each category to test statistic
(dismiss.goodtest$residuals)^2

other_terminating_motions<-test_terminating_motions %>% 
  group_by(mudac_id, motion_type) %>% select(mudac_id, motion_type) %>% 
  summarise(total_motion=n()) %>% filter(total_motion>=1) %>%
  left_join(train_dockets, by="mudac_id") %>% select(mudac_id, motion_type, total_motion, outcome) %>% 
  group_by(motion_type, outcome) %>% filter(motion_type=="Other Terminating Motions") %>% summarise(outcome_total=n())
# chi square goodness of fit
##Conduct goodness of fit test
#Set the probability values from the model in the same 
#color order as the data
modelp<- c(0.25, 0.25, 0.25, 0.25)
other_terminating_motions.goodtest<- chisq.test(other_terminating_motions[-5, 3], p = modelp)
other_terminating_motions.goodtest
#Get expected values for each category
other_terminating_motions.goodtest$expected
#Contribution from each category to test statistic
(other_terminating_motions.goodtest$residuals)^2





############################
# train_other_motions %>% 
#   group_by(mudac_id, motion_type) %>% 
#   summarise(total_motion = n()) %>% 
#   filter(total_motion >= 1) %>%
#   left_join(train_dockets, by="mudac_id") %>% 
#   ggplot(aes(x=outcome)) +
#   geom_bar() +
#   facet_wrap(~motion_type) +
#   coord_flip()
# 
# train_other_motions %>% 
#   group_by(mudac_id, motion_type) %>% select(mudac_id, motion_type) %>% 
#   summarise(total_motion=n()) %>% filter(total_motion>=1) %>%
#   left_join(train_dockets, by="mudac_id") %>% dim()
#   
# train_other_motions %>% dim()
# train_dockets %>% dim()
# 
# str(train_other_motions)
# train_other_motions$motion_type<-factor(train_other_motions$motion_type)
# levels(train_other_motions$motion_type)
# 14681+4357





# train_joined %>% filter(`Motion to Dismiss`>=1) %>% 
#   ggplot(aes(x=district)) +
#   geom_bar(aes(fill=outcome))
# 
# train_joined %>% 
#   filter(`Motion to Dismiss` >= 1) %>% 
#   ggplot(aes(x=outcome)) +
#   geom_bar(aes(fill=outcome))
# 
# train_dissmiss<-train_terminating_motions %>% 
#   filter(motion_type=="Motion to Dismiss") %>% dim()+ left_join(train_dockets, by="mudac_id")
# 
# 
# train_dissmiss %>% group_by(outcome, issue_joined) %>% summarise(n=n())
# # outcome              n
# # <fct>               <int>
# #   1 Settled         9629
# # 2 Dismissed        13378
# # 3 Summary Judgment  6127
# # 4 Verdict           1559
# # 5 Other             2040
# # 6 NA                 580
# 
# train_dissmiss %>% dim()
# 
# train_summary_judgement<-train_terminating_motions %>% 
#   filter(motion_type=="Motion for Summary Judgment") %>% left_join(train_dockets, by="mudac_id")
# 
# train_summary_judgement %>% 
#   group_by(circuit, district, office, outcome) %>% 
#   summarise(n=n()) %>% 
#   ggplot(aes(x=outcome))+geom_bar()
# # outcome              n
# # <fct>              <int>
# # 1 Settled           8610
# # 2 Dismissed         3757
# # 3 Summary Judgment 14263
# # 4 Verdict           2563
# # 5 Other             1725
# # 6 NA                 273


demographics<-read_csv("~/Documents/MUDAC/Data/DEC_10_DP_DPDP1_with_ann.csv")
