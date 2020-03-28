#test
library(tidyverse)
districts<-read_csv("~/Documents/MUDAC/Data/district_fips_code.csv")
districts[districts==-8]<-NA

district_fips_code<-read_csv("~/Documents/MUDAC/Data/district_fips_code.csv")
district_fips_code[district_fips_code==-8]<-NA

train_dockets<-read_csv("~/Documents/MUDAC/Data/train_dockets.csv")
train_dockets$outcome<-factor(train_dockets$outcome)
levels(train_dockets$outcome)[4]<-"Other"
train_dockets[train_dockets==-8]<-NA

train_other_motions<-read_csv("~/Documents/MUDAC/Data/test_other_motions.csv")
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

test_dockets<-read_csv("~/Documents/MUDAC/Data/train_dockets.csv")
test_dockets$outcome<-factor(test_dockets$outcome)
levels(test_dockets$outcome)[4]<-"Other"
test_dockets[train_dockets==-8]<-NA

test_other_motions<-read_csv("~/Documents/MUDAC/Data/test_other_motions.csv")
test_other_motions[test_other_motions==-8]<-NA
test_other_motions$motion_type<-factor(test_other_motions$motion_type)
levels(test_other_motions$motion_type)[2:4]<-"Other Non-Terminating Motions"
levels(test_other_motions$motion_type)[4:5]<-"Other Non-Terminating Motions"

#1) whether or not the case was terminated via a motion to dismiss, 
#2) whether or not the case was terminated via a motion for summary judgement, and 
#3) whether or not the case was settled before going to trial

levels(train_terminating_motions$motion_type)
# [1] "Motion for Default Judgment"          "Motion for Judgment on the Pleadings" "Motion for Summary Judgment"         
# [4] "Motion to Consolidate"                "Motion to Dismiss"                    "Motion to Remand"                    
# [7] "Motion to Transfer Venue" 

train_joined<-
  train_terminating_motions %>% 
  group_by(mudac_id, motion_type) %>% 
  summarise(total_motion=n()) %>% 
  pivot_wider(values_from=total_motion,
              names_from=motion_type) %>% left_join(train_dockets, by="mudac_id")

train_terminating_motions %>% 
  group_by(mudac_id, motion_type) %>% select(mudac_id, motion_type) %>% 
  summarise(total_motion=n()) %>% filter(total_motion>=1) %>%
  left_join(train_dockets, by="mudac_id") %>% 
  ggplot(aes(x=outcome))+
    geom_bar(aes(fill=outcome))+
    facet_wrap(~motion_type)+
    coord_flip()+labs(title="Outcomes Related to Various Terminating Motions Filed")

default_judgement<-train_terminating_motions %>% 
  group_by(mudac_id, motion_type) %>% select(mudac_id, motion_type) %>% 
  summarise(total_motion=n()) %>% filter(total_motion>=1) %>%
  left_join(train_dockets, by="mudac_id") %>% select(mudac_id, motion_type, total_motion, outcome) %>% 
  group_by(motion_type, outcome) %>% filter(motion_type=="Motion for Default Judgment") %>% summarise(outcome_total=n())
default_judgement[-5, 3]
# chi square goodness of fit
##Conduct goodness of fit test
#Set the probability values from the model in the same 
#color order as the data
modelp<- c(0.25, 0.25, 0.25, 0.25)
default_judgement.goodtest<- chisq.test(default_judgement[-c(5,6), 3], p = modelp)
default_judgement.goodtest
#Get expected values for each category
default_judgement.goodtest$expected
#Contribution from each category to test statistic
(default_judgement.goodtest$residuals)^2 


judgement_pleadings<-train_terminating_motions %>% 
  group_by(mudac_id, motion_type) %>% select(mudac_id, motion_type) %>% 
  summarise(total_motion=n()) %>% filter(total_motion>=1) %>%
  left_join(train_dockets, by="mudac_id") %>% select(mudac_id, motion_type, total_motion, outcome) %>% 
  group_by(motion_type, outcome) %>% filter(motion_type=="Motion for Judgment on the Pleadings") %>% summarise(outcome_total=n())
# chi square goodness of fit
##Conduct goodness of fit test
#Set the probability values from the model in the same 
#color order as the data
modelp<- c(0.25, 0.25, 0.25, 0.25)
judgement_pleadings.goodtest<- chisq.test(judgement_pleadings[-c(5,6), 3], p = modelp)
judgement_pleadings.goodtest
#Get expected values for each category
judgement_pleadings.goodtest$expected
#Contribution from each category to test statistic
(judgement_pleadings.goodtest$residuals)^2 


summary_judgement<-train_terminating_motions %>% 
  group_by(mudac_id, motion_type) %>% select(mudac_id, motion_type) %>% 
  summarise(total_motion=n()) %>% filter(total_motion>=1) %>%
  left_join(train_dockets, by="mudac_id") %>% select(mudac_id, motion_type, total_motion, outcome) %>% 
  group_by(motion_type, outcome) %>% filter(motion_type=="Motion for Summary Judgment") %>% summarise(outcome_total=n())
# chi square goodness of fit
##Conduct goodness of fit test
#Set the probability values from the model in the same 
#color order as the data
modelp<- c(0.25, 0.25, 0.25, 0.25)
summary_judgement.goodtest<- chisq.test(summary_judgement[-c(5,6), 3], p = modelp)
summary_judgement.goodtest
#Get expected values for each category
summary_judgement.goodtest$expected
#Contribution from each category to test statistic
(summary_judgement.goodtest$residuals)^2 

consolidate<-train_terminating_motions %>% 
  group_by(mudac_id, motion_type) %>% select(mudac_id, motion_type) %>% 
  summarise(total_motion=n()) %>% filter(total_motion>=1) %>%
  left_join(train_dockets, by="mudac_id") %>% select(mudac_id, motion_type, total_motion, outcome) %>% 
  group_by(motion_type, outcome) %>% filter(motion_type=="Motion to Consolidate") %>% summarise(outcome_total=n())
# chi square goodness of fit
##Conduct goodness of fit test
#Set the probability values from the model in the same 
#color order as the data
modelp<- c(0.25, 0.25, 0.25, 0.25)
consolidate.goodtest<- chisq.test(consolidate[-c(5,6), 3], p = modelp)
consolidate.goodtest
#Get expected values for each category
consolidate.goodtest$expected
#Contribution from each category to test statistic
(consolidate.goodtest$residuals)^2

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
dismiss.goodtest<- chisq.test(dismiss[-c(5,6), 3], p = modelp)
dismiss.goodtest
#Get expected values for each category
dismiss.goodtest$expected
#Contribution from each category to test statistic
(dismiss.goodtest$residuals)^2

remand<-train_terminating_motions %>% 
  group_by(mudac_id, motion_type) %>% select(mudac_id, motion_type) %>% 
  summarise(total_motion=n()) %>% filter(total_motion>=1) %>%
  left_join(train_dockets, by="mudac_id") %>% select(mudac_id, motion_type, total_motion, outcome) %>% 
  group_by(motion_type, outcome) %>% filter(motion_type=="Motion to Remand") %>% summarise(outcome_total=n())
# chi square goodness of fit
##Conduct goodness of fit test
#Set the probability values from the model in the same 
#color order as the data
modelp<- c(0.2, 0.2, 0.2, 0.2)
remand.goodtest<- chisq.test(dismiss[-c(5,6), 3], p = modelp)
remand.goodtest
#Get expected values for each category
remand.goodtest$expected
#Contribution from each category to test statistic
(remand.goodtest$residuals)^2

transfer_venue<-train_terminating_motions %>% 
  group_by(mudac_id, motion_type) %>% select(mudac_id, motion_type) %>% 
  summarise(total_motion=n()) %>% filter(total_motion>=1) %>%
  left_join(train_dockets, by="mudac_id") %>% select(mudac_id, motion_type, total_motion, outcome) %>% 
  group_by(motion_type, outcome) %>% filter(motion_type=="Motion to Transfer Venue") %>% summarise(outcome_total=n())
# chi square goodness of fit
##Conduct goodness of fit test
#Set the probability values from the model in the same 
#color order as the data
modelp<- c(0.25, 0.25, 0.25, 0.25)
transfer_venue.goodtest<- chisq.test(dismiss[-c(5,6), 3], p = modelp)
transfer_venue.goodtest
#Get expected values for each category
transfer_venue.goodtest$expected
#Contribution from each category to test statistic
(remand.goodtest$residuals)^2



train_other_motions %>% 
  group_by(mudac_id, motion_type) %>% 
  summarise(total_motion = n()) %>% 
  filter(total_motion >= 1) %>%
  left_join(train_dockets, by="mudac_id") %>% 
  ggplot(aes(x=outcome)) +
  geom_bar() +
  facet_wrap(~motion_type) +
  coord_flip()

train_other_motions %>% 
  group_by(mudac_id, motion_type) %>% select(mudac_id, motion_type) %>% 
  summarise(total_motion=n()) %>% filter(total_motion>=1) %>%
  left_join(train_dockets, by="mudac_id") %>% dim()
  
train_other_motions %>% dim()
train_dockets %>% dim()

str(train_other_motions)
train_other_motions$motion_type<-factor(train_other_motions$motion_type)
levels(train_other_motions$motion_type)
14681+43570





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
