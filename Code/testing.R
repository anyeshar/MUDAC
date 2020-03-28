#test
library(tidyverse)
districts<-read_csv("~/Documents/MUDAC/Data/district_fips_code.csv")
districts[districts==-8]<-NA

district_fips_code<-read_csv("~/Documents/MUDAC/Data/district_fips_code.csv")
district_fips_code[district_fips_code==-8]<-NA

train_dockets<-read_csv("~/Documents/MUDAC/Data/train_dockets.csv")
train_dockets$outcome<-factor(train_dockets$outcome)
str(train_dockets)
train_dockets[train_dockets==-8]<-NA
dim(train_dockets)

train_other_motions<-read_csv("~/Documents/MUDAC/Data/test_other_motions.csv")
train_other_motions[train_other_motions==-8]<-NA

train_terminating_motions<-read.csv("~/Documents/MUDAC/Data/train_terminating_motions.csv")
train_terminating_motions[train_terminating_motions==-8]<-NA

test_terminating_motions<-read_csv("~/Documents/MUDAC/Data/test_terminating_motions.csv")
test_dockets<-read_csv("~/Documents/MUDAC/Data/train_dockets.csv")
test_other_motions<-read_csv("~/Documents/MUDAC/Data/test_other_motions.csv")

#1) whether or not the case was terminated via a motion to dismiss, 
#2) whether or not the case was terminated via a motion for summary judgement, and 
#3) whether or not the case was settled before going to trial

levels(train_terminating_motions$motion_type)
# [1] "Motion for Default Judgment"          "Motion for Judgment on the Pleadings" "Motion for Summary Judgment"         
# [4] "Motion to Consolidate"                "Motion to Dismiss"                    "Motion to Remand"                    
# [7] "Motion to Transfer Venue" 

train_joined<-train_terminating_motions %>% 
  group_by(mudac_id, motion_type) %>% 
  summarise(total_motion=n()) %>% 
  pivot_wider(values_from=total_motion,
              names_from=motion_type) %>% left_join(train_dockets, by="mudac_id")

train_terminating_motions %>% 
  group_by(mudac_id, motion_type) %>% 
  summarise(total_motion=n()) %>% filter(total_motion>=1) %>%
  left_join(train_dockets, by="mudac_id") %>% 
  ggplot(aes(x=outcome))+
    geom_bar(aes(fill=outcome))+
    facet_wrap(~motion_type)+
    coord_flip()+labs(title="Outcomes Related to Various Terminating Motions Filed")

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
