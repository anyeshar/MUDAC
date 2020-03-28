#test
library(tidyverse)
districts<-read_csv("~/Documents/MUDAC/Data/district_fips_code.csv")
distric_fips_code<-read_csv("~/Documents/MUDAC/Data/district_fips_code.csv")

train_dockets<-read_csv("~/Documents/MUDAC/Data/train_dockets.csv")
train_dockets$outcome<-factor(train_dockets$outcome, levels=c("Settled", "Dismissed", "Summary Judgment", "Verdict","Other"))
str(train_dockets)

train_other_motions<-read_csv("~/Documents/MUDAC/Data/test_other_motions.csv")
train_terminating_motions<-read.csv("~/Documents/MUDAC/Data/train_terminating_motions.csv")

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

dim(train_terminating_motions)

train_dissmiss<-train_terminating_motions %>% 
  filter(motion_type=="Motion to Dismiss") %>% left_join(train_dockets, by="mudac_id")

train_dissmiss %>% group_by(circuit, district, office, outcome) %>% summarise(n=n()) %>% ggplot(aes(x=outcome))+geom_bar()
# outcome              n
# <fct>               <int>
#   1 Settled         9629
# 2 Dismissed        13378
# 3 Summary Judgment  6127
# 4 Verdict           1559
# 5 Other             2040
# 6 NA                 580

train_dissmiss %>% names()

train_summary_judgement<-train_terminating_motions %>% 
  filter(motion_type=="Motion for Summary Judgment") %>% left_join(train_dockets, by="mudac_id")

train_summary_judgement %>% group_by(outcome) %>% summarise(n=n())
# outcome              n
# <fct>              <int>
# 1 Settled           8610
# 2 Dismissed         3757
# 3 Summary Judgment 14263
# 4 Verdict           2563
# 5 Other             1725
# 6 NA                 273


