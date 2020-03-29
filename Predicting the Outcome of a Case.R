#Prediction Model

# --------Package Installs---------
install.packages("caret")
install.packages("ranger")
install.packages("RANN")
install.packages('kernlab')
# --------Loading Libraries---------
library(tidyverse)
library(ggplot2)
library(caret)
library(randomForest)
library(ranger)
library(RANN)
library(kernlab)
# --------Data Prepping---------

# Load data sets in
train_dockets <- read_csv("~/MUDAC/Data/train_dockets.csv") 
train_terminating_motions <- read_csv("~/MUDAC/Data/train_terminating_motions.csv")
train_other_motions <- read_csv("~/MUDAC/Data/train_other_motions.csv")
test_dockets <- read_csv("~/MUDAC/Data/test_dockets.csv")
test_terminating_motions <- read_csv("~/MUDAC/Data/test_terminating_motions.csv")
test_other_motions <-read_csv("~/MUDAC/Data/test_other_motions.csv")

# Combine the data sets together
half_docket <- train_terminating_motions %>%
  group_by(mudac_id, motion_type) %>%
  summarise(total_motion=n()) %>%
  pivot_wider(values_from=total_motion,
              names_from=motion_type) %>% right_join(train_dockets, by="mudac_id")

half_test <- test_terminating_motions %>%
  group_by(mudac_id, motion_type) %>%
  summarise(total_motion=n()) %>%
  pivot_wider(values_from=total_motion,
              names_from=motion_type) %>% right_join(test_dockets, by="mudac_id")

# Add pp
half_docket<-train_terminating_motions %>%
  group_by(mudac_id, motion_type) %>%
  summarise(mean_proceeding_percentile = mean(proceeding_precentile)) %>%
  pivot_wider(values_from=mean_proceeding_percentile,
              names_from=motion_type, names_prefix = "pp") %>% 
  right_join(half_docket, by= "mudac_id")

half_test<-test_terminating_motions %>%
  group_by(mudac_id, motion_type) %>%
  summarise(mean_proceeding_percentile = mean(proceeding_precentile)) %>%
  pivot_wider(values_from=mean_proceeding_percentile,
              names_from=motion_type, names_prefix = "pp") %>% 
  right_join(half_test, by= "mudac_id")

# Add other motions
complete_docket <- train_other_motions %>%
  group_by(mudac_id, motion_type) %>%
  summarise(total_motion=n()) %>%
  pivot_wider(values_from=total_motion,
              names_from=motion_type) %>% right_join(half_docket, by="mudac_id")

complete_test <- test_other_motions %>%
  group_by(mudac_id, motion_type) %>%
  summarise(total_motion=n()) %>%
  pivot_wider(values_from=total_motion,
              names_from=motion_type) %>% right_join(half_test, by="mudac_id")

# Remove unwanted variables
train_selection <- complete_docket %>% 
  select(summary_judgment, settled, mudac_id,
         after_ij_entry_count, days_opened, total_entry_count, filers_county,
         before_ij_entry_count, attorneys_listed, year_filed, district,
         `Motion to Dismiss`, `Motion for Summary Judgment`, `Motion in Limine`,
         `ppMotion to Consolidate`, `ppMotion for Summary Judgment`)

test_selection <- complete_test %>% 
  select(after_ij_entry_count, days_opened, total_entry_count, filers_county,
         before_ij_entry_count, attorneys_listed, year_filed, district,
         `Motion to Dismiss`, `Motion for Summary Judgment`, `Motion in Limine`,
         `ppMotion to Consolidate`, `ppMotion for Summary Judgment`, mudac_id)

# Ungroup data sets
train_selection <- ungroup(train_selection)
test_selection <- ungroup(test_selection)

# Remove MUDAC ID
train_selection <- train_selection %>% select(-mudac_id)
test_selection <- test_selection %>% select(-mudac_id)

# Replace No motions with 0
train_selection[is.na(train_selection)] <- 0 
test_selection[is.na(test_selection)] <- 0 

# Creates Dummy Variables
dummy <- dummyVars("~ .",
                   data = train_selection,
                   fullRank = T) 

dummyTest <- dummyVars("~ .",
                       data = test_selection,
                       fullRank = T) 

# New data set with dummy variables replacing categorical variables
transformed <- data.frame(predict(dummy, newdata = train_selection))

transformedTest <- data.frame(predict(dummyTest, newdata = test_selection))

# Create new data frames for SJ (summary_judgment) and SET (settled)

transformedSJ <- transformed %>% select(-settled)
transformedSET <- transformed %>% select(-summary_judgment)




# --------Modeling Start---------

# Partition the data set to make it more reasonable to deal with
inTrainingSJ <- createDataPartition(transformedSJ$summary_judgment, p = .02, list = FALSE)
trainingSJ <- transformedSJ[ inTrainingSJ,]
testingSJ  <- transformedSJ[-inTrainingSJ,]

inTrainingSET <- createDataPartition(transformedSET$settled, p = .02, list = FALSE)
trainingSET <- transformedSET[ inTrainingSET,]
testingSET  <- transformedSET[-inTrainingSET,]

#Change to factor so we can run random forest
trainingSJ$summary_judgment <- as.factor(trainingSJ$summary_judgment) 

trainingSET$settled <- as.factor(trainingSET$settled)

# Run basic model
rfFitSJ <- train(summary_judgment~., 
                data = trainingSJ, 
                method = "rf")

rfFitSET <- train(settled~.,
                  data = trainingSET,
                  method = "rf")
# Output model
rfFitSJ
rfFitSET

# Predict the testing set and output the results
final_SJPrediction <- predict(rfFitSJ, transformedTest)
head(final_SJPrediction)
summary(final_SJPrediction)

final_SETPrediction <- predict(rfFitSET, transformedTest)
head(final_SETPrediction)
summary(final_SETPrediction)

# Produce a table of probabilities
final_probSJ <- predict(rfFitSJ, transformedTest, type="prob")
head(final_probSJ)
          #write.csv(final_probSJ, "Probability of SJ.csv")

final_probSET <- predict(rfFitSET, transformedTest, type="prob")
head(final_probSET)

# Produce a confusion matrix
    # confusionMatrix(final_probSJ, as.factor(testingSJ$summary_judgment))
    # confusionMatrix(final_probSET, as.factor(testingSET$settled))

# Provide a list of the most important variables
varImp(rfFitSJ)
varImp(rfFitSET)

# -------Other Random Stuff--------
gbmFit1 <- train(summary_judgment~., 
                 data = training, 
                 method = "gbm")
gbmFit1
ggplot(gbmFit1)

gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9), 
                        n.trees = (1:30)*50, 
                        shrinkage = 0.1,
                        n.minobsinnode = 20)

gbmFit2 <- train(summary_judgment~., 
                 data = training, 
                 tuneGrid = gbmGrid, 
                 method = "gbm")
gbmFit2
ggplot(gbmFit2)

svmFit1 <- train(summary_judgment~., 
                 data = training, 
                 method = "svmRadial")
svmFit1


resamps <- resamples(list(GBM = gbmFit1,
                          SVM = svmFit1,
                          RF = rfFit1))
resamps



theme1 <- trellis.par.get()
theme1$plot.symbol$col = rgb(.2, .2, .2, .4)
theme1$plot.symbol$pch = 16
theme1$plot.line$col = rgb(1, 0, 0, .7)
theme1$plot.line$lwd <- 2
trellis.par.set(theme1)
bwplot(resamps, layout = c(3, 1))

splom(resamps)

?rfImpute

predictors<-names(training)[!names(training) %in% "summary_judgment"]

predTrain<-predict.train(object=rfFit1,testing[,predictors],type="raw")
table(predTrain)

densityplot(rfFitSJ, pch = "|")



# # Preprocess the data to more accurately represent the data
# pp_valuesSJ <- preProcess(train_procSJ, 
#                      method = c("center", "scale","YeoJohnson", "nzv")) 
# 
# # Final data set to form model from
# transformedSJ <- predict(pp_valuesSJ, train_procSJ)
