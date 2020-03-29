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
library(ranger)
library(RANN)
library(kernlab)
# --------Data Prepping---------

# Load data sets in
train_dockets <- read_csv("~/MUDAC/Data/train_dockets.csv") 
test_dockets <- read_csv("~/MUDAC/Data/test_dockets.csv")

# Remove unwanted variables
test_reducedSJ <- test_dockets %>% 
  select(-mudac_id, -nos_text, -arbitration_at_filing, -additional_nos,
         -statute) 
train_reducedSJ <- train_dockets %>% 
  select(-mudac_id, -primarytitle, -nos_text, -arbitration_at_filing, -additional_nos,
         -statute, -settled, -outcome) 

# Creates Dummy Variables
dummy <- dummyVars("~ .",
                   data = train_reducedSJ,
                   fullRank = T) 

# New data set with dummy variables replacing categorical variables
train_procSJ <- data.frame(predict(dummy, newdata = train_reducedSJ))

# Preprocess the data to more accurately represent the data
pp_valuesSJ <- preProcess(train_procSJ, 
                     method = c("center", "scale","YeoJohnson", "nzv")) 

# Final data set to form model from
transformedSJ <- predict(pp_valuesSJ, train_procSJ) 

# Replace NA'a with -888
transformedSJ[is.na(transformedSJ)]<- -888


# --------Modeling Start---------

# Partition the data set to make it more reasonable to deal with
inTrainingSJ <- createDataPartition(transformedSJ$summary_judgment, p = .05, list = FALSE)
trainingSJ <- transformedSJ[ inTrainingSJ,]
testingSJ  <- transformedSJ[-inTrainingSJ,]

#Change to factor so we can run random forest
trainingSJ$summary_judgment <- as.factor(trainingSJ$summary_judgment) 

# Run basic model
rfFitSJ <- train(summary_judgment~., 
                data = trainingSJ, 
                method = "ranger")

# Output model
rfFitSJ

# Predict the testing set and output the results
predictionSJ <- predict(rfFitSJ, testingSJ)
head(predictionSJ)
summary(predictionSJ)

# Produce a table of probabilities
pred_probSJ <- predict(rfFitSJ, testingSJ, type="prob")
head(pred_probSJ)

# Produce a confusion matrix
confusionMatrix(predictionSJ, as.factor(testingSJ$summary_judgment))

# Provide a list of the most important variables
varImp(rfFitSJ)





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
