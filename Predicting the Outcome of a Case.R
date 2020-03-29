#Prediction Model

# --------Package Installs---------
install.packages("caret")
install.packages("randomForest")
install.packages("RANN")
install.packages('kernlab')
# --------Loading Libraries---------
library(tidyverse)
library(ggplot2)
library(caret)
library(randomForest)
library(RANN)
library(kernlab)
# --------Data Prepping---------
train_dockets <- read_csv("~/MUDAC/Data/train_dockets.csv") # Load the Data Set In

train_dockets <- train_dockets %>% select(-settled)

train_dockets <- train_dockets %>% 
  select(-mudac_id, -primarytitle, -nos_text, -protected_class, -additional_nos, -arbitration_at_filing,
         -statute) 

dummy <- dummyVars("~ .",
                   data = train_dockets,
                   fullRank = T) #Creates Dummy Variables

train_proc <- data.frame(predict(dummy, newdata = train_dockets))

pp_values <- preProcess(train_proc, 
                     method = c("center", "scale","YeoJohnson", "nzv")) # Processes the data to remove NA's

transformed <- predict(pp_values, train_proc) #Becomes a dataframe again

transformed <- transformed %>% select(-outcomeSummary.Judgment)




#Workable data set

# --------Modeling Start---------
inTraining <- createDataPartition(transformed$summary_judgment, p = .015, list = FALSE)
training <- transformed[ inTraining,]
testing  <- transformed[-inTraining,]

rfFit1 <- train(summary_judgment~., 
                data = training, 
                method = "rf")
rfFit1

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

prediction <- predict(rfFit1, testing)
head(prediction)
confusionMatrix(prediction, as.factor(testing$summary_judgment))

warnings()
?train
