#Prediction Model

# --------Package Installs---------
install.packages("caret")
install.packages("randomForest")
install.packages("RANN")
# --------Loading Libraries---------
library(tidyverse)
library(ggplot2)
library(caret)
library(randomForest)
library(RANN)
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


#Remove Problem Variables




#Workable data set

# --------Modeling Start---------
set.seed(825)
control <- trainControl(method="repeatedcv", number=10, repeats=3)
inTraining <- createDataPartition(transformed$summary_judgment, p = .005, list = FALSE)
training <- transformed[ inTraining,]
testing  <- transformed[-inTraining,]
rfFit1 <- train(summary_judgment~., 
                data = training, 
                method = "rf",
                trControl = control)
rfFit1


predict(rfFit1, newdata = head(testing))

warnings()
?train
