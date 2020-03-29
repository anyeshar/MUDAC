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

train_dockets$summary_judgment <- as.factor(train_dockets$summary_judgment)
train_dockets <- train_dockets %>% select(-settled)

pp_values <- preProcess(train_dockets, 
                     method = c("center", "scale","YeoJohnson", "nzv")) # Processes the data to remove NA's

train_proc <- predict(pp_values, train_dockets) #Becomes a dataframe again

train_proc <- train_proc %>% 
  select(-mudac_id, -primarytitle, -nos_text, -protected_class, -additional_nos, -arbitration_at_filing,
         -statute) 
#Remove Problem Variables

train_proc$summary_judgment <- as.numeric(train_proc$summary_judgment)
train_proc <- train_proc %>% mutate(summary_judgment = summary_judgment - 1)


dummy <- dummyVars("~ .",
                   data = train_proc,
                   fullRank = T) #Creates Dummy Variables

transformed <- data.frame(predict(dummy, newdata = train_proc))
#Workable data set

# --------Modeling Start---------
set.seed(825)
control <- trainControl(method="repeatedcv", number=10, repeats=3)
inTraining <- createDataPartition(transformed$summary_judgment, p = .02, list = FALSE)
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
