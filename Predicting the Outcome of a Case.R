#Prediction Model

# --------Package Installs---------
install.packages("caret")
# --------Loading Libraries---------
library(tidyverse)
library(ggplot2)
library(caret)
# --------Data Prepping---------
train_dockets <- read_csv("~/MUDAC/Data/train_dockets.csv") # Load the Data Set In

train <- train_dockets %>% 
  select(-settled) # Remove Settled as we are prediciting Summary Judgment (SJ)

nzv <- nearZeroVar(train, saveMetrics= TRUE)
nzv[nzv$nzv,][1:10,]

pp_hpc <- preProcess(train[, -8], 
                     method = c("center", "scale", "YeoJohnson", "nzv"))
pp_hpc

transformed <- predict(pp_hpc, newdata = train[1:31, -8])
head(transformed)

# --------Data Cleaning Finished---------

# --------Modeling Start---------
fitControl <- trainControl(
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)

set.seed(825)
gbmFit1 <- train(Class ~ ., data = transformed, 
                 method = "gbm", 
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE)
gbmFit1

?train

