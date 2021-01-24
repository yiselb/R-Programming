library(readxl)
library(randomForest)
library(ggplot2)
library(cowplot)
library(pROC)

# importing dataset from excel
setwd("C:/Users/yisel/Downloads")
data <- read_excel("486_project_recode_final.xlsx")

head(data)

# convert party from chr to factor
data$party <- factor(data$party)
# checking data types of columns
str(data)

# setting a sample for random forest trees
set.seed(12345)
index <- sam
# running random forest on train dataset
rf_train <- randomForest(party~., data=data, importance=TRUE)
rf_train


# plot error rates
oob.error.data <- data.frame(
  Trees=rep(1:nrow(rf_train$err.rate), times=3),
  Type=rep(c("OOB","0","1"), each=nrow(rf_train$err.rate)),
  Error=c(rf_train$err.rate[,"OOB"],
          rf_train$err.rate[,"0"],
          rf_train$err.rate[,"1"]))

ggplot(data=oob.error.data, aes(x=Trees, y=Error)) + geom_line(aes(color=Type))


rf_prediction <- predict(rf_train,data, type="prob")

# get ROC and AUC
roc_rf <- roc(data$party, rf_prediction[,2])
roc_rfauc <- auc(roc_rf)

# plotting ROC curve with AUC
plot(roc_rf, col="green", main="Random Forest")
paste("Area under curve of random forest: ", roc_rfauc)