#load required packages
packages <- c("downloader", 
              "data.table", 
              "dummies", 
              "caret", 
              "FNN", 
              "pROC", 
              "rpart", 
              "rpart.plot",
              "Matrix",
              "xgboost",
              "reshape2",
              "ggplot2")

new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

library(downloader)
library(data.table)
library(dummies)
library(caret)
library(FNN)
library(pROC)
library(rpart)
library(rpart.plot)
library(Matrix)
library(xgboost)
library(reshape2)
library(ggplot2)



#set working directory
wd <- "/Users/user1/Documents" #specify directory from which to download and load the data
setwd(wd)

set.seed(32541)

#Download the loan.csv file out of the .zip archive found at the following path to access the "loan.csv" dataset: 
#https://www.kaggle.com/wendykan/lending-club-loan-data/downloads/lending-club-loan-data.zip

loans <- fread("loan.csv")

#select the numeric and categorical variables to include in the machine learning models
loans1 <- loans[loan_status %in% c("Charged Off", "Fully Paid"), c("dti",
                                                                   "annual_inc",
                                                                   "delinq_2yrs",
                                                                   "inq_last_6mths",
                                                                   "open_acc",
                                                                   "pub_rec",
                                                                   "revol_bal",
                                                                   "revol_util",
                                                                   "total_acc",
                                                                   "out_prncp",
                                                                   "out_prncp_inv",
                                                                   "total_pymnt",
                                                                   "total_pymnt_inv",
                                                                   "total_rec_prncp",
                                                                   "total_rec_int",
                                                                   "total_rec_late_fee",
                                                                   "recoveries",
                                                                   "collection_recovery_fee",
                                                                   "last_pymnt_amnt",
                                                                   "collections_12_mths_ex_med",
                                                                   "loan_status",
                                                                   "purpose",
                                                                   "home_ownership",
                                                                   "grade",
                                                                   "emp_length",
                                                                   "term",
                                                                   "addr_state",
                                                                   "verification_status",
                                                                   "application_type")]

#one hot encode the categorical variables
loans2 <- data.table(dummy.data.frame(loans1, 
                                      names = c("purpose",
                                                "home_ownership",
                                                "grade",
                                                "emp_length",
                                                "term",
                                                "addr_state",
                                                "verification_status",
                                                "application_type"), 
                                      sep = "_"))


#split the data into train and test sets
train_index <- createDataPartition(y = loans2$loan_status, p = 0.8, list = FALSE)
train.a <- loans2[train_index]
test.a <- loans2[!train_index]


#knnImputation of missing values
pre_obj <- preProcess(train.a[, -c("loan_status")], 
                      method = "knnImpute")
system.time(
  train_imp <- predict(pre_obj, 
                     train.a[, -c("loan_status")])
)

system.time(
test_imp <- predict(pre_obj, 
                    test.a[, -c("loan_status")])
)

train <- data.frame(train_imp)
test <- data.frame(test_imp)

#remove all zero-variance and near-zero-variane variables from data
nzvar <- subset(nearZeroVar(train, saveMetrics = TRUE), nzv ==TRUE & percentUnique < .5)
`%ni%` <- Negate(`%in%`)
train <- train[, names(train) %ni% rownames(nzvar)]
test <- test[, names(test) %ni% rownames(nzvar)]



##################################################################################################
#K-Nearest Neighbors Model                                                                       
##################################################################################################

#train knn model
k <- 9
system.time(
  knn_model <- knn(train = train, 
                   test = test, 
                   cl = train.a$loan_status, 
                   k = k)
)

closest <- train[c(attr(knn_model, "nn.index")[1,]), ]
closest$type <- "train"
test1 <- test
test1$type <- "test"

knn_pred <- knn_model[1:nrow(test.a)] #apply trained knn model to test data
knn_cm <- confusionMatrix(knn_pred, test.a$loan_status)
knn_sens <- sensitivity(knn_pred, as.factor(test.a$loan_status))
knn_spec <- specificity(knn_pred, as.factor(test.a$loan_status))

knn_labels <- test.a$loan_status
knn_predictions <- ifelse(knn_pred[1:nrow(test)] == "Charged Off", 1, 0)
knn_labels <- ifelse(test.a$loan_status == "Charged Off", 1, 0)
knn_roc <- roc(knn_labels, knn_predictions)
knn_auc <- auc(knn_roc)



##################################################################################################
#Logistic Regression Model using Principle Component Analysis
##################################################################################################

#transform predictor variables to principle components
pre_pca <- preProcess(train, method = "pca")
train_pca <- predict(pre_pca, train)
test_pca <- predict(pre_pca, test)

train_pca2 <- cbind(train_pca, ifelse(train.a$loan_status == "Charged Off", 1, 0))
colnames(train_pca2) <- c(colnames(train_pca), "loan_status") 

#train PCA logistic regression model
logit_model <- glm(formula = loan_status ~ ., family = binomial(link = "logit"), data = train_pca2)

logit_pred <- predict(logit_model, newdata = test_pca, type = "response") #apply trained PCA logistic regression model to test data

prediction_logit <- as.factor(ifelse(logit_pred >= .5, "Charged Off", "Fully Paid"))
logit_cm <- confusionMatrix(prediction_logit, test.a$loan_status)
logit_sens <- sensitivity(prediction_logit, as.factor(test.a$loan_status))
logit_spec <- specificity(prediction_logit, as.factor(test.a$loan_status))

logit_labels <- ifelse(test.a$loan_status == "Charged Off", 1, 0)
logit_predictions <- ifelse(prediction_logit == "Charged Off", 1, 0)
logit_roc <- roc(logit_labels, logit_predictions)
logit_auc <- auc(logit_roc)



##################################################################################################
#Decision Tree Model
##################################################################################################

train2 <- cbind(train, train.a$loan_status)
colnames(train2) <- c(colnames(train), "loan_status")

#train decision tree model
control_param <- rpart.control(cp = .005)
dtree_model <- rpart(loan_status ~ ., data = train2, control = control_param)

rpart.plot(dtree_model) #visualizes the decision tree

dtree_pred <- data.frame(predict(dtree_model, newdata = test)) #apply trained decision tree model to test data

dtree_pred$fin <- as.vector(pmax(dtree_pred[, 1], dtree_pred[ ,2]))
prediction.tree1 <- as.factor(ifelse(dtree_pred$fin == dtree_pred[, 1], "Charged Off", "Fully Paid"))
dtree_cm <- confusionMatrix(prediction.tree1, test.a$loan_status)
dtree_sens <- sensitivity(prediction.tree1, as.factor(test.a$loan_status))
dtree_spec <- specificity(prediction.tree1, as.factor(test.a$loan_status))

dtree_labels <- ifelse(test.a$loan_status == "Charged Off", 1, 0)
dtree_predictions <- ifelse(prediction.tree1 == "Charged Off", 1, 0)
dtree_roc <- roc(dtree_labels, dtree_predictions)
dtree_auc <- auc(dtree_roc)



##################################################################################################
#Random Forest with a custom model to test different combinations of mtry and ntree values
##################################################################################################

#create custom model for multiple mtry and ntree values
customRF <- list(type = "Classification", library = "randomForest", loop = NULL)
customRF$parameters <- data.frame(parameter = c("mtry", "ntree"), class = rep("numeric", 2), label = c("mtry", "ntree"))
customRF$grid <- function(x, y, len = NULL, search = "grid") {}
customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  randomForest(x, y, mtry = param$mtry, ntree=param$ntree, ...)
}
customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata)
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type = "prob")
customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes


#train the random forest model
fitControl <- trainControl(method="cv", 
                           number=3,
                           allowParallel = TRUE)

tunegrid <- expand.grid(.mtry=c(9:18), .ntree=c(250, 325, 400))

system.time(
  rf_model <- train(as.factor(loan_status) ~ ., 
                     method = customRF, 
                     data = train2, 
                     trControl = fitControl,
                     tuneGrid = tunegrid,
                     metric = 'Kappa')
)

print(rf_model) #print the trained model summary
train_plot <- plot(rf_model) #plot the Kappa scores for the different ntree and mtry combinations
print(train_plot)

rf_pred <- predict(rf_model, test) #apply trained random forest model to test data
rf_cm <- confusionMatrix(rf_pred, test.a$loan_status)
rf_sens <- sensitivity(rf_pred, as.factor(test.a$loan_status))
rf_spec <- specificity(rf_pred, as.factor(test.a$loan_status))

rf_labels <- ifelse(test.a$loan_status == "Charged Off", 1, 0)
rf_predictions <- ifelse(rf_pred == "Charged Off", 1, 0)
rf_roc <- roc(rf_labels, rf_predictions)
rf_auc <- auc(rf_roc)

#plot the top 20 most importance variables in the RF model
var_imp_plot <- plot(varImp(rf_model, scale = FALSE), top = 20) 
print(var_imp_plot)



##################################################################################################
#Gradient Boosting Model
##################################################################################################

#create sparse matrix for numeric predictors
#################################################
M.a <- sparse.model.matrix(~ dti +
                             annual_inc +
                             delinq_2yrs +
                             inq_last_6mths +
                             open_acc +
                             pub_rec +
                             revol_bal +
                             revol_util +
                             total_acc +
                             out_prncp +
                             out_prncp_inv +
                             total_pymnt +
                             total_pymnt_inv +
                             total_rec_prncp +
                             total_rec_int +
                             total_rec_late_fee +
                             recoveries +
                             collection_recovery_fee +
                             last_pymnt_amnt +
                             collections_12_mths_ex_med -1, data = rbind(train_imp, test_imp))


#create sparse matrix for categorical predictors
#################################################
cats <- loans[loan_status %in% c("Charged Off", "Fully Paid"), c("purpose",
                                                                 "home_ownership",
                                                                 "grade",
                                                                 "emp_length",
                                                                 "term",
                                                                 "addr_state",
                                                                 "verification_status",
                                                                 "application_type")]
#reorder to stack train data on top of test data
cats <- rbind(cats[train_index],
              cats[!train_index])
cats$account <- c(1:nrow(cats))

#identify unique categorical feature values for each account (record)
d1 <- cats[,list(account, purpose)] 
d2 <- cats[,list(account, home_ownership)] 
d3 <- cats[, list(account, grade)] 
d4 <- cats[, list(account, emp_length)]
d5 <- cats[, list(account, term)] 
d6 <- cats[, list(account, addr_state)] 
d7 <- cats[, list(account, verification_status)]
d8 <- cats[, list(account, application_type)]

d1[ ,purpose:= paste0("purpose: ", purpose)]
d2[ ,home_ownership:= paste0("home_ownership: ", home_ownership)]
d3[ , grade:= paste0("grade: ", grade)]
d4[ , emp_length:= paste0("emp_length: ", emp_length)]
d5[ , term:= paste0("term: ", term)]
d6[ , addr_state:= paste0("addr_state: ", addr_state)]
d7[ , verification_status:= paste0("verification_status: ", verification_status)]
d8[ , application_type:= paste0("application_type: ", application_type)]
names(d1) <- names(d2) <- names(d3) <- names(d4) <- names(d5) <- names(d6) <- names(d7) <- names(d8) <- c("account","feature_name")
d <- rbind(d1, d2, d3, d4, d5, d6, d7, d8)
rm(d1, d2, d3, d4, d5, d6, d7, d8); gc()
d <- unique(d)
setkey(d, account)

#creates a list of unique accounts (records)
ii <- as.character(unique(d$account))
#creates a list of all unique feature_names
jj <- unique(d$feature_name)
#creates a list the length of dd that gives each account a unique identifier from 1: the number of unique accounts
id_i <- match(d$account,ii)
#same thing for feature_name
id_j <- match(d$feature_name,jj)
id_ij <- cbind(id_i,id_j)
#creates a matrix frame that has the feature_names as column names and accounts as row names, and every point is blank
M.b <- Matrix(0,nrow=length(ii),ncol=length(jj),
              dimnames=list(ii,jj),sparse=T)
#if the account and feature_name are found together in the id_i data frame, then mark it as a 1 in the M.b matrix
M.b[id_ij] <- 1
rm(ii,jj,id_i,id_j,id_ij);gc()

#combine the numeric and categorical matrices
M <- cbind(M.a, M.b)


#create xgb matrices for the xgboost model
#################################################
train_data <- M[1:nrow(train.a), ]
trwr <- sample(1:nrow(train_data), round(.85*nrow(train_data), 0), replace = FALSE)
trw_data <- train_data[trwr, ]
tew_data <- train_data[-trwr, ]

test_data <- M[(nrow(train.a)+1):nrow(M), ] 

trw_label <- ifelse(train.a[trwr, ]$loan_status == "Charged Off", 1, 0)
tew_label <- ifelse(train.a[-trwr, ]$loan_status == "Charged Off", 1, 0)
test_label <- ifelse(test.a$loan_status == "Charged Off", 1, 0)

dtrain_tr <- xgb.DMatrix(data = trw_data, label = trw_label)
dtrain_te <- xgb.DMatrix(data = tew_data, label = tew_label)
dtest <- xgb.DMatrix(data = test_data, label = test_label)


#train xgboost tree model
##############################
watchlist <- list(train = dtrain_tr, test = dtrain_te)
bst <- xgb.train(data = dtrain_tr,
                 watchlist = watchlist,
                 eta = .1,
                 nround = 200,
                 objective = "binary:logistic",
                 eval_metric = "auc")

#perform prediction
gb_pred <- as.factor(ifelse(predict(bst, test_data) >= .5, "Charged Off", "Fully Paid")) #apply trained xgboost model to test data
gb_cm <- confusionMatrix(gb_pred, test.a$loan_status)
gb_sens <- sensitivity(gb_pred, as.factor(test.a$loan_status))
gb_spec <- specificity(gb_pred, as.factor(test.a$loan_status))

gb_labels <- ifelse(test.a$loan_status == "Charged Off", 1, 0)
gb_predictions <- ifelse(gb_pred == "Charged Off", 1, 0)
gb_roc <- roc(gb_labels, gb_predictions)
gb_auc <- auc(gb_roc)



##################################################################################################
#Compare the ability of the five different models to predict the outcomes of the test set
##################################################################################################

results <- data.frame(cbind(rbind(knn_auc,
                                  logit_auc,
                                  dtree_auc,
                                  rf_auc,
                                  gb_auc),
                            rbind(knn_sens,
                                  logit_sens,
                                  dtree_sens,
                                  rf_sens,
                                  gb_sens),
                            rbind(knn_spec,
                                  logit_spec,
                                  dtree_spec,
                                  rf_spec,
                                  gb_spec)))
rownames(results) <- c("K-Nearest Neighbors (k=9)", 
                       "Logistic Regression w/ PCA", 
                       "Decision Tree", 
                       "Random Forest", 
                       "Gradient Boosting")
colnames(results) <- c("AUC", 
                       "Sensitivity", 
                       "Specificity")
results <- results[order(-results$AUC), ]

results_viz <- melt(results)
results_viz$model <- rep(row.names(results), 3)
viz_comparison <- ggplot(results_viz, aes(x = model, y = value, fill = model)) + 
  geom_bar(stat = "identity") + 
  facet_grid(variable ~ .) +
  coord_flip() +
  scale_x_discrete(limits = rev(row.names(results))) +
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) +
  labs(title = "Predictive Model Comparisons")

print(viz_comparison)
print(round(results, 3))

  
