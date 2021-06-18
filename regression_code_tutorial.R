### Start


library(mlbench)
data("BostonHousing")
str(BostonHousing)



### for normal linear regression

linear_model <- lm(medv ~ rm , data = BostonHousing)
summary(linear_model)

#### caret simple linear regression

library(caret)
linear_model_caret <- train(
                        form = medv ~ rm , 
                        data = BostonHousing, 
                        method = 'lm'
                        )
summary(linear_model_caret)







## data split

library(caret)


set.seed(168)
num_train <- createDataPartition(y= BostonHousing$medv , p =0.7 , list = FALSE)
train_data <- BostonHousing[num_train,]
test_data <- BostonHousing[-num_train,]

nrow(BostonHousing)
nrow(train_data)
nrow(test_data)

### prediction

linear_model <- lm(medv ~ rm , data = BostonHousing)
predict(linear_model , newdata = data.frame(rm = c(6,7)))




lm_model <- lm(medv ~ rm , data = train_data) 

test_data['pred'] <- predict(lm_model , newdata = test_data)

## RMSE
RMSE(test_data$pred , test_data$medv) 
#[1] 5.535709

## MAE 
MAE(test_data$pred , test_data$medv) 
library(dplyr)
(test_data %>% 
    mutate(actual_y = medv) %>%
    mutate(pred_y = pred) %>%
    mutate(mean_y = mean(actual_y)) %>%
    mutate(error_squared = (pred_y - actual_y)^2) %>%
    mutate(total_squared = (actual_y - mean_y)^2) %>%
    summarize(SSE = sum(error_squared) , SST = sum(total_squared)) %>%
    summarize(R_sqaured = 1 - (SSE/SST) )
)[1,1]



########### multiple linear reg




linear_model_three <-  lm(medv ~ rm + age +dis  , data = BostonHousing)
summary(linear_model_three)


linear_model_multiple <- lm(medv ~ . , data = BostonHousing)
summary(linear_model_multiple)


library(caret)
linear_model_three_caret <- train(
    form = medv ~ rm + age +dis , 
    data = BostonHousing, 
    method = 'lm'
)
summary(linear_model_three_caret)


library(caret)
linear_model_multiple_caret <- train(
    form = medv ~ . , 
    data = BostonHousing, 
    method = 'lm'
)
summary(linear_model_multiple_caret)


#### polynomial regression

poly_model_I <- lm(medv~ lstat+ I(lstat^2) + I(lstat^3) + I(lstat^4), data = train_data)

summary(poly_model_I)





poly_model_poly <- lm(medv~ poly(lstat,4), data = train_data) 

summary(poly_model_poly)




library(caret)
poly_model_I_caret <- train(
    form = medv~ lstat+ I(lstat^2) + I(lstat^3) + I(lstat^4) , 
    data = train_data, 
    method = 'lm'
)
summary(poly_model_I_caret)


library(caret)
poly_model_poly_caret <- train(
    form = medv~ poly(lstat,4) , 
    data = train_data, 
    method = 'lm'
)
summary(poly_model_I_caret)





## ridge lasso
summary(BostonHousing)


library(caret)
scale_param <- preProcess(BostonHousing , method = c("scale", "center"))
BostonHousing_transform <- predict(scale_param, BostonHousing)
summary(BostonHousing_transform)
apply(BostonHousing_transform,2,sd)













library(glmnet)
label_col = ncol(BostonHousing)
feature_col = ncol(BostonHousing)-1


features = as.matrix(BostonHousing[,1:feature_col])
label = BostonHousing[,label_col]


fit = glmnet(features, label, alpha = 1 , lambda = 0.2)
fit






library(caret)
ridge_caret<- train(BostonHousing[,1:feature_col], label, method = "glmnet",
                    alpha = 0, lambda = 0.2)
coef(ridge_caret)

ridge_caret_lambda_testing <- ridge_caret$results[,c('RMSE','lambda')]
ggplot(ridge_caret_lambda_testing , aes(x = lambda, y = RMSE)) +
    geom_line()



###########################################################################################
### ridge laaso elasticnet

library(caret)
library(glmnet)

ridge_model <- train(medv ~ .,
                  data = BostonHousing,
                  method = "glmnet",
                  tuneGrid = expand.grid(alpha = 1,
                                         lambda = 0.2
                  ))
ridge_model



lasso_model <- train(medv ~ .,
                     data = BostonHousing,
                     method = "glmnet",
                     tuneGrid = expand.grid(alpha = 0,
                                            lambda = 0.2
                     ))
lasso_model




elas_model <- train(
                    medv ~ .,
                    data = BostonHousing,
                    method = "glmnet",
                    tuneGrid = expand.grid(
                            alpha = 0.5,
                            lambda = 0.2
                    )
                    )
elas_model



### ridge laaso utube
num_train <- createDataPartition(y= BostonHousing$medv , p =0.7 , list = FALSE)

train_data <- BostonHousing[num_train,]
test_data <- BostonHousing[-num_train,]

set.seed(639)
ridge_model <- train(medv ~.,
                    train_data,
                    method = 'glmnet',
                    tuneGrid = expand.grid(alpha = 0,
                                           lambda = seq(0.01 ,1 ,length=5)),
                   # trControl = custom
                    )

plot(ridge_model)
plot(ridge_model$finalModel , xvar = "lambda" ,label = T)
plot(ridge_model$finalModel , xvar = "dev" , label = T)
plot(varImp(ridge_model, scale = F))
##   lasso

set.seed(639)
lasso_model <- train(medv ~.,
                     train_data,
                     method = 'glmnet',
                     tuneGrid = expand.grid(alpha = 1,
                                            lambda = seq(0.01 ,1 ,length=5)),
                     # trControl = custom
)


plot(lasso_model)
plot(lasso_model$finalModel , xvar = "lambda" ,label = T)
plot(lasso_model$finalModel , xvar = "dev" , label = T)
plot(varImp(lasso_model, scale = F))



## elasticnet



set.seed(639)
elastic_net_model <- train(medv ~.,
                     train_data,
                     method = 'glmnet',
                     tuneGrid = expand.grid(alpha = seq(0, 1, length = 4),
                                            lambda = seq(0.01 ,1 ,length=5)),
                     # trControl = custom
)


plot(elastic_net_model)
plot(elastic_net_model$finalModel , xvar = "lambda" ,label = T)
plot(elastic_net_model$finalModel , xvar = "dev" , label = T)
plot(varImp(elastic_net_model, scale = F))


## combine model
list_of_model <- list(Ridge = ridge_model , Lasso = lasso_model , Elasticnet = elastic_net_model)
resam <- resamples(list_of_model)
summary(resam)
bwplot(resam)
xyplot(resam, metric = 'RMSE')


elastic_net_model$bestTune
best <- elastic_net_model$finalModel
best
coef(best , s = elastic_net_model$bestTune$lambda)

saveRDS(elastic_net_model , "final_model.rds")


######### decision tree




### dataset introduction

weight <- c(55 , 45,41,60,70,75)
sex <- c('F','F','F','M','M','M')
height <- c(165,159,151,165,171,180)


weight_data <- data.frame(weight, sex, height)
str(weight_data)

library(rpart)
library(rpart.plot)

rpart_con <-  rpart.control(minsplit = 3, cp = 0.01, 
                            maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, xval = 10,
                            surrogatestyle = 0, maxdepth = 30)
tree_model <- rpart(weight ~ .,data = weight_data , control = rpart_con)


rpart.plot(tree_model)
printcp(tree_model)
plotcp(tree_model)
summary(tree_model)


par(mfrow=c(1,2))
rsq.rpart(tree_model)

tree_fit<- prune(tree_model , cp = 0.11)
tree_fit



### caret rpart

tree_model_caret <- train(weight~.,
                                  data = weight_data,
                                  method = "rpart")
tree_model_caret




### tuning hyper parameter
## loop tuning


library(ranger)
library(dplyr)

param_grid <- expand.grid(
    mtry = seq(2,8,by =1),
    node_size = seq(3,9,by = 2),
    sample_size = c(.55,.632,.70,.80),
    RMSE = 0
    )

nrow(param_grid)

for(i in 1:nrow(param_grid)){
    
    model <- ranger(
        formula = medv ~.,
        data = BostonHousing,
        num.tree = 21,
        mtry = param_grid$mtry[i],
        min.node.size = param_grid$node_size[i],
        sample.fraction = param_grid$sample_size[i],
        seed = 789
        
    )
    param_grid$RMSE[i] <- sqrt(model$prediction.error)
}

param_grid %>% 
    arrange(RMSE) %>%
    head(10)



## randomforest model ranger


library(ranger)
randomforest_model <- ranger(formula = medv ~.,
       data = BostonHousing,
       num.tree = 21,
       mtry = 5,
       importance = "permutation")

randomforest_model





df_importance <- data.frame(randomforest_model$variable.importance)
names(df_importance) <- "importance_score"
df_importance[order(df_importance$importance_score , decreasing = TRUE),,drop = FALSE]


mtry
min.node.size
sample.fraction
num.tree



## rf caret
 
library(caret)
set.seed(639)

randomforest_model_caret <- train(medv~.,
                    data = BostonHousing,
                    importance = "permutation",
                    method = "ranger")

randomforest_model_caret



### gbm 
library(gbm)
# gbm(formula = formula(data), distribution = "bernoulli",
#    data = list(), weights, var.monotone = NULL, n.trees = 100,
#    interaction.depth = 1, n.minobsinnode = 10, shrinkage = 0.1,
#    bag.fraction = 0.5, train.fraction = 1, cv.folds = 0,
#    keep.data = TRUE, verbose = FALSE, class.stratify.cv = NULL,
#    n.cores = NULL)

library(gbm)
set.seed(639)
gbm_model <- gbm(formula = medv~., distribution = "gaussian",
    data = BostonHousing, n.trees = 20,
    interaction.depth = 5, n.minobsinnode = 10, shrinkage = 0.1,
    bag.fraction = 0.8)
summary(gbm_model)


library(caret)
library(gbm)

set.seed(639)

gbm_model_caret <- train(medv~.,
                                  data = BostonHousing,
                                  method = "gbm")

summary(gbm_model_caret)



## svm   e1071

## S3 method for class 'formula'
#svm(formula, data = NULL, ..., subset, na.action =
#       na.omit, scale = TRUE)
## Default S3 method:
#svm(x, y = NULL, scale = TRUE, type = NULL, kernel =
#        "radial", degree = 3, gamma = if (is.vector(x)) 1 else 1 / ncol(x),
#    coef0 = 0, cost = 1, nu = 0.5,
#    class.weights = NULL, cachesize = 40, tolerance = 0.001, epsilon = 0.1,
#    shrinking = TRUE, cross = 0, probability = FALSE, fitted = TRUE,
#    ..., subset, na.action = na.omit)


library(e1071)
linear_svm = svm(medv~. ,kernel = "linear", cost = 1 , epsilon = 0.1 ,data = BostonHousing)
summary(linear_svm)


library(e1071)
poly_svm = svm(medv~. ,kernel = "poly",  degree = 4,  cost = 1 , epsilon = 0.1 ,data = BostonHousing)
summary(poly_svm)



library(e1071)
radial_svm = svm(medv~. ,kernel = "radial",gamma = 1/ncol(BostonHousing) ,  cost = 1 , epsilon = 0.1 ,data = BostonHousing)
summary(radial_svm)


## caret svm
library(kernlab)
library(caret)

set.seed(639)
linear_svm_caret <- train(medv ~.,
                           data = BostonHousing,
                           method = 'svmLinear',
                           tuneGrid = expand.grid( (C = 1),
                                                  (epsilon = 0.1)))
                           
linear_svm_caret






library(kernlab)
library(caret)

set.seed(639)
poly_svm_caret <- train(medv ~.,
                          data = BostonHousing,
                          method = 'svmPoly',
                          tuneGrid = expand.grid(degree = 3,
                                                 scale = 1,
                                                 C = 1))

poly_svm_caret



library(kernlab)
library(caret)
set.seed(639)
RBF_svm_caret <- train(medv ~.,
                        data = BostonHousing,
                        method = 'svmRadial',
                        tuneGrid = expand.grid(
                                    sigma = 0.1,
                                    C = 1)
                        )

RBF_svm_caret


#degree, scale, C


#svmLinear      C
#svmPoly    	degree, scale, C
#svmRadial   	sigma, C


library(caret)

scale_param <- preProcess(BostonHousing , method = c("scale", "center"))
BostonHousing_transform <- predict(scale_param, BostonHousing)

knn_model <- knnreg(formula = medv ~., data = BostonHousing_transform, k = 5)
summary(knn_model)

knn_model <- knn(formula = medv ~., data = BostonHousing_transform, k = 5)
summary(knn_model)

#BostonHousing_transform['predicted'] <- predict(knn_model , newdata = BostonHousing_transform)
#BostonHousing_transform
#ggplot(BostonHousing_transform , aes(x = predicted , y = medv))+
#    geom_point()




### caret model

library(caret)
scale_param <- preProcess(BostonHousing , method = c("scale", "center"))
BostonHousing_transform <- predict(scale_param, BostonHousing)


knn_model_caret <- train(medv ~. , 
                            data = BostonHousing_transform ,
                            method = "knn" ,
                            tuneGrid = expand.grid(
                             k = 5)
                            )

knn_model_caret
summary(knn_model_caret)




# ensemble model

library(caret)
library(ranger)
library(kernlab)
set.seed(168)
num_train <- createDataPartition(y= BostonHousing$medv , p =0.7 , list = FALSE)
train_data <- BostonHousing[num_train,]
test_data <- BostonHousing[-num_train,]


scale_param <- preProcess(train_data , method = c("scale", "center"))
train_data_scaled <- predict(scale_param, train_data)
test_data_scaled <- predict(scale_param, test_data)

fitControl <- trainControl(
    method = "repeatedcv",
    number = 5,
    repeats = 5)



elastic_net_model <- train(medv ~ .,  data = train_data_scaled,
                    method = "glmnet",
                    trControl = fitControl, 
                    tuneGrid = expand.grid(
                        alpha = seq(0,1,0.1),
                        lambda = seq(0,5,0.5)
                        )
                        )


randomforest_model_caret <- train(medv~.,
                                  data = train_data_scaled,
                                  method = "ranger",
                                  trControl = fitControl,
                                  tuneGrid = expand.grid(
                                      mtry = seq(2,9,1),
                                      splitrule = "variance",
                                      min.node.size = seq(2,20,2)
                                  )
                                )


svmrbf_model_caret <- train(medv~.,
                                  data = train_data_scaled,
                                  method = "svmRadial",
                                  trControl = fitControl,
                                  tuneGrid = expand.grid(
                                      sigma = c(0.01,0.1,0.5,1,1.5,2,4),
                                      C = c(0.1,0.5,1,2,4,8)                                  
                                      ))


knn_model_caret <- train(medv~.,
                            data = train_data_scaled,
                            method = "knn",
                            trControl = fitControl,
                            tuneGrid = expand.grid(
                                k = seq(3,9,1)                          
                            ))

predicted_dataset = test_data_scaled

predicted_dataset['actual'] =  test_data_scaled['medv']
predicted_dataset['elasticnet_pred'] = predict(elastic_net_model , newdata = test_data_scaled)
predicted_dataset['rf_pred'] = predict(randomforest_model_caret , newdata = test_data_scaled)
predicted_dataset['svm_pred'] = predict(svmrbf_model_caret , newdata = test_data_scaled)
predicted_dataset['knn_pred'] = predict(knn_model_caret , newdata = test_data_scaled)
predicted_dataset['average_pred'] = rowMeans(predicted_dataset[c('elasticnet_pred','rf_pred','svm_pred','knn_pred')])



RMSE(predicted_dataset$elasticnet_pred , predicted_dataset$actual) 
RMSE(predicted_dataset$rf_pred , predicted_dataset$actual) 
RMSE(predicted_dataset$svm_pred , predicted_dataset$actual) 
RMSE(predicted_dataset$knn_pred , predicted_dataset$actual)
RMSE(predicted_dataset$average_pred , predicted_dataset$actual)





### tuning

set.seed(168)
num_train <- createDataPartition(y= BostonHousing$medv , p =0.7 , list = FALSE)
train_data <- BostonHousing[num_train,]
test_data <- BostonHousing[-num_train,]


scale_param <- preProcess(train_data , method = c("scale", "center"))
train_data_scaled <- predict(scale_param, train_data)
test_data_scaled <- predict(scale_param, test_data)

fitControl <- trainControl(
    method = "repeatedcv",
    number = 5,
    repeats = 5)



elastic_net_model <- train(medv ~ .,  data = train_data_scaled,
                           method = "glmnet",
                           trControl = fitControl, 
                           tuneGrid = expand.grid(
                               alpha = seq(0,1,0.1),
                               lambda = seq(0,0.5,5)
                           )
)




## Tuning SVR model by varying values of maximum allowable error and cost parameter

#Tune the SVM model
OptModelsvm=tune(svm, Y~X, data=data,ranges=list(elsilon=seq(0,1,0.1), cost=1:100))

#Print optimum value of parameters
print(OptModelsvm)

#Plot the perfrormance of SVM Regression model
plot(OptModelsvm)



library(glmnet)
library(caret)
library(e1071)
library(rpart)
label_col = ncol(BostonHousing)
feature_col = ncol(BostonHousing)-1


set.seed(168)
num_train <- createDataPartition(y= BostonHousing$medv , p =0.7 , list = FALSE)
train_data <- BostonHousing[num_train,]
test_data <- BostonHousing[-num_train,]





set.seed(168)
tc <- tune.control(cross = 5)
OptModel=tune(rpart, medv~., data=train_data ,ranges=list( cp = c(0.0001,0.0005,0.001,0.005,0.01)) , tunecontrol = tc )
plot(OptModel)
OptModel

## caret tune
library(caret)
library(rpart)

set.seed(888)
fitControl <- trainControl(
    method = "repeatedcv",
    number = 5,
    repeats = 5)

treegrid <-  expand.grid(
    cp = c(0.0001,0.0005,0.001,0.005,0.01))

rpart_caret = train(medv~. , data = train_data , method = "rpart", trControl = fitControl ,tuneGrid = treegrid)
rpart_caret



### usual prep
train_data_feature <- train_data[, -which(names(BostonHousing) == "medv")]
test_data_feature <- test_data[, -which(names(BostonHousing) == "medv")]
scaling_model <- preProcess(train_data_feature, method = c("center","scale"))
train_data_scaled <- predict(scaling_model , train_data_feature)
test_data_scaled <- predict(scaling_model , test_data_feature)
train_data_scaled$medv <- train_data$medv
test_data_scaled$medv <- test_data$medv


train_data_scaled_mat = as.matrix(train_data_scaled[,1:feature_col])
train_label = train_data_scaled[,label_col]

test_data_scaled_mat = as.matrix(test_data_scaled[,1:feature_col])
test_label = test_data_scaled[,label_col]

### ### ###
## identity prep



weight <- c(55 , 45,41,60,70,75)
sex <- c('F','F','F','M','M','M')
height <- c(165,159,151,165,171,180)


weight_data <- data.frame(weight, sex, height)
str(weight_data)


library(dplyr)
name <- c('robert','robert','kate','kate','kate','Don','Don')
weight <- c(70,71,58,57,59,70,69)
height <- c(175,175,165,165,165,180,180)
gender <- c('M','M','F','F','F','M','M')
record_day <- c(20200201,20200401,20200101,20200201,20200401,20200101,20200401)

example_data <- data.frame(name, weight, height,gender , record_day)
example_data


df_clean <- example_data %>%
    group_by(name) %>%
    arrange(desc(record_day)) %>%
    mutate(rnk = row_number()) %>%
    filter(rnk == 1)
df_clean


###### 
library(mlbench)
data("mtcars")
str(mtcars)

#install.packages("skimr")
library(skimr)
df <- mtcars
skimmed_data <- skim(df)
skimmed_data


## missing impute
 
## "center", "scale" have been automaticaly

#install.packages("RANN")
library(RANN)

imputed_model <- preProcess(df , method = "knnImpute")
df_imputed <- predict(imputed_model , newdata = df)
skim(df_imputed)
df_imputed[9:12,"hp"]



### preprocess test only
scale_center <- preProcess(df , method = c("center","scale"))
mtcars_standard <- predict(scale_center , newdata = mtcars)

mtcars_standard[9:12,"hp"]
############




### ensemble
# install.packages("caretEnsemble")
library(caretEnsemble)
library(caret)
set.seed(168)
df <- BostonHousing

num_train <- createDataPartition(y= df$medv , p =0.7 , list = FALSE)
train_data <- BostonHousing[num_train,]
test_data <- BostonHousing[-num_train,]

tc <- trainControl(method = "cv" , number = 5 ,index = createFolds(train_data$medv, 5) , savePredictions = "final")


model_list <- caretList(medv~. , data =  train_data ,trControl = tc , methodList = c("lm","svmRadial","rf","gbm","knn") ,continue_on_fail = FALSE , preProces = c("center","scale") )
model_list

resamp <- resamples(model_list)
dotplot(resamp, metric = "RMSE")
modelCor(resamp)

ensemble_model <- caretEnsemble(model_list, metric = "RMSE" ,trControl = tc )
summary(ensemble_model)

prediction_data <-test_data
prediction_data$pred <- predict(ensemble_model , newdata = test_data)
RMSE(prediction_data$pred , prediction_data$medv)



## linear and rf model for comparison
lm_model <- train(medv ~ ., data =  train_data , method = "lm" ,trControl = tc  )
rf_model <- train(medv ~ ., data =  train_data , method = "rf" ,trControl = tc  )

prediction_data$pred_lm <- predict(lm_model , newdata = test_data)
prediction_data$pred_rf <- predict(rf_model , newdata = test_data)
RMSE(prediction_data$pred_lm , prediction_data$medv)
RMSE(prediction_data$pred_rf , prediction_data$medv)




### correlation 
install.packages("PerformanceAnalytics")
install.packages("corrplot")
library(PerformanceAnalytics)
library(corrplot)
df <- mtcars
corrplot(cor(df), method = "number")
corrplot(cor(df), method = "square")

chart.Correlation(df)



## outlier
df <- mtcars
boxplot(df[, -which(names(df) == "mpg")] , col = "yellow")
boxplot(df[, -which(names(df) %in% c("mpg","disp","hp"))] , col = "yellow")


outlier_data <- which(df$hp > 300)
outlier_data <- which(df$qsec > 21)
outlier_data <- which(df$carb > 7)
df[outlier_data,]


### 








## dummyvar
library(caret)
str(mtcars)

str(BostonHousing)

df = BostonHousing
dummy_model <- dummyVars(medv ~. ,data = df)
dummied_data <- predict(dummy_model , newdata = df)
dummied_df <- data.frame(dummied_data)
str(dummied_df)


# preprocess data
df <- mtcars

#method name
#c("range",     0-1
#  "center",  -mean
#  "scale",    /sd
#  "Boxcox",  Removeskewness -> lead to normality
#  "Yeojohnson", Like Boxcox but works for negative value
#  "expoTrans" , Exponential Transformation work for negative value
#  "pca" , replace with pc
#  "Ica" ,replace with independent component
#  "spatialSign" , Project the data to a unit circle
#)
  
preprocess_model <- preProcess(df , method = c("center","scale"))
traindata <- predict(preprocess_model , newdata =df)
traindata$mpg <- mtcars$mpg
summary(traindata)

df <- mtcars
df[9:12,'hp'] <- NA
skim(df)


#split dataset


library(caret)
set.seed(168)
df <- mtcars

num_train <- createDataPartition(y= df$mpg , p =0.7 , list = FALSE)
train_data <- df[num_train,]
test_data <- df[-num_train,]

str(train_data)
str(test_data)


## scaling dataset
library(caret)
preprocess_model <- preProcess(train_data , method = c("center","scale"))
train_scaled <- predict(preprocess_model , newdata =train_data)
test_scaled <- predict(preprocess_model , newdata =test_data)
train_scaled$mpg <- train_data$mpg
test_scaled$mpg <- test_data$mpg


### train data
library(caret)
library(ranger)
set.seed(168)
fitControl <- trainControl(
  method = "repeatedcv",
  number = 3,
  repeats = 3)

randomforest_model_caret <- train(mpg~.,
                                  data = train_scaled,
                                  method = "ranger",
                                  importance = "permutation",
                                  trControl = fitControl,
                                  tuneGrid = expand.grid(
                                    mtry = seq(2,7,1),
                                    splitrule = "variance",
                                    min.node.size = seq(2,14,2)
                                  ))

randomforest_model_caret

df_importance <- data.frame(randomforest_model_caret$finalModel$variable.importance)
names(df_importance) <- "importance_score"
df_importance[order(df_importance$importance_score , decreasing = TRUE),,drop = FALSE]

## lasso model


library(caret)
library(glmnet)
set.seed(168)
fitControl <- trainControl(
  method = "repeatedcv",
  number = 3,
  repeats = 3)
lasso_model_caret <-      train(mpg ~ .,
                               data = train_scaled,
                               method = "glmnet",
                               trControl = fitControl,
                               tuneGrid = expand.grid(alpha = 0,
                                                      lambda = seq(0.01 ,6 ,length=18)
                                 ))
lasso_model_caret


coef(lasso_model_caret$finalModel, s = lasso_model_caret$bestTune$lambda)


# prediction
prediction_data <- test_scaled
prediction_data['pred_rf'] <- predict(randomforest_model_caret , newdata = prediction_data)
prediction_data['pred_lasso'] <- predict(lasso_model_caret , newdata = prediction_data)


prediction_data[c('mpg','pred_rf','pred_lasso')]

RMSE(prediction_data$pred_rf , prediction_data$mpg)
RMSE(prediction_data$pred_lasso , prediction_data$mpg)



## bostonhousing dataset
## scaling dataset
library(caret)
num_train <- createDataPartition(y= BostonHousing$medv , p =0.70 , list = FALSE) 
train_data <- BostonHousing[num_train,] 
test_data <- BostonHousing[-num_train,]
train_feature <- train_data[, -which(names(train_data) == "medv")]
scaling_model <- preProcess(train_feature, method = c("center","scale"))
train_scaled <- predict(scaling_model , newdata =train_data)
test_scaled <- predict(scaling_model , newdata =test_data)






### dataset spliting for linear regression








library(caret)
medv <- c(24,34.7,33.4,36.2,28.7,27.1,16.5)
rm <- c(6.575,7.185,6.998,7.147,6.43,6.172,5.631)
train_data <- data.frame(medv , rm)


medv <- c(21.6,22.9,18.9)
rm <- c(6.421,6.012,6.004)
test_data <- data.frame(medv , rm)

train_data
test_data

lm_model <- lm(medv ~ rm, data = train_data)
summary(lm_model)
test_data['pred'] <- predict(lm_model , newdata = test_data)
test_data
RMSE(test_data$medv , test_data$pred)



# test regression



head(BostonHousing)
write.csv(BostonHousing , "bostonhousing.csv")





linear_model_testing <- lm(medv ~ rm , data = BostonHousing)
summary(linear_model_testing)




############ validation set
library(dplyr)
library(caret)
sample_boston <- sample_n(BostonHousing, 24)
sample_boston

smp_size <- floor(0.875 * nrow(sample_boston))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(sample_boston)), size = smp_size)

train_valid <- sample_boston[train_ind, ]
test <- sample_boston[-train_ind, ]
test_data <- test
validation_data <- train_valid[1:3,]
train_data <- train_valid[4:nrow(train_valid),]



train_data
validation_data
test_data

write.csv(train_data2 , "train_data3.csv")
write.csv(validation_data2 , "validation_data3.csv")
write.csv(test_data2 , "test_data3.csv")
write.csv(sample_boston%>% select(medv , age , rm , lstat,tax) , "all_data3.csv")

library(rpart)
library(rpart.plot)



train_data2 <- train_data %>% select(medv , age , rm , lstat,tax)
validation_data2 <- validation_data %>% select(medv , age , rm , lstat,tax)
test_data2 <- test_data %>% select(medv , age , rm , lstat,tax)

train_dataset <- train_data2
validation_dataset <- validation_data2
test_dataset <- test_data2

rpart_model_1 <- rpart(medv ~ . , data = train_dataset , minsplit = 4 , cp = 0.001)
rpart.plot(rpart_model_1)
validation_dataset['pred_1'] <- predict(rpart_model_1 , newdata = validation_dataset)
RMSE(validation_dataset$pred_1 , validation_dataset$medv) 


rpart_model_2 <- rpart(medv ~ . , data = train_dataset , minsplit = 4 , cp = 0.005)
rpart.plot(rpart_model_2)
validation_dataset['pred_2'] <- predict(rpart_model_2 , newdata = validation_dataset)
RMSE(validation_dataset$pred_2 , validation_dataset$medv) 



test_dataset['pred'] <- predict(rpart_model_2 , newdata = test_dataset)
RMSE(test_dataset$pred , test_dataset$medv)

validation_dataset




### cross validation


############ validation set
library(dplyr)
library(caret)
sample_boston <- sample_n(BostonHousing, 30)
sample_boston

smp_size <- floor(0.84* nrow(sample_boston))
## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(sample_boston)), size = smp_size)

train_valid <- sample_boston[train_ind, ] %>% select(medv , age , rm , lstat,tax)
test <- sample_boston[-train_ind, ] %>% select(medv , age , rm , lstat,tax)


train_valid

test

write.csv(train_valid , "train_25.csv")
write.csv(test , "test_5.csv")
