salesDF <- read.csv("salesdata-set.csv")
View(salesDF)
dim(salesDF)

sales2DF <- read.csv("salesdata-set2.csv")
View(sales2DF)


sales2DF$holidaybin <- ifelse(sales2DF$IsHoliday == "TRUE",1,0)
View(sales2DF)

#===============logistic model===============================#

logitmod <- glm(sales2DF$holidaybin ~ sales2DF$Weekly_Sales)
summary(logitmod)

glmprob <- predict.glm(logitmod, type = "response")
glmprob

help(cor)
cor(sales2DF$holidaybin, sales2DF[,c(1,2,4)])
hist(glmprob)

#===================clustering===========================#
set.seed(1861)
install.packages("VIM")
install.packages("ggmap")
install.packages("ggplot2")
library(VIM)
library(ggmap)
library(ggplot2)

help(kmeans)
help(aggr)
?register_google
ggmap_show_api_key()
register_google(key = "AIzaSyDpjczK2fNmLLOe6f47EfLaHxsW2rVyPGc", write = TRUE)

#kmeans
aggr(salesDF)
df1 <- salesDF[,-c(1,2,3,7)]
View(df1)
dim(df1)
clust <- kmeans(df1[,c(5,6,12,20,21)], 6)
df1$clusters <- as.factor(clust$cluster)
str(clusters)

#df2 <- data.frame(matrix(unlist(df1), byrow = F, nrow = 211))
#df3 <- as.data.frame(lapply(df1, unlist))
#View(df3)
#help("as.data.frame")

usmap <- get_map("united states", zoom = 4)
ggmap(usmap) + geom_point(aes(x = df1$Longitude, y = df1$Latitude), colour = as.factor(df1$clusters), data = df1) 
  + ggtitle("US Cities demographics using KMean")

fin <- salesDF[c(which(df1$clusters == 5)),]
View(fin)
dim(fin)

install.packages("dbscan")
library(dbscan)
help(dbscan)
filter <- df1[,c(5,6,12,20,21)]
View(filter)
dist <- kNNdistplot(filter)
db <- dbscan(filter, eps = 100000, minPts = 3)
db

pairs(filter, col = db$cluster + 1L)

#============features modeling=============#
#cleaning dataset
features <- read.csv("Featuresdata4101.csv")
View(features)
dim(features)
features1 <- features[-which(is.na(features$Sum.of.Weekly.Sales)),]
View(features1)
dim(features1)
is.na(features1) <- sapply(features1, is.infinite)
features1[is.na(features1)] <- 0
features1$holidaybin <- ifelse(features1$IsHoliday == "TRUE",1,0)
features1 <- features1[,-12]
features1 <- features1[,-2]
features1 <- features1[,-1]
features1 <- features1[,-c(3:7)]
features1 <- features1[,-4]
features1 <- features1[,-3]
#features1$logsales <- log(features1$Sum.of.Weekly.Sales)

#split into train and testing
set.seed(1861)
trainSize <- 0.5
trainInd <- sample(1:nrow(features1), size = floor(nrow(features1) * trainSize))
train_set <- features1[trainInd, ]
test_set <- features1[-trainInd, ]
View(train_set)
View(test_set)

#linear models
lm1 <- lm(train_set$Sum.of.Weekly.Sales ~ train_set$CPI + train_set$holidaybin + train_set$Temperature + train_set$Fuel_Price + train_set$Unemployment)
lm2 <- lm(train_set$logsales ~ train_set$CPI + train_set$holidaybin + train_set$Temperature + train_set$Fuel_Price + train_set$Unemployment)

lm3 <- lm(train_set$Sum.of.Weekly.Sales ~ ., data = train_set)
lm4 <- lm(train_set$Sum.of.Weekly.Sales ~ train_set$White + train_set$Black + train_set$Asian + train_set$Hawaiin + train_set$Other + train_set$Multi + train_set$Hispanic)


summary(lm1)
summary(lm2)
summary(lm3)
summary(lm4)
lm1pred <- predict(lm1, data = test_set)
lm1predtrain <- predict(lm1, data = train_set)

lm2pred <- predict(lm2, data = test_set)
lm3pred <- predict(lm3, data = test_set)


RMSE <- function(true, preds) {sqrt(mean((true - preds)^2))}

lm1RMSE <- RMSE(test_set$Sum.of.Weekly.Sales, lm1pred)
lm1RMSE

lm2RMSE <- RMSE(test_set$logsales, lm2pred)
lm2RMSE

lm3RMSE <- RMSE(test_set$Sum.of.Weekly.Sales, lm3pred)
lm3RMSE

# forward stepwise selection
library(leaps)
help("regsubsets")
mod1 <- regsubsets(Sum.of.Weekly.Sales ~., data = features1, method ="forward", nvmax = 5)
plot(mod1, scale = "adjr2")
summary(mod1)
lm5 <- lm(Sum.of.Weekly.Sales ~ Med.HHld.Income + Household.Count + Mass.Transit + Other + Multi, data = train_set)
summary(lm5)
lm5pred <- predict(lm5, data = test_set)
lm5RMSE <- RMSE(test_set$Sum.of.Weekly.Sales, lm5pred)
lm5RMSE

#lasso
install.packages("glmnet")
library(glmnet)
library(glmnetUtils)
library(useful)
myformula <- as.formula(features1$Sum.of.Weekly.Sales ~ .)
Xvar <- build.x(formula = myformula, data= features1)
Yvar <- build.y(formula = myformula, data= features1)
myform <- as.formula(test_set$Sum.of.Weekly.Sales ~.)
Xvar2 <- build.x(formula = myform, data = test_set)
Yvar2 <- build.y(formula = myform, data = test_set)
LassoFit <- cv.glmnet(x = Xvar, y = Yvar, alpha = 1)
summary(LassoFit)
coef(LassoFit, s= "lambda.1se")
coef(LassoFit, s= "lambda.min")
plot(LassoFit)
r2Lasso.min <- LassoFit$glmnet.fit$dev.ratio[which(LassoFit$glmnet.fit$lambda == LassoFit$lambda.min)]
r2Lasso.1se <- LassoFit$glmnet.fit$dev.ratio[which(LassoFit$glmnet.fit$lambda == LassoFit$lambda.1se)]

r2Lasso.min
r2Lasso.1se

lasso.pred <- predict(LassoFit, s = "lambda.1se", newx = Xvar2)
lassoRMSE <- RMSE(test_set$Sum.of.Weekly.Sales, lasso.pred)
lassoRMSE
