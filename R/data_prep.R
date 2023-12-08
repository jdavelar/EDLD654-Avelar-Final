#######################################
#### DATA PREPARATION & PROCESSING ####
#######################################

#### LIBRARIES + IMPORT DATA ####
library(tidyverse)
library(here)
library(rio)
library(recipes)
library(finalfit)
library(caret)
library(rattle)
library(DT)
library(cutpointr)
library(vip)

dat <- import(here("data", "hsls-student-clean.csv"))

#### INITIAL EXPLORATION ####
checks <- import(here("data", "hsls-student-clean.csv"))
glimpse(checks)

#barchart of dems
race_bar <- dat %>% 
  select(X1RACE) %>% 
  filter(!X1RACE == "") %>% 
  mutate(rate = rep(1, nrow(.))) %>% 
  group_by(X1RACE) %>% 
  summarize(sum = sum(rate),
            pct = sum/21928) %>% 
  ggplot(., aes(reorder(X1RACE, pct), pct)) +
  geom_col(fill = "#8D4559") +
  theme_minimal() +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  labs(x = "Student Race/Ethnicity",
       y = "N Observations",
       title = "Student Sample Racial and Ethnic Demographics") +
  coord_flip() +
  geom_text(aes(label = round(signif(pct), 2)), 
            nudge_y = 0.05, 
            color = "black",
            fontface = "bold",
            size = 3.5,
            family = "sans")
ggsave("output/student-race-barchart.jpeg", race_bar)

gender_bar <- dat %>% 
  select(X1SEX) %>% 
  filter(!is.na(X1SEX)) %>% 
  mutate(rate = rep(1, nrow(.))) %>% 
  group_by(X1SEX) %>% 
  summarize(sum = sum(rate),
            pct = sum/21928) %>% 
  ggplot(., aes(reorder(X1SEX, pct), pct)) +
  geom_col(fill = "#8D4559") +
  theme_minimal() +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  labs(x = "Student Gender",
       y = "N Observations",
       title = "Student Sample Gender Demographics") +
  #coord_flip() +
  geom_text(aes(label = round(signif(pct), 2)), 
            nudge_y = 0.02, 
            color = "black",
            fontface = "bold",
            size = 5,
            family = "sans")
ggsave("output/student-gender-barchart.jpeg", gender_bar)


#### CHECK MISSINGNESS ####

#80% threshold
missing_ <- ff_glimpse(dat)$Continuous
vars <- missing_ %>% 
  filter(missing_percent > 80)
vars <- as.vector(vars$label)
#rows 89, 90, 91, 93, 169 above 80% missing - drop
dat <- dat %>% 
  select(!all_of(vars))

#convert outcome var to binary variable for classification
dat <- dat %>% 
  mutate(X3TOUTCOME = case_when(
    X3TOUTCOME == "Left other reason" ~ 0,
    X3TOUTCOME == "Dropped out" ~ 0,
    X3TOUTCOME == "Transferred" ~ 0,
    X3TOUTCOME == "Status cannot be determined" ~ 0,
    X3TOUTCOME == "Pre-fall 2012 graduate" ~ 1,
    X3TOUTCOME == "Graduation date unknown" ~ 0,
    X3TOUTCOME == "Certificate of attendance" ~ 0,
    X3TOUTCOME == "Still enrolled" ~ 0,
    X3TOUTCOME == "Post-summer 2013 graduate" ~ 1,
    X3TOUTCOME == "Fall 2012-summer 2013 graduate" ~ 1
  ),
  X3TOUTCOME = factor(X3TOUTCOME, levels = c(0,1), labels = c("No", "Yes"))) %>% 
  #remove missing obs
  filter(!is.na(X3TOUTCOME))
#remove date vars - they do not seem useful and are introducing weirdness
dates <- c(colnames(dat)[grep("DATE|DOB", colnames(dat))]) #13
dat <- dat %>% 
  select(!all_of(dates))

#histogram of outcome var after modifying
graduation_bar <-
ggplot(dat, aes(X3TOUTCOME)) +
  geom_bar(fill = "#8D4559") +
  theme_minimal() +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Graduation",
       y = "N Observations",
       title = "Modified Binary Outcome Variable")
ggsave("output/graduation-barchart.jpeg", graduation_bar)

#### BUILD BLUEPRINT ####
# My blueprint will:
# - assign X3TOUTCOME as outcome (whether student graduated)
# - assign STU_ID as the ID variable
# - assign everything else as predictors
# - apply one-hot encoding for all categorical vars
# - create missing indicators for all predictors
# - remove variables with zero/near-zero variance
# - impute missing numeric values with mean (I'd like to use KNN but short on time)
# - impute missing character values with mode

# create vectors for:
# - ID
id <- c("STU_ID")
# - outcome
outcome <- c("X3TOUTCOME")
# - numbers (without ID)
nums <- dat %>% 
  select_if(is.numeric) %>% 
  select(!STU_ID)
nums <- colnames(nums) #260
# - categorical vars
cats <- dat %>% 
  select_if(is.character)
cats <- colnames(cats) #103
for(i in cats){
  dat[,i] <- as.factor(dat[,i])
}

# blueprint
blueprint <- recipe(x = dat,
                    vars = paste(c(outcome, id, nums, cats)),
                    roles = c("outcome", "id", rep("predictor", 363))) %>% 
  step_indicate_na(all_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_impute_mean(all_numeric_predictors()) %>%
  step_impute_mode(cats) %>% 
  step_dummy(cats, one_hot = TRUE)

#### SPLIT DATA ####

#training/testing - 80/20
set.seed(80208020)
samp  <- sample(1:nrow(dat), round(nrow(dat) * 0.8))
train <- dat[samp, ]
test  <- dat[-samp, ]
#check
dim(train)
dim(test)
#save
export(train, "data/training-data.csv")
export(test, "data/testing-data.csv")


#### 10 FOLD CROSS VALIDATION ####
#shuffle data
set.seed(123456)
train = train[sample(nrow(train)), ]
#10 folds
folds = cut(seq(1,nrow(train)), breaks = 10, labels = FALSE)
#empty list
indices <- vector('list', 10)
#loop
for(i in 1:10){
  indices[[i]] <- which(folds!=i)
}
#save
cv <- trainControl(method = "cv",
                   index  = indices,
                   classProbs = TRUE,
                   summaryFunction = mnLogLoss)

##########################################
#### TRAIN LOGISTIC REGRESSION MODEL #####
##########################################
logistic <- caret::train(blueprint, 
                          data      = train, 
                          method    = "glm",
                          family    = 'binomial',
                          metric    = 'logLoss',
                          trControl = cv)
#save model
saveRDS(logistic, "models/logistic.rds")

##########################################
#### TRAIN RIDGE CLASSIFICATION MODEL ####
##########################################

#create tuning grid
grid <- data.frame(alpha = 0, lambda = seq(0,0.5,.001)) 
#train model
ridge <- caret::train(blueprint, 
                      data      = train, 
                      method    = "glmnet", 
                      family    = "binomial",
                      metric    = "logLoss",
                      trControl = cv,
                      tuneGrid  = grid)
plot(ridge)
#optimal value
ridge$bestTune

#save model
saveRDS(ridge, "models/ridge.rds")


#############################################
#### TRAIN CLASSIFICATION TREE ALGORITHM ####
#############################################

#create tuning grid
grid <- data.frame(cp = seq(0,0.5,.01))

#train model
tree <- caret::train(blueprint,
                     data      = train,
                     method    = 'rpart',
                     tuneGrid  = grid,
                     trControl = cv,
                     metric    = 'logLoss',
                     control   = list(minsplit = 5,
                                      minbucket = 2,
                                      maxdepth = 10))
plot(tree)

#optimal value
tree$bestTune

#save model
saveRDS(tree, "models/tree.rds")




################################################
#### MODEL TESTING & PERFORMANCE EVALUATION ####
################################################

#### 1. LOGISTIC REGRESSION MODEL ####
#predictions
predictions <- predict(logistic, test, type='prob')
#save in case I want to access later
export(predictions, "data/logistic-predictions.csv")
#plots
# group0 <- which(test$X3TOUTCOME==0)
# group1 <- which(test$X3TOUTCOME==1)
# plot(density(predictions[group0,]$Yes,adjust=1.5),xlab='',main='')
# points(density(predictions[group1,]$Yes,adjust=1.5),lty=2,type='l')
# legend(x=0.8,y=2.75,c('No','Yes'),lty=c(1,2),bty='n')
#LL
# LL <- logistics$results %>% 
#   filter(lambda == ridge$bestTune$lambda) %>% 
#   .$logLoss
# LL <- round(LL, 3)
#AUC
cut.obj <- cutpointr(x     = predictions$Yes,
                     class = test$X3TOUTCOME,
                     method = maximize_metric,
                     metric = F1_score)
AUC <- round(auc(cut.obj), 3)
#confusion matrix
pred_class <- ifelse(predictions$Yes > .5, 1, 0)
confusion <- table(test$X3TOUTCOME, pred_class)
#true negative rate
TNR <- round(confusion[1,1]/(confusion[1,1] + confusion[1,2]), 3)
#true positive rate
TPR <- round(confusion[2,2]/(confusion[2,1]+confusion[2,2]), 3)
#false postive rate
FPR <- round(confusion[1,2]/(confusion[1,1]+confusion[1,2]), 3)
#false negative rate
FNR <- round(confusion[2,1]/(confusion[2,1]+confusion[2,2]), 3)
#accuracy
ACC <- round((TPR + TNR)/(TPR+TNR+FPR+FNR), 3)
#precision
PRE <- round(TPR/(TPR+FPR), 3)
plot(cut.obj)

#create vector
metrics <- c("Model", "LL", "AUC", "ACC", "TPR", "TNR", "PRE")
values <- c("Logistic Regression", NA, AUC, ACC, TPR, TNR, PRE)
logistic <- data.frame(t(values))
names(logistic) <- metrics
datatable(logistic)

#### 2. RIDGE CLASSIFICATION MODEL ####
# ~ winner winner chicken dinner ~

#predictions
predictions <- predict(ridge, test, type='prob')
#save in case I want to access later
export(predictions, "data/ridge-predictions.csv")
#LL
LL <- ridge$results %>% 
  filter(lambda == ridge$bestTune$lambda) %>% 
  .$logLoss
LL <- round(LL, 3)
#AUC
cut.obj <- cutpointr(x     = predictions$Yes,
                     class = test$X3TOUTCOME)
AUC <- round(auc(cut.obj), 3)
#confusion matrix
pred_class <- ifelse(predictions$Yes > .5, 1, 0)
confusion <- table(test$X3TOUTCOME, pred_class)
#true negative rate
TNR <- round(confusion[1,1]/(confusion[1,1] + confusion[1,2]), 3)
#true positive rate
TPR <- round(confusion[2,2]/(confusion[2,1]+confusion[2,2]), 3)
#false postive rate
FPR <- round(confusion[1,2]/(confusion[1,1]+confusion[1,2]), 3)
#false negative rate
FNR <- round(confusion[2,1]/(confusion[2,1]+confusion[2,2]), 3)
#accuracy
ACC <- round((TPR + TNR)/(TPR+TNR+FPR+FNR), 3)
#precision
PRE <- round(TPR/(TPR+FPR), 3)

#create a plot
jpeg("output/ridge_density_plot.jpeg")
group0 <- which(test$X3TOUTCOME == "No")
group1 <- which(test$X3TOUTCOME == "Yes")
plot(density(predictions[group0,]$Yes,adjust=1.5),xlab='',main='')
points(density(predictions[group1,]$Yes,adjust=1.5),lty=2,type='l')
legend(x="topright",c('No', 'Yes'), lty = c(1,2), bty = 'n')
dev.off()

#performance table
metrics <- c("Model", "LL", "AUC", "ACC", "TPR", "TNR", "PRE")
values <- c("Ridge Classification", LL, AUC, ACC, TPR, TNR, PRE)
ridge <- data.frame(t(values))
names(ridge) <- metrics
performance <- bind_rows(logistic, ridge)
datatable(performance)

#most important predictors
preds <- vip(ridge, num_features = 10, geom = "point") + theme_bw()
ggsave("output/top-predictors.jpeg", preds)


#### 3. CLASSIFICATION TREE MODEL ####
#predictions
predictions <- predict(tree, test, type='prob')
export(predictions, "data/tree-predictions.csv")
#AUC
cut.obj <- cutpointr(x     = predictions$Yes,
                       class = test$X3TOUTCOME)
AUC <- round(auc(cut.obj), 3)
#confusion matrix
pred_class <- ifelse(predictions$Yes > .5, 1, 0)
confusion <- table(test$X3TOUTCOME, pred_class)
#true negative rate
TNR <- round(confusion[1,1]/(confusion[1,1] + confusion[1,2]), 3)
#true positive rate
TPR <- round(confusion[2,2]/(confusion[2,1]+confusion[2,2]), 3)
#false postive rate
FPR <- round(confusion[1,2]/(confusion[1,1]+confusion[1,2]), 3)
#false negative rate
FNR <- round(confusion[2,1]/(confusion[2,1]+confusion[2,2]), 3)
#accuracy
ACC <- round((TPR + TNR)/(TPR+TNR+FPR+FNR), 3)
#precision
PRE <- round(TPR/(TPR+FPR), 3)

#create a plot
jpeg("output/tree_density_plot.jpeg")
group0 <- which(test$X3TOUTCOME == "No")
group1 <- which(test$X3TOUTCOME == "Yes")
plot(density(predictions[group0,]$Yes,adjust=1.5),xlab='',main='')
points(density(predictions[group1,]$Yes,adjust=1.5),lty=2,type='l')
legend(x="topright",c('No', 'Yes'), lty = c(1,2), bty = 'n')
dev.off()

#performance metrics
values <- c("Classification Tree", LL, AUC, ACC, TPR, TNR, PRE)
values <- data.frame(t(values))
names(values) <- metrics
performance <- bind_rows(performance, values)
datatable(performance)
export(performance, "output/final-model-performance.csv")
