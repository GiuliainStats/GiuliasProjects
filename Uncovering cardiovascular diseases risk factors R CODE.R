#ASSESSING THE PRESENCE OF A CARDIOVASCULAR DISEASE
#Lumia, Bongiovanni

library(rsample)
library(dplyr)

########################
#0. DATA PREPROCESSING #
########################

(data<-read.csv("cardiovascular.csv"))
(data<-data[, -c(1,2)]) #the first two id column are useless for the purpose of the analysis
str(data) 
summary(data) 
#no NA values
#balanced dataset with respect to the presence of a cardiological disease. 


hist((data$age/365))
#Since data refer to adults the information about the systolic and diastolic blood 
#pressure can be summarized according to the American college of cardiology guidelines:
# - normal, S<120 & D<80
# - elevated, 120<=S<=129 & D<80;
# - hypertension stage 1, 130<=S<=139 or 80<=D<=89;
# - hypertension stage 2, 140<=S or 90<=D;
# - hypertensive crisis, 180<=S or 120<=D.

(data <- data |>                    #SYSTOLIC                DIASTOLIC  
    mutate(blood_pressure = factor(case_when(ap_hi < 120           & ap_lo < 80 ~ "normal",
                                             ap_hi %in% c(120:129) & ap_lo < 80 ~ "elevated",
                                             ap_hi %in% c(130:139) | ap_lo %in% c(80:89)  ~ "hypert_S1",
                                             ap_hi %in% c(140:179) | ap_lo %in% c(90:119) ~ "hypert_S2",
                                             ap_hi >=180           | ap_lo >= 120 ~ "hypert_crisis"),
                                   levels=c("normal", "elevated", "hypert_S1", "hypert_S2", "hypert_crisis")),
           
           #The sex dummy variable is labelled as 1: female, 2: male.         
           gender = factor(gender, labels = c("female", "male")),
           
           #Age is transformed from days to years in order to be categorized.
           age = round(age/365),
           height = height/100,    #from cm to m
           
           #The info about the weight can be summarized according to the BMI, that take 
           #into account also the height and the sex of the patient.
           BMI = weight/(height^2)))
str(data)
summary(data)

#Corrections
#There are some errors to remove:
# - the max height is 2.4;
# - the min value of BMI is 3.47 while the max is 298,67;
# - there can't exist cases in which the systolic blood pressure is lower than the diastolic;
# - blood pressure can't be negative.
(data<-data|>
    filter(BMI>10,BMI<70)|>
    filter(height<2.4) |>
    filter(ap_hi>ap_lo, 
           ap_hi>50,
           ap_hi<250,
           ap_lo>19,
           ap_lo<200)|>
    select(-ap_hi, -ap_lo, -weight))


#BMI
quantile(data$BMI)
data$class_BMI = factor(ifelse(data$gender == "female",
                               cut(data$BMI, breaks = c(-Inf, 18.7, 23.8, 28.6, 35, 40, Inf)),
                               cut(data$BMI, breaks = c(-Inf, 20.1, 25, 29.9, 35, 40, Inf))))
levels(data$class_BMI) <- c("underweight", "normal", "overweight", "obese_S1", 
                            "obese_S2", "obese_S3")
str(data)

#Let's transform these variables in factors.
(data<-data |>
    select(-BMI) |>
    mutate_at(vars(cholesterol, gluc, smoke, alco, active, cardio), as.factor))

#Remove the coding.
levels(data$cholesterol) <- c("normal", "above normal", "well above normal")
levels(data$gluc) <- c("normal", "above normal", "well above normal")
levels(data$smoke) <- c("no", "yes")
levels(data$alco) <- c("no", "yes")
levels(data$active) <- c("no", "yes")
levels(data$cardio) <- c("no", "yes")
names(data) <- c("AGE", "GENDER", "HEIGHT", "CHOLESTEROL", "GLUCOSE", "SMOKING",
                 "ALCOHOL_INTAKE", "PHYSICAL_ACTIVITY", "CARDIO",
                 "BLOOD_PRESSURE", "BMI")
str(data)

######################
#1. TRAIN-TEST SPLIT #
######################
#   - 80% to train the model;
#   - 20% for the doctor to test its performance.
set.seed(1353)
data_split<-initial_split(data, prop = 0.8, strata = "CARDIO") 
dim(data_train<-training(data_split)) #80% as training set (43941)
dim(data_test<-testing(data_split))   #20% as test set (10986)


##########################
#2. EXPLORATORY ANALYSIS #
##########################
library(ggplot2)
library(gridExtra)

#discriminant power of the categorical variables
(variables_c <- c("GENDER", "CHOLESTEROL", "GLUCOSE", "SMOKING",
                  "ALCOHOL_INTAKE", "PHYSICAL_ACTIVITY",
                  "BLOOD_PRESSURE", "BMI"))
colors <- c("#66CD00", "darkcyan")

plots <- list()
for (i in 1:length(variables_c)) {
  variable <- variables_c[i]
  plot <- ggplot(data_train, aes_string(x = variables_c[i], fill ="CARDIO")) +
    geom_bar(position = "fill", show.legend = FALSE) +
    labs(x = variables_c[i], y="", title = paste("CARDIOVASCULAR DISEASE |", variables_c[i]))+
    scale_fill_manual( values = colors, name= "CARDIO")+
    scale_y_continuous(labels = scales::percent) + 
    theme_minimal()
  plots[[i]] <- plot
}

x11()
grid.arrange(grobs = plots, ncol = 2, nrow=4)


#discriminant power of the numerical variables
p1 <- ggplot(data_train, aes(x = factor(CARDIO), y = AGE, fill = factor(CARDIO))) +
  geom_boxplot(fill = colors) +
  labs(x = "CARDIO", y = "AGE", title = "AGE | CARDIO")

p2 <- ggplot(data_train, aes(x = factor(CARDIO), y = HEIGHT, fill = factor(CARDIO))) +
  geom_boxplot(fill = colors) +
  labs(x = "CARDIO", y = "HEIGHT", title = "HEIGHT | CARDIO")

grid.arrange(p1, p2, nrow = 1, ncol = 2)


##########################
#3. SINGLE DECISION TREE #
##########################
#Decision trees work by partitioning the feature space into a number 
#of smaller (non-overlapping) regions with similar response values 
#using a set of splitting rules.
#They can be used for both classification and regression problems.
#For classification problems, the partitioning is usually made to
#maximize the reduction in cross-entropy or the Gini index.

library(rpart) 
library(lattice)
library(caret) 
library(rpart.plot)
library(vip) 

# -> CART ALGORITHM (binary recursive partitions of data subgroups)
(tree <- rpart(formula = CARDIO ~ ., 
               data = data_train, 
               method = "class",
               parms=list(split="Gini")))
rpart.plot(tree, type = 5, box.palette=list("#66CD00", "darkcyan"))
# To compare the error for each alpha value,
# rpart() performs a 10-fold CV (by default).
plotcp(tree, col="#CD2626", lty=5)
#Since the selected tree with 6 leaves is the only one that respect the 1SE 
#empirical rule there is no need to prune it.

#variable importance

#??
#To measure feature importance, the reduction in the loss function (GINI)
#attributed to each variable at each split is tabulated.
#These values are standardized so that the most
#important feature has a value of 100 and the remaining features
#are scored based on their relative reduction in the loss function.

#alternative
#vip(tree, bar = FALSE, num_features = 4)
barplot(tree$variable.importance[1:4],  
        main="FEATURE IMPORTANCE", 
        ylim=c(0, 3000), col=hcl.colors(10, palette = "viridis"), cex.names=0.95)

summary(tree)
#Rescaled variable importance (summary output)
#blood_pressure    cholesterol            age           gluc 
#64                17                     16            3 
(imp<-c(64, 17, 16, 3))
names(imp)<- c("BLOOD_PRESSURE", "CHOLESTEROL", "AGE", "GLUCOSE")

barplot(imp,  
        main="FEATURE IMPORTANCE", 
        ylim=c(0, 100), col=hcl.colors(10, palette = "viridis"), cex.names=0.95)

#ASSESSING THE SINGLE DECISION TREE PERFORMANCE
test_tree<- predict(tree, 
                    newdata = data_test, 
                    type = "class") 

#confusion matrix sulle previsioni del test set
confm_tree<-confusionMatrix(data = test_tree, 
                            reference = data_test$CARDIO, 
                            positive = "yes")
rownames(confm_tree$table) <- c("No", "Yes")
colnames(confm_tree$table) <- c("No", "Yes")
confm_tree$table

confm_tree$byClass

confm_tree$overall[1]                        #accuracy
(err_tree<- 1-unname(confm_tree$overall[1])) #misclassification rate


###################
#4. RANDOM FOREST #
###################
#Random forests are an extension of bagged decision trees in which not only 
#B independent trees are built on the base of B samples (with or without replacement)
#but also the search for the split variable is limited to a random subset 
#of mtry of the original p features.
library(ranger) 

p <- length(setdiff(names(data_train), "CARDIO")) #number of features

#4.1 HYPERPARAMETERS TUNING
#Random forests involve some hyperparameters to tune ex. by the mean of grid search:
# - n_trees in the forest;
# - mtry: number of variables to consider at each split;
# - min.node.size: minimum size of each node; 
# - replace: whether to sample with or without replacement.
# -> n_trees and mtry have the largest impact on results.


hyper_grid <- expand.grid(min.node.size = c(5, 10),
                          replace = c(TRUE, FALSE),
                          sample.fraction = c(.5, .6, .7),
                          misclass_rate = NA)

#Grid search -> fit a forest for each hyperparameters combination.
for(i in seq_len(nrow(hyper_grid))) {
  fit <- ranger(formula = CARDIO ~ .,
                data = data_train,
                num.trees = 600, 
                mtry = 3,
                min.node.size = hyper_grid$min.node.size[i],
                replace = hyper_grid$replace[i],
                sample.fraction = hyper_grid$sample.fraction[i],
                verbose = FALSE, seed = 123,
                respect.unordered.factors = 'order')
  hyper_grid$misclass_rate[i] <- fit$prediction.error
}

#The "best" hyperparameters combination can be chosen on the base of the overall
#out of bag prediction error. For classification it is computed as the fraction 
#of missclassified samples. Moreover the improvement with respect to the single 
#tree is shown.

hyper_grid %>%
  arrange(misclass_rate) %>%
  mutate(perc_var = ((err_tree - misclass_rate) / err_tree * 100)) %>%
  head(10)

#min.node.size  replace    sample.fraction    misclass_rate   perc_var
#5              FALSE      0.5                0.2996290       0.55212321


tuning_grid <- expand.grid(
  mtry = c(2,3,4,7,10),
  trees = seq(10, 1000, by = 20),
  misclass_rate  = NA
)

for (i in seq_len(nrow(tuning_grid))) {
  forest <- ranger(formula = CARDIO ~ .,
                   data = data_train,
                   num.trees = tuning_grid$trees[i], 
                   mtry = tuning_grid$mtry[i],
                   min.node.size = 5,         #hyperparameters previously tuned
                   replace = FALSE,
                   sample.fraction = 0.5,
                   verbose = TRUE, seed = 123,
                   respect.unordered.factors = 'order')  
  tuning_grid$misclass_rate[i] <- forest$prediction.error
}


tuning_grid %>%
  arrange(misclass_rate) %>%
  head(10)
#230 trees made up by sampling 2 variables at each split are enough to obtain 
#a good misclassification rate (OOB).


#Plot of the performances of each combinations
labels <- tuning_grid %>%
  filter(trees == 980) %>%
  mutate(mtry = as.factor(mtry))

library(ggrepel)

tuning_grid %>%
  mutate(mtry = as.factor(mtry)) %>%
  ggplot(aes(trees, misclass_rate, color = mtry)) +
  geom_line(size=1, show.legend = TRUE) +
  geom_text_repel(data = labels, aes(trees, misclass_rate, label = mtry), nudge_x = 50, show.legend = TRUE) +
  ylab("OOB - Misclassification rate") +
  xlab("Number of trees")+
  ggtitle("RANDOM FOREST PERFORMANCE")


#4.2 OPTIMAL RANDOM FOREST WITH TUNED PARAMETERS
opt_rf <- ranger(formula = CARDIO ~ ., data = data_train,
                 num.trees = 230, 
                 mtry = 2,
                 min.node.size = 5,
                 replace = FALSE,
                 sample.fraction = 0.5,
                 verbose = FALSE, seed = 123,
                 respect.unordered.factors = 'order')
opt_rf$prediction.error #OOB misclassification rate

#OPTIMAL RANDOM FOREST PERFORMANCE:
test_rf <- predict(opt_rf, data = data_test)
confm_rf <- confusionMatrix(test_rf$predictions, data_test$CARDIO)
rownames(confm_rf$table) <- c("No", "Yes")
colnames(confm_rf$table) <- c("No", "Yes")

#COMPARISON SINGLE TREE - RANDOM FOREST
#Confusion Matrix 
confm_rf$table   #rf
confm_tree$table #tree

#Statistics:
confm_rf$byClass   #rf
confm_tree$byClass #tree

#Accuracy:
confm_rf$overall[1]   #rf
confm_tree$overall[1] #tree

#Confusion Matrix
grid.arrange(grobs=list((
  ggplot(as.data.frame(confm_tree$table), 
         aes(y=Prediction, x=Reference, fill= Freq)) +
    geom_tile() + 
    geom_text(aes(label=Freq)) + 
    ggtitle("CONFUSION MATRIX OPTIMAL SINGLE TREE")+
    scale_fill_gradient(low="white", high="#008B00") + 
    theme(legend.position = "none")),
  (ggplot(as.data.frame(confm_rf$table), 
           aes(y=Prediction, x=Reference, fill= Freq)) +
      geom_tile() + 
      geom_text(aes(label=Freq)) + 
      ggtitle("CONFUSION MATRIX OPTIMAL RANDOM FOREST")+
      scale_fill_gradient(low="white", high="#008B00") + 
      theme(legend.position = "right"))),
  nrow = 1, ncol = 2)


#4.3 VARIABLE IMPORTANCE 
rf_impurity <- ranger(formula = CARDIO ~ ., data = data_train,
       num.trees = 230, 
       mtry = 2,
       min.node.size = 5,replace = FALSE,
       sample.fraction = 0.5,
       importance = "impurity",
       verbose = FALSE, seed = 123,
       respect.unordered.factors = 'order')

rf_impurity$variable.importance
rf_impurity$confusion.matrix


rf_permutation <- ranger(formula = CARDIO ~ ., data = data_train,
                      num.trees = 230, 
                      mtry = 2,
                      min.node.size = 5,
                      replace = FALSE,
                      sample.fraction = 0.5,
                      importance = "permutation",
                      verbose = FALSE, seed = 123,
                      respect.unordered.factors = 'order')

rf_permutation$variable.importance
rf_impurity$confusion.matrix
#results using two different ways of computing the variable importance are similar.

library(vip)
library(viridisLite)
library(viridis)
p1 <- vip(rf_impurity, p = p,
          aesthetics = list(fill = viridis(10)))+
  labs(y = "Importance (impurity)")
p2 <- vip(rf_permutation, p = p, 
          aesthetics = list(fill = viridis(10)))+
  labs(y = "Importance (permutation)")
grid.arrange(p1, p2, nrow = 1)

############################
#4. GRADIENT BOOSTED TREES #
############################
#Gradient boosting is a machine learning technique used in regression and 
#classification tasks. It gives a prediction model that is an ensemble
#of various combined weak learners, tipically trees.
#A weak model is one whose error rate is only slightly better than random guessing.
#Whereas random forests build an ensemble of deep independent trees, 
#GBMs build an ensemble of shallow trees in sequence with each tree 
#learning and improving on the previous one.
#Gradient-boosted trees, which are built using decision trees as the weak learner,
#usually outperform random forests.
#Boosted trees are grown sequentially; each tree is grown using information from
#previously grown trees to improve performance. Each tree is trained on the errors
#of the previous.
#The key idea behind gradient boosting is to optimize an arbitrary differentiable 
#loss function by using gradient descent optimization algorithm.
#Instead of optimizing a single model we optimize an ensemble of models.

library(gbm) 

levels(data_train$CARDIO) <- c("0", "1")
levels(data_test$CARDIO) <- c("0", "1")

###################
#5.1 STANDARD GBM #
###################

# -> At each iteration it computes the gradient on the whole dataset.

#5.1.1 HYPERPARAMETERS TUNING
#GRID SEARCH HYPERPARAMETERS TUNING: 
# - learning rate (shrinkage);
# - interaction.depth;
# - n.minobsinnode.
#(it takes about 20 mins)
(hyper_grid <- expand.grid(learning_rate = c(0.1, 0.05, 0.01, 0.005),
                           interaction.depth = 1:2,
                           n.minobsinnode = c(10, 15),
                           Bern_dev = NA, 
                           trees = NA, 
                           time = NA))

for(i in seq_len(nrow(hyper_grid))) {
  set.seed(123) #for reproducibility of the cv results
  train_time <- system.time({
    m <- gbm(formula = as.numeric(as.character(CARDIO)) ~ . , 
             data = data_train,
             distribution = "bernoulli", 
             n.trees = 1000, 
             shrinkage = hyper_grid$learning_rate[i], 
             interaction.depth = hyper_grid$interaction.depth[i],  
             n.minobsinnode = hyper_grid$n.minobsinnode[i],
             cv.folds = 10, 
             class.stratify.cv=TRUE, 
             n.cores = 6)
  })
  
  #For each lr value compute:
  hyper_grid$Bern_dev[i] <- min(m$cv.error)      #minimum value of the loss function cross validated
  hyper_grid$trees[i] <- which.min(m$cv.error)   #minimum number of trees in the ensemble  
  hyper_grid$time[i] <- train_time[["elapsed"]]
}

#The "optimal" hyperparameters combination is chosen by the mean of the cv value:
#the best combination is given by 0.050, 2, 15 but it requires 886 trees.
#With an irrelevant worsening of 0.0003 it is possible to halve the number of 
#trees to 406, changing the learning value of that combination from 0.05 to 0.1.
arrange(hyper_grid, Bern_dev)

#GBM with tuned hyperparameters.
set.seed(123)
basic_gbm <-gbm(formula = as.numeric(as.character(CARDIO)) ~ . ,
                data = data_train,
                distribution = "bernoulli", 
                n.trees = 1000, 
                shrinkage = 0.1, 
                interaction.depth = 2, 
                n.minobsinnode = 15,
                cv.folds = 10, 
                class.stratify.cv=TRUE, 
                n.cores = 6)

#gbm.perf(basic_gbm, method = "cv") we can do better...

plot(basic_gbm$cv.error, type = "l", lwd=2, 
     xlab="NUMBER OF ITERATIONS = NUMBER OF TREES",
     ylab="CROSS VALIDATED LOSS FUNCTION",
     main = "BASIC GRADIENT BOOSTING PERFORMANCE")
abline(v=which.min(basic_gbm$cv.error), lty=2, lwd=2, col="#CD2626")

#find index for number trees with minimum CV error
which.min(basic_gbm$cv.error) #ensemble of 406 trees
min(basic_gbm$cv.error)       #with a cv 
#The minimum value of the cross validated loss function i.e. the bernoulli deviance,
#it's obtained with about 400 ensembled trees.
#The plot shows that about 200 ensembled trees may be enough to obtain a similar performance.
#It is possible to see what happen with the test set performance.

pred = predict.gbm(object = basic_gbm,
                   newdata = data_test,
                   n.trees = 406,
                   type = "response")
round(pred) #cut-off of 0.5.

#For the Bernoulli loss the returned value is on the log odds scale.
#Specifying type = "response" we get probabilities.
#so we'll get class names with the highest prediction value.

(cm_gbm <- confusionMatrix(as.factor(data_test$CARDIO), as.factor(round(pred)), positive = "1"))
colnames(cm_gbm$table) <- c("No", "Yes")
rownames(cm_gbm$table) <- c("No", "Yes")
#Accuracy : 0.706 


#####################
#5.2 STOCHASTIC GBM #
#####################

# -> At each iteration it computes the gradient on a random subsample of the dataset.

#Stochastic Gradient Boosting is a variant of the basic gradient boosting that
#introduces randomness into the model to make it more robust and prevent overfitting.

library(h2o)

#5.2.1 HYPERPARAMETERS TUNING
hyper_grid <- list(sample_rate = c(0.25, 0.5, 0.75))

# convert training data to h2o object
h2o.no_progress()
h2o.init()
(train_h2o <- as.h2o(data_train))

(grid <- h2o.grid(algorithm = "gbm", 
                  grid_id = "gbm_grid",
                  x = setdiff(colnames(data_train), "CARDIO"), 
                  y = "CARDIO",
                  training_frame=train_h2o,
                  hyper_params = hyper_grid, 
                  ntrees = 1000, 
                  learn_rate = 0.1,   #hyperparameters 
                  max_depth = 2,      #previously 
                  min_rows = 15,      #tuned
                  nfolds = 10,
                  stopping_rounds = 10, 
                  stopping_tolerance = 0.001,
                  stopping_metric = "misclassification",
                  seed = 123))

#Collect the results and sort by the selected metric (accuracy)
grid_perf <- h2o.getGrid(grid_id = "gbm_grid", sort_by = "accuracy",
                         decreasing = TRUE)

best_model_id <- grid_perf@model_ids[[1]]
best_model <- h2o.getModel(best_model_id) #get the best model


#Predictions on the test set
(test_h2o <- as.h2o(data_test))
predictions <- h2o.predict(best_model, test_h2o)


con.h2o <- h2o.confusionMatrix(best_model, test_h2o)

#Confusion matrix stochastic GBM
con.h2 <- h2o.confusionMatrix(best_model, test_h2o)

cm_gbm.s <- matrix(c(2373,3187,638,4788), nrow = 2, byrow = TRUE)
rownames(cm_gbm.s) <- c("No", "Yes")
colnames(cm_gbm.s) <- c("No", "Yes")

cm_gbm.s <- as.data.frame(as.table(cm_gbm.s))
names(cm_gbm.s) <- c("Reference", "Prediction", "Freq")


#Comparison between two version of gbm
grid.arrange(grobs=list((
  ggplot(as.data.frame(cm_gbm$table), 
         aes(y=Prediction, x=Reference, fill= Freq)) +
    geom_tile() + 
    geom_text(aes(label=Freq)) + 
    ggtitle("CONFUSION MATRIX BASIC GBM")+
    scale_fill_gradient(low="white", high="#008B00") + 
    theme(legend.position = "none")),
  (ggplot(cm_gbm.s, 
          aes(y=Prediction, x=Reference, fill= Freq)) +
     geom_tile() + 
     geom_text(aes(label=Freq)) + 
     ggtitle("CONFUSION MATRIX STOCHASTIC GBM")+
     scale_fill_gradient(low="white", high="#008B00") + 
     theme(legend.position = "none"))),
  nrow = 1, ncol = 2)


#Comparison between variable importance in the two GBM versions.
p3 <- vip(basic_gbm, aesthetics = list(fill = viridis(10)))+
  labs(y = "Importance (impurity) - BASIC GBM")
p4 <- vip(best_model, aesthetics = list(fill = viridis(10)))+  
  labs(y = "Importance (impurity) - STOCHASTIC GBM")
grid.arrange(p3, p4, nrow = 1)

