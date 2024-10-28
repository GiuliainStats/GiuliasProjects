#####################################################
# OPTIMIZATION FOR STATISTICAL LEARNING: FINAL EXAM #
#####################################################

#Necessary libraries.
library(tidyr)
library(dplyr)
library(ggplot2)
library(h2o)
library(caret)
library(mclust)
library(stringr) 
library(cluster) 
library(factoextra)
library(purrr)
library(rpart) 
library(lattice)
library(rpart.plot)
library(vip)
library(rsample)
library(gbm) 
library(gridExtra)
library(vip)
library(viridisLite)
library(viridis)
library(ranger) 
library(ggrepel)


############################################
#1. PREPROCESSING AND EXPLORATORY ANALYSIS #
############################################

#Uploading the dataset
Sys.setenv(JAVA_HOME = "C:\\Program Files\\Java\\jdk-21")
setwd("D:/Desktop/OPTIMIZED MACHINE LEARNING/16. ESAME SOTTILE")
#(data <- read.csv("Dataset_aggiornato.csv", sep = ";"))
load("rdata_aggiornato.Rdata")
data1[1,]<-data[1,]
data<-data1
rm(data1)
View(data)
#NAs check
summary(data)
#no NAs values.

#remove unuseful variables for our purpose
data <- as_tibble(data)|>
  mutate(BMI = (Weight*10000)/(Height^2)) |>
  select(Age, BMI, everything()) |>
  select(- c(Sex, Weight, Height, Haircut, Greta_Thunberg, Andrew_Tate)) |>
  mutate(across(where(is.character), as.factor))

#Relabeling from italian to english
new_levels <- list(
  Mar_status = c("Celibe/Nubile", "Coniugato/a", "Separato/a", "Divorziato/a", "Vedovo/a"),
  Sex_orientation = c("Eterosessuale", "Omosessuale", "Bisessuale", "Asessuale", "Altro"),
  Occ_status = c ("Disoccupato", "Studente", "Studente lavoratore", "Lavoratore part-time", "Lavoratore full-time", "Pensionato"),
  Education = c( "Licenza elementare", "Licenza media", "Scuola superiore", "Laurea o superiore"),
  F_education = c ("Licenza elementare", "Licenza media", "Scuola superiore", "Laurea o superiore"),
  M_education = c("Licenza elementare", "Licenza media", "Scuola superiore", "Laurea o superiore"),
  PA_status = c("No", "Allenamento in palestra", "Sport individuali", "Sport di squadra", "Sport acquatici",  "Sport di combattimento", "Sport estremi", "Altro"),
  PA_freq = c("Mai", "Qualche volta", "Spesso", "Tutti i giorni"),
  Smoke_status = c("No", "Sigarette", "Iqos o simile", "Sigaretta elettronica",  "Marijuana"),
  Diet = c ("Onnivoro", "Vegetariano/Vegano", "Altro"),
  Meat_cons = c ("Mai", "1-2", "3-4", "5+"),
  Fish_cons = c ("Mai", "1-2", "3-4", "5+"),
  Legumes_cons = c ("Mai", "1-2", "3-4", "5+"),
  Vegetable_cons = c ("Mai", "1-2", "3-4", "5+"),
  Fruit_cons = c ("Mai", "1-2", "3-4", "5+"),
  Social_network = c("Mai", "Meno di 1 ora", "Tra 1 e 3 ore", "Più di 3 ore"),
  Free_time = c("Meno di 1 ora", "1-2 ore",  "2-4 ore", "Più di 4 ore"),
  Movies = c("Per niente", "Poco", "Abbastanza", "Molto"),
  Tv_series = c("Per niente", "Poco", "Abbastanza", "Molto"),
  Video_game = c("Per niente", "Poco", "Abbastanza", "Molto"),
  Romances = c("Per niente", "Poco", "Abbastanza", "Molto"),
  Chess = c("Per niente", "Poco", "Abbastanza", "Molto"),
  Essay = c("Per niente", "Poco", "Abbastanza", "Molto"),
  Theatre = c("Per niente", "Poco", "Abbastanza", "Molto"),
  Mental_wellness = c("Pessimo", "Mediocre", "Buono", "Eccellente"),
  Physical_wellness = c("Pessimo", "Mediocre", "Buono", "Eccellente"),
  Social_wellness = c("Pessimo", "Mediocre", "Buono", "Eccellente"),
  Financial_wellness = c("Pessimo", "Mediocre", "Buono", "Eccellente"),
  Career_wellness = c("Pessimo", "Mediocre", "Buono", "Eccellente"),
  Transport = c ("A piedi", "Bici/Monopattino", "Motociclo", "Automobile", "Trasporto pubblico",  "Altro"),
  Sleep_hours = c ("Meno di 6 ore", "6-7 ore", "7-8 ore", "Più di 8 ore"),
  Mood = c( "Gioia", "Serenità", "Indifferenza", "Tristezza", "Ansia", "Rabbia"),
  Volunteering = c("No", "Sì"),
  Children = c ("No", "Si"),
  Poligamy = c ("Non so", "Completamente in disaccordo", "Parzialmente in disaccordo", "Parzialmente in accordo", "Assolutamente d'accordo"),
  Coffee = c ("Mai", "1-2", "3-4", "5+"),
  Junkfood = c ("Mai", "1-2", "3-4", "5+"),
  Alcohol = c ("Mai", "1-2", "3-4", "5+"),
  Gambling = c("Per niente", "Poco", "Abbastanza", "Molto")
)

new_names <- list(
  Mar_status = c("Unmarried", "Married", "Separated", "Divorced", "Widow"), 
  Sex_orientation = c("Heterosexual", "Homosexual", "Bisesxual", "Asexual", "Other"),
  Occ_status = c("Unemployed", "Student", "Student worker", "Part-time worker", "Full-time worker", "Retired"),
  Education = c("Elementary degree", "Middle school", "High school", "Degree or higher"),
  F_education = c ("Elementary degree", "Middle school", "High school", "Degree or higher"),
  M_education = c("Elementary degree", "Middle school", "High school", "Degree or higher"),
  PA_status = c("No", "Gym training", "Individual sports", "Team sports", "Water sports", "Combat sports", "Extreme sports", "Other"),
  PA_freq = c("Never", "Sometimes", "Often", "Everyday"),
  Smoke_status = c("No", "Cigarettes", "Iqos or similar", "Electronic cigarettes",  "Marijuana"),
  Diet = c ("Omnivore", "Vegetarian/Vegan", "Other"),
  Meat_cons = c ("Never", "1-2", "3-4", "5+"),
  Fish_cons = c ("Never", "1-2", "3-4", "5+"),
  Legumes_cons = c ("Never", "1-2", "3-4", "5+"),
  Vegetable_cons = c ("Never", "1-2", "3-4", "5+"),
  Fruit_cons = c ("Never", "1-2", "3-4", "5+"),
  Social_network = c("Never", "Less than 1 hour", "Between 1 and 3 hours", "More than 3 hours"),
  Free_time = c("Less than 1 hour", "1-2 hours", "2-4 hours", "More than 4 hours"),
  Movies = c("Not at all", "A little", "Enough", "A lot"),
  Tv_series = c("Not at all", "A little", "Enough", "A lot"),
  Video_game = c("Not at all", "A little", "Enough", "A lot"),
  Romances = c("Not at all", "A little", "Enough", "A lot"),  
  Chess = c("Not at all", "A little", "Enough", "A lot"),    
  Essay = c("Not at all", "A little", "Enough", "A lot"),    
  Theatre = c("Not at all", "A little", "Enough", "A lot"),  
  Mental_wellness = c("Bad", "Mediocre", "Good", "Excellent"),
  Physical_wellness = c("Bad", "Mediocre", "Good", "Excellent"),
  Social_wellness = c("Bad", "Mediocre", "Good", "Excellent"),
  Financial_wellness = c("Bad", "Mediocre", "Good", "Excellent"),
  Career_wellness = c("Bad", "Mediocre", "Good", "Excellent"),
  Transport = c ("Walking", "Bicycle/Scooter", "Motorcycle", "Automobile", "Public transport",  "Other"),
  Sleep_hours = c ("Less than 6 hours", "6-7 hours", "7-8 hours", "More than 8 hours"),
  Mood = c("Joy", "Serenity", "Indifference", "Sadness", "Anxiety", "Anger"),
  Volunteering = c("No", "Yes"),
  Children = c ("No", "Yes"),
  Poligamy = c ("Don't know", "Completely disagree", "Partially disagree", "Partially agree", "Absolutely agree"),
  Coffee = c ("Never", "1-2", "3-4", "5+"),
  Junkfood = c ("Never", "1-2", "3-4", "5+"),
  Alcohol = c ("Never", "1-2", "3-4", "5+"),
  Gambling = c("Not at all", "A little", "Enough", "A lot")
)

#Relabeled dataset
(data <- data |>
  mutate(across(where(is.factor),
                ~ factor(., levels = new_levels[[cur_column()]], 
                         label = new_names[[cur_column()]]))))


################################################################################
#1.2 PREPARING DATA FOR THE CLASSIFICATION

#1.2.1 Standardization
data_s <- data 
data_s[, !sapply(data_s, is.factor)]<- data_s[, !sapply(data_s, is.factor)] |>   
  scale() 
summary(data_s) 

#1.2.2 One-hot encoding
#unclassed <- apply(data[, sapply(data, is.factor)], 2, unclass) 
dmy<-dummyVars(as.formula(paste("~", paste(colnames(data_s[, sapply(data_s, is.factor)]), collapse = "+"))), 
               data_s, fullRank = FALSE)
(data_prep<-cbind(data_s[, !sapply(data_s, is.factor)],                                     #numerical and standardized variables
                  data.frame(predict(dmy, newdata = data_s[, sapply(data_s, is.factor)])))) #one-hot encoded categorical variables
summary(data_prep)
str(data_prep)
data_prep
dim(data_prep)

write.csv(x=data_prep[514:524,], file="test_prep")

##########################
#2. DIMENSIONS REDUCTION #
##########################

#2.2 AUTOENCODERS
#An autoencoder is a neural network able to do dimension reduction.
#Number and dimension of the hidden layers: grid search
#For this purposes, we constrain the hidden layers so that the 
#number of neurons is less than the number of inputs.

#!!!!!!!!!!
# The grid results change at each run so the autoencoders values
# related to the best model where saved in a file.

# hyper_grid <- list(hidden = list(c(50), 
#                                  c(150),
#                                  c(100, 50, 10, 50, 100),
#                                  c(150, 100, 10, 100, 150)))

# ae_grid <- h2o.grid(algorithm = "deeplearning",
#                     x = seq_along(features), 
#                     training_frame = features,
#                     grid_id = "autoencoder_grid", autoencoder = TRUE,
#                     activation = "Tanh", 
#                     hype'r_params = hyper_grid,
#                     sparse = TRUE, 
#                     ignore_const_cols = FALSE, seed = 123)

#Sorting the hyperparameters combination according to the mse.
#h2o.getGrid("autoencoder_grid", sort_by = "mse", decreasing = FALSE)

#Extract the best model.
#best_model_id <- ae_grid@model_ids[[3]]
#(best_model <- h2o.getModel(best_model_id))

# -> the best dimension is given by a neural network with a single hidden layer 
#    with 150 neurons, but since the difference is very small we consider a structure 
#    with 5 hidden layers having 100, 50 and 10 neurons respectively.

h2o.init()
h2o.no_progress() 
h2o.init(max_mem_size = "5g") 
features <- as.h2o(data_prep)

autoc <-h2o.deeplearning(x = seq_along(features),
                         training_frame = features, seed = 1234, autoencoder = TRUE,
                         hidden = c(100, 50, 10, 50, 100), 
                         activation = "Tanh", sparse = FALSE)
autoc
# MSE = 0.097

#Extract the deep features:
#activation values for each neurons in the specified hidden layer.
#data_enc <- h2o.deepfeatures(best_model, features, layer = 3)
#data_enc

#(data_enc_total <- h2o.deepfeatures(autoc, features, layer = 3))
#write.csv(x=as.data.frame(data_enc_total), file="encoders2") #file to import each time
data_enc_total<-as.h2o(read.table("encoders2", sep=",", header=T, row.names = 1))

data_enc<-as.h2o(read.table("encoders", sep=",", header=T, row.names = 1))        #training
data_enc_test<-data_enc_total[513:523,]  #test

############################
#6. MODEL-BASED CLUSTERING #
############################
#It works by specifying the initial number of cluster k.
#K hyperparameter tuning by grid search.

options(scipen=T)

# -> it is necessary to use the reduced dataset.
# Computational demands are a significant limitation, especially
# in high-dimensional spaces. Classical model-based clustering 
# has disappointing computational performance in high-dimensional 
# spaces due to over-parameterization.
# Dimension reduction is a common approach to address
# computational issues, but it may lead to a loss of information
# relevant for discriminating groups.

#ENCODERS
mbc <- Mclust(data_enc, 1:20)
plot(mbc, what = "BIC",
     legendArgs = list(x = "bottomleft", ncol = 7))
summary(mbc)

mbc3 <- Mclust(data_enc, 3)
summary(mbc3)

#Mclust VII (spherical, varying volume)
# Clustering table:
#  1   2   3 
# 317 136  59 


#Choosing the model-based clustering method:

View(data)
data_512<-data[1:512, ]
data_512$mb_clustering<-mbc3$classification
View(data_512)

(data <- data_512|>
    mutate(mb_clustering = as.factor(mb_clustering)))



####################
#9. CLASSIFICATION #
####################

#Splitting the data in training e test sets
set.seed(1345)
data_split<-initial_split(data, prop = 0.8, strata = "mb_clustering") 
dim(data_train<-training(data_split)) #80% as training set (408)
dim(data_test<-testing(data_split))   #20% as test set (104)


#############################
#9.2 GRADIENT BOOSTED TREES #
#############################
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

# levels(data_train$mb_clustering) <- c("1", "2", "3")
# levels(data_test$mb_clustering) <- c("1", "2", "3")


#####################
#9.2.1 STANDARD GBM #
#####################

# -> At each iteration it computes the gradient on the whole dataset.

#5.1.1 HYPERPARAMETERS TUNING
#GRID SEARCH HYPERPARAMETERS TUNING: 
# - learning rate (shrinkage);
# - interaction.depth;
# - n.minobsinnode.
#(it takes about 20 mins)
# (hyper_grid <- expand.grid(learning_rate = c(0.1, 0.05, 0.01, 0.005),
#                            interaction.depth = 1:2,
#                            n.minobsinnode = c(10, 15),
#                            Loss_function = NA, 
#                            trees = NA, 
#                            time = NA))
# 
# for(i in seq_len(nrow(hyper_grid))) {
#   set.seed(123) #for reproducibility of the cv results
#   train_time <- system.time({
#     m <- gbm(formula = mb_clustering ~ . , 
#              data = data_train,
#              distribution = "multinomial", 
#              n.trees = 1000, 
#              shrinkage = hyper_grid$learning_rate[i], 
#              interaction.depth = hyper_grid$interaction.depth[i],  
#              n.minobsinnode = hyper_grid$n.minobsinnode[i],
#              cv.folds = 10, 
#              class.stratify.cv=TRUE, 
#              n.cores = 6)
#   })
#   
#   #For each lr value compute:
#   hyper_grid$Loss_function[i] <- min(m$cv.error) #minimum value of the loss function cross validated
#   hyper_grid$trees[i] <- which.min(m$cv.error)   #minimum number of trees in the ensemble  
#   hyper_grid$time[i] <- train_time[["elapsed"]]
# }
# 
# #The "optimal" hyperparameters combination is chosen by the mean of the cv value:
# arrange(hyper_grid, Loss_function)

# learning_rate      interaction.depth  n.minobsinnode  Loss_function  trees 
# 0.050              2                  15              0.2966201      207

#GBM with tuned hyperparameters.
set.seed(123)
basic_gbm <-gbm(formula = mb_clustering ~ . ,
                data = data_train,
                distribution = "multinomial", 
                n.trees = 400, 
                shrinkage = 0.05, 
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
#abline(v=207, lty=2, lwd=2, col="#CD2626")
abline(v=which.min(basic_gbm$cv.error), lty=2, lwd=2, col="#CD2626")

#find index for number trees with minimum CV error
which.min(basic_gbm$cv.error) #ensemble of 207 trees
min(basic_gbm$cv.error)       #with a cv of 0.2966201

#The minimum value of the cross validated loss function 
#is obtained with 207 ensembled trees.
#It is possible to see what happen with the test set performance.

pred = predict.gbm(object = basic_gbm,
                   newdata = data_test,
                   n.trees = 207,
                   type = "response")

(pred_clusters <- as.data.frame(round(pred))%>%
  rowwise() %>%
  mutate(cluster = which.max(c_across(everything()))) %>%
  select(cluster))

(cm_gbm <- confusionMatrix(as.factor(data_test$mb_clustering), as.factor(pred_clusters$cluster), positive = "1"))
colnames(cm_gbm$table) <- c("1", "2", "3")
rownames(cm_gbm$table) <- c("1", "2", "3")
#Accuracy : 0.9135
#for the third class the sensitivity is lower.




#CLUSTER CLASSIFICATION PREDICTION

#model-based assignment 
predict(mbc3, newdata=data_enc_test) 
#data_enc_test$mb_clustering<-  output di predict

#gbm cluster prediction
pred = predict.gbm(object = basic_gbm,
                   newdata = data_enc_test,
                   n.trees = 207,
                   type = "response")


cbind(data_enc_test$mb_clustering, pred)






