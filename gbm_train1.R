library(predictrace)  
library(rethnicity)  
library(caret)
library(h2o)
#Sys.setenv(JAVA_HOME = "D:/Program Files/Java/jdk-18.0.1.1")


#import the wpct file to build gbm model and then import test wpct data to make predictions 


#setwd("D:/Downloads_D/mobility_data/v2/demo")
#data <- read.csv("labelled_train_features_data_wpct_forgbm.csv",sep = ',')
#head(data)
#dim(data)



h2o.init()

data <- h2o.importFile("./labelled_train_features_data_wpct_forgbm1.csv")
data$eth1<-as.factor(data$Ethnic)

predictors<-c("pred_ethn_full_prob_asian","pred_ethn_full_prob_black",
              "pred_ethn_full_prob_hispanic",	"pred_ethn_full_prob_white",
              "pred_ethn_lname_prob_asian",	"pred_ethn_lname_prob_black",
              "pred_ethn_lname_prob_hispanic",	"pred_ethn_lname_prob_white",
              "pred_race_lname_probability_american_indian",
              "pred_race_lname_probability_asian",
              "pred_race_lname_probability_black",
              "pred_race_lname_probability_hispanic",
              "pred_race_lname_probability_white",
              "pred_race_lname_probability_2races")

response<-"Ethnic"

race_gbm <- h2o.gbm(x = predictors,
                    y = response,
                    nfolds = 5,
                    seed = 1111,
                    keep_cross_validation_predictions = TRUE,
                    training_frame = data)

#perf <- h2o.performance(race_gbm)

#save(race_gbm , file = 'GBM_raceclass.rda')
