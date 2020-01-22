library(Boruta)
library(caret)


modify_phenotypes <- function (phenotype_df) {
  phenotype_df <- phenotype_df %>% mutate (CANCER_TYPE = 
                            na_if(CANCER_TYPE, "UNCLASSIFIED")) %>%
                            mutate (CANCER_TYPE = 
                            na_if(CANCER_TYPE, "UNKNOWN")) 
  return(phenotype_df)
}


unbalancedFeatureSelectionFromBoruta <- function (train.data=train.data, seed=seed,
                                                  pval = 0.01) {
  
  
  flag <- which.max(c(length(which(train.data$class == 0)),length(which(train.data$class == 1))))
  
  minority.class.data <- train.data[which(train.data$class != (flag-1)),]
  majority.class.data <- train.data[which(train.data$class == (flag-1)),]
  class.times <- floor(nrow(majority.class.data)/nrow(minority.class.data))
  set.seed(seed)
  folds.ensemble <- createFolds(y=majority.class.data$class,k = class.times)
  ensemble.i <- 1
  predictors.ensemble <- list()
  
  for (ensemble.i in 1:class.times) {
    print(ensemble.i)
    set.seed(seed)
    ensemble.i.data <- rbind(minority.class.data,majority.class.data[folds.ensemble[[ensemble.i]],])
    boruta.train <- Boruta(class~.,data=ensemble.i.data,pValue=pval,maxRuns = 200,mcAdj=TRUE)
    feature.boruta <- gsub("\`","",getSelectedAttributes(boruta.train, withTentative = T))
    predictors.ensemble[[ensemble.i]] <- feature.boruta
    t <- 1
    while ((length(feature.boruta) <= 1)&t<=20) {
      print("few predictors left, run again")
      boruta.train <- Boruta(class~.,data=ensemble.i.data,pValue=pval,maxRuns = 200,mcAdj=TRUE)
      feature.boruta <- gsub("\`","",getSelectedAttributes(boruta.train, withTentative = T))
      predictors.ensemble[[ensemble.i]] <- feature.boruta
      t <- t+1
    }
  }
  
  #predictors.result <- unique(unlist(predictors.ensemble))
  
  return (predictors.ensemble)
}