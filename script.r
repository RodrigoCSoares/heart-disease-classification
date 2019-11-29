# Libraries import
library(ggplot2)
library(class)
library(caret)
library(e1071)
library(randomForest)
library(factoextra)

# Test and Train to classification
buildTestAndTrain <- function(dataframe, seed_value, percent) {
  
  # Getting dataframe class
  classes <- as.factor(dataframe[,ncol(dataframe)])
  
  # Selecting data for test and training
  set.seed(seed_value)
  
  sample_size <- floor(percent * nrow(dataframe))
  train_index <- sample(seq_len(nrow(dataframe)), size = sample_size)
  
  # Preparing test object and training
  train_without_column <- dataframe[train_index, -ncol(dataframe)]
  train <- dataframe[train_index,] 
  test <- dataframe[-train_index, -ncol(dataframe)]
  
  # Selecting the test class column and train class column
  trainClass <- classes[train_index]
  testClass <- classes[-train_index]
  
  return (list("train_without_column" = train_without_column,
               "train" = train,
               "test" = test,
               "trainClass" = trainClass,
               "testClass" = testClass))
}

# KNN classification
knnClassification <- function(train, test, testClass, trainClass, k) {
  
  # KNN predict
  knn_res <- knn(train, test, trainClass, k)
  
  # Accuracy of KNN
  cf_knn <- confusionMatrix(knn_res, testClass)
  
  return(cf_knn$overall)
}

# SVM classification
svmClassification <- function(train, test, testClass) {
  
  # Build model
  svm_classifier <- svm(formula = Class ~ .,
                        data = train,
                        type = 'C-classification',
                        kernel = 'linear')
  
  # SVM predict
  svm_res <- predict(svm_classifier, newdata = test)
  
  # Accuracy of SVM
  cf_svm <- confusionMatrix(svm_res, testClass)
  
  return(cf_svm$overall)
}

# RF classification
rfClassification <- function(train, test, testClass) {

  # Build model
  rf_classifier <- randomForest(formula = Class ~ .,
                                data = train,
                                ntree = 500,
                                importance = TRUE)
  
  # RF predict
  rf_res = predict(rf_classifier, newdata = test)
  
  # Accuracy of RF
  cf_rf <- confusionMatrix(as.factor(rf_res), testClass)
  
  return(cf_rf$overall)
  
}

# Importing the dataset
heartDiseaseDataframe <- read.csv("./dataset/processed.cleveland.data", fileEncoding = "UTF-8",sep=",", header = FALSE)

# Setting the name of the columns
colnames(heartDiseaseDataframe) <- c("Age", "Sex", "ChestPainType", "RestBloodPressure", "SerumCholestoral", "FastingBloodSugar", "ResElectrocardiographic", "MaxHeartRate", "ExerciseInduced", "Oldpeak", "Slope", "MajorVessels", "Thal", "Class")

# Dataset analysis

  # Header of dataset
  head(heartDiseaseDataframe)

  # Dataset structure
  str(heartDiseaseDataframe)

# Adjusting data - 1
  
  # Change "?" to NA
  heartDiseaseDataframe[heartDiseaseDataframe == "?"] <- NA
  
  # Removing NA values
  heartDiseaseDataframe <- na.omit(heartDiseaseDataframe)

  # Handling table columns
  
  # Class
  heartDiseaseDataframe$Class <- ifelse(test = heartDiseaseDataframe$Class >= 1, yes = 1, no = 0)
  heartDiseaseDataframe$Class <- as.factor(heartDiseaseDataframe$Class)
  
  # Other columns
  heartDiseaseDataframe[,c(1:9,11:13)] <- sapply(heartDiseaseDataframe[,c(1:9,11:13)], as.integer)
  
  # Create datframe result of classifications
  first_data <- data.frame()
  
  # Data classification with all datas
    
  # Separate test and train model
  first_model <- buildTestAndTrain(heartDiseaseDataframe, 123, 0.8)
  
  # KNN classification
  Knn <- knnClassification(first_model$train_without_column, first_model$test, first_model$testClass, first_model$trainClass, 9)
  first_data <- rbind(first_data, Knn)
  
  # SVM classification
  Svm <- svmClassification(first_model$train, first_model$test, first_model$testClass)
  first_data <- rbind(first_data, Svm)
  
  # RF classification
  Rf <- rfClassification(first_model$train, first_model$test, first_model$testClass)
  first_data <- rbind(first_data, Rf)
  
  # Cluster
  cluster_data <- heartDiseaseDataframe[,-ncol(heartDiseaseDataframe)]
  
  # Elbow method to see number of necessary clusters
  fviz_nbclust(cluster_data, kmeans, method = "wss")
  
  # Calculate cluster by grouping
  cluster_res_2 <- kmeans(cluster_data, 2)

  # Cluster Analysis
  class_aux <- as.factor(heartDiseaseDataframe$Class)
  class_aux <- ifelse(class_aux == 0, yes = "Healthy", no = "Unhealthy")
  
  result_2 <- as.data.frame(table(class_aux,cluster_res_2$cluster))
  
# Adjusting data - 2
  
  # Data without outliers
                                      
  # Removing outliers
  outlier_values <- boxplot.stats(heartDiseaseDataframe[,1])$out
  heartDiseaseDataframe <- heartDiseaseDataframe[!(heartDiseaseDataframe[,1] %in% outlier_values), ]
  
  outlier_values <- boxplot.stats(heartDiseaseDataframe[,4])$out
  heartDiseaseDataframe <- heartDiseaseDataframe[!(heartDiseaseDataframe[,4] %in% outlier_values), ]
  
  outlier_values <- boxplot.stats(heartDiseaseDataframe[,5])$out
  heartDiseaseDataframe <- heartDiseaseDataframe[!(heartDiseaseDataframe[,5] %in% outlier_values), ]
  
  outlier_values <- boxplot.stats(heartDiseaseDataframe[,8])$out
  heartDiseaseDataframe <- heartDiseaseDataframe[!(heartDiseaseDataframe[,8] %in% outlier_values), ]
  
  outlier_values <- boxplot.stats(heartDiseaseDataframe[,10])$out
  heartDiseaseDataframe <- heartDiseaseDataframe[!(heartDiseaseDataframe[,10] %in% outlier_values), ]
  
  # Create datframe result of classifications without dataframe outliers
  second_data <- data.frame()
  
  # Data classification without outliers
  
  # Separate test and train model
  second_model <- buildTestAndTrain(heartDiseaseDataframe, 123, 0.8)
  
  # KNN classification
  Knn <- knnClassification(second_model$train_without_column, second_model$test, second_model$testClass, second_model$trainClass, 9)
  second_data <- rbind(second_data, Knn)
  
  # SVM classification
  Svm <- svmClassification(second_model$train, second_model$test, second_model$testClass)
  second_data <- rbind(second_data, Svm)
  
  # RF classification
  Rf <- rfClassification(second_model$train, second_model$test, second_model$testClass)
  second_data <- rbind(second_data, Rf)
  
  # Cluster
  cluster_data <- heartDiseaseDataframe[,-ncol(heartDiseaseDataframe)]
  
  # Elbow method to see number of necessary clusters
  fviz_nbclust(cluster_data, kmeans, method = "wss")
  
  # Calculate cluster by grouping
  cluster_res_2 <- kmeans(cluster_data, 2)
  
  # Cluster Analysis
  class_aux <- as.factor(heartDiseaseDataframe$Class)
  class_aux <- ifelse(class_aux == 0, yes = "Healthy", no = "Unhealthy")
  
  result_2_outlier <- as.data.frame(table(class_aux,cluster_res_2$cluster))
  
# Adjusting data - 3
  
  # PCA
  dataframe.pca <- prcomp(heartDiseaseDataframe[,-ncol(heartDiseaseDataframe)], center = TRUE, scale. = TRUE)
  newDataframe <- as.data.frame(predict(dataframe.pca, heartDiseaseDataframe))
  
  # Adding class column to the new dataframe
  newDataframe$Class <- heartDiseaseDataframe$Class
  
  # Create datframe result of classifications with cleaning data pca
  third_data <- data.frame()
  
  # Data classification without outliers
  
  # Separate test and train model
  third_model <- buildTestAndTrain(newDataframe, 123, 0.8)
  
  # KNN classification
  Knn <- knnClassification(third_model$train_without_column, third_model$test, third_model$testClass, third_model$trainClass, 9)
  third_data <- rbind(third_data, Knn)
  
  # SVM classification
  Svm <- svmClassification(third_model$train, third_model$test, third_model$testClass)
  third_data <- rbind(third_data, Svm)
  
  # RF classification
  Rf <- rfClassification(third_model$train, third_model$test, third_model$testClass)
  third_data <- rbind(third_data, Rf)
  
  # Cluster
  cluster_data <- newDataframe[,-ncol(newDataframe)]
  
  # Elbow method to see number of necessary clusters
  fviz_nbclust(cluster_data, kmeans, method = "wss")
  
  # Calculate cluster by grouping
  cluster_res_2 <- kmeans(cluster_data, 2)
  
  # Cluster Analysis
  class_aux <- as.factor(newDataframe$Class)
  class_aux <- ifelse(class_aux == 0, yes = "Healthy", no = "Unhealthy")
  
  result_2_pca <- as.data.frame(table(class_aux,cluster_res_2$cluster))
  
  
  