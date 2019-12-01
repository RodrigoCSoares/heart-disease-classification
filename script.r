# ==== Libraries import ====
library(ggplot2)
library(class)
library(caret)
library(e1071)
library(randomForest)
library(factoextra)

# ==== Test and Train ====
buildTestAndTrain <- function(dataframe, seed_value, percent) {
  
  # Getting dataframe class
  classes <- as.factor(dataframe[, ncol(dataframe)])
  
  # Selecting data for test and training
  set.seed(seed_value)
  
  sample_size <- floor(percent * nrow(dataframe))
  train_index <-
    sample(seq_len(nrow(dataframe)), size = sample_size)
  
  # Preparing test object and training
  train_without_column <- dataframe[train_index,-ncol(dataframe)]
  train <- dataframe[train_index, ]
  test <- dataframe[-train_index,-ncol(dataframe)]
  
  # Selecting the test class column and train class column
  trainClass <- classes[train_index]
  testClass <- classes[-train_index]
  
  return (
    list(
      "train_without_column" = train_without_column,
      "train" = train,
      "test" = test,
      "trainClass" = trainClass,
      "testClass" = testClass
    )
  )
}

# ==== KNN classification ====
knnClassification <- function(train, test, testClass, trainClass, k) {
    
    # KNN predict
    knn_res <- knn(train, test, trainClass, k)
    
    # Accuracy of KNN
    cf_knn <- confusionMatrix(knn_res, testClass)
    
    # Plot expected
    plot(testClass, main = "Expected plot")
    
    # Plot predict
    plot(knn_res, main = "Predict knn plot")
    
    return(cf_knn$overall)
    
}

# ==== SVM classification ====
svmClassification <- function(train, test, testClass) {
  
  # Build model
  svm_classifier <- svm(
    formula = Class ~ .,
    data = train,
    type = 'C-classification',
    kernel = 'linear'
  )
  
  # SVM predict
  svm_res <- predict(svm_classifier, newdata = test)
  
  # Accuracy of SVM
  cf_svm <- confusionMatrix(svm_res, testClass)
  
  # Plot expected
  plot(testClass, main = "Expected plot")
  
  # Plot predict
  plot(svm_res, main = "Predict svm plot")
  
  return(cf_svm$overall)
  
}

# ==== RF classification ====
rfClassification <- function(train, test, testClass) {
  
  # Build model
  rf_classifier <- randomForest(
    formula = Class ~ .,
    data = train,
    ntree = 500,
    importance = TRUE
  )
  
  # RF predict
  rf_res = predict(rf_classifier, newdata = test)
  
  # Accuracy of RF
  cf_rf <- confusionMatrix(as.factor(rf_res), testClass)
  
  # Plot expected
  plot(testClass, main = "Expected plot")
  
  # Plot predict
  plot(rf_res, main = "Predict rf plot")
  
  return(cf_rf$overall)
  
}

# ==== Build dataset ====
heartDiseaseDataframe <- 
  read.csv( "./dataset/processed.cleveland.data", fileEncoding = "UTF-8", sep = ",", header = FALSE )

# Setting the name of the columns
colnames(heartDiseaseDataframe) <- c( "Age", "Sex", "ChestPainType", "RestBloodPressure", "SerumCholestoral",
                                      "FastingBloodSugar", "ResElectrocardiographic", "MaxHeartRate", 
                                      "ExerciseInduced", "Oldpeak", "Slope", "MajorVessels", "Thal", "Class" )

# ==== Dataset preview ====

  # Header of dataset
  head(heartDiseaseDataframe)

  # Dataset structure
  str(heartDiseaseDataframe)

# ==== PLOTS ====

  # General plot
  plot(heartDiseaseDataframe, col = heartDiseaseDataframe$Class)

  # Relationship between specific columns plot
  
    # Age x RestBloodPressure
    plot(
      heartDiseaseDataframe$Age,
      heartDiseaseDataframe$RestBloodPressure,
      col = heartDiseaseDataframe$Class,
      main = "Relationship between Age and RestBloodPressure",
      ylab = "RestBloodPressure",
      xlab = "Age"
    )
    
    # Age x SerumCholestoral
    plot(
      heartDiseaseDataframe$Age,
      heartDiseaseDataframe$SerumCholestoral,
      col = heartDiseaseDataframe$Class,
      main = "Relationship between Age and SerumCholestoral",
      ylab = "SerumCholestoral",
      xlab = "Age"
    )

    # Age x MaxHeartRate
    plot(
      heartDiseaseDataframe$Age,
      heartDiseaseDataframe$MaxHeartRate,
      col = heartDiseaseDataframe$Class,
      main = "Relationship between Age and MaxHeartRate",
      ylab = "MaxHeartRate",
      xlab = "Age"
    )

    # Age x Oldpeak
    plot(
      heartDiseaseDataframe$Age,
      heartDiseaseDataframe$Oldpeak,
      col = heartDiseaseDataframe$Class,
      main = "Relationship between Age and Oldpeak",
      ylab = "Oldpeak",
      xlab = "Age"
    )
    
    # Age x SerumCholestoral
    plot(
      heartDiseaseDataframe$RestBloodPressure,
      heartDiseaseDataframe$SerumCholestoral,
      col = heartDiseaseDataframe$Class,
      main = "Relationship between RestBloodPressure and SerumCholestoral",
      ylab = "SerumCholestoral",
      xlab = "RestBloodPressure"
    )

    # RestBloodPressure x MaxHeartRate
    plot(
      heartDiseaseDataframe$RestBloodPressure,
      heartDiseaseDataframe$MaxHeartRate,
      col = heartDiseaseDataframe$Class,
      main = "Relationship between RestBloodPressure and MaxHeartRate",
      ylab = "MaxHeartRate",
      xlab = "RestBloodPressure"
    )
    
    # RestBloodPressure x Oldpeak
    plot(
      heartDiseaseDataframe$RestBloodPressure,
      heartDiseaseDataframe$Oldpeak,
      col = heartDiseaseDataframe$Class,
      main = "Relationship between RestBloodPressure and Oldpeak",
      ylab = "Oldpeak",
      xlab = "RestBloodPressure"
    )
    
    # SerumCholestoral x MaxHeartRate
    plot(
      heartDiseaseDataframe$SerumCholestoral,
      heartDiseaseDataframe$MaxHeartRate,
      col = heartDiseaseDataframe$Class,
      main = "Relationship between SerumCholestoral and MaxHeartRate",
      ylab = "MaxHeartRate",
      xlab = "SerumCholestoral"
    )
    
    # SerumCholestoral x Oldpeak
    plot(
      heartDiseaseDataframe$SerumCholestoral,
      heartDiseaseDataframe$Oldpeak,
      col = heartDiseaseDataframe$Class,
      main = "Relationship between SerumCholestoral and Oldpeak",
      ylab = "Oldpeak",
      xlab = "SerumCholestoral"
    )
    
    # MaxHeartRate x Oldpeak
    plot(
      heartDiseaseDataframe$MaxHeartRate,
      heartDiseaseDataframe$Oldpeak,
      col = heartDiseaseDataframe$Class,
      main = "Relationship between MaxHeartRate and Oldpeak",
      ylab = "Oldpeak",
      xlab = "MaxHeartRate"
  )
    
  # Dataset plots
    
    # Age
    ggplot(heartDiseaseDataframe) + 
      geom_bar(aes(Age), fill = "#006b00") +
      ggtitle("Age")
    
    # Sex 
    ggplot(heartDiseaseDataframe) + 
      geom_bar(aes(Sex), fill = "#ffcbdb") +
      ggtitle("Sex")
    
    # Class
    ggplot(heartDiseaseDataframe) +
      geom_bar(aes(Class), fill = "#000080") +
      ggtitle("Class")

# === Handling data ====

  # Change "?" to NA
  heartDiseaseDataframe[heartDiseaseDataframe == "?"] <- NA

  # Removing NA values
  heartDiseaseDataframe <- na.omit(heartDiseaseDataframe)

  # Handling table columns

    # Class
    heartDiseaseDataframe$Class <- ifelse(test = heartDiseaseDataframe$Class >= 1, yes = "Unhealthy", no = "Healthy")
    heartDiseaseDataframe$Class <- as.factor(heartDiseaseDataframe$Class)

    # Other columns
    heartDiseaseDataframe[, c(1:9, 11:13)] <- sapply(heartDiseaseDataframe[, c(1:9, 11:13)], as.integer)

# ==== Boxplot with all data ====
boxplot(heartDiseaseDataframe[, -ncol(heartDiseaseDataframe)])

# ==== Data classification with all datas ====
    
  # Create datframe result of classifications #
  first_data <- data.frame()

  # Separate test and train model
  first_model <- buildTestAndTrain(heartDiseaseDataframe, 123, 0.8)

  # KNN classification
  Knn <- knnClassification(
    first_model$train_without_column,
    first_model$test,
    first_model$testClass,
    first_model$trainClass,
    9
  )
  
  first_data <- rbind(first_data, Knn)

  # SVM classification
  Svm <- svmClassification(first_model$train, 
                           first_model$test, 
                           first_model$testClass)
  
  first_data <- rbind(first_data, Svm)

  # RF classification
  Rf <- rfClassification(first_model$train, 
                         first_model$test, 
                         first_model$testClass)
  
  first_data <- rbind(first_data, Rf)

  # renaming first data columns
  colnames(first_data) <- c( "Accuracy", "Kappa", "AccuracyLower", "AccuracyUpper", "AccuracyNull", "AccuracyPValue",
                             "McnemarPValue" )

  # Cluster
  cluster_data <- heartDiseaseDataframe[, -ncol(heartDiseaseDataframe)]

  # Elbow method to see number of necessary clusters
  fviz_nbclust(cluster_data, kmeans, method = "wss")

  # Calculate cluster by grouping
  cluster_res_2 <- kmeans(cluster_data, 2)
  cluster_res_11 <- kmeans(cluster_data, 11)
  
  # Cluster Analysis
  result_2 <- as.data.frame(table(heartDiseaseDataframe$Class, cluster_res_2$cluster))
  result_11 <- as.data.frame(table(heartDiseaseDataframe$Class, cluster_res_11$cluster))
  
# ==== Handling data 2 ====

  # Removing outliers
  outlier_values <- boxplot.stats(heartDiseaseDataframe[, 1])$out
  heartDiseaseDataframe <- heartDiseaseDataframe[!(heartDiseaseDataframe[, 1] %in% outlier_values),]

  outlier_values <- boxplot.stats(heartDiseaseDataframe[, 4])$out
  heartDiseaseDataframe <- heartDiseaseDataframe[!(heartDiseaseDataframe[, 4] %in% outlier_values),]

  outlier_values <- boxplot.stats(heartDiseaseDataframe[, 5])$out
  heartDiseaseDataframe <- heartDiseaseDataframe[!(heartDiseaseDataframe[, 5] %in% outlier_values),]

  outlier_values <- boxplot.stats(heartDiseaseDataframe[, 8])$out
  heartDiseaseDataframe <- heartDiseaseDataframe[!(heartDiseaseDataframe[, 8] %in% outlier_values),]

  outlier_values <- boxplot.stats(heartDiseaseDataframe[, 10])$out
  heartDiseaseDataframe <- heartDiseaseDataframe[!(heartDiseaseDataframe[, 10] %in% outlier_values),]

# ==== Boxplot without outliers ====
boxplot(heartDiseaseDataframe[, -ncol(heartDiseaseDataframe)])

# ==== Data classification without outliers ====
  
  # Create datframe result of classifications without dataframe outliers
  second_data <- data.frame()

  # Separate test and train model
  second_model <- buildTestAndTrain(heartDiseaseDataframe, 123, 0.8)

  # KNN classification
  Knn <- knnClassification(
    second_model$train_without_column,
    second_model$test,
    second_model$testClass,
    second_model$trainClass,
    9
  )
  
  second_data <- rbind(second_data, Knn)

  # SVM classification
  Svm <- svmClassification(second_model$train,
                    second_model$test,
                    second_model$testClass)
  
  second_data <- rbind(second_data, Svm)

  # RF classification
  Rf <- rfClassification(second_model$train,
                   second_model$test,
                   second_model$testClass)
  
  second_data <- rbind(second_data, Rf)

  # Renaming dataset columns
  colnames(second_data) <- c( "Accuracy", "Kappa", "AccuracyLower", "AccuracyUpper", "AccuracyNull", "AccuracyPValue",
                              "McnemarPValue" )

  # Cluster
  cluster_data <- heartDiseaseDataframe[, -ncol(heartDiseaseDataframe)]

  # Elbow method to see number of necessary clusters
  fviz_nbclust(cluster_data, kmeans, method = "wss")

  # Calculate cluster by grouping
  cluster_res_2 <- kmeans(cluster_data, 2)
  cluster_res_11 <- kmeans(cluster_data, 11)

  # Cluster Analysis
  result_2_outlier <- as.data.frame(table(heartDiseaseDataframe$Class, cluster_res_2$cluster))
  result_11_outlier <- as.data.frame(table(heartDiseaseDataframe$Class, cluster_res_11$cluster))

# ==== Handling data 3 ====

  # PCA
  dataframe.pca <- prcomp(heartDiseaseDataframe[, -ncol(heartDiseaseDataframe)], center = TRUE, scale. = TRUE)
  newDataframe <- as.data.frame(predict(dataframe.pca, heartDiseaseDataframe))

  # Adding class column to the new dataframe
  newDataframe$Class <- heartDiseaseDataframe$Class

# ==== Data classification with PCA ====
  
  # Create datframe result of classifications with cleaning data pca
  third_data <- data.frame()

  # Separate test and train model
  third_model <- buildTestAndTrain(newDataframe, 123, 0.8)

  # KNN classification
  Knn <- knnClassification(
    third_model$train_without_column,
    third_model$test,
    third_model$testClass,
    third_model$trainClass,
    9
  )
  
  third_data <- rbind(third_data, Knn)

  # SVM classification
  Svm <- svmClassification(third_model$train, third_model$test, third_model$testClass)
  
  third_data <- rbind(third_data, Svm)

  # RF classification
  Rf <- rfClassification(third_model$train, third_model$test, third_model$testClass)
  
  third_data <- rbind(third_data, Rf)

  # renaming dataset columns
  colnames(third_data) <- c( "Accuracy", "Kappa", "AccuracyLower", "AccuracyUpper", "AccuracyNull", "AccuracyPValue",
                             "McnemarPValue" )

  # Cluster
  cluster_data <- newDataframe[, -ncol(newDataframe)]

  # Elbow method to see number of necessary clusters
  fviz_nbclust(cluster_data, kmeans, method = "wss")

  # Calculate cluster by grouping
  cluster_res_2 <- kmeans(cluster_data, 2)
  cluster_res_11 <- kmeans(cluster_data, 11)

  # Cluster Analysis
  result_2_pca <- as.data.frame(table(newDataframe$Class, cluster_res_2$cluster))
  result_11_pca <- as.data.frame(table(newDataframe$Class, cluster_res_11$cluster))
  