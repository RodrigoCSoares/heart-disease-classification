library(ggplot2)
library(class)
library(caret)
library(e1071)
library(randomForest)

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
  heartDiseaseDataframe$Class <- ifelse(test = heartDiseaseDataframe$Class == 0, yes = "Healthy", no = "Unhealthy")
  heartDiseaseDataframe$Class <- as.factor(heartDiseaseDataframe$Class)
  
  # Other columns
  heartDiseaseDataframe[,c(1:9,11:13)] <- sapply(heartDiseaseDataframe[,c(1:9,11:13)], as.integer)
  
# Data classification with all datas
  
  # Getting dataframe class
  classes <- as.factor(heartDiseaseDataframe[,ncol(heartDiseaseDataframe)])
  
  # Selecting data for test and training
  set.seed(123)
  
  sample_size <- floor(0.8 * nrow(heartDiseaseDataframe))
  train_index <- sample(seq_len(nrow(heartDiseaseDataframe)), size = sample_size)
  
  # Preparing test object and training
  train_without_column <- heartDiseaseDataframe[train_index, -ncol(heartDiseaseDataframe)]
  train <- heartDiseaseDataframe[train_index,] 
  test <- heartDiseaseDataframe[-train_index, -ncol(heartDiseaseDataframe)]
  
  # Selecting the test class column and train class column
  trainClass <- classes[train_index]
  testClass <- classes[-train_index]
  
  # KNN classification
  knn_res <- knn(train_without_column, test, trainClass, 9)
  
  # Accuracy of KNN
  cf_knn <- confusionMatrix(knn_res, testClass)
  
  # Add result to dataframe
  data <- as.data.frame(cf_knn$overall)
  
  # SVM classification
  svm_classifier <- svm(formula = Class ~ .,
                   data = train,
                   type = 'C-classification',
                   kernel = 'linear')
  
  svm_res <- predict(svm_classifier, newdata = test)
  
  # Accuracy of SVM
  cf_svm <- confusionMatrix(svm_res, testClass)
  
  # Add result to dataframe
  data[,2] <- as.data.frame(cf_svm$overall)
  
  # RF classification
  rf_classifier <- randomForest(formula = Class ~ .,
                                data = train,
                                ntree = 500,
                                importance = TRUE)
  
  rf_res = predict(rf_classifier, newdata = test)
  
  # Accuracy of RF
  cf_rf <- confusionMatrix(as.factor(rf_res), testClass)
  
  # Add result to dataframe
  data[,3] <- as.data.frame(cf_rf$overall)
  
# Adjusting data - 2
  
  # Removing outliers
  outlier_values <- boxplot.stats(heartDiseaseDataframe[,12])$out
  heartDiseaseDataframe <- heartDiseaseDataframe[!(heartDiseaseDataframe[,12] %in% outlier_values), ]
  
  outlier_values <- boxplot.stats(heartDiseaseDataframe[,10])$out
  heartDiseaseDataframe <- heartDiseaseDataframe[!(heartDiseaseDataframe[,10] %in% outlier_values), ]
  
  outlier_values <- boxplot.stats(heartDiseaseDataframe[,9])$out
  heartDiseaseDataframe <- heartDiseaseDataframe[!(heartDiseaseDataframe[,9] %in% outlier_values), ]
  
  outlier_values <- boxplot.stats(heartDiseaseDataframe[,6])$out
  heartDiseaseDataframe <- heartDiseaseDataframe[!(heartDiseaseDataframe[,6] %in% outlier_values), ]
  
  outlier_values <- boxplot.stats(heartDiseaseDataframe[,5])$out
  heartDiseaseDataframe <- heartDiseaseDataframe[!(heartDiseaseDataframe[,5] %in% outlier_values), ]
  
  outlier_values <- boxplot.stats(heartDiseaseDataframe[,1])$out
  heartDiseaseDataframe <- heartDiseaseDataframe[!(heartDiseaseDataframe[,1] %in% outlier_values), ]