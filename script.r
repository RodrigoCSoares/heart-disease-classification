library(ggplot2)
library(class)
library(caret)
library(e1071)

# Importing the dataset
heartDiseaseDataframe <- read.csv("./dataset/processed.cleveland.data", fileEncoding = "UTF-8",sep=",", header = FALSE)

# Setting the name of the columns
colnames(heartDiseaseDataframe) <- c("Age", "Sex", "ChestPainType", "RestBloodPressure", "SerumCholestoral", "FastingBloodSugar", "ResElectrocardiogr
aphic", "MaxHeartRate", "ExerciseInduced", "Oldpeak", "Slope", "MajorVessels", "Thal", "Class")

# Change "?" to NA
heartDiseaseDataframe[heartDiseaseDataframe == "?"] <- NA

# Removing rows with NA values
heartDiseaseDataframe <- na.omit(heartDiseaseDataframe)

# Visualization

  # Age distribution
  qplot(heartDiseaseDataframe$Age, geom="histogram") +
    labs(x = "Age", y = "count")
  
  # Genre distribution
  ggplot(heartDiseaseDataframe, aes(Sex)) +
    geom_bar(fill = "#0073C2FF") +
    labs(x = "Sex (0 = Female, 1 = Male)")
  
  # Relantionship between blood pressure and heart rate
  plot(heartDiseaseDataframe$RestBloodPressure, heartDiseaseDataframe$MaxHeartRate)
  
# Data classification
  
  # Getting dataframe class
  classes <- as.factor(heartDiseaseDataframe[,ncol(heartDiseaseDataframe)])
  
  # Removing class column
  data <- heartDiseaseDataframe[, -ncol(heartDiseaseDataframe)]
  
  # Selecting data for test and training
  set.seed(123)
  
  sample_size <- floor(0.8 * nrow(data))
  train_index <- sample(seq_len(nrow(data)), size = sample_size)
  
  # Preparing test object and training
  train <- data[train_index, ]
  test <- data[-train_index, ]
  
  # Selecting the test class column and train class column
  trainClass <- as.factor(classes[train_index])
  testClass <- as.factor(classes[-train_index])
  
  # KNN classification without pca
  knn_res1 <- knn(train, test, trainClass, 1)
  knn_res3 <- knn(train, test, trainClass, 3)
  knn_res5 <- knn(train, test, trainClass, 5)
  knn_res7 <- knn(train, test, trainClass, 7)
  knn_res9 <- knn(train, test, trainClass, 9)
  
  # Accuracy of KNN without pca
  confusionMatrix(knn_res1, testClass)
  confusionMatrix(knn_res3, testClass)
  confusionMatrix(knn_res5, testClass)
  confusionMatrix(knn_res7, testClass)
  confusionMatrix(knn_res9, testClass)
  