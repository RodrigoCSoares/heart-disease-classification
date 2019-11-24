library(ggplot2)
library(class)
library(caret)
library(e1071)
library(randomForest)

# Importing the dataset
heartDiseaseDataframe <- read.csv("./dataset/processed.cleveland.data", fileEncoding = "UTF-8",sep=",", header = FALSE)

# Setting the name of the columns
colnames(heartDiseaseDataframe) <- c("Age", "Sex", "ChestPainType", "RestBloodPressure", "SerumCholestoral", "FastingBloodSugar", "ResElectrocardiographic", "MaxHeartRate", "ExerciseInduced", "Oldpeak", "Slope", "MajorVessels", "Thal", "Class")

# Change "?" to NA
heartDiseaseDataframe[heartDiseaseDataframe == "?"] <- NA

# Change class 2,3,4 to 1
heartDiseaseDataframe$Class[heartDiseaseDataframe$Class %in% c("2", "3", "4")] <- 1

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
  
  # Selecting data for test and training
  set.seed(123)
  
  sample_size <- floor(0.8 * nrow(heartDiseaseDataframe))
  train_index <- sample(seq_len(nrow(heartDiseaseDataframe)), size = sample_size)
  
  # Preparing test object and training
  train <- heartDiseaseDataframe[train_index, -ncol(heartDiseaseDataframe)]
  train_svm <- heartDiseaseDataframe[train_index,] 
  test <- heartDiseaseDataframe[-train_index, -ncol(heartDiseaseDataframe)]
  
  # Selecting the test class column and train class column
  trainClass <- as.factor(classes[train_index])
  testClass <- as.factor(classes[-train_index])
  
  # KNN classification
  knn_res <- knn(train, test, trainClass, 1)
  
  # Accuracy of KNN
  confusionMatrix(knn_res, testClass)
  
  # SVM classification
  svm_classifier <- svm(formula = Class ~ .,
                   data = train_svm,
                   type = 'C-classification',
                   kernel = 'linear')
  
  svm_res <- predict(svm_classifier, newdata = test)
  
  # Accuracy of SVM
  confusionMatrix(svm_res, testClass)
  
  # NB classification
  nb_classifier <- train(train, 
                         trainClass, 
                         'nb', 
                         trControl=trainControl(method='cv',number=10))
  
  nb_res <- predict(nb_classifier, newdata = test)
  
  # Accuracy of NB
  confusionMatrix(nb_res, testClass)
  
  # RF classification
  rf_classifier <- randomForest(x = train, 
                                y = trainClass,
                                ntree = 500,
                                proximity = TRUE)
  
  rf_res = predict(rf_classifier, newdata = test)
  
  # Accuracy of RF
  confusionMatrix(rf_res, testClass)

  