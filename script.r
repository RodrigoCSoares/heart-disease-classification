library(ggplot2)

# Importing the dataset
heartDiseaseDataframe <- read.table("./dataset/processed.cleveland.data", fileEncoding = "UTF-8",sep=",")
colnames(heartDiseaseDataframe) <- c("Age", "Sex", "ChestPainType", "RestBloodPressure", "SerumCholestoral", "FastingBloodSugar", "ResElectrocardiogr
aphic", "MaxHeartRate", "ExerciseInduced", "Oldpeak", "Slope", "MajorVessels", "Thal", "Class")

# Removing rows with NA values
na.omit(heartDiseaseDataframe)

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