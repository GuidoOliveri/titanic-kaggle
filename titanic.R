setwd("C:/MACHINE LEARNING/kaggle") # o la que sea
# hago predicciones sobre el Survived del test 

titanic.train <- read.csv(file = "train.csv", stringsAsFactors = FALSE, header = TRUE)
titanic.test <- read.csv(file = "test.csv", stringsAsFactors = FALSE, header = TRUE)

titanic.train$IsTrainSet <- TRUE 
titanic.test$IsTrainSet <- FALSE

titanic.test$Survived <-NA # agrego columna Survived a test

titanic.full <- rbind(titanic.train, titanic.test) # joinea ambas tablas

titanic.full[titanic.full$Embarked == '', "Embarked"] <- '' # voy a consultar por los que no tienen valor de Embarked

age.median <- median(titanic.full$Age,na.rm=TRUE) # saca la mediana de las edades considerando los vacios
titanic.full[is.na(titanic.full$Age), "Age"] <- age.median # a los vacios le asigna la mediana, eso hay que cambiarlo

#clean missing values of fare
#fare.median <- median(titanic.full$Fare,na.rm=TRUE) # saca mediana de tarifa con espacios vacios
#titanic.full[is.na(titanic.full$Fare), "Fare"] <- fare.median  # a los vacios le asigna la mediana, eso hay que cambiarlo

#voy a usar un modelo de regression para predecir la tarifa
upper.whisker <- boxplot.stats(titanic.full$Fare)$stats[5]
outlier.filter <- titanic.full$Fare < upper.whisker


fare.equation = "Fare ~ Pclass + Sex + Age + SibSp + Parch + Embarked"

fare.model <-lm(
  formula = fare.equation, data = titanic.full[outlier.filter,]
)

fare.row <-titanic.full[
  is.na(titanic.full$Fare),
  c("Pclass", "Sex", "Age", "SibSp", "Parch", "Embarked")]


fare.predictions <- predict(fare.model, newdata = fare.row)
titanic.full[is.na(titanic.full$Fare), "Fare"] <- fare.predictions

#categorical casting, divide en factores 
titanic.full$Pclass <- as.factor(titanic.full$Pclass)
titanic.full$Sex <- as.factor(titanic.full$Sex)
titanic.full$Embarked <- as.factor(titanic.full$Embarked)

# split dataset back out into train and set, divide el conjunto que me quedo en train y set
titanic.train<-titanic.full[titanic.full$IsTrainSet==TRUE,] 
titanic.test<-titanic.full[titanic.full$IsTrainSet==FALSE,]

titanic.train$Survived <- as.factor(titanic.train$Survived)

survived.equation <- "Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"

#con as.formula se interpreta la formula
Survived.formula <- as.formula(survived.equation)

#utilizamos una gran cantidad de arboles de decision y los
#usamos para hacer una clasificacion. Partimos de una matriz (arbol de decision) que tiene un conjunto de caracteristicas y
#usamos el random forest para clasificar este conjunto de muestras y hacer una prediccion de clases
# se elije la clase del que haya salido por mayoria


install.packages("randomForest")
library(randomForest)

titanic.model<- randomForest(Survived.formula, data = titanic.train, ntree=500, mtry=3, nodesize=0.01*nrow(titanic.test))

features.equation <- "Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
Survived <- predict(titanic.model, newdata = titanic.test ) # linea clave, yo hago la prediccion en la new data que es test, entonces tengo que devolver 418 filas

PassengerId <- titanic.test$PassengerId # guarda el passengerId de test porque es el que voy a devolver
output.df <- as.data.frame(PassengerId) # mete el id pasajero como un data frame
output.df$Survived <- Survived # output data frame, creo la columna Survived y pongo la prediccion si sobrevivio o no

write.csv(output.df, file = "titanic_output.csv", row.names = FALSE)