setwd("C:/MACHINE LEARNING/titanic-kaggle/script") 
# hago predicciones sobre el Survived del test 

titanic.train <- read.csv(file = "train.csv", stringsAsFactors = FALSE, header = TRUE)
titanic.test <- read.csv(file = "test.csv", stringsAsFactors = FALSE, header = TRUE)

#me dice la estructura del dataframe train
str(titanic.train)

# si uso $, uso solo una columna 
titanic.train$IsTrainSet <- TRUE 
titanic.test$IsTrainSet <- FALSE

table(titanic.train$Survived)
prop.table(table(titanic.train$Survived))

summary(titanic.train$Sex)
prop.table(table(titanic.train$Sex, titanic.train$Survived))

#pongo uno para que me de las proporciones de cada fila
prop.table(table(titanic.train$Sex, titanic.train$Survived),1)

summary(titanic.train$Age)

#creo variable Child
titanic.train$Child <- 0
titanic.train$Child[titanic.train$Age < 18] <- 1
# muestra 4 combinaciones entre si es chico y que sexo tiene (chico, chica, hombre, mujer)
aggregate(Survived ~ Child + Sex, data=titanic.train, FUN=function(x) {sum(x)/length(x)})

titanic.train$Fare2 <- '30 o +' # si lo pongo abajo de todo tira unos warnings
titanic.train$Fare2[titanic.train$Fare < 30 & titanic.train$Fare >= 20] <- '20-30'
titanic.train$Fare2[titanic.train$Fare < 20 & titanic.train$Fare >= 10] <- '10-20'
titanic.train$Fare2[titanic.train$Fare < 10] <- '<10'

aggregate(Survived ~ Fare2 + Pclass + Sex, data=titanic.train, FUN=function(x) {sum(x)/length(x)})

#creo variable survived en el conjunto test, pongo las condiciones iniciales primero y dsps lo particular
titanic.test$Survived <- 0
titanic.test$Survived[titanic.test$Sex == 'female'] <- 1
titanic.test$Survived[titanic.test$Sex == 'female' & titanic.test$Pclass == 3 & titanic.test$Fare >= 20] <- 0

#arboles de decision se viene
library(rpart)

#ejecutar directamente el fancy
plot(fit)
text(fit)

install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')

library(rattle)
library(rpart.plot)
library(RColorBrewer)

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data=titanic.train,
             method="class", 
             control=rpart.control(minsplit=100, cp=0.01, maxdepth = 30))

#grafica el arbol mucho mejor que el plot y el text
#Try to look for overly complex decisions being made, and kill the nodes that appear to go to far.
new.fit <- prp(fit,snip=TRUE)$obj
fancyRpartPlot(new.fit)


#predice en base al fit, el conjunto test y va a ser con 0 o 1, por eso uso class
Prediction <- predict(fit, titanic.test, type = "class")

?rpart.control

titanic.train$Name[1]

submit <- data.frame(PassengerId = titanic.test$PassengerId, Survived = Prediction)
write.csv(submit, file = "titanicKaggle.csv", row.names = FALSE)
