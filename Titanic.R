## -------------------------------------------------------------------------
## SCRIPT: Titanic.R
## AUTOR: FernandoRodriguezP
## -------------------------------------------------------------------------

## -------------------------------------------------------------------------
##### 1. Bloque de inicializacion de librerias #####

if(!require("plyr")){
  install.packages("plyr")
  library("plyr")
}

if(!require("caTools")){
  install.packages("caTools")
  library("caTools")
}

if(!require("ROCR")){
  install.packages("ROCR")
  library("ROCR")
}

##### 2. Bloque de carga de datos #####

#Dataset de pasajeros del titanic
titanic=read.csv("data/train.csv", sep = ",")
#Dataset de test de pasajeros del titanic
titanicTest=read.csv("data/test.csv", sep = ",")
##### datos extraidos de https://www.kaggle.com/c/titanic/data

## -------------------------------------------------------------------------

##### 3. Bloque de revisión basica del dataset #####

# Dataset de entrenamiento de pasajeros del titanic
str(titanic)
head(titanic)
summary(titanic)
# Dataset de test de pasajeros del titanic
str(titanicTest)
head(titanicTest)
summary(titanicTest)

## -------------------------------------------------------------------------

##### 4. Bloque de limpieza del dataset #####

#### Limpieza del dataset titanic

# Se crea un nuevo vector en función del título
titanic$Title<-regmatches(as.character(titanic$Name),regexpr("\\,[A-z ]{1,20}\\.", as.character(titanic$Name)))
titanic$Title<-unlist(lapply(titanic$Title,FUN=function(x) substr(x, 3, nchar(x)-1)))
table(titanic$Title)

#Los 17 titulos que existen se agrupan en los 4 mas comunes
titanic$Title[which(titanic$Title %in% c("Mme", "Mlle"))] <- "Miss"
titanic$Title[which(titanic$Title %in% c("Lady", "Ms", "the Countess", "Dona"))] <- "Mrs"
titanic$Title[which(titanic$Title=="Dr" & d$Sex=="female")] <- "Mrs"
titanic$Title[which(titanic$Title=="Dr" & d$Sex=="male")] <- "Mr"
titanic$Title[which(titanic$Title %in% c("Capt", "Col", "Don", "Jonkheer", "Major", "Rev", "Sir"))] <- "Mr"

# Se factoriza el vector Title
titanicTrain$Title<-as.factor(titanic$Title)

#Se rellenan los NAs de Age con la mediana del grupo al que pertenece
titleAge<-aggregate(titanic$Age,by = list(titanic$Title), FUN = function(x) median(x, na.rm = T))
titanic[is.na(titanic$Age), "Age"] <- apply(titanic[is.na(titanic$Age), ] , 1, function(x) titleAge[titleAge[, 1]==x["Title"], 2])

# Se visualiza el dataframe titanic
View(titanic)

#### Limpieza del dataset de test

# Se crea un nuevo vector en función del título
titanicTest$Title<-regmatches(as.character(titanicTest$Name),regexpr("\\,[A-z ]{1,20}\\.", as.character(titanicTest$Name)))
titanicTest$Title<-unlist(lapply(titanicTest$Title,FUN=function(x) substr(x, 3, nchar(x)-1)))
table(titanicTest$Title)

#Los 17 titulos que existen se agrupan en los 4 mas comunes
titanicTest$Title[which(titanicTest$Title %in% c("Mme", "Mlle"))] <- "Miss"
titanicTest$Title[which(titanicTest$Title %in% c("Lady", "Ms", "the Countess", "Dona"))] <- "Mrs"
titanicTest$Title[which(titanicTest$Title=="Dr" & d$Sex=="female")] <- "Mrs"
titanicTest$Title[which(titanicTest$Title=="Dr" & d$Sex=="male")] <- "Mr"
titanicTest$Title[which(titanicTest$Title %in% c("Capt", "Col", "Don", "Jonkheer", "Major", "Rev", "Sir"))] <- "Mr"

# Se factoriza el vector Title
titanicTest$Title<-as.factor(titanicTest$Title)   #convert to factor variable

#Se rellenan los NAs de Age con la mediana del grupo al que pertenece
titleAge<-aggregate(titanicTest$Age,by = list(titanicTest$Title), FUN = function(x) median(x, na.rm = T))
titanicTest[is.na(titanicTest$Age), "Age"] <- apply(titanicTest[is.na(titanicTest$Age), ] , 1, function(x) titleAge[titleAge[, 1]==x["Title"], 2])

# Se visualiza el dataframe titanicTest
View(titanicTest)

## -------------------------------------------------------------------------

##### 4. Bloque de formateo de variables #####

# Formateo de variables del dataset de entrenamiento
titanic$Pclass=as.factor(titanic$Pclass)

str(titanic)
head(titanic)
summary(titanic)

# Formateo de variables del dataset de test
titanicTest$Pclass=as.factor(titanicTest$Pclass)

str(titanicTest)
head(titanicTest)
summary(titanicTest)

## -------------------------------------------------------------------------

##### 5. Bloque de creación de conjuntos de entrenamiento y test #####

SAMPLE = sample.split(titanic$Survived, SplitRatio = .75)
titanicTrain = subset(titanic, SAMPLE == TRUE)
titanicVal = subset(titanic, SAMPLE == FALSE)

## -------------------------------------------------------------------------

##### 6. Bloque de modelo de regresión logistica #####

modeloLogit=glm(Survived ~  Pclass + Sex + Age + Fare,    # Modelo Lineal Generalizado
                data=titanicTrain,family=binomial(link="logit"))
summary(modeloLogit)    # Muestra diferentes parámetros como la desviación residual, estimación, desviación std, si es o no significativo, etc.
confint(modeloLogit)    # Muestra los intervalos de confianza del modelo

## -------------------------------------------------------------------------

##### 7. Bloque de interpretación de coeficientes #####

coef(modeloLogit)         # Función que extrae los coeficientes del modelo
exp(coef(modeloLogit))    # Pasa los coef a formato exponencial. Probabilidad de que se cumpla cada uno de los predictores respecto al caso base.
                          # Ejemplo: Plcass2 = 0.2744... Es 0,2744 veces más probable que se salve viajando en segunda clase que en primera.
exp(cbind(coef(modeloLogit), confint(modeloLogit,level=0.95)))  # Este comando muestra el Odd-Ratio, el cual te muestra el valor del coeficiente 
                                                                # de cada predictor relativo a la superviviencia, así como su respectivo 
                                                                # intervalo de confianza.
#anova(modeloLogit)        # Hace falta compararlo con algo 

## -------------------------------------------------------------------------

##### 8. Bloque de evaluación del comportamiento del modelo en el dataset de Entrenamiento #####

titanicTrain$Predict = predict(modeloLogit,type = 'response')
head(titanicTrain)

titanicTrain$Predict = ifelse(titanicTrain$Predict >0.5, 1, 0)
head(titanicTrain)
View(titanicTrain)

misClassifiErrorTrain = mean(titanicTrain$Predict != titanicTrain$Survived)    # Error de predicción respecto a la supervivencia en titanicTrain
print(paste('Accuracy', 1 - misClassifiErrorTrain))

## -------------------------------------------------------------------------

##### 9. Bloque de evaluación del comportamiento del modelo  en el dataset de Validación #####

titanicVal$Predict = predict(modeloLogit, newdata=titanicVal,type="response")
head(titanicVal)

titanicVal$Predict = ifelse(titanicVal$Predict >0.5, 1, 0)
head(titanicVal)
View(titanicVal)

misClassifiErrorVal = mean(titanicVal$Predict != titanicVal$Survived)    # Error de predicción respecto a la supervivencia en titanicVal
print(paste('Accuracy', 1 - misClassifiErrorVal))

## -------------------------------------------------------------------------

##### 10. Bloque de evaluación del comportamiento del modelo  en el dataset de Test #####

titanicTest$Survived = predict(modeloLogit, newdata=titanicTest,type="response")
head(titanicTest)

titanicTest$Survived = ifelse(titanicTest$Survived >0.5, 1, 0)
head(titanicTest)
View(titanicTest)

## -------------------------------------------------------------------------

##### 11. Se crea el dataframe de resultado y se exporta a csv #####

titanicOutput <- titanicTest[,c(1,13)]    #Crea el dataframe de salida con las variables Passenger Id y Survived
View(titanicOutput)

write.table(titanicOutput, file = "data/output.csv", row.names = FALSE, sep = ",", quote = FALSE)
