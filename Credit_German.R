# Carragamento de pacotes
source('ClassTools.R')
library(tidyverse)
library(caTools)

# Coleta de dados
df <- read.csv2('5-Mini-Projeto4\\credit_dataset.csv', sep = ',')
dim(df)

# tipos de variáveis
str(df)

toFactors <- c(
  "credit.rating",
  "account.balance", 
  "previous.credit.payment.status", 
  "credit.purpose",
  "savings",
  "employment.duration",
  "installment.rate",
  "marital.status",
  "guarantor",
  "residence.duration",
  "current.assets",
  "other.credits",
  "apartment.type",
  "bank.credits",
  "occupation",
  "dependents",
  "telephone",
  "foreign.worker"
)

df[,toFactors] = lapply(df[, toFactors], as.factor)

# Checar - Duplicatas
duplicated(df)

# Checar - NA 
library(Amelia)
missmap(df)
#complete.cases(df)

# Normalização
# Criando um função de normalização
normalizar <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

colunas_numericas <- c('credit.amount', 'credit.duration.months','age')

# Normalizando os dados
df[colunas_numericas] <- as.data.frame(lapply(df[colunas_numericas], normalizar))
View(df)
str(df)

# Balancear o número de casos positivos e negativos
df <- equ.Frame(df, 2, 'credit.rating')
str(df)

# Randomização - Treino e teste
amostra <- sample.split(df$credit.duration.months, SplitRatio = 0.70)

# Criando dados de treino - 70% dos dados
treino = subset(df, amostra == TRUE)

# Criando dados de teste - 30% dos dados
teste = subset(df, amostra == FALSE)


#installed.packages(randomForest)
library(randomForest)
colnames(df)
?randomForest()

###################################################
# Modelo1 - Todas as variáveis
modelo <- randomForest( credit.rating ~ .,
                        data = df, 
                        ntree = 100, nodesize = 10, importance = T)

varImpPlot(modelo)

modelo_final <- randomForest( credit.rating ~ .,
                                data = treino,
                        )
print(modelo_final)

previsto = predict(modelo_final, newdata = teste)

previsoes <- data.frame(observado = teste$credit.rating,
                        previsto = predict(modelo_final, newdata = teste))

# matriz de confusão
cfm <- confusionMatrix(previsto, teste$credit.rating)
cfm

# Acurácia de 79,19%

###################################################
# Modelo2 - Todas as variáveis, com mais nós
modelo <- randomForest( credit.rating ~ .,
                        data = treino,
                        ntree = 200, 
                        nodesize = 100)

print(modelo)

previsto = predict(modelo, newdata = teste)

previsoes <- data.frame(observado = teste$credit.rating,
                        previsto = predict(modelo, newdata = teste))

# matriz de confusão
cfm <- confusionMatrix(previsto, teste$credit.rating)
cfm

# Acurácia de 76,85%

###################################################
# Modelo3 - Variáveis mais representativas, com mais nós
modelo <- randomForest( credit.rating ~ .,
                        data = df,
                        ntree = 200, 
                        nodesize = 100, importante = T)

modelo <- randomForest( credit.rating ~ 
                          + account.balance
                          + credit.duration.months
                          + credit.amount
                          + previous.credit.payment.status
                          + savings
                        ,
                        data = treino,
                        ntree = 100, 
                        nodesize = 10)

print(modelo)

previsto = predict(modelo, newdata = teste)

previsoes <- data.frame(observado = teste$credit.rating,
                        previsto = predict(modelo, newdata = teste))

# matriz de confusão
cfm <- confusionMatrix(previsto, teste$credit.rating)
cfm

# Acurácia de 77,52%

###################################################
# Modelo4 - naiveBayes, todas as variáveis
modelo <- naiveBayes(credit.rating ~ .,
                           data = treino)

print(modelo)

previsto = predict(modelo, newdata = teste)

previsoes <- data.frame(observado = teste$credit.rating,
                        previsto = predict(modelo, newdata = teste))


# matriz de confusão
cfm <- confusionMatrix(previsto, teste$credit.rating)
cfm

# Acurácia de 76,17%

###################################################
# Modelo4 - naiveBayes,
modelo <- naiveBayes(credit.rating ~ 
                       + account.balance
                       + credit.duration.months
                       + credit.amount
                       + previous.credit.payment.status
                       + savings,
                     data = treino)

print(modelo)

previsto = predict(modelo, newdata = teste)

previsoes <- data.frame(observado = teste$credit.rating,
                        previsto = predict(modelo, newdata = teste))


# matriz de confusão
cfm <- confusionMatrix(previsto, teste$credit.rating)
cfm

#Acurácia de 75,5%

# Acurácia: 74.5


# Gerando uma curva ROC em R
#install.packages("ROCR")
library("ROCR")

# Gerando as classes de dados
class1 <- predict(modelo_final, newdata = teste, type = 'prob')
class2 <- teste$credit.rating

pred <- prediction(class1[,2], class2)
perf <- performance(pred, "tpr","fpr") 
plot(perf, col = rainbow(10))