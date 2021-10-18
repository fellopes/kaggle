############ Titanic Predictions ##############################
# Dataset fornecido pelo Kaggle, objetivo principal é treinar as técnicas de ML;

setwd('C:\\Users\\fel_l\\OneDrive\\Área de Trabalho\\MBA - Data Science\\5. Python\\Exercícios')

dataset_treino <- read.table("train.csv", sep = ",", dec = ".", header = T)

###### Instalando os pacotes da aula de Modelos Logísticos - binários #########

pacotes <- c("plotly","tidyverse","knitr","kableExtra","fastDummies","rgl","car",
             "reshape2","jtools","lmtest","caret","pROC","ROCR","nnet", "magick",
             "cowplot")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
} 

summary(dataset_treino)

dataset_treino_dummies <- dummy_columns(.data = dataset_treino,
                                    select_columns = c("Pclass", 
                                                       "Sex",
                                                       "Embarked"),
                                    remove_selected_columns = T,
                                    remove_first_dummy = T)
dataset_treino_dummies$Name <- NULL
dataset_treino_dummies$Ticket <- NULL
dataset_treino_dummies$Cabin <- NULL

dataset_treino_dummies2  <- dataset_treino_dummies

dataset_treino_dummies2$Age <- NULL

modelo1 <- glm(formula = Survived ~ ., 
                      data = dataset_treino_dummies, 
                      family = "binomial")

modelo2 <- glm(formula = Survived ~ ., 
               data = dataset_treino_dummies2, 
               family = "binomial")

summary(modelo1)
summary(modelo2)
step_dummies <- step(object = modelo1,
                                k = qchisq(p = 0.05, df = 1, lower.tail = FALSE)) 
summary(step_dummies)

step_dummies2 <- step(object = modelo2,
                                k = qchisq(p = 0.05, df = 1, lower.tail = FALSE)) 

summary(step_dummies2)

dataset_test <- read.table("test.csv", sep = ",", dec = ".", header = T)
dataset_test_dummies <- dummy_columns(.data = dataset_test,
                                        select_columns = c("Pclass", 
                                                           "Sex",
                                                           "Embarked"),
                                        remove_selected_columns = T,
                                        remove_first_dummy = T)

dataset_test_dummies2 <- dummy_columns(.data = dataset_test,
                                      select_columns = c("Pclass", 
                                                         "Sex",
                                                         "Embarked"),
                                      remove_selected_columns = T,
                                      remove_first_dummy = T)
dataset_test_dummies$Name <- NULL
dataset_test_dummies$Parch <- NULL
dataset_test_dummies$Fare <- NULL
dataset_test_dummies$Ticket <- NULL
dataset_test_dummies$Cabin <- NULL
dataset_test_dummies$Embarked_Q <- NULL
dataset_test_dummies$Embarked_S <- NULL

dataset_test_dummies2$Name <- NULL
dataset_test_dummies2$Parch <- NULL
dataset_test_dummies2$Fare <- NULL
dataset_test_dummies2$Ticket <- NULL
dataset_test_dummies2$Cabin <- NULL
dataset_test_dummies2$Embarked_Q <- NULL

rownames(dataset_test_dummies) <- dataset_test_dummies[,1]
dataset_test_dummies$PassengerId<- NULL
predicoes <- predict(step_dummies, newdata = dataset_test_dummies)

predicoes2 <- predict(step_dummies2, newdata = dataset_test_dummies2)


predicoes <- data.frame(predicoes)
predicoes_prob <-1/(1+exp(-predicoes))

predicoes2 <- data.frame(predicoes2)
predicoes_prob2 <-1/(1+exp(-predicoes2))

predicoes_final <- c(predicoes_prob,predicoes_prob2)
predicoes_final <- data.frame(predicoes_final)

for (i in 1:nrow(predicoes_final)){
  if(is.na(predicoes_final[i,1])){
    predicoes_final[i,1] <- predicoes_final[i,2] 
    }   
}

Survived_hat <- 1:418
for (i in 1:nrow(predicoes_final)){
  if(predicoes_final[i,1] < 0.5){
    Survived_hat[i] <- 0 
   } else {
     Survived_hat[i] <- 1   
  }
}
Survived_hat <-data.frame(Survived_hat)

