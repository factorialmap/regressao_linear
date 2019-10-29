#Importar os dados propaganda vs vendas
library(xlsx)
dados <- read.xlsx(file.choose(), sheetIndex = 1)

#Plotando os dados
plot(dados, lwd=5, col="blue")

#Gerando o modelo
modelo <- lm(vendas ~ gasto_propaganda, data=dados)
modelo

#Plotando a linha de regressao
abline(modelo, col="red", lwd=3)

#Mostrando os resultados do modelo
summary(modelo)
anova(modelo)

##Correlacao


##Chamando os dados de mtcars
data(mtcars)

head(mtcars)

##Procurando variaveis correlacionadas
library(GGally)
ggpairs(mtcars,lower = list(continuous = "smooth"))

#Plotar um grafico com os dados mpg e wt(peso) na cor vermelha
plot(mpg ~ wt, data = mtcars, col = "blue", lwd=5)

#Sera que o peso interfere nas milhas por galao
modelo <-  lm(mpg ~ wt, data = mtcars)

#Inserindo a reta de regressao no grafico 
abline(modelo, col = "red", lwd=3)

summary(modelo)


##########MACHINE LEARNING##############

#Passos
#1. Separar o conjunto de dados em 70% treino e 30% teste
#2. Construir modelo de regressáo usando os dados de treino
#3. Fazer a predicao usando os dados de teste e calcular a acuracidade do modelo
library(tidyverse)
library(caret)

#Chamando os dados que iremos usar como exemplo
#Obrigado ao Kassambara pelos dados
data("marketing", package = "datarium")

#Usando a funcao sample_n do dplyr
sample_n(marketing,10)

#Separar os dados em treino e teste
set.seed(123)

#Dividindo os dados
#Caso nao use o caret amostra_treino <- sample(1:nrow(marketing), 0.7*nrow(marketing))
amostra_treino <- marketing$sales %>%   
                createDataPartition(p = 0.7, list = F)
#Criando subconjunto com 70% dos dados e outro com 30%
dados_treino <- marketing[amostra_treino,]  #treino <- marketing[amostra_treino,]
dados_teste <- marketing[-amostra_treino,]  #teste <- marketing[-amostra_treino,]

#Criando o modelo 1
modelo_1 <- lm(sales ~., data = dados_treino)
summary(modelo_1)

predicao_1 <- predict(modelo_1, dados_teste)

RMSE(predicao_1, dados_teste$sales)
R2(predicao_1,dados_teste$sales)
AIC(modelo_1)

#Criando o modelo 2
modelo_2 <- lm(sales ~ youtube, data = dados_treino)
summary(modelo_2)

predicao_2 <- predict(modelo_2, dados_teste)

RMSE(predicao_2, dados_teste$sales)
R2(predicao_2,dados_teste$sales)
AIC(modelo_2)

#Criando modelo 3
modelo_3 <- lm(sales ~ youtube + facebook, data = dados_treino)
summary(modelo_3)

predicao_3 <- predict(modelo_3, dados_teste)

RMSE(predicao_3, dados_teste$sales)
R2(predicao_3,dados_teste$sales)
AIC(modelo_3)

