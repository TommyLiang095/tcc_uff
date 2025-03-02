

#############################################
############### SCRIPT 8 -TCC ############### 
#############################################

library(tidyverse)
library(tm)
library(stringi)
library(tidytext)
library(caret)
library(randomForest)
library(xgboost)
library(neuralnet)
library(text2vec)
library(word2vec)
library(pROC)


# Carregando os dados
dados <- readRDS("dados_treino_tratados.rds")


# Carregando a lista de stopword
stop_words = read.delim(file="stopwords_full.txt", header = TRUE)


# 4.2 - Vetorização por Doc2vec

model_word2vec = 
  word2vec( 
    x= c(dados$TWEET),
    type="cbow",
    stopwords = stop_words$Palavras,
    dim = 400)


X = data.frame(
  doc_id = dados$ID,
  text = dados$TWEET)


model_doc2vec = 
  word2vec::doc2vec(
    object = model_word2vec,
    newdata = X)


matrix_doc2vec = as.matrix(model_doc2vec)



# 5. Modelos de Classificação


# 5.1 - APLICAÇÃO DO RANDOM FOREST


# MODELO - DOC2VEC + RANDOM FOREST

# Matriz DOC2VEC  
matrix_doc2vec

# Transformação para data.frame, o random forest não aceita matriz como entrada pro modelo.
matrix_doc2vec <- as.data.frame(matrix_doc2vec)


# Inclusão da variável resposta no Doc2Vec

y = ifelse(dados$TIPO == "NAO_OFENSIVO", 0, # NAO_OFENSIVO = 0
           ifelse(dados$TIPO == "OFENSIVO", 1, # OFENSIVO = 1
                  ifelse(dados$TIPO == "DISCURSO_DE_ODIO", 2, NA)))


# Transformação da variável resposta em fator
matrix_doc2vec$TIPO <- y
matrix_doc2vec$TIPO <- as.factor(matrix_doc2vec$TIPO)
class(matrix_doc2vec)

# Aplicação da semente no modelo

set.seed(218054070)
modelo_rf_doc2vec <- randomForest(TIPO ~ ., data = matrix_doc2vec, ntree = 100)


# Previsão na base de treino
pred_rf <- predict(modelo_rf_doc2vec, matrix_doc2vec)
confusionMatrix(pred_rf, matrix_doc2vec$TIPO)



# 5.2 - APLICAÇÃO DO MODELO XGBOOST


# MODELO - DOC2VEC + XGBOOST

# Matriz Doc2Vec
matrix_doc2vec


# Definição dos parâmetros

xgb_params <- list("objective" = "multi:softprob",
                   "eval_metric" = "mlogloss",
                   "num_class" = 3)

y = ifelse(dados$TIPO == "NAO_OFENSIVO", 0, # NAO_OFENSIVO = 0
           ifelse(dados$TIPO == "OFENSIVO", 1, # OFENSIVO = 1
                  ifelse(dados$TIPO == "DISCURSO_DE_ODIO", 2, NA)))


# Fixando uma semente ao rodar

set.seed(218054070)
xgb_model_doc2vec = xgboost::xgboost(params = xgb_params, data = matrix_doc2vec, label = y, nrounds = 100, verbose = 1)


# Previsão na base de treino
prev_treino = predict(xgb_model_doc2vec, matrix_doc2vec)


# Convertendo probabilidades em classes

num_classes <- 3  # Número de classes
pred_matrix_treino <- matrix(prev_treino, ncol = num_classes, byrow = TRUE)
pred_class <- max.col(pred_matrix_treino) - 1

# Calculando métricas de avaliação
confusion <- confusionMatrix(as.factor(pred_class), as.factor(y))



# 5.3 - APLICAÇÃO DO MODELO DE REDES NEURAIS


# MODELO - DOC2VEC + REDES NEURAIS

# Matriz DOC2VEC
matrix_doc2vec

# Inclusão da variável resposta no Doc2Vec, para isso, transformo num data.frame novamente


y = ifelse(dados$TIPO == "NAO_OFENSIVO", 0, # NAO_OFENSIVO = 0
           ifelse(dados$TIPO == "OFENSIVO", 1, # OFENSIVO = 1
                  ifelse(dados$TIPO == "DISCURSO_DE_ODIO", 2, NA)))


model_doc2vec_rn <- as.data.frame(matrix_doc2vec)
class(model_doc2vec_rn)


# Padronizando os dados para a modelagem
f = function(x){
  m = mean(x)
  s = sd(x)
  return((x-m)/s)
}

model_doc2vec_rn = apply(model_doc2vec_rn,2,FUN = f)


TIPO <- as.factor(y)

base = cbind(as.data.frame(model_doc2vec_rn),TIPO)
class(base)


# Garantindo que cada coluna seja única
#colnames(model_doc2vec_rn2) <- make.names(colnames(model_doc2vec_rn2), unique = TRUE)


# Fixando uma semente ao rodar
set.seed(218054070)

perceptron_rn_doc2vec = neuralnet(formula = TIPO ~ .,
                                  data = base,
                                  linear.output = F,
                                  hidden=c(2,2),
                                  err.fct="ce",
                                  lifesign = "full",
                                  threshold = 6,
                                  lifesign.step = 50)



# Previsão na base de treino
prev_treino_3 = perceptron_rn_doc2vec$net.result[[1]]


# Conversão para fator, é usado na Matriz de Confusão

custo_real = factor(y, levels = 0:2, labels = c("NAO_OFENSIVO", "OFENSIVO", "DISCURSO_DE_ODIO"))


# Conversão das probabilidade estimadas em fator
custo_estimado = factor(
  apply(prev_treino_3, 1, which.max) - 1,  # Ajuste para índices 0, 1, 2
  levels = 0:2,
  labels = c("NAO_OFENSIVO", "OFENSIVO", "DISCURSO_DE_ODIO")
)


# Matriz de confusão
CM_doc2vec = confusionMatrix(data = custo_estimado, reference = custo_real)




###############################################################################################


# Previsão na Base de Teste


# 4.2.1 - Vetorização por Doc2vec na Base de Teste


# Carregando os dados tratados de Teste
dados_teste <- readRDS("dados_teste_tratados.rds")


X_teste = data.frame(
  doc_id = dados_teste$ID,
  text = dados_teste$TWEET)


model_doc2vec_teste = 
  word2vec::doc2vec(
    object = model_word2vec,
    newdata = X_teste)


matrix_doc2vec_teste = as.matrix(model_doc2vec_teste)
#dim(matrix_doc2vec_teste)




# Previsão do modelo DOC2VEC + RANDOM FOREST na Base Teste

# Modelo RF
#modelo_rf_doc2vec

# Doc2vec no Teste
#model_doc2vec_teste
model_doc2vec_teste <- as.data.frame(model_doc2vec_teste)


# Variavel resposta
classe_real_teste <- ifelse(dados_teste$TIPO == "NAO_OFENSIVO", 0, 
                            ifelse(dados_teste$TIPO == "OFENSIVO", 1, 
                                   ifelse(dados_teste$TIPO == "DISCURSO_DE_ODIO", 2, NA)))

# Transformar em fator com níveis
classe_real_teste <- as.factor(classe_real_teste)


#Predição na Base de Teste
pred_rf <- predict(modelo_rf_doc2vec, newdata = model_doc2vec_teste)
levels(pred_rf) <- levels(classe_real_teste)

# Matriz de confusão
confusionMatrix(pred_rf, classe_real_teste)



# Transformar probabilidades previstas
pred_prob_rf <- predict(modelo_rf_doc2vec, newdata = model_doc2vec_teste, type = "prob")


# Criar uma variável binária: Classe 2 (Discurso de Ódio) como referência
classe_real_binaria <- ifelse(classe_real_teste == 2, 1, 0)
#epiDisplay::tab1(classe_real_binaria)


# Gerar a curva ROC para a classe 2 (Discurso de Ódio)
roc_classe2 <- pROC::roc(
  classe_real_binaria,                  
  pred_prob_rf[, "2"],                 
  levels = c(0, 1),                     
  direction = "<"                       
)

# Gerar o gráfico da curva ROC
plot(roc_classe2, 
     main = "Curva ROC - Discurso de Ódio", 
     col = "blue", 
     lwd = 2,
     xlab = "1 - Especificidade",  # Título do eixo X
     ylab = "Sensibilidade")  # Título do eixo Y

# Adicionar o valor de AUC ao gráfico
text(x = 0.6, y = 0.3, 
     labels = paste("AUC = ", round(auc(roc_classe2), 3)), 
     col = "black", cex = 1.2)





# Previsão do modelo DOC2VEC + XGBOOST na Base Teste


# Modelo XGBoost
#xgb_model_doc2vec

prev_teste = predict(xgb_model_doc2vec, matrix_doc2vec_teste)

pred_matrix_teste <- matrix(prev_teste, ncol = num_classes, byrow = TRUE)
pred_class <- max.col(pred_matrix_teste) - 1


y_teste = ifelse(dados_teste$TIPO == "NAO_OFENSIVO", 0, # NAO_OFENSIVO = 0
                 ifelse(dados_teste$TIPO == "OFENSIVO", 1, # OFENSIVO = 1
                        ifelse(dados_teste$TIPO == "DISCURSO_DE_ODIO", 2, NA)))

# Calculando métricas de avaliação
confusion_teste <- confusionMatrix(as.factor(pred_class), as.factor(y_teste))



# Reshape da previsão para obter as probabilidades de cada classe
pred_matrix_teste <- matrix(prev_teste, ncol = num_classes, byrow = TRUE)

# Seleção da classe com a maior probabilidade
pred_class <- max.col(pred_matrix_teste) - 1


# Gerar variável binária para a classe 2 (Discurso de Ódio)
classe_real_binaria <- ifelse(y_teste == 2, 1, 0)

# Gerar a curva ROC para a classe 2 (Discurso de Ódio)
roc_classe2 <- pROC::roc(
  classe_real_binaria,                  
  pred_matrix_teste[, 3],  # Probabilidade da classe 2 (Discurso de Ódio) - "3" se a classe 2 estiver na 3ª coluna
  levels = c(0, 1),                     
  direction = "<"                       
)


# Gerar o gráfico da curva ROC
plot(roc_classe2, 
     main = "Curva ROC - Discurso de Ódio", 
     col = "blue", 
     lwd = 2,
     xlab = "1 - Especificidade", 
     ylab = "Sensibilidade")


# Adicionar o valor de AUC ao gráfico
text(x = 0.6, y = 0.3, 
     labels = paste("AUC = ", round(auc(roc_classe2), 3)), 
     col = "black", cex = 1.2)




# Previsão do modelo DOC2VEC + REDES NEURAIS na Base Teste


# Modelo Redes Neurais
perceptron_rn_doc2vec

# Matriz Doc2Vec
matrix_doc2vec_teste


prev_teste_3 = (perceptron_rn_doc2vec |> neuralnet::compute(matrix_doc2vec_teste))$net.result
#prev_teste_3 = prev_teste_3 |> cbind(1-apply(prev_teste_3, MARGIN = 1, FUN="sum"))


# Variável resposta com fator e níveis

custo_real = factor(dados_teste$TIPO, levels = c("NAO_OFENSIVO", "OFENSIVO", "DISCURSO_DE_ODIO"))


custo_estimado = factor(
  apply(X = prev_teste_3, MARGIN = 1, FUN = "which.max"),
  levels = 1:3,
  labels = c("NAO_OFENSIVO", "OFENSIVO", "DISCURSO_DE_ODIO"))


# Matriz de confusão
CMteste3 = confusionMatrix(data = custo_estimado,reference = custo_real)
















