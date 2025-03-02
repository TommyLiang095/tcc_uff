

#############################################
############### SCRIPT 2 -TCC ############### 
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


### Tokenização ###

# A Função Unnest_tokens por padrão(default) realiza os procedimentos de remoções de pontuações, remoção de espaços em branco e conversão para letras minúsculas,
# usaremos ela para complementar com as funções feitas anteriormente.

dados_token = tidytext::unnest_tokens(dados,output=Palavras,input=TWEET) ## Mantém a estrutura Tidy

# head(dados_token,10)


### Remoção de stopwords ###


# Lista de STOPWORD utilizada é uma junção das lista do Anexo TCC(João), Github e do Pacote Tidytext

# Github: "https://gist.githubusercontent.com/alopes/5358189/raw/2107d809cca6b83ce3d8e04dbd9463283025284f/stopwords.txt"
# Pacote Tidytext: tidytext::get_stopwords(language = "pt")
# Anexo TCC(joão): "stopwords.txt" 


# Carregando a lista de stopword
stop_words = read.delim(file="stopwords_full.txt", header = TRUE)


# Remoção de Stopwords
dados_sw = dplyr::anti_join(dados_token, stop_words, by="Palavras")


# Contagem de frequência por palavras
# head(dplyr::count(dados_sw,Palavras, sort = TRUE))



### Lematização das palavras ###


# Carregando a lista de lemmatização
lemma_novo <- readRDS("lemma_novo.rds")

dados_lem = dplyr::left_join(dados_sw, lemma_novo, 
                             by='Palavras')

dados_lem$Palavras = ifelse(is.na(dados_lem$stem),
                            dados_lem$Palavras,dados_lem$stem)

dados_lem = dplyr::select(dados_lem,-stem)


# Retirando novamente as stopword, pós lematização
dados_lem = dplyr::anti_join(dados_lem, stop_words, by="Palavras")


# Trazendo uma contagem de frequência das palavras

dados_lem = group_by(dados_lem,Palavras)

freq = arrange(summarize(dados_lem, frequencia = n()),
               desc(frequencia))

dados_lem = left_join(dados_lem,freq,'Palavras')

dados_lem = ungroup(dados_lem)


#dim(dados_lem)


# Foi observado uma frequência muito grande para termos com tamanho 1, para evitar ruídos foram retirados esses termos da base
# Retirando termos com apenas uma letra.

dados_lem = dados_lem %>% 
  mutate(nch = nchar(Palavras))

#resultado <- dados_lem %>%
#  filter(nch == 1) %>%
#  distinct(Palavras)

dados_lem = dados_lem %>% 
  filter(nch > 1)

dados_lem = dados_lem %>% 
  select(-nch)


# Análise para escolha da seleção de termos
# Term Frequency Thresholding and Zipf’s Law(Lei de Zipf's para seleção dos termos)

palavras_freq = dados_lem %>% 
  select(Palavras, frequencia) %>% 
  arrange(desc(frequencia)) %>% 
  unique()


x = 1:length(palavras_freq$frequencia)
y = palavras_freq$frequencia


plot(x,y,log = "xy",
     xlab="log(rank)",ylab="log(freq)",
     cex.lab=1.5,type="l")


lm(log(x[200:500]) ~ log(y[200:500]))
curve(exp(10.429-1*log(x)),add = T,col="red")
abline(v=101,lty=2,col="gray")
abline(v=500,lty=2,col="gray")

# Seleção dos 400 termos entre as posições 101 e 500 
palavras_usadas = palavras_freq[101:500, "Palavras"]

#palavras_usadas$Palavras


# Junção dos termos a serem utilizados

dados_lem = 
  dados_lem |> 
  right_join(palavras_usadas)



saveRDS(dados_lem, "dados_lem.rds")













