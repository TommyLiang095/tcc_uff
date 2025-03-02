

#####################
### BASE DE TESTE ###
#####################


# 2.1 Tratamento e preprocessamento da base de teste para validação dos modelos de previsão

# Carregamento da base de teste
dados_teste <- readRDS("teste.rds")


# Criando uma coluna de Identificador para cada Tweet
dados_teste = mutate(dados_teste, ID = c(1:length(dados_teste$TWEET)))
dados_teste <- dplyr::select(dados_teste, ID, TWEET, TIPO)


# 3.1 Pré-processamento dos textos (Teste)


# Tratamento dos textos Base Teste


dados_teste$TWEET = gsub(",", " ", dados_teste$TWEET)
dados_teste$TWEET = gsub("“", " ",  dados_teste$TWEET)
dados_teste$TWEET = gsub("”", " ",  dados_teste$TWEET)
dados_teste$TWEET = gsub("@[^ ]*", " ", dados_teste$TWEET)
dados_teste$TWEET = gsub("[0-9]", " ", dados_teste$TWEET)
dados_teste$TWEET = gsub("[[:punct:]]", " ", dados_teste$TWEET)
#dados_teste$TWEET = dados_teste$TWEET |> removeNumbers()                            # Remoção de números
#dados_teste$TWEET = dados_teste$TWEET |> removePunctuation()                        # Remoção de pontuações
dados_teste$TWEET <- dados_teste$TWEET |> stripWhitespace()                          # Remoção de espaços em branco
dados_teste$TWEET <- dados_teste$TWEET |> tolower()                                  # Conversão para letras minúsculas
dados_teste$TWEET <- gsub("http\\S+|www\\S+", "", dados_teste$TWEET)                 # Remoção de URLs
dados_teste$TWEET = stri_trans_general(str = dados_teste$TWEET, 
                                       id = "Latin-ASCII")                           # Remoção de acentos

dados_teste$TWEET <- stri_replace_all_regex(dados_teste$TWEET, "[^\\p{ASCII}]", "")  # Remoção de emojis
dados_teste$TWEET = sub("^\\s+", "", dados_teste$TWEET)



saveRDS(dados_teste, "dados_teste_tratados.rds")




