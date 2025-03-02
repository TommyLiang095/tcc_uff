
# Balanceamento de dados Treino

set.seed(218054070)
dados <- readRDS("treino.rds")
epiDisplay::tab1(dados$TIPO)

indice1 = which(dados$TIPO=="NAO_OFENSIVO")
sorteio1 = sample(x=indice1, size=6000, replace = F)

indice2 = which(dados$TIPO=="OFENSIVO")
sorteio2 = sample(x=indice2, size=6000, replace = T)

indice3 = which(dados$TIPO=="DISCURSO_DE_ODIO")
sorteio3 = sample(x=indice3, size=6000, replace = T)

dados_balanceado = rbind(dados[sorteio1,], dados[sorteio2,], dados[sorteio3,])
epiDisplay::tab1(dados_balanceado$TIPO)

saveRDS(dados_balanceado, "treino_balanceado.rds")













