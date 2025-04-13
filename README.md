Este repositório contém os scripts e as bases de dados utilizados para a análise e modelagem de um classificador de detecção de discurso de ódio.
Os Scripts foram separados por tópicos para melhor organização e manuseio de cada etapa.

## 📜 Scripts

Os scripts estão enumerados por ordem de criação e execução.

- **0 - Script - Balanceamento da base.R**  
  Aplicação de técnicas de balanceamento para lidar com o desbalanceamento entre as classes do conjunto de dados rotulado.

- **1 - Script - Tratamento Base Treino.R**  
  Realiza a criação de identificadores únicos para os tweets e aplica uma limpeza textual inicial, incluindo remoção de menções, números, pontuações, URLs, emojis, acentos e normalização para letras minúsculas.
  
- **2 - Script - Dados Lematizados.R**
  Realiza a tokenização, remoção de stopwords, lematização e seleção de palavras (Lei de Zipf).

- **3 - Script - Tratamento Base Teste.R**
  Realiza a criação de identificadores e o pré-processamento textual da base de teste. Obs.: Os mesmos realizados na base de treino.

- **4 - Script - Dados Lematizados Teste.R**  
  Realiza a tokenização, lematização e seleção dos termos da base de teste com base no realizado e definido na base de treino.

- **5 - Script - Modelos por TF-IDF.R**
  Aplicação de três modelos de classificação — Random Forest, XGBoost e Perceptron Multicamadas — utilizando vetorização de texto via TF-IDF.
  
- **6 - Script - Modelos por MTD.R**
  Aplicação de três modelos de classificação — Random Forest, XGBoost e Perceptron Multicamadas — utilizando vetorização de texto via MTD (Matriz Termo Documento).
  
- **7 - Script - Modelos por Doc2Vec.R**
    Aplicação de três modelos de classificação — Random Forest, XGBoost e Perceptron Multicamadas — utilizando vetorização de texto via Doc2Vec.
  

## 📜 Base de Dados





