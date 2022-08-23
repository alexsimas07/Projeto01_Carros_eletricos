####### Projeto com Feedback - Machine Learning em Logística ######
# Problema de negócio:
# Prevendo o Consumo de Energia de Carros Elétricos

# Para está analise, usaremos um dataset feito no Excel


# Instalando um pacote
install.packages("GGally")
install.packages("tidymodels")

# Carregando pacotes
library(dplyr)
library(ggplot2)
library(tidyr)
library(readxl)
library(GGally)
library(caret)
library(psych)

####### Carregando os dados #####
dados <- read_excel("dados/FEV-data-Excel.xlsx")

# Dimensões
dim(dados)

# Visualizar os dados
View(dados)

# Variáveis e tipos de dados - maioria numerica
str(dados)

# Sumário das variáveis numéricas
summary(dados)

###### Análise Exploratória dos Dados - Limpeza dos Dados ######

# Quantas linhas tem casos completos
complet_case <- sum(complete.cases(dados))
View(complet_case)

# Quantas linhas tem casos incompletos
not_complet_case <- sum(!complete.cases(dados))
View(not_complet_case)

# Percentual de dados incompletos
percentual <- (not_complet_case / complet_case) * 100
percentual
rm(percentual)

# Remove os objetos anteriores para liberar memória RAM
rm(complet_case)
rm(not_complet_case)

# Nomes das colunas
colnames(dados)

# Gravar os nomes das colunas em um vetor
minhas_colunas <- colnames(dados)
minhas_colunas

# Renomeando colunas
minhas_colunas[1] <- "Nome_dos_carros"
minhas_colunas[2] <- "Marca"
minhas_colunas[3] <- "Modelo"
minhas_colunas[4] <- "Preco_minino_bruto"
minhas_colunas[5] <- "Potencia_motor"
minhas_colunas[6] <- "Maximo_torque"
minhas_colunas[7] <- "Tipo_de_freio"
minhas_colunas[8] <- "Tipo_de_direcao"
minhas_colunas[9] <- "Capacidade_bateria"
minhas_colunas[10]<- "Alcance_KM"
minhas_colunas[11]<- "Distancia_entre_eixos"
minhas_colunas[12]<- "Comprimento"
minhas_colunas[13]<- "Largura"
minhas_colunas[14]<- "Altura"
minhas_colunas[15]<- "Peso_vazio_minimo"
minhas_colunas[16]<- "Peso_bruto_permitido"
minhas_colunas[17]<- "Capacidade_carga_max"
minhas_colunas[18]<- "Numero_assento"
minhas_colunas[19]<- "Numero_porta"
minhas_colunas[20]<- "Tamanho_pneu"
minhas_colunas[21]<- "Velocidade_maxima"
minhas_colunas[22]<- "Capacidade_inicializa"
minhas_colunas[23]<- "Aceleracao_0_100"
minhas_colunas[24]<- "Potencia_maxima_de_carregamento"
minhas_colunas[25]<- "Media_Consumo_energia"

# Verificando resultado
minhas_colunas

# Atribuindo novos nomes da colunas ao dataset
colnames(dados)<-minhas_colunas
rm(minhas_colunas)
View(dados)
summary(dados)
# Pela função summy ja foi identificado valores NA´s - Porém vamos confirmar

# Verificando se existem valores NA
colSums(is.na(dados))

# Existem 9 valores NA na media de consumo, 3 na aceleracao, 1 capacidade
# Eu decidi colocar a média dos valores no lugar dos NA´s

dados$Media_Consumo_energia[is.na(dados$Media_Consumo_energia)] <- mean(dados$Media_Consumo_energia, na.rm = TRUE)

dados$Tipo_de_freio[52] <- "disc (front + rear)"

dados$Peso_bruto_permitido[is.na(dados$Peso_bruto_permitido)] <- mean(dados$Peso_bruto_permitido, na.rm = TRUE)

dados$Capacidade_carga_max[is.na(dados$Capacidade_carga_max)] <- mean(dados$Capacidade_carga_max, na.rm = TRUE)

dados$Aceleracao_0_100[is.na(dados$Aceleracao_0_100)] <- mean(dados$Aceleracao_0_100, na.rm = TRUE)

dados$Capacidade_inicializa[is.na(dados$Capacidade_inicializa)] <- mean(dados$Capacidade_inicializa, na.rm = TRUE)
View(dados)
colSums(is.na(dados))
# Não possui mais valores NA´S

# Extraindo variáveis númericas
num_variavel <- sapply(dados, is.numeric)
num_data <- dados[num_variavel]
View(num_data)

# Matriz de correlação
cor(num_data$Media_Consumo_energia, num_data$Maximo_torque)

# Correlation plot - Gráfico de Correlação - GGally
num_data %>%
  select(Media_Consumo_energia, Potencia_motor, Maximo_torque, Capacidade_bateria, Velocidade_maxima,
         Peso_bruto_permitido, Tamanho_pneu, Alcance_KM, Altura, Potencia_maxima_de_carregamento) %>%
  ggpairs() 

# Pelo gráfico pude perceber que Altura e Alcance_KM não tem correlação

##### Análise de regressão #####
# Usando metodo ANOVA
# A relação potencia do motor e Consumo médio de fato se confirma?
# Variável dependente = Media_Consumo_energia
# Variavel independente = Potencia do motor

# H0 = Não há relação com de Potencia do motor com Media de consumo
# H1 = Existe relação com a Potencia do motor e Media de consumo

modelo_anova_1 <- aov(Media_Consumo_energia ~ ., data = num_data)
summary(modelo_anova_1)

# Neste caso o valor P é menor que 0.05 sendo assim Rejeitamos H0.
# Existe relação da potencia do motor com a média de consumo

# Calculo mostrou todas as variaveis que existe uma alta Significacância

####### Partiremos para a fase de split data ou separação de dados - Teste e Treino ######

split <- createDataPartition(y = num_data$Media_Consumo_energia, p = 0.7, list = FALSE)
split
rm(num_data_teste, num_data_treino)
# Criando dados de treino e teste
num_data_treino <- num_data[split,]
num_data_teste <- num_data[-split,]

# Vamos treinar o modelo usando o calculo de AOV que mostrou a significancia também.
modelo_v1 <- lm(Media_Consumo_energia ~ Maximo_torque + Alcance_KM + Peso_bruto_permitido + Tamanho_pneu + Distancia_entre_eixos + Potencia_motor, data = num_data_treino)
summary(modelo_v1)

# R-squared = 0.84

# com este metodo pude perceber que posso retirar 2 variaveis, por escolha minha, para comparar os resultados
# Vamos fazer outro modelo com random forest e mais um de teste com as variaveis que escolhi 

modelo_v2 <- lm(Media_Consumo_energia ~ Peso_bruto_permitido + Tamanho_pneu + Alcance_KM +
                     Potencia_motor + Altura + Potencia_maxima_de_carregamento + Maximo_torque,
                   data = num_data_treino)
summary(modelo_v2)
# R-squared = 0.86
# Mais um vez foi identificado que maximo_torque não tem importancia 
# vamos fazer mais um modelo de regressão e ai partiremos para random forest

modelo_v3 <- lm(Media_Consumo_energia ~ Peso_bruto_permitido + Tamanho_pneu + Alcance_KM +
                     Potencia_motor + Altura + Potencia_maxima_de_carregamento, 
                   data = num_data_treino)
summary(modelo_v3)
# R-squared = 0.862

modelo_v4 <- lm(Media_Consumo_energia ~ Potencia_motor + Alcance_KM + Tamanho_pneu + Peso_bruto_permitido, data = num_data_teste)
summary(modelo_v4)

# R -squared = 0.94

# O modelo usado será o 'modelo_v4'

# Agora vamos para as previsões
?predict
predict_valores <- predict(modelo_v4, num_data_teste)
View(num_data_teste)

# Criando uma tabela combinando as colunas
resultado <- cbind(predict_valores, num_data_teste$Media_Consumo_energia)
colnames(resultado)<-c("Previsto", "Real")
resultado <- as.data.frame(resultado)
resultado
