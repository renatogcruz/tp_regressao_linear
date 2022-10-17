#--
# Pacotes utilizados 

pacotes <- c("plotly","tidyverse","ggrepel","fastDummies","knitr","kableExtra",
             "splines","reshape2","PerformanceAnalytics","metan","correlation",
             "see","ggraph","nortest","rgl","car","olsrr","jtools","ggstance",
             "magick","cowplot","beepr","Rcpp")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

#--
# Limpando a memoria do R
rm(list=ls(all=TRUE))


#--
# leitura do banco de dados
empresas <- read.csv(file = "empresas.csv")


#--
# OBSERVANDO OS DADOS CARREGADOS DO DATASET
empresas %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 12)

# Visualizando as observaÃ§Ãµes e as especificaÃ§Ãµes referentes Ã s variÃ¡veis 
# do dataset
glimpse(empresas) 

# EstatÃ­sticas univariadas
summary(empresas)


#--
#  análise descritiva exploratória



#--
# Descrição das variáveis e do problema relacionado ao seu conjunto de dados

#--

#  Investigação e análise da correlação linear entre a variável resposta e 
# cada uma das variáveis explicativas


#--
# Escolha, aplicação e descrição de método de seleção de variáveis aplicado, 
# o que idealmente vem acompanhado de uma an´alise de multicolinearidade. 
# An´alise de medidas de comparação entre modelos concorrentes e análise dos 
# resultados dos testes de significância. Realizar e analisar o Teste da Falta 
# de Ajuste caso acha réplicas no conjunto de variáveis explicativas.



#--
# Investigar se faz sentido a inclusão de termos de interação entre as 
# variáveis explicativas ou outros termos de ordem superior (regressão polinomial). 
# Analisar necessidade/viabilidade de transformação nas variáveis explicativas



#--
# Interpretação dos parâmetros do modelo final escolhido e análise do 
# porcentagem da variabilidade da variável resposta que é explicada 
# pela(s) variável(is) explicativa(s) no seu modelo final.


#--
# Fornecer estimativa para a variância ??² do termo de erro do modelo


#--
# Escolha de um conjunto de valores para a(s) variável(is) explicativa(s) 
# presentes no seu modelo final e realização de previsão aplicando estes 
# valores no modelo ajustado. Gráfico com bandas de confiança e de predição 
# caso o modelo final contenha apenas uma variável explicativa.



#--
# Escolha de um conjunto de valores para a(s) variável(is) explicativa(s) 
# presentes no seu modelo final e realização de previsão aplicando 
# estes valores no modelo ajustado. Gráfico com bandas de confiança e de 
# predição caso o modelo final contenha apenas uma variável explicativa.


#--
# Analisar se existem observações discrepantes/influentes que, potencialmente, 
# podem estar influenciando no ajuste obtido. Se for o caso, ajustar 
# um modelo de regressão retirando tais observações e analisar os 
# resultados obtidos.



