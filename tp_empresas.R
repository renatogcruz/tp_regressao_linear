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

# Visualizando as observações e as especificações referentes às variáveis 
# do dataset
glimpse(empresas) 

# Estatísticas univariadas
summary(empresas)


#--
#  an?lise descritiva explorat?ria



#--
# Descri??o das vari?veis e do problema relacionado ao seu conjunto de dados

#--

#  Investiga??o e an?lise da correla??o linear entre a vari?vel resposta e 
# cada uma das vari?veis explicativas


#--
# Escolha, aplica??o e descri??o de m?todo de sele??o de vari?veis aplicado, 
# o que idealmente vem acompanhado de uma an?alise de multicolinearidade. 
# An?alise de medidas de compara??o entre modelos concorrentes e an?lise dos 
# resultados dos testes de signific?ncia. Realizar e analisar o Teste da Falta 
# de Ajuste caso acha r?plicas no conjunto de vari?veis explicativas.



#--
# Investigar se faz sentido a inclus?o de termos de intera??o entre as 
# vari?veis explicativas ou outros termos de ordem superior (regress?o polinomial). 
# Analisar necessidade/viabilidade de transforma??o nas vari?veis explicativas



#--
# Interpreta??o dos par?metros do modelo final escolhido e an?lise do 
# porcentagem da variabilidade da vari?vel resposta que ? explicada 
# pela(s) vari?vel(is) explicativa(s) no seu modelo final.


#--
# Fornecer estimativa para a vari?ncia ??? do termo de erro do modelo


#--
# Escolha de um conjunto de valores para a(s) vari?vel(is) explicativa(s) 
# presentes no seu modelo final e realiza??o de previs?o aplicando estes 
# valores no modelo ajustado. Gr?fico com bandas de confian?a e de predi??o 
# caso o modelo final contenha apenas uma vari?vel explicativa.



#--
# Escolha de um conjunto de valores para a(s) vari?vel(is) explicativa(s) 
# presentes no seu modelo final e realiza??o de previs?o aplicando 
# estes valores no modelo ajustado. Gr?fico com bandas de confian?a e de 
# predi??o caso o modelo final contenha apenas uma vari?vel explicativa.


#--
# Analisar se existem observa??es discrepantes/influentes que, potencialmente, 
# podem estar influenciando no ajuste obtido. Se for o caso, ajustar 
# um modelo de regress?o retirando tais observa??es e analisar os 
# resultados obtidos.



