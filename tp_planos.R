############################################################################
## Disciplina Regressao Linear I
## Especializacao em Estatistica - UFMG
## Prof. Guilherme Lopes de Oliveira
## Tratablho Prático
## Data: XX/10/2022
## Alunos: Gustavo Macedo Miranda
##         Renato Godoi da Cruz
## Dados: "dados_venda_refeicoes_propaganda.csv"
## Objetivo: Teste da Falta de Ajuste e Regressao Polinomial
############################################################################

#--
# Limpando a memoria do R
rm(list=ls(all=TRUE))


#--
# Pacotes utilizados
pacotes <- c("plotly",
             "tidyverse",
             "ggrepel",
             'gtools',
             "fastDummies",
             "knitr",
             "kableExtra",
             "splines",
             "reshape2",
             "PerformanceAnalytics",
             "metan",
             "correlation",
             "see",
             "ggraph",
             "nortest",
             "rgl",
             "car",
             "olsrr",
             "jtools",
             "ggstance",
             "magick",
             "cowplot",
             "beepr",
             "Rcpp",
             'GGally')

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
# leitura do banco de dados
planos <- read.csv(file = "planosaude.csv")

#--
# OBSERVANDO OS DADOS CARREGADOS DO DATASET
planos %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 12)

# Matriz de gráficos no pacote
# https://rpubs.com/melinatarituba/35326
ggpairs(planos, lower = list(continuous = "smooth"))

#--
#  análise descritiva exploratória

# Função para fazer a análise descritiva #
# Vamos avaliar a distribuição de despesas por cada variável X
# Sumarizamos então y por categoria de X e montamos um gráfico de perfis

# não faz sentido (deveria ser uma variavel qualitativa no lugar de despesas)

descritiva <- function(var){
  # Sumariza a taxa de sobreviventes por categoria da variável em análise
  tgc <- Rmisc::summarySE(planos, measurevar="despmed", groupvars=c(var))
  
  ggplot(tgc) + 
    # Plota o gráfico de barras com as frequências
    geom_bar(aes(x=tgc[,var], weight=N/891, fill=as.factor(tgc[,var]))) + 
    # Plota as barras de erro
    geom_errorbar(aes(x=tgc[,var], y=despmed, ymin=despmed-se, ymax=despmed+se, colour='1'), width=.1) +
    # Plota as médias de cada grupo
    geom_point(aes(x=tgc[,var], y=despmed, colour='1', group='1')) +
    # Plota as linhas que conectam as médias
    geom_line(aes(x=tgc[,var], y=despmed, colour='1', group='1')) +
    # Escala de cores do gráfico de médias
    scale_color_viridis_d(direction = -1, begin=0, end=.25) +
    # Escala de cores do gráfico de barras
    scale_fill_viridis_d(direction = -1, begin=.85, end=.95) +
    # Estética mais 'leve' do gráfico
    theme(panel.background = element_rect(fill = "white", colour = "grey", linetype = "solid"),
          panel.grid.major = element_line(size = 0.15, linetype = 'solid', colour = "grey")) + 
    # Remove a legenda
    theme(legend.position = "none") +
    # Rótulo dos eixos
    xlab(var) + ylab("Despesas médicas") + 
    # Marcas do eixo secundário
    scale_y_continuous(sec.axis = sec_axis(~.*891, name = "Frequencia"), labels = scales::percent)
}

descritiva("idade")
descritiva("dcron")
descritiva("renda")
descritiva("plano")



#--
# Descrição das variáveis e do problema relacionado ao seu conjunto de dados

#--

#  Investigação e análise da correlação linear entre a variável resposta e 
# cada uma das variáveis explicativas

# OBSERVANDO OS DADOS CARREGADOS DA BASE planosaude
glimpse(planos)

#Estatísticas univariadas
summary(planos)

#Categorias da variável 'plano'
levels(factor(planos$plano))

#Tabela de frequências absolutas da variável 'plano'
table(planos$plano)

# ESTUDO DAS CORRELAÇÕES
chart.Correlation((planos[2:5]), histogram = TRUE)



#--
# Escolha, aplicação e descrição de método de seleção de variáveis aplicado, 
# o que idealmente vem acompanhado de uma an´alise de multicolinearidade. 
# An´alise de medidas de comparação entre modelos concorrentes e análise dos 
# resultados dos testes de significância. Realizar e analisar o Teste da Falta 
# de Ajuste caso acha réplicas no conjunto de variáveis explicativas.

# PROCEDIMENTO N-1 DUMMIES
planos_dummies <- dummy_columns(.data = planos,
                                select_columns = "plano",
                                remove_selected_columns = T,
                                remove_most_frequent_dummy = T)

#Visualizando a base de dados dummizada
planos_dummies %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 12)

#--
# Investigar se faz sentido a inclusão de termos de interação entre as 
# variáveis explicativas ou outros termos de ordem superior (regressão polinomial). 
# Analisar necessidade/viabilidade de transformação nas variáveis explicativas

# ESTIMAÇÃO DA REGRESSÃO LINEAR MÚLTIPLA
#Modelagem com todas as variáveis
modelo_planosaude <- lm(despmed ~ . - id, planos_dummies)

#Parâmetros do modelo_planosaude
summary(modelo_planosaude)

# PROCEDIMENTO STEPWISE
step_planosaude <- step(modelo_planosaude, k = 3.841459)

summary(step_planosaude)

# TESTE DE VERIFICAÇÃO DA ADERÊNCIA DOS RESÍDUOS À NORMALIDADE
#Teste de Shapiro-Francia
sf.test(step_planosaude$residuals) #função sf.test do pacote nortest

#Plotando os resíduos do modelo step_planosaude 
planos %>%
  mutate(residuos = step_planosaude$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(color = "white", 
                 fill = "#55C667FF", 
                 bins = 15,
                 alpha = 0.6) +
  labs(x = "Resíduos",
       y = "Frequências") + 
  theme_bw()

#Acrescentando uma curva normal teórica para comparação entre as distribuições
planos %>%
  mutate(residuos = step_planosaude$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "white", 
                 fill = "#55C667FF", 
                 bins = 15,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(step_planosaude$residuals),
                            sd = sd(step_planosaude$residuals)),
                size = 2, color = "grey30") +
  scale_color_manual(values = "grey50") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme_bw()

#Kernel density estimation (KDE) - forma não-paramêtrica para estimar a
#função densidade de probabilidade de uma variável aleatória
planos_dummies %>%
  ggplot() +
  geom_density(aes(x = step_planosaude$residuals), fill = "#55C667FF") +
  labs(x = "Resíduos do Modelo Stepwise",
       y = "Densidade") +
  theme_bw()

# DIAGNÓSTICO DE HETEROCEDASTICIDADE
#Teste de Breusch-Pagan para diagnÃ³stico de heterocedasticidade
ols_test_breusch_pagan(step_planosaude)
#funÃ§Ã£o ols_test_breusch_pagan do pacote olsrr
#PresenÃ§a de heterocedasticidade -> omissÃ£o de variÃ¡vel(is) explicativa(s) relevante(s)

#H0 do teste: ausÃªncia de heterocedasticidade.
#H1 do teste: heterocedasticidade, ou seja, correlaÃ§Ã£o entre resíduos e uma ou mais
#variÃ¡veis explicativas, o que indica omissÃ£o de variÃ¡vel relevante!

#Adicionando fitted values e resíduos do modelo 'step_planosaude'
#no dataset 'planosaude_dummies'
planos_dummies$fitted_step <- step_planosaude$fitted.values
planos_dummies$residuos_step <- step_planosaude$residuals

#Gráfico que relaciona resíduos e fitted values do modelo 'step_planosaude'
planos_dummies %>%
  ggplot() +
  geom_point(aes(x = fitted_step, y = residuos_step),
             color = "#55C667FF", size = 3) +
  labs(x = "Fitted Values do Modelo Stepwise",
       y = "Resíduos do Modelo Stepwise") +
  theme_bw()

# TRANSFORMAçÃO DE BOX-COX
#Para calcular o lambda de Box-Cox
lambda_BC <- powerTransform(planos$despmed)
lambda_BC

#Inserindo o lambda de Box-Cox na nova base de dados para a estimaÃ§Ã£o de um
#novo modelo
planos_dummies$bcdespmed <- (((planos$despmed ^ lambda_BC$lambda) - 1) / 
                                   lambda_BC$lambda)

#Visualizando a nova variÃ¡vel na base de dados
planos_dummies %>%
  select(id, despmed, bcdespmed, everything()) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 12)

#Estimando um novo modelo múltiplo com dummies
modelo_bc_planosaude <- lm(formula = bcdespmed ~ . -id -despmed -fitted_step
                           -residuos_step, 
                           data = planos_dummies)
#Parâmetros do modelo
summary(modelo_bc_planosaude)

#Aplicando o procedimento Stepwise
step_bc_planosaude <- step(modelo_bc_planosaude, k = 3.841459)

summary(step_bc_planosaude)

#Verificando a normalidade dos resíduos do modelo step_bc_planosaude
#Teste de Shapiro-Francia
sf.test(step_bc_planosaude$residuals) #funÃ§Ã£o sf.test do pacote nortest

#Plotando os novos resíduos do modelo step_bc_planosaude com curva normal teÃ³rica
planos_dummies %>%
  mutate(residuos = step_bc_planosaude$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "white", 
                 fill = "#440154FF", 
                 bins = 15,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(step_bc_planosaude$residuals),
                            sd = sd(step_bc_planosaude$residuals)),
                size = 2, color = "grey30") +
  scale_color_manual(values = "grey50") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme_bw()

#Kernel density estimation (KDE)
planos_dummies %>%
  ggplot() +
  geom_density(aes(x = step_bc_planosaude$residuals), fill = "#440154FF") +
  labs(x = "Resíduos do Modelo Stepwise com Transformação de Box-Cox",
       y = "Densidade") +
  theme_bw()

#Diagnóstico de Heterocedasticidade para o Modelo Stepwise com Box-Cox
ols_test_breusch_pagan(step_bc_planosaude)

#Adicionando fitted values e resíduos do modelo 'step_bc_planosaude'
#no dataset 'planosaude_dummies'
planos_dummies$fitted_step_novo <- step_bc_planosaude$fitted.values
planos_dummies$residuos_step_novo <- step_bc_planosaude$residuals

#GrÃ¡fico que relaciona resíduos e fitted values do modelo 'step_bc_planosaude'
planos_dummies %>%
  ggplot() +
  geom_point(aes(x = fitted_step_novo, y = residuos_step_novo),
             color = "#440154FF", size = 3) +
  labs(x = "Fitted Values do Modelo Stepwise com Transformação de Box-Cox",
       y = "Resíduos do Modelo Stepwise com Transformação de Box-Cox") +
  theme_bw()



#--
# Interpretação dos parâmetros do modelo final escolhido e análise do 
# porcentagem da variabilidade da variável resposta que é explicada 
# pela(s) variável(is) explicativa(s) no seu modelo final.






#--
# Fornecer estimativa para a variância σ² do termo de erro do modelo


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



