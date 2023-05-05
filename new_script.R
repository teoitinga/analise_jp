#install.packages('tidyr')
#install.packages('rlist')
#install.packages('ggplot2')
#install.packages('patchwork')

library(ggplot2)
library(readr)
library(stringr)
library(tidyr)
library(dplyr)
library(rlist)
library(patchwork)

# Função para obter as variáveis estatísticas para análise e interpretação dos dados.
analisamodelo <- function (summary){
  #Dispersion parameter for gaussian family taken to be
  dpgf <- summary$dispersion
  
  #Residual deviance
  rd <- summary$deviance
  
  #Degrees of dispersion - DF / Grau de liberdade()
  dfl <- summary$df.residual
  
  intercept <- summary[["coefficients"]][1]
  variavel <- summary[["coefficients"]][2][1]
  dependente <- summary[["terms"]][[2]]
  independente <- summary[["terms"]][[3]]
  relacao <-  rd/dfl
  index <- dpgf/relacao #se for próximo de 1 a relação é significatica
  equacao <- paste('y=', intercept, '+', variavel, '* x')
  return (c(Dispersion_parameter=dpgf, Residual_deviance=rd,     degrees_of_freedom=dfl, est_intercept = intercept, est_dependente=variavel, var_dependente=dependente, var_independente=independente, relacao=relacao, index=index, equacao=equacao))
}

coverage <- read_csv("datasets/mapbiomas-brazil-collection-71-doce-area.csv", show_col_types = FALSE)

#Seleção de variáveis de interesse neste estudo
var_interesse <- c("ANO", "Forest Formation", "Forest Plantation", "Pasture" , "River, Lake and Ocean", "Urban Infrastructure" )
coverage <- coverage%>%select("area", 'band', 'class_name')
coverage
colnames(coverage) <- c('AREA_HA', 'ANO', 'CLASSE')
coverage$ANO <- str_replace(coverage$ANO, 'classification_', '')
coverage <- coverage %>% filter(CLASSE %in% var_interesse)
coverage <- spread(coverage, CLASSE, AREA_HA)
coverage$ANO <- as.integer(coverage$ANO)

colnames(coverage) <- c("ANO", "Formacao_Florestal", "Plantacao_Florestal", "Pasto" , "Rio_Lago_Mar", "Infraestrutura_Urbana" )

dataset <- coverage

#Remove variável da memória, apartir desta linha consideramos a variável "dataset"
rm(coverage)

attach(dataset)
set.seed(2023)
colnames((dataset))
plot(dataset)
#Variável para armazenar a lista com os dados dos modelos para posterior avaliação
modelos = list()

#### 1 - Análise: Formacao_Florestal~ANO
m1 <- glm(Formacao_Florestal~ANO)
mnulo1 <- glm(Formacao_Florestal~1)

anova.md1 <- anova(m1, mnulo1, test='F')
summary.m1 <- summary(m1)

dt.modelo1 <- analisamodelo(summary.m1)
modelos <- append(modelos, dt.modelo1)

#### 1.1 - Gráfico: Formacao_Florestal~ANO
#plot(Formacao_Florestal~ANO, ylab="Formação florestal plantada (ha)", xlab=dt.modelo1$var_independente, pch=16,las=1,bty="l")
chart.m1 <- ggplot(data=dataset, aes(y=Formacao_Florestal, x=ANO)) + 
  geom_point(color='darkblue', size=2) +
  #geom_line(color='darkblue', size=0.5) +
  theme_minimal() + 
  labs(
    x = 'ANO',
    y = 'hectares',
    title = 'Área de Formação florestal'
  ) #+ annotate("text", x = 1990, y = 0, label = equacao) 
  #facet_grid(Formacao_Florestal + Plantacao_Florestal~ANO)

#### 2 - Análise: Plantacao_Florestal~ANO
m2 <- glm(Plantacao_Florestal~ANO)
mnulo2 <- glm(Plantacao_Florestal~1)

anova.md2 <- anova(m2, mnulo2, test='F')
summary.m2 <- summary(m2)

dt.modelo2 <- analisamodelo(summary.m2)
modelos <- append(modelos, dt.modelo2)
anova.md2
#### 2.1 - Gráfico: Plantacao_Florestal~ANO
chart.m2 <- ggplot(data=dataset, aes(y=Plantacao_Florestal, x=ANO)) + 
  geom_point(color='darkblue', size=2) +
  stat_smooth(method = "lm",
  col = "#C42126", linetype="dashed", color="darkred", size=1,
  se = TRUE,
  size = 0.5) + 
  theme_minimal() + 
  labs(
    x = 'ANO',
    y = 'hectares',
    title = 'Área de Plantação florestal'
  )

#Pasto~ANO
chart.m3 <- ggplot(data=dataset, aes(y=Pasto, x=ANO)) + 
  geom_point(color='darkblue', size=2) +
  theme_minimal() + 
  labs(
    x = 'ANO',
    y = 'hectares',
    title = 'Área de Pasto'
  )

#Rio_Lago_Mar~ANO
chart.m4 <- ggplot(data=dataset, aes(y=Rio_Lago_Mar, x=ANO)) + 
  geom_point(color='darkblue', size=2) +
  theme_minimal() + 
  labs(
    x = 'ANO',
    y = 'hectares',
    title = 'Área Rio e Lago'
  )


#Infraestrutura_Urbana~ANO
chart.m5 <- ggplot(data=dataset, aes(y=Infraestrutura_Urbana, x=ANO)) + 
  geom_point(color='darkblue', size=2) +
  theme_minimal() + 
  labs(
    x = 'ANO',
    y = 'hectares',
    title = 'Área Infraestrutura Urbana'
  )


charts1 <- (chart.m1 | chart.m2) / (chart.m3 | chart.m4) / (chart.m5 )
charts1

colnames(dataset)
#Como o p-value não é significativo(>0.05) então não se registra no gráfico a reta de regressão nem a sua equação.
#abline(m1, lty=2)
#equacao <- dt.modelo1$equacao
#legend(x='topleft', legend=equacao,bty="n", )
list(dt.modelo1)

anova.md1
summary.m1
