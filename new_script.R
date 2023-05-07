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

denvar.water = 'corpos hídricos'
denvar.pasto = 'Pastagem'
denvar.florestaplantada = 'Floresta plantada'
denvar.florestanatural = 'Vegetação Florestal'
denvar.urbanizada = 'urbanizada'

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
  theme(
    axis.line = element_line(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank()
  ) +
  labs(
    #x = 'ANO',
    y = 'hectares',
    title = paste('Área de ', denvar.florestanatural)
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
dt.modelo2
#### 2.1 - Gráfico: Plantacao_Florestal~ANO
chart.m2 <- ggplot(data=dataset, aes(y=Plantacao_Florestal, x=ANO)) + 
  geom_point(color='darkblue', size=2) + 
  theme(
    axis.line = element_line(),
    axis.title.x = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank()
  ) +
  stat_smooth(method = "lm",
  linetype="dashed", color="darkred", se = FALSE, size = 1) +
  labs(
    #x = 'ANO',
    y = 'hectares',
    title = paste('Área de ', denvar.florestaplantada)
  ) +
  annotate("text", x = 2005, y = 700, label = "y = -105328.672790535 + 53.3482402827061*x", parse = FALSE)
chart.m2

#### 3 - Análise: Pasto~ANO
m3 <- glm(Pasto~ANO)
mnulo3 <- glm(Pasto~1)

anova.md3 <- anova(m3, mnulo3, test='F')
summary.m3 <- summary(m3)

dt.modelo3 <- analisamodelo(summary.m3)
modelos <- append(modelos, dt.modelo3)
anova.md3
dt.modelo3
1.531e-09<0.05
#### 3.2 - Gráfico: Pasto~ANO
#Pasto~ANO
chart.m3 <- ggplot(data=dataset, aes(y=Pasto, x=ANO)) + 
  geom_point(color='darkblue', size=2) +
  theme(
    axis.line = element_line(),
    axis.title.x = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank()
  ) +
  stat_smooth(method = "lm",
              linetype="dashed", color="darkred", se = FALSE, size = 1) +
  annotate("text", y = 37500, x = 2000, label = "y = 484351.167763559 - 220.613071329796 * x", parse = FALSE) +
  labs(
    #x = 'ANO',
    y = 'hectares',
    title = paste('Área de ', denvar.pasto)
  )

chart.m3
summary.m3


#### 4 - Análise: Rio_Lago_Mar~ANO
m4 <- glm(Rio_Lago_Mar~ANO)
mnulo4 <- glm(Rio_Lago_Mar~1)

anova.md4 <- anova(m4, mnulo4, test='F')
summary.m4 <- summary(m4)

dt.modelo4 <- analisamodelo(summary.m4)
modelos <- append(modelos, dt.modelo4)
anova.md4
dt.modelo4
8.235e-08<0.05
chart.m4 <- ggplot(data=dataset, aes(y=Rio_Lago_Mar, x=ANO)) + 
  geom_point(color='darkblue', size=2) +
  theme(
    axis.line = element_line(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank()
  ) +
  stat_smooth(method = "lm",
              linetype="dashed", color="darkred", se = FALSE, size = 1) +
  annotate("text", y = 720, x = 2007, label = "y= 5290.51701434742 + -2.29950466660375 * x", parse = FALSE) +
  labs(
    x = 'ANO',
    y = 'hectares',
    title = paste('Área de ', denvar.water)
  )
chart.m4

#### 5 - Análise: Infraestrutura_Urbana~ANO
m5 <- glm(Infraestrutura_Urbana~ANO)
mnulo5 <- glm(Infraestrutura_Urbana~1)

anova.md5 <- anova(m5, mnulo5, test='F')
summary.m5 <- summary(m5)

dt.modelo5 <- analisamodelo(summary.m5)
modelos <- append(modelos, dt.modelo5)
anova.md5
dt.modelo5
summary.m5
2.2e-16<0.05
chart.m5 <- ggplot(data=dataset, aes(y=Infraestrutura_Urbana, x=ANO)) + 
  geom_point(color='darkblue', size=2) +
  theme(
    axis.line = element_line(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank()
  ) +
  stat_smooth(method = "lm",
            linetype="dashed", color="darkred", se = FALSE, size = 1) +
  annotate("text", y = 500, x = 1998, label = "y= -21411.6114725616 + 10.8738498492724 * x", parse = FALSE) +
  labs(
    x = 'ANO',
    y = 'hectares',
    title = paste('Área ', denvar.urbanizada)
  )
chart.m5

charts1 <- (chart.m2 | chart.m3) / (chart.m4 | chart.m5)
charts1 + annotate("text", y = 0, x = 0, label = "figura 01")

colnames(dataset)


#### 6 - Análise: Rio_Lago_Mar~Infraestrutura_Urbana
m6 <- glm(Rio_Lago_Mar~Infraestrutura_Urbana)
mnulo6 <- glm(Rio_Lago_Mar~1)

anova.md6 <- anova(m6, mnulo6, test='F')
summary.m6 <- summary(m6)

dt.modelo6 <- analisamodelo(summary.m6)
modelos <- append(modelos, dt.modelo6)
anova.md6
dt.modelo6
summary.m6
anova.md6$`Pr(>F)`<0.05

chart.m6 <- ggplot(data=dataset, aes(y=Infraestrutura_Urbana, x=Rio_Lago_Mar)) + 
  geom_point(color='darkblue', size=2) +
  theme(
    axis.line = element_line(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank()
  ) +
  stat_smooth(method = "lm",
              linetype="dashed", color="darkred", se = FALSE, size = 1) +
  annotate("text", y = 175, x = 670, label = "y = 765.509767577184 - 0.219415393368874 * x", parse = FALSE) +
  labs(
    x = paste(denvar.water, '(ha)'),
    y = paste('Área ', denvar.urbanizada)
  )
chart.m6
summary.m6
dt.modelo6
colnames(dataset)

#### 7 - Análise: Rio_Lago_Mar~Plantacao_Florestal
m7 <- glm(Rio_Lago_Mar~Plantacao_Florestal)
mnulo7 <- glm(Rio_Lago_Mar~1)

anova.md7 <- anova(m7, mnulo7, test='F')
summary.m7 <- summary(m7)

dt.modelo7 <- analisamodelo(summary.m7)
modelos <- append(modelos, dt.modelo7)
anova.md7
dt.modelo7
anova.md7$`Pr(>F)`<0.05

chart.m7 <- ggplot(data=dataset, aes(y=Plantacao_Florestal, x=Rio_Lago_Mar)) + 
  geom_point(color='darkblue', size=2) +
  theme(
    axis.line = element_line(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank()
  ) +
  stat_smooth(method = "lm",
              linetype="dashed", color="darkred", se = FALSE, size = 1) +
  annotate("text", y = 175, x = 670, label = "y= 743.509074544643 - 0.0385507812889891 * x", parse = FALSE) +
  labs(
    x = paste(denvar.water, '(ha)'),
    y = paste('Área de ', denvar.florestaplantada, ' (ha)'),
  )
chart.m7
summary.m7
dt.modelo7

#### 8 - Análise: Rio_Lago_Mar~Pasto
m8 <- glm(Rio_Lago_Mar~Pasto)
mnulo8 <- glm(Rio_Lago_Mar~1)

anova.md8 <- anova(m8, mnulo8, test='F')
summary.m8 <- summary(m8)

dt.modelo8 <- analisamodelo(summary.m8)
modelos <- append(modelos, dt.modelo8)
anova.md8
dt.modelo8
anova.md8$`Pr(>F)`<0.05

chart.m8 <- ggplot(data=dataset, aes(y=Pasto, x=Rio_Lago_Mar)) + 
  geom_point(color='darkblue', size=2) +
  theme(
    axis.line = element_line(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank()
  ) +
  stat_smooth(method = "lm",
              linetype="dashed", color="darkred", se = FALSE, size = 1) +
  annotate("text", y = 38000, x = 690, label = "y = 442.975810069579 + 0.00569041987797625 * x", parse = FALSE) +
  labs(
    x = paste(denvar.water, ' (ha)'),
    y = paste(denvar.pasto, ' (ha)'),
  )
chart.m8
summary.m8
dt.modelo8
4.43e+02

#### 9 - Análise: Rio_Lago_Mar~Infraestrutura_Urbana
m9 <- glm(Rio_Lago_Mar~Infraestrutura_Urbana)
mnulo9 <- glm(Rio_Lago_Mar~1)

anova.md9 <- anova(m9, mnulo9, test='F')
summary.m9 <- summary(m9)

dt.modelo9 <- analisamodelo(summary.m9)
modelos <- append(modelos, dt.modelo9)
anova.md9
dt.modelo9
anova.md9$`Pr(>F)`<0.05

chart.m9 <- ggplot(data=dataset, aes(y=Infraestrutura_Urbana, x=Rio_Lago_Mar)) + 
  geom_point(color='darkblue', size=2) +
  theme(
    axis.line = element_line(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank()
  ) +
  stat_smooth(method = "lm",
              linetype="dashed", color="darkred", se = FALSE, size = 1) +
  annotate("text", y = 100, x = 670, label = "y = 765.509767577184 - 0.219415393368874 * x", parse = FALSE) +
  labs(
    x = paste(denvar.water, ' (ha)'),
    y = 'Área de Infraestrutura Urbana (ha)',
  )
chart.m9
summary.m9
dt.modelo9
4.43e+02

#### 10 - Análise: Rio_Lago_Mar~Formacao_Florestal




