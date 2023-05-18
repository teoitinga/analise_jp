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

denvar.water = 'Superfície de água'
denvar.pasto = 'Pastagem'
denvar.florestaplantada = 'Floresta plantada'
denvar.florestanatural = 'Vegetação Florestal'
denvar.urbanizada = 'Infraestrutura urbana'

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

#Ajusta tabela de pastagens
pasture <- pasto_mapbiomas_brazil_collection_10_doce_area <- read_csv("datasets/pasto-mapbiomas-brazil-collection-10-doce-area.csv", show_col_types = FALSE)

pasture <- as.data.frame(t(pasture))
pasture$AREA_HA = as.integer(pasture$V1) + as.integer(pasture$V2) + as.integer(pasture$V3)
pasture <- as.data.frame(pasture[-1,])%>%select(AREA_HA)
pasture$ANO <- rownames(pasture)
colnames(pasture) = c('PASTURE_AREA_HA', 'ANO')
pasture
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

water <- read_csv("datasets/mapbiomas-brazil-collection-10-doce-area.csv", show_col_types = FALSE)
water <- water %>% select(band, area)
colnames(water) <- c('ANO', 'WATER_COVERAGE_HA')
water$ANO <- str_replace(water$ANO, 'water_coverage_', '')
water$ANO <- as.integer(water$ANO)
water
pasture
#water <- merge(water, pasture)
dataset <- merge(coverage, water)
#dataset$Pasto <- dataset$PASTURE_AREA_HA
dataset$Rio_Lago_Mar = dataset$WATER_COVERAGE_HA
#Remove variável da memória, apartir desta linha consideramos a variável "dataset"
rm(coverage)
rm(water)
dataset <- dataset%>%select(-c(WATER_COVERAGE_HA))
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
chart.m1

################################################################################
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
    panel.background = element_blank(),
    axis.title.y = element_blank(),
  ) +
  stat_smooth(method = "lm",
  linetype="dashed", color="darkred", se = FALSE, size = 1) +
  labs(
    #x = 'ANO',
    y = 'hectares',
    title = paste('b) Área de ', denvar.florestaplantada)
  ) +
  annotate("text", x = 2005, y = 700, label = "y = -105328.672790535 + 53.3482402827061*x", parse = FALSE)
chart.m2



################################################################################
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
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank()
  ) +
  stat_smooth(method = "lm",
              linetype="dashed", color="darkred", se = FALSE, size = 1) +
  annotate("text", y = 37300, x = 2002, label = "y= 105372268.203467 - 50368.9337662357 * x", parse = FALSE) +
  labs(
    #x = 'ANO',
    y = 'hectares',
    title = paste('c) Área de ', denvar.pasto)
  )

chart.m3
summary.m3


################################################################################
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
    axis.title.x = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank()
  ) +
  stat_smooth(method = "lm",
              linetype="dashed", color="darkred", se = FALSE, size = 1) +
  annotate("text", y = 565, x = 2000, label = "y= 5290.51701434742 + -2.29950466660375 * x", parse = FALSE) +
  labs(
    x = 'ANO',
    y = 'hectares',
    title = paste('a) Área de ', denvar.water)
  )
chart.m4


################################################################################
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
    panel.background = element_blank(),
    axis.title.y = element_blank(),
  ) +
  stat_smooth(method = "lm",
            linetype="dashed", color="darkred", se = FALSE, size = 1) +
  annotate("text", y = 200, x = 2009, label = "y= -21411.6114725616 + 10.8738498492724 * x", parse = FALSE) +
  labs(
    x = 'ANO',
    y = 'hectares',
    title = paste('d) Área ', denvar.urbanizada)
  )
chart.m5

charts1 <- (chart.m4 | chart.m2) / (chart.m3 | chart.m5)
charts1 + annotate("text", y = 0, x = 0, label = "figura 01")
charts1
colnames(dataset)
530/387
1080/1.369509
1080*2
2*788


################################################################################
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

chart.m6 <- ggplot(data=dataset, aes(x=Infraestrutura_Urbana, y=Rio_Lago_Mar)) + 
  geom_point(color='darkblue', size=5) +
  theme(
    axis.line = element_line(),
    panel.grid.major = element_blank(),
    axis.title = element_text(size=18),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    plot.caption = element_text(hjust = 0, size=22, face = "italic"),
    axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=1, size=14),
    axis.text.x = element_text(size=14)
  ) +
  stat_smooth(method = "lm",
              linetype="dashed", color="darkred", se = FALSE, size = 1) +
  annotate("text", size = 10, x = 400, y = 725, label = "y = 765.509767577184 - 0.219415393368874 * x", parse = FALSE) +
  labs(
    y = paste(denvar.water, '(ha)'),
    x = paste('Área ', denvar.urbanizada),
    caption = "Figura 04 - relação entre a área de superfície de água e infra estrutura urbana", loc='leftbottom'
  )
chart.m6
summary.m6
dt.modelo6
colnames(dataset)


################################################################################
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

chart.m7 <- ggplot(data=dataset, aes(x=Plantacao_Florestal, y=Rio_Lago_Mar)) + 
  geom_point(color='darkblue', size=5) +
  theme(
    axis.line = element_line(),
    axis.title = element_text(size=18),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    plot.caption = element_text(hjust = 0, size=22, face = "italic"),
    axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=1, size=14),
    axis.text.x = element_text(size=14)
  ) +
  stat_smooth(method = "lm",
              linetype="dashed", color="darkred", se = FALSE, size = 1) +
  annotate("text", size=10, x = 1800, y = 730, label = "y = 771.49880872459 - 0.0811982098441572 * x", parse = FALSE) +
  labs(
    y = paste(denvar.water, '(ha)'),
    x = paste('Área de ', denvar.florestaplantada, ' (ha)'),
    caption = "Figura 03", loc='leftbottom'
  )
chart.m7
summary.m7
dt.modelo7

788*1.4

################################################################################
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

chart.m8 <- ggplot(data=dataset, aes(x=Pasto, y=Rio_Lago_Mar)) + 
  geom_point(color='darkblue', size=5) +
  theme(
    axis.line = element_line(),
    panel.grid.major = element_blank(size=18),
    axis.title = element_text(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    plot.caption = element_text(hjust = 0, size=22, face = "italic"),
    axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=1, size=14),
    axis.text.x = element_text(size=14)
  ) +

  stat_smooth(method = "lm",
              linetype="dashed", color="darkred", se = FALSE, size = 1) +
  annotate("text", size=10, x = 39700, y = 700, label = "y = 81.4364024310247 + 0.0133243204093905 * x", parse = FALSE) +
  labs(
    y = paste(denvar.water, ' (ha)'),
    x = paste(denvar.pasto, ' (ha)'),
    caption = "Figura 02", loc='leftbottom'
  )
chart.m8

summary.m8
dt.modelo8

8.144e+01



