#install.packages('tidyr')
#install.packages('rlist')
#install.packages('ggplot2')
#install.packages('patchwork')
#install.packages("ggpubr")

library(ggplot2)
library(readr)
library(stringr)
library(tidyr)
library(dplyr)
library(rlist)
library(patchwork)
library(ggpubr)

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



#Aqui é necessário alguns ajustes por motivo ta tabela obtida diretamente na plataforma ser divergente da obitda via gee.
coverage <- read_csv("datasets/2023-05-19 - MapBiomas - Tabela de Dados - plataforma - regio hidro PNHR - DOCE - PNRH.csv", show_col_types = FALSE)
x <- t(coverage)
x[1,]
coluna_interesse <- c('4.2. Área Urbanizada', '3.3. Silvicultura (monocultura)', '3.1. Pastagem', '5. Corpo D`água')
x <- x[,c(30, 26, 14, 33)]
x <- x[-1,]
x <- as.data.frame(x)
View(x)
#denvar.water = 'Superfície de água'
#denvar.pasto = 'Pastagem'
#denvar.florestaplantada = 'Floresta plantada'
#denvar.florestanatural = 'Vegetação Florestal'
#denvar.urbanizada = 'Infraestrutura urbana'

colnames(x) <- c('Infraestrutura_Urbana', 'Plantacao_Florestal', 'Pasto', 'AGUA')
x$Infraestrutura_Urbana <- as.integer(x$Infraestrutura_Urbana)
x$Plantacao_Florestal <- as.integer(x$Plantacao_Florestal)
x$AGUA <- as.integer(x$AGUA)
x$Pasto <- as.integer(x$Pasto)

x$ANO <- rownames(x)
coverage <- x
rm(x)
### Fim dos ajuste


######### Em caso de uso da tabela do GEE, use este script
#Seleção de variáveis de interesse neste estudo
#var_interesse <- c("ANO", "Forest Formation", "Forest Plantation", "Pasture" , "River, Lake and Ocean", "Urban Infrastructure" )
#coverage <- coverage%>%select("area", 'band', 'class_name')
#coverage <- read_csv("datasets/mapbiomas-brazil-collection-71-doce-area.csv", show_col_types = FALSE)
#colnames(coverage) <- c('AREA_HA', 'ANO', 'CLASSE')
#coverage$ANO <- str_replace(coverage$ANO, 'classification_', '')
#coverage <- coverage %>% filter(CLASSE %in% var_interesse)
#coverage <- spread(coverage, CLASSE, AREA_HA)
#coverage$ANO <- as.integer(coverage$ANO)
#colnames(coverage) <- c("ANO", "Formacao_Florestal", "Plantacao_Florestal", "Pasto" , "Rio_Lago_Mar", "Infraestrutura_Urbana" )
#############script para uso com a plataforma GEE - FIM

water <- read_csv("datasets/mapbiomas-brazil-collection-10-doce-area.csv", show_col_types = FALSE)
water <- water %>% select(band, area)
colnames(water) <- c('ANO', 'WATER_COVERAGE_HA')
water$ANO <- str_replace(water$ANO, 'water_coverage_', '')
water$ANO <- as.integer(water$ANO)

#Tabela de dados da pastagem
#pasture <- pasto_mapbiomas_brazil_collection_10_doce_area <- read_csv("datasets/pasto-mapbiomas-brazil-collection-10-doce-area.csv", show_col_types = FALSE)
#Ajusta tabela de pastagens
#pasture <- as.data.frame(t(pasture))
#pasture$AREA_HA = as.integer(pasture$V1) + as.integer(pasture$V2) + as.integer(pasture$V3)
#pasture <- as.data.frame(pasture[-1,])%>%select(AREA_HA)
#pasture$ANO <- rownames(pasture)
#colnames(pasture) = c('PASTURE_AREA_HA', 'ANO')
#pasture
#pasture
#water <- merge(water, pasture)
dataset <- merge(coverage, water)
#dataset$Pasto <- dataset$PASTURE_AREA_HA
#dataset$Rio_Lago_Mar = dataset$WATER_COVERAGE_HA


#outro ajuste para não usar a tabela de outra plataforma
dataset$Rio_Lago_Mar = dataset$AGUA
dataset <- dataset%>%select(-c(WATER_COVERAGE_HA))
dataset <- dataset%>%select(-c(AGUA))
dataset$ANO <- as.integer(dataset$ANO)
#Remove variável da memória, apartir desta linha consideramos a variável "dataset"
rm(coverage)
rm(water)


attach(dataset)
View(dataset)
set.seed(2023)
colnames((dataset))
plot(dataset)
#Variável para armazenar a lista com os dados dos modelos para posterior avaliação
modelos = list()

#### 1 - Análise: Formacao_Florestal~ANO
#### 1.1 - Gráfico: Formacao_Florestal~ANO
############################# Não utilizada

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
summary.m2
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
  stat_smooth(method = "lm", formula = y~x,
  linetype="dashed", color="darkred", se = FALSE, size = 1) +
  labs(
    #x = 'ANO',
    y = 'hectares',
    title = paste('b) Área de ', denvar.florestaplantada)
  ) +
  #stat_regline_equation()
  annotate("text", x = 2010, y = 72000, label = "y= -10617008.6237666 + 5377.0066924067 * x")
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
  annotate("text", y = 3750000, x = 1995, label = "y= 46484110.57293 - 21082.6086229086*x", parse = FALSE) +
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
  annotate("text", y = 72000, x = 2005, label = 'y = 541286.741098241 - 236.141055341056 * x', parse = FALSE) +
  labs(
    x = 'ANO',
    y = 'hectares',
    title = paste('a) Área de', denvar.water)
  )
chart.m4
summary.m4

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
  annotate("text", y = 20000, x = 2009, label = "y= -2150200.3028743 + 1091.90875160875*x", parse = FALSE) +
  labs(
    x = 'ANO',
    y = 'hectares',
    title = paste('d) Área ', denvar.urbanizada)
  )
chart.m5





################################ #########################
################################ #########################
################################ #########################

################################ Composição de gráficos#########################
################################ #########################

charts1 <- (chart.m4 | chart.m2) / (chart.m3 | chart.m5)
#charts1 + annotate("text", y = 0, x = 0, label = "figura 01")
charts1



################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
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
7.657e+04
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
  annotate("text", size = 10, x = 28000, y = 62000, label = "y = 76571.0821369187 - 0.224414961149197 * x", parse = FALSE) +
  labs(
    y = paste(denvar.water, '(hectares)'),
    x = paste('Área ', denvar.urbanizada, '(hectares)'),
    #caption = "Figura 04 - relação entre a área de superfície de água e infra estrutura urbana", loc='leftbottom'
  )

chart.m6
summary.m6
dt.modelo6
colnames(dataset)
?stat_regline_equation

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
  annotate("text", size=10, x = 150000, y = 60000, label = "y = 74274.9278639654 - 0.0389548430038348 * x", parse = FALSE) +
  labs(
    y = paste(denvar.water, '(hectares)'),
    x = paste('Área de ', denvar.florestaplantada, ' (hectares)'),
    #caption = "Figura 03", loc='leftbottom'
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
summary.m8
dt.modelo8 <- analisamodelo(summary.m8)
modelos <- append(modelos, dt.modelo8)
anova.md8
dt.modelo8
anova.md8$`Pr(>F)`<0.05

chart.m8 <- ggplot(data=dataset, aes(x=Pasto, y=Rio_Lago_Mar)) + 
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
  annotate("text", size=10, x = 3950000, y = 73000, label = "y = 43438.2241893521 + 0.00585442099157521 * x", parse = FALSE) +
  labs(
    y = paste(denvar.water, ' (hestares)'),
    x = paste('Área de ', denvar.pasto, ' (hectares)'),
    #caption = "Figura 02", loc='leftbottom'
  )
chart.m8

summary.m8
dt.modelo8

8.144e+01

citation()

