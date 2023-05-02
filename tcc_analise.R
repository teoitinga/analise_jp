install.packages('dplyr')
install.packages('stringr')

library(dplyr)
library(readr)
library(stringr)
 
ds <- read_delim("datasets/ds_tcc_qmax_qmin_r.csv", 
                                 delim = ";", escape_double = FALSE, col_types = cols(ANO = col_integer()), 
                                 trim_ws = TRUE)

ds_riodoce <- read_delim("datasets/mapbiomas-brazil-collection-71-doce-area.csv")
ds_riodoce <- ds_riodoce %>% select(c(-'system:index', -'.geo', -'class'))
colnames(ds_riodoce) = c('AREA_HA', 'ANO', 'CLASSE' )
ds_riodoce['ANO'] <- str_replace(ds_riodoce$ANO, "classification_", "")
group_by(ds_riodoce, 'ANO')
group_by(ds_riodoce,'ANO')%>%summarise(Total=sum(AREA_HA))
attach(ds)
#Amplitude da vazão em função da Formação Florestal
View(ds_riodoce)
m1<- glm(AMPLITUDE~FORMACAO_FLORESTAL_KM2)

ds_riodoce <- mapbiomas_result_rio_doce
# Modelo nulo para confrontar com o modelo 1 e o valor 1 indica que é em função de qualuqer valor
mnulo <- glm(AMPLITUDE~1)

#anova: função para criar tabela de variâncias F: Teste de Fisher
anova(m1, mnulo, test = 'F')

anova(m1, test = 'F')

summary(m1)

#Saber se a distribuição de dados está adequada ao modelo. Usa o valor estimada para a dispersão "Dispersion parameter for gaussian family taken to be 4429.956" divide a Residual Deviance(Resid. Dev) pelo grau de liberdade.

310097/70

#Como o modelo não foi significativo, vamos tentar um novo modelo incluindo o tempo como variável explicativa.

m2 <- glm(AMPLITUDE~FORMACAO_FLORESTAL_KM2*ANO)

anova(m2, mnulo, test='F')

summary(m2)

# Como a interação não foi significativa vamos simplificar o modelo.
m3 <- glm(AMPLITUDE~FORMACAO_FLORESTAL_KM2 + ANO)

anova(m3, mnulo, test='F')

summary(m3)

#Testando a variação da formação florestal em função do tempo e da vazão .
m4 <- glm(FORMACAO_FLORESTAL_KM2~Q_num * VAZAO)

mnulo2 <- glm(FORMACAO_FLORESTAL_KM2~1)

anova(m4, mnulo2, test='F')


m5 <- glm(FORMACAO_FLORESTAL_KM2~ANO)
anova(m5, mnulo2, test = 'F')

m6 <- glm(FORMACAO_FLORESTAL_KM2~VAZAO)
anova(m6, mnulo2, test = 'F')

m7 <- glm(FORMACAO_FLORESTAL_KM2~AMPLITUDE)
anova(m7, mnulo2, test = 'F')

m8 <- glm(FORMACAO_FLORESTAL_KM2~LAMINA_AGUA_KM2)
anova(m8, mnulo2, test = 'F')

m9 <- glm(LAMINA_AGUA_KM2~ANO)
mnulo3 <-  glm(LAMINA_AGUA_KM2~1)
anova(m9, mnulo3, test = 'F')

plot(LAMINA_AGUA_KM2~ANO)
plot(ds)

# investigar áreas de apps ao invés de formação florestal(matas ciliares, app de enconsta)
# e trevho de áreas urbanoas  /solo exposto.


names(ds_riodoce)
attach(ds_riodoce)
colnames(ds_riodoce)

par(mfrow=c(2,2))
plot(AREA_HA_Urban_Infrastructure~LAMINA_HIDRICA_HA)
plot(AREA_HA_Forest_Formation~LAMINA_HIDRICA_HA)
plot(AREA_HA_Forest_Plantation~LAMINA_HIDRICA_HA)
plot(AREA_HA_Forest_Plantation~AREA_HA_Urban_Infrastructure)

plot(LAMINA_HIDRICA_HA~AREA_HA_Pasture)

plot(AREA_HA_Pasture~ANO)
par(mfrow=c(2,2))
plot(AREA_HA_Pasture~ANO)
plot(LAMINA_HIDRICA_HA~ANO)
plot(AREA_HA_Forest_Formation~ANO)
plot(AREA_HA_Forest_Plantation~ANO)
plot(LAMINA_HIDRICA_HA~AREA_HA_Urban_Infrastructure)
shapiro.test(LAMINA_HIDRICA_HA)

#Avaliando o modelo Lâmina hídrica em função da área de pastagem
mrd1 <- glm(LAMINA_HIDRICA_HA~AREA_HA_Pasture)
mrdnulo1 <- glm(LAMINA_HIDRICA_HA~1)
anova(mrd1, mrdnulo1, test = 'F')
summary(mrd1)
#Dispersion parameter for gaussian family taken to be 1806.545)
57809/32
par(mfrow=c(1,1))
#gr?fico
plot(LAMINA_HIDRICA_HA~AREA_HA_Pasture, ylab="lãmina de água (ha)", xlab="Área de pastagem (ha)", ylim=c(500,950), xlim=c(30000,50000), pch=16,las=1,bty="l")
#reta da regress?o

#curve (intercepto+estimate*x)
curve(8.892e+01 + 1.317e-02*x, add=T, lty=16)
#ou esta função que desenha a linha
abline(mrd1, lty=2)

legend(30000, 900, legend="y=(8.892e+01 + 1.317e-02*x)",bty="n")


#Avaliando o modelo Lâmina hídrica em função da formação florestal

mrd2 <- glm(LAMINA_HIDRICA_HA~AREA_HA_Forest_Formation)
mrdnulo2 <- mrdnulo1
anova(mrd2, mrdnulo2, test = 'F')

summary(mrd2)
105046/32

mrd3 <- glm(LAMINA_HIDRICA_HA~AREA_HA_Urban_Infrastructure)
mrdnulo3 <- mrdnulo1
anova(mrd3, mrdnulo3, test='F')
summary(mrd3)
18392/32

par(mfrow=c(1,1))
#gr?fico
plot(LAMINA_HIDRICA_HA~AREA_HA_Urban_Infrastructure, ylab="lãmina de água (ha)", xlab="Área de infraestrutura urbana(ha)", ylim=c(500,950), xlim=c(150,550), pch=16,las=1,bty="l")
#reta da regress?o

#curve (intercepto+estimate*x)
curve(804.13239 + (-0.42988)*x, add=T, lty=16)
#ou esta função que desenha a linha
abline(mrd3, lty=2)

legend(300, 900, legend="y=(804.13239 + (-0.42988)*x)",bty="n")

colnames(ds_riodoce)

mrd4 <- glm(LAMINA_HIDRICA_HA ~ AREA_HA_Forest_Plantation)
mrdnulo4 <- mrdnulo1
anova(mrd4, mrdnulo4, test='F')
summary(mrd4)
29807/32

par(mfrow=c(1,1))
#gr?fico
plot(LAMINA_HIDRICA_HA~AREA_HA_Forest_Plantation, ylab="lãmina de água (ha)", xlab="Área de floresta plantada (ha)", ylim=c(500,950), xlim=c(500,2800), pch=16,las=1,bty="l")
#reta da regress?o

#curve (intercepto+estimate*x)
curve(767.181306 + (-0.076104)*x, add=T, lty=16)
#ou esta função que desenha a linha
abline(mrd3, lty=2)

legend(1000, 900, legend="y=(767.181306 + (-0.076104)*x)",bty="n")

mrd5 <- glm(LAMINA_HIDRICA_HA~ANO)
mrdnulo5 <- mrdnulo1

anova(mrd5, mrdnulo5, test='F')
summary(mrd5)
20844/32
par(mfrow=c(1,1))
#gr?fico
plot(LAMINA_HIDRICA_HA~ANO, ylab="lãmina de água (ha)", xlab="Ano", ylim=c(500,950), pch=16,las=1,bty="l")
#reta da regress?o

#curve (intercepto+estimate*x)
curve(10036.9081 + (-4.6884)*x, add=T, lty=16)
#ou esta função que desenha a linha
abline(mrd5, lty=2)
legend(1990, 900, legend="y=(10036.9081 + (-4.6884)*x)",bty="n")

colnames(ds_riodoce)



mrd6 <- glm(AREA_HA_Pasture~ANO)
mrdnulo6 <- glm(AREA_HA_Pasture~1)

anova(mrd6, mrdnulo6, test='F')
summary(mrd6)
102148757/32
par(mfrow=c(1,1))

par(mai= c(1.2,1.4,0.2,0.5))
par(mgp=c(4.5, 2, 0))
par(bty="l")
#gr?fico
plot(AREA_HA_Pasture~ANO, ylab="Área de pastagem (ha)", xlab="Ano",  ylim=c(30000,50000), pch=16,las=1,bty="l")

#reta da regress?o

#curve (intercepto+estimate*x)
curve(464807.31 + (-210.92)*x, add=T, lty=16)
#ou esta função que desenha a linha
abline(mrd6, lty=2)
legend(x='bottomleft', legend="y=(464807.31 + (-210.92)*x)",bty="n", )

mrd6 <- glm(AREA_HA_Pasture~ANO)
mrdnulo6 <- glm(AREA_HA_Pasture~1)

anova(mrd6, mrdnulo6, test='F')
summary(mrd6)
102148757/32
par(mfrow=c(1,1))

par(mai= c(1.2,1.4,0.2,0.5))
par(mgp=c(4.5, 2, 0))
par(bty="l")
#gr?fico
plot(AREA_HA_Pasture~ANO, ylab="Área de pastagem (ha)", xlab="Ano",  ylim=c(30000,50000), pch=16,las=1,bty="l")

#reta da regress?o

#curve (intercepto+estimate*x)
curve(464807.31 + (-210.92)*x, add=T, lty=16)
#ou esta função que desenha a linha
abline(mrd6, lty=2)
legend(x='bottomleft', legend="y=(464807.31 + (-210.92)*x)",bty="n", )

mrd7 <- glm(AREA_HA_Forest_Plantation~ANO)
mrdnulo7 <- glm(AREA_HA_Forest_Plantation~1)

anova(mrd7, mrdnulo7, test='F')
summary(mrd7)
492960/32


par(mai= c(1.2,1.4,0.2,0.5))
par(mgp=c(4.5, 2, 0))
par(bty="l")
#gr?fico
plot(AREA_HA_Forest_Plantation~ANO, ylab="Área de floresta plantada (ha)", xlab="Ano",  ylim=c(500,2800), pch=16,las=1,bty="l")

#reta da regress?o

#curve (intercepto+estimate*x)
curve(-1.128e+05 + 5.712e+01*x, add=T, lty=16)
#ou esta função que desenha a linha
abline(mrd6, lty=2)
legend(x='topleft', legend="y=(-1.128e+05 + 5.712e+01*x)",bty="n", )

mrd6 <- glm(AREA_HA_Pasture~ANO)
mrdnulo6 <- glm(AREA_HA_Pasture~1)

anova(mrd6, mrdnulo6, test='F')
summary(mrd6)
102148757/32
par(mfrow=c(1,1))

par(mai= c(1.2,1.4,0.2,0.5))
par(mgp=c(4.5, 2, 0))
par(bty="l")
#gr?fico
plot(AREA_HA_Pasture~ANO, ylab="Área de pastagem (ha)", xlab="Ano",  ylim=c(30000,50000), pch=16,las=1,bty="l")

