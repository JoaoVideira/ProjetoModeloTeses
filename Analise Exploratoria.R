##Analise Exploratoria e Descritiva das variaveis da base:
#Sumario; Histograma; Box Plot; Grafico de Dispersao; matriz de correlacao de variaveis

# Sumario
summary(PainelFinal)
#Variavel IFDM contem o valor 0 o que nao faz sentido, por isso retiro
PainelFinal<-filter(PainelFinal, IFDM!=0)

# Histogramas
attach(PainelFinal)
hist(PainelFinal$IFDM,breaks = 50,xlim = c(0.3319,0.8776),xlab = "IFDM",ylab = "Frequência",main = "")

hist(prodha,breaks = 50,xlim = c(0.3733,18.4007),xlab = "Prod. Agrícola por hectare",ylab = "Frequência",main = "")

hist(despercapita,breaks = 50,xlim = c(0,19713),xlab = "Despesa per capita",ylab = "Frequência",main = "")

hist(ICMSpc,breaks = 50,xlim = c(102.0,4441.4),xlab = "ICMS",ylab ="Frequência",main = "")

hist(ICMSEpc,breaks = 50,xlim = c(0,2522.120),xlab = "ICMSE per capita",ylab = "Frequência",main = "")

hist(den.pop,breaks = 50,xlim = c(0,2.8),xlab = "Den. Populacional",ylab = "Frequência",main = "")

hist(prop.plant,breaks = 50,xlim = c(0,1.3),xlab = "Den. Populacional",ylab = "Frequência",main = "")

hist(Reb.hec,breaks = 50,xlim = c(0.01538,22.44179),xlab = "Rebanho por hectare",ylab = "Frequência",main = "")

hist(alt.median,breaks = 50,xlim = c(132.4,874.2),xlab = "Mediana Altitude",ylab = "Frequência", main = "")

hist(area,breaks = 50,xlim = c(34299,2795694),xlab = "Area ",ylab = "Frequência", main = "")

PainelFinal$Valor.extrat<-replace_na(PainelFinal$Valor.extrat,0)
hist(PainelFinal$Valor.extrat,breaks = 50,xlim = c(4,11000),xlab = "Valor por hec Extrativismo",ylab = "Frequência", main = "")

# Grafico de Dispersao
ggplot(PainelFinal, aes(y =PainelFinal$IFDM, x = ICMSEpc,colour = as.factor(ano))) + 
  geom_point() +
  geom_smooth()+
  labs(x = "ICMSE per capita", y = "IFDM")+
  theme(legend.title = element_blank())

#Box-Plot
# IFDM
PainelFinal%>%
  mutate(ano=as.factor(ano))%>%
  ggplot() +
  geom_boxplot(aes(x = ano, y = PainelFinal$IFDM))
#ICMSE per capita
PainelFinal%>%
  mutate(ano=as.factor(ano))%>%
  ggplot() +
  geom_boxplot(aes(x = ano, y = ICMSEpc))
