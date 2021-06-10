##Analise Exploratoria e Descritiva das variaveis da base:
#Sumario; Histograma; Box Plot; Grafico de Dispersao; matriz de correlacao de variaveis

# Sumario
summary(MTPainel)
attach(MTPainel)

# Histogramas
hist(saude,breaks = 50,xlim = c(0.2890,0.9880),xlab = "Saude IFDM",ylab = "Frequência",main = "")#assimetrica a esquerda

hist(educacao,breaks = 50,xlim = c(0.2608,0.9505),xlab = "Educação IFDM",ylab = "Frequência",main = "")#assimetrica a esquerda

hist(renda_emp,breaks = 50,xlim = c(0.2165,0.9317),xlab = "Renda/Emprego IFDM",ylab = "Frequência",main = "")#simetrica

hist(ICMSpc,breaks = 50,xlim = c(102.0,4685.8),xlab = "ICMS per capita",ylab ="Frequência",main = "")#assimet.direita

hist(ICMSEpc,breaks = 50,xlim = c(0,2522.120),xlab = "ICMSE per capita",ylab = "Frequência",main = "")#assimet direita

hist(despercapita,breaks = 50,xlim = c(107.7,51508.2),xlab = "Despesa per capita",ylab = "Frequência",main = "")#assimet direita

hist(prop.plant,breaks = 50,xlim = c(0.00225,221.51515),xlab = "prod. agrícola per capita",ylab = "Frequência",main = "")#assimet direita

hist(MTPainel$rebanho,breaks = 50,xlim = c(7174,1113134),xlab = "Quantidade Rebanho",ylab = "Frequência",main = "")#assimet direita

hist(extcapita,breaks = 50,xlim = c(0.000262,9.610531),xlab = "Extrativismo per capita",ylab = "Frequência", main = "")#assimet

hist(icmse,breaks = 50,xlim = c(0,6474210),xlab = "ICMSE",ylab = "Frequência", main = "")#assimet

hist(populacao,breaks = 50,xlim = c(953,585367),xlab = "População",ylab = "Frequência", main = "")

hist(prop.icmse,breaks = 50,xlim = c(0,0.293),xlab = "ICMSE/Receita",ylab = "Frequência", main = "")

#correlacao entre variaveis da base
library(corrplot)
M<-cor(MTPainel[,-c(1,2,14)])
corrplot(M, method = "circle")# indicou alta correlacao entre receita e populcao:tirar receita!
                              # tirar icms tambem : alta relacao com despesa

################ Graficos de Dispersao: ICMSE
#### Saude IFDM
#Separando por ano
ggplot(MTPainel, aes(y =saude, x = icmse,colour = as.factor(ano))) + 
  geom_point() +
  geom_smooth(method="lm")+
  labs(x = "ICMSE", y = "Saúde IFDM")+
  theme(legend.title = element_blank())

#Agregando os anos - colocar ao quadrado icmse
ggplot(MTPainel, aes(y =saude, x = icmse)) + 
  geom_point() +
  geom_smooth()+
  labs(x = "ICMSE", y = "Saúde IFDM")+
  theme(legend.title = element_blank())

#### Educacao IFDM
ggplot(MTPainel, aes(y =educacao, x = icmse,colour = as.factor(ano))) + 
  geom_point() +
  geom_smooth()+
  labs(x = "ICMSE", y = "Educação IFDM")+
  theme(legend.title = element_blank())

#agregando os dados - colocar em log
ggplot(MTPainel, aes(y =educacao, x = icmse)) + 
  geom_point() +
  geom_smooth()+
  labs(x = "ICMSE", y = "Educação IFDM")+
  theme(legend.title = element_blank())

#### Renda e emprego IFDM
ggplot(MTPainel, aes(y =renda_emp, x = icmse,colour = as.factor(ano))) + 
  geom_point() +
  geom_smooth(method="lm")+
  labs(x = "ICMSE", y = "Renda e Emprego")+
  theme(legend.title = element_blank())

#Agreagando os dados- colocar ao quadrado
ggplot(MTPainel, aes(y =renda_emp, x = icmse)) + 
  geom_point() +
  geom_smooth()+
  labs(x = "ICMSE", y = "Renda e Emprego")+
  theme(legend.title = element_blank())

################ Graficos de Dispersao: ICMSE/Receita
#### Saude IFDM
#Separando por ano
ggplot(MTPainel, aes(y =saude, x = prop.icmse,colour = as.factor(ano))) + 
  geom_point() +
  geom_smooth(method="lm")+
  labs(x = "ICMSE", y = "Saúde IFDM")+
  theme(legend.title = element_blank())

#Agregando os anos - colocar ao quadrado icmse
ggplot(MTPainel, aes(y =saude, x = prop.icmse)) + 
  geom_point() +
  geom_smooth()+
  labs(x = "ICMSE", y = "Saúde IFDM")+
  theme(legend.title = element_blank())

#### Educacao IFDM
ggplot(MTPainel, aes(y =educacao, x = prop.icmse,colour = as.factor(ano))) + 
  geom_point() +
  geom_smooth()+
  labs(x = "ICMSE", y = "Educação IFDM")+
  theme(legend.title = element_blank())

#agregando os dados - colocar em log
ggplot(MTPainel, aes(y =educacao, x = prop.icmse)) + 
  geom_point() +
  geom_smooth()+
  labs(x = "ICMSE", y = "Educação IFDM")+
  theme(legend.title = element_blank())

#### Renda e emprego IFDM
ggplot(MTPainel, aes(y =renda_emp, x = prop.icmse,colour = as.factor(ano))) + 
  geom_point() +
  geom_smooth(method="lm")+
  labs(x = "ICMSE", y = "Renda e Emprego")+
  theme(legend.title = element_blank())

#Agreagando os dados- colocar ao quadrado
ggplot(MTPainel, aes(y =renda_emp, x = prop.icmse)) + 
  geom_point() +
  geom_smooth()+
  labs(x = "ICMSE", y = "Renda e Emprego")+
  theme(legend.title = element_blank())

######### ICMSE per capita
#EDUCACAO
#agregando os dados 
ggplot(MTPainel, aes(y =educacao, x = ICMSEpc)) + 
  geom_point() +
  geom_smooth()+
  labs(x = "ICMSE", y = "Educação IFDM")+
  theme(legend.title = element_blank())

#RENDA E EMPREGO
#Agreagando os dados
ggplot(MTPainel, aes(y =renda_emp, x = ICMSEpc)) + 
  geom_point() +
  geom_smooth()+
  labs(x = "ICMSE", y = "Renda e Emprego")+
  theme(legend.title = element_blank())

#EDUCACAO
ggplot(MTPainel, aes(y =saude, x = ICMSEpc)) + 
  geom_point() +
  geom_smooth()+
  labs(x = "ICMSE", y = "Saúde")+
  theme(legend.title = element_blank())

################## Box-Plot
#### SaudeIFDM
MTPainel%>%
  mutate(ano=as.factor(ano))%>%
  ggplot() +
  geom_boxplot(aes(x = ano, y = saude))

#### EducacaoIFDM
MTPainel%>%
  mutate(ano=as.factor(ano))%>%
  ggplot() +
  geom_boxplot(aes(x = ano, y = educacao))

#### Renda e Emprego IFDM
MTPainel%>%
  mutate(ano=as.factor(ano))%>%
  ggplot() +
  geom_boxplot(aes(x = ano, y = renda_emp))

#### ICMSE
MTPainel%>%
  mutate(ano=as.factor(ano))%>%
  ggplot() +
  geom_boxplot(aes(x = ano, y = icmse))

#### ICMSE per capita
MTPainel%>%
  mutate(ano=as.factor(ano))%>%
  ggplot() +
  geom_boxplot(aes(x = ano, y = ICMSEpc))

#### ICMSE/Receita
MTPainel%>%
  mutate(ano=as.factor(ano))%>%
  ggplot() +
  geom_boxplot(aes(x = ano, y = prop.icmse))

### Series Temporais
## media das variaveis por ano
#Saude IFDM
avgsaude<-MTPainel %>% 
  group_by(ano) %>% 
  summarise(avg_saude = mean(saude))

ggplot(avgsaude,aes(ano,avg_saude))+geom_line()+geom_point()+labs(x = "Ano", y = "Saúde IFDM")

#Renda e Emprego IFDM
avgRE<-MTPainel %>% 
  group_by(ano) %>% 
  summarise(avg_REmp = mean(renda_emp))

ggplot(avgRE,aes(ano,avg_REmp))+geom_line()+geom_point()+labs(x = "Ano", y = "Renda e Emprego IFDM")

#Educacao IFDM
avgeducacao<-MTPainel %>% 
  group_by(ano) %>% 
  summarise(avg_edu = mean(educacao))

ggplot(avgeducacao,aes(ano,avg_edu))+geom_line()+geom_point()+labs(x = "Ano", y = "Educacao")


#ICMSE/Receita
avgicmsep<-MTPainel %>% 
  group_by(ano) %>% 
  summarise(avg_picmse = mean(prop.icmse))

ggplot(avgicmsep,aes(ano,avg_picmse))+geom_line()+geom_point()+labs(x = "Ano", y = "ICMSE/Receita")

#ICMSE
avgicmse<-MTPainel %>% 
  group_by(ano) %>% 
  summarise(avg_icmse = mean(icmse))

ggplot(avgicmse,aes(ano,avg_icmse))+geom_line()+geom_point()+labs(x = "Ano", y = "ICMSE")
