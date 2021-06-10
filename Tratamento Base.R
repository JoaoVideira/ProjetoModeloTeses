# Configuracoes
library(tidyverse)
library(readxl)
library(geobr)
library(sf)
library(units)
library(crul)
library(lwgeom)
library(sidrar)#bases do SIDRA

# lendo os dados em formato excel (planilhas)
#Base de Dados Deflacionada
ICMS <- read_excel("Base de dados ICMS-e_deflacionado_completa.xlsx",sheet =1)
colnames(ICMS)<-c("CODMUNIC","MUNICIPIO","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012",
                  "2013","2014","2015","2016")
ICMSpainel<-gather(ICMS,ano,icms,-CODMUNIC,-MUNICIPIO)
ICMSpainel$ano<-as.numeric(ICMSpainel$ano)
ICMSpainel$CODMUNIC<-as.character(ICMSpainel$CODMUNIC)
summary(ICMSpainel)
ICMSpainel<-filter(ICMSpainel,ano>2004)
ICMSpainel$CODMUNIC<-str_sub(string = ICMSpainel$CODMUNIC,end= 6)

ICMSE <- read_excel("Base de dados ICMS-e_deflacionado_completa.xlsx",sheet =2)
colnames(ICMSE)<-c("CODMUNIC","MUNICIPIO","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012",
                  "2013","2014","2015","2016")
ICMSEpainel<-gather(ICMSE,ano,icmse,-CODMUNIC,-MUNICIPIO)
ICMSEpainel$ano<-as.numeric(ICMSEpainel$ano)
ICMSEpainel$CODMUNIC<-as.character(ICMSEpainel$CODMUNIC)
summary(ICMSEpainel)
ICMSEpainel<-filter(ICMSEpainel,ano>2004)
ICMSEpainel$CODMUNIC<-str_sub(string = ICMSEpainel$CODMUNIC,end= 6)

SAUDE <- read_excel("IFDMSaude.xlsx")
SAUDE<-SAUDE[c(5180:5320),-c(2,3,6,8,10,12,14,16,18,20,22,24,26,28)]
colnames(SAUDE)<-c("CODMUNIC","MUNICIPIO","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016")
saudepainel<-gather(SAUDE,ano,saude,-CODMUNIC,-MUNICIPIO)
saudepainel$ano<-as.numeric(saudepainel$ano)
saudepainel$saude<-as.numeric(saudepainel$saude)
summary(saudepainel)
saudepainel$saude<-replace_na(saudepainel$saude,0.6724)

Renda_emp <- read_excel("IFDMEmpregoRenda.xlsx")
Renda_emp<-Renda_emp[c(5180:5320),-c(2,3,6,8,10,12,14,16,18,20,22,24,26,28)]
colnames(Renda_emp)<-c("CODMUNIC","MUNICIPIO","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016")
renda_emp_painel<-gather(Renda_emp,ano,renda_emp,-CODMUNIC,-MUNICIPIO)
renda_emp_painel$ano<-as.numeric(renda_emp_painel$ano)
renda_emp_painel$renda_emp<-as.numeric(renda_emp_painel$renda_emp)
summary(renda_emp_painel)
renda_emp_painel$renda_emp<-replace_na(renda_emp_painel$renda_emp,0.5700)

Educacao <- read_excel("IFDMEducacao.xlsx")
Educacao<-Educacao[c(5180:5320),-c(2,3,6,8,10,12,14,16,18,20,22,24,26,28)]
colnames(Educacao)<-c("CODMUNIC","MUNICIPIO","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016")
educacaopainel<-gather(Educacao,ano,educacao,-CODMUNIC,-MUNICIPIO)
educacaopainel$ano<-as.numeric(educacaopainel$ano)
educacaopainel$educacao<-as.numeric(educacaopainel$educacao)
summary(educacaopainel)
educacaopainel$educacao<-replace_na(educacaopainel$educacao,0.6724)

# Populacao municipal
POP <- read_excel("Base de dados ICMS-e_deflacionado_completa.xlsx",sheet =8)
POP<-POP[-c(142:145),]
colnames(POP)<-c("CODMUNIC","MUNICIPIO","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016")
poppainel<-gather(POP,ano,populacao,-CODMUNIC,-MUNICIPIO)
poppainel$ano<-as.numeric(poppainel$ano)
summary(poppainel)
poppainel<-filter(poppainel,ano>2004)
poppainel$CODMUNIC<-as.character(poppainel$CODMUNIC)
poppainel$CODMUNIC<-str_sub(string = poppainel$CODMUNIC,end= 6)

#Receita Municipal
RECMUNIC <- read_excel("Base de dados ICMS-e_deflacionado_completa.xlsx",sheet =4)
colnames(RECMUNIC)<-c("CODMUNIC","MUNICIPIO","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016")
receitapainel<-gather(RECMUNIC,ano,receita,-CODMUNIC,-MUNICIPIO)
receitapainel$ano<-as.numeric(receitapainel$ano)
summary(receitapainel)
receitapainel<-filter(receitapainel,ano>2004)
receitapainel$CODMUNIC<-as.character(receitapainel$CODMUNIC)
receitapainel$CODMUNIC<-str_sub(string = receitapainel$CODMUNIC,end= 6)
receitapainel$receita[receitapainel$receita==0] <- mean(receitapainel$receita)
hist(receitapainel$receita)

#Despesa municipal
DESPMUNIC <- read_excel("Base de dados ICMS-e_deflacionado_completa.xlsx",sheet =3)
colnames(DESPMUNIC)<-c("CODMUNIC","MUNICIPIO","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016")
despesaapainel<-gather(DESPMUNIC,ano,despesa,-CODMUNIC,-MUNICIPIO)
despesaapainel$ano<-as.numeric(despesaapainel$ano)
despesaapainel<-filter(despesaapainel,ano>2004)
summary(despesaapainel)
despesaapainel$CODMUNIC<-as.character(despesaapainel$CODMUNIC)
despesaapainel$CODMUNIC<-str_sub(string = despesaapainel$CODMUNIC,end = 6)
despesaapainel$despesa[despesaapainel$despesa==0] <- mean(despesaapainel$despesa)

#Gestao Municipal
gest_ambiental <- read_excel("Base de dados ICMS-e_deflacionado_completa.xlsx",sheet =5)
colnames(gest_ambiental)<-c("CODMUNIC","MUNICIPIO","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016")
G.ambientalpainel<-gather(gest_ambiental,ano,G.ambiental,-CODMUNIC,-MUNICIPIO)
G.ambientalpainel$ano<-as.numeric(G.ambientalpainel$ano)
summary(G.ambientalpainel)
G.ambientalpainel$CODMUNIC<-as.character(G.ambientalpainel$CODMUNIC)
G.ambientalpainel$CODMUNIC<-str_sub(string = G.ambientalpainel$CODMUNIC,end = 6)

#Gastos Saneamento
saneamento <- read_excel("Base de dados ICMS-e_deflacionado_completa.xlsx",sheet =6)
colnames(saneamento)<-c("CODMUNIC","MUNICIPIO","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016")
saneamentopainel<-gather(saneamento,ano,saneamento,-CODMUNIC,-MUNICIPIO)
saneamentopainel$ano<-as.numeric(saneamentopainel$ano)
summary(saneamentopainel)
saneamentopainel$CODMUNIC<-str_sub(string = saneamentopainel$CODMUNIC,end = 6)

#Agricultura- valor da producao
#Fonte:https://sidra.ibge.gov.br/tabela/5457
valor.producao <- read_excel("Agricultura.xlsx",sheet =1)
valor.producao<-valor.producao[c(5180:5320),]
colnames(valor.producao)<-c("CODMUNIC","MUNICIPIO","2005","2006","2007","2008","2009","2010","2011","2012","2013",
                            "2014","2015","2016")
pro.agropainel<-gather(valor.producao,ano,valor.prod.agro,-CODMUNIC,-MUNICIPIO)
pro.agropainel$ano<-as.numeric(pro.agropainel$ano)
pro.agropainel$valor.prod.agro<-as.numeric(pro.agropainel$valor.prod.agro)
summary(pro.agropainel)
pro.agropainel$CODMUNIC<-str_sub(string = pro.agropainel$CODMUNIC,end = 6)

#Rebanho
#Fonte:https://sidra.ibge.gov.br/tabela/3939
rebanho <- read_excel("Rebanho.xlsx")
colnames(rebanho)<-c("CODMUNIC","MUNICIPIO","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014",
                     "2015","2016")
rebanho<-rebanho[5184:5324,]
rebpainel<-gather(rebanho,ano,rebanho,-CODMUNIC,-MUNICIPIO)
rebpainel$ano<-as.numeric(rebpainel$ano)
rebpainel$rebanho<-as.numeric(rebpainel$rebanho)
summary(rebpainel)
rebpainel$CODMUNIC<-str_sub(string = rebpainel$CODMUNIC,end = 6)

#Silvicultura - base nao sera utilizada
#Fonte:https://sidra.ibge.gov.br/tabela/291
silvicultura <- read_excel("Silvicultura.xlsx")
silvicultura<-silvicultura[3357:3449,]

#Dados valor da producao do Extrativismo 
#Fonte:https://sidra.ibge.gov.br/tabela/291
extrativismo<- read_excel("Extrativismo.xlsx")
extrativismo<-extrativismo[4978:5118,]
colnames(extrativismo)<-c("CODMUNIC","MUNICIPIO","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016")
extratpainel<-gather(extrativismo,ano,extrativismo,-CODMUNIC,-MUNICIPIO)
extratpainel$ano<-as.numeric(extratpainel$ano)
extratpainel$extrativismo<-as.numeric(extratpainel$extrativismo)
summary(extratpainel)
hist(extratpainel$extrativismo)
extratpainel$extrativismo<-replace_na(extratpainel$extrativismo,3469)
extratpainel$CODMUNIC<-str_sub(string = extratpainel$CODMUNIC,end = 6)

MTPainel<-full_join(saudepainel,educacaopainel,by=c("CODMUNIC","ano"))
MTPainel<-full_join(MTPainel,renda_emp_painel,by=c("CODMUNIC","ano"))
MTPainel<-full_join(MTPainel,ICMSEpainel,by=c("CODMUNIC","ano"))
MTPainel<-full_join(MTPainel,ICMSpainel,by=c("CODMUNIC","ano"))
MTPainel<-full_join(MTPainel,poppainel,by=c("CODMUNIC","ano"))
MTPainel<-full_join(MTPainel,pro.agropainel,by=c("CODMUNIC","ano"))
MTPainel<-full_join(MTPainel,rebpainel,by=c("CODMUNIC","ano"))
MTPainel<-full_join(MTPainel,extratpainel,by=c("CODMUNIC","ano"))
MTPainel<-full_join(MTPainel,receitapainel,by=c("CODMUNIC","ano"))
MTPainel<-full_join(MTPainel,despesaapainel,by=c("CODMUNIC","ano"))
MTPainel<-full_join(MTPainel,G.ambientalpainel,by=c("CODMUNIC","ano"))
MTPainel<-MTPainel[,-c(2,5,7,9,11,13,15,17,19,21,23,25)]
MTPainel<-full_join(MTPainel,saneamentopainel,by=c("CODMUNIC","ano"))
MTPainel<-MTPainel[,-21]

#Despesas municipais per capita
MTPainel$despercapita<-MTPainel$despesa/MTPainel$populacao

#ICMS per capita
MTPainel$ICMSpc<-MTPainel$icms/MTPainel$populacao

#ICMSE per capita
MTPainel$ICMSEpc<-MTPainel$icmse/MTPainel$populacao

# Producao agricola per capita
MTPainel$prop.plant<-MTPainel$valor.prod.agro/MTPainel$populacao

# proporcao de icmse nas receitas municipais
MTPainel$prop.icmse<-MTPainel$icmse/MTPainel$receita

#valor Producao pre capita
MTPainel$extcapita<-MTPainel$extrativismo/MTPainel$populacao

#Gastos saneamento per capita
MTPainel$saneamentopc<-MTPainel$saneamento/populacao
