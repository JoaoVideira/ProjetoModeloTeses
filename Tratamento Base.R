# Configuracoes
install.packages(c("tidyverse","readxl","geobr","sf","plm","lmtest","spdep","normtest","classInt","spatialreg",
                  "tseries","units","ggspatial","pmdplyr",dependecies=TRUE ))
library(tidyverse)
library(readxl)
library(geobr)
library(sf)
library(plm)
library(lmtest)
library(spdep)
library(normtest)
library(classInt)
library(spatialreg)
library(tseries)
library(units)
library(ggspatial)
library(pmdplyr)
library(crul)
library(lwgeom)

# lendo os dados em formato excel (planilhas)
ICMS <- read_excel("Base de dados ICMS-deflacionado.xlsx",sheet =1)
ICMSE <- read_excel("Base de dados ICMS-deflacionado.xlsx",sheet =2)
IFDM <- read_excel("Base de dados ICMS-e.xlsx",sheet =3,col_types = c("text", "text", "numeric", "numeric", "numeric",
                                                                   "numeric","numeric","numeric","numeric","numeric",
                                                                      "numeric","numeric","numeric","numeric"))
POP <- read_excel("Base de dados ICMS-deflacionado.xlsx",sheet =5)
RECMUNIC <- read_excel("Base de dados ICMS-deflacionado.xlsx",sheet =3)
DESPMUNIC <- read_excel("Base de dados ICMS-deflacionado.xlsx",sheet =4)

#Agricultura
Area.plantada <- read_excel("Agricultura.xlsx",sheet =1)
valor.producao <- read_excel("Agricultura.xlsx",sheet =2)
Area.plantada<-Area.plantada[5176:5316,]
valor.producao<-valor.producao[5176:5316,]
valor.producao<-rename(valor.producao,CODMUNIC=Cód.)

Plantadapainel<-gather(Area.plantada,ano,area.plantada,-CODMUNIC,-Município)
Plantadapainel<-mutate(Plantadapainel,area.plantada=as.numeric(area.plantada),ano=as.numeric(ano))

prod.painel<-gather(valor.producao,ano,valor.producao,-CODMUNIC,-Município)
prod.painel<-mutate(prod.painel,valor.producao=as.numeric(valor.producao),ano=as.numeric(ano))

#valor da producao por hectare de area plantada
Plantadapainel$prodha<-prod.painel$valor.producao/Plantadapainel$area.plantada

#Rebanho
rebanho <- read_excel("Rebanho.xlsx",sheet =1)
rebanho<-select(rebanho,c(1,2,18:22))
colnames(rebanho)<-c("CODMUNIC","MUNICIPIO","2005","2007","2010","2013","2016")
rebanho<-rebanho[5180:5320,]
rebpainel<-gather(rebanho,ano,rebanho,-CODMUNIC,-MUNICIPIO)
rebpainel$ano<-as.numeric(rebpainel$ano)

# Despesas municipais
DESPMUNIC$Municipio<-as.character(DESPMUNIC$MUNICÍPIO)
DESPMUNIC$`COD IBGE`<-as.character(DESPMUNIC$`COD IBGE`)
DESPMUNIC<-rename(DESPMUNIC,CODMUNIC =`COD IBGE`)
DESPMUNIC<-DESPMUNIC[,-18]

#ICMS
ICMS<-rename(ICMS,CODMUNIC = "COD IBGE")
ICMS<-rename(ICMS, "2002"= "Ano 2002", "2003"= "Ano 2003","2004"= "Ano 2004","2005"= "Ano 2005",
             "2006"= "Ano 2006","2007"= "Ano 2007","2008"= "Ano 2008","2009"= "Ano 2009", "2010"= "Ano 2010",
             "2011"= "Ano 2011","2012"= "Ano 2012","2013"= "Ano 2013","2014"= "Ano 2014","2015"= "Ano 2015","2016"= "Ano 2016")

ICMS$CODMUNIC<-as.character(ICMS$CODMUNIC)

#ICMSE
ICMSE<-rename(ICMSE,CODMUNIC = "COD IBGE")
ICMSE$CODMUNIC<-as.character(ICMSE$CODMUNIC)
ICMSE<-ICMSE[-142,]

#IFDM
IFDM<-IFDM[-c(142:166),]
IFDM<-rename(IFDM,CODMUNIC = "COD IBGE")

#POP
POP<-POP[-c(142:145),]
POP<-rename(POP,CODMUNIC = "COD IBGE")
POP$CODMUNIC<-as.character(POP$CODMUNIC)

#Receitas Municipais
RECMUNIC<-rename(RECMUNIC,CODMUNIC = "COD IBGE")
RECMUNIC$CODMUNIC<-as.character(RECMUNIC$CODMUNIC)

#Despesas municipais
Despmunicpainel<-gather(DESPMUNIC,ano,DESP.MUNIC,-CODMUNIC,-MUNICÍPIO)
Despmunicpainel$ano<-as.numeric(Despmunicpainel$ano)
Despmunicpainel<-filter(Despmunicpainel,ano==2005|ano==2007|ano==2010|ano==2013|ano==2016)

#ICMS 
ICMSpainel<-gather(ICMS,ano,ICMS,-CODMUNIC,-MUNICÍPIO)
ICMSpainel$ano<-as.numeric(ICMSpainel$ano)
ICMSpainel<-filter(ICMSpainel,ano==2005|ano==2007|ano==2010|ano==2013|ano==2016)

#ICMS Ecologico
ICMSEpainel<-gather(ICMSE,ano,ICMSE,-CODMUNIC,-MUNICÍPIO)
ICMSEpainel$ano<-as.numeric(ICMSEpainel$ano)
ICMSEpainel<-filter(ICMSEpainel,ano==2005|ano==2007|ano==2010|ano==2013|ano==2016)

#IFDM
IFDMpainel<-gather(IFDM,ano,IFDM,-CODMUNIC,-MUNICÍPIO)
IFDMpainel$ano<-as.numeric(IFDMpainel$ano)
IFDMpainel<-filter(IFDMpainel,ano==2005|ano==2007|ano==2010|ano==2013|ano==2016)

#Populacao
Poppainel<-gather(POP,ano,Pop,-CODMUNIC,-Município)
Poppainel$ano<-as.numeric(Poppainel$ano)
Poppainel<-filter(Poppainel,ano==2005|ano==2007|ano==2010|ano==2013|ano==2016)

#Receitas Municipais
RecMunpainel<-gather(RECMUNIC,ano,receita,-CODMUNIC)
RecMunpainel$ano<-as.numeric(RecMunpainel$ano)

#DADOS SNIS
SNIS<-read.csv2("SNIS.csv", sep = ";", stringsAsFactors = FALSE)%>%
  rename(ano=Ano.de.Referência)%>%
  mutate(ano=as.numeric(ano))%>%
  filter(ano==2005|ano==2007|ano==2010|ano==2013|ano==2016)

#Dados Saneamento
SNIS<-SNIS[,-c(3,7)]
colnames(SNIS)<-c("CODMUNIC","MUNIC","ano","Pop_urbano")
SNIS<-select(SNIS,-5)
SNIS$Pop_urbano<-as.numeric(SNIS$Pop_urbano)
SNIS[,4] <- gsub("[.]", "",SNIS[,4])
SNIS$Pop_urbano<-as.double(SNIS$Pop_urbano)

#Dados valor da producao do Extrativismo 
val.prod.ext<- read_excel("Extrativismo.xlsx",sheet =2,col_types = c("text","text","numeric","numeric",
                          "numeric","numeric","numeric"))
val.prod.ext<-val.prod.ext[4974:5114,]
val.prod.ext<-gather(val.prod.ext,ano,Valor.extrat,-CODMUNIC,-Município)
val.prod.ext$ano<-as.numeric(val.prod.ext$ano)

#Dados valor da producao do Extrativismo 
val.prod.sil<- read_excel("Silvicultura.xlsx",sheet =2,
                          col_types = c("text","text","numeric","numeric",
                                        "numeric","numeric","numeric"))
val.prod.sil<-val.prod.sil[3353:3445,]
val.prod.sil<-gather(val.prod.sil,ano,Valor.extrat,-CODMUNIC,-Município)
val.prod.sil$ano<-as.numeric(val.prod.sil$ano)

#Criando base painel espacial do MATO GROSSO
MT2005<-read_municipality(year=2005)%>%
  filter(code_state=="51")
MT2007<-read_municipality(year=2007)%>%
  filter(code_state=="51")
MT2010<-read_municipality(year=2010)%>%
  filter(code_state=="51")
MT2013<-read_municipality(year=2013)%>%
  filter(code_state=="51")
MT2016<-read_municipality(year=2016)%>%
  filter(code_state=="51")

##Juntando para formar o painel 
#Colocando na mesma coordenada geografica
crs<-st_crs(MT2005)
MT2007<-st_transform(MT2007,crs)
MT2010<-st_transform(MT2010,crs)
MT2013<-st_transform(MT2013,crs)
MT2016<-st_transform(MT2016,crs)

#Adicionando variavel ano
ano<-as.numeric(rep(2005,141))
MT2005<-cbind(MT2005,ano)
ano<-as.numeric(rep(2007,141))
MT2007<-cbind(MT2007,ano)
ano<-as.numeric(rep(2010,141))
MT2010<-cbind(MT2010,ano)
ano<-as.numeric(rep(2013,141))
MT2013<-cbind(MT2013,ano)
ano<-as.numeric(rep(2016,141))
MT2016<-cbind(MT2016,ano)

rm(MTPainel)
MTPainel<-rbind(MT2005,MT2007,MT2010,MT2013,MT2016)
MTPainel<-rename(MTPainel,CODMUNIC =code_muni)
MTPainel<-MTPainel[,-c(3,4)]
MTPainel$CODMUNIC<-as.character(MTPainel$CODMUNIC)

MTPainel<-left_join(MTPainel,Despmunicpainel,by=c("CODMUNIC","ano"))
MTPainel<-left_join(MTPainel,ICMSpainel,by=c("CODMUNIC","ano"))
MTPainel<-left_join(MTPainel,ICMSEpainel,by=c("CODMUNIC","ano"))
MTPainel<-left_join(MTPainel,IFDMpainel,by=c("CODMUNIC","ano"))
MTPainel<-left_join(MTPainel,Poppainel,by=c("CODMUNIC","ano"))
MTPainel<-left_join(MTPainel,RecMunpainel,by=c("CODMUNIC","ano"))
MTPainel<-left_join(MTPainel,SNIS,by=c("CODMUNIC","ano"))
MTPainel<-MTPainel[,-c(4,6,8,10,12,14,15,16)]

MTPainel<-left_join(MTPainel,Plantadapainel,by=c("CODMUNIC","ano"))
MTPainel<-MTPainel[,-9]
MTPainel<-left_join(MTPainel,rebpainel,by=c("CODMUNIC","ano"))
MTPainel<-MTPainel[,-11]

#Area dos municipios e ajustando unidade de medida
MTPainel$area<-st_area(MTPainel$geom)
MTPainel$area<-MTPainel$area/10000
MTPainel$area<-set_units(MTPainel$area, NULL)

#Despesas municipais per capita
MTPainel$despercapita<-MTPainel$DESP.MUNIC/MTPainel$Pop

#ICMS per capita
MTPainel$ICMSpc<-MTPainel$ICMS/MTPainel$Pop

#ICMSE per capita
MTPainel$ICMSEpc<-MTPainel$ICMSE/MTPainel$Pop

#Densidade Populacional
MTPainel$den.pop<-MTPainel$Pop/MTPainel$area

# Proporcao de area plantada
MTPainel$prop.plant<-MTPainel$area.plantada/MTPainel$area

# Rebanho por hectare
MTPainel$Reb.hec<-MTPainel$rebanho.y/MTPainel$area

PainelFinal<-MTPainel[,-c(4:6,8,9,11)]

# DADOS Altitude-----------------------------------------------------------
# FONTE:ftp://geoftp.ibge.gov.br/organizacao_do_territorio/estrutura_territorial/localidades/Geomedia_MDB/
#Dados sobre Altitude.
altitude<- read.csv2("Dados Gerais.csv", sep = ";", stringsAsFactors = FALSE)%>%
  filter(NM_UF=="MATO GROSSO")
altitude$LONG<-as.double(altitude$LONG)
altitude$LAT<-as.double(altitude$LAT)
crs<-st_crs(PainelFinal) 
altitude<-as_tibble(altitude)
class(altitude)

#base na classe sf
altitude_sf<-st_as_sf(altitude,coords = c("LONG","LAT"),crs=crs)%>%
  group_by(CD_GEOCODMU)%>%
  summarise(Altitude = median(ALT))
altitude_sf<-rename(altitude_sf,CODMUNIC=CD_GEOCODMU)
altitude_sf<-st_drop_geometry(altitude_sf)
altitude_sf$CODMUNIC<-as.character(altitude_sf$CODMUNIC)

altitude_sf$x2005<-altitude_sf$Altitude
altitude_sf$x2007<-altitude_sf$Altitude
altitude_sf$x2010<-altitude_sf$Altitude
altitude_sf$x2013<-altitude_sf$Altitude
altitude_sf$x2016<-altitude_sf$Altitude

altitude_sf<-altitude_sf[,-2]
colnames(altitude_sf)<-c("CODMUNIC","2005","2007","2010","2013","2016")
altpainel<-gather(altitude_sf,ano,alt.median,-CODMUNIC)
altpainel$ano<-as.numeric(altpainel$ano)

PainelFinal<-left_join(PainelFinal,altpainel,by=c("CODMUNIC","ano"))
PainelFinal<-left_join(PainelFinal,val.prod.ext,by=c("CODMUNIC","ano"))
PainelFinal<-left_join(PainelFinal,val.prod.sil,by=c("CODMUNIC","ano"))
PainelFinal<-PainelFinal[,-c(13,15,16)]

#Producao por hectare
PainelFinal$ext.hec<-PainelFinal$Valor.extrat/PainelFinal$area
PainelFinal<-PainelFinal[,-c(11,13)]
