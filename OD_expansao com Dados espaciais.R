library(plyr)
library(dplyr)
library(stringr)
library(tidyr)
library(lubridate)
library(geosphere)
library(st)
library(sf)
library(sp)
library(data.table)
library(aopdata)
library(terra)
#CARREGANDO ARQUIVOS BRUTOS
##CARREGANDO GPS
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
setwd('GPS')
myfiles = list.files(path=getwd(),pattern="*.csv",full.names=TRUE)
GPS = ldply(myfiles, read.delim,header=T,sep=";")
setwd('..')
##CARREGANDO SBE
setwd('SBE')
myfiles = list.files(path=getwd(),pattern="*.csv",full.names=TRUE)
SBE = ldply(myfiles, read.csv, sep=";")
rm(myfiles)
setwd('..')
##CARREGANDO DADOS DE DEMANDA DO MCO
## NECESSÁRIO COPIAR ELES DO XLSX PRELIMINARMENTE
myfiles = list.files(path=getwd(),pattern="relopemapa*",full.names=TRUE)
MCO <- read.csv(myfiles,sep=";",encoding = 'latin1')
##BD DE VIAGENS REALIZADAS - RESUMO
setwd('VRG')
myfiles = list.files(path=getwd(),pattern="*.csv",full.names=TRUE)
VG_REALIZADAS = ldply(myfiles, read.csv, sep=";",encoding='latin1')
setwd('..')
##LISTA DE PEDS GEORREFERENCIADOS
myfiles = list.files(path=getwd(),pattern="SUBLINHAS_vs*",full.names=TRUE)
SUBLINHA_VS_PED<-read.csv(myfiles,sep=";",encoding = 'latin1')
dia_emb<-str_sub(MCO$Data.Hora.Início.Operação[1],end=10)
##LISTA DE ESTAÇÕES GEORREFERENCIADAS
myfiles = list.files(path=getwd(),pattern="SUBLINHAS_vs*",full.names=TRUE)
SUBLINHA_VS_PED<-read.csv(myfiles,sep=";",encoding = 'latin1')
myfiles = list.files(path=getwd(),pattern="X e Y*",full.names=TRUE)
PED_Est<-read.csv(myfiles,sep=";")
rm(myfiles)
#CRIANDO CHAVE PARA ASSOCIAÇÃO DE EMBARQUES NO GPS

GPS$CHAVE<-paste(GPS$vei_nro_veiculo_gestor,GPS$horario_passagem)
GPS$CHAVE<-gsub(':','',GPS$CHAVE)
GPS$CHAVE<-gsub(' ','',GPS$CHAVE)
GPS$CHAVE<-as.numeric(GPS$CHAVE)

#TRANSFORMANDO CARTOES PARA TEXTO

SBE$CARTAO_USUARIO<-as.character(SBE$CARTAO_USUARIO)

#SEPARANDO DATA E HORA NO SBE

CIT<-str_split_fixed(SBE$DATAHORA_UTILIZACAO," ",2)
colnames(CIT) <-c("Data","Hora")
SBE<-cbind.data.frame(SBE,CIT)
rm(CIT)
SBE$DATAHORA_UTILIZACAO<-NULL
SBE$Data<-NULL
SBE$X<-NULL
SBE_ESTACOES<-filter(SBE,CODIGO_LINHA %in% c("AC01","AC02","AC03","AC04","AC05","AC06","AC07","AC08","AC09","AC10","AC11","AC13","AC14","AC15","AC16","AC17","AC18","AC19","AC20","AC21","AC22","AC23","AC24","AC25","CM02","CM03","CM04","CM05","CM06","CM07","CM08","CM09","CM10","SD01","SD02","PR01","PR02","1000","1002","2000","2002","3000","3002","4002","4003","4004","6000","7000","5001","5002","5003","5004","5005","5006","5007","5008","5009","5010","5011","5012","5013","5014","5015","5016","5017","5018","5019"))

#RETIRANDO VALIDACOES EM ESTACOES

SBE<-filter(SBE,!CODIGO_LINHA %in% c("AC01","AC02","AC03","AC04","AC05","AC06","AC07","AC08","AC09","AC10","AC11","AC13","AC14","AC15","AC16","AC17","AC18","AC19","AC20","AC21","AC22","AC23","AC24","AC25","CM02","CM03","CM04","CM05","CM06","CM07","CM08","CM09","CM10","SD01","SD02","PR01","PR02","1000","1002","2000","2002","3000","3002","4002","4003","4004","6000","7000","5001","5002","5003","5004","5005","5006","5007","5008","5009","5010","5011","5012","5013","5014","5015","5016","5017","5018","5019","1001","2001","3001","4001","6001","7001"))
SBE<-filter(SBE,NOME_OPERADORA!="SINDPAUTRAS")

#VEICULO E HORA EM OPERACAO NO SBE

SBE$VEIC_COM_FX<-str_c(SBE$CODIGO_VEICULO,str_sub(SBE$Hora,end=2),sep="-")
VEIC.GPS<-data.frame(unique(str_c(GPS$vei_nro_veiculo_gestor,str_sub(GPS$horario_passagem,end=2),sep="-")))
SBE<-merge(SBE,VEIC.GPS,by.x="VEIC_COM_FX",by.y="unique.str_c.GPS.vei_nro_veiculo_gestor..str_sub.GPS.horario_passagem..")
SBE$VEIC_COM_FX<-NULL


#CRIANDO A CHAVE NO SBE

SBE$CHAVE<-paste(SBE$CODIGO_VEICULO,SBE$Hora)
SBE$CHAVE<-gsub(':','',SBE$CHAVE)
SBE$CHAVE<-gsub(' ','',SBE$CHAVE)
SBE$CHAVE<-as.numeric(SBE$CHAVE)
SBE<-arrange(SBE,CODIGO_VEICULO,Hora)
GPS<-arrange(GPS,vei_nro_veiculo_gestor,horario_passagem)

# Fun??o para associar o par_cod_siu ao n?mero mais pr?ximo na coluna CHAVE
associar_par_cod_siu <- function(SBE, GPS) {
  setDT(SBE)
  setDT(GPS)
  
  # Verificar duplicatas e remover se necess?rio
  SBE <- unique(SBE, by = "CHAVE")
  GPS <- unique(GPS, by = "CHAVE")
  
  # Verificar valores faltantes
  if (any(is.na(SBE$CHAVE))) {
    stop("Valores faltantes na coluna CHAVE do dataset SBE.")
  }
  if (any(is.na(GPS$CHAVE))) {
    stop("Valores faltantes na coluna CHAVE do dataset GPS.")
  }
  
  setkey(SBE, CHAVE)
  setkey(GPS, CHAVE)
  
  indices_proximos <- GPS[SBE, roll = "nearest", on = "CHAVE"]$sub_lin_sg_linha
  SBE$sub_lin_sg_linha <- indices_proximos
  indices_proximos <- GPS[SBE, roll = "nearest", on = "CHAVE"]$par_cod_siu
  SBE$par_cod_siu <- as.character(indices_proximos)
  indices_proximos <- GPS[SBE, roll = "nearest", on = "CHAVE"]$cod_viagem
  SBE$cod_viagem <- as.character(indices_proximos)
  indices_proximos <- GPS[SBE, roll = "nearest", on = "CHAVE"]$inicio_viagem
  SBE$inicio_viagem <- indices_proximos
  indices_proximos <- GPS[SBE, roll = "nearest", on = "CHAVE"]$sentido_itinerario
  SBE$sentido_itinerario<-indices_proximos
  indices_proximos <- GPS[SBE, roll = "nearest", on = "CHAVE"]$data
  SBE$data<-indices_proximos
  return(SBE)
}

# Utilizando a fun??o associar_par_cod_siu
EMBARQUES <- associar_par_cod_siu(SBE, GPS)
EMBARQUES$CHAVE<-NULL
SBE_ESTACOES$data<-EMBARQUES$data[1]
SBE_ESTACOES$sub_lin_sg_linha<-str_c(SBE_ESTACOES$CODIGO_LINHA,'-01')
SBE_ESTACOES$par_cod_siu<-SBE_ESTACOES$CODIGO_LINHA
SBE_ESTACOES$cod_viagem<-str_c(SBE_ESTACOES$CODIGO_LINHA,str_sub(SBE_ESTACOES$Hora,end=2))
SBE_ESTACOES$inicio_viagem<-SBE_ESTACOES$Hora
SBE_ESTACOES$sentido_itinerario<-'PC1'
EMBARQUES<-bind_rows(EMBARQUES,SBE_ESTACOES)

#REMOVENDO ARQUIVOS DESNECESS?RIOS
colnames(EMBARQUES)<-c("NOME OPERADORA","VEICULO","LINHA","CARTAO","TP_CARTAO","VALOR_COBRADO","HORARIO_VALIDACAO","CIT_SUBLINHA","SIU","COD_VIAGEM","Abertura","Sentido","CIT_DATA")
rm(associar_par_cod_siu,VEIC.GPS)

#MESCLANDO VIAGENS REALIZADAS COM EMBARQUES
EMBARQUES$CIT_SUBLINHA<-str_sub(EMBARQUES$CIT_SUBLINHA,start=-2)
EMBARQUES$CIT_SUBLINHA<-as.numeric(EMBARQUES$CIT_SUBLINHA)
EMBARQUES$Sentido<-gsub('PC','',EMBARQUES$Sentido)
EMBARQUES$Sentido[is.na(EMBARQUES$Sentido)]<-1
EMBARQUES<-filter(EMBARQUES,Sentido %in% c('1','2'))
EMBARQUES$Sentido<-as.numeric(EMBARQUES$Sentido)
EMBARQUES$inicio_viagem[is.na(EMBARQUES$inicio_viagem)]<-str_c(str_sub(EMBARQUES$Hora,end=3),'00')

#CRIANDO CHAVE PARA EXPANSAO - PASSAGEIROS COM EMBARQUE CONHECIDO
EMBARQUES_ESTACOES<-filter(EMBARQUES,LINHA %in% c("AC01","AC02","AC03","AC04","AC05","AC06","AC07","AC08","AC09","AC10","AC11","AC13","AC14","AC15","AC16","AC17","AC18","AC19","AC20","AC21","AC22","AC23","AC24","AC25","CM02","CM03","CM04","CM05","CM06","CM07","CM08","CM09","CM10","SD01","SD02","PR01","PR02","1000","1002","2000","2002","3000","3002","4002","4003","4004","6000","7000","5001","5002","5003","5004","5005","5006","5007","5008","5009","5010","5011","5012","5013","5014","5015","5016","5017","5018","5019","1001","2001","3001","4001","6001","7001"))
EMBARQUES<-filter(EMBARQUES,!LINHA %in% c("AC01","AC02","AC03","AC04","AC05","AC06","AC07","AC08","AC09","AC10","AC11","AC13","AC14","AC15","AC16","AC17","AC18","AC19","AC20","AC21","AC22","AC23","AC24","AC25","CM02","CM03","CM04","CM05","CM06","CM07","CM08","CM09","CM10","SD01","SD02","PR01","PR02","1000","1002","2000","2002","3000","3002","4002","4003","4004","6000","7000","5001","5002","5003","5004","5005","5006","5007","5008","5009","5010","5011","5012","5013","5014","5015","5016","5017","5018","5019","1001","2001","3001","4001","6001","7001"))
EMBARQUES_ESTACOES$Abertura<-str_c(str_sub(EMBARQUES_ESTACOES$HORARIO_VALIDACAO,end=2),":00")
EMBARQUES<-rbind(EMBARQUES,EMBARQUES_ESTACOES)
rm(EMBARQUES_ESTACOES)
EMBARQUES$VEIC.H.ABERT<-str_c(EMBARQUES$VEICULO,str_sub(EMBARQUES$Abertura,start=-8),sep=":")
EMBARQUES$PASS_PESQ<-1
LIST_PASS_PESQ_POR_VIAGEM_EMB<-tapply(EMBARQUES$PASS_PESQ,EMBARQUES$VEIC.H.ABERT, sum)
PASS_PESQU_VIAGEM_EMB<-as.character(LIST_PASS_PESQ_POR_VIAGEM_EMB)
PASS_PESQU_VIAGEM_EMB<-as.numeric(LIST_PASS_PESQ_POR_VIAGEM_EMB)
PASS_PESQU_VIAGEM_EMB<-data.frame(cbind(row.names(LIST_PASS_PESQ_POR_VIAGEM_EMB),PASS_PESQU_VIAGEM_EMB),row.names=NULL)
colnames(PASS_PESQU_VIAGEM_EMB)<-c("CHAVE","PASS_PESQU")
PASS_PESQU_VIAGEM_EMB$PASS_PESQU<-as.numeric(as.character(PASS_PESQU_VIAGEM_EMB$PASS_PESQU))
rm(LIST_PASS_PESQ_POR_VIAGEM_EMB)


#CALCULANDO INTERVALOS ENTRE VALIDA??ES
OD<-EMBARQUES
OD$HORARIO_VALIDACAO<- as.POSIXct(str_c(OD$CIT_DATA,' ',OD$HORARIO_VALIDACAO), format="%Y-%m-%d %H:%M:%S")
OD<-OD[order(OD$CARTAO,OD$HORARIO_VALIDACAO),]

# Selecionar apenas as valida??es que t?m mais de um registro
OD_filtrado <- OD %>% 
  group_by(CARTAO) %>% 
  filter(n() > 1)

# Calcular o intervalo de tempo entre as valida??es para cada cart?o e retirando valida??es com intervalos menores que 5 min
OD_filtrado <- OD_filtrado %>%
  group_by(CIT_DATA, CARTAO) %>%
  mutate(INT_VAL = ifelse(row_number() == 1, 0, HORARIO_VALIDACAO - lag(HORARIO_VALIDACAO))) %>%
  filter(INT_VAL >= 5 | row_number() == 1)

# Selecionar apenas as primeiras valida??es de cada cart?o
primeiras_val = OD_filtrado %>% 
  group_by(CARTAO) %>% 
  filter(row_number() == 1)

# Selecionar as valida??es com intervalos maiores que 90 minutos
val_maiores_90 = OD_filtrado %>% 
  filter(INT_VAL >= 90)
val_menores_90 = OD_filtrado %>% 
  filter(INT_VAL < 90)

# Juntar os tr?s dataframes em um ?nico dataframe
OD <- bind_rows(primeiras_val, val_maiores_90)

#VALIDA??ES COM DIST?NCIA MENOR QUE 600M

PED<-SUBLINHA_VS_PED %>% dplyr::select("Código.SIU","Coord..X","Coord..Y")
colnames(PED)<-c("SIU","x","Y")
PED<-subset(PED,!duplicated(PED$SIU))
PED$SIU<-as.character(PED$SIU)
PED<-bind_rows(PED,PED_Est)

# Juntando os datasets para obter as coordenadas de cada SIU
OD_joined <- left_join(OD, PED, by = "SIU")

# Identifica os identificadores que possuem pelo menos um registro NA na coluna X
ids_com_na <- filter(OD_joined,is.na(x))
OD_joined<-filter(OD_joined,!CARTAO %in% ids_com_na$CARTAO)

proj4string <- CRS("+proj=utm +zone=23 +south +ellps=GRS80 +units=m +no_defs")
spdf_joined <- SpatialPointsDataFrame(coords = OD_joined[,c("x", "Y")], data = OD_joined,
                                      proj4string = proj4string)

# Converte para WGS84
spdf_joined_wgs84 <- spTransform(spdf_joined, CRS("+init=epsg:4326"))

# Ordena o dataset pelo cart?o e hor?rio de utiliza??o
OD_joined <- OD_joined[order(OD_joined$CARTAO, OD_joined$HORARIO_VALIDACAO),]


# Calcula a dist?ncia entre o registro atual e o anterior
distHaversine <- function(lon1, lat1, lon2, lat2) {
  rad <- pi / 180
  R <- 6378137
  dlat <- (lat2 - lat1) * rad
  dlon <- (lon2 - lon1) * rad
  lat1 <- lat1 * rad
  lat2 <- lat2 * rad
  a <- sin(dlat / 2) ^ 2 + cos(lat1) * cos(lat2) * sin(dlon / 2) ^ 2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  d <- R * c
  return(d)
}

OD_joined$dist <- ifelse(OD_joined$CARTAO==lag(OD_joined$CARTAO),c(0, distHaversine(OD_joined$x[-nrow(OD_joined)], OD_joined$Y[-nrow(OD_joined)], OD_joined$x[-1], OD_joined$Y[-1])),9999)
OD_joined$dist[1]<-9999

# Removendo as utiliza??es com dist?ncia inferior a 600 metros
OD_joined <- OD_joined %>% filter(dist >= 600)

# Selecionar apenas as valida??es que t?m mais de um registro para repetir a matriz distancia
OD_joined <- OD_joined %>% 
  group_by(CARTAO) %>% 
  filter(n() > 1)

# Encontra o valor mínimo não faltante em OD_joined$dist
j <- min(OD_joined$dist, na.rm = TRUE)

# Verifica se j é menor que 600
while(j<600){
  OD_joined$dist <- ifelse(OD_joined$CARTAO==lag(OD_joined$CARTAO),c(0, distHaversine(OD_joined$x[-nrow(OD_joined)], OD_joined$Y[-nrow(OD_joined)], OD_joined$x[-1], OD_joined$Y[-1])),9999)
  OD_joined$dist[1]<-9999
  OD_joined <- OD_joined %>% filter(dist >= 600)
  OD_joined <- OD_joined %>% group_by(CARTAO) %>% filter(n() > 1)
  j<-min(OD_joined$dist)
}
#Definindo os destinos de cada viagem

OD<-OD_joined
OD <- OD %>% 
  arrange(CARTAO) %>% 
  group_by(CARTAO) %>% 
  mutate(SIU_prox = ifelse(row_number() == n(), first(SIU), lead(SIU)))

#EXPANS?O ESPACIAL

colnames(PED)<-c('SIU','X','Y')
PED <- SpatialPointsDataFrame(coords = PED[,c("X", "Y")], data = PED,proj4string = proj4string)
PED <- spTransform(PED, CRS("+init=epsg:4326"))
PED<-st_as_sf(PED)
grid<-read_grid(city='bho')
PED<-st_join(PED,grid)
rm(grid)
PED[,5:8]<-NULL
PED<-as.data.frame(PED)
PED[2:3]<-NULL
OD<-merge(OD,PED,by.x="SIU",by.y="SIU")
OD$id_hex<-str_c(OD$id_hex,str_sub(OD$HORARIO_VALIDACAO,start=12,end=13))
PASS_PESQ<-OD %>% dplyr::select(id_hex)
PASS_PESQ$COUNT<-1
PASS_PESQ<-PASS_PESQ %>% dplyr::group_by(id_hex) %>% dplyr::summarise(PASS_PESQ=sum(COUNT))
EMBARQUES<-merge(EMBARQUES,PED,by.x="SIU",by.y="SIU")
EMBARQUES$id_hex<-str_c(EMBARQUES$id_hex,str_sub(EMBARQUES$HORARIO_VALIDACAO,start=1,end=2))
PASS_TRA<-EMBARQUES %>% dplyr::select(id_hex)
PASS_TRA$COUNT<-1
PASS_TRA<-PASS_TRA %>% dplyr::group_by(id_hex) %>% dplyr::summarise(PASS_TRA=sum(COUNT))
FE<-merge(PASS_PESQ,PASS_TRA,all.x=T,all.y=T)
val_menores_90<-merge(val_menores_90,PED)
val_menores_90<-filter(val_menores_90,INT_VAL!=0)
val_menores_90$id_hex<-str_c(val_menores_90$id_hex,str_sub(val_menores_90$HORARIO_VALIDACAO,start=12,end=13))
VALINT<-val_menores_90 %>% dplyr::select(id_hex)
VALINT$COUNT<-1
VALINT<-VALINT %>% dplyr::group_by(id_hex) %>% dplyr::summarise(VALINT=sum(COUNT))
FE<-merge(FE,VALINT,all.x=T,all.y=T)
FE[is.na(FE)]<-0
FE$FE1<-(FE$PASS_TRA-FE$VALINT)/FE$PASS_PESQ
FE$FE1[is.infinite(FE$FE1)]<-1
FE$FE1[is.na(FE$FE1)]<-1
OD<-merge(OD,FE %>% dplyr::select(id_hex,FE1),all.x=T)
OD$FE1[is.na(OD$FE1)]<-1

#EXPANSAO MCO
#TRABALHANDO COM O MCO
MCO<-filter(MCO,Num.Terminal %in% c(1,2))
MCO<-filter(MCO,!C.digo.Externo.Linha %in% c("AC01","AC02","AC03","AC04","AC05","AC06","AC07","AC08","AC09","AC10","AC11","AC13","AC14","AC15","AC16","AC17","AC18","AC19","AC20","AC21","AC22","AC23","AC24","AC25","CM02","CM03","CM04","CM05","CM06","CM07","CM08","CM09","CM10","SD01","SD02","PR01","PR02","1000","1002","2000","2002","3000","3002","4002","4003","4004","6000","7000","5001","5002","5003","5004","5005","5006","5007","5008","5009","5010","5011","5012","5013","5014","5015","5016","5017","5018","5019","1001","2001","3001","4001","6001","7001"))
MCO$VEIC.H.ABERT<-str_c(MCO$C.digo.Externo.Linha,MCO$Sub.Linha,MCO$Num.Terminal,str_sub(MCO$Data.Hora.In.cio.Opera..o,start = -10,end=-9),"00",sep=":")
PASS_TRANSPORTADOS_POR_VIAGEM<-MCO %>% dplyr::select(VEIC.H.ABERT,Passageiros)%>% dplyr::group_by(VEIC.H.ABERT) %>% summarise(PASS_TRA=sum(Passageiros))
colnames(PASS_TRANSPORTADOS_POR_VIAGEM)<-c("CHAVE","PASS_TRA")
PASS_TRANSPORTADOS_POR_VIAGEM$CHAVE<-gsub(' ','',PASS_TRANSPORTADOS_POR_VIAGEM$CHAVE)
PASS_TRANSPORTADOS_POR_VIAGEM$CHAVE<-as.character(PASS_TRANSPORTADOS_POR_VIAGEM$CHAVE)
PASS_TRANSPORTADOS_POR_VIAGEM$PASS_TRA<-as.numeric(as.character(PASS_TRANSPORTADOS_POR_VIAGEM$PASS_TRA))

#PASSAGEIROS TRANSPORTADOS POR ESTA??O
ESTACOES_SBE<-SBE_ESTACOES
ESTACOES_SBE$COUNT<-1
ESTACOES_SBE$VEIC.H.ABERT<-str_c(ESTACOES_SBE$CODIGO_VEICULO,str_c(str_sub(ESTACOES_SBE$Hora,end=2),":00"),sep=":")
MCO_ESTACOES<-ESTACOES_SBE %>% dplyr::select(VEIC.H.ABERT,COUNT) %>% dplyr::group_by(VEIC.H.ABERT) %>% summarise(PASS_TRA=sum(COUNT))
colnames(MCO_ESTACOES)<-c("CHAVE","PASS_TRA")
PASS_TRANSPORTADOS_POR_VIAGEM<-rbind(PASS_TRANSPORTADOS_POR_VIAGEM,MCO_ESTACOES)
rm(MCO_ESTACOES,ESTACOES_SBE)

#COLETANDO VALIDACOES INTERMEDI?RIAS
val_menores_90$Abertura<-str_sub(val_menores_90$Abertura,start=-8)
val_menores_90$Abertura<-str_sub(val_menores_90$Abertura,end=5)
val_menores_90$VEIC.H.ABERT<-str_c(val_menores_90$LINHA,val_menores_90$CIT_SUBLINHA,val_menores_90$Sentido,str_sub(val_menores_90$Abertura,end=2),"00",sep=':')
MCO_int<-val_menores_90 %>% dplyr::select(VEIC.H.ABERT)
MCO_int$Count<-1
MCO_int<-MCO_int %>% dplyr::select(VEIC.H.ABERT,Count) %>% dplyr::group_by(VEIC.H.ABERT) %>% dplyr::summarise(PASS_SEC=sum(Count))
colnames(MCO_int)<-c("CHAVE","PASS_SEC")

#RETIRANDO VALIDA??ES INTERMEDI?RIAS DA EXPANS?O
PASS_TRANSPORTADOS_POR_VIAGEM<-merge(PASS_TRANSPORTADOS_POR_VIAGEM,MCO_int,all.x = T)
OD$Abertura<-str_sub(OD$Abertura,start=-8)
OD$VEIC.H.ABERT<-str_c(OD$LINHA,OD$CIT_SUBLINHA,OD$Sentido,str_sub(OD$Abertura,end=2),'00',sep=":")
PASS_PESQ<-OD %>% dplyr::select(VEIC.H.ABERT,FE1)
PASS_PESQ<-PASS_PESQ %>% dplyr::group_by(VEIC.H.ABERT) %>% dplyr::summarise(PASS_PESQ=sum(FE1))
colnames(PASS_PESQ)<-c('CHAVE','PASS_PESQ')
EXPANSAO<-merge(PASS_TRANSPORTADOS_POR_VIAGEM,PASS_PESQ,all.x = T,all.y=T)
EXPANSAO$FE2<-abs((EXPANSAO$PASS_TRA-EXPANSAO$PASS_SEC)/(EXPANSAO$PASS_PESQ))
EXPANSAO$FE2[EXPANSAO$FE2<1]<-1
EXPANSAO$FE2[is.na(EXPANSAO$FE2)]<-1

#APLICANDO A EXPANS?O FINAL
OD<-merge(OD, EXPANSAO %>% dplyr::select(CHAVE,FE2),by.x="VEIC.H.ABERT",by.y = "CHAVE",all.x=T)
OD$FE<-OD$FE1*OD$FE2
sum(OD$FE)
#SALVANDO ARQUIVO FINAL - OD-SIU
dia_emb<-("DUNOV_2023")
write.csv2(OD,file=(str_c(dia_emb,"_OD.csv")),row.names=FALSE)

