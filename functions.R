############################################################################
###############################Fun????es######################################
############################################################################

###########Fun????es de Sele????es

########### selec_var
selec_var<-function(dataset){
  
  require(tidyverse)
  
  x <- dataset %>% 
    select(LOC, ANO, CODMUNIC, DEP, 
           PERMANEN,
           COMPPEN, PC486386, COMPOUTR, 
           VTV, VVIDEO,  PARABOL, RETRO,VIMPRESS,  
           ENER_PUB, ENER_GER, ESG_PUB, ESG_FOSS, AGUA_PUB,
           INTERNET,
           LAB_INFO, LAB_CIEN, BIBLIO, MERENDA, QUADRA )
  
  rm(list = as.character(substitute(dataset)), envir = .GlobalEnv )
  
  return( x )
}

#########################
###############selec_var1
selec_var1<-function(dataset){
  
  require(tidyverse)
  
  x <- dataset %>% 
    select(LOC, ANO, CODMUNIC, DEP, 
           PERMANEN,
           COMPPENT, PC486386, COMPOUTR, 
           VTV, VVIDEO,  PARABOL, RETRO,VIMPRESS,  
           ENER_PUB, ENER_GER, ESG_PUB, ESG_FOSS, AGUA_PUB,
           INTERNET,
           LAB_INFO, LAB_CIEN, BIBLIOTE, MERENDA, QUADRA)
  
  rm(list = as.character(substitute(dataset)), envir = .GlobalEnv )
  
  return( x )
}

#########################
###############selec_var2
selec_var2<-function(dataset){
  
  require(tidyverse)
  
  x <- dataset %>% 
    select(LOC, ANO, CODMUNIC, DEP, 
           PERMANEN,
           COMPPENT, PC486386, COMPOUTR, 
           VTV, VVIDEO,  PARABOL, VCOPIAD, RETRO,VIMPRESS,  
           ENER_PUB, ENER_GER, ESG_PUB, ESG_FOSS, AGUA_PUB,
           INTERNET,
           LAB_INFO, LAB_CIEN, BIBLIOTE, MERE_ESC, QUADRA)
  
  rm(list = as.character(substitute(dataset)), envir = .GlobalEnv )
  
  return( x )
}
########################
#######selec_var3

selec_var3<-function(dataset){
  
  require(tidyverse)
  
  x <- dataset %>% 
    select(LOC, ANO, CODMUNIC, DEP, 
           PERMANEN,
           COMPPENT, PC486386, COMPOUTR, 
           VTV, VVIDEO,  PARABOL, VCOPIAD, RETRO,VIMPRESS,  
           ENER_PUB, ENER_GER, ESG_PUB, ESG_FOSS, AGUA_PUB,
           INTERNET,
           LAB_INFO, LAB_CIEN, BIBLIOTE, MERE_ESC, QUAD_COB, QUAD_DES )
  
  rm(list = as.character(substitute(dataset)), envir = .GlobalEnv )
  
  return( x )
}

##################
########selec_var4
selec_var4<-function(dataset){
  
  require(tidyverse)
  
  x <- dataset %>% 
    select(LOC, ANO, CODMUNIC, DEP, PERMANEN,COMPPENT, PC486386, 
           COMPAPP, VIMPRESS, VVIDEO, VTV, PARABOL, RETRO,ENER_PUB,
           ENER_GER, ESG_PUB, ESG_FOSS, BIBLIO, QUAD_COB, QUAD_DES, LAB_INFO, LAB_CIEN, 
           LAB_OUTR, AGUA_PUB, MERE_ESC, VCOPIAD, INTERNET )
  
  rm(list = as.character(substitute(dataset)), envir = .GlobalEnv )
  
  return( x )
}
#####################
###########selec_var5
selec_var5<-function(dataset){
  
  require(tidyverse)
  
  x <- dataset %>% 
    select(LOC, ANO, CODMUNIC, DEP, PERMANEN,COMPPENT, PC486386, 
           COMPAPP, IMPRESS, VVIDEO, VTV, ANALOGTV, DIGIT_TV, RETRO,ENER_PUB,
           ENER_GER, ESG_PUB, ESG_FOSS, BIBLIO, QUAD_COB, QUAD_DES, LAB_INFO, LAB_CIEN, 
           LAB_OUTR, AGUA_PUB, MERE_ESC, CONEX_64,CONEX_64A128, CONEX_129A512, CONEX_512, VCOPIAD )
  
  rm(list = as.character(substitute(dataset)), envir = .GlobalEnv )
  
  return( x )
}
###################
#########selec_var6
selec_var6<-function(dataset){
  
  require(tidyverse)
  
  x <- dataset %>% 
    select(LOC, ANO, CODMUNIC, DEP, PERMANEN,COMPPENT, PC486386, 
           COMPAPP, IMPRESS, VVIDEO, VTV, ANALOGTV, DIGIT_TV, RETRO,ENER_PUB,
           ENER_GER, ESG_PUB, ESG_FOSS, BIBLIO, QUAD_COB, QUAD_DES, LAB_INFO, LAB_CIEN, 
           LAB_OUTR, AGUA_PUB, MERE_ESC, VCOPIAD, CONEX_64,CONEX_64A128, CONEX_129A512, CONEX_512, VCOPIAD )
  
  rm(list = as.character(substitute(dataset)), envir = .GlobalEnv )
  
  return( x )
}

###################
#########selec_var7

selec_var7<-function(dataset){
  
  require(tidyverse)
  
  
  
  x <- dataset %>% 
    select(ID_LOCALIZACAO, ANO_CENSO, FK_COD_MUNICIPIO, ID_DEPENDENCIA_ADM,
           NUM_SALAS_EXISTENTES, 
           NUM_COMPUTADORES,
           ID_EQUIP_TV, ID_EQUIP_VIDEOCASSETE, ID_EQUIP_DVD, ID_EQUIP_PARABOLICA,ID_EQUIP_COPIADORA, ID_EQUIP_RETRO, ID_EQUIP_IMPRESSORA,
           ID_ENERGIA_REDE_PUBLICA, ID_ENERGIA_GERADOR, ID_ESGOTO_REDE_PUBLICA, ID_ESGOTO_FOSSA, ID_AGUA_REDE_PUBLICA,
           ID_INTERNET,ID_LABORATORIO_INFORMATICA, ID_LABORATORIO_CIENCIAS, ID_BIBLIOTECA, ID_ALIMENTACAO, ID_QUADRA_ESPORTES)
  
  rm(list = as.character(substitute(dataset)), envir = .GlobalEnv )
  
  return( x )
}

##################
#######selec_var8
selec_var8<-function(dataset){
  
  require(tidyverse)
  
  x <- dataset %>% 
    select(ID_LOCALIZACAO, ANO_CENSO, FK_COD_MUNICIPIO, ID_DEPENDENCIA_ADM,
           NUM_SALAS_EXISTENTES, 
           NUM_COMPUTADORES,
           ID_EQUIP_TV, ID_EQUIP_VIDEOCASSETE, ID_EQUIP_DVD, ID_EQUIP_PARABOLICA,ID_EQUIP_COPIADORA, ID_EQUIP_RETRO, ID_EQUIP_IMPRESSORA,
           ID_ENERGIA_REDE_PUBLICA, ID_ENERGIA_GERADOR, ID_ESGOTO_REDE_PUBLICA, ID_ESGOTO_FOSSA, ID_AGUA_REDE_PUBLICA,
           ID_INTERNET,ID_LABORATORIO_INFORMATICA, ID_LABORATORIO_CIENCIAS, ID_BIBLIOTECA, ID_QUADRA_ESPORTES_COBERTA,ID_QUADRA_ESPORTES_DESCOBERTA , ID_ALIMENTACAO)
  
  rm(list = as.character(substitute(dataset)), envir = .GlobalEnv )
  
  return( x )
}

#########################
###############selec_var9
selec_var9<-function(dataset){
  
  require(tidyverse)
  
  x <- dataset %>% 
    select(ID_LOCALIZACAO, ANO_CENSO, FK_COD_MUNICIPIO, ID_DEPENDENCIA_ADM,
           NUM_SALAS_EXISTENTES, 
           NUM_COMPUTADORES,
           NUM_EQUIP_TV, NUM_EQUIP_VIDEOCASSETE, NUM_EQUIP_DVD, NUM_EQUIP_PARABOLICA ,NUM_EQUIP_COPIADORA , NUM_EQUIP_RETRO, NUM_EQUIP_IMPRESSORA,
           ID_ENERGIA_REDE_PUBLICA, ID_ENERGIA_GERADOR, ID_ESGOTO_REDE_PUBLICA, ID_ESGOTO_FOSSA, ID_AGUA_REDE_PUBLICA,
           ID_INTERNET,ID_LABORATORIO_INFORMATICA, ID_LABORATORIO_CIENCIAS, ID_BIBLIOTECA, ID_QUADRA_ESPORTES_COBERTA,ID_QUADRA_ESPORTES_DESCOBERTA , ID_ALIMENTACAO)
  
  rm(list = as.character(substitute(dataset)), envir = .GlobalEnv )
  
  return( x )
}

###########################
###############selec_var10
selec_var10<-function(dataset){
  
  require(tidyverse)
  
  x <- dataset %>% 
    select(ID_LOCALIZACAO, ANO_CENSO, FK_COD_MUNICIPIO, ID_DEPENDENCIA_ADM,
           NUM_SALAS_EXISTENTES,
           NUM_COMPUTADORES,
           NUM_EQUIP_TV, NUM_EQUIP_VIDEOCASSETE, NUM_EQUIP_DVD, NUM_EQUIP_PARABOLICA ,NUM_EQUIP_COPIADORA , NUM_EQUIP_RETRO, NUM_EQUIP_IMPRESSORA,
           ID_ENERGIA_REDE_PUBLICA, ID_ENERGIA_GERADOR, ID_ESGOTO_REDE_PUBLICA, ID_ESGOTO_FOSSA, ID_AGUA_REDE_PUBLICA,
           ID_INTERNET,
           ID_LABORATORIO_INFORMATICA, ID_LABORATORIO_CIENCIAS, ID_BIBLIOTECA, ID_QUADRA_ESPORTES_COBERTA,ID_QUADRA_ESPORTES_DESCOBERTA , ID_ALIMENTACAO
    )
  
  rm(list = as.character(substitute(dataset)), envir = .GlobalEnv )
  
  return( x )
}

########################
############selec_var11
selec_var11<-function(dataset){
  
  require(tidyverse)
  
  x <- dataset %>% 
    select(TP_LOCALIZACAO, NU_ANO_CENSO, CO_MUNICIPIO, TP_DEPENDENCIA,
           NU_SALAS_EXISTENTES,
           NU_COMPUTADOR,
           NU_EQUIP_TV, NU_EQUIP_VIDEOCASSETE, NU_EQUIP_DVD, NU_EQUIP_PARABOLICA ,NU_EQUIP_COPIADORA , NU_EQUIP_RETROPROJETOR, NU_EQUIP_IMPRESSORA,
           IN_ENERGIA_REDE_PUBLICA, IN_ENERGIA_GERADOR, IN_ESGOTO_REDE_PUBLICA, IN_ESGOTO_FOSSA, IN_AGUA_REDE_PUBLICA,
           IN_INTERNET,
           IN_LABORATORIO_INFORMATICA, IN_LABORATORIO_CIENCIAS, IN_BIBLIOTECA, IN_QUADRA_ESPORTES_COBERTA,IN_QUADRA_ESPORTES_DESCOBERTA , IN_ALIMENTACAO
    )
  
  rm(list = as.character(substitute(dataset)), envir = .GlobalEnv )
  
  return( x )
}

#############################
##################selec_var12

selec_var12<-function(dataset){
  
  require(tidyverse)
  
  x <- dataset %>% 
    select(TP_LOCALIZACAO, NU_ANO_CENSO, CO_MUNICIPIO, TP_DEPENDENCIA,
           QT_SALAS_EXISTENTES,
           QT_COMPUTADOR,
           QT_EQUIP_TV, QT_EQUIP_VIDEOCASSETE, QT_EQUIP_DVD, QT_EQUIP_PARABOLICA , QT_EQUIP_COPIADORA , QT_EQUIP_RETROPROJETOR, QT_EQUIP_IMPRESSORA,
           IN_ENERGIA_REDE_PUBLICA, IN_ENERGIA_GERADOR, IN_ESGOTO_REDE_PUBLICA, IN_ESGOTO_FOSSA, IN_AGUA_REDE_PUBLICA,
           IN_INTERNET,
           IN_LABORATORIO_INFORMATICA, IN_LABORATORIO_CIENCIAS, IN_BIBLIOTECA, IN_QUADRA_ESPORTES_COBERTA,IN_QUADRA_ESPORTES_DESCOBERTA , IN_ALIMENTACAO
    )
  
  rm(list = as.character(substitute(dataset)), envir = .GlobalEnv )
  
  return( x )
}

#########################Tratement##########################


#############trat0
trat_model<-function(dataset){
  
  require(tidyverse)
  
  
  dataset[is.na(dataset)] <-0
  
  dataset <- dataset %>%
    mutate(dataset = ifelse(IN_QUADRA_ESPORTES_COBERTA == 1 | IN_QUADRA_ESPORTES_DESCOBERTA==1, 1, 0))  
  
  dataset <- dataset[-c(23,24)]
  
  
  c_names<- c("loc", "year", "cod_mun", "dep",
              "classrooms", 
              "computer",
              "equip_tv", "equip_vcas", "equip_dvd", "equip_parab", "equip_cop",
              "equip_retro", "equip_impress",
              "ener_rp", "ener_ger", "swear_rp", "sewer_tank", "water_rp",
              "internet",
              "lab_info", "lab_science", "library", "feeding","sport_court")
  
  
  
  colnames(dataset)<-c_names
  return(dataset)
}
#############trat1
trat_model1<-function(dataset){
  
  require(tidyverse)
  
  
  dataset[is.na(dataset)] <-0
  
  dataset <- dataset %>%
    mutate(dataset = ifelse(ID_QUADRA_ESPORTES_COBERTA == 1 | ID_QUADRA_ESPORTES_DESCOBERTA==1, 1, 0))  
  
  dataset <- dataset[-c(23,24)]
  
  
  c_names<- c("loc", "year", "cod_mun", "dep",
              "classrooms", 
              "computer",
              "equip_tv", "equip_vcas", "equip_dvd", "equip_parab", "equip_cop",
              "equip_retro", "equip_impress",
              "ener_rp", "ener_ger", "swear_rp", "sewer_tank", "water_rp",
              "internet",
              "lab_info", "lab_science", "library", "feeding","sport_court")
  
  
  
  colnames(dataset)<-c_names
  return(dataset)
}
#############trat2
trat_model2<-function(dataset){
  
  require(tidyverse)
  
  
  dataset[is.na(dataset)] <-0
  
  c_names<- c("loc", "year", "cod_mun", "dep",
              "classrooms", 
              "computer",
              "equip_tv", "equip_vcas", "equip_dvd", "equip_parab", "equip_cop",
              "equip_retro", "equip_impress",
              "ener_rp", "ener_ger", "swear_rp", "sewer_tank", "water_rp",
              "internet",
              "lab_info", "lab_science", "library", "feeding","sport_court")
  
  
  
  colnames(dataset)<-c_names
  return(dataset)
}
###################################




















