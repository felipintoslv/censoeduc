memory.limit( size = 80000)

library(tidyverse)

library(kableExtra)

library(readr)

source("functions.R")

###### 1998 - n??oo tem VCOPIAD

CENSOESC_1998 <- read.csv2("Dados/micro_censo_escolar_1998/Dados/CENSOESC_1998.CSV", sep ="|")

censo_1998 <- CENSOESC_1998 %>% 
  select(LOC, ANO, CODMUNIC, DEP, 
         PERMANEN,
         COMPPEN, PC486386, COMPOUTR,
         VTV,VVIDEO, PARABOL, RETRO,ENER_PUB, VIMPRESS,
         ESG_PUB, ENER_GER, ESG_PUB, ESG_FOSS, AGUA_PUB, 
         LAB_INFO, LAB_CIEN,BIBLIO, MERENDA, QUADRA )
rm(CENSOESC_1998)

censo_1998$INTERNET <- 0

censo_1998$VCOPIAD <- 0

censo_1998$VDVD <- 0

censo_1998[is.na(censo_1998)] <-0


censo_1998 <- censo_1998 %>%
  mutate(ENER_PUB1 = ifelse( ENER_PUB == "s" , 1,0 ))

censo_1998 <- censo_1998 %>%
  mutate(ENER_GER1 = ifelse( ENER_GER== "s" , 1,0 ))

censo_1998 <- censo_1998 %>%
  mutate(ESG_PUB1 = ifelse( ESG_PUB == "s" , 1,0 ))

censo_1998 <- censo_1998 %>%
  mutate(ESG_FOSS1 = ifelse( ESG_FOSS == "s" , 1,0 ))

censo_1998 <- censo_1998 %>%
  mutate(AGUA_PUB1 = ifelse( AGUA_PUB == "s" , 1,0 ))

censo_1998 <- censo_1998 %>%
  mutate(INTERNET1 = ifelse( INTERNET == "s" , 1,0 ))

censo_1998 <- censo_1998 %>%
  mutate(LAB_INFO1 = ifelse( LAB_INFO == "s" , 1,0 ))

censo_1998 <- censo_1998 %>%
  mutate(LAB_CIEN1 = ifelse( LAB_CIEN == "s" , 1,0 ))

censo_1998 <- censo_1998 %>%
  mutate(BIBLIO1 = ifelse( BIBLIO == "s" , 1,0 ))

censo_1998 <- censo_1998 %>%
  mutate(MERENDA1 = ifelse( MERENDA == "s" , 1,0 ))

censo_1998 <- censo_1998 %>% 
  mutate(QUADRA1 = ifelse( QUADRA == "s", 1,0 ))

censo_1998 <- censo_1998 %>% 
  mutate(computer =  COMPPEN + PC486386 + COMPOUTR)%>%
  select(LOC, ANO, CODMUNIC, DEP, 
         PERMANEN,
         computer,
         VTV,VVIDEO, VDVD, PARABOL, VCOPIAD, RETRO,VIMPRESS,
         ENER_PUB1,  ENER_GER1, ESG_PUB1, ESG_FOSS1, AGUA_PUB1,
         INTERNET1,
         LAB_INFO1, LAB_CIEN1,BIBLIO1, MERENDA1, QUADRA1 )


c_names<- c("loc", "year", "cod_mun", "dep",
            "classrooms", 
            "computer",
            "equip_tv", "equip_vcas", "equip_dvd", "equip_parab", "equip_cop",
            "equip_retro", "equip_impress",
            "ener_rp", "ener_ger", "swear_rp", "sewer_tank", "water_rp",
            "internet",
            "lab_info", "lab_science", "library", "feeding","sport_court")


colnames(censo_1998) <- c_names

censo_total_1998 <- censo_1998

rm(censo_1998)

###### 1999

CENSOESC_1999 <- read.csv2("Dados/micro_censo_escolar_1999/Dados/CENSOESC_1999.CSV", sep = "|")
censo_1999<-selec_var(CENSOESC_1999) 
censo_1999$VCOPIAD <-0
censo_1999$VDVD <- 0

censo_1999[is.na(censo_1999)] <-0

censo_1999 <- censo_1999 %>%
  mutate(ENER_PUB1 = ifelse( ENER_PUB == "s" , 1,0 ))

censo_1999 <- censo_1999 %>%
  mutate(ENER_GER1 = ifelse( ENER_GER== "s" , 1,0 ))

censo_1999 <- censo_1999 %>%
  mutate(ESG_PUB1 = ifelse( ESG_PUB == "s" , 1,0 ))

censo_1999 <- censo_1999 %>%
  mutate(ESG_FOSS1 = ifelse( ESG_FOSS == "s" , 1,0 ))

censo_1999 <- censo_1999 %>%
  mutate(AGUA_PUB1 = ifelse( AGUA_PUB == "s" , 1,0 ))

censo_1999 <- censo_1999 %>%
  mutate(INTERNET1 = ifelse( INTERNET == "s" , 1,0 ))

censo_1999 <- censo_1999 %>%
  mutate(LAB_INFO1 = ifelse( LAB_INFO == "s" , 1,0 ))

censo_1999 <- censo_1999 %>%
  mutate(LAB_CIEN1 = ifelse( LAB_CIEN == "s" , 1,0 ))

censo_1999 <- censo_1999 %>%
  mutate(BIBLIO1 = ifelse( BIBLIO == "s" , 1,0 ))

censo_1999 <- censo_1999 %>%
  mutate(MERENDA1 = ifelse( MERENDA == "s" , 1,0 ))

censo_1999 <- censo_1999 %>% 
  mutate(QUADRA1 = ifelse( QUADRA == "s", 1,0 ))

censo_1999 <- censo_1999 %>% 
  mutate(computer =  COMPPEN + PC486386 + COMPOUTR)%>%
  select(LOC, ANO, CODMUNIC, DEP, 
         PERMANEN,
         computer,
         VTV,VVIDEO, VDVD, PARABOL, VCOPIAD, RETRO,VIMPRESS,
         ENER_PUB1,  ENER_GER1, ESG_PUB1, ESG_FOSS1, AGUA_PUB1,
         INTERNET1,
         LAB_INFO1, LAB_CIEN1,BIBLIO1, MERENDA1, QUADRA1 )

colnames(censo_1999) <- c_names

censo_total_1999 <- censo_1999

rm(censo_1999)

####### 2000 - BIBIOTE, COMPENET, 

CENSOESC_2000 <- read.csv2("Dados/micro_censo_escolar_2000/Dados/CENSOESC_2000.CSV", sep ="|")

censo_2000 <- selec_var1(CENSOESC_2000)

censo_2000$VCOPIAD <- 0
censo_2000$VDVD <- 0

censo_2000[is.na(censo_2000)] <-0

censo_2000 <- censo_2000 %>% 
  mutate(computer =  COMPPENT + PC486386 + COMPOUTR)%>%
  select(LOC, ANO, CODMUNIC, DEP, 
         PERMANEN,
         computer,
         VTV,VVIDEO, VDVD, PARABOL, VCOPIAD, RETRO,VIMPRESS,
         ENER_PUB,  ENER_GER, ESG_PUB, ESG_FOSS, AGUA_PUB,
         INTERNET,
         LAB_INFO, LAB_CIEN,BIBLIOTE, MERENDA, QUADRA )


censo_2000 <- censo_2000 %>%
  mutate(ENER_PUB1 = ifelse( ENER_PUB == "s" , 1,0 ))

censo_2000 <- censo_2000 %>%
  mutate(ENER_GER1 = ifelse( ENER_GER== "s" , 1,0 ))

censo_2000 <- censo_2000 %>%
  mutate(ESG_PUB1 = ifelse( ESG_PUB == "s" , 1,0 ))

censo_2000 <- censo_2000 %>%
  mutate(ESG_FOSS1 = ifelse( ESG_FOSS == "s" , 1,0 ))

censo_2000 <- censo_2000 %>%
  mutate(AGUA_PUB1 = ifelse( AGUA_PUB == "s" , 1,0 ))

censo_2000 <- censo_2000 %>%
  mutate(INTERNET1 = ifelse( INTERNET == "s" , 1,0 ))

censo_2000 <- censo_2000 %>%
  mutate(LAB_INFO1 = ifelse( LAB_INFO == "s" , 1,0 ))

censo_2000 <- censo_2000 %>%
  mutate(LAB_CIEN1 = ifelse( LAB_CIEN == "s" , 1,0 ))

censo_2000 <- censo_2000 %>%
  mutate(BIBLIOTE1 = ifelse( BIBLIOTE == "s" , 1,0 ))

censo_2000 <- censo_2000 %>%
  mutate(MERENDA1 = ifelse( MERENDA == "s" , 1,0 ))

censo_2000 <- censo_2000 %>% 
  mutate(QUADRA1 = ifelse( QUADRA == "s", 1,0 ))

censo_2000 <- censo_2000 %>% 
  select(LOC, ANO, CODMUNIC, DEP, 
         PERMANEN,
         computer,
         VTV,VVIDEO, VDVD, PARABOL, VCOPIAD, RETRO,VIMPRESS,
         ENER_PUB1,  ENER_GER1, ESG_PUB1, ESG_FOSS1, AGUA_PUB1,
         INTERNET1,
         LAB_INFO1, LAB_CIEN1,BIBLIOTE1, MERENDA1, QUADRA1 )

colnames(censo_2000) <- c_names

censo_total_2000 <- censo_2000

rm(censo_2000)
######## 2001 - MERE_ESC

CENSOESC_2001 <- read.csv2("Dados/micro_censo_escolar_2001/Dados/CENSOESC_2001.CSV", sep ="|")

censo_2001 <- selec_var3(CENSOESC_2001)
censo_2001$VDVD <- 0

censo_2001[is.na(censo_2001)] <-0
  
censo_2001 <- censo_2001 %>% 
  mutate(QUADRA = ifelse( QUAD_COB== 's' |  QUAD_DES== 's', 1,0 ))

censo_2001 <- censo_2001 %>% 
  mutate(computer =  COMPPENT + PC486386 + COMPOUTR)%>%
  select(LOC, ANO, CODMUNIC, DEP, 
         PERMANEN,
         computer,
         VTV,VVIDEO, VDVD, PARABOL, VCOPIAD, RETRO,VIMPRESS,
         ENER_PUB,  ENER_GER, ESG_PUB, ESG_FOSS, AGUA_PUB,
         INTERNET,
         LAB_INFO, LAB_CIEN,BIBLIOTE, MERE_ESC, QUADRA )

censo_2001 <- censo_2001 %>%
  mutate(ENER_PUB1 = ifelse( ENER_PUB == "s" , 1,0 ))

censo_2001 <- censo_2001 %>%
  mutate(ENER_GER1 = ifelse( ENER_GER== "s" , 1,0 ))

censo_2001 <- censo_2001 %>%
  mutate(ESG_PUB1 = ifelse( ESG_PUB == "s" , 1,0 ))

censo_2001 <- censo_2001 %>%
  mutate(ESG_FOSS1 = ifelse( ESG_FOSS == "s" , 1,0 ))

censo_2001 <- censo_2001 %>%
  mutate(AGUA_PUB1 = ifelse( AGUA_PUB == "s" , 1,0 ))

censo_2001 <- censo_2001 %>%
  mutate(INTERNET1 = ifelse( INTERNET == "s" , 1,0 ))

censo_2001 <- censo_2001 %>%
  mutate(LAB_INFO1 = ifelse( LAB_INFO == "s" , 1,0 ))

censo_2001 <- censo_2001 %>%
  mutate(LAB_CIEN1 = ifelse( LAB_CIEN == "s" , 1,0 ))

censo_2001 <- censo_2001 %>%
  mutate(BIBLIOTE1 = ifelse( BIBLIOTE == "s" , 1,0 ))

censo_2001 <- censo_2001 %>%
  mutate(MERE_ESC1 = ifelse( MERE_ESC == "s" , 1,0 ))

#censo_2001 <- censo_2001 %>% 
#  mutate(QUADRA1 = ifelse( QUADRA == "s", 1,0 ))

censo_2001 <- censo_2001 %>% 
  select(LOC, ANO, CODMUNIC, DEP, 
         PERMANEN,
         computer,
         VTV,VVIDEO, VDVD, PARABOL, VCOPIAD, RETRO,VIMPRESS,
         ENER_PUB1,  ENER_GER1, ESG_PUB1, ESG_FOSS1, AGUA_PUB1,
         INTERNET1,
         LAB_INFO1, LAB_CIEN1,BIBLIOTE1, MERE_ESC1, QUADRA )

colnames(censo_2001) <- c_names

censo_total_2001 <- censo_2001

rm(censo_2001)

########## 2002 - QUADRA == QUAD_COB, QUAD_DES

CENSOESC_2002 <- read.csv2("Dados/micro_censo_escolar_2002/Dados/CENSOESC_2002.CSV", sep = "|")


censo_2002 <- selec_var3(CENSOESC_2002)

censo_2002$VDVD <- 0

censo_2002[is.na(censo_2002)] <-0

censo_2002 <- censo_2002 %>% 
  mutate(computer =  COMPPENT + PC486386 + COMPOUTR)%>%
  select(LOC, ANO, CODMUNIC, DEP, 
         PERMANEN,
         computer,
         VTV,VVIDEO, VDVD, PARABOL, VCOPIAD, RETRO,VIMPRESS,
         ENER_PUB,  ENER_GER, ESG_PUB, ESG_FOSS, AGUA_PUB,
         INTERNET,
         LAB_INFO, LAB_CIEN,BIBLIOTE, MERE_ESC, QUAD_COB, QUAD_DES )

censo_2002 <- censo_2002 %>%
  mutate(ENER_PUB1 = ifelse( ENER_PUB == "s" , 1,0 ))

censo_2002 <- censo_2002 %>%
  mutate(ENER_GER1 = ifelse( ENER_GER== "s" , 1,0 ))

censo_2002 <- censo_2002 %>%
  mutate(ESG_PUB1 = ifelse( ESG_PUB == "s" , 1,0 ))

censo_2002 <- censo_2002 %>%
  mutate(ESG_FOSS1 = ifelse( ESG_FOSS == "s" , 1,0 ))

censo_2002 <- censo_2002 %>%
  mutate(AGUA_PUB1 = ifelse( AGUA_PUB == "s" , 1,0 ))

censo_2002 <- censo_2002 %>%
  mutate(INTERNET1 = ifelse( INTERNET == "s" , 1,0 ))

censo_2002 <- censo_2002 %>%
  mutate(LAB_INFO1 = ifelse( LAB_INFO == "s" , 1,0 ))

censo_2002 <- censo_2002 %>%
  mutate(LAB_CIEN1 = ifelse( LAB_CIEN == "s" , 1,0 ))

censo_2002 <- censo_2002 %>%
  mutate(BIBLIOTE1 = ifelse( BIBLIOTE == "s" , 1,0 ))

censo_2002 <- censo_2002 %>%
  mutate(MERE_ESC1 = ifelse( MERE_ESC == "s" , 1,0 ))

censo_2002 <- censo_2002 %>%
  mutate(QUADRA = ifelse( QUAD_COB== 's' |  QUAD_DES== 's', 1,0 ))
#censo_2001 <- censo_2001 %>% 
#  mutate(QUADRA1 = ifelse( QUADRA == "s", 1,0 ))

censo_2002 <- censo_2002 %>% 
  select(LOC, ANO, CODMUNIC, DEP, 
         PERMANEN,
         computer,
         VTV,VVIDEO, VDVD, PARABOL, VCOPIAD, RETRO,VIMPRESS,
         ENER_PUB1,  ENER_GER1, ESG_PUB1, ESG_FOSS1, AGUA_PUB1,
         INTERNET1,
         LAB_INFO1, LAB_CIEN1,BIBLIOTE1, MERE_ESC1, QUADRA )

colnames(censo_2002) <- c_names

censo_total_2002 <- censo_2002

rm(censo_2002)

###### 2003

CENSOESC_2003 <- read.csv2("Dados/micro_censo_escolar_2003/Dados/CENSOESC_2003.CSV", sep ="|")

censo_2003 <- selec_var3(CENSOESC_2003)

censo_2003$VDVD <- 0

censo_2003[is.na(censo_2003)] <-0

censo_2003 <- censo_2003 %>% 
  mutate(computer =  COMPPENT + PC486386 + COMPOUTR)%>%
  select(LOC, ANO, CODMUNIC, DEP, 
         PERMANEN,
         computer,
         VTV,VVIDEO, VDVD, PARABOL, VCOPIAD, RETRO,VIMPRESS,
         ENER_PUB,  ENER_GER, ESG_PUB, ESG_FOSS, AGUA_PUB,
         INTERNET,
         LAB_INFO, LAB_CIEN,BIBLIOTE, MERE_ESC, QUAD_COB, QUAD_DES )

censo_2003 <- censo_2003 %>%
  mutate(ENER_PUB1 = ifelse( ENER_PUB == "s" , 1,0 ))

censo_2003 <- censo_2003 %>%
  mutate(ENER_GER1 = ifelse( ENER_GER== "s" , 1,0 ))

censo_2003 <- censo_2003 %>%
  mutate(ESG_PUB1 = ifelse( ESG_PUB == "s" , 1,0 ))

censo_2003 <- censo_2003 %>%
  mutate(ESG_FOSS1 = ifelse( ESG_FOSS == "s" , 1,0 ))

censo_2003 <- censo_2003 %>%
  mutate(AGUA_PUB1 = ifelse( AGUA_PUB == "s" , 1,0 ))

censo_2003 <- censo_2003 %>%
  mutate(INTERNET1 = ifelse( INTERNET == "s" , 1,0 ))

censo_2003 <- censo_2003 %>%
  mutate(LAB_INFO1 = ifelse( LAB_INFO == "s" , 1,0 ))

censo_2003 <- censo_2003 %>%
  mutate(LAB_CIEN1 = ifelse( LAB_CIEN == "s" , 1,0 ))

censo_2003 <- censo_2003 %>%
  mutate(BIBLIOTE1 = ifelse( BIBLIOTE == "s" , 1,0 ))

censo_2003 <- censo_2003 %>%
  mutate(MERE_ESC1 = ifelse( MERE_ESC == "s" , 1,0 ))

censo_2003 <- censo_2003 %>%
  mutate(QUADRA = ifelse( QUAD_COB== 's' |  QUAD_DES== 's', 1, 0 ))

#censo_2001 <- censo_2001 %>% 
#  mutate(QUADRA1 = ifelse( QUADRA == "s", 1,0 ))

censo_2003 <- censo_2003 %>% 
  select(LOC, ANO, CODMUNIC, DEP, 
         PERMANEN,
         computer,
         VTV,VVIDEO, VDVD, PARABOL, VCOPIAD, RETRO,VIMPRESS,
         ENER_PUB1,  ENER_GER1, ESG_PUB1, ESG_FOSS1, AGUA_PUB1,
         INTERNET1,
         LAB_INFO1, LAB_CIEN1,BIBLIOTE1, MERE_ESC1, QUADRA )

colnames(censo_2003) <- c_names

censo_total_2003 <- censo_2003

rm(censo_2003)


###### 2004 --- BIBLIOTE virou BIBLIO - COMPOUT - COMPAPP

CENSOESC_2004 <- read.csv2("Dados/micro_censo_escolar_2004/Dados/CENSOESC_2004.CSV", sep = "|")

censo_2004 <- selec_var4(CENSOESC_2004)


censo_2004$VDVD <- 0

censo_2004[is.na(censo_2004)] <-0

censo_2004 <- censo_2004 %>% 
  mutate(computer =  COMPPENT + PC486386 + COMPAPP)%>%
  select(LOC, ANO, CODMUNIC, DEP, 
         PERMANEN,
         computer,
         VTV,VVIDEO, VDVD, PARABOL, VCOPIAD, RETRO,VIMPRESS,
         ENER_PUB,  ENER_GER, ESG_PUB, ESG_FOSS, AGUA_PUB,
         INTERNET,
         LAB_INFO, LAB_CIEN,BIBLIO, MERE_ESC, QUAD_COB, QUAD_DES )

censo_2004 <- censo_2004 %>%
  mutate(ENER_PUB1 = ifelse( ENER_PUB == "s" , 1,0 ))

censo_2004 <- censo_2004 %>%
  mutate(ENER_GER1 = ifelse( ENER_GER== "s" , 1,0 ))

censo_2004 <- censo_2004 %>%
  mutate(ESG_PUB1 = ifelse( ESG_PUB == "s" , 1,0 ))

censo_2004 <- censo_2004 %>%
  mutate(ESG_FOSS1 = ifelse( ESG_FOSS == "s" , 1,0 ))

censo_2004 <- censo_2004 %>%
  mutate(AGUA_PUB1 = ifelse( AGUA_PUB == "s" , 1,0 ))

censo_2004 <- censo_2004 %>%
  mutate(INTERNET1 = ifelse( INTERNET == "s" , 1,0 ))

censo_2004 <- censo_2004 %>%
  mutate(LAB_INFO1 = ifelse( LAB_INFO == "s" , 1,0 ))

censo_2004 <- censo_2004 %>%
  mutate(LAB_CIEN1 = ifelse( LAB_CIEN == "s" , 1,0 ))

censo_2004 <- censo_2004 %>%
  mutate(BIBLIOTE1 = ifelse( BIBLIO == "s" , 1,0 ))

censo_2004 <- censo_2004 %>%
  mutate(MERE_ESC1 = ifelse( MERE_ESC == "s" , 1,0 ))

censo_2004 <- censo_2004 %>%
  mutate(QUADRA = ifelse( QUAD_COB== 's' |  QUAD_DES== 's', 1, 0 ))

censo_2004 <- censo_2004 %>% 
  select(LOC, ANO, CODMUNIC, DEP, 
         PERMANEN,
         computer,
         VTV,VVIDEO, VDVD, PARABOL, VCOPIAD, RETRO,VIMPRESS,
         ENER_PUB1,  ENER_GER1, ESG_PUB1, ESG_FOSS1, AGUA_PUB1,
         INTERNET1,
         LAB_INFO1, LAB_CIEN1,BIBLIOTE1, MERE_ESC1, QUADRA )

colnames(censo_2004) <- c_names

censo_total_2004 <- censo_2004

rm(censo_2004)

##### 2005 Parabol virou ANALOGTV e DIGIT_TV / INTERNET = CONEX_64,CONEX_64A128, CONEX_129A512, CONEX_512

CENSOESC_2005 <- read.csv2("Dados/micro_censo_escolar_2005/Dados/CENSOESC_2005.CSV", sep ="|")

censo_2005 <- selec_var5(CENSOESC_2005)

censo_2005$VDVD <- 0

censo_2005[is.na(censo_2005)] <-0

censo_2005 <- censo_2005 %>% 
  mutate(computer =  COMPPENT + PC486386 + COMPAPP)

censo_2005 <- censo_2005 %>% 
  mutate(PARABOL =  ANALOGTV + DIGIT_TV )

censo_2005 <- censo_2005 %>%
  mutate(INTERNET = ifelse( CONEX_64== 's' |  CONEX_64A128== 's' |  CONEX_129A512== 's' |  CONEX_512== 's', 1, 0 ))%>%
  select(LOC, ANO, CODMUNIC, DEP, 
         PERMANEN,
         computer,
         VTV,VVIDEO, VDVD, PARABOL, VCOPIAD, RETRO,IMPRESS,
         ENER_PUB,  ENER_GER, ESG_PUB, ESG_FOSS, AGUA_PUB,
         INTERNET,
         LAB_INFO, LAB_CIEN,BIBLIO, MERE_ESC, QUAD_COB, QUAD_DES )

censo_2005 <- censo_2005 %>%
  mutate(ENER_PUB1 = ifelse( ENER_PUB == "s" , 1,0 ))

censo_2005 <- censo_2005 %>%
  mutate(ENER_GER1 = ifelse( ENER_GER== "s" , 1,0 ))

censo_2005 <- censo_2005 %>%
  mutate(ESG_PUB1 = ifelse( ESG_PUB == "s" , 1,0 ))

censo_2005 <- censo_2005 %>%
  mutate(ESG_FOSS1 = ifelse( ESG_FOSS == "s" , 1,0 ))

censo_2005 <- censo_2005 %>%
  mutate(AGUA_PUB1 = ifelse( AGUA_PUB == "s" , 1,0 ))

censo_2005 <- censo_2005 %>%
  mutate(LAB_INFO1 = ifelse( LAB_INFO == "s" , 1,0 ))

censo_2005 <- censo_2005 %>%
  mutate(LAB_CIEN1 = ifelse( LAB_CIEN == "s" , 1,0 ))

censo_2005 <- censo_2005 %>%
  mutate(BIBLIOTE1 = ifelse( BIBLIO == "s" , 1,0 ))

censo_2005 <- censo_2005 %>%
  mutate(MERE_ESC1 = ifelse( MERE_ESC == "s" , 1,0 ))

censo_2005 <- censo_2005 %>%
  mutate(QUADRA = ifelse( QUAD_COB== 's' |  QUAD_DES== 's', 1, 0 ))

censo_2005 <- censo_2005 %>% 
  select(LOC, ANO, CODMUNIC, DEP, 
         PERMANEN,
         computer,
         VTV,VVIDEO, VDVD, PARABOL, VCOPIAD, RETRO,IMPRESS,
         ENER_PUB1,  ENER_GER1, ESG_PUB1, ESG_FOSS1, AGUA_PUB1,
         INTERNET,
         LAB_INFO1, LAB_CIEN1,BIBLIOTE1, MERE_ESC1, QUADRA )

colnames(censo_2005) <- c_names

censo_total_2005 <- censo_2005

rm(censo_2005)

###### 2006 VIMPRESS

CENSOESC_2006 <- read.csv2("Dados/micro_censo_escolar_2006/Dados/CENSOESC_2006.CSV", sep ="|")

censo_2006 <- selec_var6(CENSOESC_2006)

censo_2006$VDVD <- 0

censo_2006[is.na(censo_2006)] <-0

censo_2006 <- censo_2006 %>% 
  mutate(computer =  COMPPENT + PC486386 + COMPAPP)

censo_2006 <- censo_2006 %>% 
  mutate(PARABOL =  ANALOGTV + DIGIT_TV )

censo_2006 <- censo_2006 %>%
  mutate(INTERNET = ifelse( CONEX_64== 's' |  CONEX_64A128== 's' |  CONEX_129A512== 's' |  CONEX_512== 's', 1, 0 ))%>%
  select(LOC, ANO, CODMUNIC, DEP, 
         PERMANEN,
         computer,
         VTV,VVIDEO, VDVD, PARABOL, VCOPIAD, RETRO,IMPRESS,
         ENER_PUB,  ENER_GER, ESG_PUB, ESG_FOSS, AGUA_PUB,
         INTERNET,
         LAB_INFO, LAB_CIEN,BIBLIO, MERE_ESC, QUAD_COB, QUAD_DES )

censo_2006 <- censo_2006 %>%
  mutate(ENER_PUB1 = ifelse( ENER_PUB == "s" , 1,0 ))

censo_2006 <- censo_2006 %>%
  mutate(ENER_GER1 = ifelse( ENER_GER== "s" , 1,0 ))

censo_2006 <- censo_2006 %>%
  mutate(ESG_PUB1 = ifelse( ESG_PUB == "s" , 1,0 ))

censo_2006 <- censo_2006 %>%
  mutate(ESG_FOSS1 = ifelse( ESG_FOSS == "s" , 1,0 ))

censo_2006 <- censo_2006 %>%
  mutate(AGUA_PUB1 = ifelse( AGUA_PUB == "s" , 1,0 ))

censo_2006 <- censo_2006 %>%
  mutate(LAB_INFO1 = ifelse( LAB_INFO == "s" , 1,0 ))

censo_2006 <- censo_2006 %>%
  mutate(LAB_CIEN1 = ifelse( LAB_CIEN == "s" , 1,0 ))

censo_2006 <- censo_2006 %>%
  mutate(BIBLIOTE1 = ifelse( BIBLIO == "s" , 1,0 ))

censo_2006 <- censo_2006 %>%
  mutate(MERE_ESC1 = ifelse( MERE_ESC == "s" , 1,0 ))

censo_2006 <- censo_2006 %>%
  mutate(QUADRA = ifelse( QUAD_COB== 's' |  QUAD_DES== 's', 1, 0 ))

censo_2006 <- censo_2006 %>% 
  select(LOC, ANO, CODMUNIC, DEP, 
         PERMANEN,
         computer,
         VTV,VVIDEO, VDVD, PARABOL, VCOPIAD, RETRO,IMPRESS,
         ENER_PUB1,  ENER_GER1, ESG_PUB1, ESG_FOSS1, AGUA_PUB1,
         INTERNET,
         LAB_INFO1, LAB_CIEN1,BIBLIOTE1, MERE_ESC1, QUADRA )

colnames(censo_2006) <- c_names

censo_total_2006 <- censo_2006

rm(censo_2006)


############## 2007 ?? RADICALMENTE DIFERENTE



##### VARI??VEIS DO ??LTIMO PER??ODO

# IDENTIFICADORES

#LOC, ANO, SIGLA, MUNIC ,CODMUNIC, DEP 

# SALAS EXISTENTES
#PERMANEN,

# COMPUTADORES

#COMPPENT, PC486386, COMPAPP

# EQUIPAMENTOS

#IMPRESS, VVIDEO, VTV, ANALOGTV, DIGIT_TV, RETRO

# INFRAESTRUTURA ESCOLAR

# ENER_PUB, ENER_GER, ESG_PUB, ESG_FOSS, AGUA_PUB

#INTERNET

#CONEX_64,CONEX_64A128, CONEX_129A512, CONEX_512

#DEPENDENCIA DA ESCOLA

#LAB_INFO, LAB_CIEN, LAB_OUTR,  MERE_ESC, BIBLIO, QUAD_COB, QUAD_DES,  

####### 2007

#### VAR - TORCER PARA SE REPETIR MUITO 

##### Faltas APSOM, procurar copiadora nos outros anos, LAB_OUTR

##### Coisas novas:  tem equipamento de DVD copiadora


CENSO_2007 <- read.csv2("Dados/micro_censo_escolar_2007/DADOS/ESCOLAS.CSV", 
                      sep =  "|")

censo_2007<-selec_var7(CENSO_2007)

censo_2007<-trat_model2(censo_2007)

censo_total_2007 <- censo_2007

rm(censo_2007)

#### 2008

CENSO_2008 <- read.csv2("Dados/micro_censo_escolar_2008/DADOS/ESCOLAS.CSV",sep="|")
censo_2008<-selec_var7(CENSO_2008)
censo_2008<-trat_model2(censo_2008)

censo_total_2008 <- censo_2008

rm(censo_2008)

#### 2009

CENSO_2009 <- read.csv2("Dados/micro_censo_escolar_2009/DADOS/ESCOLAS.CSV",sep="|")
censo_2009<-selec_var7(CENSO_2009)
censo_2009<-trat_model2(censo_2009)

censo_total_2009 <- censo_2009

rm(censo_2009)


#### 2010

CENSO_2010 <- read.csv2("Dados/micro_censo_escolar_2010/DADOS/ESCOLAS.CSV",sep="|")
censo_2010<-selec_var7(CENSO_2010)
censo_2010<-trat_model2(censo_2010)


censo_total_2010 <- censo_2010

rm(censo_2010)

#### 2011 - ID_QUADRA = ID_QUADRA_ESPORTES_COBERTA,ID_QUADRA_ESPORTES_DESCOBERTA 

CENSO_2011 <- read.csv2("Dados/micro_censo_escolar_2011/DADOS/ESCOLAS.CSV",sep="|")
censo_2011<-selec_var8(CENSO_2011)

censo_2011<-trat_model1(censo_2011)


censo_total_2011 <- censo_2011

##### 2012

CENSO_2012 <- read.csv2("Dados/micro_censo_escolar_2012/DADOS/ESCOLAS.CSV",sep="|")
censo_2012<-selec_var8(CENSO_2012)

censo_2012<-trat_model1(censo_2012)

censo_total_2012 <- censo_2012

rm(censo_2012)

##### 2013 
## em 2013 TEM-SE GRANDES MUDAN??AS, NEM T??O GRANDES....

# antes: ID_EQUIP_TV, ID_EQUIP_VIDEOCASSETE, ID_EQUIP_DVD, ID_EQUIP_PARABOLICA,ID_EQUIP_COPIADORA, ID_EQUIP_RETRO, ID_EQUIP_IMPRESSORA,
# DEPOIS:NUM_EQUIP_TV, NUM_EQUIP_VIDEOCASSETE, NUM_EQUIP_DVD, NUM_EQUIP_PARABOLICA ,NUM_EQUIP_COPIADORA , NUM_EQUIP_RETRO, NUM_EQUIP_IMPRESSORA

CENSO_2013 <- read.csv2("Dados/micro_censo_escolar_2013/DADOS/ESCOLAS.CSV",sep="|")
censo_2013<-selec_var9(CENSO_2013)

censo_2013<-trat_model1(censo_2013)

censo_total_2013 <- censo_2013

rm(censo_2013)


##### 2014

### AGORA FORAM GRANDES AS MUDAN??AS. PQP

# AT?? AGORA:

# IDENTIFICADORES - N??O TEM SIGLA EM 2014
# ID_LOCALIZACAO, ANO_CENSO, SIGLA, FK_COD_MUNICIPIO, ID_DEPENDENCIA_ADM

# SALAS EXISTENTES
#NUM_SALAS_EXISTENTES,

# COMPUTADORES
#NUM_COMPUTADORES,

# EQUIPAMENTOS
#NUM_EQUIP_TV, NUM_EQUIP_VIDEOCASSETE, NUM_EQUIP_DVD, NUM_EQUIP_PARABOLICA ,NUM_EQUIP_COPIADORA , NUM_EQUIP_RETRO, NUM_EQUIP_IMPRESSORA,


# INFRAESTRUTURA ESCOLAR
#ID_ENERGIA_REDE_PUBLICA, ID_ENERGIA_GERADOR, ID_ESGOTO_REDE_PUBLICA, ID_ESGOTO_FOSSA, ID_AGUA_REDE_PUBLICA,

#INTERNET
#ID_INTERNET

#DEPENDENCIA DA ESCOLA
#ID_LABORATORIO_INFORMATICA, ID_LABORATORIO_CIENCIAS, ID_BIBLIOTECA, ID_QUADRA_ESPORTES_COBERTA,ID_QUADRA_ESPORTES_DESCOBERTA , ID_ALIMENTACAO)
  

CENSO_2014 <- read.csv2("Dados/micro_censo_escolar_2014/DADOS/ESCOLAS.CSV",sep="|")
censo_2014<-selec_var10(CENSO_2014)

censo_2014<-trat_model1(censo_2014)

censo_total_2014 <- censo_2014

rm(censo_2014)

### 2015
#### ROLOU MUDAN??A NOS NOMES DAS VARI??VEIS - OS CARAS QUEREM ME FUDER

CENSO_2015 <- read.csv2("Dados/micro_censo_escolar_2015/DADOS/ESCOLAS.CSV",sep="|")


censo_2015<-selec_var11(CENSO_2015)

censo_2015 <- trat_model(censo_2015)

censo_total_2015 <- censo_2015

rm(censo_2015)

### 2016

CENSO_2016 <-read.csv2("Dados/micro_censo_escolar_2016/DADOS/ESCOLAS.CSV",sep="|")



censo_2016<-selec_var11(CENSO_2016)

censo_2016 <- trat_model(censo_2016)

censo_total_2016 <- censo_2016

rm(censo_2016)


### 2017


CENSO_2017 <- read.csv2("Dados/micro_censo_escolar_2017/DADOS/ESCOLAS.CSV", 
                         sep = "|")


censo_2017<-selec_var11(CENSO_2017)

censo_2017<-trat_model(censo_2017)

censo_total_2017 <- censo_2017

rm(censo_2017)

### 2018


CENSO_2018 <- read.csv2("Dados/micro_censo_escolar_2018/DADOS/ESCOLAS.CSV", 
                         sep = "|")
censo_2018<-selec_var12(CENSO_2018)

censo_2018<-trat_model(censo_2018)


censo_total_2018 <- censo_2018

rm(censo_2018)


########################################################################
#Informa????es Importantes para o filtro:#################################
########################################################################
#   _________        ____
###| URBANA |#######| 1 |########
#  |_______|########|__|#########
####_________#######_____########
###| RURAL  |#######| 2 |########
#  |_______|########|__|######### 
#################################
##________##          ____ ######
#|FEDERAL|###########| 1 |#######
#|______|############|__|########
##_________            ____  ####
#|ESTADUAL|###########| 2 |######
#|_______|############|__|#######
#################################
##_________             ____  ###
#|MUNICIPAL|###########| 3 |#####
#|________|############|__|######
#################################
##_________             ____  ###
#|PRIVADA |############| 3 |#####
#|________|############|__|######


dfs <- Filter(function(x) is(x, "data.frame"), mget(ls()))

total<-do.call(rbind, dfs)

total_mun <- total %>% 
  filter( dep == "Municipal" | dep ==3)

options(scipen=100000)

total_mun_util_2007 <- total_mun %>%
  filter(year %in% c(1998:2006))%>%
  separate(cod_mun, into = c("init", "mid"), remove = T, sep = 2 ) 

total_mun_util_2007 <- total_mun_util_2007 %>%
  separate(mid, into = c("mid", "end"), remove = T, sep = 5 )

total_mun_util_2007 <- total_mun_util_2007 %>%
  unite(cod_mun, init, end, sep ="")

total_mun_util_2007 <- total_mun_util_2007[-4]

total_mun_util_2018 <- total_mun %>%
  filter(year %in% c(2007:2018))

total_mun_trat <- rbind(total_mun_util_2007,total_mun_util_2018)

total_mun_trat$cod_mun <-as.numeric(total_mun_trat$cod_mun)

total_rural_mun<-total_mun_trat%>%
  filter(loc == 2 | loc == "Rural" )

total_urbana_mun<-total_mun_trat%>%
  filter(loc == 1 | loc == "Urbana" )

total_urbana_mun <- total_urbana_mun[-c(1,4)]

total_rural_mun <- total_rural_mun[-c(1,4)]

total_mun_trat <- total_mun_trat[-c(1,4)]

agreg_total_mun<-total_mun_trat %>%
  group_by(year, cod_mun)%>%
  summarise_all(., sum)

agreg_rural_mun<-total_rural_mun %>%
  group_by(year, cod_mun)%>%
  summarise_all(., sum)

agreg_urbana_mun<-total_urbana_mun %>%
  group_by(year, cod_mun)%>%
  summarise_all(., sum)

write.table(agreg_total_mun, "agreg_total_mun.txt", sep="\t", row.names = F)
write.table(agreg_rural_mun, "agreg_rural_mun.txt", sep="\t", row.names = F)
write.table(agreg_urbana_mun, "agreg_urbana_mun.txt", sep="\t", row.names = F)

require(foreign)
write.dta(agreg_urbana_mun, "agreg_urbana_mun.dta")
write.dta(agreg_rural_mun, "agreg_rural_mun.dta")
write.dta(agreg_total_mun, "agreg_total_mun.dta")


evo_classroom <- function(dataset, cod){
  
  dataset%>%
    filter(cod_mun == cod) %>%
    ggplot(aes(x = year, y=internet)) +
    geom_line()
}


evo_classroom(agreg_urbana_mun, 5107925)
