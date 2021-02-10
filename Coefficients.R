library(eurostat); library(readxl); library(janitor);  library(here); library(WDI) ; library(plyr); library(dplyr); library(reshape2); 
library(ggplot2); library(WDI)

# Countries and Variables ----
WDIgeo <- c('AT','BE','BG','HR','CZ','DK','CY','EE','FI','FR','DE','GR','HU','IE','IT','LV','LT','LU','MT','NL','PL','PT','RO','SK','ES','SE')
WDIstat <- c('NY.GDP.PCAP.CN','SP.POP.TOTL','NE.EXP.GNFS.CD','BM.GSR.GNFS.CD','SL.EMP.MPYR.ZS','SL.AGR.EMPL.ZS','SL.IND.EMPL.ZS','SL.SRV.EMPL.ZS','NY.ADJ.NNTY.CD',
             'EN.ATM.CO2E.KT','SP.DYN.CBRT.IN','EN.ATM.GHGT.KT.CE','NE.CON.GOVT.CD')
WDIname <- c('GDP','POP','EXP','IMP','E_Self','E_ARG','E_IND','E_SERV','NNI','CO2t','BIRTH','TGHG','Gov_Cons')

# EUROSTAT data
EUROgeo<- c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "ES", "FI", "FR", "EL", "HR", "HU", "IE", "IT", "LT", "LU", "LV", "MT", "NL", "PL", "PT", "RO", "SE", "SK")
OIL <- c('')

nrgbal <- c('AFC','ID','FC','FC_IND_E', 'FC_IND_IS_E','FC_IND_CPC_E', 'FC_IND_NFM_E', 'FC_IND_NMM_E','FC_IND_TE_E', 'FC_IND_MAC_E','FC_IND_MQ_E',
            'FC_IND_PPP_E','FC_IND_WP_E','FC_IND_CON_E','FC_IND_TL_E','FC_TRA_E','FC_OTH_CP_E','FC_OTH_HH_E','FC_OTH_AF_E')


# Combining the data in one df ----
# WDI data
df <- NULL
for (i in 1:length(WDIstat)){
WDIdat <- WDI(indicator=WDIstat[i], country = WDIgeo , start=1999, end=2019)
if(i == 1){
  df <- data.frame(WDIdat[,c(1,2,4,3)])
} else {
df <- cbind(df,WDIdat[,3])
}
}

# Raw/cleaned Oil data from 

# First variable for testing
ENRG <- get_eurostat(eurodat, time_format = "num",
                   filters = list(geo = EUROgeodebt,
                                  nrg_bal = nrgbal[1],
                                  sinceTimePeriod = 1999),
                   select_time = "Y")


# Data normalization ----
# Normalizing the data over all years and all countries at once
ENRG.norm <- ENRG
for (i in 1:dim(ENRG)[1]){
  ENRG.norm$values[i] <- 2* (ENRG$values[i]-min(ENRG$values))/(max(ENRG$values)-min(ENRG$values))-1
}
df <- as.data.frame(cbind(eurogdp,nrgbal[l] =c(ENRG.norm$values)))
eurogdp_norm <- eurogdp
for (i in 1:dim(eurogdp)[1]){
  eurogdp_norm$values[i] <- 2* (eurogdp$values[i]-min(eurogdp$values, na.rm = TRUE))/(max(eurogdp$values, na.rm = TRUE)-min(eurogdp$values, na.rm = TRUE))-1
}
# ANOVA ----