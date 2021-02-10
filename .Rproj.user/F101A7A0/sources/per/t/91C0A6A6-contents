library(eurostat); library(readxl); library(janitor);  library(here); library(WDI) ; library(plyr); library(dplyr); library(reshape2); 
library(ggplot2);

# Eurostat contains a lot of just as the world bank indicators. The right countries 
# need to be picked and the data needs to be filtered in order to get good correlations
#WDI not used anymore  
#WDIgeo <- c('AT','BE','BG','HR','CZ','DK','CY','EE','FI','FR','DE','GR','HU','IE','IT','LV','LT','IS','LU','MT','NL','PL','PT','RO','SK','ES','SE')
#EUROgeo<- c('AT','BE','BG','HR','CZ','DK','CY','EE','FI','FR','DE','EL','HU','IE','IT','LV','LT','IS','LU','MT','NL','PL','PT','RO','SK','ES','SE')
# Iceland data not available in debt so new geo vector
EUROgeodebt<- c('AT','BE','BG','HR','CZ','DK','CY','EE','FI','FR','DE','EL','HU','IE','IT','LV','LT','LU','MT','NL','PL','PT','RO','SK','ES','SE')
nrgbal <- c('AFC','ID','FC','FC_IND_E', 'FC_IND_IS_E','FC_IND_CPC_E', 'FC_IND_NFM_E', 'FC_IND_NMM_E','FC_IND_TE_E', 'FC_IND_MAC_E','FC_IND_MQ_E',
            'FC_IND_PPP_E','FC_IND_WP_E','FC_IND_CON_E','FC_IND_TL_E','FC_TRA_E','FC_OTH_CP_E','FC_OTH_HH_E','FC_OTH_AF_E')
# Invalid filter data -> 'FC_IND_FTB_E',

eurodat <- c("nrg_cb_e")

# EUROSTAT GDP for correct countries and order. 
eurogdp <-  get_eurostat(id = 'nama_10_gdp', time_format = "num",
                        filters = list(geo = EUROgeodebt,
                                na_item = 'B1GQ',
                                unit = 'CP_MEUR',
                                sinceTimePeriod = 1990),
                        select_time = 'Y')
# Function for correlation by group only 
eurostatcor <- function(a,
                        statid) {
  return(data.frame(cor = cor(a[,5], a[,11], use = "pairwise.complete.obs")))
}
# Datarfame for all correlation data
cordf <- data.frame()

# For loop for multiple correlation findings from statid list ----
for (i in 1:length(nrgbal)){
  df <- get_eurostat(eurodat, time_format = "num",
                 filters = list(geo = EUROgeodebt,
                                nrg_bal = nrgbal[i]),
                 select_time = "Y")
  comb <-cbind(eurogdp,df) 
  cormat <- ddply(comb, .(geo), eurostatcor, statid = statid)
  if (i == 1){
    cordf <- data.frame(cormat)
    }else {
    cordf <- cbind(cordf,cormat[,2],deparse.level=0)
    }
}


# New data finances ----
naitem <- c('B9', 'GD')
for (i in 1:length(naitem)){
eurodebt <- get_eurostat(id = 'gov_10dd_edpt1',
                         time_format = "num",
                         filters = list(geo = EUROgeodebt, 
                                        na_item = naitem[i],
                                        sector = 'S13',
                                        unit = 'MIO_EUR'
                                        )
                         )
gdpdebt <-  get_eurostat(id = 'nama_10_gdp', time_format = "num",
                         filters = list(geo = EUROgeodebt,
                                        na_item = 'B1GQ',
                                        unit = 'CP_MEUR',
                                        sinceTimePeriod = 1995),
                         select_time = 'Y')                                        
comb <-cbind(gdpdebt,eurodebt) 
cormat <- ddply(comb, .(geo), eurostatcor, statid = naitem)
cordf <- cbind(cordf,cormat[,2],deparse.level=0)
}

#BEC product SITC product groups ----
# Function for correlation by group only 
eurobeccor <- function(a,statid) {
  return(data.frame(cor = cor(a[,5], a[,9], use = "pairwise.complete.obs")))
}

stk_flow <- c('IMP','EXP')
for (l in 1:length(stk_flow)){
europrod <- get_eurostat(id = 'ext_st_27_2020msbec',
                         time_format = 'num',
                         filters = list(geo = EUROgeodebt,
                                        indic_et = 'TRD_VAL',
                                        stk_flow = stk_flow[l],
                                        partner = 'EU27_2020',
                                        bclas_bec = 'TOTAL'
                                        ),
                         sinceTimePeriod = 1999
                         )
europrod <- europrod[!europrod$time >= 2020,]
gdpbec <-  get_eurostat(id = 'nama_10_gdp', time_format = "num",
                         filters = list(geo = EUROgeodebt,
                                        na_item = 'B1GQ',
                                        unit = 'CP_MEUR',
                                        sinceTimePeriod = 1999),
                         select_time = 'Y')
#construct a new dataframe with cumulative annual data
bec <- data.frame()
for (i in seq(1, dim(europrod)[1],12)){
  df <- data.frame(europrod[i,c(1,5)],floor(europrod[i,6]),sum(europrod[i:i+11,7]))
  bec <- rbind(bec,df)
}
comb <-cbind(gdpbec,bec) 
cormat <- ddply(comb, .(geo), eurobeccor, statid = naitem)
cordf <- cbind(cordf,cormat[,2],deparse.level=0)
}

# SITC data from Eurostat Production groups BEC
sitc <- c('TOTAL','SITC0_1','SITC2_4','SITC3','SITC5','SITC6_8','SITC7','SITC9')
indic_et <- c('MIO_EXP_VAL', 'MIO_IMP_VAL')

for (i in 1:length(indic_et)){
  for (l in 1:length(sitc)){
    eurositc <- get_eurostat('ext_lt_intertrd',
                         time_format = "num",
                         filters = list(geo = EUROgeodebt,
                                        indic_et = indic_et[i],
                                        partner = 'WORLD',
                                        sitc06 = sitc[l]
                                        ),
                         select_time = 'Y')
    sitcgdp <- get_eurostat(id = 'nama_10_gdp', time_format = "num",
                            filters = list(geo = EUROgeodebt,
                                           na_item = 'B1GQ',
                                           unit = 'CP_MEUR',
                                           sinceTimePeriod = 2002),
                            select_time = 'Y')
    comb <-cbind(sitcgdp,eurositc) 
    cormat <- ddply(comb, .(geo), eurostatcor, statid = naitem)
    cordf <- cbind(cordf,cormat[,2],deparse.level=0)
  }}

## Correlation names----
colnames(cordf) <- c('geo',as.character(nrgbal),paste(as.character(naitem),'debt',sep = ' '),as.character(stk_flow),paste(sitc,'EXP',sep = ' '),paste(sitc,'IMP',sep = ' '))
rownames(cordf) <- cordf[,1]
cordf[,1] <- NULL
heatmap(as.matrix(cordf),is.na=TRUE)

