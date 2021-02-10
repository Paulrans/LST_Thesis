library(readxl); library(janitor);  library(here); library(WDI) ; library(plyr); library(dplyr); library(reshape2); 
library(ggplot2); library(gridExtra); library(corrplot); library(RColorBrewer)
worldbank.raw <- read.csv("New files/worldbank.csv", fileEncoding="UTF-8-BOM", sep = ",", header = TRUE)

# All EU countries and EAA 
EU.countries <- c('AT','BE','BG','HR','CZ','DK','CY','EE','FI','FR','DE','GR','HU','IE','IT','LV','LT','IS','LU','MT','NL','PL','PT','RO','SK','ES','SE','NO')

# Loading the WDI data into R
GDP <- WDI(indicator='NY.GDP.PCAP.CN',  EU.countries , start=1960, end=2020)
POP <- WDI(indicator='SP.POP.TOTL', country = EU.countries , start=1960, end=2020)

# Function for correlation by group only 
dfcor <- function(a) {
  return(data.frame(COR = cor(a[,3], a[,7], use = "pairwise.complete.obs")))
}


# Combining the dfs of GDP and Pop and run through correlation function, while returning the correlation dataframe.
comb <-cbind(GDP,POP) 
cormat <- ddply(comb, .(country), dfcor)  
df <- data.frame(cormat) %>%  rename( "POP" = "COR")
row.names(df) <- df$country
df$country <- NULL

# National Debt relative to GDP (%) not a lot of data is known... ----
#debt <- WDI(indicator='GC.DOD.TOTL.GD.ZS', country = EU.countries , start=1960, end=2020)
#comb <-cbind(GDP,debt) 
#cormat <- ddply(comb, .(country), dfcor)
#df$Debt <- cormat$COR

# Export 
EXPT <- WDI(indicator='NE.EXP.GNFS.CD', country = EU.countries , start=1960, end=2020)
comb <-cbind(GDP,EXPT) 
cormat <- ddply(comb, .(country), dfcor)
df$EXPT <- cormat$COR

# Self Employment 
EMP <- WDI(indicator='SL.EMP.MPYR.ZS', country = EU.countries , start=1960, end=2020)
comb <-cbind(GDP,EMP) 
cormat <- ddply(comb, .(country), dfcor)
df$EMP <- cormat$COR

# Argiculture employement
ARG <- WDI(indicator='SL.AGR.EMPL.ZS', country = EU.countries , start=1960, end=2020)
comb <-cbind(GDP,ARG) 
cormat <- ddply(comb, .(country), dfcor)
df$ARG <- cormat$COR

# Industry emp
IND <- WDI(indicator='SL.IND.EMPL.ZS', country = EU.countries , start=1960, end=2020)
comb <-cbind(GDP,IND) 
cormat <- ddply(comb, .(country), dfcor)
df$IND <- cormat$COR

# Services emp
SRV <- WDI(indicator='SL.SRV.EMPL.ZS', country = EU.countries , start=1960, end=2020)
comb <-cbind(GDP,SRV) 
cormat <- ddply(comb, .(country), dfcor)
df$SRV <- cormat$COR

# Net national income NY.ADJ.NNTY.CD Malta is missing ----
NNI <- WDI(indicator='NY.ADJ.NNTY.CD', country = EU.countries , start=1960, end=2020)
comb <-cbind(GDP,NNI) 
cormat <- ddply(comb, .(country), dfcor)
df$NNI <- cormat$COR

# Water withdrawals ER.H2O.FWTL.K3 Not a lot of data but enough for the correlation for all countries ----
#WAT <- WDI(indicator='ER.H2O.FWTL.K3', country = EU.countries , start=1960, end=2020)
#omb <-cbind(GDP,WAT) 
#cormat <- ddply(comb, .(country), dfcor)
#df$WAT <- cormat$COR

# Water Stress ER.H2O.FWST.ZS Only 1 year of data ----
#WTS <- WDI(indicator='ER.H2O.FWST.ZS', country = EU.countries , start=1960, end=2020)
#comb <-cbind(GDP,WTS) 
#cormat <- ddply(comb, .(country), dfcor)
#df$WTS <- cormat$COR

# Renewable water ER.H2O.INTR.PC Few years Almost no data ----
#RWT <- WDI(indicator='ER.H2O.INTR.PC', country = EU.countries , start=1960, end=2020)
#comb <-cbind(GDP,RWT) 
#cormat <- ddply(comb, .(country), dfcor)
#df$RWT <- cormat$COR

# CO2 emissions kt EN.ATM.CO2E.KT
COt <- WDI(indicator='EN.ATM.CO2E.KT', country = EU.countries , start=1960, end=2020)
comb <-cbind(GDP,COt) 
cormat <- ddply(comb, .(country), dfcor)
df$COt <- cormat$COR

#  SP.DYN.CBRT.IN	Birth rate crude
BRT <- WDI(indicator='SP.DYN.CBRT.IN', country = EU.countries , start=1960, end=2020)
comb <-cbind(GDP,BRT) 
cormat <- ddply(comb, .(country), dfcor)
df$BRT <- cormat$COR

# Import BM.GSR.GNFS.CD
IMPT <- WDI(indicator='BM.GSR.GNFS.CD', country = EU.countries , start=1999, end=2020)
comb <-cbind(GDP,IMPT) 
cormat <- ddply(comb, .(country), dfcor)
df$IMPT <- cormat$COR

# Manufacturing value added NV.IND.MANF.CD ----
# NEEDS TO BE ADDED BY HAND TO EXCEL FILE
#VADD <- WDI(indicator='NV.IND.MANF.CD', country = EU.countries , start=1999, end=2020)
#comb <-cbind(GDP,VADD) 
#cormat <- ddply(comb, .(country), dfcor)
#df$VADD <- cormat$COR

# Total greenhouse gas emissions EN.ATM.GHGT.KT.CE	Total greenhouse gas emissions (kt of CO2 equivalent)
TGHG <- WDI(indicator='EN.ATM.GHGT.KT.CE', country = EU.countries , start=1960, end=2020)
comb <-cbind(GDP,TGHG) 
cormat <- ddply(comb, .(country), dfcor)
df$TGHG <- cormat$COR

# Central government consumption/expenditures NE.CON.GOVT.CD	General government final consumption expenditure (current US$)	
CGC <- WDI(indicator='NE.CON.GOVT.CD', country = EU.countries , start=1960, end=2020)
comb <-cbind(GDP,CGC) 
cormat <- ddply(comb, .(country), dfcor)
df$CGC <- cormat$COR

# Plotting heat maps of correlations
dft<- df[-9,] 
dft <- dft[,-2]

map <- read.csv("corr.csv", fileEncoding="UTF-8-BOM", sep = ";", header = TRUE)
row.names(map) <- map$country
map$country <- NULL
heatmap(as.matrix(map),is.na=TRUE)


# Print Excell ----
write.csv(df,file = '\\Thesis R\\Corr.csv')