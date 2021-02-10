# Opening multiple packages DOESNOT work yet ----
x <- c("plyr","reshape2","ggplot2","magrittr")
require(x)
lapply(x, require, character.only = TRUE)
do.call("require", list(x))

# Open GDP files
gdp <- read.csv("GDP EU fix.csv", fileEncoding="UTF-8-BOM", sep = ";", header = TRUE)
GDPfix <- gdp[!rowSums(is.na(gdp[-1])),]

# Reshape the data to long format ----
names(GDPfix) <- tolower(names(GDPfix))     # easier to use lowercasing
GDPfix$type <- "GDP"                        # Assign a type to the data
GDPfix$european.union <- NULL               # take out EU for the plot

LGDP <- melt(GDPfix, id.vars = c("year"))
ggplot() +
  geom_line(data = LGDP, aes(x = year, y = value, group = variable, color = variable))

# Correlations between GDPs of countries ----

cGDPfix <- cor(GDPfix[,2:length(GDPfix)])
mcgdp <- melt(cGDPfix)
ggplot(data = mcgdp,
       aes(x = Var1,
           y = Var2,
           fill = value)) + geom_tile()
# Upper and lower Triangle correlations matrix functions ----

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cGDPfix){
  cGDPfix[upper.tri(cGDPfix)] <- NA
  return(cGDPfix)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cGDPfix){
  cGDPfix[lower.tri(cGDPfix)]<- NA
  return(cGDPfix)
}

#Change the uppertriangle to NA ----
upper_tri <- get_upper_tri(cGDPfix)
upper_tri
melted_cormat <- melt(upper_tri, na.rm = TRUE)

# Heatmap with a range from 0.5 to 1 correlation ---- 
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0.75, limit = c(0.5,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

# Oil demand in Europe ----

# DATA cleaner 
library(readxl); library(janitor); library(dplyr); library(here)
oil_prod_raw <- read_excel("oil.xlsx",sheet = 1, .name_repair = make_clean_names)
oil_prod <- oil_prod_raw %>% remove_empty(c("rows", "cols")) %>% 
  mutate(european.union = European.Union,
         czech.republic = czechia,
         slovak.republic = slovakia) %>%
  select(-european_union_27_countries_from_2020, 
         -euro_area_19_countries_from_2015,
         -kosovo_under_united_nations_security_council_resolution_1244_99
         ,-moldova,-turkey,
         -bosnia_and_herzegovina,
         -serbia,-albania,-montenegro,
         -moldova,-czechia,
         -european_union_28_countries_2013_2020,
         -germany_until_1990_former_territory_of_the_frg,
         -slovakia,
         -latvia,
         -georgia,
         -united_kingdom,
         -north_macedonia)
oil_prod <- rename(oil_prod,year = time)
oil_prod$type <- "Oil products"

# order the names of the countries without Plyr
oil_prod <- oil_prod[ ,order(names(oil_prod))]

oil_fuel_raw <- read_excel("oil.xlsx",sheet = 2, .name_repair = make_clean_names)
# Changing the data to the countries in both the OIL df need to be the same. 
# Changing the european_union_28_countries_2013_2020 to european.union to compare to the GDP and take out the others
oil_fuel <- oil_fuel_raw %>% remove_empty(c("rows", "cols")) %>% 
  mutate(european.union = european_union_28_countries_2013_2020,
         germany = germany_until_1990_former_territory_of_the_frg,
         czech.republic = czechia,
         slovak.republic = slovakia) %>%
  select(-european_union_27_countries_from_2020, 
         -euro_area_19_countries_from_2015,
         -kosovo_under_united_nations_security_council_resolution_1244_99
         ,-moldova,-turkey,
         -bosnia_and_herzegovina,
         -serbia,-albania,-montenegro,
         -moldova,-czechia,
         -european_union_28_countries_2013_2020,
         -germany_until_1990_former_territory_of_the_frg,
         -slovakia,
         -latvia,
         -georgia,
         -united_kingdom,
         -north_macedonia,
         -liechtenstein) 
oil_fuel <- rename(oil_fuel,year = time)
oil_fuel$type <- "Fuel consumption"

# order the names of the countries with Plyr
oil_fuel <- oil_fuel %>%  select(sort(names(.)))

# Plot
library(reshape2); library(ggplot2) ; library(dplyr)
oil_long <- melt(oil_fuel, id.vars = c("time"))
ggplot(data = oil_long, aes(x = time, y = value, group = variable, color = variable))+ geom_smooth()


# Correlation with GDP

gdp_fuel <- vector("list",dim(GDPfix)[1]-1) # Vector to save the cor data
for (i in 1 : dim(GDPfix)[2]){
  i <- i + 1
  cor(select(GDPfix,i),select(oil_prod,i))
  gdp_fuel[[i]] <- cor(GDPfix$austria,oil_fuel$austria)
}
gdp_fuel[[i]] <- cor(GDPfix$austria,oil_fuel$austria)


# For a correlation 
for (i in 2:32) {
  gdp_fuel[[i]] <- cor(c(GDPfix[ ,i],oil_fuel[ ,i]))
}


