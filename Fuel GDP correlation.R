# Load the GDP and Fuel csv ----
# GDP
gdp <- read.csv("GDP EU fix.csv", fileEncoding="UTF-8-BOM", sep = ";", header = TRUE)
GDPfix <- gdp[!rowSums(is.na(gdp[-1])),]
names(GDPfix) <- tolower(names(GDPfix))     # easier to use lowercasing
# Products
library(readxl); library(janitor); library(dplyr); library(here)
oil_prod_raw <- read.csv("Products.csv",fileEncoding="UTF-8-BOM", sep = ";", header = TRUE)
names(oil_prod_raw) <- tolower(names(oil_prod_raw))
oil_prod <- oil_prod_raw %>% remove_empty(c("rows", "cols")) %>% 
  mutate(czech.republic = czechia,
         slovak.republic = slovakia) %>%
  select(-czechia,-slovakia,-latvia)
oil_prod <- rename(oil_prod, year = time)

# Fuel
oil_fuel_raw <- read.csv("Fuel.csv",fileEncoding="UTF-8-BOM", sep = ";", header = TRUE)
names(oil_fuel_raw) <- tolower(names(oil_fuel_raw))
oil_fuel <- oil_fuel_raw %>% remove_empty(c("rows", "cols")) %>% 
  mutate(czech.republic = czechia,
         slovak.republic = slovakia) %>%
  select(-czechia,-slovakia,-latvia)
oil_fuel <- rename(oil_fuel, year = time)


# The dimensions of both df must be the same
oil_fuel <- oil_fuel[oil_fuel$year %in% GDPfix$year,]
oil_prod <- oil_prod[oil_prod$year %in% GDPfix$year,]

# Sort the country data
oil_fuel <- oil_fuel %>%  select(sort(names(.)))
oil_prod <- oil_prod %>%  select(sort(names(.)))
GDPfix <- GDPfix %>%  select(sort(names(.)))


v <- c(names(GDPfix))
gdp.oil.corr <- data.frame(v) # Vector to save the cor data

for (i in 1 : dim(GDPfix)[2]){
    output <- cor(select(GDPfix,i),select(oil_fuel,i))
    gdp.oil.corr[i,2] <- rbind(output)
    i <- i + 1
}

for (n in 1 : dim(GDPfix)[2]){
    output <- cor(select(GDPfix,n),select(oil_prod,n))
    gdp.oil.corr[n,3] <- rbind(output)
    n <- n + 1
}
names(gdp.oil.corr) <- c("country","gdp_fuel","gdp_products")
write.csv(gdp.oil.corr,file= '\\Thesis R\\oild.csv')

