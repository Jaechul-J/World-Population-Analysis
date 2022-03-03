library(gdata)
library(tidyverse)

# This file contains the changes made to tidy the data.
pop <- read.csv("/Users/jjc/Documents/Grinnell/CSC324/population_by_country_2020.csv")
pop <- na.omit(population_by_country_2020)

# Dataset that contains continents
cbc <- read.csv("/Users/jjc/Documents/Grinnell/CSC324/ContinentsByCountry.csv")
cbc <- select(cbc, Country.Name, Continent)
cbc <- rename.vars(cbc, from = "Country.Name", to = "Country")

cbc[222, "Country"] <- "United States"
cbc[108, "Country"] <- "North Korea"
cbc[109, "Country"] <- "South Korea"

colnames(pop) <- c("Country", "2020pop", "YearlyChange", "NetChange", "Density", "LandArea", "Migrants", "Fertility", "MedianAge", "Urbanpop", "World Share")

worldpop <- read.csv("/Users/jjc/Documents/Grinnell/CSC324/Worldpopulation.csv")

tidy1 <- rename.vars(worldpop, from = "Country.Name", to = "Country")

tidy1[125, "Country"] <- "South Korea"
tidy1[192, "Country"] <- "North Korea"

tidy2 <- merge(x=tidy1,y=pop,by="Country")
tidy3 <- merge(x=tidy2,y=cbc,by="Country")

# worldpop does not include year 2019, so omitted.
tidy3 <- select(tidy3, -Indicator.Name, -Indicator.Code, -X2019)

tidy4 <- gather(data = tidy3, key = year, value = population, c(starts_with("X")))

data <- filter(tidy4, year == "X2010" | year == "X2011" | year == "X2012" | year == "X2013" | year == "X2014" | year == "X2015" | year == "X2016" | year == "X2017" | year == "X2018")
data <- data %>%
  mutate(across(where(is.character) & c(Urbanpop, year, YearlyChange), parse_number))
#ggplot(data, aes(x=Urbanpop, y=Fertility, size = population)) +
 # geom_point(alpha=0.7)