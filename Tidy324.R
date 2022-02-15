library(gdata)
library(tidyverse)

# This file contains the changes made to tidy the data.

pop <- na.omit(population_by_country_2020)
colnames(pop) <- c("Country", "2020pop", "YearlyChange", "NetChange", "Density", "LandArea", "Migrants", "Fertility", "MedianAge", "Urbanpop", "World Share")

worldpop <- read.csv("/Users/jjc/Documents/Grinnell/CSC324/Worldpopulation.csv")

tidy1 <- rename.vars(worldpop, from = "Country.Name", to = "Country")

tidy2 = merge(x=tidy1,y=pop,by="Country")

# worldpop does not include year 2019, so omitted.
tidy3 <- select(tidy2, -Indicator.Name, -Indicator.Code, -X2019)

tidy4 <- gather(data = tidy3, key = year, value = population, c(starts_with("X")))

NorthAmerica <- filter(tidy4, Country == "United States" | Country == "Canada" | Country == "Mexico")

ggplot(data = NorthAmerica) + 
  geom_point(mapping = aes(x = year, y = population, color = Country))

SouthAmerica <- filter(tidy4, Country == "Brazil" | Country == "Colombia" | Country == "Argentina" | Country == "Peru" | Country == "Chile" | Country == "Ecuador" | Country == "Bolivia" | Country == "Venezuela" | Country == "Uruguay")




