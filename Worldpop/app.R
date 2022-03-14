library(leaflet)
library(shiny)
library(gdata)
library(tidyverse)
library(plotly)
library(wordcloud2)
library(hwordcloud)
library(googleVis)

# Read in dataset
pop <- read.csv("/Users/jjc/Documents/Grinnell/CSC324/population_by_country_2020.csv")
# Remove any NA values
pop <- na.omit(pop)

# Read in regular dataset
cbc <- read.csv("/Users/jjc/Documents/Grinnell/CSC324/ContinentsByCountry.csv")
# Choose variables of interest from cbc
cbc <- select(cbc, Country.Name, Continent)
# Rename variable name to merge later with another data
cbc <- rename.vars(cbc, from = "Country.Name", to = "Country")

# Unify the country name for merge
cbc[222, "Country"] <- "United States"
cbc[108, "Country"] <- "North Korea"
cbc[109, "Country"] <- "South Korea"

# Adjust columns names so it is readable
colnames(pop) <- c("Country", "2020pop", "YearlyChange", "NetChange", "Density", "LandArea", "Migrants", "Fertility", "MedianAge", "Urbanpop", "World Share")

# Read in dataset with continents.
worldpop <- read.csv("/Users/jjc/Documents/Grinnell/CSC324/Worldpopulation.csv")

# Rename variable name to merge
tidy1 <- rename.vars(worldpop, from = "Country.Name", to = "Country")

# Unify the country name for merge
tidy1[125, "Country"] <- "South Korea"
tidy1[192, "Country"] <- "North Korea"
tidy1[201, "Country"] <- "Russia"

# Merge 3 different datasets.
tidy2 <- merge(x=tidy1,y=pop,by="Country")
tidy3 <- merge(x=tidy2,y=cbc,by="Country")

# For plot2
tidyy <- gather(data = tidy3, key = year, value = population, c(starts_with("X")))

# Change for plot 2
tidyy <- tidyy %>%
  mutate(across(where(is.character) & c(Urbanpop, year, YearlyChange), parse_number))

# worldpop does not include year 2019, so omitted. Exclude years before 21st century.
tidy4 <- select(tidy3, -Indicator.Name, -Indicator.Code, -X1960, -X1961, -X1962, -X1963, -X1964, -X1965, -X1966, -X1967, -X1968, -X1969, -X1970, -X1971, -X1972, -X1973, -X1974, -X1975, -X1976, -X1977, -X1978, -X1979, -X1980, -X1981, -X1982, -X1983, -X1984, -X1985, -X1986, -X1987, -X1988, -X1989, -X1990, -X1991, -X1992, -X1993, -X1994, -X1995, -X1996, -X1997, -X1998, -X1999)

# Gather year column into one with years as variable.
tidy4 <- gather(data = tidy4, key = year, value = population, c(starts_with("X")))

# Filter data so that it only contains years of interest.
data <- filter(tidy4, year == "X2000" | year == "X2001" | year == "X2002" | year == "X2003" | year == "X2004" | year == "X2005" | year == "X2006" | year == "X2007" | year == "X2008" | year == "X2009" | year == "X2010" | year == "X2011" | year == "X2012" | year == "X2013" | year == "X2014" | year == "X2015" | year == "X2016" | year == "X2017" | year == "X2018" | year == "X2019")
# Parse number for each cell
data <- data %>%
  mutate(across(where(is.character) & c(Urbanpop, year, YearlyChange), parse_number))

# Delete population for 2019 and 2020.
data <- select(data, -"2020pop")
data <- data[-c(2946:3100), ]

# Make numeric so it shows on plot
data$Fertility <- as.numeric(data$Fertility)
data$MedianAge <- as.numeric(data$MedianAge)

# Confirm there's no NA value
data <- na.omit(data)

# User Interface -----------------------------------------------
ui <- fluidPage(
  titlePanel(strong("World Population in the 21st century")),
  sidebarLayout(
    sidebarPanel(
      # Label each continent as x variable
      selectizeInput("Continents",
                  label = h3("Choose continent(s)"),
                  choices = data$Continent,
                  multiple = TRUE,
                  selected = data$Continent),
      br(),
      sliderInput("Year",
                  label = h3("Choose the year to display"),
                  min = min(data$year),
                  max = 2018,
                  value = 2010),
      br()
    ),
    
    
    mainPanel(
      
      
      # Output: Tabset with plots and map
      tabsetPanel(type = "tabs",
                  tabPanel("Population", plotlyOutput("plot"), br(), plotlyOutput("plot2")),
                  tabPanel("Fertility VS. Urban Pop(%)", br(), plotlyOutput("ploty")),
                  tabPanel("Migration", br(), selectizeInput(inputId = "Migration_ex",
                                                              label = "Select Year",
                                                              choices = unique(data$year),
                                                              selected=2018) , plotlyOutput("Migration"), br(), plotlyOutput("plot3"), br()),
                  tabPanel("NetChange", titlePanel("NetChange by Country"), htmlOutput("NetChange"))
      )
      
    )
  )
)


# Server Function -----------------------------------------------
server <- function(input, output) {
  
  # World pop
  output$plot <- renderPlotly({
    # Subsetting inputs into each Variable in the data
    data1 <- subset(data, year == input$Year)
    data1 <- subset(data1, Continent %in% input$Continents)
    # Create a barplot with Continents for the x axis, and Population for the y axis.
    # Display continents by different colors, and hovering over the graph will show
    # users the Country along with the number of population at a selected year.
    p <- plot_ly(data = data1, type = 'bar', x=~Continent, y=~population, 
                 color = ~Country,
                 hoverinfo = 'text',
                 text = ~paste("Country:", Country, "<br>",
                               "Pop:", population)) %>%
      # Change layout to Population for the y axis, and group the bars horizontally
      layout(yaxis = list(title='Population'), barmode = 'group')
    
    # Gives an output p 
    p
  })
  
  # World pop 2
  output$plot2 <- renderPlotly({
    # Subsetting inputs into each Variable in the data
    tidyy <- subset(tidyy, Continent %in% input$Continents)
    # Create a regression line with x axis as years (1960-2018), and Population for the y axis.
    # Display continents by different colors, and hovering over the graph will show
    # users the Country along with the number of population at a given year.
    ggplot(tidyy, aes(x = year, y = population, color = Continent)) + 
      geom_smooth() + 
      labs(x="Year", y="Population", fill="Continent"
           , title="Population Change")
  })
  
  output$ploty <- renderPlotly({
    # Subsetting inputs into each Variable in the data
    data1 <- subset(data, year == input$Year)
    print("123")
    data1 <- subset(data1, Continent %in% input$Continents)
    # Create a scatterplot with Fertility for the x axis, and percentage of urban population for the y axis.
    # Display continents by different colors, and hovering over the graph will show
    # users the Country.
    q <- plot_ly(data = data1, type = 'scatter', x=~Fertility, y=~Urbanpop,
                 color = ~Continent,
                 hoverinfo = 'text',
                 text = ~paste("Country:", Country))
    # Gives an output q
    q
  })
  
  # Migration
  output$Migration <- renderPlotly({
    # Subsetting inputs into each Variable in the data
    data1 <- subset(data, year == input$Migration_ex)
    data1 <- subset(data1, Continent %in% input$Continents)
    
    # Creates a boxplot of number of immigrants from each continent.
    ggplot(data1 %>% filter(year == input$Migration_ex)
           , aes(x = Continent, y = Migrants, fill = Continent)) + 
      geom_boxplot() + 
      labs(x="Continent", y="Migration", fill="Continent"
           , title="Migration by Continent") +
      scale_fill_brewer(palette="YlGnBu") +
      theme(plot.title = element_text(face="bold", hjust = 0.55)
            , axis.title=element_text(size=10, face="bold")
            , legend.position="bottom")
  })
  
  # Migration 2
  output$plot3 <- renderPlotly({
    # Subsetting inputs into each Variable in the data
    data1 <- subset(data, year == input$Year)
    tidyy <- subset(data1, Continent %in% input$Continents)
    # Create a scatter plot with number of migrants for the x axis, and population for the y axis.
    # Display continents by different colors, and hovering over the graph will show
    # users the Country along with the number of migrants and population at a selected year.
    ggplot(tidyy, aes(x = Migrants, y = population, color = Continent, text = paste("Country: ", Country))) + 
      geom_point() + 
      labs(x="Migrants", y="Population", fill="Continent"
           , title="Population Change over Migrants")
  })
  
  # Land Area
  output$NetChange <- renderGvis(
    # Used googlevis to display world map with the biggest 
    # Darker blue means bigger land area and vice versa
    gvisGeoChart(data
                 , "Country", "NetChange"
                 , options=list(width=500, height=400
                                , colorAxis="{colors:['#ffe9ec', 'blue']}"
                 ))
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
