library(leaflet)
library(shiny)
library(gdata)
library(tidyverse)
library(plotly)

# This file contains the changes made to tidy the data.
pop <- read.csv("/Users/jjc/Documents/Grinnell/CSC324/population_by_country_2020.csv")
pop <- na.omit(pop)

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


# User Interface -----------------------------------------------
ui <- fluidPage(
  titlePanel(strong("World Population in the 21st century")),
  sidebarLayout(
    sidebarPanel(
      h3(strong("All of World population + More in a glance")),
      # Add image
      img(src = "/Users/jjc/Documents/Grinnell/CSC324/usand.jpg", height = 120, width = 120),
      br(),
      # Label each continent as x variable
      selectInput("FY",
                  label = h3("Choose continent(s)"),
                  choices = data$Continent,
                  multiple = TRUE,
                  selected = data$Continent),
      br(),
      sliderInput("Year",
                  label = h3("Choose the year to display"),
                  min = 2000,
                  max = 2018,
                  value = 2014),
      br(),
      numericInput("Rows",
                   label = h3("Choose the number of rows to display"),
                   value = 1,
                   min = 1,
                   max = 10,
                   step = 1)
    
      
    ),
    
    mainPanel(
      # Introduction
      p(h3("Below, you notice a graph that shows each continent's population by country in a chosen year.
                 Feel free to interact with different options you have on the sidebarPanel")),
      h1("Year : ????"),
      # Creates a plot
      plotlyOutput("plot"),
      br(),
      br(),
      h1("Fertility V.S. Density"),
      # Creates a second plot
      plotlyOutput("plot2"),
      br(),
      # Creates a table
      tableOutput("table")
    )
  ))


# Define server logic required to draw a histogram
server <- function(input, output) {

    output$plot <- renderPlotly({
      # Create a barplot with FY for the x axis, and Dollars for the y axis.
      # Display organizations by different colors, and hovering over the graph will show
      # users the Budget Subtitles along with the amount of Dollars separately
      p <- plot_ly(data = data, type = 'bar', x=~Continent, y=~Country, 
                   color = ~Continent,
                   hoverinfo = 'text',
                   text = ~paste("Country:", Country, "<br>",
                                 "Pop:", population)) %>%
        # Change layout to Dollars for the y axis, and group the bars horizontally
        layout(yaxis = list(title='Population'), barmode = 'group')
      
      # Gives an output p    
      p 
    })
    
    }

# Run the application 
shinyApp(ui = ui, server = server)
