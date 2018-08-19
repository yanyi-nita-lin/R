library(shinythemes)
library(ggmap)
library(shiny)
library(leaflet)
library(readxl)
library(ggplot2)
library(dplyr)
library(jsonlite)
library(plotly)
library(tm)
library(SnowballC)
library(wordcloud)
library(stopwords)

# Load the data from excel file
data_name <- read_xlsx("dataset.xlsx", sheet = "name")
data_seats <- read_xlsx("dataset.xlsx", sheet = "seatType")
data_amount <- read_xlsx("dataset.xlsx", sheet = "amount")
data_compare <- read_xlsx("dataset-02.xlsx")
data_brand <- read_xlsx("dataset.xlsx", sheet = "name") [,c('year', 'area', 'tradingName')]
data_migrant <- read_xlsx("dataset-03.xlsx")

# Layout
ui <- shinyUI(
        # Navigation bar style
        navbarPage(theme = shinytheme("united"), "Café and Restaurant in Melmourne",
          
          # Layout for tab1: About         
          tabPanel("ABOUT",
            titlePanel("About This Application"),
              fluidPage(
                fluidRow( 
                  h5("*Please view on web browser")
                ),
                fluidRow( 
                  h3("For Who You Are", style = "background-color: #FFE4B5")
                ),
                fluidRow(
                  h4("• Inhabitants: The residents or people who live in Melbourne."),
                  h4("• Tourists: Short-term travellers in Melbourne."),
                  h4("• Business People: The people who plan to start up a café or restaurant in Melbourne.")
                ),
                fluidRow(
                  h3("What You Can Discover", style = "background-color: #FFE4B5")
                ),
                fluidRow(
                  h4("You can find out the following information from the visualisations:"),
                  h4("• The distribution of café and restaurant in certain area by type, year, or keywords."),
                  h4("• The relationship among the number of cafe and restaurants, median age, personal weekly income and residents from 2011 to 2016. You can easily compare all areas in one plot."),
                  h4("• The changing of seat type, the number of cafe and restaurants, and the names are mostly used from 2002 to 2016 by area."),
                  h4("• The changing of the names of the cafe and restaurants, which can be cmpared with the number of migrants from different countries.")
                ),
                fluidRow(
                  h3("How To Use This Application", style = "background-color: #FFE4B5")
                  ),
                fluidRow(
                  h4("• Go to the different page by clicking the tab on the navigation bar."),
                  h4("• Choose the area, year, or input keywords on the left panel to filter the data."),
                  h4("• The plots will be shown on the right panel. You can see more detail information by clicking the circle and spots or hovering the graph.")
                ),
                fluidRow(
                  h3("Data Source", style = "background-color: #FFE4B5")
                ),
                fluidRow(
                  h4("• ", a(href="http://www.melbourne.vic.gov.au/about-melbourne/research-and-statistics/city-economy/census-land-use-employment/Pages/clue-small-area-and-block-maps.aspx", "CLUE small area and block maps")),
                  h4("• ", a(href="https://data.melbourne.vic.gov.au/Economy/Cafes-and-restaurants-with-seating-capacity/xt2y-tnn9", "Cafes and restaurants, with seating capacity")),
                  h4("• ", a(href="http://www.melbourne.vic.gov.au/about-melbourne/research-and-statistics/pages/city-residents.aspx", "City of Melbourne 2016 demographic profiles")),
                  h4("• ", a(href="http://www.censusdata.abs.gov.au/census_services/getproduct/census/2011/quickstat/2GMEL?opendocument", "2011 Census QuickStats")),
                  h4("• ", a(href="http://www.censusdata.abs.gov.au/census_services/getproduct/census/2016/quickstat/SSC22095?opendocument", "2016 Census QuickStats")),
                  h4("• ", a(href="https://www.homeaffairs.gov.au/about/reports-publications/research-statistics/statistics/live-in-australia/historical-migration-statistics", "Historical migration statistics"))
                )
              )
          ),
                   
          # Layout for tab2: distribution         
          tabPanel("DISTRIBUTION",
            titlePanel("Café and Restaurant in the City of Melmourne"),
              fluidPage(
                fluidRow(
                  column(3,
                    wellPanel(
                      h4("Filter"),     
                      helpText("Choose one type and a year to navigate the distribution of Café and Restaurant in Melbourne. 
                               Also, you can input the keyword for the names to filter the result."),
                      hr(),
                      selectInput('type', 'Type:', c("All", unique(as.character(data_name$industryDescription))), selected = "All"),
                      hr(),
                      sliderInput('year', 'Year:', min = 2002, max = 2016, value = 1),
                      hr(),
                      textInput("name", "Café and restaurant names contains (e.g. Japanese):", value = ""),
                      hr(),
                      helpText("Note: The areas are categorised by CLUE (Census of Land Use and Employment) small area.")
                    )
                  ),
                  column(9,
                    leafletOutput("mymap",height = 800)
                  )
                )
              )
            ),
          
            # Layout for tab3: comparison                  
            tabPanel("COMPARISON",
              titlePanel("Relationship Among Dimensions from 2011 to 2016"),
                fluidPage(
                  fluidRow(
                    column(3,
                      wellPanel(
                        h4("Dimesion"),     
                        helpText("Choose one or more factors to see the relationship with the number of café and 
                                 restaurant between 2011 and 2016."),
                        hr(),
                        checkboxInput("age", "Median Age", FALSE),
                        hr(),
                        checkboxInput("income", "Pesonal Weekly Income", FALSE),
                        hr(),
                        checkboxInput("resident", "Residents", FALSE),
                        hr(),
                        helpText("Note: The areas are categorised by CLUE (Census of Land Use and Employment) small area.")
                      )
                    ),
                    column(9,
                      plotlyOutput("plot1", height = 800)
                    )
                  )
                )
              ),

              # Layout for tab4: changing by area 
              tabPanel("CHANGING",
                titlePanel("Changing of Café and Restaurant by Area"),
                  fluidPage(
                    fluidRow(
                      column(3,
                        wellPanel(
                          h4("Filter"),
                          helpText("Choose an area to see the changing over years."),
                          hr(),
                          selectInput("area", "Area:", c(unique(as.character(data_seats$area)))),
                          hr(),
                          helpText("Note: The areas are categorised by CLUE (Census of Land Use and Employment) small area.")
                        )
                      ),
                      column(5,
                        h4("Changing of Type of Seat"),
                        plotlyOutput("plot2", height = 450),
                        hr()
                      ),
                      column(4,
                        h4("Changing of the Number of Café and Restaurant"),
                        plotlyOutput("plot3", height = 450),
                        hr()
                      )
                    ),
                    fluidRow(
                      column(3,
                        wellPanel(
                          h3(textOutput("caption")),
                          hr(),
                          h4("Filter"),
                          helpText("Select a specific year for the area you selected to see the restaurant names that mostly used."),
                          hr(),
                          sliderInput('year2', 'Year', min = 2002, max = 2016, value = 1)
                        )
                      ),
                      column(5,
                        h4("Word Cloud for Trading Names"),
                        plotOutput("plot4", height = 450)
                      ),
                      column(4,
                        h4("Top 20 Migrants in Australia"),
                        helpText("Note: The data for 2015 & 2016 is unavailable."),
                        plotOutput("plot5", height = 450)
                      )
                    )
                  )
              )
        )
      )
              
server <- function(input, output) {
  # Caption for the tab 4 when select a certain area
  output$caption <- reactiveText(function(){
    paste("You have selected the area of", input$area)
  })
  
  # Plot for tab2: distriution
  # Map: Distribition of cafe and restaurants
  output$mymap <- renderLeaflet({
    data_name2 <- data_name %>% 
      filter(year == input$year)
    
    if(input$type != "All") {
      data_name2 <- data_name2[data_name2$industryDescription == input$type, ]
    }
    
    if (!input$name == "") {
      data_name2 <- data_name2[grep(input$name, data_name2$tradingName, ignore.case = T), ]
    }
      
    geojson <- readLines("area.geojson", warn = FALSE) %>%
      paste(collapse = "\n") %>%
      fromJSON(simplifyVector = FALSE)
    
    if (nrow(data_name2) != 0) {
      geojson$style = list(
        weight = 1,
        color ="gray",
        opacity = 1,
        fill = TRUE,
        fillOpacity = 0.5
      )
      
      pal <- substr(rainbow(34), 1, 7)
      i <- 0
      geojson$features <- lapply(geojson$features, function(feat){
        i <<- i+1
        feat$properties$style <- list(
          fillColor = pal[i]
        )
        feat
      })
    
      content <- paste(data_name2$tradingName,
                       '@', data_name2$address)
      leaflet(data_name2) %>% 
        addTiles() %>%
          addMarkers(~latitude, ~longtitude, label = content, 
                     labelOptions = labelOptions(noHide = F,direction = 'auto', 
                                                 style = list(
                                                              "color" = "brown",
                                                              "font-family" = "sans-serif",
                                                              "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                                              "font-size" = "15px",
                                                              "border-color" = "rgba(0,0,0,0.5)"
                                                              )
                                                ), 
                     clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F)) %>%
            addProviderTiles(providers$Esri.WorldStreetMap) %>%
              addMiniMap(toggleDisplay = TRUE) %>% 
                addGeoJSON(geojson) 
    } 
    else {
      leaflet() %>% 
        addProviderTiles(providers$Esri.WorldStreetMap) %>%
          setView(lat = -37.8102, lng = 144.9628 , zoom = 13) %>%
            addMiniMap(toggleDisplay = TRUE) %>% 
              addGeoJSON(geojson)  
    }
  })
    
  # Plot for tab3: comparison
  # Lie chart: compare among areas from different dimensions
  output$plot1 <- renderPlotly({
    p <- ggplot(data_compare, aes(x = year)) + 
           geom_line(aes(y = as.numeric(amount), color = "Number of Café and Restaurant")) +
           facet_wrap(~area) +
           scale_x_continuous("Year") + scale_y_continuous("Number") +
           theme(strip.text.x = element_text(size = 10, angle = 0), legend.title = element_blank(),
                 strip.background = element_rect(colour = "grey", fill = "#FFE4B5"))
         ggplotly(p, tooltip = c("x", "y", "label"))
    
    if (input$age)
      p <- p + geom_line(aes(y=as.numeric(age), color="Median Age"))
      ggplotly(p, tooltip=c("x", "y", "label"))
    
    if (input$income)
      p <- p + geom_line(aes(y = as.numeric(income), color = "Personal Weekly Income"))
      ggplotly(p, tooltip=c("x","y","label"))
    
    if (input$resident)
      p <- p + geom_line(aes(y = as.numeric(resident), color = "Number of Residents"))
      ggplotly(p, tooltip=c("x","y","label"))
  })
  
  # Plot for tab4: changing
  
  # Stack bar chart: changing of the seat type
  output$plot2 <- renderPlotly({
    if(input$area != "") {
      data_seats2 <- data_seats %>%
        filter(area == input$area)
      }
    else{
      data_seats2 <- data_seats
      }

    p <- ggplot(data_seats2, aes(x = year)) + 
      geom_bar(aes(y = seats, fill = seatType), stat="identity") +
      theme(legend.position = "top", 
            legend.direction = "horizontal",
            legend.title = element_blank())
    
    ggplotly(p, tooltip = c("x","y","label")) %>% 
      layout(height = input$area, autosize = TRUE)
  })
 
  # Line chart: changing of the number of cafe and restaurants
  output$plot3 <- renderPlotly({
    if(input$area != "") {
      data_amount2 <- data_amount %>%
        filter(area == input$area)
    } else{
        data_amount2 <- data_amount
        }
    
    p <- ggplot(data_amount2, aes(year, amount, label = amount)) + 
      geom_line(color = "red") + geom_point()
    ggplotly(p, tooltip = c("x","label")) %>% 
      layout(height = input$area, autosize=TRUE)
  })
  
  # Wordcloud: for the names of cafe and restaurants
  output$plot4 <- reactivePlot(function(){
    if(input$year2 != "" && input$area != "") {
      data_brand2 <- data_brand %>%
        filter(year == input$year2, area == input$area) %>% 
        select("tradingName")
    }

    write.table(data_brand2, "./corpus/mydata.txt", sep="\t")
    
    file <- file.path(".","corpus")
    brands <- Corpus(DirSource(file))
    
    brands <- tm_map(brands, stripWhitespace)
    brands <- tm_map(brands, tolower)
    banned <- c("bar", "the", "cafe", "restaurant", "melbourne", "melbourn")
    brands <- tm_map(brands, removeWords, c(stopwords("english"), banned))
    brands <- tm_map(brands, stemDocument)
    
    wordcloud(brands, scale=c(5,0.5), max.words=200, random.order=FALSE, 
              rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
  })
  
  # Lollipop chart: Top 20 countries where migrants are originally from
  output$plot5 <- reactivePlot(function(){
    if(input$year2 != "2015" && input$year2 != "2016") {
      data_migrant2 <- data_migrant %>%
        select(name, as.character(input$year2))
    
      colnames(data_migrant2) <- c("country", "amount")
      data_migrant2 <- data_migrant2[order(-rank(data_migrant2$amount)), ] 
      data_migrant2 = subset(data_migrant2, country %in% country [1:20])
      data_migrant2$country <- factor(data_migrant2$country, levels = unique(data_migrant2$country[order(data_migrant2$amount, decreasing = FALSE)]))
    
      ggplot(data_migrant2, aes(x = country, y = amount, label = amount)) +
        geom_point(stat = 'identity', fill = "black", size = 10) +
        geom_segment(aes(y = 0, 
                         x = country, 
                         yend = amount, 
                         xend = country), 
                         color = "black") +
          geom_text(color="white", size = 3) +
          labs(x = "country", y ="amount") +
          theme(text = element_text(size=16)) +
          coord_flip()
    } else{
      print("Data is Unavailable.")
      }
  })
}

  
shinyApp(ui, server)