library(shiny)
library(shinydashboard)
library(shinythemes)
library(dygraphs)
library(darksky)
library(tidyverse)
library(zipcode)
library(dplyr)
library(feedeR)
library(tidyRSS)
library(tidytext)
library(tidyverse)
library(gridExtra)

## Weather
data(zipcode)
darksky_api_key()

sidebarMenu <- sidebarMenu(
  menuItem("Home", tabName = "home", icon = icon("home")),
  menuItem("Models", tabName = "models", icon = icon("chart-line")),
  menuItem("News", tabName = "news", icon = icon("newspaper")),
  menuItem("Weather", tabName = "weather", icon = icon("sun")))

homeTab <-tabItem(tabName = "home",
                  tags$head(
                    tags$style(
                      HTML('
                           #monthInputBox{height: 75px}
                           #breakEvenInputBox{height: 75px}'))),
                  box(
                    width = 5, 
                    HTML('<div style="height: 40px">
                            <h4><b>News Feed</b></h4>
                         </div>'),
                    htmlOutput("newsPanelHome")
                  ),
                  box(
                    width = 7,
                    HTML('<div style="height: 40px">
                            <h4><b>Price Prediction Model</b></h4>
                         </div>'),
                    dygraphOutput("holtzPlot", height = "250px"),
                    box(id = "breakEvenInputBox",
                        width = 6, 
                        numericInput("breakEven", "Break Even Point", min = 0, value = 950)),
                    box(id = "monthInputBox",
                        width = 6, 
                        selectInput("contract-month", "Contract Date:", 
                                    c("March 2020" = "march2020",
                                      "May 2020" = "may2020",
                                      "July 2020" = "july2020")))
                  )
                  # ,
                  # box(
                  #   HTML('<div style="display:flex; flex-direction: row; justify-content: space-between; align-items: center; height: 40px">
                  #           <h4><b>7 Day Weather Forecast</b></h4>
                  #           <div class="form-group shiny-input-container" style="width: 25%; display:flex; flex-direction: row; justify-content: space-around; align-items: center">
                  #             <label for="weatherZip2">Zip</label>
                  #             <input id="weatherZip2" type = "text" class = "form-control shiny-bound-input" value="55902" size="10"/>
                  #           </div>
                  #         </div>
                  #        '),
                  #   width = 12,
                  #   plotOutput(outputId = "forecastPlotMain", height = "150px"))
                  )


modelsTab <- tabItem(tabName = "models",
                     h2("Models"))

newsTab <- tabItem(tabName = "news",
                   selectInput("newsVariable", "Query String:",
                               c("Soybeans" = "soybeans",
                                 "Soybean Prices" = "soybean+prices",
                                 "US and China Trade" = "us+and+china+trade",
                                 "Soybean Weather" = "soybean+weather",
                                 "US Soybean Production" = "us+soybean+production",
                                 "Brazil Soybean Production" = "brazil+soybean+production",
                                 "Argentina Soybean Production" = "argentina+soybean+production")),
                   box(
                     HTML('<div style="height: 40px">
                            <h4><b>News Feed</b></h4>
                          </div>'),
                     htmlOutput("newsPanelMain")
                   ),
                   box(
                     HTML('<div style="height: 40px">
                            <h4><b>Sentiment Analysis</b></h4>
                          </div>'),
                     plotOutput("newsSentiment", height = "400px")
                   ))

weatherTab <- tabItem(tabName = "weather",
                      tags$head(
                        tags$style(
                          HTML('
                               #zipInputBox{height: 75px}
                               #dateInputBox{height: 75px}'))),
                      box(id = "zipInputBox",
                          width = 6,
                          textInput("weatherZip","Zip Code:", value = "55902")),
                      box(id= "dateInputBox",
                          width = 6,
                          dateInput("historicalDate", "Start Date:", value = "2019-11-29")),
                      box(
                          HTML('<div style="height: 30px">
                                  <h4><b>Historical Weather</b></h4>
                               </div>'),
                          width = 12,
                          plotOutput(outputId = "histWeatherPlot", height = "200px")),
                      box(
                          HTML('<div style="height: 30px">
                                  <h4><b>7 Day Weather Forecast</b></h4>
                               </div>'),
                          width = 12,
                          plotOutput(outputId = "forecastPlot", height = "200px"))
                      )



ui <- dashboardPage(skin = "green",
                    dashboardHeader(
                      title = "Soybean Decision Support Tool",
                      titleWidth = 350
                    ),
                    dashboardSidebar(
                      width = 150,
                      sidebarMenu
                    ),
                    dashboardBody(
                      tabItems(
                        # Home Page Tab
                        homeTab,
                        
                        # Models Tab
                        modelsTab,
                        
                        # News Tab
                        newsTab
                        
                        # Weather Tab
                        #weatherTab
                      )
                    )
)



server <- function(input, output) {
  
  output$holtzPlot <- renderDygraph({
    dygraph(all.xtsJuly, main = "Holt Winters - July 2020 Futures") %>% 
      dyRangeSelector(height = 40,
                      dateWindow = c("2019-09-01", "2019-11-09")) %>%
      dySeries(name = "actuals", label = "Actual") %>%
      dySeries(c("lwr","fit","upr"), label = "Predicted") %>%
      dyLegend(show = "always", hideOnMouseOut = FALSE) %>%
      dyHighlight(highlightCircleSize = 5,
                  highlightSeriesOpts = list(strokeWidth = 2)) %>%
      dyOptions(axisLineColor = "navy", gridLineColor = "grey") %>%
      dyLimit(limit = input$breakEven)
  })
  
  weatherLoc <- reactive({
    subset(zipcode, zip == input$weatherZip)
  })
  
  weatherLoc <- reactive({
    subset(zipcode, zip == input$weatherZip2)
  })
  
  output$forecastPlot <- renderPlot({
    ## Get the lat and long values based on the zip code input
    dailyForecast <- get_current_forecast(weatherLoc()$latitude, weatherLoc()$longitude)$daily
    
    tempPlot <- ggplot(dailyForecast, aes(x=as.Date(dailyForecast$time), y=dailyForecast$temperatureHigh)) + 
      geom_line() + 
      geom_point() + 
      labs(title=paste("Temperature in ", weatherLoc()$city, ", ", weatherLoc()$state, sep = "", closure = ""),x="Day", y = "Temperature (C)")
    precipPlot <-  ggplot(dailyForecast, aes(x=as.Date(dailyForecast$time), y=dailyForecast$precipProbability)) + 
      geom_line() + 
      geom_point() + 
      labs(title=paste("Precipitation Probability in ", weatherLoc()$city, ", ", weatherLoc()$state, sep = "", closure = ""),x="Day", y = "Probability")
    grid.arrange(tempPlot, precipPlot, ncol = 2)
  })
  
  output$forecastPlotMain <- renderPlot({
    ## Get the lat and long values based on the zip code input
    dailyForecast <- get_current_forecast(weatherLoc()$latitude, weatherLoc()$longitude)$daily
    
    tempPlot <- ggplot(dailyForecast, aes(x=as.Date(dailyForecast$time), y=dailyForecast$temperatureHigh)) + 
      geom_line() + 
      geom_point() + 
      labs(title=paste("Temperature in ", weatherLoc()$city, ", ", weatherLoc()$state, sep = "", closure = ""),x="Day", y = "Temperature (C)")
    precipPlot <-  ggplot(dailyForecast, aes(x=as.Date(dailyForecast$time), y=dailyForecast$precipProbability)) + 
      geom_line() + 
      geom_point() + 
      labs(title=paste("Precipitation Probability in ", weatherLoc()$city, ", ", weatherLoc()$state, sep = "", closure = ""),x="Day", y = "Probability")
    grid.arrange(tempPlot, precipPlot, ncol = 2)
  })
  
  output$histWeatherPlot <- renderPlot({
    datesSinceFrame <- data.frame(lat = weatherLoc()$latitude,
                                  lon = weatherLoc()$lon,
                                  date = datesSince(input$historicalDate))
    historicalForecast <- pmap(list(datesSinceFrame$lat, datesSinceFrame$lon,
                                    datesSinceFrame$date),
                               get_forecast_for)
    
    histDF <-data.frame()
    
    for (i in seq(1, length(historicalForecast))) {
      histDF <- bind_rows(histDF, historicalForecast[[i]]$daily)
    }
    
    tempPlot <- ggplot(histDF, aes(x=as.Date(histDF$time), y=histDF$temperatureHigh)) + 
      geom_line() + 
      geom_point() + 
      labs(title=paste("Temperature History in ", weatherLoc()$city, ", ", weatherLoc()$state, sep = "", closure = ""),x="Day", y = "Temperature in Celsius")
    precipPlot <-  ggplot(histDF, aes(x=as.Date(histDF$time), y=histDF$precipAccumulation)) + 
      geom_col() + 
      labs(title=paste("Precipitation History in ", weatherLoc()$city, ", ", weatherLoc()$state, sep = "", closure = ""),x="Day", y = "Precipitation Totals") 
    grid.arrange(tempPlot, precipPlot, ncol = 2)
  })
  
  newsQuery <- reactive({
    tidyfeed(paste("https://news.google.com/rss/search?q=", input$newsVariable ,"&hl=en-US&gl=US&ceid=US:en", sep = "", collapse = ""))
  })
  
  output$newsSentiment <- renderPlot({
    # Create new dataset based on input variable
    newsDataset <- newsQuery() %>%
      unnest_tokens(word, item_title) %>%
      anti_join(stop_words) %>%
      inner_join(get_sentiments("bing"), by = "word")
    
    # Plot sentiment analysis
    ggplot(newsDataset, aes(x=sentiment)) +
      geom_bar(aes(fill=sentiment), colour = "black") + 
      theme_classic() + 
      scale_fill_manual(values = c("#616161", "#FFD700"))
  })
  
  output$newsPanelHome <- renderUI({
    tabPanel(style = "overflow-y:scroll; max-height: 350px; min-height: 350px",
             a(),
             a(newsQuery()$item_title[1], href = newsQuery()$item_link[1], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[2], href = newsQuery()$item_link[2], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[3], href = newsQuery()$item_link[3], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[4], href = newsQuery()$item_link[4], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[5], href = newsQuery()$item_link[5], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[6], href = newsQuery()$item_link[6], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[7], href = newsQuery()$item_link[7], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[8], href = newsQuery()$item_link[8], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[9], href = newsQuery()$item_link[9], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[10], href = newsQuery()$item_link[10], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[11], href = newsQuery()$item_link[11], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[12], href = newsQuery()$item_link[12], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[13], href = newsQuery()$item_link[13], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[14], href = newsQuery()$item_link[14], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[15], href = newsQuery()$item_link[15], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[16], href = newsQuery()$item_link[16], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[17], href = newsQuery()$item_link[17], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[18], href = newsQuery()$item_link[18], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[19], href = newsQuery()$item_link[19], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[20], href = newsQuery()$item_link[20], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[21], href = newsQuery()$item_link[21], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[22], href = newsQuery()$item_link[22], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[23], href = newsQuery()$item_link[23], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[24], href = newsQuery()$item_link[24], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[25], href = newsQuery()$item_link[25], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[26], href = newsQuery()$item_link[26], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[27], href = newsQuery()$item_link[27], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[28], href = newsQuery()$item_link[28], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[29], href = newsQuery()$item_link[29], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[30], href = newsQuery()$item_link[30], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[31], href = newsQuery()$item_link[31], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[32], href = newsQuery()$item_link[32], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[33], href = newsQuery()$item_link[33], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[34], href = newsQuery()$item_link[34], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[35], href = newsQuery()$item_link[35], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[36], href = newsQuery()$item_link[36], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[37], href = newsQuery()$item_link[37], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[38], href = newsQuery()$item_link[38], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[39], href = newsQuery()$item_link[39], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[40], href = newsQuery()$item_link[40], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[41], href = newsQuery()$item_link[41], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[42], href = newsQuery()$item_link[42], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[43], href = newsQuery()$item_link[43], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[44], href = newsQuery()$item_link[44], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[45], href = newsQuery()$item_link[45], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[46], href = newsQuery()$item_link[46], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[47], href = newsQuery()$item_link[47], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[48], href = newsQuery()$item_link[48], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[49], href = newsQuery()$item_link[49], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[50], href = newsQuery()$item_link[50], target = "_blank", style = "font-size: 12pt")
    )
  })
  
  output$newsPanelMain <- renderUI({
    tabPanel(style = "overflow-y:scroll; max-height: 450px; min-height: 450px",
             a(),
             a(newsQuery()$item_title[1], href = newsQuery()$item_link[1], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[2], href = newsQuery()$item_link[2], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[3], href = newsQuery()$item_link[3], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[4], href = newsQuery()$item_link[4], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[5], href = newsQuery()$item_link[5], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[6], href = newsQuery()$item_link[6], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[7], href = newsQuery()$item_link[7], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[8], href = newsQuery()$item_link[8], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[9], href = newsQuery()$item_link[9], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[10], href = newsQuery()$item_link[10], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[11], href = newsQuery()$item_link[11], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[12], href = newsQuery()$item_link[12], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[13], href = newsQuery()$item_link[13], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[14], href = newsQuery()$item_link[14], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[15], href = newsQuery()$item_link[15], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[16], href = newsQuery()$item_link[16], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[17], href = newsQuery()$item_link[17], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[18], href = newsQuery()$item_link[18], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[19], href = newsQuery()$item_link[19], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[20], href = newsQuery()$item_link[20], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[21], href = newsQuery()$item_link[21], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[22], href = newsQuery()$item_link[22], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[23], href = newsQuery()$item_link[23], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[24], href = newsQuery()$item_link[24], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[25], href = newsQuery()$item_link[25], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[26], href = newsQuery()$item_link[26], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[27], href = newsQuery()$item_link[27], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[28], href = newsQuery()$item_link[28], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[29], href = newsQuery()$item_link[29], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[30], href = newsQuery()$item_link[30], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[31], href = newsQuery()$item_link[31], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[32], href = newsQuery()$item_link[32], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[33], href = newsQuery()$item_link[33], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[34], href = newsQuery()$item_link[34], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[35], href = newsQuery()$item_link[35], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[36], href = newsQuery()$item_link[36], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[37], href = newsQuery()$item_link[37], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[38], href = newsQuery()$item_link[38], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[39], href = newsQuery()$item_link[39], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[40], href = newsQuery()$item_link[40], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[41], href = newsQuery()$item_link[41], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[42], href = newsQuery()$item_link[42], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[43], href = newsQuery()$item_link[43], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[44], href = newsQuery()$item_link[44], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[45], href = newsQuery()$item_link[45], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[46], href = newsQuery()$item_link[46], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[47], href = newsQuery()$item_link[47], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[48], href = newsQuery()$item_link[48], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[49], href = newsQuery()$item_link[49], target = "_blank", style = "font-size: 12pt"),hr(),
             a(newsQuery()$item_title[50], href = newsQuery()$item_link[50], target = "_blank", style = "font-size: 12pt")
    )
  })
  
  dateFormat <- function(dateList) {
    return((paste(as.character.Date(dateList), "T12:00:00-0400", sep = "")))
  }
  
  datesSince <- function(date) {
    return(seq(as.Date(date), as.Date(Sys.Date()), 'days'))
  }
  
  
}

shinyApp(ui = ui, server = server)

