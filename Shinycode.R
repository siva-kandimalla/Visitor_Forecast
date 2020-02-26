#loading Libraries
pacman::p_load(ggplot2, scales, grid, dplyr, readr, shiny, data.table, tibble, tidyr, stringr, forcats, tseries, forecast, maps, leaflet, geosphere, fpp2, timetk, plotly, imputeTS, timeDate, timetk, fpp2,tidyverse, shinycssloaders, htmltools, lubridate)
theme_set(theme_classic())

library('ggplot2')
library('scales')
library('grid')
library('dplyr')
library('readr')
library('tibble')
library('forcats')
library('timeDate')
library('tseries')
library('timetk')
library('fpp2')
library('shiny')
library('tidyverse')
library('tm')
library('data.table')
library('tidyr')
library('stringr')
library('forecast')
library('leaflet')
library( 'htmltools' )
library('plotly')
library('shinycssloaders')
library('RColorBrewer')


#Function that finds the lower round value
lowround <- function (a) {
  if (a<=0){a=0
  }else{
    a=floor(a)
  }
  return(a)
}

# Load data
air_visits <- as.tibble(fread('air_visit_data.csv'))
air_store <- as.tibble(fread('air_store_info.csv'))
air_reserve <- as.tibble(fread('air_reserve.csv'))
date_info = as.tibble(fread('date_info.csv'))
cum_visits <- as.tibble(fread('cum_visits.csv'))
# Data manipulation
air_visits <- air_visits %>%
  mutate(visit_date = ymd(visit_date))

cum_visits <- cum_visits %>%
  mutate(visit_date = ymd(visit_date))


air_visits <- air_visits %>%
  left_join(air_store, by = "air_store_id")


#Defining UI
ui <- fluidPage(
  tabsetPanel(
    tabPanel("Basic EDA",
             sidebarLayout(
               sidebarPanel(selectInput("restaurant","Select a Restaurant", choices = unique(air_visits$air_store_id)),
                            dateRangeInput("dtrange","Choose the Date Range",
                                           start = ymd('2016-12-01'),
                                           end = max(air_visits$visit_date),
                                           min = min(air_visits$visit_date), 
                                           max = max(air_visits$visit_date))),
               mainPanel(h1("Restaurant on Map"),
                         leafletOutput("mymap")%>% withSpinner(color="#3C8DBC"),
                         h1("Time Series Visualistion of past Visitors to the restaurant"),
                         plotlyOutput("tsplot")%>% withSpinner(color="#3C8DBC"), 
                         h1("Median Visitors to restaurant on different weekdays"),
                         plotlyOutput("tsplot2")%>% withSpinner(color="#3C8DBC"),
                         h1("Median Visitors to restaurant on different months"),
                         plotlyOutput("tsplot3")%>% withSpinner(color="#3C8DBC")
                         
               ))),
    
    tabPanel("ETS Model",
             sidebarLayout(
               sidebarPanel(selectInput("restaurant2","Select a Restaurant", choices = unique(air_visits$air_store_id)),
                            sliderInput("no_days","Choose the number of days to forecast", min = 1, max = 100, value = 7)),
               mainPanel(h1("Future Visitors forecast using ETS Model"),plotlyOutput("futrplot_ETS")%>% withSpinner(color="#3C8DBC"),
                         DT::dataTableOutput("table_ETS")%>% withSpinner(color="#3C8DBC")))),
    tabPanel("HoltWinters Model",
               mainPanel(h1("Future Visitors forecast using HoltWinters Model"),plotlyOutput("futrplot_HW")%>% withSpinner(color="#3C8DBC"),
                         DT::dataTableOutput("table_HW")%>% withSpinner(color="#3C8DBC"))),
    tabPanel("ARIMA Model",
               mainPanel(h1("Future Visitors forecast using ARIMA Model"),plotlyOutput("futrplot_ARIMA")%>% withSpinner(color="#3C8DBC"),
                         DT::dataTableOutput("table_ARIMA")%>% withSpinner(color="#3C8DBC")))
  )
)

# Server function
server <- function(input, output) {
  data = reactive({
    air_visits %>% filter(air_store_id==input$restaurant) %>% 
      group_by(visit_date) %>%
      summarise(Number_of_visitors = sum(visitors)) %>% 
      filter(visit_date>=input$dtrange[1]) %>% 
      filter(visit_date<=input$dtrange[2])
  })
  
  output$tsplot = renderPlotly({
    p = ggplot(data(), aes(visit_date,Number_of_visitors)) +
      geom_line(col = 'blue') +
      labs(y = "Number of visitors", x = "Date")
    ggplotly(p)
  })
  
  output$tsplot2 = renderPlotly({
    data3=data() %>%
      mutate(wday = wday(visit_date, label = TRUE)) %>%
      group_by(wday) %>%
      summarise(visits = median(Number_of_visitors))
    q = ggplot(data3,aes(wday, visits, fill = wday )) +
      geom_col() +
      theme(legend.position = "none", axis.text.x  = element_text(angle=45, hjust=1, vjust=0.9)) +
      labs(x = "Day of the week", y = "Median visitors")
    ggplotly(q)
  })
  
  output$tsplot3 = renderPlotly({
    data4=data() %>%
      mutate(month = month(visit_date, label = TRUE)) %>%
      group_by(month) %>%
      summarise(visits = median(Number_of_visitors))
    r = ggplot(data4,aes(month, visits, fill = month)) +
      geom_col() +
      theme(legend.position = "none") +
      labs(x = "Month", y = "Median visitors")
    ggplotly(r)
  })
  
  output$mymap <- renderLeaflet({
    data = air_visits %>% filter(air_store_id==input$restaurant)
    labs <- lapply(seq(nrow(air_store)), function(i) {
      paste0( '<p>',"Restaurant_ID=",air_store$air_store_id[i],'<p></p>',
              "Restaurant_Genre=",air_store$air_genre_name[i],'<p></p>',
              "Area Name=",air_store$air_area_name[i],'</p>' ) 

    })
    labs2 <- lapply(seq(nrow(data)), function(i) {
      paste0( '<p>',"Restaurant_ID=",data$air_store_id[i],'<p></p>',
              "Restaurant_Genre=",data$air_genre_name[i],'<p></p>',
              "Area Name=",data$air_area_name[i],'</p>' ) 
      
    })
    pal = colorFactor(palette = palette(),domain = air_store$air_genre_name)
    m <- leaflet(air_store) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~ longitude,
        lat = ~ latitude,
        color = ~ pal(air_genre_name),
        label = lapply(labs, htmltools::HTML),
        clusterOptions = markerClusterOptions()
      )%>%
      addLegend(
        position = 'bottomright',pal = pal, values = ~air_genre_name,
        title = 'Restaurant_Genre', opacity = 1
      )%>%
      addMarkers(lng = unique(data[c('longitude','latitude')])$longitude,
                 lat = unique(data[c('longitude','latitude')])$latitude,
                 label = lapply(labs2, htmltools::HTML))
    m
  })
  
  x = reactive({
    air_visits1=cum_visits %>% filter(air_store_id==input$restaurant2) %>% 
      group_by(visit_date, day_of_week) %>%
      summarise(Number_of_visitors = sum(visitors))
    #air_visits1$Number_of_visitors = DescTools::Winsorize(air_visits1$Number_of_visitors, probs = c(0.0, 0.95))
    #air_visits1 
  })
  
  
  ETS.fit = reactive({
    ets(tsclean(ts(x()$Number_of_visitors, frequency = 7)))
  })
  
  ETS_visits = reactive({
    ETS.fit() %>% forecast(h = input$no_days, level = c(95))
  })
  
  df_ETS = reactive({
    Point_forecast = sapply(as.numeric(ETS_visits()$mean),floor)
    lower_limit = sapply(as.numeric(ETS_visits()$lower),lowround)
    upper_limit = sapply(as.numeric(ETS_visits()$upper),ceiling)
    Date=seq(max(x()$visit_date)+1,max(x()$visit_date)+input$no_days,1)
    df1 = data.frame(Date,Point_forecast,lower_limit,upper_limit)
    df1
  })
  output$futrplot_ETS = renderPlotly({
    s = ggplot() +
      geom_line(data = x(), aes(x = visit_date, y = Number_of_visitors),col = 'blue') +
      geom_line(data = df_ETS(),aes(x=Date, y=Point_forecast), col = 'red')
      labs(y = "Number of visitors", x = "Date")
    ggplotly(s)
  })  
  output$table_ETS = DT::renderDataTable({df_ETS()})
  
  
  hw.fit = reactive({
    HoltWinters(tsclean(ts(x()$Number_of_visitors, frequency = 7)))
  })
    
    hw_visits = reactive({
      hw.fit() %>% forecast(h = input$no_days, level = c(95))
    })
    
    df_HW = reactive({
      Point_forecast = sapply(as.numeric(hw_visits()$mean),floor)
      lower_limit = sapply(as.numeric(hw_visits()$lower),lowround)
      upper_limit = sapply(as.numeric(hw_visits()$upper),ceiling)
      Date=seq(max(x()$visit_date)+1,max(x()$visit_date)+input$no_days,1)
      df1 = data.frame(Date,Point_forecast,lower_limit,upper_limit)
      df1
    })
    
    output$futrplot_HW = renderPlotly({
      s = ggplot() +
        geom_line(data = x(), aes(x = visit_date, y = Number_of_visitors),col = 'blue') +
        geom_line(data = df_HW(),aes(x=Date, y=Point_forecast), col = 'red')
      labs(y = "Number of visitors", x = "Date")
      ggplotly(s)
    })  
    output$table_HW = DT::renderDataTable({df_HW()})
    
    
    
    arima.fit = reactive({
      auto.arima(tsclean(ts(x()$Number_of_visitors, frequency = 7)),
                 stepwise = FALSE, approximation = FALSE)
    })
    
    arima_visits = reactive({
      arima.fit() %>% forecast(h = input$no_days, level = c(95))
    })
    
    df_ARIMA = reactive({
      Point_forecast = sapply(as.numeric(arima_visits()$mean),floor)
      lower_limit = sapply(as.numeric(arima_visits()$lower),lowround)
      upper_limit = sapply(as.numeric(arima_visits()$upper),ceiling)
      Date=seq(max(x()$visit_date)+1,max(x()$visit_date)+input$no_days,1)
      df1 = data.frame(Date,Point_forecast,lower_limit,upper_limit)
      df1
    })
    
    output$futrplot_ARIMA = renderPlotly({
      s = ggplot() +
        geom_line(data = x(), aes(x = visit_date, y = Number_of_visitors),col = 'blue') +
        geom_line(data = df_ARIMA(),aes(x=Date, y=Point_forecast), col = 'red')
      labs(y = "Number of visitors", x = "Date")
      ggplotly(s)
    })  
    output$table_ARIMA = DT::renderDataTable({df_ARIMA()})
    
    
}

shinyApp(server = server, ui = ui)






