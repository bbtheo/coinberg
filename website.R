library(shiny)
library(bslib)
library(geckor)
library(lubridate)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(shinydashboard)
library(flexdashboard)


#####
# Here are some helper functions 

change_price_to_movement_mat <- function(data){
  #Change the dataframe to n x n matrix that has all relative price movements
  p <- data$price
  mat<-t(outer(p, p, '-')) #the price difference between every date
  mat <- mat %*% diag((1 / p)) #divide every instance with the starting date
  mat[lower.tri(mat, diag = TRUE)] <- NaN # set all dates under the diagonal to NaN so we get only one solution
  return(mat)
}

#get the daily difference from the price movement matrix we get this as removing the
#first column and last row and taking the diagonal. This way we get the difference between every
#day. This could be done also with R function diff().

get_daily_diff <- function(mat) {
  diag(mat[1:nrow(mat)-1, 2:nrow(mat)])
}

get_longest_bear <- function(diff){
  #Function that returns the longest bear market when given daily differences
  
  #Splitting the timeseries with every positive difference gives us a list of bear markets
  ind_splits <- split(c(1:length(diff)),cumsum(c(diff > 0)))
  
  #lets select the longest bear market
  bear_ind <- ind_splits[which.max(lengths(ind_splits))]
  
  if (diff[bear_ind[[1]][1]] > 0) {
    #if the longest bear market is in the middle of the timeseries we have to substract 
    #the first day as the splits have always a positive day as their first entry  
    
    return(bear_ind[[1]][-1]+1) 
  }
  
  return(bear_ind[[1]]+1) #if not we just return the length
}

get_max_dates <- function(price_movement){
  # A function for the
  which(price_movement == max(price_movement, na.rm = T), arr.ind = T)
}


##### 
# Here is the code for the UI
myUI <- fluidPage(
  
  theme = bslib::bs_theme(bootswatch = "slate"),
  
  # first row where there is the title and current price
  fluidRow(column(8,
                  titlePanel(span("Coinberg terminal", style = "colour:orange"))
                  ),
           column(4,
                  fluidRow(textOutput("price_now_1")),
                  fluidRow(textOutput("price_now_2"))
                  )
           ),
  
  # second row where there are the inputs for the website          
  fluidRow(
    column(4,
           dateRangeInput("date_range", "dates",
                          end = ymd(Sys.Date()), start = ymd(Sys.Date())-months(12),
                          max = ymd(Sys.Date())) 
           #This way we can get the  start and end dates
          ),
    #This select input gets the 
    column(4, selectInput("currency", "Currency", c("eur","usd","sek"))
          ),
    column(4, selectInput("coin", "Meme coin", c("bitcoin","cardano",
                                                  "ethereum","dogecoin",
                                                  "binancecoin", "terra-luna",
                                                  "shiba-inu","floki-inu"))
          )
        ),
  
  # third row where the output of the plot
  fluidRow(
    column(12,plotOutput("plot")
           ),
    width = "100%",
    height = "1000px"
      ),
  #fourth row of the valueboxes
  fluidRow(
    column(4,valueBox(uiOutput("bear"))),
    column(4,valueBox(uiOutput("time_machine")), color = "green"),
    column(4,valueBox(uiOutput("max_volume")))
  )
  
  
)

#####
# Here is the code for the backend

myserver <- function(input,output,session){
  
  
  # A reactive function will update when the inputs are changed
  # Here it will get new values from the coingecko API when the inputs are changed
  history <- reactive(coin_history_range(coin_id = input$coin, vs_currency = input$currency, 
                                         from = as.POSIXct(ymd(input$date_range[1])-hours(1)), 
                                         to = as.POSIXct(ymd(input$date_range[2])+hours(1)))%>%
                        filter(hour(timestamp)==0)
                      )
  # When the reactive history dataframe is changed the price matrix will also be changed
  price_mat <- reactive(history()%>%
                          change_price_to_movement_mat())
  
  #Update the daily differences when the 
  daily_diff <- reactive(price_mat() %>%
                           get_daily_diff())
  
  max_ind <- reactive(price_mat() %>% get_max_dates()) #getting the indeces of max dates of the reactive price matrix
  
  max_dates <- reactive( history()[c(max_ind()[1,]),]) #getting the date of max date
  
  max_volume_ind <- reactive(which.max(history()$total_volume)[1])
  
  # the current price of input coin
  price_now <- reactive(current_price(input$coin, input$currency, include_market_cap = F,include_24h_vol = F))
  
  
  # rendering the plot from inputs
  output$plot <- renderPlot({
    
    #the lineplot of the price history
    history<-history()%>%
      ggplot(aes(y = price, x = timestamp))+
      geom_area(fill="navyblue", colour="white", alpha=0.6) +
      labs(x="",y="",title = paste0("Daily price and trading volume of ",input$coin, " in ",input$currency))+
      geom_point(data = max_dates(), aes(y = price,x = timestamp, color = "red", size = ))+
      geom_label(data = max_dates(), aes(y = price,x = timestamp,label=c("Buy", "Sell")),hjust=0, vjust=0)+
      theme_dark() + 
      theme(plot.background = element_rect(fill = "#ECF0F5"), 
            axis.text.x = element_blank(),
            axis.ticks.x=element_blank(),
            legend.position="none",
            plot.margin=unit(c(0.5,0.5,-0.4,0.5), "cm"))
    
    # the barplot of trading volumes
    volume <- history()%>%
      ggplot(aes(y = total_volume, x = timestamp,
                 fill=factor(ifelse(timestamp==history()[max_volume_ind(),]$timestamp,"max","normal"))))+
      geom_col(alpha=0.75) +
      scale_fill_manual(name = "area", values=c("red","white"))+
      labs(x="",y="")+
      theme_dark() + 
      theme(plot.background = element_rect(fill = "#ECF0F5"), 
            axis.text.y = element_blank(),
            plot.margin=unit(c(-0.5,0.5,0.5,0.5), "cm"),
            legend.position="none",
            plot.title.position = "plot")
    
    #arranging them together
    ggarrange(history, volume, heights = c(2, 1), ncol = 1, nrow = 2, align = 'v')
    
  })
  
  output$price_now_1 <- renderText({
    #This outputs the first line of the upper right infobox
    paste0("The current price of ", input$coin, " is ", price_now()$price," ",input$currency, "!")
  })
  
  output$price_now_2 <- renderText({
    #Second line of the pirce infobox
    paste0(round(price_now()$price_percent_change_24h, 2), "% change in the last 24h")
  })
  
  output$bear <- renderValueBox({
    #Outputs the text of the longest bear market as a valua
    
    bears <- get_longest_bear(daily_diff())
    text_b <- paste0("Longest bear market is between ", history()$timestamp[bears[1]], " and ", 
                    history()$timestamp[bears[length(bears)]])
    
    
    shinydashboard::valueBox(
      value = length(bears),
      subtitle = text_b,
      icon = icon("arrow-down"),
      color = "aqua",
      width = 12
    )

  })
  
  output$time_machine <- renderValueBox({
    #outputs the answer for the time machine question
    return <- round((max_dates()$price[2]-max_dates()$price[1])/max_dates()$price[1]*100,2)
    text_tm<-paste0("Is max return you can get with ",input$coin ,
                    " by buying at ", ymd(max_dates()$timestamp[1]),
                    " and selling at ", ymd(max_dates()$timestamp[2]))
    
    shinydashboard::valueBox(
      value = paste0(return,"%"),
      subtitle = text_tm,
      icon = icon("calendar"),
      color = "aqua",
      width = 12
    )
    
  })
  
  
  output$max_volume <- renderValueBox({
    #outputs the maximum trading volume
    
    max_volume <- round(history()[max_volume_ind(),]$total_volume,2)
    date <- ymd(history()[max_volume_ind(),]$timestamp)
                        
    text_mv<-paste0("Is the date of max trade volume in ",input$coin, " which was ", max_volume)
    
    shinydashboard::valueBox(
      value = date,
      subtitle = text_mv,
      icon = icon("business-time"),
      color = "aqua",
      width = 12
    )
    
  })
  
  
}

#run everything
shinyApp(
  ui = myUI,
  server = myserver
)