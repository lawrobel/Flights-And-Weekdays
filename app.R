#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#Author: Lance Wrobel

library(shiny)
library(nycflights13)
library(lubridate)
library(ggplot2)
library(tidyverse)

flights_2 <- flights %>% mutate(date = make_date(year, month, day), hour = hour(time_hour))

plot_large_dist_by_day<-function(prop_by_day){
   
  p1<-ggplot(prop_by_day, aes(wday,percent_above,
                          group=1))+geom_point(size=3, color="red")+geom_line()
  
  # the following line customizes the appearance of the plot
  p1<-p1+xlab("Day of Week")+ylab(paste("Percentage of Flights",sep=""))+theme(axis.text=element_text(size=12),
                                                                            axis.title=element_text(size=14,face="bold")) 
  return(p1)
}

  
ui <- fluidPage(
   

   # title
   titlePanel("Which day of the week has the longest flights?"),
   #subheaders
   h4("Description:"),
   h4("For each day of the week, calculate the percentage of flights that traveled"),
   h4("farther than the distance cutoff and that departed after the hour cutoff. "),
  
   # control panel
   sidebarLayout(
     sidebarPanel(
       
         sliderInput("distance_cutoff",
                     "Select Distance Cutoff (in miles):",
                     min = 1000,
                     max = 2400,
                     value = 2000),
         sliderInput("hour_cutoff",
                     "Select Hour Cutoff:",
                     min = 15, # time formatting is so that 15 hour is 3:00pm
                     max = 23,
                     value = 18)
      ),
      
      # show the plot on the webpage
      mainPanel(
         plotOutput("distPlot")
      )
   )
 )

server <- function(input, output) {
   
  values <- reactiveValues()
  
  # behind the scenes
  observe({
    values$obj <- flights_2 %>% mutate(wday = wday(date, label = TRUE)) %>% filter(hour > input$hour_cutoff) %>%
    group_by(wday) %>% 
    mutate(num_flights = n())
    
    values$obj_2 <- values$obj %>%
      filter(distance > input$distance_cutoff) %>%
      mutate(percent_above = (n()/num_flights)*100) %>%
      arrange(desc(percent_above)) %>%
      distinct(wday,percent_above)
    })  
  
  # affects what the user sees
   output$distPlot <- renderPlot({
     plot_large_dist_by_day(values$obj_2)
   })
}
# run the app
shinyApp(ui = ui, server = server)

