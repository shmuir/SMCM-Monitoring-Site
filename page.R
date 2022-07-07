#Installing the libraries
install.packages("shiny")
install.packages("shinydashboard")

#Loading the libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(googlesheets4)

ysi_data <- read_sheet("https://docs.google.com/spreadsheets/d/19E9JgnPOJ6_OhN-Het-EC9rVEH-qPRPNovM0TV-f6lQ/edit?usp=sharing") %>%
  mutate(sample_time = format(sample_time,"%H:%M:%S"))

secchi_data <- read_sheet("https://docs.google.com/spreadsheets/d/141nNE5e3-pN_fidfUrbYLAT2zkTolqzwXAnX-LqmHYk/edit?usp=sharing") %>%
  select(sample_date, secchi_depth_m)

dock_data <- ysi_data %>%
  left_join(secchi_data) %>%
  mutate(datetime = ymd_hms(paste(sample_date, sample_time))) %>%
  filter(do_percent <= 125, do_percent >= 25,
         do_mg_l <= 10, do_mg_l >= 2.5)


if(interactive()){
  
  ui<-dashboardPage(
    
    dashboardHeader(title = "SMCM dock"),
    
    dashboardSidebar(
      dateRangeInput("datetime", "Select a Date Range"),
      selectInput("y-var","Select a variable", select_values),
    ),
    
    dashboardBody(
      plotOutput("plot")
    ))
  
  server<-function(input, output, session){
    subdata<-reactive({
      dock_data %>%
        filter(
          as.Date(datetime) >= as.Date(input$date[1]),
          as.Date(datetime) <= as.Date(input$date[2]),
          select_values == input$select_values)
    })
    output$plot<-renderPlot({
      
      ggplot(data=subdata(), aes_string(x=datetime, y=input$y_var, color="depth_m")) +
        geom_point() +
        labs(x="", y=input$y_var) +
        theme_light() +
        scale_color_viridis_d()
      
    })
  }
  
  shinyApp(ui = ui, server = server)
}
