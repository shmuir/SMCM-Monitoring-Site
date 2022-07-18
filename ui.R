# Load libraries, data -----------------------------------------------
library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(googlesheets4)

ysi_data <- read_sheet("https://docs.google.com/spreadsheets/d/19E9JgnPOJ6_OhN-Het-EC9rVEH-qPRPNovM0TV-f6lQ/edit?usp=sharing") %>%
  mutate(sample_time = format(sample_time,"%H:%M:%S"))

secchi_data <- read_sheet("https://docs.google.com/spreadsheets/d/141nNE5e3-pN_fidfUrbYLAT2zkTolqzwXAnX-LqmHYk/edit?usp=sharing") %>%
  select(sample_date, secchi_depth_m)

licor_data <- read_sheet("https://docs.google.com/spreadsheets/d/1zz6CgaQ7AfsfxxM9EBBqt_9tstSGnJvC6InhXsO0Pm4/edit?usp=sharing") %>%
  mutate(sample_time = format(sample_time,"%H:%M:%S"),
         datetime = ymd_hms(paste(sample_date, sample_time))) 

dock_data <- ysi_data %>%
  left_join(secchi_data) %>%
  mutate(datetime = ymd_hms(paste(sample_date, sample_time))) %>%
  filter(do_percent <= 125, do_percent >= 25,
         do_mg_l <= 10, do_mg_l >= 2.5)


# Page 1 - Introduction ----------------------------------------------
intro_panel <- tabPanel(
  "About", icon = icon("info"),
  
  titlePanel("SMCM Dock Monitoring"),
  
  img(src = "", height = 400, width = 800),
  br(), br(),
  
  p("Some stuff about the project"),
  
 #p(a(href = "https://", "Data Source")) #can add link to data
)

# Page 2 - Vizualization -------------------------------------------
select_values <- colnames(dock_data)
select_values <- select_values[select_values %in% 
                                 c('salinity', "ph", "temp_c", "do_percent", "do_mg_l", "salinity")]

ysi_sidebar <- sidebarPanel(
  selectInput(
    "y_var",
    label = "Y Variable",
    choices = select_values,
    selected = 'datetime'),
  dateRangeInput("datetime",
                 "Select a Date Range",
                 start = "2022-05-25"),)

ysi_main <- mainPanel(
  plotOutput("ysi_plot"),
  DT::dataTableOutput("dock_table"))

licor_sidebar <- sidebarPanel(
    dateRangeInput("datetime",
                             "Select a Date Range",
                             start = "2022-05-25"))

licor_main <- mainPanel(
  plotOutput("licor_plot"),
  DT::dataTableOutput("licor_table"))

second_panel <- navbarMenu("Visualizaton", icon = icon("chart-bar"),
           tabPanel("YSI",
                    titlePanel("YSI"),
                    p("Use the selector input below to choose which variable you would like to see."),
                    sidebarLayout(
                      ysi_sidebar, ysi_main)
           
           ),
           tabPanel("Licor",
                   titlePanel("Licor"),
                   sidebarLayout(
                     licor_sidebar, licor_main)
                    )
           
           )


weather_panel <- tabPanel("Weather", icon = icon("cloud-sun"),
                          titlePanel("Weather"), 
          sidebarLayout(
            sidebarPanel("SMCM dock has a tempest weather station"),
            mainPanel(fluidRow(
              htmlOutput("frame")
            )
            )
          ))


# User Interface -----------------------------------------------------
ui <- navbarPage(
  "Dock monitoring",
  intro_panel,
  second_panel,
  weather_panel
)

