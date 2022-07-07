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

dock_data <- ysi_data %>%
  left_join(secchi_data) %>%
  mutate(datetime = ymd_hms(paste(sample_date, sample_time))) %>%
  filter(do_percent <= 125, do_percent >= 25,
         do_mg_l <= 10, do_mg_l >= 2.5)

# Page 1 - Introduction ----------------------------------------------
intro_panel <- tabPanel(
  "Introduction",
  
  titlePanel("SMCM Dock Monitoring"),
  
  img(src = "smcm_water.jpg", height = 400, width = 800),
  br(), br(),
  
  p("Some stuff about the project"),
  
 # p(a(href = "https://", "Data Source")) #can add link to data
)

# Page 2 - Vizualization -------------------------------------------
select_values <- colnames(dock_data)
select_values <- select_values[select_values %in% c('salinity', "ph", "temp_c", "do_percent", "do_mg_l", "salinity")] # remove unwanted columns

sidebar_content <- sidebarPanel(
  selectInput(
    "y_var",
    label = "Y Variable",
    choices = select_values,
    selected = 'datetime'),
  dateRangeInput("datetime",
                 "Select a Date Range",
                 start = "2022-05-25"),
)

main_content <- mainPanel(
  plotOutput("plot")
)

second_panel <- tabPanel(
  "Visualization",
  titlePanel("Characteristics"),
  p("Use the selector input below to choose which variable you would like to see."),
  sidebarLayout(
    sidebar_content, main_content
  )
)

# User Interface -----------------------------------------------------
ui <- navbarPage(
  "Dock monitoring",
  intro_panel,
  second_panel
)

