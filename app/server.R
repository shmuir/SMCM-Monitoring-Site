# Load libraries ------------------------------------------------
library(ggplot2)


# Create server -------------------------------------------------------
server <- function(input, output, session) {
  
  subdata<-reactive({
    dock_data %>%
      filter(
        as.Date(datetime) >= as.Date(input$datetime[1]),
        as.Date(datetime) <= as.Date(input$datetime[2], 
                                     select_values == input$select_values))
       
  })
  
  subdata_licor <- reactive({
    licor_data %>%
      filter(
        as.Date(datetime) >= as.Date(input$datetime[1]),
        as.Date(datetime) <= as.Date(input$datetime[2], 
                                     select_values == input$select_values))
  })
  
  subdata_nutrient <- reactive({
    nutrient_data %>%
      filter(
        as.Date(date_time) >= as.Date(input$date_time[1]),
        as.Date(date_time) <= as.Date(input$date_time[2], 
                                     select_nutrient == input$select_nutrient))
    
  })
  
  subdata_weather <- reactive({
    weather_data %>%
      filter(
        as.Date(date) >= as.Date(input$date[1]),
        as.Date(date) <= as.Date(input$date[2], 
                                      weather_select == input$weather_select))
    
  })
  
  output$ysi_plot <- renderPlot({
    ggplot(data=subdata(), aes_string(x="datetime", y=input$y_var, color="depth_m")) +
      geom_point(size = 2.5) +
      geom_line() +
      labs(x="", y=input$y_var) +
      theme_light() +
      scale_color_viridis_d()
    
  })
  
  output$dock_table <- DT::renderDataTable({
    
    
    dock_data_tab <- dock_data %>% 
      select(datetime, salinity, ph, temp_c, do_percent, do_mg_l)
    
    
    DT::datatable(dock_data_tab,
                  extensions = "Scroller",
                  filter = "top", options = list(
                    deferRender = TRUE,
                    scrollY = 200,
                    scroller = TRUE
                  ),
                  rownames = FALSE,
                  colnames = c("Date",
                               "Salinity",
                               "ph",
                               "Temperature (ºC)",
                               "DO Percent",
                               "DO (mg/l)")
    )
    
  }) 
  

  
  output$nutrient_plot <- renderPlot({
    ggplot(data=subdata_nutrient(), aes_string(x="date_time", y="mg_l", color = "nutrient")) +
      geom_point(size = 2.5) +
      geom_line() +
      labs(x="", y="mg/l") +
      theme_light() +
      scale_color_viridis_d()
    
  })
  
  output$nutrient_tab <- DT::renderDataTable({
    
    
    nutrient_tab <- nutrient_dattab %>% 
      select(date_time, nh3_mg_l, no3_l_mg_l, no3_m_mg_l, po4_mg_l)
    
    
    DT::datatable(nutrient_tab,
                  extensions = "Scroller",
                  filter = "top", options = list(
                    deferRender = TRUE,
                    scrollY = 200,
                    scroller = TRUE
                  ),
                  rownames = FALSE,
                  colnames = c("Date",
                               "NH3",
                               "NO3 low",
                               "NO3 high", 
                               "PO4")
    )
    
  }) 
  
  output$weather_plot <- renderPlot({
    ggplot(data=subdata_weather(), aes_string(x="date", y=input$y_variable)) +
      geom_point() +
      geom_line() +
      labs(x="", y=input$y_variable) +
      theme_light() +
      scale_color_viridis_d()
    
  })
  
  output$weather_table <- DT::renderDataTable({
    
    
    weather_table <- weather_data %>% 
      select(datetime, wind_avg, pressure, airtemp, rel_hum, illum, uv_index, solar_rad, rain_accum, day_rain_accum, strike_ct)
    
    
    DT::datatable(weather_table,
                  extensions = "Scroller",
                  options = list(
                    deferRender = TRUE,
                    scrollY = 200,
                    scroller = TRUE
                  ),
                  rownames = FALSE,
                  colnames = c("Date",
                               "Wind Avg. (mph)",
                               "Pressure (Pa)",
                               "Air Temperature (ºC)",
                               "Humidity",
                               "Illum",
                               "UV Index",
                               "Solar Radiation",
                               "Rain Accumulation",
                               "Daily Rain Accumulation",
                               "Lightening Strikes")
    )
    
  }) 
  
  
    observe({ 
      weather <<- paste0("https://tempestwx.com/station/69060/")
    })
    output$frame <- renderUI({
      dock_weather <- tags$iframe(src=weather, height=600, width=600)
      print(dock_weather)
      dock_weather
    })
  
}


