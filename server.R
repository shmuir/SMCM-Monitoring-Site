# Load libraries, data ------------------------------------------------
library(ggplot2)

dock_data <- dock_data


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
  
  output$ysi_plot <- renderPlot({
    ggplot(data=subdata(), aes_string(x="datetime", y=input$y_var, color="depth_m")) +
      geom_point(size = 2.5) +
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
  
  #output$licor_plot <- renderPlot({
    #ggplot(data=subdata_licor(), aes_string(x="datetime", y="prop_air", color="depth_m")) +
     # geom_point(size = 2.5) +
     # labs(x="", y="air_prop") +
     # theme_light() +
      #scale_color_viridis_d()
  #})
  
  #output$licor_table <- DT::renderDataTable({
    
    
    #licor_data_tab <- licor_data %>% 
      #select(datetime, depth_m, prop_air)
    
    
   # DT::datatable(licor_data_tab,
               #   extensions = "Scroller",
                #  filter = "top", options = list(
                 #   deferRender = TRUE,
                  #  scrollY = 200,
                  #  scroller = TRUE
                 # ),
                 # rownames = FALSE,
                 # colnames = c("Date",
                            #   "Depth (m)",
                            #   "Prop.")
    #)
    
 # })
  
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
      select(date_time, nh3_mg_l, no3_l_mg_l, po4_mg_l)
    
    
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
                               "NO3",
                               "PO4")
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


