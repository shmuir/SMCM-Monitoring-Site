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
  
  output$licor_plot <- renderPlot({
    ggplot(data=subdata_licor(), aes_string(x="datetime", y="prop_air", color="depth_m")) +
      geom_point(size = 2.5) +
      labs(x="", y="air_prop") +
      theme_light() +
      scale_color_viridis_d()
  })
  
  output$licor_table <- DT::renderDataTable({
    
    
    licor_data_tab <- licor_data %>% 
      select(datetime, depth_m, prop_air)
    
    
    DT::datatable(licor_data_tab,
                  extensions = "Scroller",
                  filter = "top", options = list(
                    deferRender = TRUE,
                    scrollY = 200,
                    scroller = TRUE
                  ),
                  rownames = FALSE,
                  colnames = c("Date",
                               "Depth (m)",
                               "Prop.")
    )
    
  }) 
  
}


