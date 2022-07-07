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
  
  output$plot <- renderPlot({
    ggplot(data=subdata(), aes_string(x=datetime, y=input$y_var, color="depth_m")) +
      geom_point() +
      labs(x="", y=input$y_var) +
      theme_light() +
      scale_color_viridis_d()
    
  })
}
