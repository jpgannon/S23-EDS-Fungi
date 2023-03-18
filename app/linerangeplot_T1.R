library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Linerange Plot"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "VarX",
                  label = "Select X-axis Variable:",
                  choices = list("Species" = "Species.x",
                                 "Legacy Plot Position" = "leg_pos",
                                 "Treatment Type" = "Treatment",
                                 "Slash Level" = "Slash",
                                 "Mycorrizal association" = "MT"),
                  selected = "Species"),

      selectInput(inputId = "VarY",
                  label = "Select Y-axis Variable:",
                  choices = list("Survival rate" = "Survival",
                                 "Growth Percent Change" = "PCG"),
                  selected = "Survival Rate")          
    ),
    
    mainPanel(
      plotOutput("Plot")
    )
  )
)


server <- (function(input, output) {
  
  cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#111111")
  
  output$Plot <- renderPlot({
    dataset <- aggregate(input$VarY ~ input$VarX,data = merge_dat3, summary)
    
    ggplot()+
      geom_vline(data = dataset, mapping = aes(xintercept = 0))+
      geom_linerange(data = dataset, mapping = aes(y = dataset[,1],
                                                    xmin = dataset[,3],
                                                    xmax = dataset[,6],
                                                    color = dataset[,1]))+
      geom_point(data = dataset, mapping = aes(y = dataset[,1],
                                                         x = dataset[,4], 
                                                         color = dataset[,1],
                                                         size = 4))+
      geom_point(data = dataset, mapping = aes(y = dataset[,1],
                                                         x = dataset[,3], 
                                                         color = "yellow",
                                                         size = 3.5))+
      geom_point(data = dataset, mapping = aes(y = dataset[,1],
                                                         x = dataset[,6], 
                                                         color = "yellow",
                                                         size = 3.5))+
      scale_color_manual(values = cbPalette)+
      theme_minimal()
   
  })

})

shinyApp(ui, server)
