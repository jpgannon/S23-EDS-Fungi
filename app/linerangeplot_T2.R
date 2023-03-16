library(shiny)
library(ggplot2)

merge_dat4 <- read.csv("https://raw.githubusercontent.com/jpgannon/S23-EDS-Fungi/main/myc_mergeDat.csv")

merge_dat4$Slash <- as.character(merge_dat4$Slash)

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
                  selected = "Growth Percent Change")          
    ),
    
    mainPanel(
      plotOutput("Plot")
    )
  )
)


server <- (function(input, output) {
  
  cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#111111")
  
  output$Plot <- renderPlot({
    dataset <- merge_dat4[ ,c(input$VarX,input$VarY)]
    data_sums <- aggregate(dataset[,2] ~ dataset[,1],data = dataset, summary)
    sums2 <- as.data.frame(data_sums[,2])
    
    data_sums[,2:7] <- sums2
    ggplot()+
      geom_vline(data = data_sums, mapping = aes(xintercept = 0))+
      geom_linerange(data = data_sums, mapping = aes(y = data_sums[,1], 
                                                     xmin = data_sums[,3], 
                                                     xmax = data_sums[,6],
                                                     color = data_sums[,1]))+
      geom_point(data = data_sums, mapping = aes(y = data_sums[,1], 
                                                 x = data_sums[,4],
                                                 color = data_sums[,1], 
                                                 size = 4))+
      geom_point(data = data_sums, mapping = aes(y = data_sums[,1], 
                                                 x = data_sums[,3],
                                                 color = "yellow",
                                                 size = 3.5))+
      geom_point(data = data_sums, mapping = aes(y = data_sums[,1],
                                                 x = data_sums[,6],
                                                 color = "yellow",
                                                 size = 3.5))+
      scale_color_manual(values = cbPalette)+
      theme_minimal()+
      labs(x = input$VarX, y = input$VarY)
   
  })

})

shinyApp(ui, server)
