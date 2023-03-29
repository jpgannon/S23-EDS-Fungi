library(shiny)
library(ggplot2)

merge_dat4 <- read.csv("https://raw.githubusercontent.com/jpgannon/S23-EDS-Fungi/main/Merged_data4.csv")

merge_dat4$Slash <- as.character(merge_dat4$Slash)

ui <- fluidPage(
  titlePanel("Linerange Plot"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "VarX",
                  label = "Select Y-axis Variable:",
                  choices = list("Species" = "Species.x",
                                 "Legacy Plot Position" = "leg_pos",
                                 "Mych Species Position" = "myc_pos",
                                 "Slash Level" = "Slash",
                                 "Mycorrizal association" = "MT",
                                 "Legacy associtation" = "myc.legacy.type"),
                  selected = "Species"),
      
      selectInput(inputId = "VarY",
                  label = "Select X-axis Variable:",
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
    stanD <- aggregate(dataset[,2] ~ dataset[,1],data = dataset, sd)
    sums2 <- as.data.frame(data_sums[,2])
    
    data_sums[,2:7] <- sums2
    data_sums[,8] <- stanD[,2]
    ggplot()+
      geom_vline(data = data_sums, mapping = aes(xintercept = 0))+
      geom_linerange(data = data_sums, mapping = aes(y = data_sums[,1], 
                                                     xmin = (data_sums[,5] - data_sums[,8]) , 
                                                     xmax = (data_sums[,5] + data_sums[,8]),
                                                     color = data_sums[,1]))+
      geom_point(data = data_sums, mapping = aes(y = data_sums[,1], 
                                                 x = data_sums[,5],
                                                 color = data_sums[,1], 
                                                 size = 4))+
      geom_point(data = data_sums, mapping = aes(y = data_sums[,1], 
                                                 x = (data_sums[,5] - data_sums[,8]),
                                                 color = "yellow",
                                                 size = 3.5))+
      geom_point(data = data_sums, mapping = aes(y = data_sums[,1],
                                                 x = (data_sums[,5] + data_sums[,8]),
                                                 color = "yellow",
                                                 size = 3.5))+
      scale_color_manual(values = cbPalette)+
      theme_minimal()+
      theme(legend.position = "none")+
      theme(text = element_text(size = 20))+
      labs(x = input$VarY, y = input$VarX,)
    
  })
  
})

shinyApp(ui, server)

