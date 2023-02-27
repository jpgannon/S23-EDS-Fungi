finalgrowth_dat <- read.csv("https://raw.githubusercontent.com/jpgannon/S23-EDS-Fungi/main/files/Final.DataTable.growth.v.15N.csv")
finalsurv_dat <- read.csv("https://raw.githubusercontent.com/jpgannon/S23-EDS-Fungi/main/files/FINALfulltable.survival.csv")
finalmstr_dat <- read.csv("https://raw.githubusercontent.com/jpgannon/S23-EDS-Fungi/main/files/FINALfulltable.moisture.csv")

merge_dat1 <- merge(finalgrowth_dat,finalsurv_dat)

merge_dat2 <- merge(merge_dat1, finalmstr_dat)


library(shiny)
library(ggplot2)

seedling_dat$Slash <- factor(seedling_dat$Slash)

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ui <- fluidPage(
  titlePanel("Create your plot with the options below"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "VarX",
                  label = "Select X-axis Variable:",
                  choices = list("Species",
                                 "myc.species.type",
                                 "myc.legacy.type",
                                 "Position")),
      selectInput(inputId = "VarY",
                  label = "Select Y-axis Variable:",
                  choices = list("Survival",
                                 "REALGrowth",
                                 "Moisture",
                                 "N15.corrected")),
      radioButtons("rb", "Choose Display:",
                   choiceNames = list("violin plot",
                                      "bar plot",
                                      "box plot",
                                      "Scatter plot"),
                   choiceValues =list("violin",
                                      "bar",
                                      "box",
                                      "scatter")
      ),
    ),
    
    mainPanel(
      plotOutput("MychPlot")
    )
  )
)


server <- (function(input, output,session) {
  
  output$MychPlot <- renderPlot({
    
    if (input$rb == "violin") {
      dataset <- merge_dat2[ ,c(input$VarX,input$VarY)]
      ggplot(data = dataset, aes(x = dataset[,1], y = dataset[,2], fill = dataset[,1]))+
        geom_violin()+
        theme_minimal()+
        xlab(input$VarX)+
        ylab(input$VarY)+
        scale_fill_manual(values = cbPalette)
    }
    else if (input$rb == "bar") {
      dataset <- merge_dat2[ ,c(input$VarX,input$VarY)]
      ggplot(data = dataset, aes(x = dataset[,1], y = dataset[,2],fill = dataset[,1]))+
        geom_bar(stat = "summary", position = "dodge", fun = "mean")+
        theme_minimal()+
        xlab(input$VarX)+
        ylab(input$VarY)+
        scale_fill_manual(values = cbPalette)
    }
    else if (input$rb == "box") {
      dataset <- merge_dat2[ ,c(input$VarX,input$VarY)]
      ggplot(data = dataset, aes(x = dataset[,1], y = dataset[,2], fill = dataset[,1]))+
        geom_boxplot()+
        theme_minimal()+
        xlab(input$VarX)+
        ylab(input$VarY)+
        scale_fill_manual(values = cbPalette)
    }
    else if (input$rb == "scatter") {
      dataset <- merge_dat2[ ,c(input$VarX,input$VarY)]
      ggplot(data = dataset, aes(x = dataset[,1], y = dataset[,2]))+
        geom_point()+
        theme_minimal()+
        xlab(input$VarX)+
        ylab(input$VarY)
    }
  },height = 400,width = 600)
})

shinyApp(ui, server)
