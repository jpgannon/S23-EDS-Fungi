library(shiny)
library(ggplot2)

soil_dat$pH <-as.numeric(soil_dat$pH)

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ui <- fluidPage(
  titlePanel("Isotope Plot"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "VarX",
                  label = "Select X-axis Variable:",
                  choices = list("Species.Myc.Type",
                                 "Species",
                                 "Position")),
      selectInput(inputId = "VarY",
                  label = "Select Y-axis Variable:",
                  choices = list("corrected.percent.C",
                                 "corrected.percent.N")),
      radioButtons("rb", "Choose Display:",
                   choiceNames = list("violin plot",
                                      "bar plot",
                                      "box plot"),
                   choiceValues =list("violin",
                                      "bar",
                                      "box")
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
      dataset <- isotope_dat[ ,c(input$VarX,input$VarY)]
      ggplot(data = dataset, aes(x = dataset[,1], y = dataset[,2], fill = dataset[,1]))+
        geom_violin()+
        theme_minimal()+
        xlab(input$VarX)+
        ylab(input$VarY)+
        scale_fill_manual(values = cbPalette)
    }
    else if (input$rb == "bar") {
      dataset <- isotope_dat[ ,c(input$VarX,input$VarY)]
      ggplot(data = dataset, aes(x = dataset[,1], y = dataset[,2], fill = dataset[,1]))+
        geom_bar(stat = "identity")+
        theme_minimal()+
        xlab(input$VarX)+
        ylab(input$VarY)+
        scale_fill_manual(values = cbPalette)
    }
    else if (input$rb == "box") {
      dataset <- isotope_dat[ ,c(input$VarX,input$VarY)]
      ggplot(data = dataset, aes(x = dataset[,1], y = dataset[,2], fill = dataset[,1]))+
        geom_boxplot()+
        theme_minimal()+
        xlab(input$VarX)+
        ylab(input$VarY)+
        scale_fill_manual(values = cbPalette)
    }
  },height = 400,width = 600)
})

shinyApp(ui, server)
