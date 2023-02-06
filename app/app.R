# library(shiny)
# library(slickR)
# library(ggplot2)

# seedling_dat <- read.csv("./files/Corinth_seedling_data.csv")
# soil_dat <- read.csv("./files/Soil_data.csv")
# isotope_dat <- read.csv("./files/Datasheet_2021.isotopes.csv")
# nitrate_phosphite_dat <- read.csv("./files/Anion_NO3_PO4_soilnutrients.csv")
# ammonium_dat <- read.csv("./files/Cation_NH4_soilnutrients.csv")

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

seedling_dat$Slash <- factor(seedling_dat$Slash)


ui <- navbarPage(
  theme = bslib::bs_theme(bootswatch = "lux"),
  "HBEF Mycorrhizal Data",   

  # Skeleton for tab panels across top
  
  # Home page with image gallery 
  tabPanel("Home",
           slickROutput("slickr", width = "800px")
           
           
           ),
  
  # Data page with plots
  tabPanel("Data",
           titlePanel("Seedling Plot"),
           
           sidebarLayout(
             sidebarPanel(
               selectInput(inputId = "VarX",
                           label = "Select X-axis Variable:",
                           choices = list("MT",
                                          "Species",
                                          "Treatment",
                                          "Slash",
                                          "Soil")),
               selectInput(inputId = "VarY",
                           label = "Select Y-axis Variable:",
                           choices = list("survival",
                                          "HT.cm",
                                          "Biomass.g")),
               radioButtons("rb", "Choose Display:",
                            choiceNames = list("violin plot",
                                               "bar plot",
                                               "box plot"),
                            choiceValues =list("violin",
                                               "bar",
                                               "box")
               ),
             ),
             
             mainPanel(plotOutput("MychPlot"))
           )
           
           ),
  
  # Tree info page
  tabPanel("Tree Information", "info")
)

server <- function(input, output, session) {
  output$slickr <- renderSlickR({
    imgs <- list.files("C:/Users/17865/OneDrive/Desktop/Senior Year Spring/EDS Capstone/Draft1/img", pattern=".jpg", full.names = TRUE)
    slickR(imgs)+ 
      settings(dots = TRUE, autoplay = TRUE)
  })
  output$MychPlot <- renderPlot({
    
    if (input$rb == "violin") {
      dataset <- seedling_dat[ ,c(input$VarX,input$VarY)]
      ggplot(data = dataset, aes(x = dataset[,1], y = dataset[,2], fill = dataset[,1]))+
        geom_violin()+
        theme_minimal()+
        xlab(input$VarX)+
        ylab(input$VarY)+
        scale_fill_manual(values = cbPalette)
    }
    else if (input$rb == "bar") {
      dataset <- seedling_dat[ ,c(input$VarX,input$VarY)]
      ggplot(data = dataset, aes(x = dataset[,1], y = dataset[,2], fill = dataset[,1]))+
        geom_bar(stat = "identity")+
        theme_minimal()+
        xlab(input$VarX)+
        ylab(input$VarY)+
        scale_fill_manual(values = cbPalette)
    }
    else if (input$rb == "box") {
      dataset <- seedling_dat[ ,c(input$VarX,input$VarY)]
      ggplot(data = dataset, aes(x = dataset[,1], y = dataset[,2], fill = dataset[,1]))+
        geom_boxplot()+
        theme_minimal()+
        xlab(input$VarX)+
        ylab(input$VarY)+
        scale_fill_manual(values = cbPalette)
    }
  },height = 600,width = 800)
  
}


shinyApp(ui = ui, server = server)
