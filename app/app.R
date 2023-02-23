# library(shiny)
# library(slickR)
# library(ggplot2)
# library(bslib)
# 
# #
# finalgrowth <- read.csv("files/eva/Final.DataTable.growth.v.15N.csv")
# fullgrowth <- read.csv("files/eva/FINALfulltable.growth.csv")
# fullmoisture <- read.csv("files/eva/FINALfulltable.moisture.csv")
# fullsurvival <- read.csv("files/eva/FINALfulltable.survival.csv")


cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# seedling_dat$Slash <- factor(seedling_dat$Slash)

# Using navbar page to structure the overall app. This will categorize the app into 
# distinct sections: Home page, data page, tree information page. 
ui <- navbarPage(
  
  # Changing the theme to minimalist black-on-white appearance
  theme = bslib::bs_theme(bootswatch = "lux"),
  "HBEF Mycorrhizal Data",
  includeCSS("www/style.css"),
  
  # Home page with image gallery. SlickROutput handles the image gallery animation. 
  tabPanel("Home",

           tags$div(class='sections',id='sections',
                    tags$h2(class = "test", "Welcome."),
           ),
           tags$div(class="gallery", id="gallery",
                    tags$img(src = "2.jpg", width = "500px", height = "375px"),
                    tags$img(src = "1.jpg", width = "500px", height = "375px"),
                    tags$img(src = "4.jpg", width = "500px", height = "375px"),
                    # tags$img(src = "4.jpg", width = "400px", height = "400px")
           ),
           # tags$h1(class = "intro", "This is a collaborative project between Virginia Tech CNRE and the Ecology, Evolution, Ecosystems and Society Program at Dartmouth College"),
           )
  ,
  
  
  # Data page for navigation between datasets.
  navbarMenu("Data",
             
       # Subpages within the data page are seedling data, isotope data and anion data. 
       # Each subpage should have its own graphs. 
       
       # Seedling subpage will have graphs for all seedling datasets
       tabPanel("Seedling Data",
                tabPanel("Seedling Plot",),
                
                sidebarLayout(
                  sidebarPanel(
                    selectInput(inputId = "seedlingVarX",
                                label = "Select X-axis Variable:",
                                choices = list("MT",
                                               "Species",
                                               "Treatment",
                                               "Slash",
                                               "Soil")),
                    selectInput(inputId = "seedlingVarY",
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
                  
                  mainPanel(plotOutput("seedlingplot"))
                )
          
                ),
       
       tabPanel("Soil Data",
                sidebarLayout(
                  sidebarPanel(
                    selectInput(inputId = "soilVarX",
                                label = "Select X-axis Variable:",
                                choices = list("MT",
                                               "Treatment",
                                               "pH",
                                               "per.C",
                                               "per.N")),
                    selectInput(inputId = "soilVarY",
                                label = "Select Y-axis Variable:",
                                choices = list("Root.wt.g",
                                               "Bulk.density")),
                    radioButtons("rb", "Choose Display:",
                                 choiceNames = list("violin plot",
                                                    "bar plot",
                                                    "box plot",
                                                    "Scatter Plot"),
                                 choiceValues =list("violin",
                                                    "bar",
                                                    "box",
                                                    "scatter")
                    ),
                  ),
                  mainPanel(
                    plotOutput("soilplot")
                  )
                )
                ),
       
       tabPanel("Isotope Data",
                sidebarLayout(
                  sidebarPanel(
                    selectInput(inputId = "isoVarX",
                                label = "Select X-axis Variable:",
                                choices = list("Species.Myc.Type",
                                               "Species",
                                               "Position")),
                    selectInput(inputId = "isoVarY",
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
                    plotOutput("IsoPlot")
                  )
                )
                )
       ),
  
  # Tree info page, navbar menu for selection between the species catalog and planting recommendations section. 
  navbarMenu("Tree Information",
             
           # Tree catalog subpage that will show fast facts about each species. 
           tabPanel("Tree Catalog",
           ),
           
           # Planting recommendations subpage that will customize planting options based on input. 
           tabPanel("Planting Recommendations"
           )
  )
)


server <- function(input, output, session) {
  
  # This output handles the image gallery on the homepage. 
  output$slickr <- renderSlickR({
    imgs <- list.files("C:/Users/17865/OneDrive/Desktop/Senior Year Spring/EDS Capstone/S23-EDS-Fungi/photos/resized", pattern=".jpg", full.names = TRUE)
    slickR(imgs) + settings(dots = TRUE, autoplay = TRUE, autoplaySpeed = 2500)
   
  })
  
  # This plot output handles the seedling visualizations using if-statements to customize how it is displayed. 
  output$seedlingplot <- renderPlot({
    
    if (input$rb == "violin") {
      dataset <- seedling_dat[ ,c(input$seedlingVarX,input$seedlingVarY)]
      ggplot(data = dataset, aes(x = dataset[,1], y = dataset[,2], fill = dataset[,1]))+
        geom_violin()+
        theme_minimal()+
        xlab(input$VarX)+
        ylab(input$VarY)+
        scale_fill_manual(values = cbPalette)
    }
    else if (input$rb == "bar") {
      dataset <- seedling_dat[ ,c(input$seedlingVarX,input$seedlingVarY)]
      ggplot(data = dataset, aes(x = dataset[,1], y = dataset[,2], fill = dataset[,1]))+
        geom_bar(stat = "identity")+
        theme_minimal()+
        xlab(input$VarX)+
        ylab(input$VarY)+
        scale_fill_manual(values = cbPalette)
    }
    else if (input$rb == "box") {
      dataset <- seedling_dat[ ,c(input$seedlingVarX,input$seedlingVarY)]
      ggplot(data = dataset, aes(x = dataset[,1], y = dataset[,2], fill = dataset[,1]))+
        geom_boxplot()+
        theme_minimal()+
        xlab(input$VarX)+
        ylab(input$VarY)+
        scale_fill_manual(values = cbPalette)
    }
  },height = 600,width = 800)

  
  output$soilplot <- renderPlot({
    if (input$rb == "violin") {
      dataset <- soil_dat[ ,c(input$soilVarX,input$soilVarY)]
      ggplot(data = soil_dat, aes(x = dataset[,1], y = dataset[,2], fill = dataset[,1]))+
        geom_violin()+
        theme_minimal()+
        xlab(input$VarX)+
        ylab(input$VarY)+
        scale_fill_manual(values = cbPalette)
    }
    else if (input$rb == "bar") {
      dataset <- soil_dat[ ,c(input$soilVarX,input$soilVarY)]
      ggplot(data = dataset, aes(x = dataset[,1], y = dataset[,2], fill = dataset[,1]))+
        geom_bar(stat = "identity")+
        theme_minimal()+
        xlab(input$VarX)+
        ylab(input$VarY)+
        scale_fill_manual(values = cbPalette)
    }
    else if (input$rb == "box") {
      dataset <- soil_dat[ ,c(input$soilVarX,input$soilVarY)]
      ggplot(data = dataset, aes(x = dataset[,1], y = dataset[,2], fill = dataset[,1]))+
        geom_boxplot()+
        theme_minimal()+
        xlab(input$VarX)+
        ylab(input$VarY)+
        scale_fill_manual(values = cbPalette)
    }
    else if (input$rb == "scatter") {
      dataset <- soil_dat[ ,c(input$soilVarX,input$soilVarY)]
      ggplot(data = dataset, aes(x = dataset[,1], y = dataset[,2]))+
        geom_point()+
        theme_minimal()+
        xlab(input$VarX)+
        ylab(input$VarY)
    }
  },height = 400,width = 600)
  
  output$IsoPlot <- renderPlot({
    
    if (input$rb == "violin") {
      dataset <- isotope_dat[ ,c(input$isoVarX,input$isoVarY)]
      ggplot(data = dataset, aes(x = dataset[,1], y = dataset[,2], fill = dataset[,1]))+
        geom_violin()+
        theme_minimal()+
        xlab(input$VarX)+
        ylab(input$VarY)+
        scale_fill_manual(values = cbPalette)
    }
    else if (input$rb == "bar") {
      dataset <- isotope_dat[ ,c(input$isoVarX,input$isoVarY)]
      ggplot(data = dataset, aes(x = dataset[,1], y = dataset[,2], fill = dataset[,1]))+
        geom_bar(stat = "identity")+
        theme_minimal()+
        xlab(input$VarX)+
        ylab(input$VarY)+
        scale_fill_manual(values = cbPalette)
    }
    else if (input$rb == "box") {
      dataset <- isotope_dat[ ,c(input$isoVarX,input$isoVarY)]
      ggplot(data = dataset, aes(x = dataset[,1], y = dataset[,2], fill = dataset[,1]))+
        geom_boxplot()+
        theme_minimal()+
        xlab(input$VarX)+
        ylab(input$VarY)+
        scale_fill_manual(values = cbPalette)
    }
  },height = 400,width = 600)
  
  # Oak tree image rendering in the tree catalog
  output$oakimage <- renderImage({
    filename <- normalizePath(file.path('C:/Users/17865/OneDrive/Desktop/Senior Year Spring/EDS Capstone/S23-EDS-Fungi/trees',
                                        paste('oak', input$n, '.jpg', sep='')))
    list(src = filename)
  }, deleteFile = FALSE)
  
  
  # Maple tree image rendering in the tree catalog
  output$mapleimage <- renderImage({
    filename <- normalizePath(file.path('C:/Users/17865/OneDrive/Desktop/Senior Year Spring/EDS Capstone/S23-EDS-Fungi/trees',
                                        paste('maple', input$n, '.jpg', sep='')))
    list(src = filename)
  }, deleteFile = FALSE)
  
  
  # Cherry tree image rendering in the tree catalog
  output$cherryimage <- renderImage({
    filename <- normalizePath(file.path('C:/Users/17865/OneDrive/Desktop/Senior Year Spring/EDS Capstone/S23-EDS-Fungi/trees',
                                        paste('cherry', input$n, '.jpg', sep='')))
    list(src = filename)
  }, deleteFile = FALSE)
  
  # Reactive Expression to generate information for the tree catalog based on the species tab
  output$speciesdesc <- renderText(string())
  string <- reactive(paste0("The ", input$species, " is native to different parts of Vermont"))
  
  
  description <- reactive({
    filter(catalog,
           name == catalog$CommonName,
           assoc == catalog$MycorrhizalAssociation,
           status == catalog$Status)
  })
}


shinyApp(ui = ui, server = server)
