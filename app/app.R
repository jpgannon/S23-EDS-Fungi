library(plotly)
library(dplyr)

# Reading in datasets
# cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# finalgrowth_dat <- read.csv("app/www/files/Final.DataTable.growth.v.15N.csv")
# fullgrowth <- read.csv("app/www/files/FINALfulltable.growth.csv")
# finalsurv_dat <- read.csv("app/www/files/FINALfulltable.survival.csv")
# finalmstr_dat <- read.csv("app/www/files/FINALfulltable.moisture.csv")
# tagid <- read.csv("app/www/files/tagID_byprox.csv")


# Merging data & renaming columns for 3D rendering
merge3d <- merge(finalmstr_dat, finalsurv_dat)
finalmerge3d <- merge(merge3d, fullgrowth)
finalmerge3d <- finalmerge3d %>% 
  mutate(mycspeciesrenamed = case_when( myc.species.type == "AM" ~ "Arbuscular Mycorrhiza",
                            myc.species.type == "EcM" ~ "Ectomycorrhiza"))


# Merge for Rasheed's plots
merge_dat1 <- merge(finalgrowth_dat,finalsurv_dat)

merge_dat2 <- merge(merge_dat1, finalmstr_dat)


# Using navbar page to structure the overall app. This will categorize the app into 
# distinct sections: Home page, data page, tree information page. 
ui <- navbarPage(
  
  # Changing the theme to minimalist black-on-white appearance
  theme = bslib::bs_theme(bootswatch = "lux"),
  "HBEF Mycorrhizal Data",
  
  # CSS sheet for styling text, images, etc
  includeCSS("www/style.css"),
  
  # Home page with image gallery.
  tabPanel("Home",
           
           # Animated welcome text, background gif found in /styles.css
           tags$div(class='sections',id='sections',
                    tags$h2(class = "greeting", "Let's talk trees."),
           ),
           
           # Container for holding the image gallery
           tags$div(class="container",
                    tags$img(id="landingimg",src = "images/2.jpg", width = "375px", height = "350px"),
                    tags$img(id="landingimg",src = "images/4.jpg", width = "375px", height = "350px"),
                    tags$img(id="landingimg",src = "images/1.jpg", width = "375px", height = "350px")),
           tags$h3(id='landingfooter',"This is a collaborative project between the College of Natural Resources and Environment at Virginia Tech and the Ecology, Evolution, Ecosystems and Society Program and Dartmouth College.")
           
           ),
  
  # Data page for navigation between datasets.
  navbarMenu("Data",
             
       
       # Merged dataset subpage will have graphs for all seedling datasets
       tabPanel("Merged Data",
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
                    plotOutput("MergedPlot", width = "1750px")
                  )
                )
                ),
       
       # Real growth 3d scatterplot, ideally would represent a top-down view of the experimental area
       tabPanel("3D Real Growth",
                tags$h1("Real growth visualized in a 3D scatterplot"),
                plotlyOutput("threedplot", height = "600px", width = "1750px"),
                tags$p("")
         
         
       ),
       
       # Real growth 3d surface plot, ideally would represent a top-down view of the experimental area
       tabPanel("Moisture Surface Plot",
                tags$h1("Soil moisture displayed in a 3 dimensional surface plot"),
                plotlyOutput("moistureplot", height = "600px", width = "1750px")
                
                
       )),
  
  # Tree info page, navbar menu for selection between the species catalog and planting recommendations section. 
  navbarMenu("Tree Information",
             
           # Tree catalog subpage that will show fast facts about each species. 
           tabPanel("Tree Catalog",
                    titlePanel("Learn information about different species"),
                    mainPanel(
                      tabsetPanel(
                        id = "species",
                        tabPanel("Northern Red Oak",
                                 tags$h1("Northern Red Oak - Quercus Rubra"),
                                 tags$img(src = "catalog/nroleaf.jpg"),
                                 tags$img(src = "catalog/nro.jpg"),
                                 tags$h2("Mycorrhizal association: Ectomycorrhiza"),
                                 tags$h2("Geographic Range: Eastern United States and Southeastern Canada"),
                                 tags$h2("Status: prominent in Vermont")),
                        tabPanel("Red Maple",
                                 tags$h1("Red Maple - Acer Rubrum"),
                                 tags$img(src = "catalog/rmleaf.jpg"),
                                 tags$img(src = "catalog/rm.jpg"),
                                 tags$h2("Mycorrhizal association: Arbuscular mycorrhiza"),
                                 tags$h2("Geographic Range: Maine west to Minnesota, south to Texas, east to Florida"),
                                 tags$h2("Status: prominent in Vermont")),
                        tabPanel("Sugar Maple",
                                 tags$h1("Sugar Maple - Acer Saccharum"),
                                 tags$img(src = "catalog/smleaf.jpg"),
                                 tags$img(src = "catalog/sm.jpg"),
                                 tags$h2("Mycorrhizal association: Arbuscular mycorrhiza"),
                                 tags$h2("Geographic Range: New England & Mid-Atlantic"),
                                 tags$h2("Status: declining in Vermont")),
                        tabPanel("Sweet Cherry",
                                 tags$h1("Sweet Cherry"),
                                 tags$img(src = "catalog/scleaf.jpg"),
                                 tags$img(src = "catalog/sc.jpg"),
                                 tags$h2("Mycorrhizal association: Arbuscular mycorrhiza"),
                                 tags$h2("Geographic Range: Coastal regions of North America"),
                                 tags$h2("Status: potentially expanding in Vermont")),
                        tabPanel("Blackgum",
                                 tags$h1("Blackgum - Nyssa Sylvatica"),
                                 tags$img(src = "catalog/bgleaf.jpg"),
                                 tags$img(src = "catalog/bg.jpg"),
                                 tags$h2("Mycorrhizal association: Arbuscular mycorrhiza"),
                                 tags$h2("Geographic Range: Southwestern Maine to Central Florida"),
                                 tags$h2("Status: potential for range expansion in Vermont")),
                        tabPanel("Sweet Birch",
                                 tags$h1("Sweet Birch - Betulaceae Lenta"),
                                 tags$img(src = "catalog/sbleaf.jpg"),
                                 tags$img(src = "catalog/sb.jpg"),
                                 tags$h2("Mycorrhizal association: Ectomycorrhiza"),
                                 tags$h2("Geographic Range: Northeastern United States"),
                                 tags$h2("Status: potential for range expansion in Vermont")),
                        tabPanel("American Basswood",
                                 tags$h1("American Basswood - Tilia Americana"),
                                 tags$img(src = "catalog/ableaf.jpg"),
                                 tags$img(src = "catalog/bg.jpg"),
                                 tags$h2("Mycorrhizal association: Arbuscular mycorrhiza"),
                                 tags$h2("Geographic Range: Eastern United States"),
                                 tags$h2("Status: potential for range expansion in Vermont")),
                        tabPanel("Bitternut Hickory",
                                 tags$h1("Bitternut Hickory - Carya Cordiformis"),
                                 tags$img(src = "catalog/bhleaf.jpg"),
                                 tags$img(src = "catalog/bg.jpg"),
                                 tags$h2("Mycorrhizal association: Arbuscular mycorrhiza"),
                                 tags$h2("Geographic Range: Eastern United States"),
                                 tags$h2("Status: prominent in Vermont"))
                      )
                    )
                    
           ),
           
           # Planting recommendations subpage that will customize planting options based on input. 
           tabPanel("Planting Recommendations",
                    tags$h1("Let us help."),
                    tags$p("Here, forest practicioners can enter information about their plots. Please
                            input information regarding logged gap size, basal area, and soil fertility. Based
                            on your inputs, we can recommend what species to plant."),
                    numericInput("gapsize", " Average 15N Value", value = 0, min = 0, max = 100),
                    sliderInput("basalarea", "Basal Area (sq ft/acre)", value = 0, min = 0, max = 43560),
                    numericInput("soilfertility", "Average Soil Fertility of Plot", value = 0, min = 0, max = 100)
                    
           )
  )
)




server <- function(input, output, session) {

  
  # This plot output handles the seedling visualizations using if-statements to customize how it is displayed. 
  output$MergedPlot <- renderPlot({
    
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
  
  # 3D scatterplot, from input. Need to include event generative text panels for clicking, dragging, etc.
  output$threedplot <- renderPlotly({
    plot_ly(finalmerge3d, x = ~Unit, y = ~Plot, z = ~REALGrowth, color =~ mycspeciesrenamed) %>% 
      layout(plot_bgcolor = "#e5ecf6", 
             xaxis = list(title = 'Unit Number', yaxis = list(title = 'Plot Number'),
                          zaxis = list(title = 'Real Growth (cm)'),
                          legend = list(title=list(text='<b> Mycorrhizal Species Association </b>'))))
  })
  
  # 3D soil moisture plot
  output$moistureplot <- renderPlotly({
    #Surface plot
    a <- finalmerge3d %>% 
      select(c(Unit, Plot, REALGrowth))
    surf <- plot_ly(z = ~as.matrix(a))
    surf <- surf %>% add_surface()
  })

}


shinyApp(ui = ui, server = server)
