library(plotly)
library(dplyr)

# Reading in datasets
 cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#669919", "#001600")
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

merge_dat4 <- read.csv("https://raw.githubusercontent.com/jpgannon/S23-EDS-Fungi/main/myc_mergeDat.csv")

merge_dat4$Slash <- as.character(merge_dat4$Slash)

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
           tags$div(class="gallery",
                    tags$img(id="landingimg",src = "images/2.jpg"),
                    tags$img(id="landingimg",src = "images/1.jpg"),
                    tags$img(id="landingimg",src = "images/4.jpg")),
           tags$h3(id='landingfooter',"This is a collaborative project between the College of Natural Resources and Environment at Virginia Tech and the Ecology, Evolution, Ecosystems and Society Program and Dartmouth College.")
           
           ),
  
  # Data page for navigation between datasets.
  navbarMenu("Data",
             
       
       # Merged dataset subpage will have graphs for all seedling datasets
       tabPanel("Merged Data",
                titlePanel("Create your plot with the options below"),
                
                sidebarLayout(
                  sidebarPanel(
                    selectInput(inputId = "mergeVarX",
                                label = "Select X-axis Variable:",
                                choices = list("Species",
                                               "myc.species.type",
                                               "myc.legacy.type",
                                               "Position")),
                    selectInput(inputId = "mergeVarY",
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
                ),
                tags$br(),
                tags$br(),
                tags$p("Select between different variables for the X and Y axes. The data incorporated in the graphs contain information 
                       about seedling, survival, growth, moisture, soil, and isotope data. Consists of numeric and categorical data with 
                       key characteristics including tree proximity, unit number, plot number, tree species, tag ID, and treatment.")
                ),
       
       # Real growth 3d surface plot, ideally would represent a top-down view of the experimental area
       tabPanel("Line Range Plot",
                titlePanel("Linerange Plot"),
                
                sidebarLayout(
                  sidebarPanel(
                    selectInput(inputId = "VarX",
                                label = "Select X-axis Variable:",
                                choices = list("Species" = "Species.x",
                                               "Legacy Plot Position" = "leg_pos",
                                               "Mych Species Position" = "myc_pos",
                                               "Slash Level" = "Slash",
                                               "Mycorrizal association" = "MT",
                                               "Legacy associtation" = "myc.legacy.type"),
                                selected = "Species"),
                    
                    selectInput(inputId = "VarY",
                                label = "Select Y-axis Variable:",
                                choices = list("Survival rate" = "Survival",
                                               "Growth Percent Change" = "PCG"),
                                selected = "Growth Percent Change")          
                  ),
                  
                  mainPanel(
                    plotOutput("linerangeplot")
                  )
                ),
                tags$br(),
                tags$br(),
                tags$p("Select between different variables for the X and Y axes. The data provides an indication of how different species fare in
                       different locations. Choose between a variety of factors in visualizing species growth percent change or survival.")
                
       ),
       
       # Real growth 3d scatterplot, ideally would represent a top-down view of the experimental area
       tabPanel("3D Real Growth",
                tags$h1("Real growth visualized in a 3D scatterplot"),
                plotlyOutput("threedplot", height = "600px", width = "1750px"),
                tags$p("")
                
                
       )),
  
  # Tree info page, navbar menu for selection between the species catalog and planting recommendations section. 
  navbarMenu("Tree Information",
             
           # Tree catalog subpage that will show fast facts about each species. 
           tabPanel("Tree Catalog",
                    titlePanel("Learn information about different species"),
                      tabsetPanel(
                        id = "species",
                        tabPanel("Northern Red Oak",
                                 tags$h1("Northern Red Oak - Quercus Rubra"),
                                 tags$img(class = "ai", src = "ai/nroleaf.webp"),
                                 tags$img(class = "ai", src = "ai/nro.webp"),
                                 tags$h2("Mycorrhizal association: Ectomycorrhiza"),
                                 tags$h2("Geographic Range: Eastern United States and Southeastern Canada"),
                                 tags$h2("Status: prominent in Vermont")),
                        tabPanel("Red Maple",
                                 tags$h1("Red Maple - Acer Rubrum"),
                                 tags$img(class = "ai", src = "ai/rmleaf.webp"),
                                 tags$img(class = "ai", src = "ai/rm.webp"),
                                 tags$h2("Mycorrhizal association: Arbuscular mycorrhiza"),
                                 tags$h2("Geographic Range: Maine west to Minnesota, south to Texas, east to Florida"),
                                 tags$h2("Status: prominent in Vermont")),
                        tabPanel("Sugar Maple",
                                 tags$h1("Sugar Maple - Acer Saccharum"),
                                 tags$img(class = "ai", src = "ai/smleaf.webp"),
                                 tags$img(class = "ai", src = "ai/sm.webp"),
                                 tags$h2("Mycorrhizal association: Arbuscular mycorrhiza"),
                                 tags$h2("Geographic Range: New England & Mid-Atlantic"),
                                 tags$h2("Status: declining in Vermont")),
                        tabPanel("Sweet Cherry",
                                 tags$h1("Sweet Cherry - Prunus Avium"),
                                 tags$img(class = "ai", src = "ai/scleaf.webp"),
                                 tags$img(class = "ai", src = "ai/sc.webp"),
                                 tags$h2("Mycorrhizal association: Arbuscular mycorrhiza"),
                                 tags$h2("Geographic Range: Coastal regions of North America"),
                                 tags$h2("Status: potentially expanding in Vermont")),
                        tabPanel("Blackgum",
                                 tags$h1("Blackgum - Nyssa Sylvatica"),
                                 tags$img(class = "ai", src = "ai/bgleaf.webp"),
                                 tags$img(class = "ai", src = "ai/bg.webp"),
                                 tags$h2("Mycorrhizal association: Arbuscular mycorrhiza"),
                                 tags$h2("Geographic Range: Southwestern Maine to Central Florida"),
                                 tags$h2("Status: potential for range expansion in Vermont")),
                        tabPanel("Sweet Birch",
                                 tags$h1("Sweet Birch - Betulaceae Lenta"),
                                 tags$img(class = "ai", src = "ai/sbleaf.webp"),
                                 tags$img(class = "ai", src = "ai/sb.webp"),
                                 tags$h2("Mycorrhizal association: Ectomycorrhiza"),
                                 tags$h2("Geographic Range: Northeastern United States"),
                                 tags$h2("Status: potential for range expansion in Vermont")),
                        tabPanel("American Basswood",
                                 tags$h1("American Basswood - Tilia Americana"),
                                 tags$img(class = "ai", src = "ai/ableaf.webp"),
                                 tags$img(class = "ai", src = "ai/bg.webp"),
                                 tags$h2("Mycorrhizal association: Arbuscular mycorrhiza"),
                                 tags$h2("Geographic Range: Eastern United States"),
                                 tags$h2("Status: potential for range expansion in Vermont")),
                        tabPanel("Bitternut Hickory",
                                 tags$h1("Bitternut Hickory - Carya Cordiformis"),
                                 tags$img(class = "ai", src = "ai/bhleaf.webp"),
                                 tags$img(class = "ai", src = "ai/bh.webp"),
                                 tags$h2("Mycorrhizal association: Arbuscular mycorrhiza"),
                                 tags$h2("Geographic Range: Eastern United States"),
                                 tags$h2("Status: prominent in Vermont"))
                      ),
                    tags$br(),
                    tags$br(),
                    tags$h3("Images featured in the tree information catalog were generated from Craiyon, an AI model image generator.")
                    
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
      dataset <- merge_dat2[ ,c(input$mergeVarX,input$mergeVarY)]
      ggplot(data = dataset, aes(x = dataset[,1], y = dataset[,2], fill = dataset[,1]))+
        geom_violin()+
        theme_minimal()+
        xlab(input$VarX)+
        ylab(input$VarY)+
        scale_fill_manual(values = cbPalette)
    }
    else if (input$rb == "bar") {
      dataset <- merge_dat2[ ,c(input$mergeVarX,input$mergeVarY)]
      ggplot(data = dataset, aes(x = dataset[,1], y = dataset[,2],fill = dataset[,1]))+
        geom_bar(stat = "summary", position = "dodge", fun = "mean")+
        theme_minimal()+
        xlab(input$VarX)+
        ylab(input$VarY)+
        scale_fill_manual(values = cbPalette)
    }
    else if (input$rb == "box") {
      dataset <- merge_dat2[ ,c(input$mergeVarX,input$mergeVarY)]
      ggplot(data = dataset, aes(x = dataset[,1], y = dataset[,2], fill = dataset[,1]))+
        geom_boxplot()+
        theme_minimal()+
        xlab(input$VarX)+
        ylab(input$VarY)+
        scale_fill_manual(values = cbPalette)
    }
    else if (input$rb == "scatter") {
      dataset <- merge_dat2[ ,c(input$mergeVarX,input$mergeVarY)]
      ggplot(data = dataset, aes(x = dataset[,1], y = dataset[,2]))+
        geom_point()+
        theme_minimal()+
        xlab(input$VarX)+
        ylab(input$VarY)
    }
  },height = 400,width = 600)
  
  # Line Range Plot
  output$linerangeplot <- renderPlot({
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
      labs(x = input$VarX, y = input$VarY)
    
  })
  
  # 3D scatterplot, from input. Need to include event generative text panels for clicking, dragging, etc.
  output$threedplot <- renderPlotly({
    plot_ly(finalmerge3d, x = ~Unit, y = ~Plot, z = ~REALGrowth, color =~ mycspeciesrenamed) %>% 
      layout(plot_bgcolor = "#e5ecf6", 
             xaxis = list(title = 'Unit Number', yaxis = list(title = 'Plot Number'),
                          zaxis = list(title = 'Real Growth (cm)'),
                          legend = list(title=list(text='<b> Mycorrhizal Species Association </b>'))))
  })

}


shinyApp(ui = ui, server = server)
