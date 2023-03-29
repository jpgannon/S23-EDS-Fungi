library(plotly)
library(dplyr)
library(rsconnect)


# Reading in datasets
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#669919", "#001600")
finalgrowth_dat <- read.csv("www/files/Final.DataTable.growth.v.15N.csv")
finalgrowth <- read.csv("www/files/Final.DataTable.growth.v.15N.csv")
finalsurv_dat <- read.csv("www/files/FINALfulltable.survival.csv")
finalmstr_dat <- read.csv("www/files/FINALfulltable.moisture.csv")
tagid <- read.csv("www/files/tagID_byprox.csv")

# Merge for Rasheed's plots
merge_dat1 <- merge(finalgrowth_dat,finalsurv_dat)
merge_dat2 <- merge(merge_dat1, finalmstr_dat)
merge_dat4 <- read.csv("https://raw.githubusercontent.com/jpgannon/S23-EDS-Fungi/main/Merged_data4.csv")
merge_dat4$Slash <- as.character(merge_dat4$Slash)


# Using navbar page to structure the overall app. This will categorize the app into 
# distinct sections: Home page, data page, tree information page. 
ui <- navbarPage(
  
  # Changing the theme to minimalist black-on-white appearance
  theme = bslib::bs_theme(bootswatch = "lux"),
  "HBEF Mycorrhizal Data",
  
  # CSS sheet for styling text, images, etc
  includeCSS("www/style.css"),
  
  # Home page with image gallery, project context, experiment setup. 
  tabPanel("Home",
           
           # Main landing gallery with welcome text. 
           tags$section(
           tags$h1(class='greeting', "Let's Talk Trees."),
           tags$div(class="gallery", id="gallery",
                    tags$img(src = "images/2.webp", height = "16em"),
                    tags$img(src = "images/1.webp", height = "16em"),
                    tags$img(src = "images/7.webp", height = "16em"),
           ),
           tags$h2('This is a collaborative project between the College of 
                  Natural Resources and Environment at Virginia Tech and the Ecology, 
                  Evolution, Ecosystems and Society Program and Dartmouth College.'),
           tags$br(),tags$br()),
           
           
           # Blurb about project context, image from previous article on Corinth, VT research. 
           tags$section(
           tags$h1(class='greeting','About the Project'),
           tags$h3(class='blurb', "As Virginia Tech undergraduates majoring in Environmental Data Science,
                   our team was tasked with creating an R-Shiny app of effective data analysis for our
                   capstone project. Our group worked with scientists at Dartmouth College to investigate 
                   how post-logging legacy mycorrhizal fungi can affect seedling regeneration. The app 
                   data was collected at Clement Woodlot in Corinth, Vermont. The study itself was 
                   conducted under the Adaptive Silviculture for Climate Change Project, led by
                   Anthony D'Amato Ph.D."),
           tags$figure(class="map",
             tags$img(src = "images/6.webp", height = "16em"),
             tags$figcaption("Figure 1. Forest ecology researchers at a site in Corinth, Vermont"),
             tags$br(),tags$br()
           )),
           
           
           # Blurb on project design, image included from Eva's research poster. 
           tags$section(
             tags$h1(class='greeting','Context & Design'),
             tags$h3(class='blurb', "Throughout natural history, trees and mycorrhizal fungi have coevolved
                    in a symbiotic relationship. Trees provide mycorrhizae with photosynthesized sugars, which in
                    turn assist trees in uptake of nutrients like nitrogen and phosphorous. For project setup, eight
                    quarter-acre plots in Corinth, VT were harvested in the winter of 2020-2021. Of these eight plots, 
                    half are dominated by Ectomycorrhizal associated (EcM-Legacy) trees, while the other half
                    are dominated by by Arbuscular mycorrhizal associated (AM-Legacy) trees. Within these recently logged
                    plots, 20 seedlings of eight different species were planted. To quantify seedling survival and growth
                    between the EcM-Legacy and AM-Legacy plots, several factors were measured. Navigate to the data tab to 
                    see multiple data visualizations methods of these factors. "),
             tags$figure(class="map",
                         tags$img(src = "images/9.webp", height = "16em"),
                         tags$figcaption("Figure 2. Schematic of project design within greater Vermont area."),
                         tags$br(),tags$br()
             ))
           ),
           
  
  # Data page for navigation between datasets.
  navbarMenu("Data",
             
       
       # Merged dataset subpage will have graphs for all seedling datasets
       tabPanel("Customizable Data Visualization",
                titlePanel("Create your plot with the options below"),
                
                sidebarLayout(
                  sidebarPanel(
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
                                               "N15.corrected"))
                  ),
                  
                  mainPanel(
                    plotOutput("MergedPlot"),
                    tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),
                    tags$h3(textOutput("test")),
                  )
                )),
       
       # Line range plot to display both mean and standard deviation of real growth and survival.
       tabPanel("Growth & Survival Line Range",
                
                titlePanel("Growth & Survival Line Range"),
                
                sidebarLayout(
                  sidebarPanel(
                    selectInput(inputId = "rangeVarX",
                                label = "Select Y-axis Variable:",
                                choices = list("Species" = "Species.x",
                                               "Legacy Plot Position" = "leg_pos",
                                               "Mych Species Position" = "myc_pos",
                                               "Slash Level" = "Slash",
                                               "Mycorrizal association" = "MT",
                                               "Legacy associtation" = "myc.legacy.type"),
                                selected = "Species"),
                    
                    selectInput(inputId = "rangeVarY",
                                label = "Select X-axis Variable:",
                                choices = list("Survival rate" = "Survival",
                                               "Growth Percent Change" = "PCG"),
                                selected = "Growth Percent Change")          
                  ),
                  
                  mainPanel(
                    plotOutput("rangePlot")
                  )
                )
                
       ),
       
       # Planting recommendations subpage that will customize planting options based on input. 
       tabPanel("Planting Recommendations",
                titlePanel("Let us help"
                ),
                sidebarLayout(
                  sidebarPanel(
                    tags$h3("Select the parameters that most accurately describe the planting area:"),
                    selectInput("Moisture", "Soil Moisture",
                                choices = c("Wet", "Moderate", "Dry")),
                    selectInput("Chemistry", "Soil Chemistry", 
                                choices = c("Acidic", "Alkaline")),
                    selectInput("Shade", "Sunlight Patterns", 
                                choices = c("Moderate Shade", "Minimal Shade"))
                  ),
                  mainPanel(
                    tags$h1("We recommend you plant..."),
                    uiOutput("recommendation"),
                  )
                ),
                tags$br(),
                tags$h5("Ideal tree condition information was gathered from the Virginia Tech Dendrology 
                        Textbook, the work of Dr. John Seiler, John Peterson, and many others.")
                
                
       )
       ),
  
           # Tree catalog subpage that will show fast facts about each species, along with AI pictures and a map.
       tabPanel("Tree Information Catalog",
                titlePanel("Learn information about different species"),
                  tabsetPanel(
                    id = "species",
                    tabPanel("Northern Red Oak",
                             tags$h2("Northern Red Oak - Quercus Rubra"),
                             tags$div(class='catalog',
                               tags$img(class = "ai", src = "images/ai/nroleaf.webp"),
                               tags$img(class = "ai", src = "images/ai/nromap.webp"),
                               tags$img(class = "ai", src = "images/ai/nro.webp"),
                               ),
                             tags$h2("Mycorrhizal association: Ectomycorrhiza"),
                             tags$h2("Geographic Range: Eastern United States and Southeastern Canada"),
                             tags$h2("Status: prominent in Vermont")
                             ),
                    tabPanel("Red Maple",
                             tags$div(class='catalog',
                               tags$h2("Red Maple - Acer Rubrum"),
                               tags$img(class = "ai", src = "images/ai/rmleaf.webp"),
                               tags$img(class = "ai", src = "images/ai/rmmap.webp"),
                               tags$img(class = "ai", src = "images/ai/rm.webp"),
                               tags$h2("Mycorrhizal association: Arbuscular mycorrhiza"),
                               tags$h2("Geographic Range: Maine west to Minnesota, south to Texas, east to Florida"),
                               tags$h2("Status: prominent in Vermont")
                               )
                             ),
                    tabPanel("Sugar Maple",
                             tags$div(class='catalog',
                               tags$h2("Sugar Maple - Acer Saccharum"),
                               tags$img(class = "ai", src = "images/ai/smleaf.webp"),
                               tags$img(class = "ai", src = "images/ai/smmap.webp"),
                               tags$img(class = "ai", src = "images/ai/sm.webp"),
                               tags$h2("Mycorrhizal association: Arbuscular mycorrhiza"),
                               tags$h2("Geographic Range: New England & Mid-Atlantic"),
                               tags$h2("Status: declining in Vermont")
                               )
                             ),
                    tabPanel("Sweet Cherry",
                             tags$div(class='catalog',
                               tags$h2("Sweet Cherry - Prunus Avium"),
                               tags$img(class = "ai", src = "images/ai/scleaf.webp"),
                               tags$img(class = "ai", src = "images/ai/scmap.webp"),
                               tags$img(class = "ai", src = "images/ai/sc.webp"),
                               tags$h2("Mycorrhizal association: Arbuscular mycorrhiza"),
                               tags$h2("Geographic Range: Coastal regions of North America"),
                               tags$h2("Status: potentially expanding in Vermont")
                               )
                             ),
                    tabPanel("Blackgum",
                             tags$div(class='catalog',
                               tags$h2("Blackgum - Nyssa Sylvatica"),
                               tags$img(class = "ai", src = "images/ai/bgleaf.webp"),
                               tags$img(class = "ai", src = "images/ai/bgmap.webp"),
                               tags$img(class = "ai", src = "images/ai/bg.webp"),
                               tags$h2("Mycorrhizal association: Arbuscular mycorrhiza"),
                               tags$h2("Geographic Range: Southwestern Maine to Central Florida"),
                               tags$h2("Status: potential for range expansion in Vermont")
                               )
                             ),
                    tabPanel("Sweet Birch",
                             tags$div(class='catalog',
                               tags$h2("Sweet Birch - Betulaceae Lenta"),
                               tags$img(class = "ai", src = "images/ai/sbleaf.webp"),
                               tags$img(class = "ai", src = "images/ai/sbmap.webp"),
                               tags$img(class = "ai", src = "images/ai/sb.webp"),
                               tags$h2("Mycorrhizal association: Ectomycorrhiza"),
                               tags$h2("Geographic Range: Northeastern United States"),
                               tags$h2("Status: potential for range expansion in Vermont")
                               )
                             ),
                    tabPanel("American Basswood",
                             tags$div(class='catalog',
                               tags$h2("American Basswood - Tilia Americana"),
                               tags$img(class = "ai", src = "images/ai/ableaf.webp"),
                               tags$img(class = "ai", src = "images/ai/abmap.webp"),
                               tags$img(class = "ai", src = "images/ai/bg.webp"),
                               tags$h2("Mycorrhizal association: Arbuscular mycorrhiza"),
                               tags$h2("Geographic Range: Eastern United States"),
                               tags$h2("Status: potential for range expansion in Vermont")
                               )
                             ),
                    tabPanel("Bitternut Hickory",
                             tags$div(class='catalog',
                               tags$h2("Bitternut Hickory - Carya Cordiformis"),
                               tags$img(class = "ai", src = "images/ai/bhleaf.webp"),
                               tags$img(class = "ai", src = "images/ai/bhmap.webp"),
                               tags$img(class = "ai", src = "images/ai/bh.webp"),
                               tags$h2("Mycorrhizal association: Arbuscular mycorrhiza"),
                               tags$h2("Geographic Range: Eastern United States"),
                               tags$h2("Status: prominent in Vermont"))
                    )
                  ),
                tags$h5("Images featured in the tree information catalog were generated from Craiyon, an AI model image generator.
                        Species range maps were obtained from Virginia Tech's Dendrology course online syllabus.")
         )
  )




server <- function(input, output, session) {

  
  # This output handles the plotting of the merged dataset, customized based on user input
  output$MergedPlot <- renderPlot({
    
    if (input$rb == "violin") {
      dataset <- merge_dat2[ ,c(input$mergeVarX,input$mergeVarY)]
      ggplot(data = dataset, aes(x = dataset[,1], y = dataset[,2], fill = dataset[,1]))+
        geom_violin()+
        theme_minimal()+
        xlab(element_blank())+
        theme(legend.position = "none")+
        theme(text = element_text(size = 20))+
        ylab(input$mergeVarY)+
        scale_fill_manual(values = cbPalette)+
        guides(color = guide_legend(title = "Users By guides"))
    }
    else if (input$rb == "bar") {
      dataset <- merge_dat2[ ,c(input$mergeVarX,input$mergeVarY)]
      ggplot(data = dataset, aes(x = dataset[,1], y = dataset[,2],fill = dataset[,1]))+
        geom_bar(stat = "summary", position = "dodge", fun = "mean")+
        theme_minimal()+
        xlab(element_blank())+
        theme(legend.position = "none")+
        theme(text = element_text(size = 20))+
        ylab(input$mergeVarY)+
        scale_fill_manual(values = cbPalette)
    }
    else if (input$rb == "box") {
      dataset <- merge_dat2[ ,c(input$mergeVarX,input$mergeVarY)]
      ggplot(data = dataset, aes(x = dataset[,1], y = dataset[,2], fill = dataset[,1]))+
        geom_boxplot()+
        theme_minimal()+
        theme(legend.position = "none")+
        theme(text = element_text(size = 20))+
        xlab(element_blank())+
        ylab(input$mergeVarY)+
        scale_fill_manual(values = cbPalette)
    }
    else if (input$rb == "scatter") {
      dataset <- merge_dat2[ ,c(input$mergeVarX,input$mergeVarY)]
      ggplot(data = dataset, aes(x = dataset[,1], y = dataset[,2]))+
        geom_point()+
        theme_minimal()+
        theme(legend.position = "none")+
        theme(text = element_text(size = 20))+
        xlab(element_blank())+
        ylab(input$mergeVarY)+
        scale_fill_manual(values = cbPalette)
    }
  },height = 550,width = 750)
  
  
  # Dynamic description for different plotting formats of merged dataset
  output$test <- renderText(
    if (input$rb == "violin") {
      'Violin plots display data distribution of variables, with more 
      prevalent values appearing wider in each figure. Recommended for 
      numerical data only.'
    }
    else if (input$rb == "bar") {
      'This bar plot represents averages for the selected variables. 
      Recommended for both binary and numerical data.'
    }
    else if (input$rb == "box") {
      'Box plots display data distribution. The Line in the center 
      of the box represents the mean value, the upper line represents 
      the upper quantile, the lower line represents the lower quantile, 
      and the dots represent outliers within the data. Recommended for 
      numerical data only.'
    }
    else if (input$rb == "scatter") {
      'Scatter displays a point for each data value along the Y-axis for 
      variables selected. Recommended for numerical data only.'
    }
  )
  
  
  output$rangePlot <- renderPlot({
    dataset <- merge_dat4[ ,c(input$rangeVarX,input$rangeVarY)]
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
      labs(x = input$rangeVarY, y = input$rangeVarX,)
    
  })
  
  output$recommendation <- renderUI({
    if(input$Shade == "Minimal Shade") {
      
      if(input$Chemistry == "Acidic"){
        tagList(
          tags$h3("Northern Red Oak: Ideal conditions include full sun and a soil preference of  acidic, loamy, moist, sandy, well-drained and clay soils. While the species prefers normal moisture, the tree has drought tolerance."),
          tags$br(), tags$br(),
          tags$img(src="images/rec/nro.jpg")
        )
      }
      
      else if(input$Chemistry == "Alkaline"){
        tagList(
          tags$h3("Sweet Cherry: Ideal conditions include sun with partial shade and a soil preference of well-drained soils. The pH preference depends on variety and region. "),
          tags$br(), tags$br(),
          tags$img(src="images/rec/sc.jpg")
        )
      }
    }
    
    else if(input$Shade == "Moderate Shade") {
      
      if(input$Chemistry == "Acidic"){
        
        if(input$Moisture == "Wet"){
          tagList(
            tags$h3("Red Maple: Ideal conditions include full sun, partial sun/shade and a soil preference of acidic, clay, loamy, moist, rich, sandy, silty loam, well drained, and wet soils. "),
            tags$br(), tags$br(),
            tags$img(src="images/rec/rm.jpg")
          )
        }
        
        else if(input$Moisture == "Moderate"){
          tagList(
            tags$h3("Sweet Birch: Ideal conditions include full sun with partial shade and a soil preference of moist, acidic, sandy, or rocky, and well-drained loams. It is typically found in 
                  preferring north-facing slopes, moist soils, and rocky soils; however it is sensitive to compacted soil. "),
            tags$br(), tags$br(),
            tags$img(src="images/rec/sb.jpg")
          )
        }
          
        else if(input$Moisture == "Dry"){
          tagList(
            tags$h3("Blackgum: Ideal conditions include full sun with part shade and a soil preference of medium to wet, well-drained soils. The species prefers moist and acidic soils 
                    and can tolerate poorly-drained soils, dry soils, and is drought tolerant. "),
            tags$br(), tags$br(),
            tags$img(src="images/rec/bg.jpg")
          )
        }
        
      }
      else if(input$Chemistry == "Alkaline") {
        tagList(
          tags$h3("Sugar Maple: Ideal conditions include full sun and partial shade with a soil preference of deep, well-drained acidic to slightly alkaline soils. It prefers moist soil
                  conditions but has moderate drought tolerance. "),
          tags$br(), tags$br(),
          tags$img(src='images/rec/sm.jpg')
        )
      }
    }
    
  })

}


shinyApp(ui = ui, server = server)
