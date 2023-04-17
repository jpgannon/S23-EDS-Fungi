library(plotly)
library(dplyr)
library(rsconnect)
library(tidyverse)
library(stringr)

# Reading in datasets
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#669919", "#001600")
finalgrowth_dat <- read.csv("www/files/Final.DataTable.growth.v.15N.csv")
finalsurv_dat <- read.csv("www/files/FINALfulltable.survival.csv")
finalmstr_dat <- read.csv("www/files/FINALfulltable.moisture.csv")
tag_id_dat<- read.csv("www/files/tagID_byprox.csv")
seedling_dat <- read.csv("www/files/seedling.csv")


# Merging and character substitutions 
tag_id_dat$ID_new <- gsub('_','.',tag_id_dat$Tag.ID)

merge_dat1 <- merge(finalgrowth_dat,finalsurv_dat)

merge_dat2 <- merge(merge_dat1, finalmstr_dat)
merge_dat2$ID2 <- gsub('_','.',merge_dat2$Tag.ID)
merge_dat2$ID2 <- gsub(' ','.',merge_dat2$ID2)
merge_dat2$leg_pos <- paste(merge_dat2$myc.legacy.type, merge_dat2$Position)

merge_dat3 <- merge(seedling_dat,merge_dat2, by ='ID2')
merge_dat3$PCG <- (((merge_dat3$HT.cm - merge_dat3$HT.cm.initial)/merge_dat3$HT.cm.initial)*100)
merge_dat3$Mycorrhizal.Species.Position <- paste(merge_dat3$Mycorrhizal.Species.Type, merge_dat3$Position)
merge_dat3$Slash <- as.character(merge_dat3$Slash)

# Data cleaning for further readability when plotting speies and mycorrhizae types
merge_dat3 <- merge_dat3 %>% 
  mutate(Mycorrhizal.Species.Type = case_when(Mycorrhizal.Species.Type == "EcM" ~ "Ectomycorrhiza",
                             Mycorrhizal.Species.Type == "AM" ~ "Arbuscular Mycorrhiza")) %>%
  mutate(Mycorrhizal.Legacy.Type = case_when(Mycorrhizal.Legacy.Type == "EcM" ~ "Ectomycorrhiza",
                             Mycorrhizal.Legacy.Type == "AM" ~ "Arbuscular Mycorrhiza")) %>% 
  mutate(Species = case_when(Species.x == "ACRU" ~ "Red Maple",
                             Species.x == "ACSA" ~ "Sugar Maple",
                             Species.x == "BELE" ~ "Black Birch",
                             Species.x == "CACO" ~ "Bitternut Hickory",
                             Species.x == "NYSY" ~ "Blackgum",
                             Species.x == "PRSE" ~ "Black Cherry",
                             Species.x == "QURU" ~ "Northern Red Oak",
                             Species.x == "TIAM" ~ "American Basswood"
                             )) %>% 
  rename("N15 to N14 Ratio" = "Ratio.15N.to.14N") %>%
  rename("Mycorrhizal Legacy Type" = "Mycorrhizal.Legacy.Type") %>%
  rename("Mycorrhizal Species Type" = "Mycorrhizal.Species.Type") %>%
  rename("Mycorrhizal Species Position" = "Mycorrhizal.Species.Position") %>%
  rename("Seedling Growth (cm)" = "Growth.cm") %>% 
  rename("Survival Rate" = "Survival") %>% 
  rename("Tree Species" = "Species") %>% 
  rename("Tree Position within Plot" = "Proximity") %>% 
  rename("Volumetric Soil Moisture" = "Moisture") %>% 
  rename("Growth (Percent Change)" = "PCG") %>% 
  rename("Legacy Plot Position" = "leg_pos") 

# Using navbar page to structure the overall app. This will categorize the app into 
# distinct sections: Home page, data page, tree information page. 
ui <- navbarPage(
  
  # Changing the theme to minimalist black text on white background appearance
  theme = bslib::bs_theme(bootswatch = "lux"),
  "Celement Woodlot Data",
  
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
                  Evolution, Ecosystems and Society Program at Dartmouth College.'),
           tags$br(),tags$br()
           
           ),
           
           
           # Blurb about project context, image from included from site work in Corinth, VT. 
           tags$section(
             tags$h1(class='greeting','About the Project'),
             tags$div(class='text',
                      tags$h3(class='blurb', "As Virginia Tech undergraduates majoring in Environmental Data Science,
                               our team was tasked with creating an R-Shiny app of effective data analysis for our
                               capstone project. Our group worked with scientists at Dartmouth College to investigate 
                               how post-logging legacy mycorrhizal fungi can affect seedling regeneration. The app 
                               data was collected at Clement Woodlot in Corinth, Vermont. The study itself was 
                               conducted with assistance of the Adaptive Silviculture for the Emerald Ash Borer experiment, led by
                               Anthony D'Amato Ph.D.")
           ),
           tags$div(class="fig",
                    tags$img(src = "images/6.webp"),
                    tags$figcaption("Figure 1. Forest ecology researchers Eva Legge and Amelia Fitch at an experimental site in Corinth, VT."),
                    tags$br(),tags$br())
           
           ),
           
           
           # Blurb on project design, image included from Eva's research poster. 
           tags$section(
             tags$h1(class='greeting','Context & Design'),
             tags$div(class='text',
                      tags$h3(class='blurb', "Throughout natural history, trees and mycorrhizal fungi have coevolved
                              in a symbiotic relationship. Trees provide mycorrhizae with photosynthesized sugars, which in
                              turn assist trees in uptake of nutrients like nitrogen and phosphorous. For project setup, eight
                              quarter-acre plots in Corinth, VT were harvested in the winter of 2020-2021. Of these eight plots, 
                              half are dominated by ectomycorrhizal associated (EcM-Legacy) trees, while the other half
                              are dominated by arbuscular mycorrhizal associated (AM-Legacy) trees. Within these recently logged
                              plots, 20 seedlings of eight different species were planted. To quantify seedling survival and growth
                              between the EcM-Legacy and AM-Legacy plots, several factors were measured. Navigate to the data tab to 
                              see multiple data visualization methods of these factors. ")
             ),
             tags$div(class="fig",
                         tags$img(src = "images/3.webp"),
                         tags$figcaption("Figure 2. View of project setup at Clement Woodlot."),
                         tags$br(),tags$br())
             
             )
           ),
           
  
  # Data page for navigation between customizable data visualization and line range plot
  navbarMenu("Data",
             
       
       # Subpage for graph for with customizable visualization of all variables across merged dataset
       tabPanel("Customizable Data Visualization",
                titlePanel("Create your plot with the options below"),
                
                
                # Sidebar selection for choosing graph type 
                sidebarLayout(
                  sidebarPanel(
                    radioButtons("rb", "Choose Display:",
                                 choiceNames = list("violin plot",
                                                    "bar plot",
                                                    "box plot",
                                                    "scatterplot"),
                                 choiceValues =list("violin",
                                                    "bar",
                                                    "box",
                                                    "scatter")
                    ),
                    
                    # Sidebar selection for choosing X variable 
                    selectInput(inputId = "mergeVarX",
                                label = "Select X-axis Variable:",
                                choices = list("Tree Species",
                                               "Mycorrhizal Species Type",
                                               "Mycorrhizal Legacy Type",
                                               "Tree Position within Plot")),
                    
                    # Sidebar selection for choosing Y variable
                    selectInput(inputId = "mergeVarY",
                                label = "Select Y-axis Variable:",
                                choices = list("Survival Rate",
                                               "Seedling Growth (cm)",
                                               "Volumetric Soil Moisture",
                                               "N15 to N14 Ratio"))
                  ),
                  
                  # Output of customizable plot, figure caption to describe the different graph types 
                  mainPanel(
                    plotOutput("MergedPlot"),
                    tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),
                    tags$h3(textOutput("mergeplotdesc")),
                  )
                )),
       
       
       
       # Line range plot to display both mean and standard deviation of real growth (aka growth percent change) and survival.
       tabPanel("Growth & Survival Line Range",
                
                titlePanel("Growth & Survival Line Range"),
                
                # Sidebar input for X variable selection
                sidebarLayout(
                  sidebarPanel(
                    selectInput(inputId = "rangeVarX",
                                label = "Select Y-axis Variable:",
                                choices = list("Tree Species",
                                               "Legacy Plot Position",
                                               "Mycorrhizal Species Position",
                                               "Slash Level" = "Slash",
                                               "Mycorrhizal Species Type",
                                               "Mycorrhizal Legacy Type"),
                                selected = "Tree Species"),
                    
                    # Sidebar input for Y variable selection
                    selectInput(inputId = "rangeVarY",
                                label = "Select X-axis Variable:",
                                choices = list("Survival Rate",
                                               "Growth (Percent Change)"),
                                selected = "Growth (Percent Change)")          
                  ),
                  
                  # Output of line range plot as designed by user input
                  mainPanel(plotOutput("rangePlot"),
                  tags$br(),tags$br(),
                  tags$h3("For each Y-variable the central dot represents mean x-value and the lines indicate standard deviation."))
                )
                
       )
       ),
  
  
 # Tree catalog page that will show fast facts about each species, along with pictures and a map of geographic occurence.
 tabPanel("Tree Information Catalog",
          titlePanel("Learn information about different species"),
          
          # Tabset panel for navigation between different species
            tabsetPanel(
              id = "species",
              tabPanel("Northern Red Oak",
                       tags$div(class='catalog',
                               tags$h2("Northern Red Oak - Quercus Rubra"),
                               tags$img(class = "catalog", src = "images/catalog/nroleaf.webp"),
                               tags$img(class = "catalog", src = "images/catalog/nromap.webp"),
                               tags$img(class = "catalog", src = "images/catalog/nro.webp"),
                               tags$h2("Mycorrhizal association: Ectomycorrhiza"),
                               tags$h2("Geographic Range: Eastern United States and Southeastern Canada"),
                       )
                       ),
              tabPanel("Red Maple",
                       tags$div(class='catalog',
                               tags$h2("Red Maple - Acer Rubrum"),
                               tags$img(class = "catalog", src = "images/catalog/rmleaf.webp"),
                               tags$img(class = "catalog", src = "images/catalog/rmmap.webp"),
                               tags$img(class = "catalog", src = "images/catalog/rm.webp"),
                               tags$h2("Mycorrhizal association: Arbuscular mycorrhiza"),
                               tags$h2("Geographic Range: Maine west to Minnesota, south to Texas, east to Florida")
                         )
                       ),
              tabPanel("Sugar Maple",
                       tags$div(class='catalog',
                               tags$h2("Sugar Maple - Acer Saccharum"),
                               tags$img(class = "catalog", src = "images/catalog/smleaf.webp"),
                               tags$img(class = "catalog", src = "images/catalog/smmap.webp"),
                               tags$img(class = "catalog", src = "images/catalog/sm.webp"),
                               tags$h2("Mycorrhizal association: Arbuscular mycorrhiza"),
                               tags$h2("Geographic Range: New England and Mid-Atlantic States,
                                       Midwestern states and south to Tennessee")
                       )
                               
                       ),
              tabPanel("Black Cherry",
                       tags$div(class='catalog',
                                tags$h2("Black Cherry - Prunus Serotina"),
                                tags$img(class = "catalog", src = "images/catalog/bcleaf.webp"),
                                tags$img(class = "catalog", src = "images/catalog/bcmap.webp"),
                                tags$img(class = "catalog", src = "images/catalog/bc.webp"),
                                tags$h2("Mycorrhizal association: Arbuscular mycorrhiza"),
                                tags$h2("Geographic range: Maine south to Florida and west to the midwestern states")
                         )
                       ),
              tabPanel("Blackgum",
                       tags$div(class='catalog',
                               tags$h2("Blackgum - Nyssa Sylvatica"),
                               tags$img(class = "catalog", src = "images/catalog/bgleaf.webp"),
                               tags$img(class = "catalog", src = "images/catalog/bgmap.webp"),
                               tags$img(class = "catalog", src = "images/catalog/bg.webp"),
                               tags$h2("Mycorrhizal association: Arbuscular mycorrhiza"),
                               tags$h2("Geographic Range: Southwestern Maine to Central Florida"),
                         )
                       ),
              tabPanel("Black Birch",
                       tags$div(class='catalog',
                                tags$h2("Black Birch - Betula Lenta"),
                                tags$img(class = "catalog", src = "images/catalog/bbleaf.webp"),
                                tags$img(class = "catalog", src = "images/catalog/bbmap.webp"),
                                tags$img(class = "catalog", src = "images/catalog/bb.webp"),
                                tags$h2("Mycorrhizal association: Arbuscular mycorrhiza"),
                                tags$h2("Geographic Range: New England states and northern appalachian states"),
                         )
                       ),
              tabPanel("American Basswood",
                       tags$div(class='catalog',
                               tags$h2("American Basswood - Tilia Americana"),
                               tags$img(class = "catalog", src = "images/catalog/ableaf.webp"),
                               tags$img(class = "catalog", src = "images/catalog/abmap.webp"),
                               tags$img(class = "catalog", src = "images/catalog/bg.webp"),
                               tags$h2("Mycorrhizal association: Arbuscular mycorrhiza"),
                               tags$h2("Geographic Range: New england states, southern canada and the midwest"),
                         )
                       ),
              tabPanel("Bitternut Hickory",
                       tags$div(class='catalog',
                               tags$h2("Bitternut Hickory - Carya Cordiformis"),
                               tags$img(class = "catalog", src = "images/catalog/bhleaf.webp"),
                               tags$img(class = "catalog", src = "images/catalog/bhmap.webp"),
                               tags$img(class = "catalog", src = "images/catalog/bh.webp"),
                               tags$h2("Mycorrhizal association: Arbuscular mycorrhiza"),
                               tags$h2("Geographic Range: Eastern United States and midwestern United States"),
                          )
              )
            ),tags$br(),
          
          # Credits for the VT dendro textbook
          tags$h5("Images and geographic ranges in the tree information catalog were sourced from the 
                  Virginia Tech Dendrology textbook, the hard work of Dr. John Seiler, Dr. John Peterson 
                  and many others. ")
   ),
 
 # Authors page to describe individuals involved in the project
  tabPanel("About the Authors",
           navlistPanel(id = "tabset",
             "Virginia Tech",
             tabPanel("JP Gannon, PhD",
                      tags$div(class='person',
                               tags$h2("J.P. Gannon, PhD"),
                               tags$img(src="images/people/jp.webp"),tags$br(),
                               tags$h3("JP Gannon is a collegiate assistant professor of environmental data science in
                               the Forest Resources and Environmental Conservation (FREC) department in the College of Natural Resources and Environment at Virginia Tech.
                                       His Research focuses on examining streamflow generation in rivers and streams at a variety of scales, and using remotely
                                       sensed data to address environmental issues. He is passionate about teaching and involving undergraduate students in research projects, 
                                       and enjoys mountain biking in his free time.")
                               )
                      ),
             tabPanel("Rasheed Pongnon",
                      tags$div(class='person',
                               tags$h2("Rasheed Pongnon"),
                               tags$img(src="images/people/rasheed.webp"),tags$br(),
                               tags$h3("Rasheed is a student at Virginia Tech studying environmental data science. He hopes to work to study and conserve marine and coastal environments after graduation.")
                      )),
             tabPanel("Trayda Murakami",
                      tags$div(class='person',
                               tags$h2("Trayda Murakami"),
                               tags$img(src="images/people/trayda.webp"),tags$br(),
                               tags$h3("Trayda is a Virginia Tech student who studied environmental data science with a minor in Leadership and Social Change. In her career, she hopes to build 
                                       interdisciplinary managerial skills in environmental consulting.")
                      )),
             tabPanel("Will Poncy",
                      tags$div(class='person',
                               tags$h2("Will Poncy"),
                               tags$img(src="images/people/will.webp"),tags$br(),
                               tags$h3("Will is a Virginia Tech environmental data science major and a FAA-licensed remote pilot. In the future, he aims to conduct graduate research at the intersection of geospatial analysis, unmanned 
                                       aerial vehicles, and environmental science.")
                      )),
             "Dartmouth College",
             tabPanel("Amelia Fitch",
                      tags$div(class='person',
                               tags$h2("Amelia Fitch"),
                               tags$img(src="images/people/amelia.webp"),tags$br(),
                               tags$h3("Amelia is a PhD candidate in the Ecology, Evolution, Environment, and Society graduate program, working with Caitlin Hicks Pries. She earned a BS at the University of Oregon, where she 
                                       studied tidal wetlands, and MS at the University of Cambridge studying boreal lake sediments. She first became interested in soil during her contrbutions to a 
                                       estuary restoration project while working as a park ranger in Oregon. Her current work is focused on how soil microbes (and specifically these really cool fungi called mycorrhizae) affect decomposition and nutrient cycling in northeastern forests.
                                       Outside of research she loves baking sourdough, trail running, or mountain biking with her dog Waldo. ")
                      )),
             tabPanel("Eva Legge",
                      tags$div(class='person',
                               tags$h2("Eva Legge"),
                               tags$img(src="images/people/eva.webp"),tags$br(),
                               tags$h3("Eva is a senior at Dartmouth College majoring in Biology (with an Ecology concentration), and an aspiring forest ecologist working at the nexus of mycorrhizal ecology and adaptive management.
                               As a member of the Hicks Pries Soil Ecology Lab, her research examines the role arbuscular mycorrhizae may play in facilitating forest regeneration under varied levels of forest disturbance 
                               (selective harvest and quarter-acre clear cuts). Eva is also an avid science communicator, a practice inspired by her science writer grandfather. She believes good communication is necessary 
                                       for any scientific practice that strives to be wide-reaching and inclusive. Outside of research, she loves watching Gilmore Girls, gardening, and playing music with her band.")
                      ))
           ))
  )





server <- function(input, output, session) {

  
  # This output handles the plotting of the merged dataset, customized based on user input
  output$MergedPlot <- renderPlot({
    
    if (input$rb == "violin") {
      dataset <- merge_dat3[ ,c(input$mergeVarX,input$mergeVarY)]
      ggplot(data = dataset, aes(x = dataset[,1], y = dataset[,2], fill = dataset[,1]))+
        geom_violin()+
        theme_minimal()+
        xlab(input$mergeVarX)+
        theme(legend.position = "none")+
        theme(text = element_text(size = 18.5))+
        ylab(input$mergeVarY)+
        scale_fill_manual(values = cbPalette)+
        guides(color = guide_legend(title = "Users By guides"))
    }
    else if (input$rb == "bar") {
      dataset <- merge_dat3[ ,c(input$mergeVarX,input$mergeVarY)]
      
      colnames(dataset) <- c("Name", "Value")
      dataset <- dataset %>% 
        group_by(Name) %>% 
        summarise(mean = mean(Value), se = (sd(Value)/sqrt(length(Value)))/2)
        
      ggplot(data = dataset, aes(x = Name, y = mean,fill = Name))+
        geom_col(aes(y=mean))+
        geom_errorbar(aes(ymin = mean - se, ymax = mean + se),width = 0.5)+
        theme_minimal()+
        xlab(input$mergeVarX)+
        theme(legend.position = "none")+
        theme(text = element_text(size = 18.5))+
        ylab(input$mergeVarY)+
        scale_fill_manual(values = cbPalette)
    }
    else if (input$rb == "box") {
      dataset <- merge_dat3[ ,c(input$mergeVarX,input$mergeVarY)]
      ggplot(data = dataset, aes(x = dataset[,1], y = dataset[,2], fill = dataset[,1]))+
        geom_boxplot()+
        theme_minimal()+
        theme(legend.position = "none")+
        theme(text = element_text(size = 18.5))+
        xlab(input$mergeVarX)+
        ylab(input$mergeVarY)+
        scale_fill_manual(values = cbPalette)
    }
    else if (input$rb == "scatter") {
      dataset <- merge_dat3[ ,c(input$mergeVarX,input$mergeVarY)]
      ggplot(data = dataset, aes(x = dataset[,1], y = dataset[,2]))+
        geom_point(color = "#009e73", position = "jitter")+
        theme_minimal()+
        theme(legend.position = "none")+
        theme(text = element_text(size = 18.5))+
        xlab(input$mergeVarX)+
        ylab(input$mergeVarY)+
        scale_fill_manual(values = cbPalette)
    }
  },height = 550,width = 1150)
  
  
  # Dynamic description for different plotting formats of merged dataset
  output$mergeplotdesc <- renderText(
    if (input$rb == "violin") {
      'Violin plots display data distribution of variables, with more 
      prevalent values appearing wider in each figure. Recommended for 
      numerical data only.'
    }
    else if (input$rb == "bar") {
      'Bar plot represents averages for the selected variables, lines above individual bars represent mean standard error.
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
  
  # Output handling rendering of line range plot
  output$rangePlot <- renderPlot({
    dataset <- merge_dat3[ ,c(input$rangeVarX,input$rangeVarY)]
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

}


shinyApp(ui = ui, server = server)
