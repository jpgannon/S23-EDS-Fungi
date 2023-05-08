#This code preforms the data modifications to the data used in the Clement Woodlot app.
#Follow the instructions provided if there are any additions to the data sets used

library(plotly)
library(dplyr)
library(rsconnect)
library(tidyverse)
library(stringr)


# Reading in datasets
# before starting, import the final growth dataset as "finalgrowth_dat",
# the final moisture data set as "finalmstr_dat",
# seedling data as "seedling_dat" and the
# Final survival dataset as "finalsurv_dat"

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#669919", "#001600")
finalgrowth_dat <- read.csv("www/files/Final.DataTable.growth.v.15N.csv")
finalsurv_dat <- read.csv("www/files/FINALfulltable.survival.csv")
finalmstr_dat <- read.csv("www/files/FINALfulltable.moisture.csv")
tag_id_dat<- read.csv("www/files/tagID_byprox.csv")
seedling_dat <- read.csv("www/files/seedling.csv")


# Below are a series of code blocks to merge the data into a single dataset allowing for the customizable plots

# The following code will merge growth and survival data into merge_dat1
merge_dat1 <- merge(finalgrowth_dat,finalsurv_dat)

# The line below creates merge_dat2 by incorporating moisture data into the previously merged growth and survival data
merge_dat2 <- merge(merge_dat1, finalmstr_dat)

# The tag IDs within the seedling_dat is then modified to match that of the merged datasets
tag_id_dat$ID_new <- gsub('_','.',tag_id_dat$Tag.ID)
merge_dat2$ID2 <- gsub('_','.',merge_dat2$Tag.ID)
merge_dat2$ID2 <- gsub(' ','.',merge_dat2$ID2)

# A new column with the position of the legacy myc speceis is then created here
merge_dat2$leg_pos <- paste(merge_dat2$myc.legacy.type, merge_dat2$Position)

# seedling data is then added to the merged dataset bt the new ID
merge_dat3 <- merge(seedling_dat,merge_dat2, by ='ID2')

# Percent Growth change is calculated and added to a new column
merge_dat3$PCG <- (((merge_dat3$HT.cm - merge_dat3$HT.cm.initial)/merge_dat3$HT.cm.initial)*100)

# Similar to before, here the postion of current myc species is put into a new column
merge_dat3$Mycorrhizal.Species.Position <- paste(merge_dat3$Mycorrhizal.Species.Type, merge_dat3$Position)

# Slash levels are converted to character values
merge_dat3$Slash <- as.character(merge_dat3$Slash)

# Finally, the cells of containing tree species & mycorrhiza types are adjusted to be more clear in visualizations
# Columns are also renamed for more descrriptive axes when plotting
# merge_dat3 is the final merged dataset that will be used when creating the customizable plots
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
# distinct sections: Home page, data page, tree information page, bios page
ui <- navbarPage(
  
  # Changing the theme to minimalist black text on white background appearance
  theme = bslib::bs_theme(bootswatch = "lux"),
  "Clement Woodlot Data",
  
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
                      tags$h3("As Virginia Tech undergraduates majoring in Environmental Data Science,
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
           
           
           # Blurb on project design, close up image of seedlings included for further reference 
           tags$section(
             tags$h1(class='greeting','Context & Design'),
             tags$div(class='text',
                      tags$h3("Throughout natural history, trees and mycorrhizal fungi have coevolved
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
             
       
       # Subpage for graph for customizable visualization of all variables across merged dataset (merge_dat3)
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
                    
                    # Sidebar selection for choosing X variable of the merged dataset
                    selectInput(inputId = "mergeVarX",
                                label = "Select X-axis Variable:",
                                choices = list("Tree Species",
                                               "Mycorrhizal Species Type",
                                               "Mycorrhizal Legacy Type",
                                               "Tree Position within Plot")),
                    
                    # Sidebar selection for choosing Y variable of the merged dataset
                    selectInput(inputId = "mergeVarY",
                                label = "Select Y-axis Variable:",
                                choices = list("Survival Rate",
                                               "Seedling Growth (cm)",
                                               "Volumetric Soil Moisture",
                                               "N15 to N14 Ratio"))
                  ),
                  
                  # Output of customizable plot, a few line breaks and a figure caption to describe the different graph types 
                  mainPanel(
                    plotOutput("MergedPlot"),
                    tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),
                    tags$h3(textOutput("mergeplotdesc")),
                  )
                )),
       
       
       
       # Line range plot to display both mean and standard deviation of real growth (aka growth percent change) and survival.
       tabPanel("Growth & Survival Line Range",
                
                titlePanel("Growth & Survival Line Range"),
                
                # Sidebar input for X variable selection of the line range dataset
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
                    
                    # Sidebar input for Y variable selection of the line range dataset
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
  
  
 # Tree catalog page that will show fast facts about each species, along with pictures and a map of geographic prevalence
 tabPanel("Tree Information Catalog",
          titlePanel("Learn information about different species"),
          
          # Tabset panel is used for navigation between different species across top
            tabsetPanel(
              id = "species",
              
              # Individual tabs for the eight species involved in the app
              # Each tab has a title, 2 pictures and a map. Below the figures are captions for mycorrhizal association and geographic range.
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
                                tags$h2("Mycorrhizal association: Ectomycorrhiza"),
                                tags$h2("Geographic Range: New England states and northern appalachian states"),
                         )
                       ),
              tabPanel("American Basswood",
                       tags$div(class='catalog',
                               tags$h2("American Basswood - Tilia Americana"),
                               tags$img(class = "catalog", src = "images/catalog/ableaf.webp"),
                               tags$img(class = "catalog", src = "images/catalog/abmap.webp"),
                               tags$img(class = "catalog", src = "images/catalog/bg.webp"),
                               tags$h2("Mycorrhizal association: Ectomycorrhiza"),
                               tags$h2("Geographic Range: New england states, southern canada and the midwest"),
                         )
                       ),
              tabPanel("Bitternut Hickory",
                       tags$div(class='catalog',
                               tags$h2("Bitternut Hickory - Carya Cordiformis"),
                               tags$img(class = "catalog", src = "images/catalog/bhleaf.webp"),
                               tags$img(class = "catalog", src = "images/catalog/bhmap.webp"),
                               tags$img(class = "catalog", src = "images/catalog/bh.webp"),
                               tags$h2("Mycorrhizal association: Ectomycorrhiza"),
                               tags$h2("Geographic Range: Eastern United States and midwestern United States"),
                          )
              )
            ),tags$br(),
          
          # Credits for sourcing images from the virgina Tech dendrology textbook
          tags$h5("Images and geographic ranges in the tree information catalog were sourced from the 
                  Virginia Tech Dendrology textbook, the hard work of Dr. John Seiler, Dr. John Peterson 
                  and many others. ")
   ),
 
 
 # Authors page to describe individuals involved in the project
  tabPanel("About the Authors",
           
           # Rather than using a tabset panel like the tree information catalog where the tabs would run across the top, 
           # a navlistPanel was used to display the tabs along the left side. 
           navlistPanel(id = "tabset",
                        
                        # Two sublists within the navlistPanel: one for individuals from Virginia Tech and another for individuals from Dartmouth
                        # Each tab has the individual's name, a photo of them, and a blurb about their research interests/current position
             "Virginia Tech",
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
             tabPanel("J.P. Gannon, PhD",
                      tags$div(class='person',
                               tags$h2("J.P. Gannon, PhD"),
                               tags$img(src="images/people/jp.webp"),tags$br(),
                               tags$h3("Dr. JP Gannon is a collegiate assistant professor of environmental data science in
                               the Forest Resources and Environmental Conservation (FREC) department in the College of Natural Resources and Environment at Virginia Tech.
                                       His Research focuses on examining streamflow generation in rivers and streams at a variety of scales, and using remotely
                                       sensed data to address environmental issues. He is passionate about teaching and involving undergraduate students in research projects, 
                                       and enjoys mountain biking in his free time.")
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
                      )),
             tabPanel("Sarah Goldsmith",
                      tags$div(class='person',
                               tags$h2("Sarah Goldsmith"),
                               tags$img(src="images/people/sarah.webp"),tags$br(),
                               tags$h3("Sarah is the laboratory technician in the Hicks Pries lab. She graduated from the Rochester Institute of Technology with a Master and Bachelor of Environmental Science where she studied salt marsh ecosystems.
                               Since then, she has held positions working in wetlands at the Cape Cod National Seashore in Truro, MA, and monitoring water quality at the Green Mountain Conservation Group in Effingham, NH. 
                               Sarah grew up in NH and is happy to be back and contributing to all aspects of research in the Hicks Pries lab. In her spare time, she enjoys running, cross country skiing, creating art, hanging out with her cat, and reading.")
                      )),
             tabPanel("Caitlin Hicks Pries, PhD",
                      tags$div(class='person',
                               tags$h2("Catlin Hicks Pries, PhD"),
                               tags$img(src="images/people/pries.webp"),tags$br(),
                               tags$h3("Dr. Caitlin Hicks Pries is an Assistant Professor in the Department of Biological Sciences at Dartmouth College who is broadly interested in how terrestrial ecosystems and soils are responding to global change. 
                               Caitlin did her undergraduate work in biology and environmental studies at Middlebury College and earned an M.S. in Soil and Water Science and a Ph.D. in Biology at the University of Florida. She was a postdoctoral
                               researcher in the Climate Sciences Department at the Lawrence Berkeley National Laboratory before moving to Dartmouth.")
                      )),
             
           ))
  )




# Server function to handle the creation of a r-shiny session
server <- function(input, output, session) {

  
  # This output handles the plotting of the customizable merged dataset
  # Series of if-statements for the different plot types based on the user's selection 
  output$MergedPlot <- renderPlot({
    
    # For each different plot type, a temporary dataset of name 'dataset' is created 
    # It will pull columns from merge_dat3 as shown by the [ , c(input$mergeVarX,input$mergeVarY)] syntax
    # The resulting temporary dataset will only have two columns, one for the independent and dependent variables to plot. 
    # Temporary dataset was created to avoid altering the merged dataset
    
    # Begin the series of if-statements, using violin plot as our default choice
    if (input$rb == "violin") {
      dataset <- merge_dat3[ ,c(input$mergeVarX,input$mergeVarY)]
      
      # Generation of a violin plot using the temporary dataset, the scale fill palette accommodates colorblind viewers
      ggplot(data = dataset, aes(x = dataset[,1], y = dataset[,2], fill = dataset[,1]))+
        geom_violin()+
        theme_minimal()+
        xlab(input$mergeVarX)+
        theme(legend.position = "none")+
        theme(text = element_text(size = 18.5))+
        ylab(input$mergeVarY)+
        scale_fill_manual(values = cbPalette)
        # guides(color = guide_legend(title = "Users By guides"))
    }
    
    # Next choice for visualizing data is a bar plot 
    else if (input$rb == "bar") {
      dataset <- merge_dat3[ ,c(input$mergeVarX,input$mergeVarY)]
      
      # This mini-chunk allows for creation of standard error bars when visualizing the barplot
      colnames(dataset) <- c("Name", "Value")
      dataset <- dataset %>% 
        group_by(Name) %>% 
        summarise(mean = mean(Value), se = (sd(Value)/sqrt(length(Value)))/2)
        
      # Generation of a bar plot using the temporary dataset, the scale fill palette accommodates colorblind viewers
      # Take note of the geom_errorbar() line that will display the mean and standard error values we just calculated 
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
    
    # Next choice for visualizing data is a box plot 
    else if (input$rb == "box") {
      dataset <- merge_dat3[ ,c(input$mergeVarX,input$mergeVarY)]
      
      
      # Generation of a bar plot using the temporary dataset, the scale fill palette accommodates colorblind viewers
      ggplot(data = dataset, aes(x = dataset[,1], y = dataset[,2], fill = dataset[,1]))+
        geom_boxplot()+
        theme_minimal()+
        theme(legend.position = "none")+
        theme(text = element_text(size = 18.5))+
        xlab(input$mergeVarX)+
        ylab(input$mergeVarY)+
        scale_fill_manual(values = cbPalette)
    }
    
    # Final choice for visualizing data is a scatterplot 
    else if (input$rb == "scatter") {
      dataset <- merge_dat3[ ,c(input$mergeVarX,input$mergeVarY)]
      
      # Generation of a bar plot using the temporary dataset, the scale fill palette accommodates colorblind viewers
      # Take note of the position = 'jitter' line, wherein all values were included to show distribution from a broader perspective rather than just mean or max
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
    
    # Creation of temporary dataset similar to before, ensures merge_dat3 is not altered
    dataset <- merge_dat3[ ,c(input$rangeVarX,input$rangeVarY)]
    
    # Aggregation of data
    data_sums <- aggregate(dataset[,2] ~ dataset[,1],data = dataset, summary)
    
    # Determining standard deviation of aggregated dataset to properly display line range
    stanD <- aggregate(dataset[,2] ~ dataset[,1],data = dataset, sd)
    sums2 <- as.data.frame(data_sums[,2])
    
    data_sums[,2:7] <- sums2
    data_sums[,8] <- stanD[,2]
    
    # Generation of linerange plot using the summed dataset, scale colors chosen to accomodate colorblind viewers
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

# Function call to host shiny app on live server
shinyApp(ui = ui, server = server)
