# Shiny app for songs data

library(tidyverse)
library(here)
library(leaflet)
library(DT)
library(rsconnect)
library(shinythemes)
# library(ShinyR)


#####----------Load data---------####

transect <- read_csv(here("SONGS","data","transect_data.csv")) %>% 
  drop_na() %>% 
  separate(col = transect_code,
           into = c("reef","transect_val"),
           sep = " ") %>% 
  mutate(reef_name = reef) %>% 
  unite("transect_code", #new col name
        reef:transect_val, #columns to unite
        sep = " ")

fish <- read_csv(here("SONGS","data","songs_surveydata_clean.csv")) %>% 
  select(c(phase_built_code,transect_code)) 
fish <- unique(fish[,c("phase_built_code","transect_code")]) %>% 
  mutate(phase_code = case_when(phase_built_code=="E" ~ 1,
                                phase_built_code=="M"~ 2,
                                phase_built_code=="M2"~ 3))#get unique combination of columns

transect <- full_join(transect,fish)

#make subset of data
wnr <- transect %>% filter(reef_name%in% "WNR")
bk <- transect %>% filter(reef_name%in% "BK")
smk <- transect %>% filter(reef_name%in% "SMK")


#add fish data
fish_length <- read_csv(here("SONGS","data","songs_surveydata_clean.csv")) 

#clean dataset
songs_count <- fish_length %>% 
  group_by( year,total_length, species_code, reef_code) %>%  #group analysis 
  mutate(species_code = as_factor(factor(species_code,   #reorder the factors
                                         levels = c( "SEPU","PACL","PANE","EMJA","CHPU","OXCA"))),
         total_length = as.numeric(total_length), #change length to numeric
         reef_code = as_factor(reef_code),
         year = as.numeric(year)) %>%    
  summarise(count = sum(count), #sum the count through the years
            total_area_sampled = sum(total_area_sampled)) %>%    
  uncount(count) %>%                   #reorder so each row = 1 count
  select(year, species_code,reef_code,total_length,total_area_sampled) #reorder df order


####------Make interactive map with Leaflet package------#####
# Create a color palette with handmade bins.
mybins <- c("WNR","BK","SMK")
myphase <- c("1","2","3")
mypalette <- colorFactor( palette=c("green","aquamarine","darkgreen"), 
                          domain=transect$phase_code, #use phase_built_code
                          na.color="transparent") # if not on color palette, make transparent


#create text box to place in map descriptor
wnrtext <- paste(
  "Longitude: ", wnr$Longitude, "<br/>", #Br/ = break/next row
  "Latitude: ", wnr$Latitude, "<br/>",
  "Transect code: ",wnr$transect_code, "<br/>",
  "Transect full name: ", wnr$Transect, sep="","<br/>",
  "Phase built: ", wnr$phase_built_code) %>%
  lapply(htmltools::HTML)

bktext <- paste(
  "Longitude: ", bk$Longitude, "<br/>", 
  "Latitude: ", bk$Latitude, "<br/>",
  "Transect code: ",bk$transect_code, "<br/>",
  "Transect full name: ", bk$Transect, sep="","<br/>",
  "Phase built: ", bk$phase_built_code) %>%
  lapply(htmltools::HTML)

smktext <- paste(
  "Longitude: ", smk$Longitude, "<br/>", 
  "Latitude: ", smk$Latitude, "<br/>",
  "Transect code: ",smk$transect_code, "<br/>",
  "Transect full name: ", smk$Transect, sep="","<br/>",
  "Phase built: ", smk$phase_built_code) %>%
  lapply(htmltools::HTML)




################################################
##############  BUILD SHINY  ###################
################################################

ui <- fluidPage(
  titlePanel( "SONGS Map"), #add a initial title
  
  #add leaflet map
  leafletOutput("map"),
  p(),
  
  fluidRow( #make seleciton into separate columns
    column(3, #number of column
           h2("Plot parameters"), #section title
  #Add species selection choicebox 
  checkboxGroupInput("species_code",
                                   label = h3("Select Species"),  #label section
                     choices = list("SEPU" ="SEPU", "OXCA"="OXCA", #add choices
                                    "CHPU"="CHPU", "PACL"="PACL", 
                                    "PANE" = "PANE", "EMJA" = "EMJA"),
                     selected = "SEPU"), #initial selection
  # hr(),
  # fluidRow(column(3, verbatimTextOutput("value")))
    ),
  
  column(3, 
         offset = 0, #how much spacing between columns
  
  #add site selection slide
  checkboxGroupInput("site", 
                     label = h3("Select Reef"), 
                     choices = list("WNR" = "WNR" , 
                                    "SMK" = "SMK",
                                    "BK"="BK"),
                     selected = "WNR"),
     ),
  #add a slider for date
 column(3,
        checkboxGroupInput("year", 
                           label = h3("Select Year"), 
                           choices = c(2009:2021),
                           selected = c(2009:2020))
        
 )
    ),
  
  # Make space for a table
  h2("Data Table"),
  DT::dataTableOutput("datatable"),
 
 #Customization for plot type
 
 fluidRow(
   column(3,
          h2("Plot Selections"),
  
          # select title
  textInput(inputId = "choice",
            label = "Label plot title here",
            value = "Wheeler North Reef")
  ),
  # leafletOutput("map"), # can attach to leaflet if desired
  
  #selection for graph type
  column(3,
         selectInput("graph", label = h4("Select graph type"), 
                     choices = list("Density" = "density", "Bar" = "bar"), 
                     selected = "density")
  
  ),
  
  #add selection for axis limits
  column(3,
         numericInput("xaxis", label = h4("x-axis input"), value = 75),
         numericInput("yaxis", label = h4("y-axis input"), value = 0.5)
         
  )
 ),
  
  # making space for density plot 
  plotOutput("density"),
  
  
  #edit theme
  theme = shinytheme("cosmo")
  
  )




server <- function(input, output){
  
  data<-reactive({
    tibble(songs_count %>%
             filter(
               species_code %in% c(input$species_code),
               reef_code %in% c(input$site),
               year %in% c(input$year))) #make dataset own object that is reactive everytime num is changed
  })
  
  
  ####-----add leaflet map to input-----
  output$map <- renderLeaflet(
    leaflet() %>% 
      addTiles()  %>% 
      setView( lat=33.38, lng=-117.55 , zoom=12) %>% #make view australia
      addProviderTiles("Esri.WorldImagery") %>% #change mape style
      addCircleMarkers(data = wnr,
                       lat = wnr$Latitude,  #specify long/lat
                       lng = wnr$Longitude,
                       radius=8 , color= "green",
                       fillColor= mypalette(wnr$phase_code),
                       stroke = TRUE,
                       fillOpacity = 0.8,
                       group="WNR",
                       label = wnrtext,
                       labelOptions =  labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
      ) %>% 
      addCircleMarkers(data = bk,
                       lng=bk$Longitude,
                       lat=bk$Latitude,
                       radius=8 ,color="grey",
                       fillColor="blue", stroke = TRUE, fillOpacity = 0.8, 
                       group="BK",
                       label = bktext,
                       labelOptions =  labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
      ) %>% 
      addCircleMarkers(data = smk,
                       lng=smk$Longitude,
                       lat=smk$Latitude,
                       radius=8 ,
                       color="grey",
                       fillColor="red",
                       stroke = TRUE,
                       fillOpacity = 0.8,
                       group="SMK",
                       label = smktext,
                       labelOptions =  labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
      ) %>% 
      # addControl(input$choice, #input title name, which is determined by input - taken out but can add if desired
      #            position = "topleft", #position of the map
      #            className = "map-title") %>%
      addLayersControl(overlayGroups = c(mybins), 
                       options = layersControlOptions(collapsed = FALSE)) 
  )

  ####-----Print Data Table-----
  output$datatable = DT::renderDataTable({
    fish_length %>% 
      group_by( year,total_length, species_code, reef_code) %>%  #group analysis 
      mutate(total_length = as.numeric(total_length), #change length to numeric
             reef_code = as_factor(reef_code),
             year = as_factor(year)) %>%   
      summarise(count = sum(count)) %>% 
      filter(species_code %in% c(input$species_code),
             reef_code%in% c(input$site),
             year %in% c(input$year))
  })
  
  
  #####-----Add Checkbox----####  
  #Print check box to select species you want to look at
  output$value <- renderPrint({ input$species_code })
  
  output$value <- renderPrint({ input$year })
  
  #####-----Add text for title----####  
  #Output for printing text for map
  output$value <- renderPrint({input$choice})
  
  
  #####-----Print Density plot----####  
  output$density <- renderPlot(
    if(input$graph == "density")
    {
    data() %>% 
      mutate(year = as_factor(year)) %>% 
      ggplot()+   #specify x & solor by year
      geom_density(
        aes(x = total_length, color = year, group = year),linewidth = 1.5)+   #use density plot
      # scale_x_continuous(breaks = seq(0,max(fish_length$total_length), 10))+  #scale x to be max lenght

      facet_wrap(~species_code+reef_code, ncol=length(input$site), scales = "free")+ #separate plots by species
        scale_x_continuous(limits = c(0,input$xaxis))+
        scale_y_continuous(limits = c(0,input$yaxis))+
      labs(x = "Total Length (cm)",    #edit axis & legend labels
           y = "Density", 
           color = "Year",
           title = element_text(input$choice))+
      # scale_color_viridis_d()+  #color palatte
      scale_fill_manual( values = c("#440154","#481f70","#443983",
                                             "#3b528b","#31688e","#287c8e",
                                             "#21918c","#20a486", "#35b779",
                                             "#5ec962", "#90d743","#c8e020",
                                             "#fde725"),
                                             aesthetics = c("color", "fill"),
                         name = "Year")+
      theme_classic()+
      theme(panel.background = element_rect(fill = "grey90", color = "black"),
            plot.title = element_text(hjust = 0.5, size = 30),
            axis.text = element_text(size = 20),
            axis.title = element_text(size = 17),
            strip.text = element_text(size = 20),
            legend.text = element_text(size = 20),
            legend.key.size = unit(1.5,"cm"),
            legend.title = element_text(size = 20),
            axis.title.x = element_text(size = 20),
            axis.title.y = element_text(size = 20)
            )
    

    
  } else{ #make bar plot if not density plot
    fish_length %>% 
      group_by( year,total_length, species_code, reef_code) %>%  #group analysis 
      mutate(total_length = as.numeric(total_length), #change length to numeric
             reef_code = as_factor(reef_code),
             year = as_factor(year)) %>%   
      summarise(count = sum(count)) %>% 
      filter(species_code %in% c(input$species_code),
             reef_code%in% c(input$site),
             year %in% c(input$year)) %>% 
      ggplot(aes(x = total_length, y = count, fill = year))+
      geom_col()+
      facet_wrap(~reef_code+species_code, 
                 ncol=length(input$site))+
      labs(x = "Total Length (cm)",    #edit axis & legend labels
           y = "Count", 
           color = "Year",
           title = element_text(input$choice))+
      scale_fill_viridis_d()+
      theme_minimal()+
      theme(panel.background = element_rect(fill = "grey90", color = "black"), #edit theme
            plot.title = element_text(hjust = 0.5, size = 30),
            axis.text = element_text(size = 20),
            axis.title = element_text(size = 17),
            strip.text = element_text(size = 20),
            legend.text = element_text(size = 20),
            legend.key.size = unit(1.5,"cm"),
            legend.title = element_text(size = 20),
            axis.title.x = element_text(size = 20),
            axis.title.y = element_text(size = 20)
      )
    
  }, height = 900, width = 1300)
  
}


####----- Run the application ----####
shinyApp(ui = ui, server = server)
