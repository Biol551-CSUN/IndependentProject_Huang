# Shiny app for songs data

library(tidyverse)
library(here)
library(leaflet)
library(shiny)
library(DT)

#####----------Load data---------####

transect <- read_csv(here("data","transect_code.csv")) %>% 
  drop_na() %>% 
  separate(col = transect_code,
           into = c("reef","transect_val"),
           sep = " ") %>% 
  mutate(reef_name = reef) %>% 
  unite("transect_code", #new col name
        reef:transect_val, #columns to unite
        sep = " ")

fish <- read_csv(here("data","songs_clean.csv")) %>% 
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
fish <- read_csv(here("data","songs_clean.csv")) 

#clean dataset
songs_count <- fish %>% 
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
  textInput(inputId = "choice",
             label = "SONGS Dive sites",
             value = "All Survey Sites"),
  leafletOutput("map"),
  
  
  
  #Add selection slide 
  checkboxGroupInput("species_code", label = h3("Select Species"), 
                     choices = list("SEPU" ="SEPU", "OXCA"="OXCA",
                                    "CHPU"="CHPU", "PACL"="PACL", 
                                    "PANE" = "PANE", "EMJA" = "EMJA"),
                     selected = "SEPU"),
  # hr(),
  # fluidRow(column(3, verbatimTextOutput("value")))
  
  # Make space for a table
  h2("Data Table"),
  DT::dataTableOutput("datatable"),
  
  # making space for density plot 
  plotOutput("density")
  
  )




server <- function(input, output){
  
  data<-reactive({
    tibble(songs_count %>%
             filter(species_code %in% c(input$species_code))) #make dataset own object that is reactive everytime num is changed
  })
  
  
  #####-----Add text----####  
  #Output for printing text for map
  output$value <- renderPrint({input$choice})
  
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
      addControl(input$choice, #input title name, which is determined by input
                 position = "topleft", #position of the map
                 className = "map-title") %>%
      addLayersControl(overlayGroups = c(mybins), 
                       options = layersControlOptions(collapsed = FALSE)) 
  )

  ####-----Print Data Table-----
  output$datatable = DT::renderDataTable({
    fish %>% 
      group_by( year,total_length, species_code, reef_code) %>%  #group analysis 
      mutate(total_length = as.numeric(total_length), #change length to numeric
             reef_code = as_factor(reef_code),
             year = as_factor(year)) %>%   
      summarise(count = sum(count)) %>% 
      filter(species_code %in% c(input$species_code))
  })
  
  
  #####-----Add Checkbox----####  
  #Print check box to select species you want to look at
  output$value <- renderPrint({ input$species_code })
  
  
  #####-----Print Density plot----####  
  output$density <- renderPlot({
    data() %>% 
      # filter(reef_code%in% c("BK","SMK")) %>% 
      mutate(year = as_factor(year)) %>% 
      ggplot()+   #specify x & solor by year
      geom_density(
        aes(x = total_length, color = year, group = year),linewidth = 1.5)+   #use density plot
      facet_wrap(~species_code, ncol=1, scales = "free")+ #separate plots by species
      ggh4x::facetted_pos_scales(x = NULL, y = list( #custom scales for each facet
        SEPU = scale_y_continuous(limits = c(0,0.075)),
        PACL = scale_y_continuous(limits = c(0,0.15)),
        PANE = scale_y_continuous(limits = c(0,0.24)),
        EMJA = scale_y_continuous(limits = c(0,0.15)),
        CHPU = scale_y_continuous(limits = c(0,0.9)),
        OXCA = scale_y_continuous(limits = c(0,1.0))
      ))+
      scale_x_continuous(breaks = seq(0,max(fish$total_length), 10),
                         limits = c(0,NA))+  #scale x to be max lenght
      labs(x = "Total Length (mm)",    #edit axis & legend labels
           y = "Density", 
           color = "Year",
           title = element_text("Size distribution on Natural Reefs (BK & SMK)"))+
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
            plot.title = element_text(hjust = 0.5, size = 40),
            axis.text = element_text(size = 30),
            axis.title = element_text(size = 17),
            strip.text = element_text(size = 30),
            legend.text = element_text(size = 30),
            legend.key.size = unit(1.5,"cm"),
            legend.title = element_text(size = 30))
    
  }, height = 750, width = 750)
  
}


####----- Run the application ----####
shinyApp(ui = ui, server = server)
