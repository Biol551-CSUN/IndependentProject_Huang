# Shiny app for songs data

library(tidyverse)
library(here)
library(leaflet)
library(shiny)

#Load data
#load data
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

#Make interactive map with Leaflet package
# Create a color palette with handmade bins.
mybins <- c("WNR","BK","SMK")
myphase <- c("1","2","3")
mypalette <- colorFactor( palette=c("green","aquamarine","darkgreen"), 
                          domain=transect$phase_code, #use phase_built_code
                          na.color="transparent") # if not on color palette, make transparent



#create text box to place in 
wnrtext <- paste(
  "Longitude: ", wnr$Longitude, "<br/>", 
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
  leafletOutput("map")
  #select the output type
  )




server <- function(input, output){
  output$value <- renderPrint({input$choice})
  
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
}


# Run the application 
shinyApp(ui = ui, server = server)
