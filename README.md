# IndependentProject_Huang

Jonathan Huang's Independent project folder

This independent project utilizes public fish size and abundance survey data from [SONGS Mitigation Monitoring Program](https://marinemitigation.msi.ucsb.edu/). Survey was conducted at 3 sites Wheeler North Reef (WNR), San Mataeo Kelp (SMK), and Barn Kelp (BK). The Wheeler North Reef is built in 3 phases - an experimtal reef (1999), and two expansions of a mitigation reef after (2008 and 2019). The full data can be found and downloaded [here](https://portal.edirepository.org/nis/mapbrowse?packageid=edi.668.1).

# This repository consist of a shiny appropriate folder named "SONGS". 

Within this folder you have:
-   R script for the shiny app
-   Data folder 

    - 2 data dictionary files (data_dictionary.csv & species_code_dictionary.csv) to provide information on column names and their definitions
    - 2 data files to provide data on fish size and abundance (songs_surveydata_clean.csv) and transect locations (transect_data.csv)
    
-   rsconnect folder to connect R to shinyapp.io

## This project utilizes a subset of the full survey dataset, focusing on 6 species:

-   Kelp Bass (PACL; Paralabrax clathratus)
-   Barred Sand Bass (PANE; Paralabrax nebulifer)
-   California Sheephead (SEPU; Semicossyphus pulcher)
-   Black Perch (EMJA; Embiotoca jacksoni)
-   Blacksmith (CHPU; Chromis punctipinnis)
-   Señorita (OXCA; Oxyjulis californica)

This project creates an shiny app to visualize the data as annual size structure, the shiny is published [here](https://jonlhuang.shinyapps.io/SONGS/). NOTE: to publish the app to shiny.io, "SONGS" will need to be removed from the import of data (lines 14,24,40). Running the app in R will require the "SONGS" to be added.

## The shiny app has 3 outputs

-   leaflet map visualizing locations of transect with information on each transect

    -   Transect latitude
    -   Transect longitude
    -   Transect code and name
    -   phase built (N = Natural reef; E = experimental pahse; M = Mitigation phase; M2 = Mitigation phase 2)


-   Data table summarizing the count of fish by species, size, site, and year

-   Plot visualizing the data either using bar graph or density plot