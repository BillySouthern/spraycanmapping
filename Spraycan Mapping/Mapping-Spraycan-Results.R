#11/9/25, initiated by BS
#Goal: To map results from the PPGIS Spray Can Exercise

#Analysis includes
#Tidying data, mapping with html

#Libraries
require(tidyverse)
require(sf)
library(tigris)
require(tidycensus)

options(tigris_use_cache = TRUE)

library(leaflet) #For interactive mapping
library(htmlwidgets) #To export map
library(RColorBrewer)

#Create a filepath to OneDrive
onedrivepath="~/OneDrive-UniversityOfOregon/"

#-------------------------------------------------------------------------------
#Load CSV data
PPGIS_Results_untidy <- read_csv("/Users/billy/Documents/GitHub/spraycanmapping/PPGIS-Results/map-me_blobs_14-11-2025_11-50.csv")

#Check values
# Count how many submission
PPGIS_Results_untidy %>%
  summarise(unique_id_persons = n_distinct(id_question))

#Check unique entries for a coumn
PPGIS_Results_untidy %>%
  distinct(id_person)

# id_person
# 119202
#  119207
#  119219

#Create New Unique person identifier
PPGIS_Results <- PPGIS_Results_untidy %>%
  mutate(Entry = dense_rank(id_person))%>%
  mutate(Answer = dense_rank(id_person)) %>%
  mutate(Answer = case_when(
    id_question == 23278 ~ "Urban",
    id_question == 23279 ~ "Suburban",
    TRUE ~ NA_character_   )) %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%
  filter(!id_person %in% c(119202, 119207, 119219))

#Check unique entries for a coumn
PPGIS_Results %>%
  distinct(id_person)



#-------------------------------------------------------------------------------
#Download relevant counties/cities

#Oregon counties
Oregon_Counties <- counties("OR", cb = TRUE)

# Filter Lane County only
Lane <- Oregon_Counties %>%
  filter(NAME == "Lane")

# Download Oregon places (cities, towns)
Oregon_Places <- places(state = "OR", year = 2022)

# Filter only places **within Lane County**
Lane_places <- Oregon_Places %>%
  st_transform(st_crs(Lane)) %>%   # make sure CRS matches
  st_filter(Lane, .predicate = st_within) %>%
  rename(Cities = NAME)            


# Create an interactive map
Zoning_Atlas <- st_transform(Zoning_Atlas, 4326)



# Create the leaflet map 
Spraycan_Results <- leaflet() %>%
  addMapPane(name = "polygons", zIndex = 410) %>% 
  addMapPane(name = "maplabels", zIndex = 420) %>% # higher zIndex rendered on top
  addProviderTiles("CartoDB.PositronNoLabels") %>%
  addProviderTiles("CartoDB.PositronOnlyLabels", 
                   options = leafletOptions(pane = "maplabels"),
                   group = "Map Labels") %>%
  addMiniMap(tiles = "CartoDB.Positron",
             position = "bottomleft") %>%
  #Add counties
  addPolygons(
    data = Lane,
    color = "black",
    weight = 2,
    fill = FALSE,                   # no fill
    group = "Lane County",
    options = pathOptions(pane = "polygons"),
    highlightOptions = highlightOptions(
      color = "darkgrey",
      weight = 3,
      bringToFront = TRUE
    ),
    label = ~NAME
  ) %>%
  # Add places
  addPolygons(
    data = Lane_places,
    color = "black",
    weight = 1.5,
    fillColor = "transparent",
    fillOpacity = 0,
    group = "Cities",                    # <--- add this
    options = pathOptions(pane = "polygons"),
    highlightOptions = highlightOptions(
      color = "darkgrey",
      weight = 2,
      bringToFront = TRUE
    ),
    label = ~Cities
  ) %>%  #Add Spraycan points
  addCircleMarkers(
    data = PPGIS_Results %>% filter(Answer == "Urban"),
    radius = 2.5,
    fillColor = "#e41a1c",
    fillOpacity = 0.5,
    stroke = FALSE,
    label = ~Answer,
    group = "Urban",
    options = pathOptions(pane = "polygons")
  ) %>%
  addCircleMarkers(
    data = PPGIS_Results %>% filter(Answer == "Suburban"),
    radius = 2.5,
    fillColor = "#377eb8",
    fillOpacity = 0.5,
    stroke = FALSE,
    label = ~Answer,
    group = "Suburban",
    options = pathOptions(pane = "polygons")
  ) %>%
  addLayersControl(
    overlayGroups = c( "Urban", "Suburban", "Lane County", "Cities", "Map Labels"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  # Legend
  addLegend(
    position = "bottomright",
    colors = c("#e41a1c", "#377eb8"),
    labels = c("Urban", "Suburban"),
    title = "Area Type") 

# Save the map
saveWidget(
  Spraycan_Results, 
  file = "/Users/billy/Documents/GitHub/spraycanmapping/442_UrbSub_Results.html",
  selfcontained = FALSE)









