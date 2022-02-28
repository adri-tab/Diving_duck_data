# Package loading -------------------------------------------------------------------

Sys.setlocale("LC_ALL", "English")
require(tidyverse)
require(readxl)
require(lubridate)
require(sf)
require(leaflet)
require(htmlwidgets)

options(viewer = NULL) 

leaflet() %>%
  setView(lng = 3.4, lat = 47, zoom = 6) %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas,
                   group = "Fond clair") %>% 
  addProviderTiles(providers$CartoDB.DarkMatter,
                   group = "Fond noir") %>% 
  addProviderTiles(providers$GeoportailFrance.orthos,
                   group = "Fond satellite") %>% 
  addProviderTiles(providers$OpenTopoMap,
                   group = "Fond topographie") -> map_base

# Exploration du jeu de données -----------------------------------------------------

read_rds("./Output/dataset_Aythya.rds") -> jeu

jeu %>% 
  filter(obs == "BAGUAGE") %>% 
  mutate(origin = 
           case_when(dpt %in% c("44", "53") ~ "West",
                     dpt %in% c("41", "77", "89") ~ "Center",
                     dpt %in% c("42", "01", "51", "55") ~ "East") %>% 
           as_factor(),
         across(c(sex, age), ~ .x %>% replace_na("ind"))) -> bird

bird %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  mutate(id_co = dpt %>% as_factor() %>% as.numeric(),
         colo = hcl(h = seq(15, 300, length = max(id_co)), 
                    l = 65, 
                    c = 100)[id_co]) -> bird2

map_base %>% 
  addCircleMarkers(
    data = bird2,
    group = ~ dpt,
    radius = ~ 15 * log10(1 + 1),
    fillColor = ~ colo,
    fillOpacity = 0.7,
    stroke = TRUE,
    color = "#696773",
    opacity = 0.7,
    weight = 2,
    label = ~ str_c(id, com, spot, sep = "~")) %>%
  addLayersControl(position = "topleft",
                   baseGroups = c("Fond clair",
                                  "Fond noir", 
                                  "Fond satellite",
                                  "Fond topographie"),
                   overlayGroups = unique(bird2$dpt),
                   options = layersControlOptions(collapsed = FALSE)) %>% 
  addLegend(data = bird2 %>% 
              st_drop_geometry() %>% 
              distinct(dpt, colo),
            position = "topright",
            labels = ~ dpt,
            colors = ~ colo, 
            title = NULL, 
            opacity = 0.7) -> mp1; mp1

rm(mp1)