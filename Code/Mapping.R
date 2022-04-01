# Package loading -------------------------------------------------------------------

Sys.setlocale("LC_ALL", "English")
require(tidyverse)
require(readxl)
require(lubridate)
require(sf)
require(leaflet)
require(htmlwidgets)

options(viewer = NULL) 


# Leaflet function ------------------------------------------------------------------

leaflet() %>%
  setView(lng = 30, lat = 55, zoom = 4) %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas,
                   group = "Fond clair") %>% 
  addProviderTiles(providers$CartoDB.DarkMatter,
                   group = "Fond noir") %>% 
  addProviderTiles(providers$GeoportailFrance.orthos,
                   group = "Fond satellite") %>% 
  addProviderTiles(providers$OpenTopoMap,
                   group = "Fond topographie") -> map_base

# dataset -> 
# préparer les groupes ticks "gp"
# préparer les groupes couleurs "gp_col"
mapping <- function(ds) {
  ds %>% 
    st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
    mutate(id_co = gp_col %>% as_factor() %>% as.numeric(),
           colo = hcl(h = seq(15, 300, length = max(id_co)), 
                      l = 65, 
                      c = 100)[id_co]) -> tmp1
  
  map_base %>% 
    addCircleMarkers(
      data = tmp1,
      group = ~ gp,
      radius = ~ 15 * log10(1 + 1),
      fillColor = ~ colo,
      fillOpacity = 0.7,
      stroke = TRUE,
      color = "#696773",
      opacity = 0.7,
      weight = 2,
      label = ~ str_c(id, datetime, sep = "~")) %>%
    addLayersControl(position = "topleft",
                     baseGroups = c("Fond clair",
                                    "Fond noir", 
                                    "Fond satellite",
                                    "Fond topographie"),
                     overlayGroups = unique(tmp1$gp),
                     options = layersControlOptions(collapsed = FALSE)) %>% 
    addLegend(data = tmp1 %>% 
                st_drop_geometry() %>% 
                distinct(gp_col, colo),
              position = "topright",
              labels = ~ gp_col,
              colors = ~ colo, 
              title = NULL, 
              opacity = 0.7) -> out_mapping
  return(out_mapping)
}

# Preparation du jeu de données -----------------------------------------------------

read_rds("./Output/dataset_Aythya.rds") -> jeu

jeu %>% 
  filter(obs == "BAGUAGE", 
         yday(datetime) %in% c(yday(ymd(20000415)):yday(ymd(20000731)))) %>% 
  mutate(origin = 
           case_when(dpt %in% c("44", "53") ~ "West",
                     dpt %in% c("41", "77", "89") ~ "Center",
                     dpt %in% c("42", "01", "51", "55") ~ "East") %>% 
           as_factor(),
         across(c(sex, age), ~ .x %>% replace_na("ind"))) -> ring_sel

# obs
jeu %>% 
  select(id, obs, ring, datetime, lon, lat) %>% 
  filter(obs != "BAGUAGE") %>%
  mutate(period = 
           case_when(
             yday(datetime) %in% yday(ymd(20000415)):yday(ymd(20000630)) ~ "repro",
             yday(datetime) %in% yday(ymd(20000701)):yday(ymd(20000831)) ~ "mue",
             yday(datetime) %in% yday(ymd(20000901)):yday(ymd(20001031)) ~ "post-mue",
             yday(datetime) %in% yday(ymd(20001101)):yday(ymd(20001115)) ~ "départ_hivernage",
             yday(datetime) %in% c(yday(ymd(20001116)):yday(ymd(20001231)), yday(ymd(20000101)):yday(ymd(20000215))) ~ "hivernage",
             yday(datetime) %in% yday(ymd(20000216)):yday(ymd(20000414)) ~ "départ_repro") %>% 
           factor(levels = c("repro", "mue", "post-mue", "départ_hivernage", "hivernage", "départ_repro"))) %>% 
  inner_join(ring_sel %>% 
               filter(nobs > 1) %>% 
               distinct(ring, sp, sex, age, dpt, origin)) -> tp1


# Mapping ---------------------------------------------------------------------------

tp1 %>% 
  mutate(gp = str_c(sp, origin, sex, period, sep = "~"),
         gp_col = origin) %>% 
  arrange(sp, origin, sex, period) %>% 
  filter(period == "hivernage",
         sex == "fem", 
         sp == "AYTFER") -> tp2

mapping(tp2)

ring_sel %>% 
  mutate(gp = str_c(sp, sex, sep = "~"),
         gp_col = origin) %>% 
  arrange(sp, origin, sex) -> tp3

mapping(tp3)
