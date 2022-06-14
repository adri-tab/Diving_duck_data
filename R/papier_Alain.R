# Package loading -------------------------------------------------------------------

require(tidyverse)
require(readxl)
require(lubridate)
require(sf)
require(igraph)
require(lwgeom)
require(units)
require(leaflet)
require(htmlwidgets)
require(stars)
require(unikn)
require(rnaturalearth)
require(dtplyr)
require(bpnreg)

options(viewer = NULL) 

# Preparation du jeu de données -----------------------------------------------------

read_rds("./Output/dataset_Aythya.rds") -> jeu

jeu %>% 
  filter(obs == "BAGUAGE", 
         yday(datetime) %in% c(yday(ymd(20000401)):yday(ymd(20000731))),
         year(datetime) %in% 2003:2011) %>% 
  mutate(nes_reg = 
           case_when(dpt %in% c("44", "53") ~ "Ouest",
                     dpt %in% c("01", "51") ~ "Est", 
                     TRUE ~ NA_character_) %>% 
           as_factor(),
         across(c(sex, age), ~ .x %>% replace_na("ind"))) %>% 
  filter(!is.na(nes_reg)) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) %>% 
  rename(nes_dpt = dpt, nes_loc = geometry, n_lon = lon, n_lat = lat) %>% 
  as_tibble() -> ring_sel

# obs
jeu %>% 
  select(id, obs, ring, datetime, lon, lat) %>% 
  filter(ring %in% ring_sel$ring,
         obs != "BAGUAGE") %>%
  mutate() %>% 
  left_join(ring_sel %>% 
               distinct(ring, sp, sex, age, nes_dpt, nes_reg)) %>% 
  group_by(ring, year = year(datetime), month(datetime)-> cont
