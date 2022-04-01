# Package loading -------------------------------------------------------------------

require(tidyverse)
require(readxl)
require(lubridate)
require(sf)
require(geosphere) # function st_azimuth()
require(leaflet)
require(htmlwidgets)

options(viewer = NULL) 

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
  mutate(
    period = 
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

# Winter spot selection -------------------------------------------------------------

tp1 %>% 
  mutate(winter = if_else(month(datetime) < 7, -1, 0) + year(datetime),
         wintering = (ymd(str_c(winter, "1224")) - date(datetime)) %>% as.numeric() %>% abs()) %>% 
  filter(wintering < 40) %>% 
  count(ring, winter)

tp1 %>% 
  mutate(winter = if_else(month(datetime) < 7, -1, 0) + year(datetime),
         wintering = (ymd(str_c(winter, "1224")) - date(datetime)) %>% as.numeric() %>% abs()) %>% 
  filter(wintering < 40) %>% 
  arrange(wintering) %>% 
  distinct(ring, winter, .keep_all = TRUE) %>% 
  select(-wintering) %>% 
  left_join(ring_sel %>% 
              select(ring, datetime_c = datetime, lon_c = lon, lat_c = lat, origin)) %>% 
  mutate(angle = atan2(lat - lat_c, lon - lon_c) * 180/pi) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  rowwise() %>% 
  mutate(geom = st_sfc(st_point(c(lon_c, lat_c)), crs = 4326), 
         dist = st_distance(geom, geometry, by_element = TRUE) %>% as.numeric() %>% "*"(1e-3),
         age = case_when(
           age %in% c("1A", "2A", "pull") & winter == year(datetime_c) ~ "je",
           TRUE ~ "ad")) %>% 
  ungroup() -> tmp; tmp
  
  tmp %>% 
    ggplot(aes(x = angle, y = dist)) +
    geom_point()
  
