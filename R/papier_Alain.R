Sys.setlocale("LC_ALL", "en_EN.utf8")

# Package loading -------------------------------------------------------------------

require(tidyverse)
require(readxl)
require(lubridate)
require(sf)
require(units)
require(unikn)
require(rnaturalearth)
require(leaflet)

options(viewer = NULL) 

# Préparation du jeu de données -----------------------------------------------------

read_rds("./Output/dataset_Aythya.rds") -> jeu

jeu %>% 
  filter(obs == "BAGUAGE", 
         yday(datetime) %in% c(yday(ymd(20000401)):yday(ymd(20000731))),
         year(datetime) %in% 2003:2011) %>% 
  mutate(nes_reg = 
           case_when(dpt %in% c("44", "53") ~ "Ouest",
                     dpt %in% c("01", "51") ~ "Est", 
                     TRUE ~ NA_character_),
         across(c(sex, age), ~ .x %>% replace_na("ind"))) %>% 
  filter(!is.na(nes_reg)) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) %>% 
  rename(nes_dpt = dpt, nes_loc = geometry, n_lon = lon, n_lat = lat) %>% 
  as_tibble() -> ring_sel

leaflet() %>%
  setView(lng = 30, lat = 55, zoom = 4) %>%
  addProviderTiles(providers$CartoDB.DarkMatter,
                   group = "Fond noir") %>% 
  addProviderTiles(providers$CartoDB.Positron,
                   group = "Fond clair") %>% 
  addCircles(data = ring_sel %>% st_set_geometry("nes_loc"))

# obs
jeu %>% 
  select(id, obs, ring, datetime, lon, lat) %>% 
  filter(ring %in% ring_sel$ring,
         obs != "BAGUAGE") %>%
  left_join(ring_sel %>% 
               distinct(ring, sp, sex, age, nes_dpt, nes_reg, n_lon, n_lat)) %>% 
  mutate(dist = abs(day(datetime) - 15),
         year = year(datetime), 
         month = month(datetime)) %>% 
  arrange(ring, year, month, dist) %>% 
  group_by(ring, year, month) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(-dist, -year) -> ds

ds %>% 
  count(nes_reg, sp, month)

bind_cols(
ds %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) %>% 
  as_tibble() %>% 
  rename(g_postcatch = geometry),
ds %>% st_as_sf(coords = c("n_lon", "n_lat"), crs = 4326) %>%
  as_tibble() %>% 
  select(g_catch = geometry)) %>% 
  mutate(dist_km = st_distance(g_postcatch, g_catch, by_element = TRUE) %>% 
           units::drop_units() %>% "*"(1e-3),
         mig = if_else(dist_km > 10, TRUE, FALSE)) -> ds2


# Figures for article -------------------------------------------------------------------------

ne_countries(scale = "medium", returnclass = "sf") %>%
  st_set_crs(4326) -> cou

tibble(x = c(-10, 18), y = c(35, 55)) %>% 
  st_as_sf(coords = c("x", "y"), crs = 4326) %>% 
  st_transform(2154) %>% 
  st_coordinates() %>% 
  as_tibble() -> border

ds2 %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  st_transform(2154) %>%
  mutate(coord = st_coordinates(.) %>% as_tibble()) %>% 
  unnest(coord) %>%
  as_tibble() %>% 
  arrange(sp, nes_reg, month) %>% 
  add_count(sp, nes_reg, month) %>% 
  group_split(nes_reg, sp) %>% 
  map(~ .x %>% 
        mutate(tit = month(datetime, label = TRUE, abbr = FALSE),
               sub = str_c(n, " obs."),
               head_fig = str_c(tit, " - ", sub) %>% as_factor())) %>% 
  walk(function(x) {
    prop <- x %>% 
      filter(month(datetime) %in% c(1, 11, 12)) %>% 
      mutate(mig = if_else(dist_km > 10, 1, 0)) %>% 
      group_by(ring, winter = year(datetime - months(1))) %>% 
      summarize(mig = if_else(sum(mig) > 0, 1, 0)) %>% 
      pull(mig) %>% 
      (function(x) str_c(" migration rate: ", (100 * sum(x) / length(x)) %>% round(), "%"))
    exp1 <- if_else(unique(x$sp) == "AYTFER", "Aythya ferina", "Aythya fuligula")
    exp2 <- if_else(unique(x$nes_reg) == "Est", "East", "West")
    capt <- bquote(italic(.(exp1))~" - "~.(exp2)~" - "~.(prop))
    ggplot(x) + 
      geom_sf(data = cou, fill = "antiquewhite") +
      coord_sf(crs = 2154, xlim = border$X, ylim = border$Y) +
      geom_point(aes(x = X, y = Y), alpha = 0.2) +
      geom_density_2d_filled(aes(x = X, y = Y), 
                             contour_var = "ndensity",
                             adjust = 1,
                             breaks = c(0.01, 0.1, 0.5, 0.8, 1),
                             alpha = 0.7) + 
      scale_x_continuous(limits = border$X) + 
      scale_y_continuous(limits = border$Y) +
      labs(x = "", y = "", caption = capt) +
      scale_fill_discrete(name = "Density > X% of\nmaximum density", 
                          labels = c("> 1%", "> 10%", "> 50%", "> 80%")) +
      facet_wrap(~ head_fig) +
      theme_minimal() -> pl
    
    ggsave(filename = str_c("./Output/plot_Alain/", unique(x$sp), "_", unique(x$nes_reg), ".png"),
           plot = pl, width = 10, height = 10, dpi = 1200)
  })


