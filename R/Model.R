# Package loading -------------------------------------------------------------------

require(tidyverse)
require(readxl)
require(lubridate)
require(sf)
require(lwgeom)
require(units)
require(leaflet)
require(htmlwidgets)
require(stars)

options(viewer = NULL) 

# Leaflet function ------------------------------------------------------------------

leaflet() %>%
  setView(lng = 30, lat = 55, zoom = 4) %>%
  addProviderTiles(providers$CartoDB.DarkMatter,
                   group = "Fond noir") -> map_base

pal <- colorBin("YlOrRd", 
                domain = ds$n, 
                bins = c(1, 2, 5, 10, 20, 50, 100))

function(ds) {
  map_base %>% 
    addPolygons(
      data = ds,
      fillColor = ~ pal(n),
      weight = 2,
      opacity = 1,
      color =  ~ pal(n),
      fillOpacity = 0.7,
      highlightOptions = highlightOptions(
        weight = 10, 
        bringToFront = TRUE),
      label = ~ str_c(n, " hivernants"),
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"))} -> plot_wint

# Preparation du jeu de données -----------------------------------------------------

read_rds("./Output/dataset_Aythya.rds") -> jeu

jeu %>% 
  filter(obs == "BAGUAGE", 
         yday(datetime) %in% c(yday(ymd(20000415)):yday(ymd(20000731)))) %>% 
  mutate(origin = 
           case_when(dpt %in% c("44") ~ "Grand-Lieu",
                     dpt %in% c("53") ~ "Mayenne",
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
               distinct(ring, sp, sex, age, dpt, origin)) -> cont

# Winter spot selection -------------------------------------------------------------

cont %>% 
  mutate(winter = if_else(month(datetime) < 7, -1, 0) + year(datetime),
         wintering = (ymd(str_c(winter, "1224")) - date(datetime)) %>% as.numeric() %>% abs()) %>% 
  filter(wintering < 40) -> cont2

cont2 %>% 
  arrange(ring, winter, wintering) %>% 
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
  ungroup() -> cont3; cont3


# Effort par pays -------------------------------------------------------------------

cont3 %>% 
  st_drop_geometry() %>% 
  left_join(
    jeu %>% 
      select(id, co)) %>%
  mutate(wintering_spot = TRUE) %>% 
  select(ring, winter, co, age, wintering_spot) %>% 
  right_join(
    cont2 %>% 
      select(-age) %>% 
      left_join(
        jeu %>% 
          select(id, co))) %>% 
  filter(wintering_spot == TRUE) %>% 
  count(ring, winter, co, sp, sex, age) %>% 
  group_by(co) %>% 
  summarize(nobs_win = mean(n)) %>% 
  arrange(desc(nobs_win)) -> obs_pressure

cont3 %>% 
  left_join(
    jeu %>% 
      select(id, co)) %>% 
  left_join(obs_pressure %>% mutate(wgt = 1 / nobs_win)) %>%
  select(-nobs_win) -> cont4

# direction & angle -----------------------------------------------------------------

cont4 %>% 
  filter(dist > 10) %>% 
  ggplot(aes(x = dist, color = sp, group = sp, fill = sp)) + 
  geom_density(aes(weight = wgt), alpha = .3) +
  facet_wrap( ~ sp)
# en moyenne les oiseaux se distribuent selon une loi gamma

cont4 %>% 
  filter(dist > 10) %>% 
  ggplot(aes(x = angle, y = dist)) +
  geom_point() +
  geom_smooth(method = "gam") + 
  coord_polar(start = pi/2, direction = -1) +
  ylim(c(0, 1500)) +
  scale_x_continuous(limits = c(-180, 180), 
                     breaks = seq(-90, 180, length.out = 4),
                     labels = c("S", "E", "N", "W"))
# -> les oiseaux vont plus loin direction sud ouest

cont4 %>% 
  filter(dist > 10, origin == "West") %>% 
  ggplot(aes(x = angle, color = sp, group = sp, fill = sp)) + 
  # geom_histogram(binwidth = 22.5, boundary = -180, alpha = .3) +
  geom_density(alpha = .3, aes(weight = wgt)) +
  coord_polar(direction = - 1, start = pi/2) +
  scale_x_continuous(limits = c(-180, 180), 
                     breaks = seq(-90, 180, length.out = 4),
                     labels = c("S", "E", "N", "W")) +
  facet_wrap( ~ sp)

# Coline Land Cover ----------------------------------------------------------------

st_read("./Data/Coline_Land_Cover_2018/clc.shp", as_tibble = TRUE) -> clc

clc %>% 
  st_union() %>% 
  st_cast("POLYGON") -> part

part %>% 
  st_as_sf() %>%
  rename(geometry = x) %>% 
  mutate(st_centroid(part) %>% st_as_sf() %>% st_transform(4326) %>% rename(centroid = x),
         area = st_area(.) %>% drop_units(),
         perimeter = st_perimeter(.) %>% drop_units()) %>% 
  st_transform(4326) %>% 
  rowid_to_column() -> water

water %>% 
  st_filter(cont4, .predicate = st_contains) -> occupied

water %>% 
  filter((1e-6 * area) < 10) %>% 
  left_join(occupied %>% distinct(rowid) %>% mutate(occupied = TRUE)) %>% 
  mutate(across(occupied, replace_na, FALSE)) %>% 
  ggplot(aes(x = area * 1e-6, fill = occupied, group = occupied)) + 
  geom_histogram() + 
  facet_grid(rows = vars(occupied), scales = "free_y")

water %>% 
  filter(perimeter < 1e5) %>% 
  left_join(occupied %>% distinct(rowid) %>% mutate(occupied = TRUE)) %>% 
  mutate(across(occupied, replace_na, FALSE)) %>% 
  ggplot(aes(x = perimeter, fill = occupied, group = occupied)) + 
  geom_histogram() + 
  facet_grid(rows = vars(occupied), scales = "free_y")

# weighting water bodies ------------------------------------------------------------

water %>% 
  left_join(
    occupied %>% 
      select(rowid) %>% 
      as_tibble() %>% 
      mutate(occupied = TRUE)) %>% 
  filter(occupied == TRUE) %>% 
  glm(log(area) ~ 1, data = ., family = gaussian()) %>% 
  residuals() %>% 
  qqnorm()
# impossible à modéliser simplement, on passe par les quantiles


water %>% 
  slice(st_nearest_feature(cont4, water)) %>% 
  count(rowid) -> ds 
  


water %>% 
  as_tibble() %>% 
  slice(st_nearest_feature(cont4, water)) %>% 
  pull(area) %>% 
  quantile(seq(0, 1, length.out = 10)) %>% 
  as_tibble_col(column_name = "down") %>% 
  add_row(down = 0, .before = 1) %>% 
  mutate(
    up = c(down[-1] - 1e-6, max(water$area) + 1),
    down = down,
    wgt = c(0, rep(1, nrow(.) - 2), 0)) %>% 
  rowid_to_column("id") -> occup

water %>% 
  as_tibble() %>% 
  select(rowid, area) %>% 
  rowwise() %>% 
  mutate(id = which(
    (cbind(
      as.numeric(occup$down <= area), 
      as.numeric(occup$up >= area)) %>% 
       rowSums()) == 2)) %>% 
  ungroup() -> cat_wb

occup %>% 
  left_join(cat_wb %>% count(id)) %>% 
  mutate(wgt = wgt / n,
         wgt  = wgt / sum(wgt))

  
  