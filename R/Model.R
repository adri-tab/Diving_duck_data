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

options(viewer = NULL) 

# Leaflet function ------------------------------------------------------------------

leaflet() %>%
  setView(lng = 30, lat = 55, zoom = 4) %>%
  addProviderTiles(providers$CartoDB.DarkMatter,
                   group = "Fond noir") %>% 
  addProviderTiles(providers$CartoDB.Positron,
                   group = "Fond clair") -> map_base

plot_leaf <- function(
    ds, 
    legend = " hivernants") {

  map_base %>% 
    
    addPolygons(
      data = ds,
      group = "presence",
      fillColor = ~ col,
      fillOpacity = 0.7,
      color =  ~ col,
      opacity = 1,
      weight = 1,
      highlightOptions = highlightOptions(
        weight = 5, 
        bringToFront = TRUE),
      label = ~ str_c(n, legend)) %>%
    
    addPolygons(
      data = sys_b %>% 
        filter(!sys_id %in% (
          ds %>% as_tibble() %>% pull(sys_id))) %>% 
        st_combine(),
      group = "absence",
      fillColor = "#0c4da2",
      fillOpacity = 0.7,
      color = "#0c4da2",
      opacity = 1,
      weight = 1) %>% 
    
    addLayersControl(
      position = "topleft",
      baseGroups = c("Fond noir", "Fond clair"),
      overlayGroups = c("presence", "absence"),
      options = layersControlOptions(collapsed = FALSE)) %>%
    
    addLegend(
      data = ds %>% as_tibble() %>% arrange(lv_id) %>% distinct(lv, col) %>% 
        add_row(lv = 0, col = "#0c4da2", .before = 1),
      position = "topright",
      labels = ~ lv,
      colors = ~ col, 
      title = NULL, 
      opacity = 0.7)
}


# Preparation du jeu de données -----------------------------------------------------

read_rds("./Output/dataset_Aythya.rds") -> jeu

jeu %>% 
  filter(obs == "BAGUAGE", 
         yday(datetime) %in% c(yday(ymd(20000415)):yday(ymd(20000731)))) %>% 
  mutate(nes_reg = 
           case_when(dpt %in% c("44", "53") ~ "Ouest",
                     dpt %in% c("41", "77", "89") ~ "Centre",
                     dpt %in% c("42", "01", "51", "55") ~ "Est") %>% 
           as_factor(),
         across(c(sex, age), ~ .x %>% replace_na("ind"))) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) %>% 
  rename(nes_dpt = dpt, nes_loc = geometry, n_lon = lon, n_lat = lat) %>% 
  as_tibble() -> ring_sel

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
        yday(datetime) %in% c(yday(ymd(20001116)):yday(ymd(20001231)), 
                              yday(ymd(20000101)):yday(ymd(20000215))) ~ "hivernage",
        yday(datetime) %in% yday(ymd(20000216)):yday(ymd(20000414)) ~ "départ_repro") %>% 
      factor(levels = c("repro", "mue", "post-mue", "départ_hivernage", "hivernage", 
                        "départ_repro"))) %>% 
  inner_join(ring_sel %>% 
               filter(nobs > 1) %>% 
               distinct(ring, sp, sex, age, nes_dpt, nes_reg)) -> cont

# Winter spot selection -------------------------------------------------------------

cont %>% 
  mutate(winter = if_else(month(datetime) < 7, -1, 0) + year(datetime),
         wintering = (ymd(str_c(winter, "1224")) - date(datetime)) %>% 
           as.numeric() %>% abs()) %>% 
  filter(wintering < 40) %>% 
  select(-period) -> cont2

cont2 %>% 
  arrange(ring, winter, wintering) %>% 
  distinct(ring, winter, .keep_all = TRUE) %>% 
  select(-wintering) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) %>% 
  rename(win_loc = geometry) %>% 
  left_join(ring_sel %>% 
              select(ring, n_datetime = datetime, n_lon, n_lat, nes_loc, nes_reg)) %>% 
  mutate(angle_deg = atan2(lat - n_lat, lon - n_lon) * 180/pi) %>% 
  rowwise() %>% 
  mutate(dist_pt_km = st_distance(nes_loc, win_loc, by_element = TRUE) %>% 
           drop_units() %>% "*"(1e-3),
         age = case_when(
           age %in% c("1A", "2A", "pull") & winter == year(n_datetime) ~ "je",
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
  mutate(nb = nrow(.), 
         wgt = nb * wgt / sum(wgt)) %>% 
  select(id, obs, ring, sp, sex, age, 
         win = winter,
         angle_deg, dist_pt_km, 
         win_date = datetime, win_loc, 
         nes_date = n_datetime, nes_loc,
         win_co = co, win_wgt = wgt, nes_dpt, nes_reg) -> cont4

# Explo direction et angle  ----------------------------------------------------------

cont4 %>% 
  filter(dist_pt_km > 10, sex != "ind") %>% 
  ggplot(aes(x = dist_pt_km, color = sp, group = sp, fill = sp)) + 
  geom_density(aes(weight = win_wgt), alpha = .3) +
  facet_wrap(sex ~ sp)
# en moyenne les oiseaux se distribuent selon une loi gamma

cont4 %>% 
  filter(dist_pt_km > 10) %>% 
  ggplot(aes(x = angle_deg, y = dist_pt_km)) +
  geom_point() +
  geom_smooth(method = "gam") + 
  coord_polar(start = pi/2, direction = -1) +
  ylim(c(0, 1500)) +
  scale_x_continuous(limits = c(-180, 180), 
                     breaks = seq(-90, 180, length.out = 4),
                     labels = c("S", "E", "N", "W"))
# -> les oiseaux vont plus loin direction sud ouest depuis leur site de nidif

cont4 %>% 
  filter(dist_pt_km > 10) %>% 
  ggplot(aes(x = angle_deg, color = sp, group = sp, fill = sp)) + 
  # geom_histogram(binwidth = 22.5, boundary = -180, alpha = .3) +
  geom_density(alpha = .3, aes(weight = win_wgt)) +
  coord_polar(direction = - 1, start = pi/2) +
  scale_x_continuous(limits = c(-180, 180), 
                     breaks = seq(-90, 180, length.out = 4),
                     labels = c("S", "E", "N", "W")) +
  facet_grid(nes_reg ~ sp)
# ils ne se dirigent pas préférentiellement au sud


# Corine Land Cover manip ---------------------------------------------------------------------

# 411 inland marshes
# 412 peat bogs
# 421 salt marshes
# 422 salines
# 423 intertidal flats
# 511 water courses
# 512 water bodies
# 521 coastal lagoons
# 522 estuaries
# 523 sea and ocean

# st_read("../Database/corine_land_cover_water_only/clc2018_water.shp",
#         as_tibble = TRUE) %>%
#   st_set_crs(value = 3035) -> clc
# 
# # clusters des wb adjacents
# clc %>%
#   select(g_wb = geometry,
#          sys_type = Code_18) %>%
#   st_cast("POLYGON") %>%
#   mutate(sys_type =
#            case_when(
#              sys_type %in% c(411, 512) ~ "soft_wb",
#              sys_type %in% c(412) ~ "peat",
#              sys_type %in% c(423, 522) ~ "coast",
#              sys_type %in% c(421, 422, 521) ~ "salt_wb",
#              sys_type %in% c(511) ~ "river",
#              sys_type %in% c(523) ~ "sea")) %>%
#   group_by(sys_type) %>%
#   mutate(cluster = g_wb %>%
#            st_intersects() %>%
#            graph_from_adj_list() %>%
#            components() %>%
#            pluck(membership) %>%
#            as.integer()) %>%
#   add_count(sys_type, cluster) -> clc2
# 
# # regroupement des wb adjacents
# clc2 %>%
#   filter(n > 1) %>%
#   group_by(sys_type, cluster) %>%
#   summarize(across(g_wb, st_union)) %>%
#   bind_rows(clc2 %>% filter(n == 1)) %>%
#   ungroup() %>%
#   select(-cluster, -n) %>%
#   st_cast("POLYGON") %>%
#   # -> st_union fournit un multipolygon avec st_intersects si 1 seul point en commun
#   # donc on prend st_cast pour avoir des polygons uniques
#   rowid_to_column("wb_id") -> clc3
# 
# # -> !!! methode bourrin directe : st_union() de tous les wb et st_cast("POLYGON"), mais très lourd
# 
# # clusters des wb à moins de 1km pour décrire les systèmes
#   # methode plus rapide que d'utiliser st_is_within_distance()
# clc3 %>%
#   mutate(
#     across(g_wb,
#            ~ st_simplify(.x, preserveTopology = TRUE, dTolerance = 100))) -> sys1 # on lisse à 30 mètres
# # on buffer à la bonne distance
# sys1 %>%
#   mutate(
#     across(g_wb,
#            ~ st_buffer(.x, 10e2))) -> sys2 # semi-distance max pour associer
# # on cluster
# sys2 %>%
#   group_by(sys_type) %>%
#   mutate(cluster = g_wb %>%
#            st_intersects() %>%
#            graph_from_adj_list() %>%
#            components() %>%
#            pluck(membership) %>%
#            as.integer()) %>%
#   ungroup() %>%
#   add_count(sys_type, cluster) -> sys3
# #
# sys3 %>%
#   st_drop_geometry() %>%
#   right_join(clc3) -> clc4
# 
# # creation of water system
# clc4 %>%
#   filter(n > 1) %>%
#   group_by(sys_type, cluster) %>%
#   summarize(across(g_wb, st_combine)) %>%
#   ungroup() %>%
#   bind_rows(clc4 %>% filter(n == 1) %>% select(-wb_id)) %>%
#   select(-n) %>%
#   rename(g_sys = g_wb) %>%
#   rowid_to_column("sys_id") -> sys4
# 
# sys4 %>%
#   mutate(sys_area_ha = g_sys %>% st_area() %>% drop_units() %>% "*"(1e-4)) %>%
#   mutate(across(g_sys, ~ .x %>% st_transform(4326))) %>%
#   select(sys_id, sys_type, sys_area_ha, g_sys) %>%
#   arrange(sys_type, sys_id) -> sys5
# 
# clc4 %>%
#   left_join(sys4 %>% select(-g_sys)) %>%
#   mutate(wb_area_ha = g_wb %>% st_area() %>% drop_units() %>% "*"(1e-4)) %>%
#   mutate(across(g_wb, ~ .x %>% st_transform(4326))) %>%
#   select(wb_id, sys_id, sys_type, sys_n = n, wb_area_ha, g_wb) %>%
#   arrange(sys_type, sys_id, wb_id) -> clc5
# 
# write_rds(sys5, "../Database/clc_modified/system.rds")
# write_rds(clc5, "../Database/clc_modified/waterbody.rds")

# Attribution à des masses d'eau ----------------------------------------------------

read_rds("./Data_water/waterbody.rds") %>% 
  filter(sys_type %in% c("soft_wb", "salt_wb")) %>% 
  st_as_sf() -> wat_b

read_rds("./Data_water/system.rds") %>% 
  filter(sys_type %in% c("soft_wb", "salt_wb")) %>% 
  st_as_sf() -> sys_b

wat_b %>% 
  slice(st_nearest_feature(ring_sel %>% st_set_geometry("nes_loc"), 
                           wat_b)) %>% 
  bind_cols(ring_sel %>% st_set_geometry("nes_loc"), 
            .) %>% 
  mutate(dist_alloc_km = st_distance(nes_loc, g_wb, by_element = TRUE) %>% 
           drop_units() %>% "*"(1e-3)) %>% 
  as_tibble() %>% 
  left_join(sys_b %>% as_tibble()) -> nes_spot

wat_b %>% 
  slice(st_nearest_feature(cont4, 
                           wat_b)) %>% 
  bind_cols(cont4, 
            .) %>% 
  as_tibble() %>% 
  left_join(nes_spot %>% select(ring, g_wb_nes = g_wb)) %>% 
  mutate(dist_alloc_km = st_distance(win_loc, g_wb, by_element = TRUE) %>% 
           drop_units() %>% "*"(1e-3),
         dist_wb_km = st_distance(g_wb, g_wb_nes, by_element = TRUE) %>% 
           drop_units() %>% "*"(1e-3)) %>%
  left_join(sys_b %>% as_tibble()) -> win_spot

win_spot %>% 
  filter(dist_alloc_km < 100,
         dist_pt_km < 10 | dist_wb_km < 10) %>% 
  ggplot(aes(x = dist_pt_km, y = dist_wb_km, color = dist_alloc_km)) +
  geom_point()

seecol(usecol(pal_pinky, n = 7))

win_spot %>% 
  count(sys_id) %>%
  rowwise() %>% 
  mutate(tibble(n, lv = c(1, 2, 5, 10, 20, 50, 100)) %>% 
           rowid_to_column("lv_id") %>% 
           filter(lv >= n) %>% slice(1)) %>% 
  ungroup() %>% 
  mutate(col = usecol(pal_pinky, n = n_distinct(lv))[lv_id]) %>% 
  left_join(sys_b) %>% 
  st_set_geometry("g_sys") %>% 
  plot_leaf()
  
win_spot %>% 
  filter(dist_alloc_km > 1) %>% 
  count(sys_id, sys_type) %>% 
  arrange(desc(n)) %>%
  rowwise() %>% 
  mutate(tibble(n, lv = c(1:7)) %>% 
           rowid_to_column("lv_id") %>% 
           filter(lv >= n) %>% slice(1)) %>% 
  ungroup() %>% 
  mutate(col = usecol(pal_pinky, n = n_distinct(lv))[lv_id]) %>% 
  left_join(sys_b) %>% 
  st_set_geometry("g_sys") %>% 
  plot_leaf(legend = " oiseaux à plus de 1 km")

# ok revoir les masses d'eau CLC pour que les oiseaux tombent aux bons endroits. 
# On peut mieux faire


# Models Leo ----------------------------------------------------------------------------------

win_spot %>% 
  filter(nes_reg == "Ouest") %>% 
  count(ring, sp) %>% 
  mutate(n = if_else(n > 1, 0, 1)) %>% 
  count(sp, n)

# proportion de migrants
win_spot %>% 
  mutate(mig = if_else(dist_pt_km > 10, 1, 0)) %>%
  filter(nes_reg == "Ouest") %>% 
  lme4::glmer(mig ~ sp + (1|ring), data = ., family = binomial(), weights = win_wgt) -> mod_mig

confint(mod_mig)

# distance des migrants
win_spot %>% 
  filter(dist_pt_km > 10, 
         nes_reg == "Ouest") %>% 
  ggplot(aes(x = dist_pt_km, color = sp, group = sp, fill = sp)) + 
  geom_density(aes(weight = win_wgt), alpha = .3) +
  facet_wrap( ~ sp)
  
win_spot %>% 
  filter(dist_pt_km > 10, 
         nes_reg == "Ouest") %>% 
  # lme4::glmer(log(dist_pt_km) ~ sp + (1|ring), data = ., weights = win_wgt) %>% 
  lme4::glmer(dist_pt_km ~ sp + (1|ring), data = ., family = Gamma(link = "log"), weights = win_wgt) -> mod_dis

summary(mod_dis)$varcor$ring %>% attr("stddev") + summary(mod_dis)$varcor %>% attr("sc") -> sig_dis
1/ (sig_dis^2) -> shape_dis
exp(summary(mod_dis)$coefficients[1, 1] + sig_dis^2 / 2) / shape_dis -> scale_dis_fer
exp(summary(mod_dis)$coefficients[1, 1] + summary(mod_dis)$coefficients[2, 1] + sig_dis^2 / 2) / shape_dis -> scale_dis_ful

dgamma(1:1500, shape = shape_dis, scale = scale_dis_fer) %>% plot()
  
win_spot %>% 
  filter(dist_pt_km > 10, 
         nes_reg == "Ouest") %>% 
  glm(log(dist_pt_km) ~ sp, data = ., weights = win_wgt) -> mod_dis2

summary(mod_dis2)$varcor$ring %>% attr("stddev") + summary(mod_dis)$varcor %>% attr("sc") -> sig_dis
1/ (sig_dis^2) -> shape_dis
exp(summary(mod_dis)$coefficients[1, 1] + sig_dis^2 / 2) / shape_dis -> scale_dis_fer
exp(summary(mod_dis)$coefficients[1, 1] + summary(mod_dis)$coefficients[2, 1] + sig_dis^2 / 2) / shape_dis -> scale_dis_ful

dgamma(1:1500, shape = shape_dis, scale = scale_dis_fer) %>% plot()




# Poids des types de masses d'eau -------------------------------------------------------------

wat_b %>% 
  left_join(
    cont6 %>% 
      st_drop_geometry() %>% 
      distinct(rowid) %>% 
      mutate(occupied = TRUE)) %>% 
  filter(occupied == TRUE) %>% 
  glm(log(wb_area_ha) ~ 1, data = ., family = gaussian()) %>% 
  residuals() %>% 
  qqnorm()
# impossible à modéliser simplement, on passe par les quantiles

plot_wint(cont6 %>% count(rowid))

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
         wgt = wgt / sum(wgt))

  
  