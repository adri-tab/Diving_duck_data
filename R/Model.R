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

options(viewer = NULL) 

# Great circle azimuth bearing function -----------------------------------------------

# https://www.igismap.com/formula-to-find-bearing-or-heading-angle-between-two-points-latitude-longitude/

bearing_fun <- function(x1, x2, y1, y2) {
 atan2(
   cos(y2 * pi / 180) * sin((x2 - x1) * pi / 180), 
   cos(y1 * pi / 180) * sin(y2 * pi / 180) - 
     sin(y1 * pi / 180) * cos(y2 * pi / 180) * cos((x2 - x1) * pi / 180)) * 
    180 / pi
}

basic_fun <- function(x1, x2, y1, y2) {
  atan2(x2 - x1, y2 - y1) * 
    180 / pi
}

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
  mutate(angle_deg = bearing_fun(x1 = n_lon, x2 = lon, y1 = n_lat, y2 = lat),
         angle_deg_base = basic_fun(x1 = n_lon, x2 = lon, y1 = n_lat, y2 = lat)) %>% 
  rowwise() %>% 
  mutate(dist_pt_km = st_distance(nes_loc, win_loc, by_element = TRUE) %>% 
           drop_units() %>% "*"(1e-3),
         age = case_when(
           age %in% c("1A", "2A", "pull") & winter == year(n_datetime) ~ "je",
           TRUE ~ "ad")) %>% 
  ungroup() -> cont3; cont3


# Effort par pays -------------------------------------------------------------------

c(-1.66, 47.1) %>% 
  st_point() %>% 
  st_sfc(crs = 4326) %>% 
  st_buffer(dist = set_units(7, "km")) %>%  
  st_sf() %>% 
  mutate(GL = TRUE) -> GLieu

jeu %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  st_join(GLieu, join = st_intersects) %>% 
  as_tibble() %>% 
  mutate(co = if_else(is.na(GL), co, "GRAND-LIEU")) %>% 
  select(id, co) -> jeu_co

cont3 %>% 
  st_drop_geometry() %>% 
  left_join(jeu_co) %>%
  mutate(wintering_spot = TRUE) %>% 
  select(ring, winter, co, age, wintering_spot) %>% 
  right_join(
    cont2 %>% 
      select(-age) %>% 
      left_join(jeu_co)
    ) %>% 
  filter(wintering_spot == TRUE) %>% 
  count(ring, winter, co, sp, sex, age) %>% 
  group_by(co) %>% 
  summarize(nobs_win = mean(n)) %>% 
  arrange(desc(nobs_win)) -> obs_pressure

cont3 %>% 
  left_join(jeu_co) %>% 
  left_join(obs_pressure %>% mutate(wgt = 1 / nobs_win,
                                    pres = nobs_win)) %>%
  mutate(nb = nrow(.), 
         wgt = nb * wgt / sum(wgt),
         pres = nb * pres / sum(pres)) %>% 
  select(id, obs, ring, sp, sex, age, 
         win = winter,
         angle_deg, angle_deg_base, dist_pt_km, 
         win_date = datetime, win_loc, 
         nes_date = n_datetime, nes_loc,
         win_co = co, wgt_co = wgt, pres_co = pres, nes_dpt, nes_reg) -> cont4

# Explo direction et angle  ---------------------------------------------------------

cont4 %>% 
  filter(dist_pt_km > 10, sex != "ind") %>% 
  ggplot(aes(x = dist_pt_km, color = sp, group = sp, fill = sp)) + 
  geom_density(aes(weight = wgt_co), alpha = .3) +
  facet_wrap(sex ~ sp)
# en moyenne les oiseaux se distribuent selon une loi gamma

cont4 %>% 
  filter(dist_pt_km > 10) %>% 
  ggplot(aes(x = angle_deg, y = dist_pt_km)) +
  geom_point() +
  geom_smooth(method = "gam") + 
  coord_polar(start = pi) +
  ylim(c(0, 1500)) +
  scale_x_continuous(limits = c(-180, 180), 
                     breaks = seq(-90, 180, length.out = 4),
                     labels = c("W", "N", "E", "S"))
# -> les oiseaux vont plus loin direction sud ouest depuis leur site de nidif

cont4 %>% 
  filter(dist_pt_km > 10) %>% 
  ggplot(aes(x = angle_deg, color = sp, group = sp, fill = sp)) + 
  # geom_histogram(binwidth = 22.5, boundary = -180, alpha = .3) +
  geom_density(alpha = .3, aes(weight = wgt_co)) +
  coord_polar(start = pi) +
  scale_x_continuous(limits = c(-180, 180), 
                     breaks = seq(-90, 180, length.out = 4),
                     labels = c("W", "N", "E", "S")) +
  facet_grid(nes_reg ~ sp)
# ils ne se dirigent pas préférentiellement au sud


# Corine Land Cover manip -----------------------------------------------------------

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

seecol(pal_pinky, n = 7)


# Leaflet mapping -------------------------------------------------------------------

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


# Model data formatting -------------------------------------------------------------

# intérêt de l'effet aléatoire
win_spot %>% 
  filter(nes_reg == "Ouest") %>% 
  count(ring, sp) %>% 
  mutate(n = if_else(n > 1, "n hivers", "1 hiver")) %>% 
  count(sp, n)

# préparation des données avec et sans répétition par indiv
win_spot %>% 
  filter(nes_reg == "Ouest") %>% 
  mutate(mig = if_else(dist_pt_km > 10, 1, 0)) -> data_full

data_full %>% 
  add_count(ring) %>% 
  group_by(ring) %>%
  mutate(win_avg = abs(dist_pt_km - mean(dist_pt_km))) %>%
  arrange(ring, win_avg) %>% 
  slice(1) %>% 
  ungroup() -> data_simp

data_full %>% 
  filter(mig == 1, 
         dist_pt_km < 2000) -> data_full2

data_simp %>% 
  filter(mig == 1, 
         dist_pt_km < 2000) -> data_simp2

# Model prop migration --------------------------------------------------------------

# fit
glm(mig ~ sp, data = data_simp, family = binomial(), weights = wgt_co) -> mod_mig

lme4::glmer(mig ~ sp + (1|ring), data = data_full, 
            family = binomial(), weights = wgt_co) -> mod_rand_mig

# Residuals distribution
tibble(x = residuals(mod_mig)) %>% 
  ggplot(aes(sample = x)) + 
  # geom_histogram(aes(x = x))
  geom_qq_line() +
  geom_qq()

tibble(x = residuals(mod_rand_mig)) %>% 
  ggplot(aes(sample = x)) + 
  # geom_histogram(aes(x = x))
  geom_qq_line() +
  geom_qq()

# Predicts
data_full %>% 
  distinct(sp) %>% 
  mutate(fit = predict(mod_mig, type = "response", newdata = .),
         se_fit = predict(mod_mig, type = "response", newdata = ., se.fit = TRUE)$se.fit,
         conf_fit_low = fit - 1.96 * se_fit,
         conf_fit_high = fit + 1.96 * se_fit) -> mig_prob

predict(mod_rand_mig, type = "response")

# Model distance -------------------------------------------------------------------

data_simp2 %>% 
  ggplot(aes(x = dist_pt_km, color = sp, group = sp, fill = sp, weight = wgt_co)) + 
  geom_density(alpha = 0, linetype = 2, adjust = 2, show.legend = FALSE) +
  geom_histogram(aes(y = ..density..), alpha = .3, bins = 10, show.legend = FALSE, boundary = 0) +
  scale_x_continuous(n.breaks = 10) +
  facet_wrap( ~ sp, scales = "free_y", ncol = 1)

glm(data = data_simp2, dist_pt_km ~ sp, family = Gamma(link = "log"), weights = wgt_co) -> mod_dis
lme4::glmer(data = data_full2, dist_pt_km ~ sp + (1|ring), 
            family = Gamma(link = "log"), weights = wgt_co) -> mod_rand_dis

# Residuals distribution
tibble(x = residuals(mod_dis)) %>% 
  ggplot(aes(sample = x)) + 
  # geom_histogram(aes(x = x))
  geom_qq_line() +
  geom_qq()

tibble(x = residuals(mod_rand_dis)) %>% 
  ggplot(aes(sample = x)) + 
  # geom_histogram(aes(x = x))
  geom_qq_line() +
  geom_qq()

# Predicts
data_full2 %>% 
  distinct(sp) %>% 
  mutate(fit = predict(mod_dis, type = "response", newdata = .),
         se_fit = predict(mod_dis, type = "response", newdata = ., se.fit = TRUE)$se.fit,
         conf_fit_low = fit - 1.96 * se_fit,
         conf_fit_high = fit + 1.96 * se_fit)

predict(mod_rand_dis, type = "response")

exp(coef(mod_dis)[1]) -> med_fer
exp(sum(coef(mod_dis))) -> med_ful
summary(mod_dis)$dispersion -> disp

bind_rows(
  tibble(
    sp = "AYTFER",
    val = rgamma(1e5, shape = 1 / disp, rate = 1 / (disp * med_fer))),
  tibble(
    sp = "AYTFUL",
    val = rgamma(1e5, shape = 1 / disp, rate = 1 / (disp * med_ful)))) %>% 
  filter(val < 2000) -> mod_dis_sim 

ggplot() + 
  geom_histogram(data = data_simp2, 
                 aes(x = dist_pt_km, y = ..density.., color = sp, fill = sp, weight = wgt_co),
                 alpha = .3, bins = 13, show.legend = FALSE, boundary = 0) +
  scale_x_continuous(n.breaks = 10, limits = c(0, 2000)) +
  geom_density(data = mod_dis_sim, 
               aes(x = val, color = sp), 
               linetype = "dashed", show.legend = FALSE) +
  facet_wrap( ~ sp, scales = "free_y", ncol = 1)

# Poids associé aux caractéristiques des masses d'eau --------------------

win_spot %>% 
  select(wb_area_ha, wgt_co) %>% 
  mutate(status = "occupied") %>% 
  bind_rows(
    wat_b %>% 
      st_drop_geometry() %>% 
      select(wb_area_ha) %>% 
      mutate(status = "all", 
             wgt_co = 1)
  ) %>% 
  ggplot(aes(x = wb_area_ha, fill = status, color = status, weight = wgt_co)) + 
  geom_histogram(boundary = 0, alpha = 0.5) +
  scale_x_log10() +
  facet_wrap(~ status, ncol = 1, scales = "free_y")

# distribution pourries des sites occupés, 
# pas de modélisation possible
# donc on pondère par la méthode des quantiles

win_spot %>% 
  pull(wb_area_ha) %>% 
  quantile(seq(0, 1, length.out = 8)) %>% 
  as_tibble_col(column_name = "down") %>% 
  add_row(down = 0, .before = 1) %>% 
  mutate(
    up = c(down[-1] - 1e-6, max(wat_b$wb_area_ha) + 1),
    down = down,
    wgt = c(0, rep(1, nrow(.) - 2), 0)) %>% 
  rowid_to_column("id") -> occup

wat_b %>% 
  as_tibble() %>% 
  select(wb_id, wb_area_ha) %>% 
  rowwise() %>% 
  mutate(id = which(
    (cbind(
      as.numeric(occup$down <= wb_area_ha), 
      as.numeric(occup$up >= wb_area_ha)) %>% 
       rowSums()) == 2)) %>% 
  select(-wb_area_ha) %>% 
  add_count(id) %>% 
  group_by(id, n) %>% 
  nest() %>% 
  right_join(occup) %>% 
  arrange(id) %>% 
  mutate(wgt_area = wgt / n) %>% 
  unnest(data) %>% 
  ungroup() %>% 
  select(wb_id, wgt_area) %>% 
  left_join(
    wat_b %>% 
      as_tibble() %>% 
      mutate(g_wb = st_centroid(g_wb))) %>% 
  select(wb_id, wgt_area, ctr_wb = g_wb) -> cat_wb


# N'EST PLUS UTILISE - Poids associé à la pression d'observation (par pays et Grand Lieu) ----

ne_countries(scale = "large", returnclass = "sf") %>% 
  st_transform(4326) %>% 
  as_tibble() %>% 
  filter(continent == "Europe") %>% 
  select(name, geometry) %>% 
  mutate(win_co = if_else(name == "United Kingdom",
                          "U.K. OF GREAT BRITAIN AND NORTHERN IRELAND",
                          str_to_upper(name))) %>% 
  select(-name) -> cou_tmp

cou_tmp %>% 
  filter(win_co != "FRANCE") %>% 
  bind_rows(
    st_combine(c(cou_tmp %>% filter(win_co == "FRANCE") %>% pull(geometry), GLieu$geometry)) %>% 
      st_sf() %>% mutate(win_co = "FRANCE"),
    GLieu %>% mutate(win_co = "GRAND-LIEU") %>% select(-GL) %>% st_cast("MULTIPOLYGON")) %>% 
  inner_join(cont4 %>% 
               distinct(win_co, wgt_co, pres_co)) %>% 
  st_set_geometry("geometry") -> cou

map_base %>% 
  addPolygons(
    data = cou %>% mutate(col = usecol(pal_unikn_pref, n = nrow(.))),
    fillColor = ~ col,
    fillOpacity = 0.7, 
    color = ~ col, 
    weight = 1, 
    group = ~ win_co) %>% 
  addLayersControl(
    overlayGroups = cou %>% as_tibble() %>% pull(win_co),
    options = layersControlOptions(collapsed = FALSE))

st_join(
  x = cat_wb %>% 
    st_set_geometry("ctr_wb"),
  y = cou %>% 
    st_set_geometry("geometry"), 
  join = st_covered_by, 
  left = TRUE) %>% 
  mutate(wgt_pres = if_else(is.na(pres_co), 1, pres_co)) %>% 
  select(-c(wgt_co, win_co, pres_co)) -> cat_wb2

# Poids associé à la distance aux oiseaux ---------------------------------------

nes_spot %>%
  filter(ring %in% data_simp2$ring) %>%
  mutate(g_wb = st_centroid(g_wb),
         n_lon = st_coordinates(g_wb)[, 1],
         n_lat = st_coordinates(g_wb)[, 2]) %>%
  select(ring, sp, nes_loc = g_wb, n_lon, n_lat) %>%
  nest(rings = ring) %>% 
  mutate(n = map_int(rings, nrow)) %>% 
  left_join(
    tibble(
      sp = unique(win_spot$sp),
      shape_dis = 1 / disp,
      rate_dis = 1 / (disp * c(med_fer, med_ful)))) %>%
  mutate(cat_wb = list(
    cat_wb %>%
      filter(wgt_area != 0) %>% 
      mutate(lon = st_coordinates(ctr_wb)[, 1],
             lat = st_coordinates(ctr_wb)[, 2]) %>%
      as_tibble())) %>%
  rowid_to_column() %>% 
  split(~ rowid) %>% 
  map(~ .x %>% 
  lazy_dt() %>% 
  mutate(cat_wb = cat_wb %>% 
           map(~ .x %>% 
                 mutate(
                   dist_km = st_distance(nes_loc, ctr_wb, by_element = TRUE) %>%
                     units::drop_units() %>% "*"(1e-3),
                   wgt_dis = dgamma(dist_km, shape = shape_dis, rate = rate_dis),
                   angle_deg = bearing_fun(x1 = n_lon, x2 = lon, y1 = n_lat, y2 = lat),
                   angle_deg_base = basic_fun(x1 = n_lon, x2 = lon, y1 = n_lat, y2 = lat),
                   wgt = wgt_area * wgt_dis
                 ) %>% 
                 select(wb_id, wgt, angle_deg, angle_deg_base, dist_km))) %>% 
  as_tibble()) -> data_for_sim

# Tirage prenant en compte les "caractéristiques intrinsèques" et la "distance" -----

data_simp2 %>% 
  ggplot(aes(x = angle_deg, color = sp, group = sp, fill = sp)) + 
  # geom_histogram(binwidth = 22.5, boundary = -180, alpha = .3) +
  geom_density(alpha = .3, aes(weight = wgt_co)) +
  coord_polar(start = pi) +
  scale_x_continuous(limits = c(-180, 180), 
                     breaks = seq(-90, 180, length.out = 4),
                     labels = c("W", "N", "E", "S")) +
  facet_grid( ~ sp)

1:1e3 %>% 
  map(function(i) {
    data_for_sim %>% 
      # '['(1) %>% 
      map(~ .x %>% 
            select(sp, nes_loc, rings, n, cat_wb) %>% 
            mutate(angle_deg = cat_wb %>% 
                     map(~ sample(
                       x = .x$angle_deg, 
                       size = n, 
                       replace = TRUE, 
                       prob = .x$wgt))) %>% 
            select(-cat_wb)) %>% 
      bind_rows() %>% 
      unnest(c(angle_deg, rings)) %>% 
      select(sp, ring, angle_deg) %>% 
      mutate(migration = "potential",
             id = i)}) %>% 
  bind_rows() %>% 
  bind_rows(data_simp2 %>% 
              ungroup() %>% 
              # filter(nes_dpt == 44) %>% 
              mutate(migration = "realized",
                     id = 0) %>% 
              select(sp, ring, angle_deg, migration, id, wgt_co), .) %>% 
  mutate(across(wgt_co, ~ if_else(is.na(.x), 1, wgt_co))) -> sim

bind_rows(sim, sim, sim) %>% 
  mutate(angle_deg = angle_deg + rep(-1:1, nrow(sim)) %>% sort %>% "*"(360)) %>% 
  group_split(sp, migration) %>% 
  map( ~ 
         .x$angle_deg %>% 
         density(weight = .x$wgt_co, bw = 12, n = 1000) %>% 
         "["(1:2) %>% 
         bind_rows() %>% 
         set_names("angle_deg", "density") %>% 
         filter(angle_deg >= -180, angle_deg <= 180) %>% 
         mutate(density = density / sum(density)) %>% 
         bind_cols(.x %>% distinct(sp, migration))) %>% 
  bind_rows() %>% 
  ggplot(aes(x = angle_deg, y = density, color = migration, group = migration, fill = migration)) + 
  geom_area(alpha = .3) +
  coord_polar(start = pi) +
  scale_x_continuous(limits = c(-180, 180), 
                     breaks = seq(-90, 180, length.out = 4),
                     labels = c("W", "N", "E", "S")) +
  facet_grid( ~ sp)

# Tropisme -----------------------------------------------------------------------

sim %>% 
  mutate(angle_rad = angle_deg * pi / 180,
         East = sin(angle_rad),
         North = cos(angle_rad)) %>% 
  pivot_longer(cols = c(East, North), 
               names_to = "axis", values_to = "proj") -> trop

trop %>% 
  ggplot(aes(x = proj, 
             color = migration, 
             fill = migration, 
             group = migration)) + 
  geom_vline(xintercept = 0, linetype = "dotted") + 
  geom_density(alpha = .3, aes(weight = wgt_co), adjust = 1) +
  facet_grid(sp ~ axis)

trop %>%
  ungroup() %>% 
  group_split(sp, id, axis) %>% 
  map(function(x) {
             dst = density(x$proj, weights = x$wgt_co, from = -1, to = 1)
             out = tibble(x = dst$x, y = dst$y, bw = dst$bw)
             up = sum(out$y[out$x > 0] * out$bw[out$x > 0]) / sum(out$y * out$bw)
             bind_cols(x %>% distinct(sp, id, axis), tibble(prop = up))
           }) %>% 
  bind_rows() -> trop2

ggplot() + 
  geom_density(data = trop2 %>% 
                 filter(id != 0, 
                        axis == "North"),
               aes(x = prop, color = sp, fill = sp), 
               alpha = .3, 
               adjust = 2, show.legend = FALSE) + 
  scale_x_continuous(limits = c(0, 1), breaks = c(0, .5, 1), labels = c("100%\nsouthward", "50% southward\n50% northward", "100%\nnorthward")) +
  geom_vline(data = trop2 %>% filter(id == 0, axis == "North"), aes(xintercept = prop), linetype = "dotted") +
  facet_wrap(~ sp, ncol = 1)

ggplot() + 
  geom_density(data = trop2 %>% 
                 filter(id != 0, 
                        axis == "East"),
               aes(x = prop, color = sp, fill = sp), 
               alpha = .3, 
               adjust = 2, show.legend = FALSE) + 
  scale_x_continuous(limits = c(0, 1), breaks = c(0, .5, 1), labels = c("100%\nwestward", "50% westward\n50% eastward", "100%\neastward")) +
  geom_vline(data = trop2 %>% filter(id == 0, axis == "East"), aes(xintercept = prop), linetype = "dotted") +
  facet_wrap(~ sp, ncol = 1)

# tropisme pour le nord, pas pour l'est
# on peut faire un test

trop2 %>% 
  filter(id != 0) %>% 
  left_join(trop2 %>% 
              filter(id == 0) %>% 
              select(sp, axis, realized = prop)) %>% 
  group_by(sp, axis) %>% 
  summarize(realized = unique(realized),
            s1 = EnvStats::ebeta(prop)$parameters[1],
            s2 = EnvStats::ebeta(prop)$parameters[2],
            conf_int_low = qbeta(p = 0.025, s1, s2),
            conf_int_high = qbeta(p = 0.975, s1, s2),
            p_critique = integrate(dbeta, 
                                   shape1 = s1, shape2 = s2, 
                                   lower = realized, upper = 1)$value * 2) %>% 
  ungroup() %>% 
  select(sp, axis, realized, p_critique, conf_int_low, conf_int_high)

