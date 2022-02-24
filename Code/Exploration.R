# Package loading -------------------------------------------------------------------

Sys.setlocale("LC_ALL", "English")
require(tidyverse)
require(readxl)
require(lubridate)
require(sf)
require(leaflet)
require(htmlwidgets)
require(fuzzyjoin)

options(viewer = NULL) 

# Importation -----------------------------------------------------------------------

c("t_capture", "t_controle", "t_controle_coo", "t_reprise", "t_commune", "t_spot") %>% 
  set_names() %>% 
  map(~ read_excel("./Data/data_duck.xlsx",
           sheet = .x, 
           na = c("", "NA"), 
           guess_max = 40000)) -> l1

l2 <- list()


# Table cleaning  ------------------------------------------------------------------

# t_capture
l1 %>% 
  pluck(1) %>% 
  filter(ESPECE %in% c("AYTFER", "AYTFUL"), # only 2 species
         DEPARTEMENT %in% c("44", "53", "01", "77", "89", "41", "42"), # only main dpt
         !is.na(BAGUE)) %>%  # only banded birds
  mutate(across(HEURE, ~ if_else(is.na(HEURE), ymd_hms(20000101120000), HEURE)),
         datetime = str_c(date(DATE), "T", hour(HEURE), ":", minute(HEURE)) %>% 
           ymd_hm(tz = "Europe/Paris"),
         across(ACTION, ~ if_else(.x == "BAGUAGE", .x, "CONTROLE")),
         dir_lon = if_else(LONG_DEG %>% str_detect("-"), -1, 1),
         dir_lat = if_else(LAT_DEG %>% str_detect("-"), -1, 1),
         across(c(LONG_DEG, LONG_MIN, LONG_SEC, LAT_DEG, LAT_MIN, LAT_SEC), 
                ~ .x %>% 
                  str_replace(",|:|;", ".") %>%
                  as.numeric() %>% 
                  abs()),
         lon = dir_lon * (LONG_DEG + LONG_MIN / 60 + LONG_SEC / 60^2),
         lat = dir_lat * (LAT_DEG + LAT_MIN / 60 + LAT_SEC / 60^2), 
         raw_lon = str_c(dir_lon, LONG_DEG, LONG_MIN, LONG_SEC, sep = ":"),
         raw_lat = str_c(dir_lat, LAT_DEG, LAT_MIN, LAT_SEC, sep = ":"), 
         sex = case_when(
           SEXE %in% c("F", "Femelle") ~ "fem",
           SEXE == "M" ~ "mal", 
           is.na(SEXE) ~ NA_character_,
           TRUE ~ "ind"), 
         age = case_when(
           AGE %>% str_detect("\\?") ~ "ind",
           AGE %>% str_detect("\\+1") ~ "+1A",
           AGE %>% str_detect("\\+") ~ "+2A",
           AGE %>% str_starts("1|VOL") ~ "1A",
           AGE %>% str_starts("2") ~ "2A",
           AGE == "PUL" ~ "pul",
           is.na(AGE) ~ NA_character_,
           TRUE ~ "ind")) %>% 
  select(obs = ACTION, datetime, ring = BAGUE,
         sp = ESPECE, sex, age, 
         lon, lat, co = PAYS, dpt = DEPARTEMENT, com = COMMUNE, spot = LIEUDIT, 
         raw_lon, raw_lat, 
         `CODE MARQUE`, `DOUBLE MARQUAGE`, `COULEUR MARQUE`,
         MODE, CENTRE, BAGUEUR, `N°PROGRAMME`, `BAGUE RECOMPENSE`, FREQUENCE_EMETTEUR, 
         POIDS, PLUMAGE, `AILE G`, `AILE D`, `TARSE G`, `TARSE D`, 
         BEC, NARINE, BLEU_BEC_1, BLEU_BEC_2, BLEU_BEC_3, 
         PARASITES, `PRISE DE SANG`, `PRELEVEMENT PLUME`, 
         `PRELEVEMENT CLOACAL`, `BUT PREL CLOACAL`, `N°ECOUVILLON`, 
         DATE_IMPORT, NUMERO_IMPORT, REMARQUES) %>% 
  arrange(obs, datetime, sp, ring) -> l2[[1]] 

# t_controle
l1 %>% 
  pluck(2) %>% 
  filter(!is.na(BAGUE), 
         ESPECE %in% c("AYTFER", "AYTFUL")) %>% 
  mutate(across(HEURE, ~ if_else(is.na(HEURE), ymd_hms(20000101120000), HEURE)),
         datetime = str_c(date(DATE), "T", hour(HEURE), ":", minute(HEURE)) %>% 
           ymd_hm(tz = "Europe/Paris"),
         obs = "CONTROLE",
         dir_lon = if_else(LONG_DEG %>% str_detect("-"), -1, 1),
         dir_lat = if_else(LAT_DEG %>% str_detect("-"), -1, 1),
         across(c(LONG_DEG, LONG_MIN, LONG_SEC, LAT_DEG, LAT_MIN, LAT_SEC), 
                ~ .x %>% 
                  str_replace(",|:|;", ".") %>%
                  as.numeric() %>% 
                  abs()),
         lon = dir_lon * (LONG_DEG + LONG_MIN / 60 + LONG_SEC / 60^2),
         lat = dir_lat * (LAT_DEG + LAT_MIN / 60 + LAT_SEC / 60^2), 
         raw_lon = str_c(dir_lon, LONG_DEG, LONG_MIN, LONG_SEC, sep = ":"),
         raw_lat = str_c(dir_lat, LAT_DEG, LAT_MIN, LAT_SEC, sep = ":")) %>%
  select(obs, datetime, ring = BAGUE,
         sp = ESPECE, 
         lon, lat, co = PAYS, dpt = DEPARTEMENT, com = COMMUNE, spot = LIEUDIT, 
         raw_lon, raw_lat, 
         `CODE MARQUE`, `COULEUR MARQUE`, `PRESENCE MARQUE`, MARQUE,
         MODE, CENTRE, BAGUEUR, `N°PROGRAMME`, FREQUENCE_EMETTEUR, 
         DISTANCE, ANGLE,
         SEXE, AGE, POIDS, PLUMAGE, `AILE G`, `AILE D`, `TARSE G`, `TARSE D`, 
         BEC, BLEU_BEC_1, BLEU_BEC_2, BLEU_BEC_3, 
         PARASITES, `PRISE DE SANG`, `PRELEVEMENT PLUME`, 
         COMPORTEMENT_1, COMPORTEMENT_2, 
         DATE_IMPORT, NUMERO_IMPORT, REMARQUES) %>% 
  arrange(obs, datetime, sp, ring) -> l2[[2]] 

# t_controle_coo ~ 200 données perdues
l1 %>% 
  pluck(3) %>% 
  filter(ESPECE %in% c("AYTFER", "AYTFUL"), 
         !is.na(BAGUE)) %>% 
  mutate(across(HEURE, ~ if_else(is.na(HEURE), ymd_hms(20000101120000), HEURE)),
         datetime = str_c(date(DATE), "T", hour(HEURE), ":", minute(HEURE)) %>% 
           ymd_hm(tz = "Europe/Paris"),
         obs = "CONTROLE",
         dir_lon = if_else(LONG_DEG %>% str_detect("-"), -1, 1),
         dir_lat = if_else(LAT_DEG %>% str_detect("-"), -1, 1),
         across(c(LONG_DEG, LONG_MIN, LONG_SEC, LAT_DEG, LAT_MIN, LAT_SEC), 
                ~ .x %>% 
                  str_replace(",|:|;", ".") %>%
                  as.numeric() %>% 
                  abs())) %>% 
  filter(!(
    LONG_DEG < -90 | LONG_DEG > 90 | is.na(LONG_DEG) |
      LONG_MIN < 0 | LONG_MIN >= 60 | is.na(LONG_MIN) |
      LONG_SEC < 0 | LONG_SEC >= 60 | is.na(LONG_SEC) |
      LAT_DEG < -90 | LAT_DEG > 90 | is.na(LAT_DEG) |
      LAT_MIN < 0 | LAT_MIN >= 60 | is.na(LAT_MIN) |
      LAT_SEC < 0 | LAT_SEC >= 60 | is.na(LAT_SEC))) %>% 
  mutate(lon = dir_lon * (LONG_DEG + LONG_MIN / 60 + LONG_SEC / 60^2),
         lat = dir_lat * (LAT_DEG + LAT_MIN / 60 + LAT_SEC / 60^2), 
         raw_lon = str_c(dir_lon, LONG_DEG, LONG_MIN, LONG_SEC, sep = ":"),
         raw_lat = str_c(dir_lat, LAT_DEG, LAT_MIN, LAT_SEC, sep = ":")) %>%
  select(obs, datetime, ring = BAGUE,
         sp = ESPECE, 
         lon, lat,
         raw_lon, raw_lat, 
         `CODE MARQUE`, `COULEUR MARQUE`,
         `QUALITE LOCALISATION`, NUMERO_IMPORT, DATE_IMPORT, CORRECTION) %>% 
  arrange(obs, datetime, sp, ring) -> l2[[3]]

# t_reprise
l1 %>% 
  pluck(4) %>% 
  filter(ESPECE %in% c("AYTFER", "AYTFUL"), 
         !is.na(`N° BAGUE`)) %>% 
  mutate(datetime = str_c(date(DATE), "T12:00") %>% 
           ymd_hm(tz = "Europe/Paris"),
         dir_lon = if_else(LONG_DEG %>% str_detect("-"), -1, 1),
         dir_lat = if_else(LAT_DEG %>% str_detect("-"), -1, 1),
         across(c(LONG_DEG, LONG_MIN, LONG_SEC, LAT_DEG, LAT_MIN, LAT_SEC), 
                ~ .x %>% 
                  str_replace(",|:|;", ".") %>%
                  as.numeric() %>% 
                  abs()),
         lon = dir_lon * (LONG_DEG + LONG_MIN / 60 + LONG_SEC / 60^2),
         lat = dir_lat * (LAT_DEG + LAT_MIN / 60 + LAT_SEC / 60^2), 
         raw_lon = str_c(dir_lon, LONG_DEG, LONG_MIN, LONG_SEC, sep = ":"),
         raw_lat = str_c(dir_lat, LAT_DEG, LAT_MIN, LAT_SEC, sep = ":")) %>% 
  select(obs = ACTION, datetime, ring = `N° BAGUE`,
         sp = ESPECE, 
         lon, lat, co = PAYS, dpt = DEPARTEMENT, com = COMMUNE, spot = LIEUDIT,
         raw_lon, raw_lat, 
         `CODE MARQUE`, `COULEUR MARQUE`, `PRESENCE MARQUE NASALE`,
         MODE, CENTRE, OBSERVATEUR, 
         SEXE, AGE, POIDS, COND, REMARQUES) %>% 
    arrange(obs, datetime, sp, ring) -> l2[[4]]

# First filtering -------------------------------------------------------------------

l3 <- list()

# capture slmt : - 50 données
l2 %>% 
  pluck(1) %>% 
  filter(obs == "BAGUAGE") -> l3[[1]]

# on vire duplicata de baguage : - 2 données
l3 %>% 
  pluck(1) %>% 
  filter(!duplicated(.)) %>% 
  arrange(datetime, sp, ring) -> l3[[1]]

# on vire les autres duplicata de baguage
l3 %>% 
  pluck(1) %>% 
  add_count(ring) %>% 
  filter(n > 1) %>% 
  slice(c(1, 3, 5)) -> to_rm_1

anti_join(l3 %>% pluck(1), to_rm_1) -> l3[[1]]
  
# fusion des controles et restriction sur les capturés des sites choisis
l2 %>% 
  pluck(1) %>% 
  filter(obs == "CONTROLE") %>% 
  bind_rows(l2 %>% pluck(2)) %>% 
  filter(ring %in% (l3 %>% pluck(1) %>% pull(ring))) %>% 
  filter(!duplicated(.)) -> l3[[2]]

# couple bague marque de référence
l3 %>% 
  pluck(1) %>% 
  select(ring, `CODE MARQUE`) %>% 
  filter(!is.na(`CODE MARQUE`)) -> good_couple

# nbses marques correspondant à une mauvaise bague dans les contrôles.
l3 %>% 
  pluck(2) %>% 
  anti_join(good_couple) %>% 
  add_count(ring) %>% 
  select(datetime, ring, sp, spot, `CODE MARQUE`, n) %>% 
  left_join(good_couple, by = "ring") %>% 
  arrange(desc(n), ring, datetime)

# on garde seulement les NA, ou "non marque(e)"
l3 %>% 
  pluck(2) %>% 
  semi_join(good_couple) %>% 
  bind_rows(
    anti_join(l3 %>% pluck(2), good_couple) %>% 
      filter(is.na(`CODE MARQUE`) | str_detect(`CODE MARQUE`, "MARQUE"))) -> l3[[2]]

# coordonnées des controles
l2 %>% 
  pluck(3) %>% 
  count(datetime, ring) %>% 
  filter(n > 1)

l2 %>% 
  pluck(3) %>% 
  group_by(datetime, ring) %>% 
  summarize(across(c(lon, lat), mean),
            across(contains("raw"), ~ .x %>%  pluck(1))) %>% 
  ungroup() -> l3[[3]]

# reprise formatting
l2 %>% 
  pluck(4) %>% 
  filter(ring %in% (l3[[1]] %>% pull(ring))) -> l3[[4]]

# Fusion des controles avec les coordonnées -----------------------------------------

list() -> l4

l3[[1]] -> l4[[1]]

# data with coordinates in the second tibble
inner_join(l3[[2]] %>% select(-starts_with(c("lon", "lat", "raw"))), 
           l3[[3]]) -> l4[[2]]

# controle without coordinates
anti_join(l3[[2]], l3[[3]] %>% select(1:2)) %>% nrow()

anti_join(l3[[2]], l3[[3]] %>% select(1:2)) %>% 
  filter(!is.na(lon), !is.na(lat)) -> lon_lat; lon_lat

anti_join(l3[[2]], l3[[3]] %>% select(1:2)) %>% 
  filter(is.na(lon) | is.na(lat)) %>% 
  filter(!is.na(com), !is.na(spot)) -> to_be_found

bind_rows(l4[[2]], lon_lat, to_be_found) -> l4[[2]]

l3[[4]] -> l4[[3]]

# Data counts -----------------------------------------------------------------------

# oiseaux bagués
l4 %>% pluck(1) %>% nrow()

# oiseaux controlés
semi_join(l4 %>% pluck(1), l4 %>% pluck(2), by = "ring") %>% 
  nrow()

# oiseaux repris
semi_join(l4 %>% pluck(1), l4 %>% pluck(3), by = "ring") %>% 
  nrow()

# oiseaux controlés & repris
semi_join(l4 %>% pluck(1), l4 %>% pluck(2), by = "ring") %>% 
  semi_join(l4 %>% pluck(3), by = "ring") %>% nrow()

# oiseaux controlés &/ou repris
bind_rows(semi_join(l4 %>% pluck(1), l4 %>% pluck(2), by = "ring"), 
          semi_join(l4 %>% pluck(1), l4 %>% pluck(3), by = "ring")) %>% 
  distinct() %>% nrow()

# oiseaux non controlés & non repris
anti_join(l4 %>% pluck(1), l4 %>% pluck(2), by = "ring") %>% 
  anti_join(l4 %>% pluck(3), by = "ring") %>% nrow()

# Coordinates check by mapping ------------------------------------------------------

l4 %>% 
  pluck(2) %>% 
  filter(!is.na(lon), !is.na(lat)) %>% 
  distinct(lon, lat, com, spot) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  arrange(com, spot) -> posi

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

# ajout des données par groupes + les popups
map_base %>% 
    addCircleMarkers(
      data = posi,
      radius = ~ 15 * log10(1 + 1),
      fillColor = "#EE6352",
      fillOpacity = 0.7,
      stroke = TRUE,
      color = "#EE6352",
      opacity = 0.7,
      weight = 2,
      label = ~ com) %>%
  addLayersControl(position = "topleft",
                   baseGroups = c("Fond clair",
                                  "Fond noir", 
                                  "Fond satellite",
                                  "Fond topographie"),
                   overlayGroups = unique(posi$com),
                   options = layersControlOptions(collapsed = FALSE)) -> map_1; map_1



# withr::with_dir('Output', saveWidget(widget = map_1, 
#                                   file = "position_com.html",
#                                   selfcontained = TRUE))

rm(map_1)

# Coordinates checking by country ----------------------------------------------------

st_read("../../LIFE_Donnees/France_shp/countries") -> cos

cos %>% 
  st_drop_geometry() %>% 
  pull(name) %>% 
  str_to_upper() %>% 
  sort() -> cos2

l4 %>% 
  map(~ .x %>% 
        rowid_to_column() %>% 
        mutate(id = str_c(rowid, ".", obs), 
               co = co %>% 
                 iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>% # remove accent
                 str_to_upper(), 
               co = case_when(
                   co %>% str_detect("0000") | is.na(co) ~ "FRANCE", 
                   co %>% str_detect("BIELORUSSIA") ~ "BELARUS",
                   co %>% str_starts("BELG") ~ "BELGIUM",
                   co == "GRECE" ~ "GREECE",
                   co  %>% str_starts("NETHER") ~ "NETHERLANDS",
                   co  %>% str_starts("RUSSIA") ~ "RUSSIAN FEDERATION",
                   co %>% str_starts(c("SWI|SUI|SWT")) ~ "SWITZERLAND",
                   co %in% c("UK", "OK", "SCOTLAND") ~ "U.K. OF GREAT BRITAIN AND NORTHERN IRELAND",
                   co == "ITALIA" ~ "ITALY", 
                   TRUE ~ co))) -> l5

l5 %>%
  map(~ .x %>% 
        select(id, co, dpt, com, spot, lon, lat)) %>% 
  reduce(bind_rows) -> spots

# how many spots without coordinates
spots %>% 
  mutate(
  pos = if_else(is.na(lon) | is.na(lat), 0, 1)) %>% 
  count(co, com, spot, pos) %>% 
  group_by(co, com, spot) %>% 
  summarize(across(c(pos, n), sum)) %>% 
  filter(pos == 0) -> no_pos; no_pos

spots %>% 
  mutate(locali = str_c(com, spot)) %>% 
  filter(locali %in% c(no_pos %>% mutate(loc = str_c(com, spot)) %>% pull(loc))) %>% 
  pull(id) -> id_no_pos

# does it fall in the countries? 
spots %>% 
  filter(!is.na(lon), !is.na(lat), 
         lon >= -90, lon <= 90,
         lat >= -90, lat <= 90) %>% 
  left_join(cos %>% 
              mutate(co = name %>% str_to_upper()) %>% 
              select(co), 
            by = "co") -> spot_base

spot_base %>% 
  mutate(geom2 = spot_base %>% 
           select(lon, lat) %>% 
           st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
           pull(geometry)) -> spot2

# long calcul
spot2 %>% 
  mutate(dist = st_distance(geom2, geometry, by_element = TRUE)) -> spot3

spot3 %>% 
  select(id, co, com, spot, geom2, dist) %>% 
  mutate(across(dist, ~ .x %>% as.numeric() %>% "*"(10^-3) %>% round()),
         across(c(id, co, com, spot), str_replace_na),
         spot_full = str_c(id, co, com, spot, sep = " / ")) %>% 
  filter(dist > 0) %>% 
  arrange(co, com, spot, desc(dist)) -> spot4; spot4

spot4 %>% 
  mutate(id_co = co %>% as_factor() %>% as.numeric(),
         colo = hcl(h = seq(15, 300, length = max(id_co)), l = 65, c = 100)[id_co]) %>% 
  st_as_sf() -> spot5

map_base %>% 
  addCircleMarkers(
    data = spot5,
    group = ~ co,
    radius = ~ 15 * log10(1 + 1),
    fillColor = ~ colo,
    fillOpacity = 0.7,
    stroke = TRUE,
    color = "#696773",
    opacity = 0.7,
    weight = 2,
    label = ~ spot_full) %>%
  addLayersControl(position = "topleft",
                   baseGroups = c("Fond clair",
                                  "Fond noir", 
                                  "Fond satellite",
                                  "Fond topographie"),
                   overlayGroups = unique(spot4$co),
                   options = layersControlOptions(collapsed = FALSE)) %>% 
  addLegend(data = spot5 %>% 
              st_drop_geometry() %>% 
              distinct(co, colo),
            position = "topright",
            labels = ~ co,
            colors = ~ colo, 
            title = NULL, 
            opacity = 0.7) -> map_2; map_2

rm(map_2)

# Corrections : check dans le pays --------------------------------------------------

# Belgique
spot_base %>% 
  filter(id %in% c("11719.CONTROLE", "11720.CONTROLE")) %>% 
  mutate(lon = abs(lon)) -> modif

# Denmark -> ok

# Finland -> ok

# Netherlands
spot_base %>% 
  filter(id %in% c("192.REPRISE")) %>% 
  mutate(lat = lat - 1) %>% 
  bind_rows(modif) -> modif

spot_base %>% 
  filter(id %in% c("2920.CONTROLE")) %>% 
  mutate(co = "BELGIUM") %>% 
  bind_rows(modif) -> modif

# Portugal
spot_base %>% 
  filter(id %in% c("4122.CONTROLE")) %>% 
  mutate(co = "SPAIN") %>% 
  bind_rows(modif) -> modif

spot_base %>% 
  filter(id %in% c("6720.CONTROLE")) %>% 
  mutate(lon = lon - 5) %>% 
  bind_rows(modif) -> modif

# Russia -> ok

# Spain
spot_base %>% 
  filter(id %in% c("15248.CONTROLE")) %>% 
  mutate(lon = -lon) %>% 
  bind_rows(modif) -> modif

spot_base %>% 
  filter(id %in% c("13376.CONTROLE")) %>% 
  mutate(lon = -lon) %>% 
  bind_rows(modif) -> modif

spot_base %>% 
  filter(id %in% c("7576.CONTROLE")) %>% 
  mutate(lat = 43.537) %>% 
  bind_rows(modif) -> modif

spot_base %>% 
  filter(id %in% c("15192.CONTROLE", "15244.CONTROLE", "15263.CONTROLE", "15280.CONTROLE", 
                   "18368.CONTROLE", 
                   "10133.CONTROLE", "10153.CONTROLE", "10205.CONTROLE",
                   "12804.CONTROLE",
                   "9124.CONTROLE",
                   "15283.CONTROLE")) %>% 
  mutate(lon = -lon) %>% 
  bind_rows(modif) -> modif

# Sweden -> ok

# Switzerland
spot_base %>% 
  filter(id %in% c("19999.CONTROLE")) %>% 
  mutate(lat = lat + .2) %>% 
  bind_rows(modif) -> modif

spot_base %>% 
  filter(id %in% c("7939.CONTROLE", "10159.CONTROLE")) %>% 
  mutate(lat = lat - 1) %>% 
  bind_rows(modif) -> modif

# UK
spot_base %>% 
  filter(id %in% c("26238.CONTROLE")) %>% 
  mutate(lon = -lon) %>% 
  bind_rows(modif) -> modif

spot_base %>% 
  filter(id %in% c("500.REPRISE")) %>% 
  mutate(lat = lat - 8) %>% 
  bind_rows(modif) -> modif

spot_base %>% 
  filter(id %in% c("16708.CONTROLE")) %>% 
  mutate(lon = lon - 2) %>% 
  bind_rows(modif) -> modif

spot_base %>% 
  filter(id %in% c("23853.CONTROLE")) %>% 
  mutate(lon = -lon) %>% 
  bind_rows(modif) -> modif

spot_base %>% 
  filter(id %in% c("15337.CONTROLE",
                   "4605.CONTROLE")) %>% 
  mutate(lat = lat + 10) %>% 
  bind_rows(modif) -> modif

# France

spot_base %>% 
  filter(id %in% c("8879.CONTROLE", "8912.CONTROLE", "8939.CONTROLE", 
                   "25841.CONTROLE",
                   "10008.CONTROLE", "10663.CONTROLE",
                   "563.REPRISE",
                   "6628.CONTROLE")) %>% 
  mutate(co = "GERMANY") %>% 
  bind_rows(modif) -> modif

spot_base %>% 
  filter(id %in% c("12051.CONTROLE", "12092.CONTROLE", "12181.CONTROLE",
                   "12076.CONTROLE",
                   "8272.CONTROLE")) %>% 
  mutate(co = "POLAND") %>% 
  bind_rows(modif) -> modif

spot_base %>% 
  filter(id %in% c("7955.CONTROLE", "8075.CONTROLE",
                   "10782.CONTROLE",
                   "13323.CONTROLE",
                   "7366.CONTROLE", "7370.CONTROLE",
                   "15294.CONTROLE",
                   "9054.CONTROLE",
                   "25847.CONTROLE",
                   "25583.CONTROLE")) %>% 
  mutate(co = "U.K. OF GREAT BRITAIN AND NORTHERN IRELAND") %>% 
  bind_rows(modif) -> modif

spot_base %>% 
  filter(id %in% c("8944.CONTROLE", "8945.CONTROLE", 
                   "10145.CONTROLE", 
                   "23938.CONTROLE",
                   "8268.CONTROLE")) %>% 
  mutate(co = "BELGIUM") %>% 
  bind_rows(modif) -> modif

spot_base %>% 
  filter(id %in% c("6206.CONTROLE", "1008.CONTROLE", 
                   "10957.CONTROLE", "10962.CONTROLE", 
                   "6421.CONTROLE",
                   "12622.CONTROLE", "12621.CONTROLE",
                   "10966.CONTROLE")) %>% 
  mutate(co = "SWITZERLAND") %>% 
  bind_rows(modif) -> modif

spot_base %>% 
  filter(id %in% c("23928.CONTROLE", 
                   "11969.CONTROLE",
                   "27113.CONTROLE",
                   "6251.CONTROLE", 
                   "9277.CONTROLE")) %>% 
  mutate(co = "SPAIN") %>% 
  bind_rows(modif) -> modif

spot_base %>% 
  filter(id %in% c("568.REPRISE", 
                   "145.REPRISE",
                   "894.REPRISE",
                   "751.REPRISE", "752.REPRISE", 
                   "796.REPRISE",  
                   "794.REPRISE")) %>% 
  mutate(co = "RUSSIAN FEDERATION") %>% 
  bind_rows(modif) -> modif

spot_base %>% 
  filter(id %in% c("10627.CONTROLE", 
                   "4681.CONTROLE",
                   "5688.CONTROLE",
                   "737.REPRISE")) %>% 
  mutate(co = "NETHERLANDS") %>% 
  bind_rows(modif) -> modif

spot_base %>% 
  filter(id %in% c("652.REPRISE")) %>% 
  mutate(co = "ITALY") %>% 
  bind_rows(modif) -> modif

#French commune to check
spot5 %>% 
  filter(co == "FRANCE", !id %in% modif$id) %>% 
  mutate(id_co = com %>% as_factor() %>% as.numeric(),
         colo = hcl(h = seq(15, 300, length = max(id_co)), 
                    l = 65, 
                    c = 100)[id_co]) -> commune

map_base %>% 
  addCircleMarkers(
    data = commune,
    group = ~ com,
    radius = ~ 15 * log10(1 + 1),
    fillColor = ~ colo,
    fillOpacity = 0.7,
    stroke = TRUE,
    color = "#696773",
    opacity = 0.7,
    weight = 2,
    label = ~ spot_full) %>%
  addLayersControl(position = "topleft",
                   baseGroups = c("Fond clair",
                                  "Fond noir", 
                                  "Fond satellite",
                                  "Fond topographie"),
                   overlayGroups = unique(commune$com),
                   options = layersControlOptions(collapsed = FALSE)) -> map_3; map_3

rm(map_3)

# Balloy
spot_base %>% 
  filter(id %in% c("403.BAGUAGE")) %>% 
  mutate(lat = lat + 20) %>% 
  bind_rows(modif) -> modif

# Barbatre ok

# Bistroff
spot_base %>% 
  filter(id %in% c("18201.CONTROLE")) %>% 
  mutate(lat = lat + 2) %>% 
  bind_rows(modif) -> modif

# Calais
spot_base %>% 
  filter(id %in% c("944.REPRISE")) %>% 
  mutate(lon = -lon) %>% 
  bind_rows(modif) -> modif

# Cappy
spot_base %>% 
  filter(id %in% c("829.REPRISE")) %>% 
  mutate(lat = lat + 30) %>% 
  bind_rows(modif) -> modif

# Cappy
spot_base %>% 
  filter(id %in% c("987.REPRISE")) %>% 
  rename(lon = lat, 
         lat = lon) %>%  
  bind_rows(modif) -> modif

# Etaples
spot_base %>% 
  filter(id %in% c("11990.CONTROLE")) %>% 
  mutate(lon = -lon) %>%  
  bind_rows(modif) -> modif

# Frossay ok

# GONDREVILLE-L'ORCHER
spot_base %>% 
  filter(id %in% c("4661.CONTROLE", "4665.CONTROLE")) %>% 
  mutate(lat = lat - .4) %>%  
  bind_rows(modif) -> modif

# Grand Lavier
spot_base %>% 
  filter(id %in% c("26225.CONTROLE")) %>% 
  mutate(lon = -lon) %>%  
  bind_rows(modif) -> modif

# Havre ok

# ile d'olonne
spot_base %>% 
  filter(id %in% c("16852.CONTROLE", "16969.CONTROLE")) %>% 
  mutate(lat = lat + 1) %>%  
  bind_rows(modif) -> modif

# jard sur mer ok

# la benne ok

# lundon medoc ok

# macau ok

# marsilly ok

# meazangers
spot_base %>% 
  filter(id %in% c("5506.BAGUAGE")) %>% 
  rename(lon = lat, 
         lat = lon) %>% 
  mutate(lat = -lat) %>% 
  bind_rows(modif) -> modif

# MORTAGNE-SUR-GIRONDE ok

# Noirmoutier ok

# Saint-andre de seigneux 
spot_base %>% 
  filter(id %in% c("6335.CONTROLE")) %>% 
  mutate(lat = lat + 1) %>% 
  bind_rows(modif) -> modif

# SAINT AYBERT ok

# Saint georges sur loire
spot_base %>% 
  filter(id %in% c("6623.CONTROLE", "8557.CONTROLE")) %>% 
  mutate(lon = -0.703333) %>% 
  bind_rows(modif) -> modif

# Saint jean de thurigneux
spot_base %>% 
  filter(id %in% c("13444.CONTROLE")) %>% 
  mutate(lat = lat + 30) %>% 
  bind_rows(modif) -> modif

spot_base %>% 
  filter(id %in% c("8905.CONTROLE")) %>% 
  mutate(lon = lon + 41) %>%
  rename(lon = lat, lat = lon) %>% 
  bind_rows(modif) -> modif
  
# saint philbert
spot_base %>% 
  filter(id %in% (commune %>% filter(com %>% str_detect("PHILBERT")) %>% pull(id))) %>% 
  mutate(lon = -1.65611111111111,
         lat = 47.0841666666667) %>% 
  bind_rows(modif) -> modif

# SAINT-ROMAIN-SUR-GIRONDE
spot_base %>% 
  filter(id == "593.REPRISE") %>% 
  rename(lon = lat,
         lat = lon) %>% 
  bind_rows(modif) -> modif

# SAINTES-MARIES-DE-LA-MER
spot_base %>% 
  filter(id == "628.REPRISE") %>% 
  mutate(lat = lat + 0.05) %>% 
  bind_rows(modif) -> modif

# SANDOUVILLE ok

# TRANCHE SUR MER ok

# TREGUENNEC
spot_base %>% 
  filter(id == "10377.CONTROLE") %>% 
  mutate(lon = -4.33502778) %>%
  bind_rows(modif) -> modif

# VAL-DE-REUIL
spot_base %>% 
  filter(id == "5615.CONTROLE") %>% 
  mutate(lat = lat + 40) %>%
bind_rows(modif) -> modif

# VILLARS-LES-DOMBES
spot_base %>% 
  filter(id == "7768.CONTROLE") %>% 
  mutate(lat = lat - 4) %>%
  bind_rows(modif) -> modif

# VILLENEUVE-LA-GUYARD
spot_base %>% 
  filter(id == "412.REPRISE") %>% 
  mutate(co = "U.K. OF GREAT BRITAIN AND NORTHERN IRELAND",
         dpt = NA_character_,
         com = "ASHBOURNE",
         spot = "CARSINGTON WATER") %>%
  bind_rows(modif) -> modif

anti_join(spot_base, modif, by = "id") %>% 
  bind_rows(modif) %>% 
  select(-geometry) %>% 
  mutate(across(c(co, com, spot), 
                ~ .x %>% 
                  iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>% # remove accent
                  str_to_upper())) -> spot_base1

spots %>% 
  anti_join(spot_base1 %>% select(id)) %>% 
  mutate(across(c(lon, lat), ~ NA_real_)) %>%  
  bind_rows(spot_base1) -> spot_base2

# Corrections par nom de commune / spot -----------------------------------------------

l1 %>% 
  pluck(6) %>% 
  select(co = PAYS, dpt = DEPARTEMENT, com = COMMUNE, spot = `LIEU-DIT`, 6:11) %>% 
  mutate(dir_lon = if_else(LONG_DEG %>% str_detect("-"), -1, 1),
         dir_lat = if_else(LAT_DEG %>% str_detect("-"), -1, 1),
         across(c(LONG_DEG, LONG_MIN, LONG_SEC, LAT_DEG, LAT_MIN, LAT_SEC), 
                ~ .x %>%
                  as.numeric() %>% 
                  abs()),
         lon_2 = dir_lon * (LONG_DEG + LONG_MIN / 60 + LONG_SEC / 60^2),
         lat_2 = dir_lat * (LAT_DEG + LAT_MIN / 60 + LAT_SEC / 60^2)) %>% 
  select(1:4, 13:14) %>%
  filter(!(is.na(lon_2) | is.na(lat_2))) %>% 
  mutate(across(c(co, com, spot), 
                ~ .x %>% 
                  iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>% # remove accent
                  str_to_upper()), 
         co = case_when(
           co == "CHOLET" ~ "FRANCE", 
           co == "UK" ~ "U.K. OF GREAT BRITAIN AND NORTHERN IRELAND", 
           TRUE ~ co)) %>% 
  group_by(co, com, spot) %>% 
  summarize(across(c(lon_2, lat_2), mean)) %>% 
  ungroup() %>% 
  arrange(co, com, spot) %>% 
  rowid_to_column("id_spo") -> of_spot

l1 %>% 
  pluck(5) %>% 
  select(co = `Code pays`, com = Nom, dpt = `Dept français`, starts_with(c("Long", "Lat"))) %>% 
  mutate(dir_lon = if_else(Long_de %>% str_detect("-"), -1, 1),
         dir_lat = if_else(Lat_de %>% str_detect("-"), -1, 1),
         across(c(Long_de, Long_min, Long_sec, Lat_de, Lat_min, Lat_sec), 
                ~ .x %>%
                  as.numeric() %>% 
                  abs()),
         lon_2 = dir_lon * (Long_de + Long_min / 60 + 
                            if_else(is.na(Long_sec), 0, Long_sec) / 60^2),
         lat_2 = dir_lat * (Lat_de + Lat_min / 60 + 
                            if_else(is.na(Lat_sec), 0, Lat_sec) / 60^2)) %>% 
  select(co, com, dpt, lon_2, lat_2) %>% 
  filter(!(is.na(lon_2) | is.na(lat_2))) %>% 
  mutate(across(c(co, com), 
                ~ .x %>% 
                  iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>% # remove accent
                  str_to_upper()), 
         co = case_when(
           co == "AU" ~ "AUSTRIA", 
           co == "BL" ~ "BELGIUM",
           co == "BY" ~ "BELARUS", 
           co == "DE" ~ "GERMANY", 
           co == "DK" ~ "DENMARK", 
           co == "ER" ~ "U.K. OF GREAT BRITAIN AND NORTHERN IRELAND", 
           co == "ES" ~ "SPAIN", 
           co == "ET" ~ "ESTONIA", 
           co == "FR" ~ "FRANCE", 
           co == "GB" ~ "U.K. OF GREAT BRITAIN AND NORTHERN IRELAND", 
           co == "HE" ~ "SWITZERLAND", 
           co == "HG" ~ "HUNGARY", 
           co == "IA" ~ "ITALY", 
           co == "LI" ~ "LITHUANIA", 
           co == "LV" ~ "LATVIA", 
           co == "NL" ~ "NETHERLANDS", 
           co == "NO" ~ "NORWAY",
           co == "NV" ~ "NETHERLANDS",
           co == "PL" ~ "POLAND",
           co == "PO" ~ "PORTUGAL",
           co == "RU" ~ "RUSSIAN FEDERATION",
           co == "SF" ~ "FINLAND",
           co == "SK" ~ "SLOVAKIA",
           co == "SV" ~ "SLOVENIA",
           co == "TU" ~ "TURKEY",
           co == "UK" ~ "UKRAINE", 
           TRUE ~ co)) %>% 
  arrange(co, com, dpt) %>% 
  filter(co %in% unique(spot_base2$co)) %>% 
  rowid_to_column("id_co") -> of_com

# match exact par spot
spot_base2 %>% 
  filter(!is.na(co), 
         !is.na(com), 
         !is.na(spot)) %>% 
  inner_join(of_spot %>% 
               filter(!is.na(spot))) %>% 
  select(-id_spo) -> match_spot1

# fuzzy match exact par spot
spot_base2 %>% 
  filter(!is.na(co), 
         !is.na(com), 
         !is.na(spot), 
         !id %in% match_spot1$id) %>% 
  stringdist_inner_join(of_spot %>% 
                         filter(!is.na(spot)),
                       method = "jw",
                       max_dist = .1, # 10% max de diff
                       distance_col = "d") -> fuzzy_spot1

fuzzy_spot1 %>% 
  group_by(id) %>% 
  nest() %>% 
  mutate(data = 
           data %>% 
           map(~ .x %>% 
                 mutate(d = co.d + com.d + spot.d) %>% 
                 arrange(d) %>% 
                 slice(1))) %>% 
  unnest(data) %>% 
  ungroup() %>% 
  select(id, id_spo) %>% 
  left_join(spot_base2) %>% 
  left_join(of_spot %>% 
              rename(co_2 = co, com_2 = com, spot_2 = spot), 
            by = "id_spo") %>% 
  select(-id_spo) -> fuzzy_spot2

# match exact par commune
spot_base2 %>% 
  filter(!is.na(co), 
         !is.na(com),
         !is.na(dpt),
         !id %in% c(match_spot1$id, fuzzy_spot2$id)) %>% 
  inner_join(of_com %>%
               filter(co == "FRANCE")) -> match_com1

spot_base2 %>% 
  filter(!is.na(co), 
         !is.na(com),
         !id %in% c(match_spot1$id, fuzzy_spot2$id)) %>% 
  inner_join(of_com %>%
               filter(co != "FRANCE") %>% 
               select(-dpt)) %>% 
  bind_rows(match_com1) %>% 
  select(-id_co) -> match_com2

# fuzzy match par commune
spot_base2 %>% 
  filter(!is.na(co), 
         !is.na(com),
         !is.na(dpt),
         !id %in% c(match_spot1$id, fuzzy_spot2$id, match_com2$id)) %>% 
  stringdist_inner_join(of_com %>%
                          filter(co == "FRANCE"),
                        method = "jw",
                        max_dist = .1, # 10% max de diff
                        distance_col = "d") -> fuzzy_com1

spot_base2 %>% 
  filter(!is.na(co), 
         !is.na(com), 
         !id %in% c(match_spot1$id, fuzzy_spot2$id, match_com2$id)) %>% 
  stringdist_inner_join(of_com %>%
                          filter(co != "FRANCE") %>% 
                          select(-dpt),
                        method = "jw",
                        max_dist = .1, # 10% max de diff
                        distance_col = "d") %>% 
  bind_rows(fuzzy_com1) -> fuzzy_com2

fuzzy_com2 %>% 
  group_by(id) %>% 
  nest() %>% 
  mutate(data = 
           data %>% 
           map(~ .x %>% 
                 mutate(d = co.d + com.d) %>% 
                 arrange(d) %>% 
                 slice(1))) %>% 
  unnest(data) %>% 
  ungroup() %>% 
  select(id, id_co) %>% 
  left_join(spot_base2) %>% 
  left_join(of_com %>% 
              rename(co_2 = co, com_2 = com) %>% 
              select(-dpt), 
            by = "id_co") %>% 
  select(-id_co) -> fuzzy_com3

list(match_spot1 %>% mutate(match = "spot"), 
     fuzzy_spot2 %>% mutate(match = "spot"), 
     match_com2 %>% mutate(match = "com"), 
     fuzzy_com3 %>% mutate(match = "com")) %>% 
  reduce(bind_rows) -> spot_match

anti_join(spot_base2, spot_match) %>% 
  filter(if_any(c(lon, lat), is.na)) %>% 
  select(-lon, -lat) %>% 
  nest_by(co, com, spot) %>% 
  rowid_to_column("id_rep") -> to_find

rbind( 
  c(52.34632019544103, 4.527200043337543),
  c(47.35971150199786, -0.13032148874992358),
  c(47.35229176310093, 2.5861804820696053),
  c(45.786541046002526, 4.987740502521813),
  c(45.92648376559903, 4.954877448370128),
  c(53.46846872109407, 12.714431366935548),
  c(51.01943241756598, 12.479192777961744),
  c(43.541235726085176, -5.875858501571685),
  c(43.408018456134926, -3.8250945105443503),
  c(41.85415860485217, -5.593267966755118),
  c(46.82387183434558, 6.778031064574606),
  c(46.513984910402684, 6.504950776999205),
  c(51.64524075518134, -1.9077289598904172),
  c(52.42353791220175, -1.6833674228793085),
  c(52.68942527973278, -2.2007056411264863),
  c(51.24970845538929, -0.14996957217968374)) %>% 
  as_tibble(rownames = NULL) %>% 
  set_names(c("lat", "lon")) %>% 
  mutate(id_rep = to_find$id_rep) %>% 
  full_join(to_find) %>% 
  unnest(data) %>% 
  mutate(co = if_else(id_rep == 1, "NETHERLANDS", co)) %>% 
  select(-id_rep) -> found

spot_base2 %>% 
  filter(!id %in% c(spot_match$id, found$id)) %>% 
  bind_rows(spot_match) %>% 
  bind_rows(found %>% mutate(match = "search")) %>% 
  mutate(match = if_else(is.na(match), "-", match)) -> spot_base3

# analyse 
spot_base3 %>% 
  filter(if_all(c(lon, lat, lon_2, lat_2), ~ !is.na(.x))) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  mutate(geom = spot_base3 %>% 
           filter(if_all(c(lon, lat, lon_2, lat_2), ~ !is.na(.x))) %>% 
           st_as_sf(coords = c("lon_2", "lat_2"), crs = 4326) %>% 
           pull(geometry),
         dist = st_distance(geometry, 
                            geom, by_element = TRUE) * 10^-3, 
         dist = as.numeric(dist)) -> check_dist

check_dist %>% 
  filter(dist > 15) %>% 
  ggplot(aes(x = dist)) + 
  geom_histogram()

# on garde les coordonnées du spot qd distance > 15 km, ~ 1%
# on garde aussi les non matchés, ~ 5.5%

spot_base3 %>% 
  left_join(check_dist %>% select(id, dist) %>% st_drop_geometry()) %>% 
  mutate(lon = if_else(dist > 15 & !is.na(dist), lon_2, lon),
         lat = if_else(dist > 15 & !is.na(dist), lat_2, lat), 
         co = if_else(!is.na(com_2), co_2, co),
         com = if_else(!is.na(com_2), com_2, com),
         com = if_else(!is.na(spot_2), spot_2, spot)) %>% 
  select(id, lon, lat, co, dpt, com, spot) -> spot_base4

filtration

# Cohérence de la timeline ----------------------------------------------------------

l5 %>% 
  map(
    ~ .x %>%
      select(id, obs, datetime, ring, wgh = POIDS) %>% 
      mutate(wgh = as.numeric(wgh))) %>% 
  reduce(bind_rows) -> l6

l6 %>%  
  arrange(ring, datetime) %>% 
  nest_by(ring) %>% 
  mutate(data = data %>% 
           mutate(time = if_else(obs == "BAGUAGE", 1, 0) %>% cumsum()) %>% 
           filter(time > 0) %>% 
           mutate(time = if_else(obs == "REPRISE", 1, 0) %>% cumsum(), 
                  time = if_else(obs == "REPRISE", 0, time)) %>% 
           filter(time == 0) %>% 
           list()) %>% 
  unnest(data) %>% 
  filter(time < 1)
  mutate(nobs = nrow(data),
         reprise = if_else(any(data$obs == "REPRISE"), TRUE, FALSE)) %>% 
  

