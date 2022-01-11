# Package loading -------------------------------------------------------------------

Sys.setlocale("LC_ALL", "English")
require(tidyverse)
require(readxl)
require(lubridate)
require(sf)


# Importation -----------------------------------------------------------------------

c("t_capture", "t_controle", "t_controle_coo", "t_reprise") %>% 
  set_names() %>% 
  map(~ read_excel("./Data/data_duck.xlsx",
           sheet = .x, 
           na = c("", "NA"), 
           guess_max = 40000)) -> l1

l2 <- list()


# Nettoyage par table ---------------------------------------------------------------

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
         across(c(LONG_DEG, LONG_MIN, LONG_SEC, LAT_DEG, LAT_MIN, LAT_SEC), 
                ~ .x %>% 
                  str_replace(",|:|;", ".") %>%
                  as.numeric()),
         lon = if_else(LONG_DEG < 0, 
                        LONG_DEG - LONG_MIN / 60 - LONG_SEC / 60^2, 
                        LONG_DEG + LONG_MIN / 60 + LONG_SEC / 60^2),
         lat = if_else(LAT_DEG < 0, 
                        LAT_DEG - LAT_MIN / 60 - LAT_SEC / 60^2, 
                        LAT_DEG + LAT_MIN / 60 + LAT_SEC / 60^2), 
         raw_lon = str_c(LONG_DEG, LONG_MIN, LONG_SEC, sep = ":"),
         raw_lat = str_c(LAT_DEG, LAT_MIN, LAT_SEC, sep = ":"), 
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
         across(c(LONG_DEG, LONG_MIN, LONG_SEC, LAT_DEG, LAT_MIN, LAT_SEC), 
                ~ .x %>% 
                  str_replace(",|:|;", ".") %>%
                  as.numeric()),
         lon = if_else(LONG_DEG < 0, 
                       LONG_DEG - LONG_MIN / 60 - LONG_SEC / 60^2, 
                       LONG_DEG + LONG_MIN / 60 + LONG_SEC / 60^2),
         lat = if_else(LAT_DEG < 0, 
                       LAT_DEG - LAT_MIN / 60 - LAT_SEC / 60^2, 
                       LAT_DEG + LAT_MIN / 60 + LAT_SEC / 60^2), 
         raw_lon = str_c(LONG_DEG, LONG_MIN, LONG_SEC, sep = ":"),
         raw_lat = str_c(LAT_DEG, LAT_MIN, LAT_SEC, sep = ":")) %>%
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
         across(c(LONG_DEG, LONG_MIN, LONG_SEC, LAT_DEG, LAT_MIN, LAT_SEC), 
                ~ .x %>% 
                  str_replace(",|:|;", ".") %>%
                  as.numeric())) %>% 
  filter(!(
    LONG_DEG < -90 | LONG_DEG > 90 | is.na(LONG_DEG) |
      LONG_MIN < 0 | LONG_MIN >= 60 | is.na(LONG_MIN) |
      LONG_SEC < 0 | LONG_SEC >= 60 | is.na(LONG_SEC) |
      LAT_DEG < -90 | LAT_DEG > 90 | is.na(LAT_DEG) |
      LAT_MIN < 0 | LAT_MIN >= 60 | is.na(LAT_MIN) |
      LAT_SEC < 0 | LAT_SEC >= 60 | is.na(LAT_SEC))) %>% 
  mutate(lon = if_else(LONG_DEG < 0, 
                       LONG_DEG - LONG_MIN / 60 - LONG_SEC / 60^2, 
                       LONG_DEG + LONG_MIN / 60 + LONG_SEC / 60^2),
         lat = if_else(LAT_DEG < 0, 
                       LAT_DEG - LAT_MIN / 60 - LAT_SEC / 60^2, 
                       LAT_DEG + LAT_MIN / 60 + LAT_SEC / 60^2), 
         raw_lon = str_c(LONG_DEG, LONG_MIN, LONG_SEC, sep = ":"),
         raw_lat = str_c(LAT_DEG, LAT_MIN, LAT_SEC, sep = ":")) %>%
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
         across(c(LONG_DEG, LONG_MIN, LONG_SEC, LAT_DEG, LAT_MIN, LAT_SEC), 
                ~ .x %>% 
                  str_replace(",|:|;", ".") %>%
                  as.numeric())) %>% 
  mutate(lon = if_else(LONG_DEG < 0, 
                       LONG_DEG - LONG_MIN / 60 - LONG_SEC / 60^2, 
                       LONG_DEG + LONG_MIN / 60 + LONG_SEC / 60^2),
         lat = if_else(LAT_DEG < 0, 
                       LAT_DEG - LAT_MIN / 60 - LAT_SEC / 60^2, 
                       LAT_DEG + LAT_MIN / 60 + LAT_SEC / 60^2), 
         raw_lon = str_c(LONG_DEG, LONG_MIN, LONG_SEC, sep = ":"),
         raw_lat = str_c(LAT_DEG, LAT_MIN, LAT_SEC, sep = ":")) %>% 
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

l4
