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
         DATE_IMPORT, NUMERO_IMPORT, `REMARQUES`) %>% 
  arrange(obs, datetime, sp, ring) %>% 
  rowid_to_column() -> l2[[1]] 

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
  select(obs = ACTION, datetime, ring = BAGUE,
         sp = ESPECE, SEXE, AGE, 
         lon, lat, co = PAYS, dpt = DEPARTEMENT, com = COMMUNE, spot = LIEUDIT, 
         raw_lon, raw_lat, 
         `CODE MARQUE`, `DOUBLE MARQUAGE`, `COULEUR MARQUE`,
         MODE, CENTRE, BAGUEUR, `N°PROGRAMME`, `BAGUE RECOMPENSE`, FREQUENCE_EMETTEUR, 
         POIDS, PLUMAGE, `AILE G`, `AILE D`, `TARSE G`, `TARSE D`, 
         BEC, NARINE, BLEU_BEC_1, BLEU_BEC_2, BLEU_BEC_3, 
         PARASITES, `PRISE DE SANG`, `PRELEVEMENT PLUME`, 
         `PRELEVEMENT CLOACAL`, `BUT PREL CLOACAL`, `N°ECOUVILLON`, 
         DATE_IMPORT, NUMERO_IMPORT, `REMARQUES`) %>% 
  arrange(obs, datetime, sp, ring) -> l2[[2]] 
