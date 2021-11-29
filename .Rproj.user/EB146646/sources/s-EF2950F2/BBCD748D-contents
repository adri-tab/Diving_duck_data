# Package loading -------------------------------------------------------------------

Sys.setlocale("LC_ALL", "English")
require(tidyverse)
require(readxl)
require(lubridate)

# UK data import --------------------------------------------------------------------

# sample data
read_excel("./Data/UK/cull.xlsx",
           na = c("", "NA"), 
           guess_max = 30000) %>% 
  mutate(date = date(date)) -> UK_kill

# count data
read_excel("./Data/UK/count.xlsx", 
           na = c("", "NA"), 
           guess_max = 30000) %>%
  mutate(date = date(date)) %>%
  select(-age_sex, -...1) %>% 
  arrange(date, site_code) -> UK_nb

# UK count data: pop size ------------------------------------------------------------

UK_nb %>% 
  group_by(site_code, year = year(date), month = month(date)) %>% 
  summarize(obs = max(obs)) %>% 
  group_by(year, month) %>% 
  summarize(obs = sum(obs)) %>% 
  mutate(date = ymd(str_c(year, month, 15, sep = "-")),
         season = case_when(month %in% c(12, 1, 2) ~ "winter",
                            month %in% 3:5 ~ "spring",
                            month %in% 6:8 ~ "summer",
                            TRUE ~ "fall")) %>% 
  ggplot(aes(x = date, y = obs, group = season, color = as_factor(season))) +
  geom_point() +
  geom_smooth(method = "gam") +
  scale_x_date(date_labels = "%m-%Y")

# -> detection in winter > fall > spring > summer
# -> one keeps the winter time series to monitor the pop size

# monthly distribution of the count time series
UK_nb %>% 
  mutate(mois = month(date)) %>%
  group_by(source) %>% 
  count(mois, name = "count") %>% 
  ggplot(aes(x = mois, y = count, fill = source)) +
  geom_col() + 
  facet_wrap(~ source, ncol = 1) +
  theme(legend.position = "none")

# monthly distribution of the counted sites
UK_nb %>% 
  filter(month(date) %in% c(12, 1)) %>% 
  group_by(source) %>% 
  count(date, name = "site_count") %>% 
  ggplot(aes(x = date, y = site_count, color = source)) + 
  geom_point() + 
  facet_wrap(~ source, ncol = 1) +
  theme(legend.position = "none")

# evolution of the apparent male proportion
UK_nb %>% 
  filter(!is.na(obs_type_fem) & !is.na(obs_type_mal)) %>%
  mutate(year = if_else(month(date) == 1, year(date), year(date) + 1) %>% 
           str_c("0101") %>% ymd()) %>% 
  group_by(year, date) %>% 
  summarize(across(starts_with("obs"), sum)) %>% 
  ungroup() %>% 
  mutate(obs = obs_type_mal + obs_type_fem,
         prop = obs_type_mal / obs) %>% 
  ggplot(aes(x = year, y = prop, size = obs)) +  
  geom_smooth() +
  geom_point() +
  scale_size(trans = "log10")

# one pools data from a same month and year, and keeps the biggest count
# No 0 in the monthly protocol, so impossible to interpolate over the sites.
UK_nb %>% 
  filter(month(date) %in% c(12, 1), obs != 0) %>%
  mutate(date_av = ymd(str_c(year(date), month(date), 15, sep = "-"))) %>% 
  group_by(site_code, date_av) %>% 
  nest() %>% 
  mutate(data = data %>% map( ~ .x %>% arrange(desc(obs)) %>% slice(1))) %>% 
  unnest(data) -> UK_nb_2 

UK_nb_2 %>%
  group_by(date_av) %>% 
  add_count(name = "site") %>% 
  mutate(count = sum(obs)) %>%
  rowwise() %>% 
  mutate(prop = obs / count) %>% 
  group_by(date = date_av) %>% 
  filter(prop == max(prop)) %>% 
  ungroup() %>% 
  select(date, count, site, source) %>% 
  distinct() -> UK_nb_3

# count selection for pop size time series
UK_nb_3 %>% 
  pivot_longer(cols = c(count, site), names_to = "var", values_to = "count") %>% 
  ggplot(aes(x = date, y = count, shape = var, color = month(date, label = TRUE))) + 
  geom_point() +
  scale_y_log10()
# -> no time series better than the other, so one keeps the max counts

# final time series
UK_nb_3 %>% 
  mutate(year = if_else(month(date) == 12, date + days(17), date - days(14)),
         source = str_c(month(date, label = TRUE, abbr = TRUE), 
                        source %>% str_split(" ") %>% map_chr(last), sep = " - ")) %>% 
  group_by(year) %>% 
  filter(count == max(count)) %>% 
  ungroup() %>% 
  distinct() %>% 
  mutate(end = c(.$date[-1], last(.$date) + years(1)) - days(1)) %>% 
  select(year, start = date, end, count, site, source) -> UK_nb_4

UK_nb_4 %>% 
  ggplot(aes(x = year, y = count)) +
  geom_line(linetype = "dashed") +
  geom_point(aes(color = source))

# UK count data: apparent winter sex counts -----------------------------------------

# male proportion (only based on winter counts)
UK_nb %>% 
  filter(!is.na(obs_type_fem) & !is.na(obs_type_mal)) %>%
  mutate(year = if_else(month(date) == 1, year(date), year(date) + 1) %>% 
           str_c("0101") %>% ymd()) %>% 
  group_by(year, date) %>% 
  summarize(across(starts_with("obs"), sum)) %>% 
  ungroup() %>% 
  select(-obs) -> UK_sex_app

UK_nb_4 %>% 
  select(year) %>% 
  full_join(UK_sex_app) %>% 
  mutate(across(starts_with("obs"), ~ .x %>% replace_na(0))) %>% 
  group_by(year) %>% 
  nest() %>% 
  ungroup() %>% 
  rename(sex_app = data) -> UK_sex_app_2

# UK sample data: age evolution analyses ---------------------------------------------

# age of the samples over the year 
UK_kill %>% 
  mutate(age = age_sex %>% str_extract(regex(".*(?=_)"))) %>% 
  group_by(yday = yday(date),
           year = year(date), 
           age) %>% 
  summarize(across(shot, sum)) %>% 
  ungroup() -> UK_age

# sampling intensity over the year
UK_age %>% 
  group_by(yday, age) %>% 
  summarize(across(shot, sum)) %>% 
  ggplot(aes(x = yday, y = shot, fill = age)) +
  geom_col(position = "dodge")
# high sampling pressure in winter

# age proportion in the samples 
UK_age %>% 
  group_by(yday, age) %>% 
  summarize(across(shot, sum)) %>% 
  group_by(yday) %>% 
  nest() %>% 
  mutate(tot = data %>% map_dbl(~ .x %>% pull(shot) %>% sum())) %>% 
  unnest(data) %>% 
  ungroup() %>% 
  mutate(prop = shot / tot) %>% 
  ggplot(aes(x = yday, y = prop, color = age)) +
  geom_point() +
  geom_smooth()
# chicks appears from the 150th day (1st of June)
# Juvenile character disappears by the end of the winter
# best picture of the recruitment in winter because there are many samples and 
# it is the last moment with the juvenile character

# Adult proportion in the removals over the year
UK_kill %>% 
  mutate(age = age_sex %>% str_extract(regex(".*(?=_)"))) %>% 
  filter(age != "ind") %>% 
  group_by(age, month = month(date, label = TRUE)) %>% 
  summarize(across(shot, sum)) %>% 
  group_by(month) %>% 
  mutate(ratio = shot / sum(shot))  %>% 
  filter(age == "ad") %>% 
  ggplot(aes(x = month, y = ratio)) +
  geom_col()
# -> switch by the end of March, ok to consider the winter as a good picture of the 
# juvenile proportion

# age proportion by year
UK_age %>% 
  group_by(yday) %>% 
  nest() %>% 
  mutate(tot = data %>% 
           map_dbl(~ .x %>% filter(age!= "ind") %>% pull(shot) %>% sum())) %>% 
  unnest(data) %>% 
  ungroup() %>% 
  mutate(prop = shot / tot) %>%
  filter(age == "no_ad") %>% 
  ggplot(aes(x = yday,  y = prop, color = tot)) +
  geom_point() +
  geom_smooth(aes(weight = tot)) +
  facet_wrap(~ year) +
  ylim(c(0, 1))

# il y a moyen d'estimer la prop de jeunes en prenant une fenetre large
# Bourse de Fabricius visible 8 mois environ, source? & repro commence vers le 1er Juin 
# donc on détecte theoriquement tous les jeunes jusqu'au 1er février 
# ca semble même plus large car il y a un switch qu'à partir de fin mars
# on pourrait mm prendre en compte les samples en février jusqu'à la mi mars visiblement

# UK sample data: adult male proportion target ---------------------------------------

# evolution of the sex ratio over the year
UK_kill %>% 
  mutate(age_sex = age_sex %>% str_replace("no_", "no-"),
         age = age_sex %>% str_extract(regex(".*(?=_)")),
         sex = age_sex %>% str_extract(regex("(?<=_).*")),
         age = age %>% str_replace("-", "_")) %>% 
  filter(age != "ind", sex != "ind") %>% 
  group_by(age, month = month(date, label = TRUE)) %>%
  nest() %>% 
  mutate(data = data %>% 
           map( ~.x %>% 
                  mutate(tot = sum(shot)) %>% 
                  filter(sex == "mal") %>%
                  summarize(mal = sum(shot),
                            tot = unique(tot), 
                            ratio = mal/tot))) %>%
  unnest(data) %>% 
  ungroup() -> UK_target

UK_target %>% 
  ggplot(aes(x = month, y = ratio, group = age, fill = age)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(tot))) + 
  ylim(0, 1)
# no trend over the year, on can use the average for adult

UK_kill %>% 
  mutate(age_sex = age_sex %>% str_replace("no_", "no-"),
         age = age_sex %>% str_extract(regex(".*(?=_)")),
         sex = age_sex %>% str_extract(regex("(?<=_).*")),
         age = age %>% str_replace("-", "_")) %>% 
  filter(age != "ind", sex != "ind") %>% 
  group_by(age) %>%
  nest() %>% 
  mutate(data = data %>% 
           map(~ .x %>% 
                 mutate(tot = sum(shot)) %>% 
                 filter(sex == "mal") %>%
                 summarize(mal = sum(shot),
                           tot = unique(tot), 
                           ratio = mal/tot))) %>%
  unnest(data) %>% 
  ungroup()
# -> male proportion: 60% for adults, 54% pour the chicks

# UK sample data: recruit proportion -------------------------------------------------

# one consider only winter age difference as explained before (age evolution analyses)
UK_kill %>% 
  filter(month(date) %in% c(12, 1)) %>% 
  mutate(year = if_else(month(date) == 12, year(date) + 1, year(date)) %>% 
           str_c("0101") %>% ymd) %>% 
  filter(age_sex %>% str_detect("ad")) %>% 
  group_by(year) %>% 
  nest() %>% 
  mutate(data = 
           data %>% 
           map(~ tibble(
             ad = .x %>% filter(age_sex %>% str_starts("ad")) %>% 
               pull(shot) %>% sum(),
             no_ad = .x %>% filter(age_sex %>% str_detect("no_ad")) %>% 
               pull(shot) %>% sum()))) %>% 
  unnest(data) %>% 
  ungroup() %>%
  filter(ad + no_ad > 0) %>% 
  arrange(year) -> UK_prop

# full time series
UK_nb_4 %>% 
  select(year) %>% 
  left_join(UK_prop) %>%
  mutate(across(c(ad, no_ad), ~ .x %>% replace_na(0))) %>% 
  group_by(year) %>% 
  nest() %>% 
  ungroup() %>% 
  rename(age = data) -> UK_prop_2

# UK sample data: removal formatting -------------------------------------------------

# function to attribute the right year
get_year <- function(x) {
  UK_nb_4 %>% filter(start <= x, end >= x) %>% pull(year)}

# attribution to the right year: 
UK_kill %>% 
  rowwise() %>% 
  mutate(year = get_year(date)) %>% 
  left_join(UK_nb_4 %>% select(start, end, year)) -> UK_kill_2

UK_kill %>% 
  mutate(age = age_sex %>% str_extract(regex(".*(?=_)")),
         month = month(date, label = TRUE)) %>% 
  group_by(year = year(date), month, age) %>% 
  summarize(across(shot, sum)) %>% 
  ggplot(aes(x = month, y = shot, fill = age)) +
  geom_col(position = "dodge")

# Any removal must be subtracted from the pop size if it was killed after the date of 
# the winter count but before the reproduction
# so one must differentiate removals between this period in the year
# since no few chicks are observed before June, it is the criterion for the conversion.
# but by precaution, one remove the juveniles also in May in case 
# they are chicks of the year

UK_kill_2 %>% 
  mutate(age_sex = age_sex %>% str_replace("no_", "no-"),
         age = age_sex %>% str_extract(regex(".*(?=_)")),
         sex = age_sex %>% str_extract(regex("(?<=_).*")),
         age = age %>% str_replace("-", "_"),
         repro = if_else(date < (year + months(5)), "before_rep", "after_rep"),
         repro = if_else(month(date) == 5 & age == "no_ad", "after_rep", repro)) %>% 
  group_by(year, start, end, repro, age, sex) %>% 
  summarize(across(shot, sum)) %>%
  ungroup() -> UK_kill_3

# raw dataviz of the removals by year
UK_kill_3 %>% 
  mutate(age_sex = str_c(age, "_", sex)) %>% 
  group_by(year, age_sex) %>% 
  summarize(across(shot, sum)) %>% 
  ggplot(aes(x = year, y = shot, fill = age_sex)) +
  geom_col()

# full dataset
crossing(UK_nb_4 %>% distinct(year, start, end), 
         UK_kill_3 %>% distinct(repro, age, sex)) %>% 
  left_join(UK_kill_3 %>% select(-c(start, end))) %>% 
  mutate(shot = shot %>% replace_na(0)) -> UK_kill_4

UK_kill_4 %>% 
  filter(repro == "before_rep") %>% 
  group_by(year) %>% 
  summarize(killed_before_rep = sum(shot)) -> UK_to_remove

# UK count data: merge to complete all variables -------------------------------------

UK_nb_4 %>% 
  left_join(UK_to_remove) %>% 
  left_join(UK_sex_app_2) %>% 
  left_join(UK_prop_2) -> UK_count

# FR data import ----------------------------------------------------------------------

# sample data
read_excel("./Data/FR/Observation_et_operation_2021-01-16.xlsx",
           na = c("", "NA"),
           col_types = c("date", rep("guess", 45)),
           guess_max = 6000) %>%
  mutate(Date = date(Date),
         Dpt = as.character(Dpt)) %>%
  filter(Date > ymd("1950-01-01")) -> FR_kill

# count data
read_excel("./Data/FR/Comptage_hiver_2021-01-16.xlsx", 
           na = c("", "NA"), col_types = c("date", rep("numeric", 3))) %>% 
  mutate(Date = date(Date)) %>% 
  rename(date = Date, tot = Total, mal_nb = M, fem_nb = `F`) -> FR_nb

# FR count data: size - app sex - no rec prop ----------------------------------------

# winter counts 
FR_nb %>% 
  mutate(year = if_else(month(date) > 6, year(date) + 1, year(date)) %>% 
           str_c("0101") %>% ymd()) %>% 
  arrange(year) %>% 
  select(year, date, count = tot, 
         obs_type_mal = mal_nb, obs_type_fem = fem_nb) -> FR_nb_2

# all year counts
FR_kill %>% 
  filter(Lieu_nom == "LAC DE GRAND LIEU", 
         month(Date) %in% c(10, 11, 12, 1, 2, 3),
         !is.na(Nb_obs_tot)) %>%
  mutate(year = if_else(month(Date) > 6, year(Date) + 1, year(Date)) %>%
           str_c("0101") %>% ymd()) %>% 
  select(year, date = Date, count = Nb_obs_tot, 
         obs_type_mal = Nb_obs_mal_ad, obs_type_fem = Nb_obs_fem_ad) %>% 
  anti_join(FR_nb_2, by = c("date", "year")) %>%
  bind_rows(FR_nb_2) %>% 
  right_join(tibble(year = seq(min(.$year), max(.$year), by = "year"))) %>% 
  arrange(year, date) %>% 
  mutate(date = if_else(is.na(date), year, date),
         across(where(is.numeric), ~ .x %>% replace_na(0))) -> FR_nb_3

FR_nb_3 %>% 
  group_by(year) %>% 
  arrange(desc(count)) %>% 
  rowid_to_column() %>% 
  filter(rowid == min(rowid)) %>% 
  ungroup() %>% 
  arrange(year) %>% 
  mutate(source = str_c(month(date, abbr = TRUE, label = TRUE), " - SNPN"),
         site = 1,
         start = date,
         ad = 0, 
         no_ad = 0) %>% 
  group_by(across(-c(ad, no_ad))) %>% 
  nest() %>% 
  ungroup() %>% 
  rename(age = data) %>% 
  left_join(FR_nb_3 %>% 
              select(-count) %>% 
              group_by(year) %>% 
              nest() %>% 
              ungroup() %>% 
              rename(sex_app = data)) %>% 
  mutate(end = c(.$start[-1], last(.$start) + years(1)) - days(1)) %>% 
  select(year, start, end, count, site, source, age, sex_app) -> FR_nb_4

# FR sample data: age evolution analyses ---------------------------------------------

get_year_2 <- function(x) {
  FR_nb_4 %>% filter(start <= x, end >= x) %>% pull(year)}

FR_kill %>% 
  select(Date, starts_with("Nb_tue"), -Nb_tue_oeuf, Nb_obs_oeuf) %>% 
  rename(Nb_tue_oeuf = Nb_obs_oeuf) %>% 
  filter((!is.na(Nb_tue_tot)) | 
           !(is.na(Nb_tue_mal_ad) & 
               is.na(Nb_tue_fem_ad) & 
               is.na(Nb_tue_juv) &
               is.na(Nb_tue_pulli) & 
               is.na(Nb_tue_ind))) %>% 
  rowwise() %>% 
  mutate_at(vars(starts_with("Nb")), replace_na, 0) %>% 
  mutate(
    check = sum(Nb_tue_fem_ad, Nb_tue_mal_ad, Nb_tue_juv, Nb_tue_pulli, Nb_tue_ind, 
                Nb_tue_oeuf),
    Nb_tue_tot = case_when(
      is.na(Nb_tue_tot) ~ check,
      Nb_tue_tot < check ~ check,
      TRUE ~ Nb_tue_tot),
    Nb_tue_ind = if_else(check < Nb_tue_tot, Nb_tue_ind + (Nb_tue_tot - check), 
                         Nb_tue_ind)) %>% 
  filter(Nb_tue_tot > 0) %>% 
  mutate(ad_fem = Nb_tue_fem_ad, 
         ad_mal = Nb_tue_mal_ad, 
         jeu_ind = Nb_tue_juv + Nb_tue_pulli + Nb_tue_oeuf, 
         ind_ind = max(Nb_tue_ind, Nb_tue_tot - ad_fem - ad_mal - jeu_ind)) %>%
  select(date = Date, !starts_with("Nb"), -check) %>% 
  ungroup() %>% 
  filter(year(date) > 1986) %>% 
  rowwise() %>% 
  mutate(year = get_year_2(date)) %>% 
  left_join(FR_nb_4 %>% select(start, end, year)) %>% 
  ungroup() %>% 
  pivot_longer(contains("_"), names_to = "age_sex", values_to = "shot") -> FR_kill_2

# age of the samples over the year 
FR_kill_2 %>% 
  mutate(age = age_sex %>% str_extract(regex(".*(?=_)"))) %>% 
  group_by(yday = yday(date),
           year = year(date), 
           age) %>% 
  summarize(across(shot, sum)) %>% 
  ungroup() -> FR_age

# sampling intensity over the year
FR_age %>% 
  group_by(yday, age) %>% 
  summarize(across(shot, sum)) %>% 
  ggplot(aes(x = yday, y = shot, fill = age)) +
  geom_col(position = "dodge")
# high sampling pressure in summer

# age proportion in the samples 
FR_age %>% 
  group_by(yday, age) %>% 
  summarize(across(shot, sum)) %>% 
  group_by(yday) %>% 
  nest() %>% 
  mutate(tot = data %>% map_dbl(~ .x %>% pull(shot) %>% sum())) %>% 
  unnest(data) %>% 
  ungroup() %>% 
  mutate(prop = shot / tot) %>% 
  ggplot(aes(x = yday, y = prop, color = age)) +
  geom_point() +
  geom_smooth()
# chicks appears from the 125th day (1st of May)
# No data in winter but UK shows juvenile character disappears by the end of the winter
# best picture of the recruitment in winter because there are many samples and 
# it is the last moment with the juvenile character

# Adult proportion in the removals over the year
FR_kill_2 %>% 
  mutate(age = age_sex %>% str_extract(regex(".*(?=_)"))) %>% 
  filter(age != "ind") %>% 
  group_by(age, month = month(date, label = TRUE)) %>% 
  summarize(across(shot, sum)) %>% 
  group_by(month) %>% 
  mutate(ratio = shot / sum(shot))  %>% 
  filter(age == "ad") %>% 
  ggplot(aes(x = month, y = ratio)) +
  geom_col()
# -> not a clear switch by the end of January, but almost no removal son complicated to
# judge on this, ok to consider the winter as a good picture of the juvenile proportion

# age proportion by year
FR_age %>% 
  group_by(yday) %>% 
  nest() %>% 
  mutate(tot = data %>% 
           map_dbl(~ .x %>% filter(age!= "ind") %>% pull(shot) %>% sum())) %>% 
  unnest(data) %>% 
  ungroup() %>% 
  mutate(prop = shot / tot) %>%
  filter(age == "jeu") %>% 
  ggplot(aes(x = yday,  y = prop, color = tot)) +
  geom_point() +
  geom_smooth(aes(weight = tot)) +
  facet_wrap(~ year) +
  ylim(c(0, 1))

# il y a moyen d'estimer la prop de jeunes en prenant une fenetre large
# Bourse de Fabricius visible 8 mois environ, source? & repro commence vers le 1er Juin 
# donc on détecte theoriquement tous les jeunes jusqu'au 1er février 
# pas d'info ici

# FR sample data: adult male proportion target ---------------------------------------

# evolution of the sex ratio over the year
FR_kill_2 %>% 
  mutate(age = age_sex %>% str_extract(regex(".*(?=_)")),
         sex = age_sex %>% str_extract(regex("(?<=_).*"))) %>% 
  filter(age != "ind", sex != "ind") %>% 
  group_by(age, month = month(date, label = TRUE)) %>%
  nest() %>% 
  mutate(data = data %>% 
           map( ~.x %>% 
                  mutate(tot = sum(shot)) %>% 
                  filter(sex == "mal") %>%
                  summarize(mal = sum(shot),
                            tot = unique(tot), 
                            ratio = mal/tot))) %>%
  unnest(data) %>% 
  ungroup() -> FR_target

FR_target %>% 
  ggplot(aes(x = month, y = ratio, group = age, fill = age)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(tot))) + 
  ylim(0, 1)
# no trend over the year, issue in fall, perhaps because of bad sampling
# one remove the August to December

FR_kill_2 %>% 
  mutate(age = age_sex %>% str_extract(regex(".*(?=_)")),
         sex = age_sex %>% str_extract(regex("(?<=_).*"))) %>% 
  filter(age != "ind", sex != "ind", month(date) %in% c(1:7)) %>% 
  group_by(age) %>%
  nest() %>% 
  mutate(data = data %>% 
           map(~ .x %>% 
                 mutate(tot = sum(shot)) %>% 
                 filter(sex == "mal") %>%
                 summarize(mal = sum(shot),
                           tot = unique(tot), 
                           ratio = mal/tot))) %>%
  unnest(data) %>% 
  ungroup()
# -> male proportion: 60% for adults, consistent with UK data

# FR sample data: removal formatting -------------------------------------------------

# attribution to the right year: 
FR_kill_2 %>% 
  left_join(FR_nb_4 %>% select(start, end, year)) -> FR_kill_3

FR_kill_3 %>% 
  mutate(age = age_sex %>% str_extract(regex(".*(?=_)")),
         month = month(date, label = TRUE)) %>% 
  group_by(year = year(date), month, age) %>% 
  summarize(across(shot, sum)) %>% 
  ggplot(aes(x = month, y = shot, fill = age)) +
  geom_col(position = "dodge")

# Any removal must be subtracted from the pop size if it was killed after the date of 
# the winter count but before the reproduction
# so one must differentiate removals between this period in the year
# since no few chicks are observed before June, it is the criterion for the conversion.
# but by precaution, one remove the juveniles also in May in case 
# they are chicks of the year

FR_kill_3 %>% 
  mutate(age = age_sex %>% str_extract(regex(".*(?=_)")),
         sex = age_sex %>% str_extract(regex("(?<=_).*")),
         repro = if_else(date < year + months(5), "before_rep", "after_rep"),
         repro = if_else(month(date) == 5 & age == "no_ad", "after_rep", repro)) %>% 
  group_by(year, start, end, repro, age, sex) %>% 
  summarize(across(shot, sum)) %>%
  ungroup() -> FR_kill_4

# raw dataviz of the removals by year
FR_kill_4 %>% 
  mutate(age_sex = str_c(age, "_", sex)) %>% 
  group_by(year, age_sex) %>% 
  summarize(across(shot, sum)) %>% 
  ggplot(aes(x = year, y = shot, fill = age_sex)) +
  geom_col()

# full dataset
crossing(FR_nb_4 %>% distinct(year, start, end), 
         FR_kill_4 %>% distinct(repro, age, sex)) %>% 
  left_join(FR_kill_4 %>% select(-c(start, end))) %>% 
  mutate(shot = shot %>% replace_na(0)) %>% 
  mutate(age = if_else(age == "jeu", "no_ad", age)) -> FR_kill_5

FR_kill_5 %>% 
  filter(repro == "before_rep") %>% 
  group_by(year) %>% 
  summarize(killed_before_rep = sum(shot)) -> FR_to_remove

# FR count data: merge to complete all variables -------------------------------------

FR_nb_4 %>% 
  left_join(FR_to_remove) %>% 
  relocate(killed_before_rep, .after = source) -> FR_count

# Binding of the two datasets ----------------------------------------------------------

# unique dataset
crossing(year = seq(ymd(19600101), ymd(20210101), "year"), 
         repro = c("before_rep", "after_rep"), 
         age = c("ad", "no_ad", "ind"), 
         sex = c("fem", "mal", "ind")) %>% 
  filter(!(age == "ind" & sex != "ind")) -> base

base %>% 
  left_join(UK_kill_4 %>% distinct(year, start, end)) %>% 
  left_join(UK_kill_4 %>% select(-c(start, end))) %>% 
  mutate(shot = shot %>% replace_na(0), 
         pop = "UK") -> UK_frag

base %>%
  left_join(FR_kill_5 %>% distinct(year, start, end)) %>% 
  left_join(FR_kill_5 %>% select(-c(start, end))) %>% 
  mutate(shot = shot %>% replace_na(0), 
         pop = "FR") -> FR_frag

bind_rows(UK_frag, FR_frag) %>% 
  mutate(start = if_else(is.na(start), year, start),
         end = if_else(is.na(end), year + years(1) - days(1), end)) %>% 
  select(year, start, end, pop, repro, age, sex, shot) -> frag

frag %>% 
  ggplot(aes(x = year, y = shot, fill = interaction(sex, age))) + 
  geom_col() + 
  facet_wrap(~ pop, ncol = 1, scales = "free_y")

base %>% 
  distinct(year) %>%
  left_join(UK_count) %>% 
  mutate(pop = "UK") -> UK_count_2

base %>% 
  distinct(year) %>% 
  left_join(FR_count) %>% 
  mutate(pop = "FR") -> FR_count_2

bind_rows(UK_count_2, FR_count_2) %>% 
  mutate(start = if_else(is.na(start), year, start),
         end = if_else(is.na(end), year + years(1) - days(1), end),
         across(c(count, site, killed_before_rep), ~ .x %>% replace_na(0))) -> count

count %>% 
  ggplot(aes(x = year, y = count, color = pop)) + 
  geom_line() +
  geom_point() +
  facet_wrap(~ pop, scales = "free_y", ncol = 1) +
  theme(legend.position = "none")

list(frag, count) %>% 
  write_rds("./Output/Ruddy_duck_data.rds")

# Exploration of the undetermined  ----------------------------------------------------

FR_kill %>% 
  select(Date, starts_with("Nb_tue"), -Nb_tue_oeuf, Nb_obs_oeuf) %>% 
  rename(Nb_tue_oeuf = Nb_obs_oeuf) %>% 
  filter((!is.na(Nb_tue_tot)) | 
           !(is.na(Nb_tue_mal_ad) & 
               is.na(Nb_tue_fem_ad) & 
               is.na(Nb_tue_juv) &
               is.na(Nb_tue_pulli) & 
               is.na(Nb_tue_ind))) %>% 
  rowwise() %>% 
  mutate_at(vars(starts_with("Nb")), replace_na, 0) %>% 
  mutate(
    Nb_tue_tot = max(c(Nb_tue_tot, 
                       sum(Nb_tue_fem_ad, Nb_tue_mal_ad, Nb_tue_juv, 
                           Nb_tue_pulli, Nb_tue_ind, Nb_tue_oeuf))),
    Nb_tue_ind = Nb_tue_tot - sum(Nb_tue_fem_ad, Nb_tue_mal_ad, 
                                  Nb_tue_juv, Nb_tue_pulli, Nb_tue_oeuf)
  ) %>% 
  filter(Nb_tue_tot > 0) %>% 
  mutate(ad = Nb_tue_fem_ad + Nb_tue_mal_ad, 
         jeu = Nb_tue_juv + Nb_tue_pulli + Nb_tue_oeuf, 
         ind = Nb_tue_ind) %>%
  select(date = Date, !starts_with("Nb")) %>% 
  ungroup() %>% 
  pivot_longer(cols = -date, names_to = "age", values_to = "shot") -> ind

ind %>% 
  group_by(year = year(date), age) %>% 
  summarise(across(shot, sum)) %>% 
  ggplot(aes(x = year, y = shot, fill = age)) +
  geom_col(position = "dodge") +
  scale_y_sqrt()

ind %>% 
  group_by(yday = yday(date), age) %>% 
  summarise(across(shot, sum)) %>% 
  ggplot(aes(x = yday, y = shot, color = age)) +
  geom_smooth()

ind %>% 
  group_by(yday = yday(date), age) %>% 
  summarise(across(shot, sum)) %>% 
  nest() %>% 
  mutate(data = data %>% map(~ .x %>% mutate(shot = shot / sum(shot)))) %>% 
  unnest(data) %>% 
  ggplot(aes(x = yday, y = shot, color = age)) +
  geom_smooth() +
  geom_point()
