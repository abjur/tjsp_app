library(tidyverse)
library(sf)

# Download this from https://github.com/abjur/prodTJSP
prod_complete <- readRDS("prod_complete.rds")

# Obtain d_sf d_sf_min.R script
d_from_sf <- d_sf$sf$municipio %>%
  as_tibble() %>%
  select(-geometry) %>%
  distinct(comarca, .keep_all = TRUE)

# types of productivity that we are considering
trab_lab <- c("Escreventes", "Oficiais de justiça", "Auxiliares judiciários")
prod_lab <- c("Completas", "Resumidas")

# pipeline to get our productivity data
prod_tidy <- prod_complete %>%
  mutate(tipo_prod = case_when(
    txt %in% trab_lab ~ "trab",
    txt %in% prod_lab ~ "prod",
    TRUE ~ "lixo"
  )) %>%
  filter(!tipo_prod %in% "lixo") %>%
  mutate(num = as.numeric(num)) %>%
  group_by_at(vars(year, month, tipo_prod, starts_with("id_"),
                   starts_with("nm_"))) %>%
  summarise(total = sum(num)) %>%
  ungroup() %>%
  spread(tipo_prod, total, fill = 0) %>%
  filter(prod > 0, trab > 0) %>%
  mutate(razao = prod / trab) %>%
  filter(!str_detect(nm_court, "EXEC")) %>%
  mutate(civel = str_detect(nm_court, "CIVEL|UNICA|CUMUL|JEC.*CIV"),
         criminal = str_detect(nm_court, "CRIM|UNICA|CUMUL|JEC.*CRIM")) %>%
  filter(civel | criminal) %>%
  mutate_at(vars(year, month), funs(as_factor)) %>%
  rename(comarca = nm_comarca) %>%
  dplyr::mutate(comarca = dplyr::case_when(
    comarca == "BRODOSQUI" ~ "BRODOWSKI",
    comarca == "ESTRELA D OESTE" ~ "ESTRELA DOESTE",
    comarca == "IPAUCU" ~ "IPAUSSU",
    comarca == "MOJI GUACU" ~ "MOGI GUACU",
    comarca == "PARIQUERA ACU" ~ "PARIQUERA-ACU",
    comarca == "CENTRAL" ~ "SAO PAULO",
    comarca == 'BRAS CUBAS' ~ 'MOGI DAS CRUZES',
    comarca == 'BUTANTA' ~ 'SAO PAULO',
    comarca == 'CENTRAL' ~ 'SAO PAULO',
    comarca == 'IPIRANGA' ~ 'SAO PAULO',
    comarca == 'ITAQUERA' ~ 'SAO PAULO',
    comarca == 'JABAQUARA' ~ 'SAO PAULO',
    comarca == 'LAPA' ~ 'SAO PAULO',
    comarca == 'MOJI GUACU' ~ 'MOGI GUACU',
    comarca == 'NOSSA SENHORA DO O' ~ 'SAO PAULO',
    comarca == 'PARELHEIROS' ~ 'SAO PAULO',
    comarca == 'PENHA' ~ 'SAO PAULO',
    comarca == 'PINHEIROS' ~ 'SAO PAULO',
    comarca == 'SANTANA' ~ 'SAO PAULO',
    comarca == 'SANTO AMARO' ~ 'SAO PAULO',
    comarca == 'SAO MIGUEL PAULISTA' ~ 'SAO PAULO',
    comarca == 'TATUAPE' ~ 'SAO PAULO',
    comarca == 'VILA MIMOSA' ~ 'SAO PAULO',
    comarca == 'VILA PRUDENTE' ~ 'SAO PAULO',
    comarca == "PALMEIRA D OESTE" ~ "PALMEIRA D'OESTE",
    comarca == "SANTA BARBARA D OESTE" ~ "SANTA BARBARA D'OESTE",
    TRUE ~ comarca
  )) %>%
  inner_join(d_from_sf, "comarca")

# save data in data/ directory
write_rds(prod_tidy, "../data/prod_tidy.rds", compress = "xz")
