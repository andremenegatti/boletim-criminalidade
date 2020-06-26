library(tidyverse)
library(cagedExplorer)
source('custom_map_settings.R')
theme_set(custom_theme())

# Reading dataset
df_crimes <- readRDS('data/criminalidade-sp.rds')

# Estatísticas agregadas para todo o estado -----------------------------------
df_sp <- df_crimes %>% 
  select_if(is.numeric) %>% 
  select(-Codmun7, -codigo) %>%
  group_by(ano) %>% 
  summarise_all(sum) %>% 
  ungroup() %>% 
  mutate(
    taxa_hom = homicidio / populacao * 1e+5,
    taxa_furto = furto / populacao * 1e+5,
    taxa_roubo = roubo / populacao * 1e+5,
    taxa_frv = frv / populacao * 1e+5
  )

# Lineplot: crimes SP ---------------------------------------------------------
lineplot_crimes_sp <- 
  df_sp %>% 
  select(ano, contains('taxa_')) %>% 
  pivot_longer(cols = -ano, names_to = 'crime',
               values_to = 'taxa', names_prefix = 'taxa_') %>% 
  mutate(crime = case_when(
    crime == 'hom' ~ 'Homicídio',
    crime == 'furto' ~ 'Furto',
    crime == 'roubo' ~ 'Roubo',
    crime == 'frv' ~ 'Furto ou roubo de veículo'
  )) %>% 
  filter(ano >= 2010) %>% 
  ggplot() +
  geom_line(aes(x = ano, y = taxa)) +
  facet_wrap(~ crime, nrow = 2, scales = 'free_y') +
  scale_x_continuous(breaks = 2010:2019) +
  theme(panel.grid = element_blank(),
        strip.text = element_text(face= 'bold'),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    x = 'Ano', y = 'Ocorrências por 100 mil habitantes',
    title = 'Evolução da criminalidade no Estado de São Paulo',
    subtitle = 'Ocorrências por 100 mil habitantes, de 2010 a 2019'
  ) ; lineplot_crimes_sp

ggsave(plot = lineplot_crimes_sp, width = 6, height = 6,
       filename = 'plots/lineplot-crimes-sp.png')

# Estatísticas agregadas: regiões administrativas -----------------------------
# pct_change = function(x) (x - lag(x)) / lag(x) * 100

df_adm <- 
df_crimes %>% 
  group_by(ano, regiao_administrativa) %>% 
  summarise(
    taxa_hom = sum(homicidio) / sum(populacao) * 1e+5,
    taxa_furto = sum(furto) / sum(populacao) * 1e+5,
    taxa_roubo = sum(roubo) / sum(populacao) * 1e+5,
    taxa_frv = sum(frv) / sum(populacao) * 1e+5
  ) %>% 
  ungroup()

# Lineplot: roubos nas regiões administrativas --------------------------------
lineplot_roubos_reg_adm <- df_adm %>% 
  filter(ano >= 2010) %>% 
  ggplot() +
  geom_line(aes(x = ano, y = taxa_roubo)) +
  # geom_hline(yintercept = 0, linetype = 'dotted', color = 'darkred') +
  facet_wrap(~ regiao_administrativa, scales = 'free_y') +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        strip.text = element_text(face = 'bold')) +
  scale_x_continuous(breaks = 2010:2019) +
  labs(
    y = 'Roubos por 100 mil habitantes',
    x = 'Ano',
    title = 'Evolução da taxa de roubos',
    subtitle = 'Regiões Administrativas de São Paulo, 2010 a 2019'
  ) ; lineplot_roubos_reg_adm

ggsave(plot = lineplot_roubos_reg_adm, width = 7, height = 7,
       filename = 'plots/lineplot_roubos_reg_adm.png')

# Mapa: furtos ----------------------------------------------------------------
mapa_furtos <- df_adm %>% 
  filter(ano == 2019) %>% 
  add_geometry_regioes_adm() %>% 
  rename('Furtos/100 mil hab.' = taxa_furto) %>% 
  tm_shape() +
  tm_style(
    "beaver",
    legend.format = list(fun = function(x) round(x),
                         text.separator = " a ")
  ) +
  tm_fill(
    'Furtos/100 mil hab.',
    palette = 'Reds',
    style = 'quantile',
    n = 4,
    alpha = 1,
    id = "regiao_administrativa",
    textNA = "Sem dados",
    colorNA = '#fff7bc'
  ) +
  tm_layout(main.title = 
              'Regiões Adm. de SP - Furtos por 100 mil habitantes - 2019') +
  custom_map_settings ; mapa_furtos

# Saving
tmap_save(mapa_furtos, height = 6, width = 6,
          filename = 'plots/mapa-furtos')


# Mapa: homicídios ------------------------------------------------------------
mapa_hom <- df_adm %>% 
  filter(ano == 2019) %>% 
  add_geometry_regioes_adm() %>% 
  rename('Homicídios/100 mil hab.' = taxa_hom) %>% 
  tm_shape() +
  tm_style(
    "beaver",
    legend.format = list(fun = function(x) round(x),
                         text.separator = " a ")
  ) +
  tm_fill(
    'Homicídios/100 mil hab.',
    palette = 'Reds',
    style = 'quantile',
    n = 4,
    alpha = 1,
    id = "regiao_administrativa",
    textNA = "Sem dados",
    colorNA = '#fff7bc'
  ) +
  tm_layout(main.title = 
              'Regiões Adm. de SP - Homicídios por 100 mil habitantes - 2019') +
  custom_map_settings ; mapa_hom

# Saving
tmap_save(mapa_hom, height = 6, width = 6,
          filename = 'plots/mapa-hom.png')

# Mapa: roubos ----------------------------------------------------------------
mapa_roubos <- df_adm %>% 
  filter(ano == 2019) %>% 
  add_geometry_regioes_adm() %>% 
  rename('Roubos/100 mil hab.' = taxa_roubo) %>% 
  tm_shape() +
  tm_style(
    "beaver",
    legend.format = list(fun = function(x) round(x),
                         text.separator = " a ")
  ) +
  tm_fill(
    'Roubos/100 mil hab.',
    palette = 'Reds',
    style = 'quantile',
    n = 4,
    alpha = 1,
    id = "regiao_administrativa",
    textNA = "Sem dados",
    colorNA = '#fff7bc'
  ) +
  tm_layout(main.title = 
              'Regiões Adm. de SP - Roubos por 100 mil habitantes - 2019') +
  custom_map_settings ; mapa_roubos

# Saving
tmap_save(mapa_roubos, height = 6, width = 6,
          filename = 'plots/mapa-roubos.png')


# Mapa: FRV -------------------------------------------------------------------
mapa_frv <- df_adm %>% 
  filter(ano == 2019) %>% 
  add_geometry_regioes_adm() %>% 
  rename('FRV/100 mil hab.' = taxa_frv) %>% 
  tm_shape() +
  tm_style(
    "beaver",
    legend.format = list(fun = function(x) round(x),
                         text.separator = " a ")
  ) +
  tm_fill(
    'FRV/100 mil hab.',
    palette = 'Reds',
    style = 'quantile',
    n = 4,
    alpha = 1,
    id = "regiao_administrativa",
    textNA = "Sem dados",
    colorNA = '#fff7bc'
  ) +
  tm_layout(main.title = 
              'Regiões Adm. de SP - Furto/roubo de veículo/100 mil hab. - 2019') +
  custom_map_settings ; mapa_frv

# Saving
tmap_save(mapa_frv, height = 6, width = 6,
          filename = 'plots/mapa-frv.png')
