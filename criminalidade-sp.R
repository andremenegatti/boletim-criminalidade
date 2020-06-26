library(tidyverse)
library(readxl)
library(cagedExplorer)

# Reading data from .xlsx spreadsheets (one for each city) --------------------
files <- list.files("data/xlsx-ssp-pre-join")
paths <- str_c('data/xlsx-ssp-pre-join/', files)

read_excel_partial <- partial(read_excel, sheet = 1, range = 'A1:E20')

df_list <- paths %>% 
  map(.f = safely(read_excel_partial))

df_list_t <- df_list %>% 
  transpose() %>%
  simplify_all()

pt1 <- map2(.x = df_list_t$result,
       .y = str_remove(files, '\\.xlsx'),
       .f = ~ mutate(.x, municipio_clean = .y) %>% 
         select(municipio_clean, ano, Homicidio, Furto, Roubo, FRV)
       ) %>% 
  bind_rows()

# Reading data for the remaining cities (stored in a single .csv file) --------
pt2 <- read_delim('data/criminalidade-ssp-remaining-cities.csv', delim = ';') %>% 
  select(municipio_clean = municipio, ano, Homicidio, Furto, Roubo, FRV)

# Joining datasets ------------------------------------------------------------  
df_ocorrencias <- pt1 %>% 
  bind_rows(pt2) %>% 
  arrange(municipio_clean, ano) %>% 
  left_join(
    municipios_sp %>% 
      select(Codmun7, codigo, municipio, municipio_clean,
             regiao_administrativa, regiao_governo), 
    by = 'municipio_clean'
    )

# Joining population data from IBGE -------------------------------------------
# 2001 to 2019 (except 2010 and 2007): estima pop
df_estima_pop <- read_excel('data/estimativa-populacao-municipios-ibge.xlsx',
                            sheet = 1, range = 'A1:S5571') %>% 
  filter(Codmun7 %in% df_ocorrencias$Codmun7) %>% 
  select(-municipio) %>% 
  mutate_all(as.numeric) 

# 2010: census data
df_pop2010 <- read_excel('data/populacao-municipios-censo-2010.xlsx',
                         range = 'A3:C5568', sheet = 1,
                         col_types = c('numeric', 'text', 'numeric')) %>% 
  filter(Codmun7 %in% df_ocorrencias$Codmun7) %>% 
  select(-municipio)

# Joining estima pop and census
df_pop <- df_estima_pop %>% 
  left_join(df_pop2010, by = 'Codmun7') %>% 
  pivot_longer(cols = -Codmun7, names_to = 'ano', values_to = 'populacao') %>%
  mutate(ano = as.numeric(ano)) %>% 
  arrange(Codmun7, ano)

# Joining crimes and population datasets
df_crimes <- df_ocorrencias %>% 
  left_join(df_pop, by = c('Codmun7', 'ano')) %>% 
  group_by(municipio_clean) %>% 
  # 2007: linear interpolation
  mutate(populacao = ifelse(is.na(populacao),
                            (lag(populacao) + lead(populacao)) / 2,
                            populacao)) %>% 
  ungroup() %>% 
  rename(homicidio = Homicidio, furto = Furto, roubo = Roubo, frv = FRV) 

# Saving datasets -------------------------------------------------------------
saveRDS(df_crimes, 'data/criminalidade-sp.rds')
write_excel_csv2(df_crimes, 'data/criminalidade-sp.csv')
