

# Tarea 2 -----------------------------------------------------------------

superficie <- read_rds("data/superficie.rds")

str(superficie)

summary(superficie)

superficie %>% 
  slice_head(n=10)

superficie %>% 
  filter(anio==1991)

superficie %>% 
  filter(anio==1991 & superficie_total>700000)

superficie %>% 
  select(starts_with("superficie"))

superficie %>% 
  mutate(mes_nombre = case_when(mes_num == 1 ~ "Enero",
                                             mes_num == 2 ~ "Febrero",
                                             mes_num == 3 ~ "Marzo",
                                             mes_num == 4 ~ "Abril",
                                             mes_num == 5 ~ "Mayo",
                                             mes_num == 6 ~ "Junio",
                                             mes_num == 7 ~ "Julio",
                                             mes_num == 8 ~ "Agosto",
                                             mes_num == 9 ~ "Septiembre",
                                             mes_num == 10 ~ "Octubre",
                                             mes_num == 11 ~ "Noviembre",
                                             mes_num == 12 ~ "Diciembre"))

superficie %>% group_by(anio) %>% summarise(superficie_promedio = mean(superficie_total))

superficie %>% mutate(tramo_superficie = case_when(superficie_total >= 1500000 ~ "grande",
                                                   superficie_total >= 1200000 ~ "mediana",
                                                   TRUE ~ "pequeña")) %>% 
  filter(tramo_superficie=="grande" & anio == 2019) %>% 
  mutate(vivienda_mayor = if_else(superficie_vivienda > superficie_no_vivienda, TRUE, FALSE)) %>% 
  mutate(superficie_hect = superficie_total/10000)
