##################################
## Blog de investigación Emilio ##

rm(list=ls())
options(scipen=999)
# options(max.print=100)

library (tidyverse)
library (readxl)
# library (hrbrthemes)
# library (scales)
# library (flextable)
# library (modeest)

# Población de los seis municipios
setwd("C:/Users/mckfm/Documents/Inputs/Inputs")
pob_mun <- read_excel("pob_municipios_raw.xlsx")

# Defunciones por municipio con refinería
setwd("C:/Users/mckfm/Documents/Inputs/Inputs")
def_mun_sex <- read_excel("defunciones_mun_sex.xlsx")

def_mun_sex$tipo <- trimws(def_mun_sex$tipo)
def_mun_sex$sexo <- trimws(def_mun_sex$sexo)

# Defunciones nacionales
def_nac_sex <- read_excel("defunciones_nac_sex.xlsx")

def_nac_sex$tipo <- trimws(def_nac_sex$tipo)
def_nac_sex$sexo <- trimws(def_nac_sex$sexo)

# Población nacional
pob_nac <- read_excel("pob_nac_raw.xlsx")

def_nac_sex_bn <- def_nac_sex %>% 
  inner_join(pob_nac, by = "año") %>% 
  mutate(casos_nac_10mil = (total/pob_nac)*10000) %>% 
  select(-total)

# Unificación de datos
base_final <- def_mun_sex %>% 
  pivot_longer(Salamanca:Minatitlán, 
               names_to = "municipio", 
               values_to = "total") %>% 
  # mutate(municipio = case_when(
  #   municipio == "Tula.de.Allende" ~ "Tula de Allende",
  #   municipio == "Cadereyta.Jiménez" ~ "Cadereyta Jiménez",
  #   municipio == "Salina.Cruz" ~ "Salina Cruz",
  #   municipio == "Ciudad.Madero" ~ "Ciudad Madero",
  #   municipio == "Salamanca" ~ "Salamanca",
  #   municipio == "Minatitlán" ~ "Minatitlán"
  # )) %>% 
  inner_join(pob_mun, by = c("año", "municipio")) %>% 
  select(-indicador) %>% 
  mutate(casos_mun_10mil = (total/pob_tot)*10000) %>% 
  inner_join(def_nac_sex_bn, by = c("año", "sexo", "tipo"))

## Ahora sí, ¡a graficar!
base_final %>% 
  filter(sexo == "Total") %>% 
  ggplot(aes(x = año))+
  geom_line(aes(y = casos_mun_10mil,
            colour = municipio),
            size=1.25)+
  scale_colour_manual(values = c("#FFD700", "#FFB14E",
                                 "#FA8775", "#EA5F94",
                                 "#CD34B5", "#9D02D7"))+
  geom_line(aes(y = casos_nac_10mil),
            size=1.5,
            color = "seagreen3")+
  facet_wrap(~tipo,
            ncol=2, 
            scales="free_y")+
  theme_bw()+
  labs(title = "\nMuertes por distintas enfermedades en municipios con refinerías de PEMEX",
       subtitle = "La línea verde es la media nacional",
       y = "\nMuertes por cada 10,000 habitantes",
       x = "Año",
       colour = "Municipio",
       caption = "Elaboración: Bernardo Luis Mc Kelligan y Emilio del Río Castro
       Datos: Censos y encuestas INEGI de población y mortalidad (2000-2020)")


base_final %>% 
  filter(sexo == "Total") %>% 
  group_by(año, tipo) %>% 
  summarise(casos_mun_10mil = mean(casos_mun_10mil),
            casos_nac_10mil = mean(casos_nac_10mil)) %>% 
  pivot_longer(casos_mun_10mil:casos_nac_10mil,
               names_to = "nivel", values_to = "casos_10mil") %>% 
  mutate(nivel = case_when(
    nivel == "casos_mun_10mil" ~ "Mun. con refinerías",
    nivel == "casos_nac_10mil" ~ "Nacional"
  )) %>% 
  ggplot(aes(x = año))+
  geom_line(aes(y = casos_10mil,
                colour = nivel),
            size=1.25)+
  facet_wrap(~tipo,
             ncol=2, 
             scales="free_y")+
  theme_bw()+
  labs(title = "\nMuertes por distintas enfermedades en municipios con refinerías de PEMEX",
       y = "\nMuertes por cada 10,000 habitantes",
       x = "Año",
       colour = "Nivel",
       caption = "Elaboración: Bernardo Luis Mc Kelligan y Emilio del Río Castro
       Datos: Censos y encuestas INEGI de población y mortalidad (2000-2020)")

