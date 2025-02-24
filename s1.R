library(tidyverse)
library(srvyr)
library(labelled)
library(gt)
options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")

enoe <- rio::import('./data/ENOE_SDEMT224.sav')


mydesign <- enoe %>% 
  #filter(r_def == 0) %>% 
  srvyr::as_survey_design(
  strata = est_d_tri,
  id = upm,
  weights = fac_tri,
  nest = T
)

jovnes_tabla <- mydesign %>% 
  filter(between(eda, 12, 29), ingocup > 0) %>% 
  group_by(ent) %>% 
  summarise(survey_mean(ingocup))

original_labels <- sapply(enoe, get_labels)

ingreso_jovens <- jovnes_tabla %>% 
  mutate(ent = ent_labels[as.character(ent)]) %>% 
  gt() %>% 
  tab_header('Ingreso de los jóvenes de 15 a 29',
             subtitle = 'Elaboración propia con base en la ENOE') %>% 
  cols_label(
    ent = md('Entidad'),
    coef = md('Media de ingreso'),
    `_se` = md('Error Estándar')
  )

ent_labels <- setNames(original_labels[["ent"]], 1:32)

gtsave(ingreso_jovens, filename = "tab_1.html", path = 'figs/',inline_css = TRUE)

