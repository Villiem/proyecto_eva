



mydesign |> 
  filter(clase2 == 1, eda >= 15, ambito1 ==2, ingocup >0) |>
  ggplot(aes(ingocup, col = factor(sjlabelled::as_label(emp_ppal)))) +
  stat_lorenz() +
  geom_abline(linetype = "dashed") +
  theme_economist() +
  labs(
    title = "Curva de Lorenz para el empleo formal y informal",
    color = NULL,
    x = "Porcentaje de la población",
    y = "Porcentaje del ingreso",
    caption = 'Elaboración propia con datos de la ENOE 2024Q2'
  )

mydesign |> 
  filter(clase2 == 1, eda >= 15, ambito1 ==2, ingocup >0) |>
  with(ineq(ingocup2, #variable de ingresos
            type="Gini", #tipo de medida
            na.rm==T))
enoe1 <- enoe |> 
  filter(clase2 == 1, between(eda, 15,64), ingocup > 0) 

mydesign <- enoe1 %>% 
  #filter(r_def == 0) %>% 
  srvyr::as_survey_design(
    strata = est_d_tri,
    id = upm,
    weights = fac_tri,
    nest = T
  )  


enoe |> 
  filter(clase2 == 1, eda >= 15, ambito1 ==2, ingocup >0, emp_ppal ==2) |> 
  with(ineq(ingocup, type = "Gini", na.rm = T))

reldist::gini(pob_informal$variables$ingocup, pob_informal$variables$fac_tri)

edad_por_ingreso <- mydesign |> 
  filter(clase2 == 1, between(eda,15,64), ambito1 ==2) |> 
  group_by(ing7c, emp_ppal) |> 
  summarise(survey_mean(eda))

edad_por_ingreso |> 
  ungroup() |> 
create_labeled_df() |> 
  select(-ends_with("_se")) |> 
  #select(emp_ppal, ing7c, porcentaje_ingreso) %>%  # Seleccionar las columnas relevantes
  pivot_wider(names_from = emp_ppal, values_from = coef) |> 
  gt() |> 
  tab_spanner(
    label = "Edad promedio",
    columns = c(`Empleo informal`, `Empleo formal`))

    