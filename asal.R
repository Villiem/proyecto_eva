enoe1 <- enoe |> 
  filter(
    clase1 == 1,
    clase2 == 1, 
    between(eda, 15,64), 
    ing_x_hrs > 0, 
    ambito1 != 1, 
    pos_ocu %in% c(1,3),
    mh_col %in% c(1,2,7,8)
    #remune2c == 1
    ) |> 
  mutate(log2_ing_x_hrs = log2(ing_x_hrs))

mydesign <- enoe1 %>% 
  #filter(r_def == 0) %>% 
  srvyr::as_survey_design(
    strata = est_d_tri,
    id = upm,
    weights = fac_tri,
    nest = T
  )  

autoempleados <- mydesign |> 
  filter(mh_col %in% c(7,8)) |>
  group_by(emp, mh_col) |> 
  summarise(survey_total(), .groups = "drop")



mydesign |> 
  filter(pos_ocu %in% c(1,3)) 
  
create_labeled_df(autoempleados)

asalariados <- mydesign |> 
  filter(mh_col %in% c()) |>
  group_by(ing7c, mh_col) |> 
  summarise(survey_total(), .groups = "drop")


my_data_wide <- mydata_gt |> 
  select(-ends_with("_se")) |> 
  mutate(mh_col = janitor::make_clean_names(mh_col, allow_dupes = T)) |> 
  pivot_wider(
    names_from = mh_col,
    values_from = c(promedio_horas, promedio_ingreso, promedio_trabajo, promedio_edad),
    names_sep = "_"
  )

# Create the gt table with tab_spanner_delim
gt_table <- my_data_wide %>%
  gt(rowname_col = "ing7c") %>%
  tab_spanner_delim(delim = "_", reverse = T)

# Display the table
gt_table
