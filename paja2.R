# Convert survey design object to a data frame to use in ggplot

test <- mydesign |> 
  filter(mh_col %in% c(1,2,7,8))
  #group_by(mh_col) |> 
  #summarise(mean_log_ing = survey_mean(ing_x_hrs, na.rm = TRUE, .groups = "drop"))

mydesign |> 
  filter(mh_col == 7) |> 
  #group_by(ing7c, mh_col) |> 
  summarise(survey_total())

mydata <- mydesign |> 
  group_by(ing7c, mh_col) |> 
  #group_by(ing7c, mh_col) |> 
  summarise(promedio_horas = survey_mean(hrsocup),
            promedio_ingreso = survey_mean(ing_x_hrs),
            promedio_trabajo = survey_mean(t_tra),
            promedio_edad = survey_mean(eda),
            .groups = "drop")


mydata_gt <- create_labeled_df(mydata)



mydata_gt |> 
  select(-ends_with("_se")) |> 
  mutate(mh_col = janitor::make_clean_names(mh_col, allow_dupes = T)) |> 
  gt(rowname_col = "ing7c") |>   # Set ing7c as the main row label|> 
  tab_spanner_delim(columns = mh_col, split = "last", delim = "_")
    
  tab_spanner(label = "Horas", columns = starts_with("promedio_horas")) %>%
  tab_spanner(label = "Ingreso", columns = starts_with("promedio_ingreso")) %>%
  tab_spanner(label = "Trabajo", columns = starts_with("promedio_trabajo")) %>%
  tab_spanner(label = "Edad", columns = starts_with("promedio_edad")) %>%
  cols_label(ing7c = "Income Level")  # Optionally relabel 'ing7c'


ggplot(test$variables, aes(x = log2_ing_x_hrs, fill = factor(mh_col))) +
  geom_histogram(position = "dodge", bins = 30, alpha = 0.7) +
  labs(title = "Distribution of Log2(ing_x_hrs) by mh_col",
       x = "Log2(ing_x_hrs + 1)",
       y = "Count",
       fill = "mh_col") +
  theme_minimal()

mydata
