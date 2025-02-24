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

towny |>
  dplyr::select(name, starts_with("population")) #|>
  dplyr::filter(grepl("^F", name)) |>
  gt() |>
  tab_spanner_delim(delim = "_") |>
  fmt_integer()

towny_subset_gt
