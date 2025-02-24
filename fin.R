pacman::p_load(tidyverse, srvyr, convey, gt, ineq)
descarga_enoe <- function(){
  link.base = 'https://www.inegi.org.mx/contenidos/programas/enigh/nc/'
  
  # Construcción del enlace
  url.base = 'https://www.inegi.org.mx/contenidos/programas/enoe/15ymas/microdatos//enoe_2024_trim2_sav.zip'
  #print(link)
  temp.enoe = tempfile()
  formato = 'sav'
  zipdir = tempdir()
  utils::download.file(url.base, temp.enoe)
  utils::unzip(temp.enoe, exdir = './data')
  list_dataraw = list.files(zipdir, pattern = paste0(formato,'$'), full.names = T)
  list_names = basename(tools::file_path_sans_ext(list_dataraw))
  # Read all files in the folder
  output = lapply(list_dataraw, rio::import)
  names(output) = list_names
}

#descarga_enoe()

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

# Function to replace values with labels
replace_with_labels <- function(x) {
  labels <- attr(x, "labels")
  if (!is.null(labels)) {
    label_names <- names(labels)
    result <- sapply(x, function(val) {
      index <- which(labels == val)
      if (length(index) > 0) {
        return(label_names[index])
      } else {
        return(as.character(val))
      }
    })
    factor(result, levels = unique(c(label_names, as.character(x))))
  } else {
    x
  }
}

# Function to create labeled data frame
create_labeled_df <- function(df) {
  # Store original column labels
  col_labels <- sapply(df, function(x) attr(x, "label"))
  
  # Replace values with labels
  df_labeled <- df %>%
    mutate(across(everything(), replace_with_labels))
  
  # Apply original column labels as attributes
  for (col in names(df_labeled)) {
    if (!is.null(col_labels[[col]])) {
      attr(df_labeled[[col]], "label") <- col_labels[[col]]
    }
  }
  
  df_labeled
}
# Tasas de formalidad e informalidad, no agropecuario
mydesign |> 
  filter(clase2 == 1, eda >= 15, ambito1!=1) |> 
  summarise(
    #survey_total()
    tasa_informalidad = survey_ratio(emp_ppal == 1, na.rm = TRUE, clase2 == 1),
    tasa_formalidad = survey_ratio(emp_ppal == 2, na.rm = TRUE, clase2 == 1)
  )


# Calculamos el total para cada grupo de formalidad/informalidad primero
totales_emp_ppal <- mydesign |> 
  filter(clase2 == 1, eda >= 15, ambito1 ==2) |>  
  group_by(emp_ppal) |> 
  summarise(total_emp_ppal = survey_total()) 

# Luego calculamos el porcentaje por nivel de ingreso
porcentajes_ingreso <- mydesign |> 
  filter(clase2 == 1, eda >= 15, ambito1 == 2) |>  
  group_by(emp_ppal, ing7c) |> 
  summarise(
    total_grupo = survey_total()
  ) |> 
  left_join(totales_emp_ppal, by = "emp_ppal") |> 
  mutate(porcentaje_ingreso = total_grupo / total_emp_ppal) |> 
  ungroup()


# Use the function to create your labeled data frame
labeled_df <- create_labeled_df(porcentajes_ingreso)

summary_df <- labeled_df |> 
  select(-ends_with("_se"),-total_emp_ppal) |> 
  #select(emp_ppal, ing7c, porcentaje_ingreso) %>%  # Seleccionar las columnas relevantes
  pivot_wider(names_from = emp_ppal, values_from = c(porcentaje_ingreso,total_grupo))


summary_df %>%
  gt() %>%
  # Formatting porcentaje_ingreso as percentage
  fmt_percent(
    columns = vars(`porcentaje_ingreso_Empleo informal`, `porcentaje_ingreso_Empleo formal`),
    decimals = 2
  ) %>%
  # Formatting total_grupo as millions
  fmt_number(
    columns = vars(`total_grupo_Empleo informal`, `total_grupo_Empleo formal`),
    scale_by = 1e-6,  # Scale the numbers to millions
    decimals = 2,
    suffixing = TRUE
  ) %>%
  # Add column spanners
  tab_spanner(
    label = "Empleo informal",
    columns = vars(`porcentaje_ingreso_Empleo informal`, `total_grupo_Empleo informal`)
  ) %>%
  tab_spanner(
    label = "Empleo formal",
    columns = vars(`porcentaje_ingreso_Empleo formal`, `total_grupo_Empleo formal`)
  ) %>%
  # Label the sub-columns as "Porcentaje" and "Total (millones)"
  cols_label(
    `porcentaje_ingreso_Empleo informal` = "Porcentaje",
    `total_grupo_Empleo informal` = "Total",
    `porcentaje_ingreso_Empleo formal` = "Porcentaje",
    `total_grupo_Empleo formal` = "Total"
  ) |> 
  tab_footnote("No agropecuaria",
               locations = cells_column_labels(columns = ing7c)) |> 
  tab_source_note(source_note = "Elaboración propia con datos de la ENOE 2024-Q2")
