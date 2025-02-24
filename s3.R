library(srvyr)
library(tidyverse)
library(sjlabelled)
library(gt)

options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")

mydesign |> 
  filter(clase2 == 1, eda >= 15, ambito1==2) |> 
  group_by(ing7c) |> 
  summarise(survey_total())


mydesign |> 
  filter(clase2 == 1, eda >= 15) |> 
  #srvyr::group_by(ing7c) |>
  srvyr::group_by(emp_ppal) |>
  summarise(tasa_infomalidad = survey_ratio(emp_ppal ==1,1))

mydesign |> 
  filter(clase2 == 1, eda >= 15) |> 
  summarise(
    tasa_informalidad = survey_ratio(emp_ppal == 1, na.rm = TRUE, clase2 == 1),
    tasa_formalidad = survey_ratio(emp_ppal == 2, na.rm = TRUE, clase2 == 1)
  )

mydesign |> 
  filter(clase2 == 1, eda >= 15, ambito1!=1) |> 
  summarise(
    tasa_informalidad = survey_ratio(emp_ppal == 1, na.rm = TRUE, clase2 == 1),
    tasa_formalidad = survey_ratio(emp_ppal == 2, na.rm = TRUE, clase2 == 1)
  )


total_general <- mydesign |> 
  filter(clase2 == 1, eda >= 15, ambito1 != 1, emp_ppal == 2) |> 
  summarise(total = survey_total()) |> 
  pull(total)

porcentaje_informalidad_ing7c <- mydesign |> 
  filter(clase2 == 1, eda >= 15, ambito1 != 1, emp_ppal == 1) |>  
  group_by(ing7c) |> 
  summarise(
    survey_total()
  )

porcentaje_formalidad_ing7c <- mydesign |> 
  filter(clase2 == 1, eda >= 15, ambito1 != 1, emp_ppal == 2) |>  
  group_by(ing7c) |> 
  summarise(
    survey_prop()
  )

ingresos_por_sector <- mydesign |> 
  filter(clase2 == 1, eda >= 15, ambito1 != 1) |>  
  group_by(interact(emp_ppal,ing7c)) |> 
  summarise(
    informales = survey_ratio(survey_total(), emp_ppal == 1),
    formales = survey_ratio(survey_total(), emp_ppal == 2)
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

# Use the function to create your labeled data frame
labeled_df <- create_labeled_df(ungroup(porcentajes_ingreso))

porcentajes_ingreso |> 
  select(-ends_with("_se")) |> 
  group_by(emp_ppal) |> 
  mutate(row_number = row_number()) |> 
  pivot_wider(names_from = row_number, values_from = c(ing7c, porcentaje_ingreso)) |> 
  ungroup() |> 
  select(-emp_ppal) |> 
  gt(row_group_as_column = TRUE) |> 
  tab_header(
    title = md("**Ingresos por sector**"),
    subtitle = md("Con base en población ocupada no agropecuaria")
  ) |> 
  fmt_percent()

