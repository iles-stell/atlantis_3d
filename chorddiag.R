library(devtools)
library(dplyr)
library(readr)
library(tidyverse)
library(reshape2)
library(circlize)
library(tidyr)

devtools::install_github('mattflor/chorddiag')
library(chorddiag)

main.path <- "EAAM29_test_model/example_model_run/ea_input_files"

# Choose output folder
out.folder <- "/eaam-output/Outputs/output_EA_biol_newdiet_29_10yrs"
out.folder <- "/eaam-output/Outputs/output_001_EA_biol_030924"
out.folder <- "/eaam-output/Outputs/output_001_EA_biol_040924"
out.folder <- "/eaam-output/Outputs/output_301024_EA_biol_001" # switch out to 003

df <- read_table(file.path(main.path, out.folder, "output_EAAM_DietCheck.txt"))
head(df)

df_w <- df %>%
  filter(Time == 1000) %>%
  dplyr::select(-Time, -Stock, -Updated, -Cohort)
head(df_w)

# Filter for Time == 1000 and select relevant columns
df_filtered <- df %>%
  filter(Time == 1000) %>%
  select(-Time, -Stock, -Updated, -Cohort)
head(df_filtered)

# Reshape data to long format with "from" and "to" columns
df_long <- df_filtered %>%
  pivot_longer(cols = -Predator, names_to = "to", values_to = "value") %>%
  rename(from = Predator) %>%
  filter(value > 0)  # Optional: keep only rows where there is an interaction (value > 0)

# Display the resulting data frame
head(df_long)

circlize::chordDiagram(df_long)
