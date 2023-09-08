rm(list = ls())
gc()

# Loading libraries ###
library(tidyverse)
library(vroom)
library(data.table)

# today date
date_today<-as.Date(today(), "%Y_%m_%d")

# Reading ###
metadata <- fread("~/Downloads/metadata_2023-08-29_01-36.tsv.gz")

# Processing metadata ###
metadata_ct <- metadata |> 
  ## Filtering for US only sequences first, and CT only sequences, and Human host
  filter(country == "USA", 
         division == "Connecticut") |> 
  mutate(date = as.Date(ymd(date))) |> 
  filter(date >= "2022-01-01",
         date <= "2023-01-01")

# rm(metadata)

# Write the metadata
vroom_write(x = metadata_ct, 
            file = paste0("data/metadata_gisaid_ct_2023_09_04.tsv.xz"))
