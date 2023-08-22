rm(list = ls())
gc()

# Loading libraries ###
library(tidyverse)
library(vroom)

# today date
date_today<-as.Date(today(), "%Y_%m_%d")

# Reading ###
metadata <- vroom("data/metadata_2023-08-20_01-32.tsv.gz")

# Processing metadata ###
metadata_us <- metadata |> 
  ## Filtering for US only sequences first, and CT only sequences, and Human host
  filter(country == "USA", 
         division == "Connecticut", 
         host == "Human")

rm(metadata)

# Mutating to have the necessary columns ###
metadata_us <- metadata_us |> 
  rename(`Collection.date` = date)

# Write the metadata
vroom_write(x = metadata_us, 
            file = paste0("data/gisaid_hcov-19_",date_today,".tsv.xz"))
