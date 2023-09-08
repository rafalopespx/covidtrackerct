########################################################

# I. PACKAGE INSTALL ####
packages = c("dplyr","tidyr","ggplot2","RColorBrewer","shiny",
             "shinythemes","plotly","DT","data.table",
             "stats","ggpubr","stringr", "vroom")

## Now load or install&load all
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

rm(packages, package.check)

library(gsheet)
library(purrr)
library(readr)
library(DescTools)
library(lubridate)
library(zoo)
library(EpiEstim)
library(ggpubr)
library(httr)
library(jsonlite)

set.seed(1234)


########################################################
##### SET RECENT DATES (CHANGE FOR WEEKLY BUILD) #######

date_gisaid = as.Date("2023-09-01") # FOR FILTERING GISAID DOWNLOAD, REFERENCE THIS DATE
date_3_months = as.Date(date_gisaid - 92) # CHANGE TO MOST RECENT 3 MONTHS FOR WHO VARIANT PLOT
date_3_weeks = as.Date(date_gisaid - 22) # CHANGE TO MOST RECENT 3 WEEKS FOR LINEAGE PLOT
date_covidestim<-"2023-05-04" # CHANGE TO MOST RECENT DATE, FIXED UNTIL COVIDESTIM IS UP AGAIN

# II. Read metadata files ########

# we want to read metadata for all Connecticut sequences, format and filter it to add WHO variant designation and limit to sequences in the past 3 months

# read master file
metadata_CT_old = read.table("data/metadata_CT_all.tsv", sep = "\t", header = TRUE) 
metadata_CT_old$`Collection.date` <- as.Date(metadata_CT_old$`Collection.date`)

# read recent gisaid submission file.
new_genomes_title<-list.files('data',pattern = "gisaid",full.names = TRUE,recursive=TRUE,include.dirs=TRUE) 
new_genomes = read.table(new_genomes_title, sep = "\t", header = TRUE)
new_genomes$`Collection.date` <- as.Date(new_genomes$`Collection.date`)
new_genomes = new_genomes %>%
  filter(Collection.date > "2021-01-01") |> 
  select(colnames(metadata_CT_old))

# add new submissions to master file
metadata_CT_all = rbind(metadata_CT_old,new_genomes)
metadata_CT_all = metadata_CT_all %>%
  filter(Lineage != "") %>%
  drop_na(Collection.date)

metadata_CT_all = subset(metadata_CT_all, !duplicated(subset(metadata_CT_all, select=c(`Accession.ID`)))) # remove duplicate sequences

write_tsv(metadata_CT_all,"data/metadata_CT_all.tsv") # write master tsv file for next week

# filter badly named lineages by GISAID
metadata_CT_all$pango_lineage <- metadata_CT_all$Lineage
metadata_CT_all["pango_lineage"][metadata_CT_all["pango_lineage"] == "BA.2.75 (marker override based on Emerging Variants AA substitutions)"] = "BA.2.75"
metadata_CT_all["pango_lineage"][metadata_CT_all["pango_lineage"] == "BF.6 (marker override based on Emerging Variants AA substitutions)"] = "BF.6"
metadata_CT_all["pango_lineage"][metadata_CT_all["pango_lineage"] == "XBB (marker override based on Emerging Variants AA substitutions)"] = "XBB"
metadata_CT_all["pango_lineage"][metadata_CT_all["pango_lineage"] == "BA.2" & metadata_CT_all["Collection.date"] > "2022-12-16"] = "XBB.1.5"

# create week column
#metadata_CT_all <- metadata_CT_all %>% mutate(CopyDate = `Collection.date`)
metadata_CT_all <- metadata_CT_all %>% group_by(`week` = cut(`Collection.date`, "week")) 
metadata_CT_all$week <- as.Date(metadata_CT_all$week)
metadata_CT_all <- metadata_CT_all[!is.na(metadata_CT_all$`week`),]

# filter metadata within last 3 months
metadata_CT_recent = metadata_CT_all[metadata_CT_all$`Collection.date` >= date_3_months,] 
metadata_CT_recent =  metadata_CT_recent[!is.na(metadata_CT_recent$`pango_lineage`),]  

# load color schemes
source("functions/who_colors.R")
source("functions/lineage_shortlist.R")

# II.1. Create WHO variant name file ######

# read lineage designation csv from pangolin github
lineages_notes<-vroom("https://raw.githubusercontent.com/cov-lineages/pango-designation/master/lineage_notes.txt")

lineages_notes<-lineages_notes |> 
  mutate(lineage_short = if_else(substr(Lineage, 1, 1) == "X",
                                 substr(Lineage, 1, 5),
                                 substr(Lineage, 1, 4)))
  # mutate(lineage_short = substr(Lineage, 1, 4))

names<-lineages_notes |> 
  select(Lineage, lineage_short) |> 
  rename(pango_lineage = Lineage,
         who_variants = lineage_short)

# names <-  read.csv("data/variant_names.csv")
metadata_CT_who <-  merge(metadata_CT_all,
                          names[,c("pango_lineage","who_variants")], 
                          by = "pango_lineage", 
                          all.x = TRUE)

# metadata_CT_who <-  metadata_CT_who %>%
#   mutate(who_variants = replace(who_variants,
#                                 is.na(metadata_CT_who$who_variants) & week >= "2022-05-05", 
#                                 "Other Omicrons")) # replace NAs to other Omicrons, may change if new variant appears

# create cumulative & most recent 3 week variant files for summary table
cumulative <- metadata_CT_who %>% 
  select(`Collection.date`,`who_variants`,`pango_lineage`) %>%
  group_by() %>% count(`who_variants`)
colnames(cumulative) = c("WHO.label","Cumulative.sequenced.cases.")
cumulative[which(is.na(cumulative$WHO.label)),1] <- "Other"

last_3_weeks <- metadata_CT_who %>% 
  select(`Collection.date`,`who_variants`,`pango_lineage`) %>%
  subset(`Collection.date` >= date_3_weeks) %>% # update date every 3 weeks
  count(`who_variants`) %>%
  mutate(freq = round(100*n/sum(n),2)) 
colnames(last_3_weeks) = c("WHO.label","Total.sequences.collected.from.past.3.weeks..","Percent.sequences.collected.from.past.3.weeks..")
last_3_weeks[which(is.na(last_3_weeks$WHO.label)),1] <- "Other"

# recategorize minor variants (in Connecticut) into Other
metadata_CT_who[which(is.na(metadata_CT_who$who_variants)),length(colnames(metadata_CT_who))] <- 'Other'
metadata_CT_who[which(metadata_CT_who$who_variants == "Eta"),length(colnames(metadata_CT_who))] <- 'Other'
metadata_CT_who[which(metadata_CT_who$who_variants == "Kappa"),length(colnames(metadata_CT_who))] <- 'Other'
metadata_CT_who[which(metadata_CT_who$who_variants == "Zeta"),length(colnames(metadata_CT_who))] <- 'Other'
metadata_CT_who[which(metadata_CT_who$who_variants == "Lambda"),length(colnames(metadata_CT_who))] <- 'Other'


# III. Daily lineage freq in last 3 months #######

# we will now filter data to create plot for SARS-CoV-2lineage frequency in Connecticut within the last 3 months

# create dummy column for lineage counting
metadata_CT_recent$`CopyDate` = as.numeric(metadata_CT_recent$`Collection.date`)

# remove most recent 3 collection dates (inadequate data)
metadata_CT_recent = metadata_CT_recent[-c(which(metadata_CT_recent$`Collection.date` == max(metadata_CT_recent$`Collection.date`))),] # omit latest 3 days due to reporting bias

# have all lineages be displayed every day, ensuring no breaks when plotting
lineages_daily_draft <- metadata_CT_recent %>% 
  complete(`Collection.date`, 
           nesting(`pango_lineage`), 
           fill = list(CopyDate = 0))

lineages_daily_draft$`CopyDate` = ifelse(lineages_daily_draft$`Collection.date` == 0,0,1) # for counting lineages

# find cumulative lineage scores & filter out minor lineages & assign colors
lineages_sum <- metadata_CT_recent %>% group_by(`pango_lineage`) %>% count(pango_lineage) 

lineages_sum = lineage_shortlist(lineages_sum, cut = TRUE)

# add html color code to lineages
n_variants<-unique(lineages_sum$ShortLin)

lineages_sum = lineages_sum %>%
  mutate(color =  case_when(
    ShortLin == "BA.1" ~ "#2b931a",
    (ShortLin == "BA.2" & n >= quantile(lineages_sum$n)[5]) ~ "#c5f367", 
    (ShortLin == "BA.2" & n < quantile(lineages_sum$n)[5] & n > quantile(lineages_sum$n)[3]) ~ "#faf994", 
    (ShortLin == "BA.2" & n < quantile(lineages_sum$n)[3] ) ~ "#f8ffda", 
    ShortLin == "BA.2.12.1" ~ "#78f0a1",
    ShortLin == "BA.2.75" ~ "#3eb480",
    ShortLin == "BA.4" ~ "#225fd6", 
    ShortLin == "BA.4.6" ~ "#76e4f5", 
    (ShortLin == "BA.5" & n >= quantile(lineages_sum$n)[5]) ~ "#f3152f", 
    (ShortLin == "BA.5" & n < quantile(lineages_sum$n)[5] & n > quantile(lineages_sum$n)[4]) ~ "#fcaa99",
    (ShortLin == "BA.2" & n < quantile(lineages_sum$n)[4] & n > quantile(lineages_sum$n)[3]) ~ "#faf994", 
    (ShortLin == "BA.5" & n < quantile(lineages_sum$n)[3] ) ~ "#ffe4ee", 
    (ShortLin == "XBB" ~ "#333333"),
    (ShortLin == "EG" ~ "#00a4b9"),
    TRUE ~ "#fef9f3"
  )
  )
row.names(lineages_sum) = lineages_sum$pango_lineage


# reclassify lineages, placing minor lineages as Other
lineages_daily_draft$`pango_lineage` = ifelse(lineages_daily_draft$`pango_lineage`%in% rownames(lineages_sum),lineages_daily_draft$`pango_lineage`,"Other")

lineages_daily <- lineages_daily_draft %>% group_by(`Collection.date`,`pango_lineage`) %>% 
  summarise_at(vars(`CopyDate`),list(n = sum)) %>%
  mutate(freq = round(100*n/sum(n),2)) 

# insert 3-day rolling average
lineages_daily = lineages_daily %>%
  group_by(`pango_lineage`) %>%
  mutate(freq = as.numeric(freq)) %>%
  mutate(movavg = round(rollmean(freq, 3, na.pad = TRUE),2)) 

# sort by cumulative counts
lineages_daily_sorted = lineages_daily[order(-lineages_daily$freq),]

# add shortened lineage names
lineages_daily_sorted = lineage_shortlist(lineages_daily_sorted, cut = TRUE)
rm(lineages_daily_draft)

# write csv file of lineage frequency in last 3 months
write.csv(lineages_daily_sorted,"outputs/SARS-CoV-2 lineage frequency in Connecticut in the last 3 months.csv")


# IV. Weekly variant freq since Jan 2021 ###########

# We will create a SARS-CoV-2 WHO variant frequency plot for Connecticut after January 2021

who_weekly <- metadata_CT_who %>% 
  group_by(`week`) %>% 
  count(who_variants) %>%
  mutate(freq = round(n/sum(n),2)) 

# filter out sequences before 2021
who_weekly = who_weekly[c(which(who_weekly$week > '2020-12-31')),] 

# omit latest week (inadequate data)
who_weekly = who_weekly[-c(which(who_weekly$week == max(who_weekly$week))),] 
who_weekly$`Variant names` <- factor(who_weekly$who_variants)

# write csv file of WHO variant frequency in Connecticut after Jan 2021
write.csv(who_weekly,"outputs/SARS-CoV-2 variant frequency in Connecticut.csv")


# V. Summary table ########

# Table of the composition of SARS-CoV-2 sequence submissions (to GISAID) in Connecticut

# read last week's summary table
table1_old = read.csv("data/summary_table.csv")
table1_old = table1_old %>%
  select(-`X`)

# merge old summary table with cumulative & recent variant data
tab = merge(table1_old,cumulative[,c("WHO.label","Cumulative.sequenced.cases.")], by = "WHO.label", all.x = TRUE)
table1_new = merge(tab,last_3_weeks[,c("WHO.label","Total.sequences.collected.from.past.3.weeks..","Percent.sequences.collected.from.past.3.weeks..")], by = "WHO.label", all.x = TRUE)
table1_new[is.na(table1_new)] <- 0 # assign 0 to NA values
table1_new_draft <- table1_new 

# y is new report, x is old
# find changes since last week's report
table1_new$`Percent.change.from.previous.report.y` = round((table1_new_draft$`Percent.sequences.collected.from.past.3.weeks...y` - table1_new_draft$`Percent.sequences.collected.from.past.3.weeks...x`),2)
table1_new = table1_new[,-c(4:7)] # remove old entries
table1_new[which(table1_new$`WHO label` == 0),1] <- "Other"
colnames(table1_new) = c("WHO label",
                         "Pango lineage",
                         "CDC classification",
                         "Cumulative sequenced cases*",
                         "Total sequences collected from past 3 weeks**",
                         "Percent sequences collected from past 3 weeks**",
                         "Percent change from previous report")
# write csv for summary table
write.csv(table1_new,"data/summary_table_new.csv")
write.csv(table1_new,"outputs/Summary table of SARS-CoV-2 variant frequency.csv")

recent_variants_count = length(which(table1_new$`Total sequences collected from past 3 weeks**` != 0)) # find non-zero variants in past 3 weeks

# VI. Sequence proportion ######

# Create plot tracking SARS-CoV-2 weekly sequence proportion compared to case count in Connecticut 

# read cumulative case file
cases = fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv", sep = ",")

# filter for Connecticut & remove columns
cases_CT_t = cases %>%
  filter(Province_State == "Connecticut") %>%
  select(-c(UID,iso2,iso3,code3,FIPS,Admin2,Country_Region,Lat,Long_,Combined_Key,Province_State)) 
cases_CT = tibble(apply(cases_CT_t,2,sum),mdy(colnames(cases_CT_t)))
colnames(cases_CT) = c("case_cumulative","date")


# read cumulative case file
cases = fread("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv", sep = ",")
cases_CT = cases %>%
  filter(state == "Connecticut") 

# find sequence counts
seq_CT = metadata_CT_all %>%
  group_by(`week`,`Collection.date`) %>%
  count()

# create data frame for plotting
source("functions/perc_seq.R")
seq_prop = perc_seq(cases_CT,seq_CT)

gc()



# write sequence proportion csv file
write.csv(seq_prop,"outputs/SARS-CoV-2 case count and sequenced proportion in Connecticut.csv")

# manually create y-axis for sequence proportion plot (we will need 2 y-axis)
ay2 <- list(
  tickfont = list(size=11, color = "orange"),
  titlefont=list(size=14.6, color = "orange"),
  min = 0,
  max = 100,
  overlaying = "y",
  nticks = 5,
  side = "right",
  title = "proportion of cases sequenced"
)

ay1 <- list(
  tickfont = list(size=11, color = "blue"),
  titlefont=list(size=14.6, color = "blue"),
  min = 0,
  max = 100,
  nticks = 5,
  side = "left",
  title = "case count"
)


# VII. Ct Values by Lineages #############

# Create plot to compare PCR cycle threshold (Ct value) of SARS-CoV-2 samples that were sequenced and submitted by Grubaugh Lab

# glab = read.csv("data/GLab_SC2_sequencing_data - Sample metadata.csv")
glab = if(requireNamespace('readr', quietly=TRUE)){
  library(readr)
  read_csv(construct_download_url("https://docs.google.com/spreadsheets/d/1dUm-OtDOxvbS9OdCfnBrjBF9H5AKphauzzWxY-h4oUQ/edit#gid=1740944573"), col_types = cols(
    mpg = col_double(),
    cyl = col_integer(),
    disp = col_double(),
    hp = col_integer(),
    drat = col_double(),
    wt = col_double(),
    qsec = col_double(),
    vs = col_integer(),
    am = col_integer(),
    gear = col_integer(),
    carb = col_integer()
  ))
}

glab = glab %>%
  mutate(
    Sample.ID = `Sample-ID`,
    Report.CT = `Report CT`,
    Yale.N1.FAM. = `Yale-N1(FAM)`,
    pango_lineage = Lineage
  )
gc()

glab = lineage_shortlist(glab, cut = TRUE)
glab <- glab %>% mutate(Ctvals = ShortLin) 

Ct_values <- glab %>% dplyr::filter(`Ctvals` %in% c("BA.2","BA.5","XBB"))
rm(glab)
gc()


# VIII. Rt Values by Variant #############

# Calculate and plot effective reproduction number (Rt) for SARS-CoV-2 variant in Connecticut, based on serial interval estimation

# Making API request using the GET() function and specifying the API’s URL:
url = paste("api2.covidestim.org/runs?geo_name=eq.Connecticut&run_date=gt.",date_covidestim,"&select=*%2Ctimeseries(*)",sep = "")
res = GET(url)

# convert the raw Unicode into a character vector that resembles the JSON format to obtain case data
data = fromJSON(rawToChar(res$content))
infect_import = data[[8]][[1]]
rm(data,res,url)

# filter columns for use
infect_import$run_id = "Connecticut"
colnames(infect_import)[1] <- "state"
infect_data = infect_import %>%
  select(state,
         date,
         infections,
         infections_p2_5,
         infections_p97_5) %>%
  #*****************************************
  rename(week = date) %>% #rename for merging with var_data
  mutate(week = as.Date(week))

# variant frequency data (who_weekly obtained in shinyapp pipeline)
var_data = who_weekly %>%
  select(-c(`Variant names`)) %>%
  mutate(week = week +3) %>% # create week to match 
  filter(!(who_variants == "Omicron (BA.2)" & week >= "2022-08-15")) # remove one-off BA.2 infections

var_data = data.frame(var_data)
var_data = reshape(var_data, idvar = "week", timevar = "who_variants", direction = "wide")
var_data[is.na(var_data)] <- 0
var_data$state = "Connecticut"

#*******************************************************************************
#####MERGE DATA#####
#*#*******************************************************************************

# load function to prep your dataset into correct format for Rt calculation
source("functions/prepare_rt.R")
var_merge3 <-  prepare_rt(var_data,
                          infect_data)

#creates list of dataframes by variant for calculating estimate_R
var_list <-  var_merge3[[1]] %>%
  group_by(variant) %>%
  arrange(variant) %>%
  group_split()


var_name <-  unique(var_merge3[[1]]$variant)

gc()

######## IX. TOTAL SEQUENCES ########
# estimate total sequences by Grubaugh Lab, may differ from submitted sequences on GISAID database

old_total = read.csv("data/total_sequences.csv")
old_total = old_total %>%
  select(-c(X))
# mutate(date = mdy(date))

total_sequences = old_total$total[nrow(old_total)] + nrow(new_genomes[grep("Yale", new_genomes$Virus.name),]) # recalculate total

today = as.character(today())
updated_total = c(today,total_sequences)

new_total = old_total %>%
  rbind(updated_total) %>%
  mutate(date = as.Date(date)) 
new_total = new_total %>%
  subset(!duplicated(subset(new_total, select=c(date)))) # delete duplicated rows by date (in case of multiple test runs)

write.csv(new_total,"data/total_sequences.csv")

new_total_fixed = new_total
colnames(new_total_fixed) = c("Date of website update","Total number of sequences")
write.csv(new_total_fixed,"outputs/Total number of SARS-CoV-2 sequences by Grubaugh Lab")
