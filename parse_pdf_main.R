# for rJava package 
# https://cimentadaj.github.io/blog/2018-05-25-installing-rjava-on-windows-10/installing-rjava-on-windows-10/
# Sys.setenv(JAVA_HOME="C:/Program Files/Java/jdk-10.0.2/")

# for rJava package help on Windows
# https://cimentadaj.github.io/blog/2018-05-25-installing-rjava-on-windows-10/installing-rjava-on-windows-10/

# for tabulizer package help
# https://github.com/ropensci/tabulizer#installing-java-on-windows-with-chocolatey 

# If pacman is not installed, install it
# install.packages('pacman')
pacman::p_load(tidyverse, glue, lubridate, pdftools, devtools, remotes, rJava, tabulizer)

# for parallelizing
# http://www.business-science.io/code-tools/2016/12/18/multidplyr.html
# devtools::install_github("hadley/multidplyr")
library(multidplyr)
pacman::p_load(parallel)

# LOAD FUNCTION
source('parse_pdf_functions.R')

unlink("scraped_pdfs/2015_world_championships/ROMA12203_C73.pdf")

#### PARALIZE
num_cores <- detectCores()
cluster <- create_cluster(cores = num_cores)

championship_by_year <- 
  tibble(year = 2010:2017) %>%
  mutate(year_directory = paste0("scraped_pdfs/", year, "_world_championships/"),
         approx_num_races = map(year_directory, ~ length(list.files(.x))/3)) %>%
  unnest() %>%
  mutate(rank = row_number(row_number(approx_num_races))) %>%
  arrange(-rank) %>%
  mutate(rank_rev  = row_number(),
         rank_comb = abs(rank - rank_rev),
         rank_alt  = dense_rank(rank_comb),
         core_group = rank_alt %% num_cores + 1) %>%
  select(year, year_directory, core_group)

chip_by_year_partitioned <- 
  championship_by_year %>%
  partition(core_group, cluster = cluster)

chip_by_year_partitioned %>%
  # Assign libraries
  cluster_library("tidyverse") %>%
  cluster_library("glue") %>%
  cluster_library("lubridate") %>%
  cluster_library("rJava") %>%
  cluster_library("tabulizer") %>%
  cluster_library("pdftools") %>%
  # Assign values (use this to load functions or data to each core)
  cluster_assign_value("extract_gps_data", extract_gps_data) %>%
  cluster_assign_value("extract_race_information_gps", extract_race_information_gps) %>%
  cluster_assign_value("parse_c51a", parse_c51a) %>%
  cluster_assign_value("parse_c73", parse_c73) %>%
  cluster_assign_value("parse_files_for_year", parse_files_for_year) %>%
  cluster_assign_value("parse_gps", parse_gps) %>%
  cluster_assign_value("separate_name_birthday_cols", separate_name_birthday_cols)


# Takes roughly 24.23 when done in parallel
all_years_parsed <- 
  chip_by_year_partitioned %>%
  mutate(data = map(year_directory, parse_files_for_year)) %>%
  collect() %>% # Special collect() function to recombine partitions
  as_tibble() 



all_years_wide <-
  all_years_parsed %>%
  ungroup() %>%
  select(-core_group) %>%
  arrange(year) %>%
  unnest() %>%
  make_boats_race_the_observation()


all_years_augmented <-
  all_years_wide %>%
  augment_races()



all_years_augmented %>%
  count(event_cateogry_abbreviation, weight_class, gender, size, discipline,
        coxswain, adaptive, adapt_desig, junior) %>%
  View

all_years_augmented %>%
  count(round_type) %>%
  View




# Takes roughly 57.98 minutes when not done in parallel
# all_years_parsed <- 
#     championship_by_year %>%
#     mutate(data = map(year_directory, parse_files_for_year))

# for debugging
# 
# directory <- "scraped_pdfs/2010_world_championships/"
#
# parse_files_for_year(directory)
# parse_gps(gps_file_name)
# parse_c73(c73_file_name)
# parse_c51a(c51a_file_name)
# 
# 
# 
# gps_file_name <- "scraped_pdfs/2017_world_championships/ROXR43P01_MGPS.pdf"
# 
# c73_file_name <- "scraped_pdfs/2017_world_championships/ROM012206_C73.pdf"
# 
# c51a_file_name <- "scraped_pdfs/2017_world_championships/ROM012101_C51A.pdf"
