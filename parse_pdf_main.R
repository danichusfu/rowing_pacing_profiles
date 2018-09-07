# If pacman is not installed, install it
# install.packages('pacman')

# for rJava package 
# https://cimentadaj.github.io/blog/2018-05-25-installing-rjava-on-windows-10/installing-rjava-on-windows-10/
# Sys.setenv(JAVA_HOME="C:/Program Files/Java/jdk-10.0.2/")

# for rJava package help on Windows
# https://cimentadaj.github.io/blog/2018-05-25-installing-rjava-on-windows-10/installing-rjava-on-windows-10/

# for tabulizer package help
# https://github.com/ropensci/tabulizer#installing-java-on-windows-with-chocolatey 


pacman::p_load(tidyverse, glue, lubridate, pdftools, devtools, remotes, rJava, tabulizer)

# for parallelizing
# http://www.business-science.io/code-tools/2016/12/18/multidplyr.html
devtools::install_github("hadley/multidplyr")
library(multidplyr)
pacman::p_load(parallel)
source('parse_pdf_functions.R')

directory <- "scraped_pdfs/2011_world_championships/"
file_name <- list.files(directory)
files     <- tibble(file_name)
files_nested <- 
  files %>% 
  mutate(race_id   = str_extract(file_name, ".+(?=_)"),
         file_type = str_extract(file_name, "(?<=_).+(?=.pdf)"),
         file_path = paste0(directory, file_name)
  ) %>%
  select(-file_name) %>%
  spread(file_type, file_path) %>%
  rename_all(tolower) %>%
  drop_na(mgps, c73, c51a) %>%
  nest(-race_id)
files_parsed <- files_nested %>%
  mutate(c51a_parsed = map(data, ~ parse_c51a(.$c51a)))
   

files_parsed %>% select(-data) %>% unnest() %>% summary()
files_parsed %>% select(-data) %>% unnest() %>% filter_all(any_vars(is.na(.))) 
#parse_files_for_year(directory)

#directory <- "scraped_pdfs/"

#%>%
#  unite("measurement_type_distance", measurement_type, distance) %>%
#  spread(measurement_type_distance, measurement)

2011
2012
2013


parse_gps(gps_file_name)
parse_c73(c73_file_name)
parse_c51a(c51a_file_name)



gps_file_name <- "scraped_pdfs/2014_world_championships/ROM012101_MGPS.pdf"

c73_file_name <- "scraped_pdfs/2016_world_championships/ROM112303_C73.pdf"

c51a_file_name <- "scraped_pdfs/2010_world_championships/ROM012101_C51A.pdf"


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
  # Assign values (use this to load functions or data to each core)
  cluster_assign_value("extract_gps_data", extract_gps_data) %>%
  cluster_assign_value("extract_race_information_gps", extract_race_information_gps) %>%
  cluster_assign_value("parse_c51a", parse_c51a) %>%
  cluster_assign_value("parse_c73", parse_c73) %>%
  cluster_assign_value("parse_files_for_year", parse_files_for_year) %>%
  cluster_assign_value("parse_gps", parse_gps) %>%
  cluster_assign_value("separate_name_birthday_cols", separate_name_birthday_cols)


start <- proc.time() # Start clock
all_years_parsed <- 
  chip_by_year_partitioned %>%
  filter(year %in% c(2010, 2014)) %>%
  mutate(data = list(parse_files_for_year(year_directory))) %>%
  collect() %>% # Special collect() function to recombine partitions
  as_tibble() 
time_elapsed_series <- proc.time() - start # End clock

start <- proc.time() # Start clock
all_years_parsed <- 
  championship_by_year %>%
  filter(year %in% 2010:2011) %>%
  mutate(data = list(parse_files_for_year(year_directory)))
time_elapsed_series <- proc.time() - start # End clock
