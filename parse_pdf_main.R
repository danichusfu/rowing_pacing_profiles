#####################
##### PACKAGES ######
#####################
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

#####################
#### FIND IF WE CAN CALL A RUN TO SHELL BASH FILE FROM R ####
#####################


#####################
#### DELETE FILE ####
#####################
# delete file as it is a broken file both online and locally
# doing it in code so we can be more reproducible
unlink("scraped_pdfs/2015_world_championships/ROMA12203_C73.pdf")

#####################
#### FUNCTIONS ######
#####################
source('parse_pdf_functions.R')

# in case you want to do it in parallel
num_cores <- detectCores()

#####################
#### CREATE DF ######
#####################
championship_by_year <- create_df_of_race_pdfs_by_year(num_cores)

#####################
# DO IT IN PARALLEL #
#####################
# Cut done time from 60 to 25 minutes, refers to reference above
cluster <- create_cluster(cores = num_cores)

chip_by_year_partitioned <- 
  championship_by_year %>%
  partition(core_group, cluster = cluster) 

chip_by_year_partitioned %>% load_library_functions_to_clusters()

# Takes roughly 24.23 when done in parallel
all_years_parsed <- 
  chip_by_year_partitioned %>%
  mutate(data = map(year_directory, parse_files_for_year)) %>%
  collect() %>% # Special collect() function to recombine partitions
  as_tibble() 

# Takes roughly 57.98 minutes when not done in parallel
# all_years_parsed <- 
#     championship_by_year %>%
#     mutate(data = map(year_directory, parse_files_for_year))

#####################
#### WIDEN DATA #####
#####################
all_years_wide <-
  all_years_parsed %>%
  ungroup() %>%
  select(-core_group) %>%
  arrange(year) %>%
  unnest() %>%
  make_boats_race_the_observation()

#####################
### AUGMENT DATA ####
#####################
all_years_augmented <-
  all_years_wide %>%
  augment_races()


all_years_augmented %>%
  mutate_if(is.character, as.factor) %>%
  summary()



# for debugging
# 
# directory <- "scraped_pdfs/2017_world_championships/"
#
# parse_files_for_year(directory)
# parse_gps(gps_file_name)
# parse_c73(c73_file_name)
# parse_c51a(c51a_file_name)
# 
# 
# 
# gps_file_name <- "scraped_pdfs/2017_world_championships/ROM022101_MGPS.pdf"
# 
# c73_file_name <- "scraped_pdfs/2017_world_championships/ROM022101_C73.pdf"
# 
# c51a_file_name <- "scraped_pdfs/2017_world_championships/ROM022101_C51A.pdf"
