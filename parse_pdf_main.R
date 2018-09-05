# If pacman is not installed, install it
# install.packages('pacman')

# for rJava package 
# https://cimentadaj.github.io/blog/2018-05-25-installing-rjava-on-windows-10/installing-rjava-on-windows-10/
# Sys.setenv(JAVA_HOME="C:/Program Files/Java/jdk-10.0.2/")

# for rJava package help on Windows
# https://cimentadaj.github.io/blog/2018-05-25-installing-rjava-on-windows-10/installing-rjava-on-windows-10/

# for tabulizer package help
# https://github.com/ropensci/tabulizer#installing-java-on-windows-with-chocolatey 


pacman::p_load(tidyverse, glue, lubridate, devtools, remotes, rJava, tabulizer)
source('parse_pdf_functions.R')

directory <- "scraped_pdfs/2014_world_championships/"
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

files_nested %>%
  mutate(c51a_parsed = map(data, ~ parse_c51a(.$c51a)))


parse_gps(gps_file_name)
parse_c73(c73_file_name)
parse_c51a(c51a_file_name)



gps_file_name <- "scraped_pdfs/2014_world_championships/ROM012101_MGPS.pdf"
c73_file_name <- "scraped_pdfs/2014_world_championships/ROM012101_C73.pdf"
c73_file_name <- "scraped_pdfs/2014_world_championships/ROXT22902_C73.pdf"
c51a_file_name <- "scraped_pdfs/2014_world_championships/ROM012101_C51A.pdf"
c51a_file_name <- "scraped_pdfs/2014_world_championships/ROXT22902_C51A.pdf"
c51a_file_name <- "scraped_pdfs/2014_world_championships/ROM041101_C51A.pdf"
c51a_file_name <- "scraped_pdfs/2014_world_championships/ROW042102_C51A.pdf"

