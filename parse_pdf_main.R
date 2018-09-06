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

directory <- "scraped_pdfs/2010_world_championships/"
parse_files_for_year(directory)

directory <- "scraped_pdfs/"
list.files(directory)

tibble(year = 2010:2018) %>%
  filter(year == 2010) %>%
  mutate(year_directory = paste0("scraped_pdfs/", year, "_world_championships/"),
         data = list(parse_files_for_year(year_directory)))





#%>%
#  unite("measurement_type_distance", measurement_type, distance) %>%
#  spread(measurement_type_distance, measurement)



  
  

parse_gps(gps_file_name)
parse_c73(c73_file_name)
parse_c51a(c51a_file_name)



gps_file_name <- "scraped_pdfs/2014_world_championships/ROM012101_MGPS.pdf"
gps_file_name <- "scraped_pdfs/2010_world_championships/ROM012104_MGPS.pdf"

c73_file_name <- "scraped_pdfs/2014_world_championships/ROM012205_C73.pdf"
c73_file_name <- "scraped_pdfs/2014_world_championships/ROXT22902_C73.pdf"

c73_file_name <- "scraped_pdfs/2010_world_championships/ROM012101_C73.pdf"
c73_file_name <- "scraped_pdfs/2010_world_championships/ROM012104_C73.pdf"


c51a_file_name <- "scraped_pdfs/2010_world_championships/ROM012101_C51A.pdf"
c51a_file_name <- "scraped_pdfs/2014_world_championships/ROW042101_C51A.pdf"

c51a_file_name <- "scraped_pdfs/2014_world_championships/ROM041101_C51A.pdf"
c51a_file_name <- "scraped_pdfs/2014_world_championships/ROM012101_C51A.pdf"

