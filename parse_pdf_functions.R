# extract race information from GPS pdf
extract_race_information_gps <- function(gps_file_name){
  
  # list of common lines of text in the gps pdf
  # to remove before extracting information
  exclude_list <- 
    c("INTERNET Service:", 
      "Report Created", 
      "FISA Data Service", 
      "(Event)", 
      "Dist\\.", 
      "\\[m\\]",
      "Speed",
      "\\[m\\/s\\]",
      "Page",
      "RACE DATA")
  # make it a regex for str_detect
  exclude_list <- glue_collapse(exclude_list, "|")
  
  
  race_info <-
    extract_text(gps_file_name) %>%
    str_split('\r\n') %>%
    unlist() %>%
    as_tibble() %>%
    # remove the actual GPS data as we have it from the other function call
    filter(str_detect(value, "[[:alpha:]]")) %>%
    # remove the rows without important information
    filter(!str_detect(value, exclude_list)) %>%
    drop_na(value) %>%
    pull(value)
  
  
  teams <- 
    race_info[7] %>%
    str_split(' ') %>% 
    unlist()
  teams_lane <-
    teams %>%
    paste(1:length(teams), sep = "_") %>% 
    rep(each = 2) 
  vars <- c("speed", "strokes")
  col_names <- c("distance", paste(teams_lane, vars, sep = "_"))
  
  
  
  event_category                <- race_info[1]
  race_date                     <- race_info[2] %>% word(2, -1) %>% dmy()
  championship_name_description <- race_info[3] 
  event_num                     <- race_info[4] %>% word(1)
  event_cateogry_abbreviation   <- race_info[4] %>% word(2, -1)
  round                         <- race_info[5]
  race_number                   <- race_info[6] %>% parse_number()
  
  race_info_final <- 
    tibble(championship_name_description, 
           race_date, 
           event_category, 
           event_cateogry_abbreviation, 
           event_num, 
           round, 
           race_number)
  
  return(tibble(race_info_final = list(race_info_final),
                col_names       = list(col_names)))
}

extract_gps_data <- function(gps_file_name, col_names){
  num_cols <-length(col_names)
  race_table <- extract_tables(gps_file_name, method = "lattice")
  if(length(race_table) == 0){
    return(tibble(distance = double(),
                  team = character(), 
                  lane_pre_2017 = double(), 
                  measurement_type = character(), 
                  measurement = double()
          )
    )
  }
  
  speed_strokes <- 
    race_table %>%
    do.call(rbind, .) %>%
    as_tibble() %>%
    filter_all(any_vars(str_detect(., "\\d"))) %>%
    select(1:num_cols) %>%
    rename_all(~ col_names) %>%
    gather(team_lane_measurement_type, measurement, -distance) %>%
    separate(team_lane_measurement_type, into = c("team", "lane_pre_2017", "measurement_type"), sep = "_")
  return(speed_strokes)
}

parse_gps <- function(gps_file_name){
  race_info <- extract_race_information_gps(gps_file_name)
  col_names <- race_info %>% pull(col_names) %>% unlist()
  race_info <- race_info %>% select(race_info_final) %>% unnest()
  gps_data  <- tibble(gps_data = list(extract_gps_data(gps_file_name, col_names)))
  gps_parsed <- bind_cols(race_info, gps_data)
  gps_parsed <- 
    gps_parsed %>% 
    unnest() %>%
    mutate_at(vars(c("lane_pre_2017", "event_num", "distance", "measurement")), parse_number)
  
  return(gps_parsed)
}


# get lane, team, position and birthdays
parse_c51a <- function(c51a_file_name){
  
  # if file name is na return empty tibble 
  if(is.na(c51a_file_name)){
    return(tibble(lane = double(), 
                  team = character(), 
                  position = character(), 
                  name = character(), 
                  birthday = date()
                  )
           )
  }
  
  possible_positions <-
    c("\\d",
      "s",
      "b",
      "c"
    )
  possible_positions_reg_ex <- paste0("^\\(", possible_positions, "\\)(?= )") %>% glue_collapse("|")
  # regular expressions for filtering relevant information
  lane_team_reg_ex          <- "^\\d{1} [[:alpha:]]{3} "
  lane_extract_reg_ex       <- "^\\d{1}"
  team_extract_reg_ex       <- "(?<=^\\d{1} )[[:alpha:]]{3}"
  position_extract_reg_ex   <- paste0("\\(", possible_positions, "\\)(?= )") %>% glue_collapse("|")
  name_extract_reg_ex       <- paste0("f[^\\d]+(?= \\d{2})")
  bday_extract_reg_ex       <- "\\d{2} [[:alpha:]]{3} \\d{4}$"
  
  c51a_info <-
    pdf_text(c51a_file_name) %>%
    str_split('\r\n') %>% 
    unlist() %>%
    str_squish() %>%
    as_tibble() %>%
    filter(str_detect(value, lane_team_reg_ex) |
           str_detect(value, possible_positions_reg_ex)) %>%
    filter(!str_detect(value, "bow|stroke|seat|cox"))
 
  
  start_list_parsed <-
    c51a_info %>%
    mutate(lane     = str_extract(value, lane_extract_reg_ex) %>% parse_number(),
           team     = str_extract(value, team_extract_reg_ex),
           position = str_extract(value, position_extract_reg_ex),
           # sometimes missing
           birthday = str_extract(value, bday_extract_reg_ex) %>% dmy(),
           # to help find name since sometimes position is missing for single boats
           # then for bigger boats the secondary names do not have team or lane
           # so remove them as well
           name     = str_remove(value, position_extract_reg_ex) %>%
                      str_remove(team_extract_reg_ex) %>%
                      str_remove(lane_extract_reg_ex) %>%
                      str_remove(bday_extract_reg_ex) %>%
                      str_squish()) %>%
    select(-value) %>%
    fill(lane, team)
    

    return(start_list_parsed)
}


# Goal of this function is to parse
# the Results c73 pdf
# and return the rank, lane, country and progression of each boat.
parse_c73 <- function(c73_file_name){
  
  if(is.na(c73_file_name)){
    return(tibble(rank_final  = double(), 
                  lane        = double(), 
                  team        = character(), 
                  progression = character()
    )
    )
  }
  
  possible_progressions <-
    c("\\(\\d{1,2}\\)",
      "FA",
      "FB",
      "FC",
      "FD",
      "FE",
      "FF",
      "FG",
      "FH",
      "R",
      "ELM",
      "SA/B",
      "SC/D",
      "SE/F",
      "S[[:upper:]]///[[:upper:]]",
      "Q",
      "F",
      "(1) WB",
      "Q WB"
    )
  
  possible_progressions_reg_ex <- paste0("(?<= )", possible_progressions, "($| WB$)") %>% glue_collapse("|")
  # sometimes rank is missing if they did not start
  lane_team_reg_ex  <- "\\d [[:upper:]]{3}"
  split_time_reg_ex <- "\\d\\:\\d{2}\\.\\d{2}"

  c73_info <-
    pdf_text(c73_file_name) %>% 
    str_split('\r\n')%>% 
    unlist() %>% 
    str_squish() %>%
    as_tibble() %>%
    filter(row_number() > 7) %>%
    filter(!str_detect(value, "Report Created|RESULTS|Race")) %>%
    filter(str_detect(value, lane_team_reg_ex) & str_detect(value, split_time_reg_ex))
  
  results_parsed <-
    c73_info %>%
    mutate( # sometimes no final rank is given (it can be assumed from the progression)
           rank_final  = str_extract(value, "^\\d(?= \\d [[:upper:]]{3})") %>% parse_number(),
           lane        = str_extract(value, "\\d(?= [[:upper:]]{3})") %>% parse_number(),
           team        = str_extract(value, "(?<=\\d )[[:upper:]]{3}"),
           progression = str_extract(value, possible_progressions_reg_ex),
           dns         = str_detect(value, " DNS"),
           exc         = str_detect(value, " EXC"),
           dnf         = str_detect(value, " DNF"),
           split_1_time = str_extract(value, split_time_reg_ex),
           value        = str_remove(value, split_time_reg_ex),
           split_2_time = str_extract(value, split_time_reg_ex),
           value        = str_remove(value, split_time_reg_ex),
           split_3_time = str_extract(value, split_time_reg_ex),
           value        = str_remove(value, split_time_reg_ex),
           split_4_time = str_extract(value, split_time_reg_ex)) %>%
    mutate_at(vars(matches("time")), ~ ms(., quiet = T) %>% seconds() %>% parse_number()) %>%
    select(-value) 
  
  return(results_parsed)
}


separate_name_birthday_cols <- function(data){
  data <-
    data %>%
    #separate(single, c("single_birthday", "single_name"), sep = "_") %>%
    separate(second, c("second_birthday", "second_name"), sep = "_") %>% 
    separate(third, c("third_birthday", "third_name"), sep = "_") %>%
    separate(fourth, c("fourth_birthday", "fourth_name"), sep = "_") %>% 
    separate(fifth, c("fifth_birthday", "fifth_name"), sep = "_") %>%
    separate(sixth, c("sixth_birthday", "sixth_name"), sep = "_") %>% 
    separate(seventh, c("seventh_birthday", "seventh_name"), sep = "_") %>%
    separate(stroke, c("stroke_birthday", "stroke_name"), sep = "_") %>% 
    separate(bow, c("bow_birthday", "bow_name"), sep = "_") %>%
    separate(coxswain, c("coxswain_birthday", "coxswain_name"), sep = "_") 
  return(data)
}

parse_files_for_year <- function(directory){
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
  
  files_parsed <-
    files_nested %>%
    mutate(c51a_parsed = map(data, ~ parse_c51a(.$c51a)),
           c73_parsed  = map(data, ~ parse_c73(.$c73)),
           gps_parsed  = map(data, ~ parse_gps(.$mgps)))

  # for debugging purposes
   # files_parsed <- files_nested %>% mutate(c51a_parsed = map(data, ~ possibly(parse_c51a, otherwise = NA_real_)(.$c51a)))
   # files_parsed %>% filter(is.na(c51a_parsed))
   # files_parsed <- files_nested %>% mutate(c73_parsed = map(data, ~ possibly(parse_c73, otherwise = NA_real_)(.$c73)))
   # files_parsed %>% filter(is.na(c73_parsed))
   # files_parsed <- files_nested %>% mutate(gps_parsed  = map(data, ~ possibly(parse_gps, otherwise = NA_real_)(.$mgps)))
   # files_parsed %>% filter(is.na(gps_parsed))
  
  files_joined <-
    files_parsed %>%
    mutate(data_joined = map2(c51a_parsed, c73_parsed, ~ full_join(.x, .y, by = c("team", "lane"))),
           # in 2017 they stopped listing boats in their lane order in the gps tables
           # as far as i can tell from looking at tables like this http://www.worldrowing.com/assets/pdfs/WCH_2017/RO0000000_C30A.pdf
           # only one entry into each category by country
           # so there should never be two of the same country in the race
           data_joined = map2(data_joined, gps_parsed, ~ full_join(.x, .y, by = c("team"))))
  
  files_cleaned <-
    files_joined %>%
    select(race_id, data_joined) %>%
    unnest() %>%
    mutate(position = case_when(position == "(2)" ~ "second",
                                position == "(3)" ~ "third",
                                position == "(4)" ~ "fourth",
                                position == "(5)" ~ "fifth",
                                position == "(6)" ~ "sixth",
                                position == "(7)" ~ "seventh",
                                position == "(s)" ~ "stroke",
                                position == "(b)" ~ "bow",
                                position == "(c)" ~ "coxswain",
                                position == "single" ~ "single"))
  
  return(files_cleaned)
  
}


augment_races <- function(data){
  data <- 
    data %>%
    # https://en.wikipedia.org/wiki/Rowing_(sport)#Terminology_and_event_nomenclature
    mutate(weight_class  = if_else(str_detect(event_cateogry_abbreviation, "L"), "light", "open"),
           gender        = case_when(str_detect(event_cateogry_abbreviation, "W")   ~ "women",
                                     str_detect(event_cateogry_abbreviation, "Mix") ~ "mixed",
                                     TRUE                                           ~ "men"),
           size          = str_extract(event_cateogry_abbreviation, "(?<=L|W|Mix|M)\\d") %>% parse_number(),
           discipline    = if_else(str_detect(event_cateogry_abbreviation, "x$"), "scull", "sweep"),
           coxswain      = if_else(str_detect(event_cateogry_abbreviation, "\\+$"), "coxed", "coxless"),
           # TA Trunks and Arms
           # PR used to be AS used to be A, Arms and Shoulders
           # ID intellectually disabled
           # Legs Trunks and Arms
           # https://en.wikipedia.org/wiki/Adaptive_rowing
           adaptive      = if_else(str_detect(event_cateogry_abbreviation, "^PR|^AS|^A|^ID|^TA|^LTA"), T, F),
           adapt_desig   = case_when(str_detect(event_cateogry_abbreviation, "^PR1|^AS|^A") ~ "arm_shoulder",
                                     str_detect(event_cateogry_abbreviation, "^ID")         ~ "intel_disab",
                                     str_detect(event_cateogry_abbreviation, "^PR2|^TA")    ~ "trunk_arm",
                                     str_detect(event_cateogry_abbreviation, "^PR3|^LTA")   ~ "leg_trunk_arm",
                                     TRUE                                                   ~ "not_adapt"),
           junior        = if_else(str_detect(event_cateogry_abbreviation, "^J"), "junior", "senior"),
           round_type    = case_when(str_detect(round, "F") ~ "final",
                                     str_detect(round, "H") ~ "heat",
                                     str_detect(round, "Q") ~ "quarterfinal",
                                     str_detect(round, "R") ~ "repecharge",
                                     str_detect(round, "S") ~ "semifinal",
                                     str_detect(round, "X") ~ "exhibition"),
           heat_or_final = if_else(round_type == "final", "final", "heat"))
  return(data)
}


make_boats_race_the_observation <- function(data){
  data <- 
    data %>%
    # not all years have measurement severy 25m or every 10m like in some years
    filter(distance %% 50 == 0) %>%
    unite(measurement_type_distance, measurement_type, distance) %>%
    spread(measurement_type_distance, measurement) %>%
    unite(name_birthday, name, birthday) %>%
    spread(position, name_birthday) %>%
    separate_name_birthday_cols() 
  return(data)
}