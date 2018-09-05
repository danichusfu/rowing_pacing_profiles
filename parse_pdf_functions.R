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
  event_cateogry_abbreviation   <- race_info[4] %>% word(2)
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
  speed_strokes <- 
    extract_tables(gps_file_name)[[1]] %>%
    as_tibble() %>%
    filter_all(any_vars(str_detect(., "\\d"))) %>%
    select(1:num_cols) %>%
    rename_all(~ col_names) %>%
    gather(team_lane_measurement_type, measurement, -distance) %>%
    separate(team_lane_measurement_type, into = c("team", "lane", "measurement_type"), sep = "_")
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
    mutate_at(vars(c("lane", "event_num", "distance", "measurement")), parse_number)
  
  return(gps_parsed)
}



parse_c51a <- function(c51a_file_name){
  
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
      "RACE DATA",
      "Page")
  # make it a regex for str_detect
  exclude_list <- glue_collapse(exclude_list, "|")
  
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
  
  # regualr expressions for filtering relevant information
  lane_team_position_reg_ex               <- "^\\d{1} [[:alpha:]]{3} \\([[:alnum:]]{1}\\)"
  lane_team_position_name_birthday_reg_ex <- "^\\d{1} [[:alpha:]]{3} \\([[:alnum:]]{1}\\) .+ \\d{2} [[:alpha:]]{3} \\d{4}$"
  lane_team_reg_ex                        <- "^\\d{1} [[:alpha:]]{3}"
  
  c51a_info <-
    extract_text(c51a_file_name) %>%
    str_split('\r\n') %>% 
    unlist() %>% 
    as_tibble() %>%
    mutate(new_team = str_detect(value, lane_team_position_reg_ex),
           team_num = cumsum(new_team),
           # in 2010 a position is included with the single boats
           # this finds if this is the case
           pos_incl = str_detect(value, lane_team_position_name_birthday_reg_ex))
 
  if(max(c51a_info$team_num) == 0 | max(c51a_info$pos_incl) > 0){
    
    start_list_parsed <-
      c51a_info %>%
      filter(str_detect(value, lane_team_reg_ex)) %>%
      mutate(value = str_remove(value, "\\([[:alnum:]]{1}\\)") %>% str_squish()) %>%
      select(value) %>%
      mutate(lane     = str_extract(value, "^\\d{1}") %>% parse_number(),
             team     = str_extract(value, "(?<=^\\d{1} )[[:alpha:]]{3}"),
             position = "single",
             name     = str_extract(value, "(?<=^\\d{1} [[:alpha:]]{3} )[^\\d]+(?= \\d{2})"),
             birthday = str_extract(value, "\\d{2} [[:alpha:]]{3} \\d{4}$") %>% dmy()
             ) %>%
      select(-value)
    
      return(start_list_parsed)
    
  } else {
    pre_amble <- c51a_info %>% filter(team_num == 0) %>% pull(value)
    
    c51a_info <-
      c51a_info %>%
      filter(team_num != 0) %>%
      add_count(team_num) %>%
      add_count(new_team) %>%
      mutate(n  = min(n),
             nn = min(nn)) %>% 
      rename(entries_per_team = n, num_teams = nn) %>%
      filter(!value %in% pre_amble) %>%
      filter(!str_detect(value, "Page \\d")) %>%
      filter(row_number() <= entries_per_team * num_teams) %>%
      select(value)
    
    
    lane_team_positions <- c51a_info %>% filter(str_detect(value, "\\([[:alnum:]]{1}\\)"))
    names               <- c51a_info %>% filter(str_detect(value, "^[[:alpha:]]"))                 %>% rename(name = value)
    birthdays           <- c51a_info %>% filter(str_detect(value, "\\d{2} [[:alpha:]]{3} \\d{4}")) %>% rename(birthday = value)
    
    # return(nrow(lane_team_positions) == nrow(names) & nrow(names) == nrow(birthdays))
    
    start_list_parsed <-
      lane_team_positions %>%
      separate(value, c("lane", "team", "position"), sep = " ", fill = "left") %>%
      fill(lane, team) %>%
      bind_cols(names, birthdays) %>%
      mutate(lane     = parse_number(lane),
             birthday = dmy(birthday))
    return(start_list_parsed)
  }
}


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
  
  possible_progressions <- paste0("^", possible_progressions, "($| WB$)") %>% glue_collapse("|")
  rank_lane_team_reg_ex <- "^[[:alnum:]]{1} [[:alnum:]]{1,2} [[:alnum:]]{3}"
  
  
  c73_info <-
    extract_text(c73_file_name) %>% 
    str_replace('DNS', 'STR_TO_FIND_LINE STR_TO_SPLIT_ON') %>%
    str_split('(\r\n|STR_TO_SPLIT_ON )') %>% 
    unlist() %>% 
    as_tibble() %>%
    mutate(value = str_squish(value)) %>%
    filter(row_number() > 7) %>%
    filter(# either its a progression value which is always preceded by a split time
      str_detect(value, possible_progressions) & str_detect(lag(value), "\\d\\.\\d{2}|STR_TO_FIND_LINE") |
        # or it is the line that has the rank lane and team information
        str_detect(value, rank_lane_team_reg_ex))
  
  
  race_splits  <- c73_info %>% filter(str_detect(value, rank_lane_team_reg_ex))
  progressions <- c73_info %>% filter(str_detect(value, possible_progressions)) %>% rename(progression = value)
  if(nrow(progressions)  == 0){
    progressions <- tibble(progression = rep("final_race", nrow(race_splits)))
  }
  
  reg_ex_for_name <- glue("(?<={rank_lane_team_reg_ex})[^\\d]+(?= \\d)")
  
  # return(nrow(progressions) == nrow(race_splits))
  
  results_parsed <-
    race_splits %>%
    mutate(rank_final = word(value, 1) %>% parse_number(),
           lane       = word(value, 2) %>% parse_number(),
           team       = word(value, 3)) %>%
    select(-value) %>%
  bind_cols(progressions)
  
  return(results_parsed)
}


separate_name_birthday_cols <- function(data){
  data %>%
    separate(single, c("single_birthday", "single_name"), sep = "_") %>%
    separate(second, c("second_birthday", "second_name"), sep = "_") %>% 
    separate(third, c("third_birthday", "third_name"), sep = "_") %>%
    separate(fourth, c("fourth_birthday", "fourth_name"), sep = "_") %>% 
    separate(fifth, c("fifth_birthday", "fifth_name"), sep = "_") %>%
    separate(sixth, c("sixth_birthday", "sixth_name"), sep = "_") %>% 
    separate(seventh, c("seventh_birthday", "seventh_name"), sep = "_") %>%
    separate(stroke, c("stroke_birthday", "stroke_name"), sep = "_") %>% 
    separate(bow, c("bow_birthday", "bow_name"), sep = "_") %>%
    separate(coxswain, c("coxswain_birthday", "coxswain_name"), sep = "_") 
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
    mutate(#c51a_parsed = map(data, ~ parse_c51a(.$c51a)),
           #c73_parsed  = map(data, ~ parse_c73(.$c73)),
           gps_parsed  = map(data, ~ parse_gps(.$mgps)))
  
  files_joined <-
    files_parsed %>%
    mutate(data_joined = map2(c51a_parsed, c73_parsed, ~ full_join(.x, .y, by = c("team", "lane"))),
           data_joined = map2(data_joined, gps_parsed, ~ full_join(.x, .y, by = c("team", "lane"))))
  
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
                                position == "single" ~ "single")) %>%
    unite("name_birthday", name, birthday) %>%
    spread(position, name_birthday) %>%
    separate_name_birthday_cols()
  
  return(files_cleaned)
  
}

