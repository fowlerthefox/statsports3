library(tidyverse)
library(usethis)
library(DBI)
library(RPostgreSQL)
library(tidyverse)
library(geosphere)
library(lubridate)
library(sf)


#setting enviroment variables
#set postgres_db = 'trackingdata'
usethis::edit_r_environ()

#access variables at anytime but will be specific to the machine that set enviromental variable
Sys.getenv('postgres_db')

#' function to connect to DB - note we should have stored our database name with using
#'  usethis::edit_r_environ()
initConnection <- function(db = Sys.getenv('postgres_db')){
  RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(),
                         dbname = db,
                         host = "localhost",
                         user = "postgres")
}

#----------------------
# Setup schema
#----------------------


con <- initConnection()
# ensure time is character and we have a primary key
DBI::dbSendQuery(con, 'CREATE TABLE IF NOT EXISTS positions2 (id SERIAL PRIMARY KEY, 
                 player_display_name VARCHAR(64), time VARCHAR(10), seconds NUMERIC,
                 lat NUMERIC, lon NUMERIC, speed_m_s NUMERIC, heart_rate_bpm NUMERIC, instantaneous_acceleration_impulse NUMERIC,
                 match_day VARCHAR(4), match_date VARCHAR(10), team VARCHAR, distance NUMERIC);')
# add constraint so we don't accidently add duplicate records
DBI::dbSendQuery(con, "ALTER TABLE positions2 ADD CONSTRAINT nodup2 UNIQUE (player_display_name, seconds, lat, lon, speed_m_s, heart_rate_bpm, instantaneous_acceleration_impulse, match_day, match_date, team);")

# close db connection
DBI::dbDisconnect(con)




# ---------------------
# Data Cleaning
#---------------------

# use seq_along(x) instead of 1:length(x) as 1:length does not account for 0
# loop to read file paths
team_paths <- list.files('Data', full.names = T)

my_paths <- character()
team_paths <- list.files('Data', full.names = T)
for(i in seq_along(team_paths)){
  first_sub <- list.files(team_paths[i], full.names = T)
  for(j in seq_along(first_sub)){
    # only include csv files in case we have other file extensions
    second_sub <- list.files(first_sub[j], full.names = T, pattern = '.csv')
    my_paths <- c(my_paths, second_sub)
  }
}
my_paths




con <- initConnection()
# function to read in our data and write to database
read_with_metadata <- function(path, con=con){
  message('reading file ', path)
  # specify column types - set time column to character
  df <- read_csv(path, col_types= cols('c','c','d','d','d','d','d')) %>%
    janitor::clean_names() %>%
    arrange(time) 
  
  # throw error if we don't have the right number of columns
  if(ncol(df) != 7) {
    stop(glue::glue('error in {path}\n expected 7 columns, got {ncol(df)}. Please check the file.'))
  } 
  # get metadata from file path
  matchDay <- str_extract(path, '(MD|RHB)([-+0-9]{0,})')
  matchDate <-  str_extract(path, '[0-9]{4}-[0-9]{2}-[0-9]{2}')
  team <- str_extract(path, '(?<=Team )([0-9]{1,})')
  clean_df <- df %>% mutate(
    match_day = matchDay,
    match_date = matchDate,
    team = team ) %>%
    # remove nas in lat/lng
    filter(!is.na(lat), !is.na(lon))
  
  # calculate distance between consecutive positions
  distance <- clean_df %>%
    st_as_sf(coords = c('lon', 'lat'), crs = 4326, remove = FALSE) %>%
    as('Spatial') %>%
    geosphere::distGeo()
  
  clean_df$distance <- distance
  
  # calculate time since the first timestamp
  # differnt files have different formats so read data depending on format
  if(str_detect(clean_df$time[1], '^\\d{2}:\\d{2}.\\d{1,}$')){
    time_col <-  strptime(x = clean_df$time, format = "%M:%OS")
  } else{
    # else time should be in format HH:MM:SS
    time_col <-  strptime(x = clean_df$time, format = "%H:%M:%OS")
  }
  # time_col <-  strptime(x = clean_df$time, format = "%H:%M:%S")
  # remove NA's
  
  # time_since_start <- as.numeric(time_col - min(time_col, na.rm = T))/60
  # clean_df$time_since_start <- time_since_start
  
  clean_df$seconds  <- as.numeric(round(seconds(time_col)- seconds(min(time_col, na.rm=TRUE))))
  
  summarised_df <- clean_df %>%
    # these columns stay constant
    group_by(player_display_name, seconds, match_day, match_date, team) %>%
    # these columns need to be summarised by second
    summarise(time = last(time), lat = mean(lat), lon = mean(lon), speed_m_s = mean(speed_m_s),
              heart_rate_bpm = mean(heart_rate_bpm), 
              instantaneous_acceleration_impulse = mean(instantaneous_acceleration_impulse),
              distance = sum(distance)) 
  
  
  # write to temporary table
  #suppressWarnings(dbSendQuery(con, "CREATE TABLE IF NOT EXISTS temp_tbl (like positions including all);"))
  
  dbSendQuery(con, "CREATE TABLE IF NOT EXISTS temp_tbl2 (id SERIAL PRIMARY KEY, 
    player_display_name VARCHAR(64), time VARCHAR(10), seconds NUMERIC,
    lat NUMERIC, lon NUMERIC, speed_m_s NUMERIC, heart_rate_bpm NUMERIC, instantaneous_acceleration_impulse NUMERIC,
    match_day VARCHAR(4), match_date VARCHAR(10), team VARCHAR, distance NUMERIC)")
  
  dbWriteTable(con, 'temp_tbl2', summarised_df, row.names = FALSE, append=TRUE)
  # Insert into db - only write to new table if not a duplicate
  my_sql <- glue::glue_sql("
    INSERT INTO positions2
    (player_display_name, time, seconds,
                 lat, lon, speed_m_s, heart_rate_bpm, instantaneous_acceleration_impulse,
                 match_day, match_date, team, distance)
    SELECT player_display_name, time, seconds,
                 lat, lon, speed_m_s, heart_rate_bpm, instantaneous_acceleration_impulse,
                 match_day, match_date, team, distance
    FROM temp_tbl2
    ON CONFLICT 
    DO NOTHING;",.con=con)
  
  
  # run insert into
  dbSendQuery(con, my_sql)
  
  dbSendQuery(con, 'DROP TABLE temp_tbl2;')
}

# need to manually fix the path: Data/Team 1/20190415. MD/2019-04-15-Sonra 02-RawDataExport.csv

# read in all files using function to clean data from paths found in loop
lapply(my_paths, function(x) read_with_metadata(x, con))



player_distance_per_game <- tbl(con, "positions")  %>%
  filter(team == '2') %>%
  group_by(player_display_name, match_date) %>%
  summarise(distance_covered = sum(distance, na.rm=T)) %>%
  arrange(desc(distance_covered))

player_1 <- tbl(con, "positions")  %>%
  filter(player_display_name == 'Sonra 05') 

team_1 <- tbl(con, "positions")  %>%
  filter(team == '1') %>%
  group_by(player_display_name, match_date, match_day) %>%
  summarise(totaldist = sum(distance, na.rm = T))

print(team_1)

per_second <- player_1 %>%
  collect() %>%
  mutate(dt = parse_date_time(paste0(match_date,' ',time),orders = '%Y-%m-%d %H:%M%S',tz = 'GMT')) %>%
  group_by(dt) %>%
  summarise(speed_m_s = mean(speed_m_s), distance = sum(distance)) 

player_1_sf <- tbl(con, "positions")  %>%
  filter(player_display_name == 'Sonra 02')  %>%
  collect() %>%
  mutate(dt = lubridate::parse_date_time(paste0(match_date,' ',time),orders = '%Y-%m-%d %H:%M%S',tz = 'GMT')) %>%
  st_as_sf(coords = c('lon', 'lat'), crs = 4326, remove=F) 

DBI::dbDisconnect(con)


#----------------
# -- HEATMAP
#---------------
library(leaflet)
library(leaflet.extras)

leaflet(player_1_sf) %>%
  addProviderTiles('Esri.WorldImagery') %>%
  addHeatmap(group="heat", lng=~lon, lat=~lat, max=.6, blur = 60)


tbl(con, 'positions') %>% filter(team == '1', player_display_name=='Sonra 02', match_day == 'MD', 
                                 match_date == '2019-04-15' ) %>% glimpse

#debug distance problem
# data <- tbl(con, 'positions') %>%
#   filter(team == '2',
#          player_display_name == 'STATSports 01',
#          match_day == 'MD',
#          match_date == '2019-10-10') %>%
#   collect()
# 
# 
# data %>%
#   select(lat, lon, speed_m_s, distance, time) %>%
#   st_as_sf(coords = c('lon', 'lat'), crs = 4326, remove = FALSE) %>%
#   as('Spatial') %>%
#   geosphere::distGeo()
# 
# 
# 
# 
# df <- read_csv('Data/Team 2/20191010 MD/2019-10-10-STATSports 01-RawDataExport.csv')
# con <- initConnection()
# # ensure time is character and we have a primary key
# DBI::dbSendQuery(con, 'CREATE TABLE IF NOT EXISTS test (id SERIAL PRIMARY KEY, 
#                  player_display_name VARCHAR(64), time VARCHAR(10), time_since_start NUMERIC,
#                  lat NUMERIC, lon NUMERIC, speed_m_s NUMERIC, heart_rate_bpm NUMERIC, instantaneous_acceleration_impulse NUMERIC,
#                  match_day VARCHAR(4), match_date VARCHAR(10), team VARCHAR, distance NUMERIC);')
# # add constraint so we don't accidently add duplicate records
# DBI::dbSendQuery(con, "ALTER TABLE test ADD CONSTRAINT noduptest UNIQUE (player_display_name, time, lat, lon, speed_m_s, heart_rate_bpm, instantaneous_acceleration_impulse, match_day, match_date, team);")
# path <- 'Data/Team 2/20191010 MD/2019-10-10-STATSports 01-RawDataExport.csv'
# df <- read_csv(path, col_types= cols('c','c','d','d','d','d','d')) %>%
#   janitor::clean_names() %>%
#   arrange(time) 
# # get metadata from file path
# matchDay <- str_extract(path, '(MD|RHB)([-+0-9]{0,})')
# matchDate <-  str_extract(path, '[0-9]{4}-[0-9]{2}-[0-9]{2}')
# team <- str_extract(path, '(?<=Team )([0-9]{1,})')
# clean_df <- df %>% mutate(
#   match_day = matchDay,
#   match_date = matchDate,
#   team = team ) %>%
#   # remove nas in lat/lng
#   filter(!is.na(lat), !is.na(lon))
# # calculate distance between consecutive positions
# distance <- clean_df %>%
#   st_as_sf(coords = c('lon', 'lat'), crs = 4326, remove = FALSE) %>%
#   as('Spatial') %>%
#   geosphere::distGeo()
# clean_df$distance <- distance
# # calculate time since the first timestamp
# # differnt files have different formats so read data depending on format
# if(str_detect(clean_df$time[1], '^\\d{2}:\\d{2}.\\d{1,}$')){
#   time_col <-  strptime(x = clean_df$time, format = "%M:%OS")
# } else{
#   # else time should be in format HH:MM:SS
#   time_col <-  strptime(x = clean_df$time, format = "%H:%M:%OS")
# }
# # time_col <-  strptime(x = clean_df$time, format = "%H:%M:%S")
# # remove NA's
# time_since_start <- as.numeric(time_col - min(time_col, na.rm = T))/60
# clean_df$time_since_start <- time_since_start
# clean_df %>%
#   filter(team == '2',
#          player_display_name == 'STATSports 01',
#          match_day == 'MD',
#          match_date == '2019-10-10')
# dbSendQuery(con, "CREATE TABLE IF NOT EXISTS test_temp_tbl (id SERIAL PRIMARY KEY, 
#     player_display_name VARCHAR(64), time VARCHAR(10), time_since_start NUMERIC,
#     lat NUMERIC, lon NUMERIC, speed_m_s NUMERIC, heart_rate_bpm NUMERIC, instantaneous_acceleration_impulse NUMERIC,
#     match_day VARCHAR(4), match_date VARCHAR(10), team VARCHAR, distance NUMERIC)")
# dbWriteTable(con, 'test_temp_tbl', clean_df, row.names = FALSE, overwrite=TRUE)
# my_sql <- glue::glue_sql("
#     INSERT INTO test
#     (player_display_name, time, time_since_start,
#                  lat, lon, speed_m_s, heart_rate_bpm, instantaneous_acceleration_impulse,
#                  match_day, match_date, team, distance)
#     SELECT player_display_name, time, time_since_start,
#                  lat, lon, speed_m_s, heart_rate_bpm, instantaneous_acceleration_impulse,
#                  match_day, match_date, team, distance
#     FROM test_temp_tbl
#     ON CONFLICT 
#     DO NOTHING;",.con=con)
# dbSendQuery(con, my_sql)
# tbl(con, 'test') %>%
#   filter(team == '2',
#          player_display_name == 'STATSports 01',
#          match_day == 'MD',
#          match_date == '2019-10-10') %>%
#   collect()
# 
# tbl(con, 'test') %>%
#   collect
# tbl(con, 'test_temp_tbl') %>%




df <- tbl(pool,'positions2') %>% 
  filter(team == '1',
         player_display_name == 'Sonra 02',
         match_day == 'MD',
         match_date == '2019-04-08') %>%
  collect()

df %>%
  arrange(.,seconds) %>%
  ggplot() +
  geom_line(aes(x= seconds/60, y = speed_m_s*50), col = 'green', alpha = 0.5) +
  geom_line(aes(x= seconds/60, y = heart_rate_bpm), col = '#cb4b16') +
  scale_y_continuous(name="BPM", sec.axis = sec_axis(~./50, name = "speed"))
scale_x_continuous(name = 'minutes since start')+
  ggthemes::theme_solarized(light=FALSE)

#heart rate and speed smoothed
df %>%
  arrange(.,seconds) %>%
  ggplot() +
  geom_smooth(aes(x= seconds/60, y = speed_m_s*50), col = 'green', alpha = 0.5, method = 'loess', span=0.1) +
  geom_smooth(aes(x= seconds/60, y = heart_rate_bpm), col = '#cb4b16' , method = 'loess', span=0.1) +
  scale_y_continuous(name="BPM", sec.axis = sec_axis(~./50, name = "speed"))
scale_x_continuous(name = 'minutes since start')+
  ggthemes::theme_solarized(light=FALSE)

