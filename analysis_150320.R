install.packages("usethis")
install.packages("geosphere")
install.packages("leaflet")
install.packages("leaflet.extras")

library(tidyverse)
library(usethis)
library(DBI)
library(RPostgreSQL)
library(tidyverse)
library(sf)
library(geosphere)
library(lubridate)

##############################################################################
# Database Connection

# create connection to database
#con <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(),
#                             dbname = "trackingdata",
#                              host = "localhost",
#                              user = "postgres")


#setting enviroment variables
#set postgres_db = 'trackingdata'
usethis::edit_r_environ()

#access variables at anytime but will be specific to the machine that set enviromental variable
Sys.getenv('postgres_db')

#function to connect to DB 
initConnection <- function(db = Sys.getenv('postgres_db')){
  RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(),
                         dbname = db,
                         host = "localhost",
                         user = "postgres")
}

# enviroment variables defined inside the script 
# useful when you have API keys
con <- initConnection("trackingdata")

# check DB connection
DBI::dbDisconnect(con)

# create table schema in postgres
con <- initConnection()
DBI::dbSendQuery(con, 'CREATE TABLE IF NOT EXISTS example (player_display_name VARCHAR(64), time VARCHAR(10),
                 lat NUMERIC, lon NUMERIC, speed_m_s NUMERIC, heart_rate_bpm NUMERIC, instantaneous_acceleration_impulse NUMERIC,
                 match_day VARCHAR(4), match_date VARCHAR(10), team VARCHAR, distance NUMERIC);')

# add primary key constraint 
DBI::dbSendQuery(con, "ALTER TABLE example ADD CONSTRAINT noduplicates UNIQUE (player_display_name, time, match_day, match_date, team, lat, lon, speed_m_s, heart_rate_bpm, instantaneous_acceleration_impulse);")

##############################################################################
# Data Cleaning

# use seq_along(x) instead of 1:length(x) as 1:length does not account for 0
# loop to read file paths
team_paths <- list.files('Data', full.names = T)

my_paths <- character()
team_paths <- list.files('Data', full.names = T)
for(i in seq_along(team_paths)){
  first_sub <- list.files(team_paths[i], full.names = T)
  for(j in seq_along(first_sub)){
    second_sub <- list.files(first_sub[j], full.names = T)
    my_paths <- c(my_paths, second_sub)
  }
}
my_paths

# glue for sql and string
# function 
read_with_metadata <- function(path, con=con){
  df <- read_csv(path) %>%
    janitor::clean_names() %>%
    arrange(time) 
  # throw error if we don't have the right number of columns
  if(ncol(df) != 7) {
    stop(glue::glue('error in {path}, expected 7 columns, got {ncol(df)}. Please check the file.'))
  } else if(!identical(colnames(df),c("player_display_name", "time", "lat", "lon", "speed_m_s",
                                      "heart_rate_bpm", "instantaneous_acceleration_impulse"))){
    stop(glue::glue('error: names do not match expected column names. Check file {path}.\nExpected: {c("player_display_name", "time", "lat", "lon", "speed_m_s",
                                       "heart_rate_bpm", "instantaneous_acceleration_impulse")},\nGot: {colnames(df)}'))
  }
  matchDay <- str_extract(path, '(?<=MD)([-+0-9]{0,})')
  matchDate <-  str_extract(path, '[0-9]{4}-[0-9]{2}-[0-9]{2}')
  team <- str_extract(path, '(?<=Team )([0-9]{1,})')
  clean_df <- df %>% mutate(
    match_day = matchDay,
    match_date = matchDate,
    team = team,
  )
  
  distance <- clean_df %>%
    sf::st_as_sf(coords = c('lon', 'lat'), crs = 4326, remove = FALSE) %>%
    as('Spatial') %>%
    geosphere::distGeo()
  
  clean_df$distance <- distance
  
  # write to temporary table
  dbSendQuery(con, "CREATE TABLE IF NOT EXISTS temp_tbl (like example including all);")
  dbWriteTable(con, 'temp_tbl', clean_df, row.names = FALSE, overwrite=TRUE)
  # only write to new table if not a duplicate
  my_sql <- glue::glue_sql("
    INSERT INTO example
    SELECT player_display_name, time,
                 lat, lon, speed_m_s, heart_rate_bpm, instantaneous_acceleration_impulse,
                 temp_tbl.match_day, match_date, team, distance
    FROM temp_tbl
    ON CONFLICT 
    DO NOTHING;",.con=con)
  # run insert into
  dbSendQuery(con, my_sql)
}


# insert cleaned data to db first 10 file paths
lapply(my_paths[10], function(x) read_with_metadata(x, con))

# count rows in tables in db
tbl(con, "example")  %>% count

##########################################################################
# Data Exploration

# 
df <- tbl(con, "example")


# filter player
player_1 <- tbl(con, "example")  %>%
  filter(player_display_name == 'Sonra 05')

tbl(con, 'example') %>% glimpse

# filter team
team_1 <- tbl(con, "example")  %>%
  filter(team == '1')

# compare distance in lat and lon to speed/time
# time and speed wont be equal because there is an uneven number of observations per second
player_1 %>% group_by(time, match_day) %>%summarise(speed_m_s = mean(speed_m_s), 
                                                    distance = sum(distance))

# parse time to time field instead of a character
player_1 %>%
  collect() %>%
  mutate(dt = parse_date_time(paste0(match_date,' ',time),orders = '%Y-%m-%d %H:%M%S',tz = 'GMT')) %>%
  group_by(dt) %>%
  summarise(speed_m_s = mean(speed_m_s), distance = sum(distance)) 


player_1 %>%
  summarise(distance = sum(distance))

team_1 %>%
  group_by(player_display_name, match_date)%>%
  summarise(distance = sum(distance))



player_1_sf <- tbl(con, "example")  %>%
  filter(player_display_name == 'Sonra 08') %>%
  collect() %>%
  st_as_sf(coords = c('lon', 'lat'), crs = 4326, remove=F) 
library(leaflet)
library(leaflet.extras)
leaflet(player_1_sf) %>%
  addTiles() %>%
  addHeatmap(group="heat", lng=~lon, lat=~lat, max=.6, blur = 60)


leaflet(player_1_sf) %>%
  addProviderTiles('Esri.WorldImagery') %>%
  addHeatmap(group="heat", lng=~lon, lat=~lat, max=.6, blur = 60)