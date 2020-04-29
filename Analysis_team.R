library(DBI)
library(RPostgreSQL)
library(tidyverse)
initConnection <- function(db = Sys.getenv('postgres_db')){
  RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(),
                         dbname = db,
                         host = "localhost",
                         user = "postgres")
}
con <- initConnection()
df <- tbl(con, 'positions')

#team 1 matchday
team_1_MD <- df %>% 
  filter(team=='1', match_day == 'MD', match_date == '2019-04-08') %>% 
  collect

#average lat by for 
team_1_MD %>%
  group_by(player_display_name) %>%
  summarise(avg_lat = mean(lat))

#wider format pivot data to sync through time(not really good pratice)
team_1_MD_wide %>%
  select(time_since_start, player_display_name, lat, lon) %>%
  pivot_wider(id_cols = time_since_start,names_from = player_display_name,values_from=c('lat','lon'))

#get xy cordinates for lat and lon values for team 1 Match day 
xy <- rotate_pitch(team_1_MD$lat, team_1_MD$lon,lookup = pitch_lookup)
team_1_xy <- bind_cols(team_1_MD, xy)

#average x and y for eacch player 
team_1_avg_position <- team_1_xy %>%
  group_by(player_display_name) %>%
  summarise(avg_x_position = mean(x), avg_y_position = mean(y))

#plot average positions
team_1_avg_position %>%
  ggplot() +
  geom_point(aes(x = avg_x_position, y=avg_y_position, color = player_display_name)) +
  coord_cartesian(xlim=c(0,100), ylim = c(0, 60),expand = 0) +
  theme(legend.position = 'bottom',
        legend.title = element_blank())







