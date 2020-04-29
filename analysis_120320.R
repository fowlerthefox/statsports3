install.packages("RPostgreSQL")
install.packages("DBI")
#clean data names
install.packages("janitor")

library(sf)
library(tidyverse)
library(DBI)
library(RPostgreSQL)
library(janitor)

#connect to database
con <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(),
                              dbname = "trackingdata",
                              host = "localhost",
                              user = "postgres")

#read in data
df <- read_csv("./Data/Team 1/20190404. MD-4/2019-04-04-Sonra 02-RawDataExport.csv")

glimpse(df)

#tidy name
df %>%
  janitor::clean_names() %>%
  glimpse

#extract the details from path itself
path <- "./Data/Team 1/20190404. MD-4/2019-04-04-Sonra 02-RawDataExport.csv"
df <- read_csv(path)
matchday <- str_extract(path, '(?<=MD[- ])([-+0-9])')
matchDate <-  str_extract(path, '[0-9]{4}-[0-9]{2}-[0-9]{2}')
team <- str_extract(path, '[0-9]{1}')


df_clean <- df %>%
  janitor::clean_names() %>%
  arrange(time) %>%
#  st_as_sf(coords = c('lon', 'lat')) %>%
  mutate(matchday = matchday)

glimpse(df_clean)

#write clean table back to DB
DBI::dbWriteTable(con, "test_table", df_clean)

#contnect to table in DB
tbl(con, "test_table")

#arrange by time
tbl(con, "test_table") %>% arrange(time)



