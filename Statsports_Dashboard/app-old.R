#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(ggsoccer)
library(ggplot2)
library(ggthemes)
library(leaflet)
library(leaflet.extras)
library(shiny)
library(tidyverse)
library(usethis)
library(DBI)
library(RPostgreSQL)
library(tidyverse)
library(sf)
library(geosphere)
library(bootstraplib)
library(promises)
library(future)
library(plotly)
plan(multiprocess)

# bring in our custom functions
source('./functions.R')


#function to connect to DB 
initConnection <- function(db = Sys.getenv('postgres_db')){
    RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(),
                           dbname = db,
                           host = "localhost",
                           user = "postgres")
}

# initConnection <- function(db = Sys.getenv('postgres_db')){
#     RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(),
#                            dbname = 'testdb',
#                            host = "test-db.cjwxrjz1vsce.eu-west-2.rds.amazonaws.com",
#                            port = 5432,
#                            user = "postgres",
#                            password="postgres")
# }


#connect database
con <- initConnection()

#distinct player names
#team_choices <- tbl(con,'positions2') %>% distinct(team) %>% collect() %>% pull

#distinct player names
#player_choices <- tbl(con,'positions2') %>% distinct(player_display_name) %>% collect() %>% pull

#distinct player names
#match_choices <- tbl(con,'positions2') %>% distinct(match_day) %>% collect() %>% pull

#set time range to minute
#time_range <- tbl(con,'positions2')  %>% distinct(seconds) %>% pull


team_choices <- tbl(con,'positions2') %>% distinct(team) %>% arrange(team) %>% collect() %>% pull
player_choices <- ''
match_choices <- ''
date_choices <- ''
seconds <- c(1:6000)

bs_theme_new(bootswatch = 'darkly')
# alter bs theme variables
## Variables we can override: https://github.com/twbs/bootstrap/blob/v4-dev/scss/_variables.scss
bs_theme_add_variables(
    "danger" = "#bf4600",
    "font-family-base" = "Roboto"
)

ui <- bootstrapPage(bootstrap(),
                    #add link to icon repo and embed new font
                    tags$head(tags$script(src="https://kit.fontawesome.com/7d28f142d5.js", crossorigin="anonymous"),
                              tags$link(href="https://fonts.googleapis.com/css2?family=Roboto:wght@100;300;400;500;700;900&display=swap", rel="stylesheet")),
                    # nav bar
                    div(class="navbar navbar-dark bg-primary mb-3",
                        div(class="container",
                            tags$img(height = 50, width = 200, src = "https://image4.owler.com/logo/statsports_owler_20191115_201345_original.jpg", href = "https://statsports.com/")
                        )
                    ),
                    
                    # body
                    div(class="container",
                        div(class = "card text-white bg-secondary mb-3",
                            # COLLAPSE BUTTON 
                            tags$button(class= "btn btn-primary", type = "button", `data-toggle`="collapse",
                                        `data-target`="#collapseOptions", `aria-expanded`="false", `aria-controls`="collapseOptions",
                                        "Click to toggle options"),
                            # SOURCE OUR FILTERS WITHIN THE COLLAPSE
                            div(id = "collapseOptions", class="collapse",
                                source('./filters.R', local=TRUE)$value   
                            )
                        ),
                        
                            div(class="tabbable sticky-top",
                                tags$ul(class="nav nav-tabs mb-2",
                                        tags$li(class="active mr-2",
                                                tags$a(href="#tab1", `data-toggle`="tab", class ="bg-danger mb-2",
                                                       "Player Stats")
                                        ),
                                        tags$li(class="mr-2", 
                                                tags$a(href="#tab2", `data-toggle`="tab", class ="bg-danger mb-2",
                                                       "Positioning")
                                        ),
                                        tags$li(class="mr-2", 
                                                tags$a(href="#tab3", `data-toggle`="tab", class ="bg-danger mb-2",
                                                       "Team Comparison")
                                        )
                                ),
                                div(class="tab-content",
                                    div(class="tab-pane active fade show", id="tab1",
                                        source('./tab-1.R', local=TRUE)$value
                                    ),
                                    div(class="tab-pane fade", id="tab2",
                                        source('./tab-2.R', local=TRUE)$value
                                    ),
                                    div(class="tab-pane fade", id="tab3",
                                        source('./tab-3.R', local=TRUE)$value
                                    )
                                )
                            )
                        
                    )
)


# Server logic
server <- function(input, output, session) {
    # render blank leaflet map to update
    # output$map <- renderLeaflet({
    #     leaflet() %>% addProviderTiles('Esri.WorldImagery')  %>%
    #         setView(lng = -0.106, lat=51.5177,zoom=12)})
    
    
    # update player dropdown
    player_opts <- reactive({
        tbl(con,'positions2') %>% 
            filter(team == local(input$team_input)) %>%
            distinct(player_display_name) %>%
            arrange(player_display_name) %>%
            pull(player_display_name)
    })
    
    # update the player input
    observe({
        tryCatch({
            updateSelectInput(session, 'player_input',  choices = player_opts(),selected = player_opts()[1])
        },
        error = function(err){
            # showNotification(paste0('player input error',err),type = 'error')
        })
    })
    
    md <- reactiveValues( v = 0)
    # update matchday dropdown
    matchday_opts <- reactive({
        input$player_input

        isolate({
            md$v <- md$v + 1
            tbl(con,'positions2') %>% 
                filter(team == local(input$team_input), player_display_name == local(input$player_input)) %>%
                distinct(match_day) %>%
                arrange(match_day) %>%
                pull(match_day)
        })
    })
    
    # update the match input
    observe({
        tryCatch({
            updateSelectInput(session, 'match_input',  choices = matchday_opts(),selected = matchday_opts()[1])
        },
        error = function(err){
            # showNotification(paste0('match input error',err),type = 'error')
        })
    })
    
    mdt <- reactiveValues( v = 0)
    # update matchdate dropdown
    matchdate_opts <- reactive({
        input$match_input
        md$v
        isolate({
            mdt$v <- mdt$v + 1
            tbl(con,'positions2') %>% 
                filter(team == local(input$team_input), player_display_name == local(input$player_input), 
                       match_day == local(input$match_input)) %>%
                distinct(match_date) %>%
                arrange(match_date) %>%
                pull(match_date)
        })
    })
    
    # update the match input
    observe({
        tryCatch({
            updateSelectInput(session, 'date_input',  choices = matchdate_opts(),selected = matchdate_opts()[1])
        },
        error = function(err){
            # showNotification(paste0('match date input error',err),type = 'error')
        })
    })
    
    tm <- reactiveValues( v = 0)
    # update slider input
    time_opts <- reactive({
        # we need to update the time options if the date input changes or if it's unchanged but previous options have updated
        input$date_input
        mdt$v
        isolate({
            tm_vals <- tbl(con,'positions2') %>% 
                filter(team == local(input$team_input), player_display_name == local(input$player_input), 
                       match_day == local(input$match_input), match_date == local(input$date_input)) %>%
                distinct(seconds) %>%
                arrange(seconds) %>%
                pull(seconds)
            # convert to minutes!
            tm_vals <- tm_vals/60
            # only manually invalidate time slider if options are the same as before
            if(round(input$time[1]) == round(min(tm_vals)) & round(input$time[2]) == round(max(tm_vals))) {
                tm$v <- tm$v + 1
            }
            tm_vals
        })
    })
    
    observe({
        tryCatch({
            min_time <- min(time_opts(),na.rm = T)
            max_time <- max(time_opts(),na.rm = T)
            updateSliderInput(session, 'time',  value = c(min_time,max_time),
                              min = floor(min(min_time, na.rm = T)),
                              max = ceiling(max(max_time, na.rm = T)))
        }, error = function(err){
            # showNotification(paste0('slider error:', err), type = 'error')
        })
    })
    # assign data that relies on changing inputs
    # wrap code in tryCatch like so: 
    data <- reactive({
        tryCatch({
            # only react when time changes ( it is the last of the inputs to update )
            tm$v
            input$time
            isolate({
                max_time <- max(local(input$time))
                min_time <- min(local(input$time))
                filtered_data <- tbl(con,'positions2') %>% 
                    filter(player_display_name == local(input$player_input),
                           match_day == local(input$match_input),
                           team == local(input$team_input),
                           match_date == local(input$date_input),
                           seconds/60 < max_time,
                           seconds/60 > min_time) %>%
                    collect() 
            })
        },
        error = function(err){
            showNotification(paste0('data error:',err),type = 'error')
        })
    })
    
    
    # update the existing map
    # observe({
    #     tryCatch({
    #         data_centroid <- st_bbox(data()) %>%
    #             st_as_sfc() %>%
    #             st_centroid %>%
    #             st_coordinates()

    #         # clear previous data
    #         leafletProxy('map',data = data()) %>%
    #             setView(lng = data_centroid[1], lat= data_centroid[2], zoom = 16) %>%
    #             clearShapes() %>%
    #             clearMarkers() %>%
    #             clearHeatmap()  %>% 
    #             addHeatmap(group="heat", lng=~lon, lat=~lat, max=.6, blur = 60)
    #     },
    #     error = function(err){
    #         showNotification(paste0('error in leaflet map',err),type = 'error')
    #     })
    # })
    
    
    
    #app some plots
    #heart rate per minute over time
    output$hrPlot <- renderPlot({
        data() %>%
        arrange(seconds) %>%
            ggplot() +
            geom_line(aes(x= seconds/60, y = heart_rate_bpm)) +
            scale_x_continuous(name = 'minutes since start')+
            scale_y_continuous(name = 'BPM') +
            ggthemes::theme_solarized(light=FALSE)
    })
    
    #accumalated distance over time
    output$distPlot <- renderPlot({
        data() %>%
            arrange(seconds) %>%
            mutate(cum_distance = cumsum(distance)) %>%
            ggplot() +
            geom_line(aes(x= seconds/60, y = cum_distance)) +
            scale_x_continuous(name = 'minutes since start')+
            scale_y_continuous(name = 'distance') +
            ggthemes::theme_solarized(light=FALSE)
    })
    
    #speed over time
    output$speedPlot <- renderPlot({
        data() %>%
        arrange(seconds) %>%
            ggplot() +
            geom_line(aes(x= seconds/60, y = speed_m_s)) +
            scale_x_continuous(name = 'minutes since start')+
            scale_y_continuous(name = 'm/s') +
            ggthemes::theme_solarized(light=FALSE)
    })

    #output for distance icon
    output$distanceOutput2 <- output$distanceOutput <- renderText({ tryCatch({data() %>%
            summarise(dist = sum(distance, na.rm=T)) %>%
            pull(dist) %>%
            round() %>%
            format(n_small=1, big.mark=',') %>%
            paste0('',., 'm')},
            error = function(e){
                return('')
            })
    })



    
    #pull player name from first row of the filtered data
    output$nameOutput <- renderText({ tryCatch({data() %>%
            head(1) %>%
            pull(player_display_name) %>%
            paste0('',.)},
            error = function(e){
                return('')
            })
    })

    
    #output for date
    output$dateOutput <- renderText({ tryCatch({data() %>%
            head(1) %>%
            pull(match_date)},
            error = function(e){
                return('')
    
            })
    })
    
    #pull top speed
    output$mSpeedOutput <- renderText({ tryCatch({data() %>%
            summarise(maxSpeed = max(speed_m_s, na.rm=TRUE)) %>%
            pull(maxSpeed) %>%
            round(digits = 2) %>%
            paste0('Max Speed: ',., 'm/s')},
            error = function(e){
                return('')
            })
    })

    
    
    #high speed running
    output$hSpeedOutput <- renderText({ tryCatch({
        if(str_detect(pull(data(), time)[1], '^\\d{2}:\\d{2}.\\d{1,}$')){
            time_diff <-  strptime(x = pull(data(), time), format = "%M:%OS")[2] - strptime(x =  pull(data(), time), format = "%M:%OS")[1] 
        } else{
            # else time should be in format HH:MM:SS
            time_diff <-  strptime(x = pull(data(), time), format = "%H:%M:%OS")[2] - strptime(x = pull(data(), time), format = "%H:%M:%OS")[1] 
        }
        data() %>%
            st_drop_geometry() %>%
            filter(speed_m_s > 7) %>%
            summarise(time_high_speed = n() * time_diff) %>%
            as.numeric %>% round(.,2) %>% paste0('', ., ' seconds')
    },
    error = function(e){
        return('')
        })
    })
    
    #pull top speed
    output$mSpeedOutput <- renderText({ tryCatch({data() %>%
            summarise(maxSpeed = max(speed_m_s, na.rm=TRUE)) %>%
            pull(maxSpeed) %>%
            round(digits = 2) %>%
            paste0('',., 'm/s')},
            error = function(e){
                return('')
            })
    })
    
    #accelerations
    output$accOutput <- renderText({ tryCatch({
            data() %>%
            summarise(accel = sum(instantaneous_acceleration_impulse > 3), na.rm=T) %>%
            pull(accel) %>%
            format(n_small=1, big.mark=',') %>%
            paste0('',.)},
            error = function(e){
                return('')
            })
    })
    
    #decelerations
    output$decOutput <- renderText({ tryCatch({data() %>%
            summarise(decel = sum(instantaneous_acceleration_impulse < -3), na.rm=T) %>%
            pull(decel) %>%
            format(n_small=1, big.mark=',') %>%
            paste0('',.)},
            error = function(e){
                return('')
            })
    })
    
    
    #insert in table
    #date output
    output$hrOutput <- renderText({ tryCatch({
            data() %>%
            summarise(maxHR = max(heart_rate_bpm, na.rm=TRUE)) %>%
            pull(maxHR) %>%
            round(digits = 2) %>%
            paste0('',., 'm/s')},
            error = function(e){
            return('')
        
    },
    error = function(e){
        return('')
        })
    })
    
    #-------------------------------
    # TAB 2 
    # add plot of pitch
    output$pitchPlot <- renderPlot({
        tryCatch({
            df <- data()
            future({
                df %>%
                    filter(!is.na(lon)) %>%
                    filter( !is.na(lat)) %>%
                    collect
            }) %...>%
                {
                    rotate_pitch(.$lat, .$lon,lookup = pitch_lookup) %>%
                        ggplot(aes(x= x, y = y)) +
                        annotate_pitch(fill="#007700", colour="white") +
                        theme_pitch()+
                        stat_density2d(aes(fill=after_stat(level)),geom='polygon',colour=NA,show.legend = FALSE) + 
                        scale_fill_continuous(low="yellow",high="red")
                }
        }, error= function(err){
            ggplot()
        })
    })
    #-------------------------------
    # TAB 3
    # player comparisons
    comparison_data <- reactive({
        # like before isolate the reactivity to only invalidate when the time field changes
        tm$v
        input$time
        df <- tbl(con, 'positions2') 
        isolate({
            team_input <- local(input$team_input)
            match_input <- local(input$match_input)
            match_date_input <- local(input$date_input)
            max_time <- max(local(input$time))
            min_time <- min(local(input$time))
            team <- df %>% 
                filter(team == team_input,
                       match_day == match_input,
                       match_date == match_date_input,
                       seconds/60 < max_time,
                       seconds/60 > min_time) %>% 
                collect 
            xy <- rotate_pitch(team$lat, team$lon,lookup = pitch_lookup)
            bind_cols(team, xy)
        })
    })
    
    output$comparison_plot <- renderPlotly({
        p <- comparison_data() %>%
            group_by(player_display_name) %>%
            summarise(avg_x_position = mean(x),
                      avg_y_position = mean(y)) %>%
            ggplot() +
            annotate_pitch(fill="#007700", colour="white") +
            theme_pitch()+
            geom_point(aes(x = avg_x_position, y=avg_y_position, color = player_display_name)) +
            theme(legend.position = 'bottom',
                  legend.title = element_blank())
        ggplotly(p)
    })
    
    #plot total distance
    output$comparison_bar <- renderPlotly({
        p <- comparison_data() %>%
            group_by(player_display_name) %>%
            summarise(distance = sum(distance)) %>%
            ggplot() +
            geom_bar(aes(x = player_display_name, y=distance, fill=player_display_name), stat = 'identity') +
            geom_hline(aes(yintercept = mean(distance))) +
            ggthemes::theme_solarized(light=FALSE) +
            theme(legend.position = 'bottom',
                  legend.title = element_blank()) 
        ggplotly(p)
    })
    
}


# Complete app with UI and server components
shinyApp(ui, server)






