#remotes::install_github("rstudio/bootstraplib")
# install.packages("profvis")
library(ggsoccer)
library(plotly)
library(pool)
# library(profvis)
library(leaflet)
library(leaflet.extras)
library(shiny)
library(bootstraplib)
library(tidyverse)
library(usethis)
library(DBI)
library(RPostgreSQL)
library(tidyverse)
library(sf)
library(geosphere)
library(promises)
library(future)
plan(multiprocess)


# bring in our custom functions
source('./functions.R')

initConnection <- function(db = 'trackingdata'){
  pool::dbPool(
    RPostgreSQL::PostgreSQL(),
    minSize = 0,
    maxSize = Inf,
    idleTimeout = 0,
    dbname = db,
    host = "db",
    user = "postgres",
    password= "password"
  )   
}

# init new bootstrap theme
bs_theme_new(bootswatch = 'darkly')
# alter bs theme variables
## Variables we can override: https://github.com/twbs/bootstrap/blob/v4-dev/scss/_variables.scss
bs_theme_add_variables(
  "danger" = "#bf4600",
  "font-family-base" = "Roboto"
)



#connect database
pool <- initConnection()


team_choices <- tbl(pool,'positions2') %>% distinct(team) %>% arrange(team) %>% collect() %>% pull
player_choices <- ''
match_choices <- ''
date_choices <- ''
seconds <- c(1:100)


ui <- bootstrapPage(bootstrap(),
                    tags$head(tags$script(src="https://kit.fontawesome.com/7d28f142d5.js", crossorigin="anonymous")),
                    # nav bar
                    div(class="navbar mb-3 bg-primary",
                        div(class="container",
                            tags$img(height = 50, width = 200, src = "https://image4.owler.com/logo/statsports_owler_20191115_201345_original.jpg", href = "https://statsports.com/")
                        )
                    ),
                    
                    # body
                    div(class="container",
                        div(class = "card text-white bg-secondary mb-3",
                            tags$button(class= "btn btn-primary", type = "button", `data-toggle`="collapse", `data-target`="#collapseOptions", `aria-expanded`="false", `aria-controls`="collapseOptions",
                                        "Click to toggle options"),
                            
                            div(id = "collapseOptions", class="collapse",
                                source('./filters.R', local=TRUE)$value   
                            )
                        ),
                        
                        
                        div(class="tabbable",
                            tags$ul(class="nav nav-tabs mb-2",
                                    tags$li(class="active mr-2",
                                            tags$a(href="#tab1", `data-toggle`="tab", class ="bg-danger",
                                                   "Player Stats")
                                    ),
                                    tags$li(class="mr-2",
                                            tags$a(href="#tab2", `data-toggle`="tab", class ="bg-danger",
                                                   "Positioning")
                                    ),
                                    tags$li(class="mr-2",
                                            tags$a(href="#tab3", `data-toggle`="tab", class ="bg-danger",
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
                        ),
                        
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
    tbl(pool,'positions2') %>% 
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
      tbl(pool,'positions2') %>% 
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
  
  
  mdt <- reactiveValues( v = 0 )
  # update matchdate dropdown
  matchdate_opts <- reactive({
    input$match_input
    md$v
    isolate({
      mdt$v <- mdt$v + 1
      tbl(pool,'positions2') %>% 
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
      
      tm_vals <- tbl(pool,'positions2') %>% 
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
    # only react when time changes ( it is the last of the inputs to update )
    # validate(need(!identical(input$time, c(1:5))))
    tm$v
    input$time
    isolate({
      max_time <- max(local(input$time))
      min_time <- min(local(input$time))
      player_input <- local(input$player_input)
      match_input <- local(input$match_input)
      team_input <- local(input$team_input)
      date_input <- local(input$date_input)
      
      future({
        # we have to open up a new connection and close it again within future
        pool <- initConnection()
        on.exit(pool::poolClose(pool))
        
        filtered_data <- tbl(pool,'positions2') %>% 
          filter(player_display_name == player_input,
                 match_day == match_input,
                 team == team_input,
                 match_date == date_input,
                 seconds/60 < max_time,
                 seconds/60 > min_time) %>%
          collect()
      }) 
      
    })
  })
  
  
  
  #add some plots
  #heart rate per minute over time
  output$hrPlot <- renderPlot({
    data() %...>% {
      arrange(.,seconds) %>%
        ggplot() +
        geom_line(aes(x= seconds/60, y = heart_rate_bpm), col = '#cb4b16') +
        scale_x_continuous(name = 'minutes since start')+
        scale_y_continuous(name = 'BPM')+
        ggthemes::theme_solarized(light=FALSE)
    } %...!% {
      # on error create blank plot
      ggplot() + ggthemes::theme_solarized(light=FALSE)
    } 
  })
  
  #accumalated distance over time
  output$distPlot <- renderPlot({
    data() %...>%
      {
        arrange(.,seconds) %>%
          mutate(cum_distance = cumsum(distance)) %>% 
          ggplot() +
          geom_line(aes(x= seconds/60, y = cum_distance), col = '#cb4b16') +
          scale_x_continuous(name = 'minutes since start')+
          scale_y_continuous(name = 'distance')+
          ggthemes::theme_solarized(light=FALSE)
      } %...!% {
        # on error create blank plot
        ggplot() + ggthemes::theme_solarized(light=FALSE)
      }
    
  })
  
  #speed distance over time
  output$speedPlot <- renderPlot({
    data() %...>% {
      arrange(.,seconds) %>% 
        ggplot() +
        geom_line(aes(x= seconds/60, y = speed_m_s), col = '#cb4b16') +
        scale_x_continuous(name = 'minutes since start')+
        scale_y_continuous(name = 'speed m/s') +
        ggthemes::theme_solarized(light=FALSE)
    } %...!% {
      # on error create blank plot
      ggplot() + ggthemes::theme_solarized(light=FALSE)
    }
    
  })
  
  
  # distance text output
  output$distanceOutput2 <- output$distanceOutput <- renderText({ 
    tryCatch({
      data() %...>% {
        dist <-summarise(.,dist = sum(distance)) 
        round(dist$dist) %>%
          format(n_small=1, big.mark=',')
      } %...!% {
        # if error
        return('')
      }
      
    },
    error = function(e){
      return('')
    })
  })
  
  output$nameOutput <- renderText({ tryCatch({
    data() %...>% {
      head(.,1) %>%
        pull(player_display_name)
    } %...!% {
      # if error
      return('')
    }
  },
  error = function(e){
    return('')
  })
  })
  
  output$dateOutput <- renderText({ tryCatch({
    data() %...>% {
      head(.,1) %>%
        pull(match_date)
    } %...!% {
      # if error
      return('')
    }
  },
  error = function(e){
    return('')
  })
  })
  
  
  output$mSpeedOutput <- renderText({
    tryCatch({
      data() %...>% {
        max_speed <- summarise(.,max_speed = max(speed_m_s, na.rm=TRUE)) 
        round(max_speed$max_speed, digits = 2) 
      } %...!% {
        # if error
        return('')
      }
    },
    error = function(e){
      return('')
    })
  })
  
  output$hSpeedOutput <- renderText({
    tryCatch({
      data() %...>% {
        if(str_detect(.$time[1], '^\\d{2}:\\d{2}.\\d{1,}$')){
          time_diff <-  as.numeric(strptime(x = .$time[2], format = "%M:%OS") - strptime(x =  .$time[1], format = "%M:%OS"))
        } else{
          # else time should be in format HH:MM:SS
          time_diff <-  as.numeric(strptime(x = .$time[2], format = "%H:%M:%OS") - strptime(x =  .$time[1], format = "%H:%M:%OS"))
        }
        
        hspeed <- filter(.,speed_m_s > 7) %>%
          summarise(time_high_speed = n() *time_diff)
        
        round(hspeed$time_high_speed,2)
        
        
      } %...!% {
        # if error
        return('')
      }
    },
    error = function(e){
      return('')
    })
  })
  
  
  
  output$accOutput <- renderText({
    tryCatch({
      data() %...>% {
        acc <- summarise(.,acc = sum(instantaneous_acceleration_impulse > 0), na.rm=T)
        format(acc$acc,n_small=1, big.mark=',')
      } %...!% {
        # if error
        return('')
      }
    },
    error = function(e){
      return('')
    }) 
  })
  
  output$decOutput <- renderText({ tryCatch({
    data() %...>% {
      decel <- summarise(.,decel = sum(instantaneous_acceleration_impulse < 0), na.rm=T) 
      format(decel$decel, n_small=1, big.mark=',')
    } %...!% {
      # if error
      return('')
    }
  },
  error = function(e){
    return('')
  })
    
  })
  
  output$hrOutput <- renderText({ tryCatch({
    data() %...>% {
      heart <- summarise(.,hr = max(heart_rate_bpm), na.rm=T)
      format(heart$hr, n_small=1, big.mark=',')
    } %...!% {
      # if error
      return('')
    }
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
      data() %...>% {
        dat <- filter(.,!is.na(lon)) %>%
          filter( !is.na(lat))
        
        rotate_pitch(dat$lat, dat$lon,lookup = pitch_lookup) %>%
          ggplot(aes(x= x, y = y)) +
          annotate_pitch(fill="#007700", colour="white") +
          theme_pitch() +
          stat_density2d(aes(fill=..level..),geom='polygon',colour=NA) + 
          scale_fill_continuous(low="yellow",high="red") +
          theme(plot.background = element_rect(fill = 'red'))
        
      }
    }, error= function(err){
      ggplot()
    })
    
  })
  
  
  #-------------------------------
  # TAB 3
  # player comparisons
  
  comparison_data <- reactive({
    tm$v
    input$time
    isolate({
      team_input <- local(input$team_input)
      match_input <- local(input$match_input)
      match_date_input <- local(input$date_input)
      max_time <- max(local(input$time))
      min_time <- min(local(input$time))
      
      future({
        # iwe have to open up a new connection and close it again within future
        pool <- initConnection()
        on.exit(pool::poolClose(pool))
        
        tbl(pool, 'positions2') %>%
          filter(team == team_input,
                 match_day == match_input,
                 match_date == match_date_input,
                 seconds/60 < max_time,
                 seconds/60 > min_time) %>% 
          collect 
      }) %...>% {
        xy <- rotate_pitch(.$lat, .$lon,lookup = pitch_lookup)
        bind_cols(., xy)
      }
      
      
      
      
      
    })
    
  })
  
  output$comparison_plot <- renderPlotly({
    comparison_data() %...>% {
      p <- group_by(.,player_display_name) %>%
        summarise(avg_x_position = mean(x),
                  avg_y_position = mean(y)) %>%
        
        ggplot() +
        annotate_pitch(fill="#007700", colour="white") +
        theme_pitch()+
        geom_point(aes(x = avg_x_position, y=avg_y_position, color = player_display_name)) +
        theme(legend.position = 'bottom',
              legend.title = element_blank()) 
      # make ggplotly
      ggplotly(p, width = 700, height = 450 )
    }
    
  })
  
  output$comparison_bar <- renderPlotly({
    comparison_data() %...>% {
      p <- group_by(.,player_display_name) %>%
        summarise(distance = sum(distance)) %>%
        ggplot() +
        geom_bar(aes(x = player_display_name, y=distance, fill=player_display_name), stat = 'identity') +
        geom_hline(aes(yintercept = mean(distance))) +
        ggthemes::theme_solarized(light=FALSE) +
        theme(legend.position = 'bottom',
              legend.title = element_blank()) 
      
      ggplotly(p)
    }
  })
  
}

onStop(function() {
  pool::poolClose(pool)
})
# Complete app with UI and server components
#shinyOptions(plot.autocolours=TRUE)
shinyApp(ui, server)

# profvis({runApp()})


