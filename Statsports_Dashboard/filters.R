div(class = 'card-body',
    # Sidebar with a slider input
    fluidRow(class = 'justify-content-between',
             column(3,
                    fluidRow(selectInput('team_input', 'Team', choices = team_choices))
             ),
             column(3,
                    fluidRow(selectInput('player_input', 'Player', choices = player_choices))
             ),
             column(3,
                    fluidRow(selectInput('match_input', 'Match', choices = match_choices))
             ),
    ),
    fluidRow(class = 'justify-content-between',
             column(4,
                    fluidRow(selectInput('date_input', 'Match Date', choices = date_choices))
             ),
             column(6,
                    sliderInput("time","time since start:", 
                                min = min(seconds, na.rm = T),
                                max = max(seconds, na.rm = T),
                                value = c(min(seconds, na.rm = T),
                                          max(seconds, na.rm = T)),round=TRUE))
    )
)