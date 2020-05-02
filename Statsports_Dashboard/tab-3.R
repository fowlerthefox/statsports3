
tagList(
  div(class='d-flex flex-column mb-3',
    div(class='card px-0 d-flex flex-column',
      div(class='card-header',
          h4('Average Position')
          ),
      div(class='card-body px-3',
          plotlyOutput('comparison_plot')
          ),
      ),
  ),
  
  div(class='d-flex flex-column mb-3',
    div(class='card px-0 d-flex flex-column',
      div(class='card-header',
          h4('Distance Covered')
          ),
      div(class='card-body px-3',
          plotlyOutput('comparison_bar')
          ),
      ),
    ),
  
  div(class='d-flex flex-column mb-3',
    div(class='card px-0 d-flex flex-column',    
      div(class='card-header',
          h4('Max Speed')
          ),
      div(class='card-body px-3',
          plotlyOutput('comparison_speed')
          ),
      ),
    ),

  div(class='d-flex flex-column mb-3',
    div(class='card px-0 d-flex flex-column',            
      div(class='card-header',
          h4('Average Heart Rate')
          ),
      div(class='card-body px-3',
          plotlyOutput('comparison_heartrate')
          )
      )
    )
  )

