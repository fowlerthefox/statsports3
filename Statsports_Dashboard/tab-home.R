
# bring in our custom functions
source('./functions.R')

tagList(
  div(class='d-flex flex-column mb-3',
    div(class='card px-0 d-flex flex-column',
      div(class='card-header',
        h4('Description')
        ),
      div(class='card-body justify-content-between px-3',
          p('This viz aims to utilize raw GPS sensor data from football matches & training sessions 
            to provide insightful information with valuable findings for end users who are Physical 
            Performance Coaches, Sports Scientist and Head Coaches. It also offers the user to 
            interactively select a session by using the filters in the toggle options section')
          ),
      )
    ),
      
  div(class='d-flex flex-column mb-3',    
    div(class='card px-0 d-flex flex-column',
      div(class='card-header',
        h4('Player Stats')
        ),
      div(class='card-body justify-content-between px-3',
          p('This tab shows details of players distance covered, max speed, accelerations, 
            decelerations and max heart rate during the selected session. Also shows plots of distance, 
            heart rate and speed plotted over time')
          ),
      )
    ),
  
  div(class='d-flex flex-column mb-3',
    div(class='card px-0 d-flex flex-column',
      div(class='card-header',
        h4('Positioning')
        ),
      div(class='card-body justify-content-between px-3',
          p('This tab displays details of the selected players poisition in terms of a 
            heatmap plotted on a pitch during the session')
          ),
      )
    ),
  
  div(class='d-flex flex-column mb-3',
    div(class='card px-0 d-flex flex-column',
      div(class='card-header',
        h4('Team Comparison')
        ),
      div(class='card-body justify-content-between px-3',
          p('This tab provides visibility for a team view in terms of distance travelled,
            average poisiton, max speed and average heart rate for the selected session')
          ),
      )
    ),
)
