

tagList(
  fluidRow(class = "justify-content-between",
           div(id ="playerStats", class='card card-body text-white bg-secondary mb-3 col-sm-8 mx-auto',
               div(class="mb-2 display-3", textOutput("nameOutput") ),
               div(class="mb-2 h1 font-weight-lighter", textOutput("dateOutput")), 
               tags$table(class="table table-dark",
                          tags$tbody(
                            tags$tr(
                              tags$td("Distance:"),
                              textOutput("distanceOutput", container = tags$td)
                            ),
                            tags$tr(
                              tags$td("Max speed:"),
                              textOutput("mSpeedOutput", container = tags$td)
                            ),
                            tags$tr(
                              tags$td("Time spent over 7m/s:"),
                              textOutput("hSpeedOutput", container = tags$td)
                            ),
                            tags$tr(
                              tags$td("Number of Accelerations:"),
                              textOutput("accOutput", container = tags$td)
                            ),
                            tags$tr(
                              tags$td("Number of Decelerations:"),
                              textOutput("decOutput", container = tags$td)
                            )
                          )
               )
           ),
           div(id="stat-boxes", class= "mx-auto d-flex flex-column mb-3",
               div(class="card card-body bg-info d-flex align-items-center mb-2",
                   icon("heartbeat", class="mx-auto fa-3x py-2"),
                   div(class="display-4 mb-1",
                       textOutput("hrOutput", container = tags$span)
                   ),
                   div(class="h3 font-weight-lighter", "Max heart rate")
               ),
               div(class="card card-body bg-warning d-flex align-items-center",
                   icon("running", class="mx-auto fa-3x py-2"),
                   div(class="display-4 mb-1",
                       textOutput("distanceOutput2", container = tags$span)
                   ),
                   div(class="h3 font-weight-lighter","Distance covered")
               )
           )
  ),
  div(class = "d-flex flex-row flex-wrap justify-content-between mx-auto",
      div(class='card mr-1 mb-3 px-0 flex-grow-1',
          div(class='card-header',
              h4('Heart Rate')
          ),
          div(class='card-body justify-content-between px-3',
              plotOutput('hrPlot')
          )
      ),
      div(class='card mr-1 mb-3 px-0 flex-grow-1',
          div(class='card-header',
              h4('Total Distance')
          ),
          div(class='card-body justify-content-between px-3',
              plotOutput('distPlot')
          )
      ),
      div(class='card mb-3 px-0 flex-grow-1',
          div(class='card-header',
              h4('Speed')
          ),
          div(class='card-body justify-content-between px-3',
              plotOutput('speedPlot')
          )
      )
  )
)
