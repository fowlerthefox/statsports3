
tagList(
  div(class='d-flex flex-column',
      div(class='card-body px-3',
          plotlyOutput('comparison_plot')
      ),
      div(class='card-body px-3',
          plotlyOutput('comparison_bar')
      )
  )
)