server <- function(input, output) {
  
  # render the scatterplot output ----
  output$penguin_scatterplot <- build_penguin_scatterplot(input)
  
  # render the histogram output ----
  output$penguin_histogram <- build_penguin_histogram(input)
  
} # END server