penguinSpp_pickerInput <- function(inputId) {
  pickerInput(inputId = inputId, label = "Select a species:",
              choices = c("Adelie", "Chinstrap", "Gentoo"),
              options = pickerOptions(actionsBox = TRUE),
              selected = c("Adelie", "Chinstrap", "Gentoo"),
              multiple = TRUE)
}