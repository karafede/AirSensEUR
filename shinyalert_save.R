library(shiny)
library(shinyalert)

ui <- fluidPage(
    useShinyalert()
)

server <- function(input, output) {
    r <- reactiveVal()
    shinyalert(
        inputId = "Essai",
        title = "Hello",
        text = "This is a modal",
        closeOnEsc = FALSE,
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "input",
        inputType = "text",
        inputValue = "",
        inputPlaceholder = "",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE, 
        callbackR = function(value) { 
            cat(paste("Welcome", value,"\n"))
            r(value)
            print(r)
        }
    )
    
    
}

shinyApp(ui, server)
