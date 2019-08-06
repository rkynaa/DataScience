library(shiny)
library(shinydashboard)

vids <- read.csv("youtubetrends.csv")

ui <- fluidPage(
    dashboardPage(
        dashboardHeader(
            title = "Youtube Trends"
        ),
        dashboardSidebar(),
        dashboardBody()
    )
)

server <- function(input, output, session) {
}

shinyApp(ui, server)