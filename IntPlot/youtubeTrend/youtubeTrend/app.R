library(shiny)
library(shinydashboard)
library(ggplot2)
library(scales)

vids <- read.csv("youtubetrends.csv")

ui <-
    dashboardPage(
        dashboardHeader(
            title = "Youtube Trends"
        ),
        dashboardSidebar(
            sidebarMenu(
                menuItem("Category", tabName = "tab_category", icon = icon("atlas")),
                menuItem("Table", tabName = "tab_table", icon = icon("table"))
            )
        ),
        dashboardBody(
            tabItems(
                tabItem(
                    tabName = "tab_category",
                    fluidRow(
                        selectInput(
                            inputId = "categoryid",
                            label = "Choose Category",
                            choices = levels(
                                vids$category_id
                                )
                            ),
                        plotOutput(
                            "plot_category"
                            )
                        )
                    ),
                tabItem(
                    tabName = "tab_table",
                    fluidRow(
                        dataTableOutput(
                            "table"
                            )
                        )
                    )
                )
            )
        )

server <- function(input, output, session) {
    output$plot_category <- renderPlot(
        {
            options(scipen = 99)
            plot2 <- vids %>% 
                filter(category_id == input$categoryid) %>% 
                mutate(publish_wday = factor(publish_wday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>% 
                group_by(publish_wday, publish_when) %>% 
                summarise(total_views = sum(views)) %>%
                ggplot(aes(publish_wday, y = total_views, text = paste(comma(total_views), "views","\n","Published on",publish_when))) + 
                geom_col(aes(fill = publish_when), position = "dodge") + 
                labs(x = NULL, y = NULL, title = "Total Views Based on Publish Time") +
                theme(legend.title = element_blank()) +
                scale_y_continuous(labels = comma)
            #plot2.ly <- ggplotly(plot2, tooltip = "text")
            #plot2.ly
            plot2
        }
        )
    output$table <- renderDataTable(vids, options = list(scrollX = T))
}

shinyApp(ui, server)