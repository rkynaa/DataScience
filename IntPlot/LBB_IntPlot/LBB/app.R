library(shiny, warn.conflicts = F)
library(shinydashboard, warn.conflicts =F)
library(scales, warn.conflicts = F)
library(magrittr, warn.conflicts = F)
library(graphics, warn.conflicts = F)
library(knitr, warn.conflicts = F)
library(rmdformats, warn.conflicts = F)
library(ggpubr, warn.conflicts = F)
library(ggthemes, warn.conflicts = F)
library(splines, warn.conflicts = F)
library(lubridate, warn.conflicts = F)
library(timevis, warn.conflicts = F)
library(network, warn.conflicts = F)
library(sna, warn.conflicts = F)
library(ggplot2, warn.conflicts = F)
library(igraph, warn.conflicts = F)
library(ndtv, warn.conflicts = F)
library(plotly, warn.conflicts = F)
library(GGally, warn.conflicts = F)
library(visNetwork, warn.conflicts = F)
library(tidyr, warn.conflicts = FALSE)

data <- read.csv("MarvelUniverse.csv")
data.conn <- read.csv("CharacterNetwork.csv")
data.time <- read.csv("MarvelTimeline.csv")
data.gen.mon <- data[,c("Movie","YearOfRelease","Budget", "DomesticScreens", "DomesticGross","WeekendGross","OverseasGross","WorldwideGross")]
data.gen.rat <- data[,c("Movie","YearOfRelease","RottenTomatoesTomato", "RottenTomatoesUser","IMDB", "CinemaScore","TrailerViews")]
data.gen.rat.tomato <- data[,c("Movie","RottenTomatoesTomato")]
data.gen.rat.audience <- data[,c("Movie","RottenTomatoesUser")]
data.gen.rat.imdb <- data[,c("Movie","IMDB")]
data.gen.rat.cinemascore <- data[,c("Movie","CinemaScore")]
data.gen.rat.trailerviews <- data[,c("Movie","TrailerViews")]

data.gen.mon.char <- data.gen.mon
data.gen.mon.char$DomesticGross <- paste("$", comma(data.gen.mon.char$DomesticGross))
data.gen.mon.char$WeekendGross <- paste("$", comma(data.gen.mon.char$WeekendGross))
data.gen.mon.char$OverseasGross <- paste("$", comma(data.gen.mon.char$OverseasGross))
data.gen.mon.char$WorldwideGross <- paste("$", comma(data.gen.mon.char$WorldwideGross))
data.gen.rat.trailerviews$TrailerViews <- paste(comma(data.gen.rat.trailerviews$TrailerViews),"views")
data.gen.mon.char$Budget <- paste("$", data.gen.mon.char$Budget, "million")

data.gen.mon.top10 <- head(data.gen.mon[order(-data.gen.mon$OverseasGross),],10)
data.gen.rat.tomato.top10 <- head(data.gen.rat.tomato[order(-data.gen.rat.tomato$RottenTomatoesTomato),],10)
data.gen.rat.audience.top10 <- head(data.gen.rat.audience[order(-data.gen.rat.audience$RottenTomatoesUser),],10)
data.gen.rat.imdb.top10 <- head(data.gen.rat.imdb[order(-data.gen.rat.imdb$IMDB),],10)
#data.gen.rat.trailerviews.top10 <- head(data.gen.rat.trailerviews[order(-data.gen.rat.trailerviews$TrailerViews),],10)

data.time$Time <- year(as.Date(as.character(data.time$Time), format = "%Y"))
data.time$Timeline <- year(as.Date(as.character(data.time$Timeline), format = "%Y"))
data.time$MainHero......................... <- gsub("[;]","",data.time$MainHero.........................)
data.time$MainHero <- as.factor(data.time$MainHero)

igraph.options(size = 50)

colnames(data.time) <- c("Movie","Time","Timeline","MainHero")

main_hero <- levels(data.time$MainHero)
main_hero_color <- c("brown2","azure3","blueviolet","darkblue","palegoldenrod","gold","deepskyblue","chartreuse3","red3","red","orangered")
data.time$MainHero<- factor(data.time$MainHero,levels = main_hero, ordered = T)
data.time.df.timeMovie <- data.frame(
    id = 1:22,
    content = data.time$Movie,
    start = data.time$Time,
    end = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
)

data.time.df.timeStory <- data.frame(
    id = 1:22,
    content = data.time$Movie,
    start = data.time$Timeline,
    end = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
)

conn <- data.frame(
    source = data.conn$Hero1,
    target = data.conn$Hero2
)

conn.viznet <- data.frame(
    from = data.conn$Hero1,
    to = data.conn$Hero2,
    type = "mention",
    weight = 1
)

data.gen.mon.top10.char <- data.gen.mon.top10
data.gen.mon.top10.char$DomesticGross <- paste("$", comma(data.gen.mon.top10.char$DomesticGross))
data.gen.mon.top10.char$WeekendGross <- paste("$", comma(data.gen.mon.top10.char$WeekendGross))
data.gen.mon.top10.char$OverseasGross <- paste("$", comma(data.gen.mon.top10.char$OverseasGross))
data.gen.mon.top10.char$WorldwideGross <- paste("$", comma(data.gen.mon.top10.char$WorldwideGross))

ui <-
    dashboardPage(
        dashboardHeader(
            title = "MCU"
        ),
        dashboardSidebar(
            sidebarMenu(
                menuItem(
                    "General",
                    icon = icon(
                        "atlas"
                    ),
                    menuItem(
                        "List of Movies",
                        icon = icon(
                            "clipboard-list"
                        ),
                            menuItem(
                                "Generally",
                                icon = icon(
                                    "newspaper"
                                ),
                                tabName = "tab_list_movies_gen"
                            ),
                            menuItem(
                                "Year of Release",
                                icon = icon(
                                    "calendar-alt"
                                ),
                                tabName = "tab_list_movies_year"
                            )
                        ),
                        menuItem(
                            "Top 10",
                            icon = icon(
                                "medal"
                            ),
                            tabName = "tab_top10"
                        )
                    ),
                menuItem(
                    "Inside MCU",
                    icon = icon(
                        "film"
                    ),
                    menuItem(
                        "Connections",
                        icon = icon(
                            "user-friends"
                        ),
                        tabName = "tab_conn"
                    ),
                    menuItem(
                        "Timeline",
                        icon = icon(
                            "hourglass"
                        ),
                        tabName = "tab_timeline"
                    )
                )
            )
        ),
        dashboardBody(
            tabItems(
                tabItem(
                    tabName = "tab_list_movies_gen",
                    fluidRow(
                        selectInput(
                            inputId = "selectId",
                            label = "Choose what type of dataset",
                            choices = c("Gross", "Ratings")
                        ),
                        tabBox(width = 12,
                            tabPanel(
                                "Graph",
                                fluidRow(
                                    box(
                                        width = 10,
                                        plotOutput(
                                            "plot_list_movie_gen"
                                        )
                                    )
                                ),
                                fluidRow(
                                    valueBoxOutput(
                                        "valueBoxOutputGen1"
                                    ),
                                    valueBoxOutput(
                                        "valueBoxOutputGen2"
                                    ),
                                    valueBoxOutput(
                                        "valueBoxOutputGen3"
                                    )
                                )
                            ),
                            tabPanel(
                                "Analytics",
                                dataTableOutput(
                                    "table_analytics_gen"
                                )
                            )
                        )
                    )
                ),
                tabItem(
                    tabName = "tab_list_movies_year",
                    fluidRow(
                        selectInput(
                            inputId = "yearId",
                            label = "Choose Year of Release",
                            choices = levels(
                                as.factor(data.gen.mon$YearOfRelease)
                                )
                            ),
                        tabBox(
                            width = 12,
                            tabPanel(
                                "Graph",
                                fluidRow(
                                    box(
                                        width = 10,
                                        plotlyOutput(
                                            "plot_list_movie_year"
                                        )
                                    )
                                ),
                                fluidRow(
                                    valueBoxOutput(
                                        "valueBoxOutputYearly1"
                                    ),
                                    valueBoxOutput(
                                        "valueBoxOutputYearly2",width = 7
                                    )
                                )
                            ),
                            tabPanel(
                                "Analytics",
                                dataTableOutput(
                                    "table_analytics_year"
                                )
                            )
                        )
                    )
                ),
                tabItem(
                    tabName = "tab_top10",
                    fluidRow(
                        selectInput(
                            inputId = "selectId_top10",
                            label = "Choose what type of dataset",
                            choices = c("Gross", "Rotten Tomatoes' Tomatometer", "Rotten Tomatoes' Audience Score", "IMDB")
                        ),
                        tabBox(
                            width = 12,
                            tabPanel(
                                "Graph",
                                fluidRow(
                                    box(
                                        width = 10,
                                        plotlyOutput(
                                            "plot_list_movie_top10"
                                        )
                                    )
                                ),
                                fluidRow(
                                    valueBoxOutput(
                                        "valueBoxOutputTop10_1",width = 5
                                    ),
                                    valueBoxOutput(
                                        "valueBoxOutputTop10_2",width = 5
                                    ),
                                    valueBoxOutput(
                                        "valueBoxOutputTop10_3",width = 5
                                    )
                                )
                            ),
                            tabPanel(
                                "Analytics",
                                dataTableOutput(
                                    "table_analytics_top10"
                                )
                            )
                        )
                    )
                ),
                tabItem(
                    tabName = "tab_conn",
                    fluidRow(
                        box(
                            title = "Connection between every characters in Marvel Cinematic Universe",
                            width = 15,
                            plotOutput(
                                "plot_conn_char"
                            )
                        )
                    )
                ),
                tabItem(
                    tabName = "tab_timeline",
                    fluidRow(
                        box(title = "According to Every Movie's Release Year",
                            width = 10,
                            timevisOutput(
                                "plot_timeline_movie"
                            )
                        )
                    ),
                    fluidRow(
                        box(title = "According to The Story",
                            width = 10,
                            timevisOutput(
                                "plot_timeline_story"
                            )
                        )
                    )
                )
            )
        )
    )

server <- function(input, output, session) {
    # Output for table_analytics_gen
    output$table_analytics_gen <- renderDataTable(
        re_table_analytics_gen()
    )
    
    # Reactive for table_analytics_gen
    re_table_analytics_gen <- reactive({
        if(input$selectId == "Gross"){ 
            data.gen.mon.char
        } else { 
            data.gen.rat
        }
    })
    
    # Output for plot_list_movie_gen
    output$plot_list_movie_gen <- renderPlot(
        re_plot_gen()
    )
    
    # Reactive for plot_list_movie_gen
    re_plot_gen <- reactive({
        if(input$selectId == "Gross"){
            options(scipen = 99)
            plot.list <- 
                aggregate.data.frame(list(DomesticGross = data.gen.mon$DomesticGross,
                                          OverseasGross = data.gen.mon$OverseasGross,
                                          WorldwideGross= data.gen.mon$WorldwideGross),
                                     by = list(data.gen.mon$Movie),
                                     mean) %>% 
                gather(key = "var", value = "Val_in_mil", -Group.1) %>% 
                ggplot(aes(x = Group.1, y = Val_in_mil)) +
                geom_col(aes(fill = var), position = "dodge") + 
                coord_flip() +
                labs(title = "List of Marvel Cinematic Universe Movies", x = NULL, y = NULL, fill = "Ratio") +
                theme_economist() +
                theme(legend.position = "right", plot.title = element_text(hjust = .5)) +
                scale_fill_discrete(labels = c("Domestic Gross", "Overseas Gross", "Worldwide Gross"))
            plot.list
        } else { 
            plot.list <- aggregate.data.frame(list(RottenTomatoesTomato = data.gen.rat$RottenTomatoesTomato,
                                                   RottenTomatoesUser = data.gen.rat$RottenTomatoesUser,
                                                   IMDB = data.gen.rat$IMDB*10),
                                              by = list(data.gen.rat$Movie),
                                              mean) %>% 
                gather(key = "var", value = "rating", -Group.1) %>% 
                ggplot(aes(x = Group.1, y = rating)) +
                geom_col(aes(fill = var), position = "dodge") + 
                coord_flip() +
                labs(title = "List of Marvel Cinematic Universe Movies", x = NULL, y = NULL, fill = "Ratio") +
                theme_economist() +
                theme(legend.position = "right", plot.title = element_text(hjust = .5)) +
                scale_fill_discrete(labels = c("IMDB","Tomatometer", "Audience Score"))
            plot.list
        }
    })
    
    # Output for valueBoxOutputGen1
    output$valueBoxOutputGen1 <- renderValueBox({
        print(re_valueBoxOutputGen1())
    })
    
    # Reactiv
    re_valueBoxOutputGen1 <- reactive({
        if (input$selectId == "Gross") {
            valueBox(
                paste0(
                    "$", comma(mean(data.gen.mon$DomesticGross))
                ), "Domestic Gross in Average", icon = icon("dollar-sign"),
                color = "green"
            )
        } else {
            valueBox(
                paste0(
                    as.integer(mean(data.gen.rat$RottenTomatoesTomato)), "%"
                ), "Rotten Tomatoes' Tomatometer in Average", icon = icon("star"),
                color = "red"
            )
        }
    })
    
    # Output for valueBoxOutputGen2
    output$valueBoxOutputGen2 <- renderValueBox({
        print(re_valueBoxOutputGen2())
    })
    
    # Reactive for valueBoxOutputGen2
    re_valueBoxOutputGen2 <- reactive({
        if (input$selectId == "Gross") {
            valueBox(
                paste0(
                    "$", comma(mean(data.gen.mon$OverseasGross))
                ), "Overseas Gross in Average", icon = icon("passport"),
                color = "blue"
            )
        } else {
            valueBox(
                paste0(
                    as.integer(mean(data.gen.rat$RottenTomatoesUser)), "%"
                ), "Rotten Tomatoes' Audience Score in Average", icon = icon("user"),
                color = "red"
            )
        }
    })
    # Output for valueBoxOutputGen3
    output$valueBoxOutputGen3 <- renderValueBox({
        print(re_valueBoxOutputGen3())
    })
    
    # Reactive for valueBoxOutputGen3
    re_valueBoxOutputGen3 <- reactive({
        if (input$selectId == "Gross") {
            valueBox(
                paste0(
                    "$", comma(mean(data.gen.mon$WorldwideGross))
                ), "Worldwide Gross in Average", icon = icon("globe-americas"),
                color = "yellow"
            )
        } else {
            valueBox(
                paste0(
                    format(round(mean(data.gen.rat$IMDB),1), nsmall = 1)
                ), "IMDb Rate in Average", icon = icon("imdb"),
                color = "yellow"
            )
        }
    })
    
    # output for valueBoxOutputYearly1
    output$valueBoxOutputYearly1 <- renderValueBox({
        print(re_valueBoxOutputYearly1())
    })
    
    # reactive for valueBoxOutputYearly1
    re_valueBoxOutputYearly1 <- reactive({
        valueBox(
            paste0(nrow(data.gen.mon[data.gen.mon$YearOfRelease == input$yearId,]), " out of ", nrow(data.gen.mon)), paste("Total movies in", input$yearId), icon = icon("list"),
            color = "blue"
        )
    })
    
    # output for valueBoxOutputYearly1
    output$valueBoxOutputYearly2 <- renderValueBox({
        print(re_valueBoxOutputYearly2())
    })
    
    # reactive for valueBoxOutputYearly1
    re_valueBoxOutputYearly2 <- reactive({
        temp <- data.gen.mon[data.gen.mon$YearOfRelease == input$yearId,]
        temp <- head(temp[order(-temp$WorldwideGross),],1)
        valueBox(
            paste0(temp$Movie), paste("Biggest Worldwide Gross in", input$yearId), icon = icon("dollar-sign"),width = 15,
            color = "green"
        )
    })
    
    # output for valueBoxOutputTop10_1()
    output$valueBoxOutputTop10_1 <- renderValueBox({
        print(re_valueBoxOutputTop10_1())
    })
    
    # reactive for valueBoxOutputTop10_1
    re_valueBoxOutputTop10_1 <- reactive({
        if(input$selectId_top10 == "Gross"){
            options(scipen = 99)
            plot.list <- 
                aggregate.data.frame(list(DomesticGross = data.gen.mon.top10$DomesticGross),
                                     by = list(data.gen.mon.top10$Movie),
                                     mean) %>% 
                gather(key = "var", value = "Val_in_mil", -Group.1) 
            plot.list <- head(plot.list[order(-plot.list$Val_in_mil),],1)
            valueBox(
                paste0(plot.list$Group.1), "Biggest Domestic Gross", icon= icon("dollar-sign"),width = 15,
                color = "green"
            )
        } else { 
            plot.list <- 
            options(scipen = 99)
            plot.list <- head(data.gen.rat.tomato.top10[order(-data.gen.rat.tomato.top10$RottenTomatoesTomato),],1)
            valueBox(
                paste0(plot.list$Movie), paste0("Biggest Tomatometer: ", plot.list$RottenTomatoesTomato,"%"), icon = icon("star"),width = 15,
                color = "red"
            )
        }
    })
    
    # output for valueBoxOutputTop10_2()
    output$valueBoxOutputTop10_2 <- renderValueBox({
        print(re_valueBoxOutputTop10_2())
    })
    
    # reactive for valueBoxOutputTop10_2
    re_valueBoxOutputTop10_2 <- reactive({
        if(input$selectId_top10 == "Gross"){
            options(scipen = 99)
            plot.list <- 
                aggregate.data.frame(list(OverseasGross = data.gen.mon.top10$OverseasGross),
                                     by = list(data.gen.mon.top10$Movie),
                                     mean) %>% 
                gather(key = "var", value = "Val_in_mil", -Group.1) 
            plot.list <- head(plot.list[order(-plot.list$Val_in_mil),],1)
            valueBox(
                paste0(plot.list$Group.1), "Biggest Overseas Gross", icon= icon("dollar-sign"),width = 15,
                color = "green"
            )
        } else { 
            plot.list <- 
                options(scipen = 99)
            plot.list <- head(data.gen.rat.audience.top10[order(-data.gen.rat.audience.top10$RottenTomatoesUser),],1)
            valueBox(
                paste0(plot.list$Movie), paste0("Biggest Audience Score: ", plot.list$RottenTomatoesUser,"%"), icon = icon("user"),width = 15,
                color = "red"
            )
        }
    })
    
    # output for valueBoxOutputTop10_3()
    output$valueBoxOutputTop10_3 <- renderValueBox({
        print(re_valueBoxOutputTop10_3())
    })
    
    # reactive for valueBoxOutputTop10_3
    re_valueBoxOutputTop10_3 <- reactive({
        if(input$selectId_top10 == "Gross"){
            options(scipen = 99)
            plot.list <- 
                aggregate.data.frame(list(WorldwideGross = data.gen.mon.top10$WorldwideGross),
                                     by = list(data.gen.mon.top10$Movie),
                                     mean) %>% 
                gather(key = "var", value = "Val_in_mil", -Group.1) 
            plot.list <- head(plot.list[order(-plot.list$Val_in_mil),],1)
            valueBox(
                paste0(plot.list$Group.1), "Biggest Worldwide Gross", icon= icon("dollar-sign"),width = 15,
                color = "green"
            )
        } else { 
            plot.list <- 
                options(scipen = 99)
            plot.list <- head(data.gen.rat.imdb.top10[order(-data.gen.rat.imdb.top10$IMDB),],1)
            valueBox(
                paste0(plot.list$Movie), paste0("Biggest IMDb Rating: ", plot.list$IMDB), icon = icon("imdb"),width = 15,
                color = "yellow"
            )
        }
    })
    
    output$table_analytics_year <- renderDataTable(
        data.gen.mon.char %>% 
            filter(YearOfRelease == input$yearId)
    )
    
    output$plot_list_movie_year <- renderPlotly({
        plot.list <- data.gen.mon %>% 
            filter(YearOfRelease == input$yearId)
        plot.list <- aggregate.data.frame(list(DomesticGross = plot.list$DomesticGross,
                                               OverseasGross = plot.list$OverseasGross,
                                               WorldwideGross= plot.list$WorldwideGross),
                                          by = list(plot.list$Movie),
                                          mean) %>% 
            gather(key = "var", value = "Val_in_mil", -Group.1) %>% 
            ggplot(aes(x = Group.1, y = Val_in_mil, text = paste("$", comma(Val_in_mil)))) +
            geom_col(aes(fill = var), position = "dodge") + 
            coord_flip() +
            labs(title = "List of Marvel Cinematic Universe Movies", x = NULL, y = NULL, fill = "Ratio") +
            theme_economist() +
            theme(legend.position = "right", plot.title = element_text(hjust = .5)) +
            scale_fill_discrete(labels = c("Domestic Gross", "Overseas Gross", "Worldwide Gross"))
        print(ggplotly(plot.list, tooltip = "text"))
    })
    
    output$plot_list_movie_gen <- renderPlot(
        {
            re_plot_gen()
        }
    )
    
    output$plot_list_movie_gen <- renderPlot({
        re_plot_gen()
    })
    
    re_plot_gen_top10 <- reactive({
        if(input$selectId_top10 == "Gross"){
            options(scipen = 99)
            plot.list <- 
                aggregate.data.frame(list(DomesticGross = data.gen.mon.top10$DomesticGross,
                                          OverseasGross = data.gen.mon.top10$OverseasGross,
                                          WorldwideGross= data.gen.mon.top10$WorldwideGross),
                                     by = list(data.gen.mon.top10$Movie),
                                     mean) %>% 
                gather(key = "var", value = "Val_in_mil", -Group.1) %>% 
                ggplot(aes(x = Group.1, y = Val_in_mil, text = paste("$",comma(Val_in_mil)))) +
                geom_col(aes(fill = var), position = "dodge") + 
                coord_flip() +
                labs(title = "List of Marvel Cinematic Universe Movies", x = NULL, y = NULL, fill = "Ratio") +
                theme_economist() +
                theme(legend.position = "right", plot.title = element_text(hjust = .5)) +
                scale_fill_discrete(labels = c("Domestic Gross", "Overseas Gross", "Worldwide Gross"))
            ggplotly(plot.list, tooltip = "text")
        } else if (input$selectId_top10 == "Rotten Tomatoes' Tomatometer") { 
            plot.list <- 
                ggplot(data.gen.rat.tomato.top10, aes(x = reorder(Movie, RottenTomatoesTomato), y = RottenTomatoesTomato, text = paste("Rating", RottenTomatoesTomato))) +
                geom_col(fill = "#FF9999") +
                coord_flip() +
                labs(title = "List of Marvel Cinematic Universe Movies", x = NULL, y = NULL, fill = "Ratio") +
                theme_economist() +
                theme(legend.position = "none", plot.title = element_text(hjust = .5))
            ggplotly(plot.list, tooltip = "text")
        } else if (input$selectId_top10 == "Rotten Tomatoes' Audience Score") { 
            plot.list <-
                ggplot(data.gen.rat.audience.top10, aes(x = reorder(Movie, RottenTomatoesUser), y = RottenTomatoesUser, text = paste("Rating", RottenTomatoesUser))) +
                geom_col(fill = "#FF9999") + 
                coord_flip() +
                labs(title = "List of Marvel Cinematic Universe Movies", x = NULL, y = NULL, fill = "Ratio") +
                theme_economist() +
                theme(legend.position = "none", plot.title = element_text(hjust = .5))
            ggplotly(plot.list, tooltip = "text")
        } else { 
            plot.list <- 
                ggplot(data.gen.rat.imdb.top10, aes(x = reorder(Movie, IMDB), y = IMDB, text = paste("Rating", IMDB))) +
                geom_col(fill = "#F0E442") + 
                coord_flip() +
                labs(title = "List of Marvel Cinematic Universe Movies", x = NULL, y = NULL, fill = "Ratio") +
                theme_economist() +
                theme(legend.position = "none", plot.title = element_text(hjust = .5))
            ggplotly(plot.list, tooltip = "text")
        }
    })
    
    re_table_analytics_top10 <- reactive({
        if(input$selectId_top10 == "Gross"){ 
            data.gen.mon.top10.char
        } else if (input$selectId_top10 =="Rotten Tomatoes' Tomatometer"){ 
            data.gen.rat.tomato.top10
        } else if (input$selectId_top10 =="Rotten Tomatoes' Audience Score"){
            data.gen.rat.audience.top10
        } else {
            data.gen.rat.imdb.top10
        }
    })
    
    output$table_analytics_top10 <- renderDataTable(
        re_table_analytics_top10()
    )
    
    output$plot_list_movie_top10 <- renderPlotly(
        {
            print(re_plot_gen_top10())
        }
    )
    
    output$plot_conn_char <- renderPlot({
        network_conn <- graph_from_data_frame(d = conn, directed = F)
        plot(network_conn)
    })
    
    output$plot_timeline_movie <- renderTimevis(
        timevis(data.time.df.timeMovie)
    )
    
    output$plot_timeline_story <- renderTimevis(
        timevis(data.time.df.timeStory)
    )
}

shinyApp(ui, server)