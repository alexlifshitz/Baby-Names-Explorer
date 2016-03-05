library(shiny)

states_list<-scan("data/states.txt", what="", sep="\n")
shinyUI(fluidPage(
     sidebarLayout(
          sidebarPanel(
               img(src='top10_logo_small.png', align = "center"),
               helpText("By Alexander Lifshitz"),
               helpText("Discover trends in baby names popularity by state, year and gender."),
               helpText("Explore how these trends evolve geographically from state to state."),
               helpText("For more details see", a("here", href="https://datarocks.shinyapps.io/Top10BabyNames/", target="_blank")),
               
               selectInput("state", 
                           label = "State", c("All States", states_list), selected = "CA"),
               sliderInput("years", 
                           label = "Born in",
                           min = 1910, max = 2014, value = 1910,sep = "", step=1,
                           animate = animationOptions(interval=3000, loop=F)),
               
               radioButtons("gender", label = "Gender",
                            #choices = list("All" = 1, "Female" = 2, "Male" = 3),
                            choices = c("Female", "Male"),
                            selected = "Female"),
               br(),
               br(),
               img(src='joke.jpg', align = "center")
          ),
          mainPanel(
               tabsetPanel(
                    tabPanel("Top10", h3(textOutput("text1"),align = "center"),
                             plotOutput("barplot"),h3(textOutput("text5"),align = "center")), 
                    tabPanel("Map Plot", h3(textOutput("text2"),align = "center"),
                             p(textOutput("text3"),align = "center"),
                             plotOutput("mapplot")), 
                    tabPanel("Data Table", h3(textOutput("text4"),align = "center"), dataTableOutput('table_all'))
                    #tabPanel("Line Plot", h3(textOutput("text4"),align = "center"), plotOutput("lineplot"))
               )
          )
     )
))