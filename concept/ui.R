#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)

teams <- as.list(c("Team SoloMid", "Immortals", "Counter Logic Gaming", "Cloud 9", "Dignitas", "Team EnvyUs", "Echo Fox", "Team Liquid", "Phoenix1"))

sidebar <- dashboardSidebar(
  width = 200,
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("2015 Split", tabName = "2015", icon = icon("calendar")),
    menuItem("2016 Split", tabName = "2016", icon = icon("calendar")),
    menuItem("Game Forecast", tabName = "tool", icon = icon("wrench"))
  )
)

body <- dashboardBody(
  
  tags$head(
    tags$script(
      HTML("
           window.onload = function() {
           resize();
           }
           window.onresize = function() {
           resize();
           }
           Shiny.addCustomMessageHandler ('triggerResize',function (val) {
           window.dispatchEvent(new Event('resize'));
           });
           function resize(){
           var h = window.innerHeight - $('.navbar').height() - 150; // Get dashboardBody height
           $('#box').height(h); 
           }"
      )
      )
      ), #end html
  
  tabItems(
    tabItem(tabName = "dashboard",
            img(src = "dashboard.png", width = "100%"), align = "center"
    ),
    
    tabItem(tabName = "2015",
            fluidPage(
                
                img(src = "tsmbanner.jpg", width = "100%"),
                align = "center", 
                br(),br(),br(),
                tabBox(title = "KDA/Week", id = "tab1", height = 400, 
                       tabPanel("Spring", plotlyOutput("spring2015WTKDA", height = 350)),
                       tabPanel("Summer", plotlyOutput("summer2015WTKDA", height = 350))),
              
                tabBox(title = "KDA Rank", id = "tab2", height = 400, 
                       tabPanel("Spring", plotlyOutput("spring2015KDARank", height = 350)),
                       tabPanel("Summer", plotlyOutput("summer2015KDARank", height = 350))),
              
                tabBox(title = "CSM Rank", id = "tab3", height = 400, 
                       tabPanel("Spring", plotlyOutput("spring2015CSMRank", height = 350)),
                       tabPanel("Summer", plotlyOutput("summer2015CSMRank", height = 350))),
              
                tabBox(title = "Most Deaths", id = "tab4", height = 400, 
                       tabPanel("Spring", plotlyOutput("spring2015Deaths", height = 350)),
                       tabPanel("Summer", plotlyOutput("summer2015Deaths", height = 350)))
              )
            ),
    
    tabItem(tabName = "2016",
            fluidPage(
              img(src = "imtbanner.png", width = "100%"), align = "center", 
              br(),br(),br(),
              tabBox(title = "KDA/Week", id = "tab1", height = 400, 
                     tabPanel("Spring", plotlyOutput("spring2016WTKDA", height = 350)),
                     tabPanel("Summer", plotlyOutput("summer2016WTKDA", height = 350))),
              #box(title = "Spring 2015 KDA/Week", status = "primary", solidHeader = TRUE, plotlyOutput("spring2015WTKDA", height = 250)),
              tabBox(title = "KDA Rank", id = "tab2", height = 400, 
                     tabPanel("Spring", plotlyOutput("spring2016KDARank", height = 350)),
                     tabPanel("Summer", plotlyOutput("summer2016KDARank", height = 350))),
              #box(title = "KDA Rank", status = "primary", solidHeader = TRUE, plotlyOutput("spring2015KDARank", height = 400)),
              
              tabBox(title = "CSM Rank", id = "tab3", height = 400, 
                     tabPanel("Spring", plotlyOutput("spring2016CSMRank", height = 350)),
                     tabPanel("Summer", plotlyOutput("summer2016CSMRank", height = 350))),
              #box(title = "CSM Rank", status = "primary", solidHeader = TRUE, plotlyOutput("spring2015CSMRank", height = 400)),
              
              tabBox(title = "Most Deaths", id = "tab4", height = 400, 
                     tabPanel("Spring", plotlyOutput("spring2016Deaths", height = 350)),
                     tabPanel("Summer", plotlyOutput("summer2016Deaths", height = 350)))
            )
    ),
    
    tabItem(tabName = "tool",
            fluidPage(
              img(src = "flybanner.jpg", width = "100%"), align = "center",
              fluidRow(
                column(12, h3("Player Index Performance:") ,br(), align = "center")),
              fluidRow(
                column(1),
                column(2, img(src = "FLYBALLS.jpg", width = 100)),
                column(2, img(src = "FLYMOON.png", width = 100)),
                column(2, img(src = "FLYHAI.jpg", width = 100)),
                column(2, img(src = "FLYWT.png", width = 100)),
                column(2, img(src = "FLYLEMON.jpg", width = 100)),
                column(1),
                align = "center"),
              fluidRow(
                column(1),
                column(2, numericInput("num_balls",label = tags$b("Balls"),value = 1, min = 0.1, max = 2, step = 0.1, width = 100)),
                column(2, numericInput("num_moon",label = tags$b("Moon"),value = 1, min = 0.1, max = 2, step = 0.1, width = 100)),
                column(2, numericInput("num_hai",label = tags$b("Hai"),value = 1, min = 0.1, max = 2, step = 0.1, width = 100)),
                column(2, numericInput("num_wt",label = tags$b("WildTurtle"),value = 1, min = 0.1, max = 2, step = 0.1, width = 100)),
                column(2, numericInput("num_lemon",label = tags$b("LemonNation"),value = 1, min = 0.1, max = 2, step = 0.1, width = 100)),
                column(1), br(),
                align = "center"),
             
              fluidRow(
                column(2, offset = 2, numericInput("num_drag",label = tags$b("Dragon % Secured"), value = 0, min = 0, max = 100, step = 10, width = 150)),
                column(4, numericInput("num_time",label = tags$b("Game Length"), value = 30, min = 20, max = 60, step = 5, width = 150)),
                column(2, offset = 0, numericInput("num_baron",label = tags$b("Barons % Secured"), value = 0, min = 0, max = 100, step = 10, width = 150)),
                align = "center"
              ),
              fluidRow(
                column(12,
                       selectInput(
                         "team",
                         label = h3("Opponent:"),
                         choices = teams
                       )
                ),
              fluidRow(
                column(12, imageOutput("opp", height = "100px"))
              ),
              fluidRow(
                column(12 , br(),br(), actionButton("action_Calc", label = "Refresh & Calculate"), align = "center")),
              fluidRow(
                column(12, h4(" Results:"), align = "CENTER", br()),
                column(12, offset = 4, valueBoxOutput("text_winloss", width = 4), align = "CENTER"))

              )
            )
    )
    )
  )

shinyUI(
  
  dashboardPage(
    skin = "black",
    dashboardHeader(title = "WildTurtle", titleWidth =  200), 
    sidebar, 
    body)
)
