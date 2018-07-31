#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)


data <- read.csv("Spring2015R.csv", header = TRUE, sep = ",")
dataPlayers <- read.csv("playerstats.csv", header = TRUE, sep = ",")
summerdataPlayers <- read.csv("summerplayerstats.csv", header = TRUE, sep = ",")
summer <- read.csv("Summer2015R.csv", header = TRUE, sep = ",")

imtspring <- read.csv("Spring2016R.csv", header = TRUE, sep = ",")
imtsummer <- read.csv("Summer2016R.csv", header = TRUE, sep = ",")
dataPlayers16 <- read.csv("playerstats2016spring.csv", header = TRUE, sep = ",")
summerdataPlayers16 <- read.csv("playerstats2016summer.csv", header = TRUE, sep = ",")

data$Week <- as.factor(data$Week)
data$KDA <- (data$Kills +data$Assists)/data$Deaths

cons <- (-8.667283784)
balls <- 3.019684396
hai <- 3.277577979
lemon <- 2.682913865
moon <- 1.353141833
wt <- 1.983524786
gametime <- (-0.082414724)
oppstr <- (-1.315091125)
drag <- 0.377277956
baron <- 0.464694592


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
    imtspr <-plot_ly(data = imtspring, x = ~imtspring$Week, y = ~imtspring$KDA, type = 'scatter', mode = 'lines') %>%
    add_trace(x = ~imtspring$Week, y= ~imtspring$Average.LCS.KDA, line = list(dash ='dash')) %>%
    layout(xaxis = list(title = "Week"),
           yaxis = list(title = "KDA"),
           showlegend = FALSE)
    output$spring2016WTKDA <- renderPlotly({imtspr})
    
    imtsm <-plot_ly(data = imtsummer, x = ~imtsummer$Week, y = ~imtsummer$KDA, type = 'scatter', mode = 'lines') %>%
    add_trace(x = ~imtsummer$Week, y= ~imtsummer$Average.LCS.KDA, line = list(dash ='dash')) %>%
    layout(xaxis = list(title = "Week"),
           yaxis = list(title = "KDA"),
           showlegend = FALSE)
    output$summer2016WTKDA <- renderPlotly({imtsm})

  dataf = data.frame(data$Week, data$KDA, data$Average.LCS.KDA)
  WTKDA <-plot_ly(data = dataf, x = ~data$Week, y = ~data$KDA, type = 'scatter', mode = 'lines') %>%
    add_trace(x = ~data$Week, y= ~data$Average.LCS.KDA, line = list(dash ='dash')) %>%
    layout(xaxis = list(title = "Week"),
           yaxis = list(title = "KDA"),
           showlegend = FALSE)

  output$spring2015WTKDA <- renderPlotly({WTKDA})
  
  dataPlayers <- head(arrange(dataPlayers,desc(Games)), n = 10)
  
  dataTop10 <- arrange(dataPlayers,desc(KDA))
  KDATop10 <- plot_ly(data = dataTop10, x = reorder(dataTop10$Player, -dataTop10$KDA), y = dataTop10$KDA, type = 'bar', marker = list(color = ifelse(dataTop10$Player == "WildTurtle", 'rgb(26, 118, 255)', 'rgb(55, 83, 109)'))) %>%
    layout(xaxis = list(tickangle = 30),
           yaxis = list(title = "KDA"))
  
  output$spring2015KDARank <- renderPlotly({KDATop10})
  
  dataTop10 <- head(arrange(dataPlayers,desc(CSM)), n = 10)
  CSMTop10 <- plot_ly(data = dataTop10, x = reorder(dataTop10$Player, -dataTop10$CSM), y = dataTop10$CSM, type = 'bar', marker = list(color = ifelse(dataTop10$Player == "WildTurtle", 'rgb(26, 118, 255)', 'rgb(55, 83, 109)'))) %>%
    layout(xaxis = list(tickangle = 30),
           yaxis = list(title = "CSM"))
  
  output$spring2015CSMRank <- renderPlotly({CSMTop10})
  
  dataTop10 <- head(arrange(dataPlayers,desc(Avg.deaths)), n = 10)
  DeathsTop10 <- plot_ly(data = dataTop10, x = reorder(dataTop10$Player, -dataTop10$Avg.deaths), y = dataTop10$Avg.deaths, type = 'bar', marker = list(color = ifelse(dataTop10$Player == "WildTurtle", 'rgb(26, 118, 255)', 'rgb(55, 83, 109)'))) %>%
    layout(xaxis = list(tickangle = 30),
           yaxis = list(title = "Deaths"))
  
  output$spring2015Deaths <- renderPlotly({DeathsTop10})
  
  #summer 2015
  
  WTKDASummer <-plot_ly(data = summer, x = ~summer$Week, y = ~summer$KDA, type = 'scatter', mode = 'lines') %>%
    add_trace(x = ~summer$Week, y= ~summer$Average.LCS.KDA, line = list(dash ='dash')) %>%
    layout(xaxis = list(title = "Week"),
           yaxis = list(title = "KDA"),
           showlegend = FALSE)
  
  output$summer2015WTKDA <- renderPlotly({WTKDASummer})
  
  summerdataPlayers <- head(arrange(summerdataPlayers,desc(Games)), n = 10)
  
  summerdataTop10 <- arrange(summerdataPlayers,desc(KDA))
  summerKDATop10 <- plot_ly(data = summerdataTop10, x = reorder(summerdataTop10$Player, -summerdataTop10$KDA), y = summerdataTop10$KDA, type = 'bar', marker = list(color = ifelse(summerdataTop10$Player == "WildTurtle", 'rgb(26, 118, 255)', 'rgb(55, 83, 109)'))) %>%
    layout(xaxis = list(tickangle = 30),
           yaxis = list(title = "KDA"))
  
  output$summer2015KDARank <- renderPlotly({summerKDATop10})
  
  summerdataTop10 <- head(arrange(summerdataPlayers,desc(CSM)), n = 10)
  summerCSMTop10 <- plot_ly(data = summerdataTop10, x = reorder(summerdataTop10$Player, -summerdataTop10$CSM), y = summerdataTop10$CSM, type = 'bar', marker = list(color = ifelse(summerdataTop10$Player == "WildTurtle", 'rgb(26, 118, 255)', 'rgb(55, 83, 109)'))) %>%
    layout(xaxis = list(tickangle = 30),
           yaxis = list(title = "CSM"))
  
  output$summer2015CSMRank <- renderPlotly({summerCSMTop10})
  
  summerdataTop10 <- head(arrange(summerdataPlayers,desc(Avg.deaths)), n = 10)
  summerDeathsTop10 <- plot_ly(data = summerdataTop10, x = reorder(summerdataTop10$Player, -summerdataTop10$Avg.deaths), y = summerdataTop10$Avg.deaths, type = 'bar', marker = list(color = ifelse(summerdataTop10$Player == "WildTurtle", 'rgb(26, 118, 255)', 'rgb(55, 83, 109)'))) %>%
    layout(xaxis = list(tickangle = 30),
           yaxis = list(title = "Deaths"))
  
  output$summer2015Deaths <- renderPlotly({summerDeathsTop10})
  
  dataPlayers16 <- head(arrange(dataPlayers16,desc(Games)), n = 10)
  
  dataTop10spr16 <- arrange(dataPlayers16,desc(KDA))
  KDATop10spr16 <- plot_ly(data = dataTop10spr16, x = reorder(dataTop10spr16$Player, -dataTop10spr16$KDA), y = dataTop10spr16$KDA, type = 'bar', marker = list(color = ifelse(dataTop10spr16$Player == "WildTurtle", 'rgb(26, 118, 255)', 'rgb(55, 83, 109)'))) %>%
    layout(xaxis = list(tickangle = 30),
           yaxis = list(title = "KDA"))
  
  output$spring2016KDARank <- renderPlotly({KDATop10spr16})
  
  dataTop10spr16 <- head(arrange(dataPlayers16,desc(CSM)), n = 10)
  CSMTop10spr16 <- plot_ly(data = dataTop10spr16, x = reorder(dataTop10spr16$Player, -dataTop10spr16$CSM), y = dataTop10spr16$CSM, type = 'bar', marker = list(color = ifelse(dataTop10spr16$Player == "WildTurtle", 'rgb(26, 118, 255)', 'rgb(55, 83, 109)'))) %>%
    layout(xaxis = list(tickangle = 30),
           yaxis = list(title = "CSM"))
  
  output$spring2016CSMRank <- renderPlotly({CSMTop10spr16})
  
  dataTop10spr16 <- head(arrange(dataPlayers16,desc(Avg.deaths)), n = 10)
  DeathsTop10spr16 <- plot_ly(data = dataTop10spr16, x = reorder(dataTop10spr16$Player, -dataTop10spr16$Avg.deaths), y = dataTop10spr16$Avg.deaths, type = 'bar', marker = list(color = ifelse(dataTop10spr16$Player == "WildTurtle", 'rgb(26, 118, 255)', 'rgb(55, 83, 109)'))) %>%
    layout(xaxis = list(tickangle = 30),
           yaxis = list(title = "Deaths"))
  
  output$spring2016Deaths <- renderPlotly({DeathsTop10spr16})
  
  summerdataPlayers16 <- head(arrange(summerdataPlayers16,desc(Games)), n = 10)
  
  summer16dataTop10 <- arrange(summerdataPlayers16,desc(KDA))
  summer16KDATop10 <- plot_ly(data = summer16dataTop10, x = reorder(summer16dataTop10$Player, -summer16dataTop10$KDA), y = summer16dataTop10$KDA, type = 'bar', marker = list(color = ifelse(summer16dataTop10$Player == "WildTurtle", 'rgb(26, 118, 255)', 'rgb(55, 83, 109)'))) %>%
    layout(xaxis = list(tickangle = 30),
           yaxis = list(title = "KDA"))
  
  output$summer2016KDARank <- renderPlotly({summer16KDATop10})
  
  summer16dataTop10 <- head(arrange(summerdataPlayers16,desc(CSM)), n = 10)
  summer16CSMTop10 <- plot_ly(data = summer16dataTop10, x = reorder(summer16dataTop10$Player, -summer16dataTop10$CSM), y = summer16dataTop10$CSM, type = 'bar', marker = list(color = ifelse(summer16dataTop10$Player == "WildTurtle", 'rgb(26, 118, 255)', 'rgb(55, 83, 109)'))) %>%
    layout(xaxis = list(tickangle = 30),
           yaxis = list(title = "CSM"))
  
  output$summer2016CSMRank <- renderPlotly({summer16CSMTop10})
  
  summer16dataTop10 <- head(arrange(summerdataPlayers16,desc(Avg.deaths)), n = 10)
  summer16DeathsTop10 <- plot_ly(data = summer16dataTop10, x = reorder(summer16dataTop10$Player, -summer16dataTop10$Avg.deaths), y = summer16dataTop10$Avg.deaths, type = 'bar', marker = list(color = ifelse(summer16dataTop10$Player == "WildTurtle", 'rgb(26, 118, 255)', 'rgb(55, 83, 109)'))) %>%
    layout(xaxis = list( tickangle = 30),
           yaxis = list(title = "Deaths"))
  
  output$summer2016Deaths <- renderPlotly({summer16DeathsTop10})
  
  output$opp <- renderImage({
    list( src = switch(input$team,
                        "Team SoloMid" = "tsmlogo.png",
                        "Immortals" = "imtlogo.png",
                        "Counter Logic Gaming" = "clglogo.png",
                        "Echo Fox" = "foxlogo.png",
                        "Cloud 9" = "c9logo.png",
                        "Dignitas" = "diglogo.png",
                        "Phoenix1" = "p1logo.png",
                        "Team Liquid" = "tllogo.png",
                        "Team EnvyUs" = "envlogo.png")
          )}, deleteFile = FALSE)
  
  
  #tool calculation
  values <- reactiveValues()
  observe({
  input$action_Calc
  values$int <- isolate({
    input$action_Calc
    logitTEAM <- input$num_balls * balls + 
      input$num_moon * moon + 
      input$num_hai * hai + 
      input$num_wt * wt + 
      input$num_lemon * lemon + 
      input$num_drag * drag / 100 + 
      input$num_baron * baron / 100 +
      input$num_time * gametime
    logitnow <- switch(input$team,
                       "Team SoloMid" = 1.78425656,
                       "Immortals" = 1.587912088,
                       "Counter Logic Gaming" = 1.382417582,
                       "Echo Fox" = 0.903654485,
                       "Cloud 9" = 1.466307278,
                       "Dignitas" = 1.232967033,
                       "Phoenix1" = 0.791925466,
                       "Team Liquid" = 0.717532468,
                       "Team EnvyUs" = 1.074175824)*oppstr + logitTEAM + cons
    values$int <- isolate({  
      round(1/(1+exp(-logitnow))*100)
    })
    })
  })
  
  output$text_winloss <- renderValueBox({
    valueBox(paste0(ifelse(values$int > 100, 100, values$int), "%"), "Probability", color = ifelse(values$int > 49, "yellow", "red"), icon = icon(ifelse(values$int > 49, "thumbs-up", "thumbs-down"), lib = "glyphicon"), width = 4)
  })

})

#shinyapps::deployApp('/Users/Ivan.Sheng/Documents/RStudio/concept/www')