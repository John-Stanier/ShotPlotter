library(shiny)
library(shinythemes)
library(shinyWidgets)
library(ggplot2)
library(png)
library(patchwork)
library(grid)
library(dplyr)
library(keys)
library(lubridate)
library(RColorBrewer)

hotkeys <- c(
  "w", #wrist shot
  "s", #slapshot
  "shift+s", #snapshot
  "b", #backhand
  "t", #tip
  "r", #wrap around
  "d", #deflection
  "a", #AWAY
  "h", #HOME
  "1", #Rush
  "2", #Breakaway
  "3", #Forechek
  "4", #Cycle
  "5", #Dump in
  "6", #On Net
  "7", #Goal
  "8", #Missed
  "9", #Blocked
  "backspace", #stop timer
  "enter" #start timer
)


ui <- navbarPage("Hockey Shot Plotter by John Stanier",
                 theme = shinytheme("spacelab"),

                 tabPanel("Shot Plotter",
                          useKeys(),
                          keysInput("keys", hotkeys),
                          
                          fluidRow(
                            column(2,
                                   textInput('homename', "Home team name:", value = "Home")),
                            column(2, textInput('awayname', "Away team name:", value = "Away")),
                            column(2, align = "center",
                                   #numeric input for the period of the game
                                   numericInput("period", "Period", value = 1, min = 1)
                            ),
                            column(3, align = "center",
                                   numericInput('seconds','Length of Period (Seconds)',
                                                value=1200,min=0,max=99999,step=1)
                            ),
                            
                            column(3,
                                   tags$b(textOutput('timeleft')),
                                   actionButton('start','Start'),
                                   actionButton('stop','Stop'),
                                   actionButton('reset','Reset')
                                   )
                            ),
                          
                          fluidRow(
                            column(12,
                                   #PlotOutput where shots will be plotted 
                                   plotOutput("rink", width = "100%", height = "450px", click = "plot_click"))
                            
                          ),
                          
                          #Fourth row containing a variety of inputs to contextualize the shots plotted
                          fluidRow(
                            
                            column(3,
                                   radioButtons("gamedirection", "The Home net is on the:", list("Left", "Right"),
                                                inline = TRUE)
                                   ),
                            
                            column(3,
                                   #Allows the user to select the team that took the shot
                                   radioButtons("team", "Shooting Team", list("HOME", "AWAY"))
                            ),
                            
                            column(3, align = "center",
                                   #Slider to adjust the number of players on the ice for of the home team
                                   sliderInput(inputId = "homeskaters",
                                               label = "Number of home skaters",
                                               value = 5, min = 3, max = 6)
                            ),
                            
                            column(3, align = "center",
                                   #Slider to adjust the number of players on the ice for of the away team
                                   sliderInput(inputId = "awayskaters", 
                                               label = "Number of away skaters",
                                               value = 5, min = 3, max = 6)
                            )
                          ),
                          
                          #Fifth row containing inputs for the strength of each team
                          fluidRow(
                            
                            column(3,
                                   #Input for the context in which the shot came
                                   selectInput("shotcontext", "Shot Context",
                                               list("Rush",
                                                    "Breakaway",
                                                    "Forecheck",
                                                    "Cycle",
                                                    "Dump In")
                                   )
                            ),
                            
                            column(3,
                                   #Allows for input of shot type
                                   selectInput("shottype", "Shot Type",
                                               list("Wrist Shot" = "WRIST",
                                                    "Slap Shot" = "SLAP",
                                                    "Backhand" = "BACK",
                                                    "Snapshot" = "SNAP",
                                                    "Tip" = "TIP",
                                                    "Wrap" = "WRAP",
                                                    "Deflection" = "DEFL"
                                               ),
                                               selected = NULL, multiple = FALSE, selectize = TRUE
                                   )
                            ),
                            column(3,
                                   #Input for the result of the players shot
                                   selectInput("result", "Shot Result",
                                               list("On Net" = "SHOT", 
                                                    "Goal" = "GOAL",
                                                    "Missed" = "MISS",
                                                    "Blocked" = "BLOCK"
                                               ),
                                               selected = NULL, multiple = FALSE, selectize = TRUE
                                   )
                            ),
                            
                            column(3,
                                   numericInput(inputId = "shooternumber",
                                                label = "Number of shooter",
                                                value = 99, min = 1, max = 99)
                            )
                            

                          ),
        
                          #Six row containing the table of previous shots
                          fluidRow(
                            column(10,
                                   tableOutput("table")
                            ),
                            column(2,
                                   #Allows for the removal of the last shot taken
                                   actionButton(inputId = "remove",
                                                label = "Remove Shot"))
                            )
                 ),
                 
                 tabPanel("Home HeatMaps",
                          sidebarLayout(
                            sidebarPanel(
                                          selectInput("homestrengthfilter", "Select Situations to Include:",
                                                      list("5on5",
                                                           "5on4",
                                                           "5on3",
                                                           "4on4",
                                                           "6on3",
                                                           "4on5",
                                                           "3on5",
                                                           "3on6"),
                                                     multiple = TRUE
                                                      )
                            ),
                            mainPanel(
                              h3("Offensive Zone Maps"),
                              plotOutput("homeoffensivemapcustom", width = "100%", height = "600px"),
                              h3("Defensive Zone Map"),
                              plotOutput("homedefensivemapcustom", width = "100%", height = "600px")
                              )
                              
                            )
                          ),
                 
                 tabPanel("Away HeatMaps",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("awaystrengthfilter", "Select Situations to Include:",
                                          list("5on5",
                                               "5on4",
                                               "5on3",
                                               "4on4",
                                               "6on3",
                                               "4on5",
                                               "3on5",
                                               "3on6"),
                                          multiple = TRUE
                              )
                            ),
                            mainPanel(
                              h3("Away Offensive Zone Maps"),
                              plotOutput("awayoffensivemapcustom", width = "100%", height = "800px"),
                              h3("Away Defensive Zone Map"),
                              plotOutput("awaydefensivemapcustom", width = "100%", height = "800px")
                            )
                            
                          )
                 ),
                 
                 tabPanel("Download",
                          sidebarLayout(
                            sidebarPanel(
                              
                              #Download button
                              h3("Download Shot Data"),
                              downloadButton("downloadshots", "Download"),
                              
                              #Upload Button
                              h3("Upload Past Shot Data"),
                              fileInput("fileupload", "Upload Shot Data",
                                        multiple = TRUE,
                                        accept = c(
                                          'text/csv',
                                          'text/comma-separated-values',
                                          'text/tab-separated-values',
                                          'text/plain',
                                          '.csv',
                                          '.tsv'
                                        )
                              )
                              
                            ),
                            mainPanel(
                              h2("All Shots"),
                              tableOutput("shottable"),
                              h2("Uploaded Shots"),
                              tableOutput("shotupload")
                            )
                          )
                 )
                 )



server <- function(input, output, session) {
  
  # Initialize the timer, 1200 seconds, not active.
  timer <- reactiveVal(1200)
  active <- reactiveVal(FALSE)
  
  # observer that invalidates every second. If timer is active, decrease by one.
  observe({
    invalidateLater(1000, session)
    isolate({
      if(active())
      {
        timer(timer()-1)
        if(timer()<1)
        {
          active(FALSE)
          showModal(modalDialog(
            title = "Important message",
            "Countdown completed!"
          ))
        }
      }
    })
  })
  
  # observers for actionbuttons
  observeEvent(input$start, {active(TRUE)})
  observeEvent(input$stop, {active(FALSE)})
  observeEvent(input$reset, {timer(input$seconds)})
  
  # Output the time left.
  output$timeleft <- renderText({
    paste("Time left: ", seconds_to_period(timer()))
  })
  
  
  #Shot Plotting Graph
  #Importing the rink image to use in the plotter
  rink_image <- readPNG("C:/Users/jstan/Documents/ESPN Rink.png", native = TRUE)
  
  #Making the rink usable in plots
  rink_image <- rasterGrob(rink_image,
                           width = unit(1,"npc"),
                           height =  unit(1, "npc")
  )
  
  #Adding Ozone and Dzone images for heatmaps
  o_zone <- readPNG("ESPN Rink Ozone.png")
  d_zone <- readPNG("ESPN Rink dzone.png")
  
  o_zone <- rasterGrob(o_zone,width = unit(1,"npc"),
                       height =  unit(1, "npc" ))
  
  d_zone <- rasterGrob(d_zone,width = unit(1,"npc"),
                       height =  unit(1, "npc" ))
  
  #Creating an xG model to measure shot quality
  training_set <- read.csv('compact_shots.csv')
  training_set$shottype <- as.factor(training_set$shottype)
  training_set <- filter(training_set, result != "BLOCK")
  
  training_set$strength <- ifelse(training_set$team == "HOME",
                                  paste0(training_set$homeskaters,
                                        "on",
                                        training_set$awayskaters,
                                        sep = ""),
                                  paste0(training_set$awayskaters,
                                        "on",
                                        training_set$homeskaters,
                                        sep = "")
                                  
                                  )
  training_set$strength <- as.factor(training_set$strength)
  
  xG_model <- glm(goal ~ poly(distance, 3, raw = TRUE) +
                    poly(absangle, 3, raw = TRUE) +
                    shottype +
                    strength +
                    isrush,
                  data = training_set,
                  family = binomial(link = 'logit')
  )
  
  #Creating reactives hot values
  shots <- reactiveValues()
  
  #Creating reactive output of shots
  shot_output <- reactiveValues()
  
  #Creating the dataframe to contain shots and a variety of inputs
  shots$DT <- data.frame(x = numeric(), 
                         y = numeric(),
                         shottype = factor(),
                         result = factor(),
                         period = numeric(),
                         time = character(),
                         minutes = character(),
                         seconds = character(),
                         secondsRemaining = numeric(),
                         team = factor(),
                         teamname = character(),
                         homeskaters = numeric(),
                         awayskaters = numeric(),
                         distance = numeric(),
                         absangle = numeric(),
                         netempty = numeric(),
                         adjxhome = numeric(),
                         adjxaway = numeric(),
                         adjy = numeric(),
                         strength = factor(),
                         isrush = numeric(),
                         xG = numeric()
  )
  
  uploaded_shots <- data.frame(x = numeric(), 
                               y = numeric(),
                               shottype = factor(),
                               homenet = factor(),
                               result = factor(),
                               period = numeric(),
                               secondsRemaining = numeric(),
                               time = character(),
                               minutes = character(),
                               seconds = character(),
                               team = factor(),
                               teamname = character(),
                               shotcontext = factor(),
                               homeskaters = numeric(),
                               awayskaters = numeric(),
                               distance = numeric(),
                               absangle = numeric(),
                               netempty = numeric(),
                               adjxhome = numeric(),
                               adjxaway = numeric(),
                               adjy = numeric(),
                               strength = factor(),
                               isrush = numeric(),
                               xG = numeric()
  )
  
  #Rendering the plot to add shot locations
  output$rink <- renderPlot({
    input$newplot
    ggplot(shots$DT, aes(x, y, color = team)) + lims(x = c(0,200), y = c(0,85)) +
      annotation_custom(rink_image, xmin =  1, xmax = 199, ymin = 0, ymax = 85) +
      geom_point(size = 4)
  })
  
  
  #Attaching specific events to key inputs. Reducing the amount of time a user spends inputting data
  observeEvent(input$keys, {
    switch(input$keys,
    "w" = updateSliderInput(session, "shottype", label = "Shot Type", value = "WRIST"),       
    "s" = updateSliderInput(session, "shottype", label = "Shot Type", value = "SLAP"),
    "d" = updateSliderInput(session, "shottype", label = "Shot Type", value = "DEFL"),
    "shift+s" = updateSliderInput(session, "shottype", label = "Shot Type", value = "SNAP") ,
    "b" = updateSliderInput(session, "shottype", label = "Shot Type", value = "BACK"),
    "t" = updateSliderInput(session, "shottype", label = "Shot Type", value = "TIP"),
    "r" = updateSliderInput(session, "shottype", label = "Shot Type", value = "WRAP"),
    "h" = updateRadioButtons(session, "team", label = "Shooting Team", selected = "HOME"),
    "a" = updateRadioButtons(session, "team", label = "Shooting Team", selected = "AWAY"),
    "1" = updateRadioButtons(session, "shotcontext", label = "Shot Context", selected = "Rush"),
    "2" = updateRadioButtons(session, "shotcontext", label = "Shot Context", selected = "Breakaway"),
    "3" = updateRadioButtons(session, "shotcontext", label = "Shot Context", selected = "Forecheck"),
    "4" = updateRadioButtons(session, "shotcontext", label = "Shot Context", selected = "Cycle"),
    "5" = updateRadioButtons(session, "shotcontext", label = "Shot Context", selected = "Dump In"),
    "6" = updateRadioButtons(session, "result", label = "Shot Result", selected = "SHOT"),
    "7" = updateRadioButtons(session, "result", label = "Shot Result", selected = "GOAL"),
    "8" = updateRadioButtons(session, "result", label = "Shot Result", selected = "MISS"),
    "9" = updateRadioButtons(session, "result", label = "Shot Result", selected = "BLOCK"),
    "backspace" = active(FALSE),
    "enter" = active(TRUE)
    )     
  })
  
  #Removing the last shooting event with a click of the remove button
  observeEvent(input$remove, {
    shots$DT <- slice(shots$DT, 0:(n()-1))
    shot_output$DT <- slice(shot_output$DT, 0:(n()-1))
  })
  
  #Adding new events to a separate dataframe with each click
  observeEvent(input$plot_click, {
    add_row <- data.frame(
      #Adding the x coordinate
      x = input$plot_click$x,
      #Adding the y coordinate
      y = input$plot_click$y, 
      #Adding the shot type
      shottype = input$shottype,
      #Adding the game direction
      homenet = input$gamedirection,
      #Adding the shot result
      result = input$result,
      #Adding the period the event occurred
      period = input$period,
      #Second Remaining
      secondsRemaining = timer(),
      #minutes
      minutes =  minute(seconds_to_period(timer())),
      #seconds
      seconds = seconds_to_period(timer()),
      #Adding the shooter's jersey number
      shooternumber = input$shooternumber,
      #Adding the number of home skaters
      homeskaters = input$homeskaters,
      #Adding the number of away skaters
      awayskaters = input$awayskaters,
      #Adding the team that took the shot
      team = input$team,
      #Adding the name of the team that the user inputted
      teamname = ifelse(input$team == "HOME", input$homename, input$awayname),
      #Adding shot context variable
      shotcontext = input$shotcontext,
      #Adding a binary input to determine if the shot was a goal
      goal = ifelse(input$result == "Goal", 1, 0),
      #Calculating the distance of the shot from the center of the net
      distance = ifelse(
        (input$gamedirection == "Right" & input$team == "HOME") |
          (input$gamedirection == "Left" & input$team == "AWAY"),
        sqrt((11 - input$plot_click$x)^2 + (42.5 - input$plot_click$y)^2),
        sqrt((189 - input$plot_click$x)^2 + (42.5 - input$plot_click$y)^2)
      ),
      #Calculating the absolute angle, this is always between 0 and 90 degrees
      absangle = ifelse(
        input$gamedirection == "Right",
        180 - abs(atan2(42.5 - input$plot_click$y, 11 - input$plot_click$x) * (180 / 3.14)),
        abs(atan2(42.5 - input$plot_click$y, 189 - input$plot_click$x) * (180 / 3.14)
        )
      ),
      #Adding variable to identify if a goalie has been pulled
      netempty = ifelse((input$team == "HOME" & input$awayskaters == 6) |
                          (input$team == "AWAY" & input$homeskaters == 6), 1, 0),
      
      #Adjusting the x coordinate for the heat map
      adjxhome = ifelse(input$plot_click$x < 100, 100 - input$plot_click$x, input$plot_click$x - 100),
      adjxaway = ifelse(input$plot_click$x < 100, input$plot_click$x, 200 - input$plot_click$x),
      #Adjusting the y coordinate for the heat map
      adjy = ifelse(
        input$gamedirection == "Right",
        input$plot_click$y,
        85 - input$plot_click$y),
      
      strength = ifelse(input$team == "HOME",
                        paste0(input$homeskaters,
                              "on",
                              input$awayskaters,
                              sep = ""),
                        paste0(input$awayskaters,
                              "on",
                              input$homeskaters,
                              sep = "")
                        ),
      isrush = ifelse(input$shotcontext == "Rush" |
                        input$shotcontext == "Breakaway", 1,0)
    )
    
    #Using a logistic regression to determine expected goals
    add_row$xG = ifelse(add_row$distance < 97.5,
                        predict(xG_model, add_row, type = 'response'),
                        0)
    
    add_row$time = ifelse(
      add_row$minutes > 9,
      paste0(substr(gsub("[^0-9.-]", "", add_row$seconds),1,2),
             ":", substr(gsub("[^0-9.-]", "", add_row$seconds),3,4)
      ),
      paste0(substr(gsub("[^0-9.-]", "", add_row$seconds),1,1),
             ":", substr(gsub("[^0-9.-]", "", add_row$seconds),2,3))
             )
    
    add_row$time <- ifelse(nchar(add_row$time) < 4 & add_row$minutes == 0,
                           paste0("0", ":", substr(add_row$time,1,1), substr(add_row$time,3,3)),
                           add_row$time
    )
    
    add_row$time <- ifelse(nchar(add_row$time) < 5 & add_row$minutes > 9,
                           paste0(substr(add_row$time,1,3),"0", substr(add_row$time,4,4)),
                           add_row$time
                           )
    
    add_row$time <- ifelse(nchar(add_row$time) < 4 & add_row$minutes <= 9 & add_row$minutes != 0,
                               paste0(substr(add_row$time,1,3),"0", substr(add_row$time,4,4)),
                               add_row$time
    )

    #Adding the new inputs to the existing data frame used in the plotting app
    shots$DT <- rbind(shots$DT, add_row)
    
    #Creating a smaller output to be shown in the app
    shot_output$DT <- subset(shots$DT, select = c(x,
                                                  y,
                                                  time,
                                                  team,
                                                  shooternumber,
                                                  result,
                                                  shottype,
                                                  distance,
                                                  absangle,
                                                  xG))
  })
  
  
  #Creating a table with all past inputs
  output$table <- renderTable({
    shot_output$DT
  })
  
  
  
  #Download Page#
  #Table displaying all shot data
  output$shottable <- renderTable({
    shots$DT
  })
  
  #Download of CSV
  output$downloadshots <- downloadHandler(
    filename = function() {
      paste("shot_data", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(shots$DT, file, row.names = FALSE)
    })
  
  #Upload of CSV
  output$shotupload <- renderTable({
    inFile <- input$fileupload
    
    
    if (is.null(inFile))
      return(NULL)
    
    
    uploaded_shots <- read.csv(inFile$datapath)
    uploaded_shots
  })
  
  observeEvent(input$fileupload, {
    inFile <- input$fileupload
    
    
    if (is.null(inFile))
      return(NULL)
    
    shots$DT <- rbind(shots$DT, read.csv(inFile$datapath))
    
  })
  
  #HEATMAP PAGE - HOME#
  
  #Creating the offensive zone heatmap
  output$homeoffensivemapcustom <- renderPlot({ input$offensivemap
    ggplot(subset(shots$DT, team  == "HOME" & (strength  %in% input$homestrengthfilter)),
           aes(adjy, adjxhome)) +
      lims(x = c(0,85), y = c(0,100)) +
      annotation_custom(o_zone, xmin =  0, xmax = 85, ymin = 0, ymax = 100) +
      stat_density_2d(aes(fill = stat(..level..)),
                      geom = "polygon", alpha = 1/3)
    
  })
  
  #Creating a defensive zone shot map
  output$homedefensivemapcustom <- renderPlot({ input$defensivemap
    ggplot(subset(shots$DT, team  == "AWAY" & (strength %in% input$homestrengthfilter)),
           aes(adjy, adjxaway)) + lims(x = c(0,85), y = c(0,100)) +
      annotation_custom(d_zone, xmin =  0, xmax = 85, ymin = 0, ymax = 100) +
      stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = 1/3)
    
  })
  
  #HEATMAP PAGE - AWAY#
  
  #Creating the offensive zone heatmap
  output$awayoffensivemapcustom <- renderPlot({ input$offensivemap
    ggplot(subset(shots$DT, team  == "AWAY" & (strength  %in% input$awaystrengthfilter)),
           aes((85-adjy), adjxhome)) +
      lims(x = c(0,85), y = c(0,100)) +
      annotation_custom(o_zone, xmin =  0, xmax = 85, ymin = 0, ymax = 100) +
      stat_density_2d(aes(fill = stat(..level..)),
                      geom = "polygon", alpha = 1/3)
    
  })
  
  #Creating a defensive zone shot map
  output$awaydefensivemapcustom <- renderPlot({ input$defensivemap
    ggplot(subset(shots$DT, team  == "HOME" & (strength %in% input$awaystrengthfilter)),
           aes((85-adjy), adjxaway)) + lims(x = c(0,85), y = c(0,100)) +
      annotation_custom(d_zone, xmin =  0, xmax = 85, ymin = 0, ymax = 100) +
      stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = 1/3)
    
  })
  
}

shinyApp(ui = ui, server = server)

