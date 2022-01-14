library(tidyverse)
library(shiny)
library(shinyWidgets)
library(plotly)

## DATA #######################################################################
# GK missing
data <- read.csv("data/fifa_players.csv", sep="|", encoding = 'UTF-8')

# https://dplyr.tidyverse.org/reference/ (library for select, filter, rename...)
data <- data %>% select(short_name, club_name,
                        league_name, nationality_name, player_position,
                        age, value_eur, wage_eur, preferred_foot,
                        pace, shooting, passing, dribbling, defending, physic,
                        search_name)
# search_name has to be the last one

# Rename columns
data <- data %>% rename(Name = short_name, Club = club_name, League = league_name, 
                        Nation = nationality_name, Position = player_position,
                        Age = age, Value = value_eur, Salary = wage_eur, Foot = preferred_foot,
                        Pace = pace, Shooting = shooting, 
                        Passing = passing, Dribbling = dribbling, Defending = defending, 
                        Physic = physic)
# These are the last names of the columns

# Create inputs for filters (database) ordering by name
nations <- as.list(unique(data[c("Nation")]))
nations <- lapply(nations,sort,decreasing=FALSE)
leagues <- as.list(unique(data[c("League")]))
leagues <- lapply(leagues,sort,decreasing=FALSE)

# Create radar chart data 
stats_names <- c('Pace','Shooting','Dribbling', 'Passing', 'Defending', 'Physic')
data_stats <- data %>% select ("Name", "Club", stats_names)
data_stats$Name_Club <- paste(data_stats$Name, "-", data_stats$Club)
# Create inputs for filters (radar chart) ordering by name
players <- as.list(unique(data_stats[c("Name_Club")]))
players <- lapply(players,sort,decreasing=FALSE)

## UI ##########################################################################

ui <- navbarPage("Scouting App",
                 
  ## TAB Compare players #######################################################################
                 tabPanel("Compare Players", fluid = TRUE,
                          
                          sidebarLayout(
                            
                            sidebarPanel(
                              h3('Select Players'),
                              
                              selectInput("player_1",
                                          label = "Player 1:", 
                                          choices = players,
                                          selected="L. Messi - Paris Saint-Germain"),
                              
                              selectInput("player_2",
                                          label = "Player 2:", 
                                          choices = players,
                                          selected = "N. Kanté - Chelsea"),
                              
                              selectInput("player_3",
                                          label = "Player 3:", 
                                          choices = players,
                                          selected = "Cristiano Ronaldo - Manchester United"),
                              
                              helpText("To visualize the stats of a player
                                       click his name in the legend.")
                              
                            ), # Close sidebar
                            
                            mainPanel(plotlyOutput("radar")
                            ) # Close main panel
                            
                          ) # Close the sidebar layout
                          
                 ),# Close tab panel            
                 
                 
  ## TAB Similar players #######################################################################
                 tabPanel("Similar Players", fluid = TRUE,
                          
                          sidebarLayout(
                            
                            sidebarPanel(
                              
                              
                              
                            ), # Close sidebar
                            
                            mainPanel(
                            ) # Close main panel
                            
                          ) # Close the sidebar layout
                          
                 ),# Close tab panel                     

  ## TAB Leagues #######################################################################
tabPanel("Leagues", fluid = TRUE,
         
         sidebarLayout(
           
           sidebarPanel(
             
             
             
           ), # Close sidebar
           
           mainPanel(
           ) # Close main panel
           
         ) # Close the sidebar layout
         
),# Close tab panel

  ## TAB Teams #######################################################################
tabPanel("Teams", fluid = TRUE,
         
         sidebarLayout(
           
           sidebarPanel(
             
             
             
           ), # Close sidebar
           
           mainPanel(
           ) # Close main panel
           
         ) # Close the sidebar layout
         
),# Close tab panel 

  ## TAB Stats Correlation #######################################################################
tabPanel("Stats Correlation", fluid = TRUE,
         
         sidebarLayout(
           
           sidebarPanel(
             
             
             
           ), # Close sidebar
           
           mainPanel(
           ) # Close main panel
           
         ) # Close the sidebar layout
         
),# Close tab panel     

  ## TAB Database ####################################################################### 
                 
                 tabPanel("Database", fluid = TRUE,
                          
                          sidebarLayout(
                            
                            sidebarPanel(
                              h3('Apply Filters'),
                              
                              pickerInput("nations",
                                          label = "Nations:", 
                                          choices = nations, 
                                          selected = unlist(nations, use.names = FALSE),
                                          options = list(`actions-box` = TRUE),
                                          multiple = T),
                              
                              pickerInput("leagues",
                                          label = "Leagues:", 
                                          choices = leagues, 
                                          selected = unlist(leagues, use.names = FALSE),
                                          options = list(`actions-box` = TRUE),
                                          multiple = T),
                              
                              checkboxGroupInput("positions", 
                                                 label = "Positions:", 
                                                 choices = list("GK" = "GK",
                                                                "RB" = "RB",
                                                                "RWB" = "RWB",
                                                                "LB" = "LB",
                                                                "LWB" = "LWB",
                                                                "CB" = "CB", 
                                                                "CDM" = "CDM",
                                                                "CM" = "CM",
                                                                "CAM" = "CAM",
                                                                "LM" = "LM",
                                                                "LW" = "LW",
                                                                "RM" = "RM",
                                                                "RW" = "RW",
                                                                "CF" = "CF",
                                                                "ST" = "ST"
                                                 ), inline = TRUE, selected = "ST"),
                              
                              checkboxGroupInput("foot", 
                                                 label = "Foot:", 
                                                 choices = list("Left" = "Left",
                                                                "Right" = "Right"
                                                 ), inline = TRUE, selected = c("Left", "Right")),
                              
                              fluidRow(
                                
                                column(6, sliderInput("pace_range", 
                                                      label = "Pace:",
                                                      min = 1, max = 99, 
                                                      value = c(1, 99))),
                                column(6, sliderInput("shooting_range", 
                                                      label = "Shooting:",
                                                      min = 1, max = 99, 
                                                      value = c(1, 99)))
                                
                              ),
                              
                              fluidRow(
                                
                                column(6, sliderInput("passing_range", 
                                                      label = "Passing:",
                                                      min = 1, max = 99, 
                                                      value = c(1, 99))),
                                column(6, sliderInput("dribbling_range", 
                                                      label = "Dribbling:",
                                                      min = 1, max = 99, 
                                                      value = c(1, 99)))
                                
                              ),
                              
                              fluidRow(
                                
                                column(6, sliderInput("defending_range", 
                                                       label = "Defending:",
                                                       min = 1, max = 99, 
                                                       value = c(1, 99))),
                                column(6, sliderInput("physic_range", 
                                                      label = "Physic:",
                                                      min = 1, max = 99, 
                                                      value = c(1, 99)))
                                
                              ),
                              
                            ), # Close sidebar
                            
                            mainPanel(dataTableOutput('table')
                            ) # Close main panel
                            
                          ) # Close the sidebar layout
                          
                 ), # Close tab panel
                 
                 
) # Close navbar


## SERVER #######################################################################

server <- function(input, output) {

  ## Create database table #######################   
  output$table <- renderDataTable(
    data %>% filter(Nation %in% input$nations &
                      League %in% input$leagues & 
                      Position %in% input$positions & 
                      Foot %in% input$foot & 
                      between(Pace, input$pace_range[1], input$pace_range[2]) &
                      between(Shooting, input$shooting_range[1], input$shooting_range[2]) &
                      between(Passing, input$passing_range[1], input$passing_range[2]) &
                      between(Dribbling, input$dribbling_range[1], input$dribbling_range[2]) &
                      between(Defending, input$defending_range[1], input$defending_range[2]) &
                      between(Physic, input$physic_range[1], input$physic_range[2])
    )
    %>% select(-Foot),
    options = list(pageLength = 10, scrollX = T,
                   columnDefs = list(list(visible=FALSE, targets=c(-1))))
  )

  ## Create radar chart plot #######################     
  output$radar <- renderPlotly({
    
    plot_ly(
      type = 'scatterpolar',
      mode = "closest",
      fill = 'toself'
    )  %>%
      add_trace(
        r = as.numeric(as.data.frame(data_stats %>% filter(Name_Club == input$player_1)
                                     %>% select(stats_names))[1,]),
        theta = stats_names,
        name = as.data.frame(data_stats %>% filter(Name_Club == input$player_1))[1,"Name_Club"],
        mode = "markers"
      )  %>%
      add_trace(
        r = as.numeric(as.data.frame(data_stats %>% filter(Name_Club == input$player_2)
                                     %>% select(stats_names))[1,]),
        theta = stats_names,
        name = as.data.frame(data_stats %>% filter(Name_Club == input$player_2))[1,"Name_Club"],
        showlegend = TRUE,
        mode = "markers",
        visible="legendonly"
      )  %>%
      add_trace(
        r = as.numeric(as.data.frame(data_stats %>% filter(Name_Club == input$player_3)
                                     %>% select(stats_names))[1,]),
        theta = stats_names,
        name = as.data.frame(data_stats %>% filter(Name_Club == input$player_3))[1,"Name_Club"],
        showlegend = TRUE,
        mode = "markers",
        visible="legendonly"
      ) %>% 
      layout(
        autosize = T, width = 800, height=700, margin = 30,
        polar = list(
          radialaxis = list(
            visible = T,
            range = c(0,100)
          )
        )
      )
    
  })
  
}

## APP ##########################################################################
shinyApp(ui = ui, server = server)