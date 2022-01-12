library(tidyverse)
library(shiny)
library(shinyWidgets)

## DATA #######################################################################
# Fix gk missing
data <- read.csv("data/fifa_players.csv", sep="|", encoding = 'UTF-8')
# search_name has to be the last one
data <- data %>% select(short_name, club_name,
                        league_name, nationality_name, player_position,
                        age, value_eur, wage_eur, preferred_foot,
                        pace, shooting, passing, dribbling, defending, physic,
                        search_name)
# Inputs
nations <- as.list(unique(data[c("nationality_name")]))
nations <- lapply(nations,sort,decreasing=FALSE)
leagues <- as.list(unique(data[c("league_name")]))
leagues <- lapply(leagues,sort,decreasing=FALSE)
#as.list(unique(data[c("player_position")]))
#as.list(unique(data[c("preferred_foot")]))

# Ranges
min_pace <- min(data$pace)
max_pace <- max(data$pace)  
min_shooting <- min(data$shooting)
max_shooting <- max(data$shooting)  
min_passing <- min(data$passing)
max_passing <- max(data$passing)  
min_dribbling <- min(data$dribbling)
max_dribbling <- max(data$dribbling)  
min_defending <- min(data$defending)
max_defending <- max(data$defending)  
min_physic <- min(data$physic)
max_physic <- max(data$physic)

# Rename columns
data <- data %>% rename(Name = short_name, Club = club_name ,League = league_name, 
                        Nation = nationality_name, Position = player_position,
                        Age = age, Value = value_eur, Salary = wage_eur, Foot = preferred_foot,
                        Pace = pace, Shooting = shooting, 
                        Passing = passing, Dribbling = dribbling, Defending = defending, 
                        Physic = physic)

## UI ##########################################################################

ui <- navbarPage("Scouting App",
                 
    tabPanel("General", fluid = TRUE,
                       
                       sidebarLayout(
              
                         sidebarPanel(
                                      pickerInput("nations",
                                                  label = "Nations:", 
                                                  choices = nations, 
                                                  options = list(`actions-box` = TRUE),
                                                  multiple = T),
                                      
                                      pickerInput("leagues",
                                                  label = "Leagues:", 
                                                  choices = leagues, 
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
                                                         ), inline = TRUE, selected = NULL),
                                      
                                      checkboxGroupInput("foot", 
                                                         label = "Foot:", 
                                                         choices = list("Left" = "Left",
                                                                        "Right" = "Right"
                                                         ), inline = TRUE, selected = NULL),
                                      
                                      fluidRow(
                                        
                                        column(6, sliderInput("pace_range", 
                                                              label = "Pace:",
                                                              min = 1, max = 99, 
                                                              value = c(min_pace, max_pace))),
                                        column(6, sliderInput("shooting_range", 
                                                              label = "Shooting:",
                                                              min = 1, max = 99, 
                                                              value = c(min_shooting, max_shooting)))
                                        
                                      ),
                                      
                                      fluidRow(
                                        
                                        column(6, sliderInput("passing_range", 
                                                              label = "Passing:",
                                                              min = 1, max = 99, 
                                                              value = c(min_passing, max_passing))),
                                        column(6, sliderInput("dribbling_range", 
                                                              label = "Dribbling:",
                                                              min = 1, max = 99, 
                                                              value = c(min_dribbling, max_dribbling)))
                                        
                                      ),
                                      
                                      fluidRow(
                                        
                                        column(6,  sliderInput("defending_range", 
                                                               label = "Defending:",
                                                               min = 1, max = 99, 
                                                               value = c(min_defending, max_defending))),
                                        column(6, sliderInput("physic_range", 
                                                              label = "Physic:",
                                                              min = 1, max = 99, 
                                                              value = c(min_physic, max_physic)))
                                        
                                      ),
                                      
                         ), # Close sidebar
                         
                         mainPanel(dataTableOutput('table')
                                   ) # Close main panel
                         
                         ) # Close the sidebar layout
             
             ), # Close tab panel
    
    
    tabPanel("Compare", fluid = TRUE,
             ),# Close tab panel
    
    tabPanel("Similar player", fluid = TRUE,
             ),# Close tab panel
    
)

## SERVER #######################################################################

server <- function(input, output) {
  
  # observe({
  #   print(input$positions)
  # })
  
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
    options = list(pageLength = 10,
                   columnDefs = list(list(visible=FALSE, targets=c(-1))))
  )
    
}

## APP ##########################################################################
shinyApp(ui = ui, server = server)