# this app will allow the user to compare the relative positional value
# of a fantasy player against the relative positional value of 
# other available players
# relative positional value is the points above the average starter
# at the player's position. this comp is used because different positions
# have different starting requirements and varying amounts of quality players
# for example, QBs tend to all produce at a high level, while there are only
# 2-3 tight ends that produce at a high level
# knowing the right time to select a high quality player at one position
# over the other depends on how much the player adds to your expected team 
# points relative to other available players and the stock of those players
# "stock" meaning the quality of all available players, not just the best available
# because the "best" available will likely still not be available at the next pick
# so its important to know what quality of position is likely to be available


# packages
library(shiny)
library(tidyverse)
library(ggplot2)
library(shinythemes)


# data

finishes <- read.csv('data/fp finishes_16_18.csv')

projections <- read.csv('data/fp_projections_19.csv')



# creating list of players names 

projections <- projections %>% mutate(
  Name = str_c(first, last, sep = ' ')
)

players <- projections[, 'Name']




# UI ============================================================================
ui <- fluidPage(
  theme = 'sandstone',
  titlePanel('Draft Value Tool'), 
  sidebarLayout(
    sidebarPanel(
      numericInput(
        inputId = 'league_size', label = 'Enter League Size', 
        value = 10, min = 8, max = 12, step = 1
      ), 
      numericInput(
        inputId = 'ppr', label = 'Enter Points Per Reception', 
        value = .5, min = 0, max = 1, step = .5
      ), 
      numericInput(
        inputId = 'qb_tds', label = 'Enter Points per QB Touchdowns', 
        value = 4, min = 4, max = 6, step = 2
      ), 
      numericInput(
        inputId = 'qb_yds', label = 'Enter Passing Yards per Point', 
        value = 25, min = 10, max = 25, step = 15
      ), 
      selectizeInput(
        inputId = 'positions', label = 'Select Positions to not Show', 
        choices = c('QB', 'WR', 'TE', 'RB'), multiple = T
      )
    ), 
    mainPanel(
      selectizeInput(
        inputId = 'drafted', label = 'Enter Drafted Players', 
        choices = players, multiple = T, width = '100%'
      ), h3('Top Reamaning Players'), 
      tableOutput(outputId = 'top_remaining'), 
      tableOutput(outputId = 'best_table'),
      plotOutput(outputId = 'remaining_stock')
    )
  )
)


# server ========================================================================
server <- function(input, output) {
  
  
  projection_points <- reactive({
    
    projections %>% mutate(
      Points = (PASSING_YDS / input$qb_yds) + (PASSING_TDS * input$qb_tds) +
        (PASSING_INTS * -2) + (RUSHING_YDS * .1) + (RUSHING_TDS * 6) +
        (RECEIVING_REC * input$ppr) + (RECEIVING_YDS * .1) +
        (RECEIVING_TDS * 6) + (MISC_FL * -2)
    )%>% group_by(
      position
    ) %>% mutate(
      finish = rank(desc(Points), ties.method = 'first')
    ) %>% ungroup() %>% select(
      Name, position, Points, finish
    )
    
  })
  
  
  
  
  available <- reactive({
    
    projection_points() %>% filter(
      (!Name %in% input$drafted) & 
        (!position %in% input$positions)
      )
    
  })
  
  
  projection_averages <- reactive({
    projection_points() %>% filter(
      (
        position == 'QB' & 
          finish <= input$league_size
        ) | 
        (
        position == 'TE' & 
        finish <= input$league_size
    ) | (
      position == 'WR' & 
        finish <= (2*input$league_size)
    ) | (
      position == 'RB' & 
        finish <= (2*input$league_size)
    ) 
    ) %>% group_by(position) %>% 
      summarize(
        proj_average = median(Points)
      )
  })
  
  
  actual_averages <- reactive({
    finishes %>% filter(
      (
        Position == 'QB' & 
          finish <= input$league_size
      ) | 
        (
          Position == 'TE' & 
            finish <= input$league_size
        ) | (
          Position == 'WR' & 
            finish <= (2*input$league_size)
        ) | (
          Position == 'RB' & 
            finish <= (2*input$league_size)
        ) 
    ) %>% group_by(Position) %>% 
      summarize(
        act_average = median(Points)
      )
  })
  
  
  
  
  
  available_adjusted <- reactive({
    
    av_adj <- left_join(
      available(), projection_averages(), by = 'position'
    )
    
    av_adj <- left_join(
      av_adj, actual_averages(), 
      by = c('position' = 'Position')
    )
    
    av_adj <- av_adj %>% mutate(
      proj_value = Points - proj_average, 
      act_value = Points - act_average
    )
    
    av_adj
    
  })
  
  
  output$top_remaining <- renderTable({
    
    available_adjusted() %>% mutate(
      proj_rank = rank(desc(proj_value), ties.method = 'first'), 
      act_rank = rank(desc(act_value), ties.method = 'first')
    )  %>% filter(
      proj_rank <= 10 | act_rank <= 10
    ) %>% select(
      Name, proj_value, proj_rank, act_value, act_rank
    ) %>% rename(
      Projected_Value = proj_value, 
      Projected_Rank = proj_rank, 
      Historical_Value = act_value, 
      Historical_Rank = act_rank
    ) %>% arrange(Projected_Rank)
    
  })
  
  
  
  
  
  output$best_table <- renderTable({
    
    stock_graph <- available_adjusted() %>% group_by(position) %>%
      mutate(
        proj_rank = rank(desc(proj_value), ties.method = 'first')
      ) %>% arrange(proj_rank) %>% mutate(
        above_1 = Points - lead(Points, 1), 
        above_5 = Points - lead(Points, 5),
        above_10 = Points - lead(Points, 10), 
        above_20 = Points - lead(Points, 20)
      ) %>% filter(proj_rank == 1) %>% 
      select(
        Name, position, Points, above_1,
        above_5, above_10, above_20
        ) %>% rename(
          Position = position, 
          Projected_Points = Points, 
          Points_Above_Next = above_1, 
          Points_Above_5 = above_5,
          Points_Above_10 = above_10, 
          Points_Above_20 = above_20
        ) %>% ungroup() %>% 
      arrange(desc(Points_Above_Next))
    
  })
  
  
  
  
  
  output$remaining_stock <- renderPlot({
    
    stock_graph <- available_adjusted() %>% group_by(position) %>%
      mutate(
      proj_rank = rank(desc(proj_value), ties.method = 'first'), 
      act_rank = rank(desc(act_value), ties.method = 'first')
    )  %>% ungroup() %>% filter(
      proj_rank <= (2*input$league_size)
    )
    
    stock_graph %>% ggplot(aes(proj_rank, proj_value))+
      geom_line(aes(color = position))+
      geom_point(aes(color = position))+
      theme_bw()
    
  })
  
  
}





# Run the application 
shinyApp(ui = ui, server = server)

