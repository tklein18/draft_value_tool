# this script will pull ESPN pre-season 
# fantasy rankings from ESPN web pages
# and fantasy season finishes
# and save them for later use as a csv


# libraries

library(rvest)
library(tidyr)
library(dplyr)







# finishes v2 ===============================================================================================


finish_url_base <- 'https://www.fantasypros.com/nfl/stats/' 


positions <- c('qb', 'wr', 'rb', 'te')

for(i in c(2016:2018)){
  
  finish_holder <- data.frame()
  
  for(p in positions){
    
    temp_url <- paste(
      finish_url_base, p, '.php?year=', i,
      '&range=full', sep = ''
    )
    
    temp_webpage <- read_html(temp_url)
    
    temp_tables <- html_nodes(temp_webpage, 'table')
    
    table_count <- c(1 : length(temp_tables))
    
    for( t in table_count){
      
      if(temp_tables[[t]] %>% html_table %>% 
         nrow() > 30){
        tbl_num <- t
      }
    }
    
    temp_finish <- temp_tables[[tbl_num]] %>% 
      html_table()
    
    
    names(temp_finish) <- str_c(temp_finish[1,], temp_finish[2,], sep='_')
    
    names(temp_finish) <- c('Player', names(temp_finish[,c(2:ncol(temp_finish))])) 
    
    temp_finish <- temp_finish[-c(1:2), ]
    
    temp_finish <- temp_finish %>% mutate(
      position = toupper(p)
    )
    
    finish_holder <- bind_rows(finish_holder, temp_finish)
  }
  
  temp_assign <- paste(
    'finish_', i, sep = ''
  )
  
  finish_holder <- finish_holder %>% mutate(
    year = as.numeric(i)
  )
  
  assign(temp_assign, finish_holder)
  
  
}





# binding finishes V2 ======================================================================================



fp_finishes <- bind_rows(
  finish_2016, finish_2017, finish_2018
)



fp_finishes <- fp_finishes %>% separate(
  Player, into = c('first', 'last'), extra = 'drop', 
  sep = ' '
)

fp_finishes[, c('first', 'last')] <- lapply(
  fp_finishes[, c('first', 'last')], tolower
)

fp_finishes <- fp_finishes %>% filter(
  !is.na(first) & !is.na(last)
)


fp_finishes[is.na(fp_finishes)] <- 0

fp_finishes <- fp_finishes %>% select(
  first, last, position, year, PASSING_YDS, PASSING_TD, PASSING_INT, 
  RUSHING_YDS, RUSHING_TD, RECEIVING_REC, RECEIVING_YDS, RECEIVING_TD, 
  MISC_FL
)


fp_finishes[, c(5:13)] <- lapply(
  fp_finishes[, c(5:13)], 
  function(x) str_replace(x, pattern = ',', replacement = '')
)



fp_finishes[, c(5:13)] <- lapply(
  fp_finishes[, c(5:13)], as.numeric
)







# projection v2 ======================================================================================


proj_url_base <- 'https://www.fantasypros.com/nfl/projections/'

positions <- c('qb', 'wr', 'rb', 'te')



for(i in positions){
  
  temp_url <- paste(
    proj_url_base, i, '.php?week=draft', sep = ''
  )
  
  
  temp_webpage <- read_html(temp_url)
  
  temp_tables <- html_nodes(temp_webpage, 'table')
  
  table_count <- c(1 : length(temp_tables))
  
  for( t in table_count){
    
    if(temp_tables[[t]] %>% html_table %>% 
       nrow() > 30){
      tbl_num <- t
    }
  }
  
  temp_finish <- temp_tables[[tbl_num]] %>% 
    html_table()
  
  
  names(temp_finish) <- str_c(temp_finish[1,], temp_finish[2,], sep='_')
  
  names(temp_finish) <- c('Player', names(temp_finish[,c(2:ncol(temp_finish))])) 
  
  temp_finish <- temp_finish[-c(1:2), ]
  
  
  temp_assign <- paste(
    'projection_', i, sep = ''
  )
  
  temp_finish <- temp_finish %>% mutate(
    position = toupper(i)
  )
  
  assign(temp_assign, temp_finish)
  
  
}







# binding projects v2 ======================================================================================



fp_projections <- bind_rows(
  projection_qb, projection_wr, projection_rb, projection_te
)



fp_projections <- fp_projections %>% separate(
  Player, into = c('first', 'last'), extra = 'drop', 
  sep = ' '
)

fp_projections[, c('first', 'last')] <- lapply(
  fp_projections[, c('first', 'last')], tolower
)

fp_projections <- fp_projections %>% filter(
  !is.na(first) & !is.na(last)
)


fp_projections[is.na(fp_projections)] <- 0


fp_projections[, c(3:12, 14:16)] <- lapply(
  fp_projections[, c(3:12, 14:16)], 
  function(x) str_replace(x, pattern = ',', replacement = '')
)



fp_projections[, c(3:12, 14:16)] <- lapply(
  fp_projections[, c(3:12, 14:16)], as.numeric
)



# saving the data sets =======================================================================================



write.csv(
  fp_finishes, row.names = F, 
  'data/fp finishes_16_18.csv'
)



write.csv(
  fp_projections, row.names = F,
  'data/fp_projections_19.csv'
  )




