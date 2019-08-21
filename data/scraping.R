# this script will pull ESPN pre-season 
# fantasy rankings from ESPN web pages
# and fantasy season finishes
# and save them for later use as a csv


# libraries

library(rvest)
library(tidyr)
library(dplyr)






# reading in final rankings ======================================================================





for( i in c(2016:2018)){
  
  temp_url <- paste(
    'https://www.fantasypros.com/nfl/reports/leaders/?year=', 
    i, '&start=1&end=16', sep = ''
  )
  
  temp_webpage <- read_html(temp_url)
  
  temp_tables <- html_nodes(temp_webpage, 'table')
  
  table_count <- c(1 : length(temp_tables))
  
  for( t in table_count){
    
    if(temp_tables[[t]] %>% html_table %>% 
       nrow() > 100){
      tbl_num <- t
    }
  }
  
  temp_finish <- temp_tables[[tbl_num]] %>% 
    html_table()
  
  temp_assign <- paste(
    'finish_', substr(i, 3, 4), sep = ''
  )
  
  temp_finish <- temp_finish %>% mutate(
    year = i
  )
  
  assign(temp_assign, temp_finish)
  
}





# cleaning and binding finishes =====================================================================

fp_finishes <- bind_rows(
  finish_16, finish_17, finish_18
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


fp_finishes <- fp_finishes %>% group_by(
  year, Position
) %>% mutate(
  finish = rank(desc(Points), ties.method = 'first')
) %>% ungroup()






# reading in projection data =========================================================================



finish_url_base <- 'https://www.fantasypros.com/nfl/projections/'

positions <- c('qb', 'wr', 'rb', 'te')


for(i in positions){
  
  temp_url <- paste(
    finish_url_base, i, '.php?week=draft', sep = ''
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
  
  
  # only selecting first and last rows because these are 
  # always player name and projected fantasy points
  temp_finish <- temp_finish[, c(1, ncol(temp_finish))]
  
  names(temp_finish) <- temp_finish[2, ]
  
  temp_finish <- temp_finish[-c(1:2), ]
  
  temp_assign <- paste(
    'projection_', i, sep = ''
  )
  
  temp_finish <- temp_finish %>% mutate(
    position = toupper(i)
  )
  
  assign(temp_assign, temp_finish)
  
}






# binding projection data ===========================================================================

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


fp_projections$FPTS <- as.numeric(fp_projections$FPTS)

fp_projections <- fp_projections %>% group_by(
  position
) %>% mutate(
  finish = rank(desc(FPTS), ties.method = 'first')
) %>% ungroup()








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




