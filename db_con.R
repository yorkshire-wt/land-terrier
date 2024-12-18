library(DBI)
library(RPostgreSQL)
library(glue)

# Connect DB function

db_con <- function(db=NA, user=NA, password_required = F, verbose = T) {
  
  if(!is.na(db)) {
    
    config <- config::get(file = 'config.yml', config = db)
    
  } else {
    
    config <- config::get(file = 'config.yml')
    
  }
  
  if (is.na(user)) {
    
    usr <- Sys.info()[["user"]]
    
  } else {
    
    usr <- user
    
  }
  
  if(password_required) {
    
    pwd <- rstudioapi::askForPassword(prompt = paste0('Password for ', usr, ':'))
    con <- DBI::dbConnect(RPostgres::Postgres(),
                          host = config$host,
                          dbname = config$dbname,
                          port = config$port,
                          user = usr,
                          password = pwd)
    
  } else {

    con <- DBI::dbConnect(RPostgres::Postgres(),
                          host = config$host,
                          dbname = config$dbname,
                          port = config$port,
                          user = usr)
    
  }
  
  if (verbose) {
  
  cat(glue('Established connection...
            Database: {config$dbname}
            User: {usr}\n'))
  }

  return(con)
}
