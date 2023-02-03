# get host depending of if running locally or on the server
getHost <- function(){
  if(Sys.getenv('SHINY_PORT') == ""){
    return(NULL)
  } else {
    return(Sys.getenv('host'))
  }
}

# Get all user credentials
getUserbase <- function(my_conn){
  dbReadTable(conn = my_conn, name = "user_base")
}


#' Save table to selected user's master table
#'
#' @param conne database connection
#' @param user string containing the user name
#' @param tmp_tbl data frame to save
#' @param col.info named vector where names are column names and values
#' are the SQL data types
#' @return nothing
saveTableToMaster <- function(conne, user, tmp_tbl, col.info){

  # create a new master table if it doesn't exist
  hasTbl <- dbExistsTable(conn = conne, name = user)
  if(!hasTbl){
    dbCreateTable(conn = conne,
                  name = user,
                  fields = col.info)
    
    # otherwise, check a pedigree by the same name already exists
  } else {
    
    # if the pedigree already exists, treat as overwrite save and remove previous version
    db.pedids <- unique(dbGetQuery(conn = conne,
                                   statement = paste0("SELECT PedigreeID FROM ", 
                                                      user, ";"))$PedigreeID)
    if(any(db.pedids == tmp_tbl$PedigreeID[1])){
      dbExecute(conn = conne,
                statement = paste0("DELETE FROM ", user, 
                                   " WHERE PedigreeID = '", 
                                   tmp_tbl$PedigreeID[1], "';"))
    }
  }
  
  # if this is a pedigree update, remove the old pedigree first
  dbAppendTable(conn = conne,
                name = user,
                value = tmp_tbl)
}
