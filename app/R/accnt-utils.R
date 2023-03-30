# get database host depending of if running locally or on the server
getHost <- function(){
  if(Sys.getenv('SHINY_PORT') == ""){
    return(NULL)
  } else {
    return(Sys.getenv('host'))
  }
}

# generate numbers for bot check
getBotCheckNums <- function(){
  quant <- sample(2:4, size = 1)
  sample(1:6, size = quant, replace = FALSE)
}

# Get all user credentials
getUserbase <- function(my_conn){
  dbReadTable(conn = my_conn, name = "user_base")
}

#' Retrieve usernames of accounts with manager permissions, in a vector
getAllManagerUN <- function(my_conn){
  ub <- getUserbase(my_conn = my_conn)
  ub$user[which(ub$permissions == "manager")]
}

#' Retrieve vector of usernames under a given manager username
getUsersUnderManager <- function(manager, my_conn){
  mt <- dbReadTable(conn = my_conn, name = "managers")
  mt$user[which(mt$manager == manager)]
}

#' Retrieve vector of manager usernames which manage a given user account
getManagerUN <- function(un, my_conn){
  mt <- dbReadTable(conn = my_conn, name = "managers")
  mt$manager[which(mt$user == un)]
}

#' Check new password meets requirements
#' 
#' @param pwEntry string, proposed password entry 1
#' @param pwEntry2 string, proposed password entry 2
#' @return a string, "Success" if password meets are requirements, otherwise 
#' a description of the problem.
checkNewPassword <- function(pwEntry, pwEntry2){
  
  # check if passwords present
  if(nchar(pwEntry) == 0 | nchar(pwEntry2) == 0){
    mssg <- "One or both password entries are missing."
    
    # check passwords match each other
  } else if(pwEntry != pwEntry2){
    mssg <- "Passwords entries do not match."
    
    # check strength of password
  } else {
    lowerP <- grepl(pattern = "[[:lower:]]", pwEntry)
    upperP <- grepl(pattern = "[[:upper:]]", pwEntry)
    digitP <- grepl(pattern = "[[:digit:]]", pwEntry)
    punctP <- grepl(pattern = "[[:punct:]]", pwEntry)
    lengthP <- nchar(pwEntry) >= 8
    
    if(any(c(!lowerP, !upperP, !digitP, !punctP, !lengthP))){
      mssg <- "Your password does not meet the requirements."
      
      # valid password
    } else {
      mssg <- "Success"
    }
  }
  return(mssg)
}

#' Check new username meets requirements
#' 
#' Usernames will be used to name the user's MariaDB table therefore it must 
#' comply table name rules. 
#' 
#' @param unEntry string, proposed user name to check
#' @param ubu data frame of the user base for which to check for pre-existing 
#' usernames
#' @return string stating "Success" if the username meets all requirements or, 
#' if all requirements are not met, a message stating which requirements 
#' have meet violated.
#' @details 
#'  - Cases restricted to lower case to ensure compatibility with UNIX and 
#'  Windows MariaDB table name rules.
checkNewUsername <- function(unEntry, ubu){
  
  problems <- ""
  
  # check if the username is available
  avail <- !unEntry %in% ubu
  if(!avail){
    problems <- paste0(problems, " Username already exists.")
  }
  
  # check the length
  chars <- nchar(unEntry)
  len <- chars <= 64
  if(!len){
    problems <- paste0(problems, " Username too long (max 64 characters).")
  }
  
  # check that it only consists of lower case letter, digits, and _
  # alt
  content <- TRUE
  if(grepl(pattern = "[[:punct:]]|[[:upper:]]| ", unEntry)){
    
    # check for upper case letters
    u.content <- TRUE
    if(grepl(pattern = "[[:upper:]]", unEntry)){
      u.content <- FALSE
    }
    
    # check for symbols that aren't _
    sy.content <- TRUE
    if(grepl(pattern = "[[:punct:]]", unEntry)){
      syms <- unique(gsub(x = unEntry, pattern = "[[:alnum:]]", replacement = ""))
      if(all(syms != "_")){
        sy.content <- FALSE
      }
    }
    
    # check for spaces
    sp.content <- TRUE
    if(grepl(pattern = " ", unEntry)){
      sp.content <- FALSE
    }
    
    content <- u.content & sy.content & sp.content
    if(!content){
      problems <- paste0(problems, " Username contains unauthorized characters.")
    }
  }
  
  # check for reserved words
  resv.words.u <- c("CON", "PRN", "AUX", "NUL", 
                    paste0("COM", 1:9), paste0("LPT", 1:9), "CLOCK$",
                    "maria_secrets", "user_base", "managers")
  resv.words.l <- tolower(resv.words.u)
  resv <- (!unEntry %in% resv.words.u & !unEntry %in% resv.words.l)
  if(!resv){
    problems <- paste0(problems, " Username is a reserved word, pick another.")
  }
  
  # check it does not solely consist of digits
  all.digits <- TRUE
  if(grepl(pattern = "[[:digit:]]", unEntry)){
    dig.len <- nchar(gsub(x = unEntry, pattern = "[[:alpha:]]|[[:punct:]]", replacement = ""))
    all.digits <- ifelse(chars == dig.len, FALSE, TRUE)
    if(!all.digits){
      problems <- paste0(problems, " Username cannot consist of all digits.")
    }
  }
  
  # check for scientific notation at the start of word names
  sci <- TRUE
  if(grepl(pattern = "e", unEntry)){
    first.num.pos <- stringr::str_locate(string = unEntry, pattern = "[[:digit:]]")[1,1]
    if(!is.na(first.num.pos)){
      first.e.pos <- stringr::str_locate(string = unEntry, pattern = "e")[1,1]
      if(!is.na(first.e.pos)){
        if(first.num.pos == 1 & first.e.pos == 2){
          sci <- FALSE
          problems <- paste0(problems, " Username cannot start with a number followed by the letter 'e'.")
        }
      }
    }
  }
  
  # check all and return results
  all.check <- avail & len & content & resv & all.digits & sci
  if(all.check){
    return("Success")
  } else {
    return(problems)
  }
}

#' Check new email meets requirements
#' 
#' @param emEntry string, new email, first entry
#' @param emEntry2 string, new email, 2nd entry
#' @param ube character vector of emails from the user base
#' @return string, Success if all checks passed, otherwise a description of the 
#' problems with the new email address.
checkNewEmail <- function(emEntry, emEntry2, ube){
  
  # check if email entries present
  if(nchar(emEntry) == 0 | nchar(emEntry2) == 0){
    mssg <- "One or both email entries are missing."
    
    # check emails match each other
  } else if(emEntry != emEntry2){
    mssg <- "Email entries do not match."
    
    # check if email already in user base
  } else if(emEntry %in% ube){
    mssg <- "You already have an account under this email address, please sign-in."
    
    # check if @ and . present
  } else if(!grepl(pattern = "[@]", x = emEntry) | !grepl(pattern = "[.]", x = emEntry)){
    mssg <- "Email address format is incorrect; must have an '@' and a period."
    
    # no issues
  } else {
    mssg <- "Success"
  }
  return(mssg)
}

#' Check selected managers for a user account are valid
#' 
#' @param un a string containing the account username
#' @param prop.managers a character vector of poposed manager usernames
#' @return a list with two elements: mssg which contains either "Success" or an error message and 
#' not.managers which is either NA if all proposed managers were valid or a character vector of 
#' the proposed managers who are not eligible to be managers for this user account
checkManagers <- function(prop.managers, un, my_conn){
  
  # check if their own username was proposed as a manager
  own.un <- ""
  if(any(prop.managers == un)){
    own.un <- "Your username cannot be one of your managers, please remove it."
    prop.managers <- prop.managers[which(prop.managers != un)]
  } 
  
  # check if any of the proposed manager are already a manager for this user account
  dups <- ""
  users.managers <- getManagerUN(un = un, my_conn = my_conn)
  dup.managers <- intersect(prop.managers, users.managers)
  if(length(dup.managers) == 1){
    dups <- paste0("The username '", dup.managers, "' is already a manager for your account.")
  } else if(length(dup.managers) > 1){
    dups <- paste0("The following usernames are already managers for your account: '", 
                   paste0(dup.managers, collapse = "', '"), "'.")
  }
  
  # check proposed managers against actual usernames with manager permissions
  actual.managers <- getAllManagerUN(my_conn = my_conn)
  not.managers <- prop.managers[which(!prop.managers %in% actual.managers)]
  invalid.managers <- ""
  if(length(not.managers) == 1){
    invalid.managers <- paste0("The username '", not.managers,"' either do not exist or does not have manager level permissions, please remove it.")
  } else if(length(not.managers) > 1){
    invalid.managers <- paste0("These user names either do not exist or do not have manager level permissions, please remove them: '", 
                               paste0(not.managers, collapse = "', '"), "'.")
  }
  
  # consolidate the result message
  if(own.un == "" & invalid.managers == "" & dups == ""){
    mssg <- "Success"
    not.managers <- NA
  } else {
    mssg <- paste0(own.un, dups, invalid.managers)
    not.managers <- unique(c(dup.managers, not.managers))
    if(dups != ""){
      not.managers <- c(un, not.managers)
    }
  }
  
  return(list(mssg = mssg,
              not.managers = not.managers))
}

# Generate recovery code
createRecoveryCode <- function() {
  v <- c(sample(LETTERS, 2, replace = TRUE),
         sample(0:9, 2, replace = TRUE),
         sample(letters, 2, replace = TRUE))
  v <- sample(v, 6, replace = FALSE)
  return(paste0(v,collapse = ""))
}

#' Email the user recovery information
#' 
#' @param userEmail string, user's email address
#' @param emailType string, one of c("un", "pw") for a username or password 
#' recovery email
#' @param userName string, username. Optional, only required if 
#' `emailType` is "un".
#' @param rCode string, the recovery code. Optional, only required if 
#' `emailType` is "pw".
#' @return an email is sent to the user containing either their username or the 
#' password recovery code.
emailUser <- function(userEmail, emailType, userName = NULL, rCode = NULL){
  
  # username email subject and body
  if(emailType == "recoverUn"){
    if(is.null(userName)){
      warning("userName arguement missing. Cannot email username.")
    }
    subject <- "account username recovery"
    body <- paste0("Dear User, your hereditarycancer account username is: ", userName,
                   "\n\nYou can now return to the site and log-in.\n\n 
                   Thanks, \n
                   The BayesMendel Lab at Dana-Farber Cancer Institute.")
    
    # password email subject and body
  } else if(emailType == "recoverPw"){
    subject <- "account password recovery code"
    body <- paste0("Dear User, your hereditarycancer account password recovery code is: ", rCode,
                   "\n\nThe code expires in ",expireCode/1000," minutes. You can now return to the site and log-in.\n\n
                   Thanks, \n
                   The BayesMendel Lab at Dana-Farber Cancer Institute.")
  }
  
  # credentials: get token using service account method
  token <- gargle::credentials_service_account(
    scopes = c("https://mail.google.com/"), 
    path = Sys.getenv("gmail.json.path"), 
    subject = Sys.getenv("gmail.email") # Allow service account to act on behalf of this subject
  )
  # Assign token to .auth internal object from gmailr package
  assign("cred", token, gmailr:::.auth)
  
  # compose and send
  email_message <- 
    gm_mime() %>%
    gm_to(userEmail) %>%
    gm_from(Sys.getenv("gmail.noreply.email")) %>%
    gm_subject(subject) %>%
    gm_text_body(body)
  sent_id <- gm_send_message(email_message)$id
  
  # permanently delete message for privacy, but wait a few seconds to allow send to finish
  Sys.sleep(5)
  gm_delete_message(id = sent_id)
}

#' Save table to selected user's master table
#'
#' @param conne database connection
#' @param user string containing the user name
#' @param tmp_tbl data frame to save
#' @return nothing
savePedigreeToDB <- function(conne, user, tmp_tbl){
  
  # column names and data types
  if(any(colnames(tmp_tbl) == "CK5.6")){
    colnames(tmp_tbl)[which(colnames(tmp_tbl) == "CK5.6")] <- "CK5_6"
  }
  mod.ped.cols <- ped.cols
  mod.ped.cols[which(ped.cols == "CK5.6")] <- "CK5_6"
  col.info <- setNames(ped.col.dtypes, mod.ped.cols)
  
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
  
  # add to table
  dbAppendTable(conn = conne,
                name = user,
                value = tmp_tbl)
}

