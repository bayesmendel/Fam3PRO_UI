# validate cancer age (excluding CBC)
validateCanAge <- function(in.age, cur.age){
  if(!is.na(in.age)){
    isNum <- is.numeric(in.age)
    if(isNum){
      inRange <- (in.age >= min.age & in.age <= max.age)
      isInt <- in.age %% 1 == 0
      if(inRange & isInt & !is.na(cur.age)){
        noConflict <- in.age <= cur.age
        need(noConflict, paste0("Ages must be at or below the person's current age of ", cur.age,"."))
      } else {
        need(all(isInt, inRange), paste0("Ages must be integers from ", min.age," to ",max.age,"."))
      }
    } else {
      need(isNum, paste0("Ages must be integers from ", min.age," to ", max.age,"."))
    }
  }
}


# validate CBC ages are valid and between the 1st BC age and current age
validateCBCAge <- function(can, cbc.age, bc.age, cur.age){
  if(can == "Breast" & !is.na(cbc.age)){
    isNum <- is.numeric(cbc.age)
    if(isNum){
      inRange <- (cbc.age >= min.age & cbc.age <= max.age)
      isInt <- cbc.age %% 1 == 0
      if(isInt & inRange){
        if(is.na(bc.age) & !is.na(cur.age)){
          noCurAgeConflict <- cbc.age <= cur.age
          need(noCurAgeConflict, paste0("Ages must be at or below the person's current age of ", cur.age,"."))
        } else if(!is.na(bc.age) & is.na(cur.age)){
          noBCAgeConflict <- cbc.age > bc.age
          need(noBCAgeConflict, paste0("CBC age must be greater than 1st BC age."))
        } else if(!is.na(bc.age) & !is.na(cur.age)){
          noConflict <- (cbc.age > bc.age & cbc.age <= cur.age)
          need(noConflict, paste0("CBC age must be greater than the 1st BC age, ",bc.age,", to the current age, ",cur.age,"."))
        }
      } else {
        need(all(isInt, inRange), paste0("Ages must be integers from ",min.age," to ",max.age,"."))
      }
      
    } else {
      need(isNum, paste0("Ages must be integers from ",min.age," to ",max.age,"."))
    }
  }
}

#' Create data frame of cancer history 
#'
#' @param cr a list containing the cancer history module tracking information
#' @param rel a number, the ID number of the relative for which to create the data frame
#' @param inp, the shiny input list
#' @return a data frame of cancer history for a relative with columns: Cancer, Age, and Other
makeCancerDF <- function(rel, cr = canReactive$canNums, inp = input){
  
  # consolidate all cancer inputs into a single data frame by looping through each exiting module
  can.df <- cancer.inputs.store
  trackInputs <- cr[[rel]]$dict
  if(!(length(trackInputs) == 1 & is.na(trackInputs[1]))){
    for(cn in as.numeric(names(trackInputs))){
      id <- paste0("rel", rel, "canModule", trackInputs[cn])
      if(inp[[paste0(id, '-Can')]] != "No cancer selected"){
        can.df[cn, ] <- c(inp[[paste0(id, '-Can')]],
                          inp[[paste0(id, '-CanAge')]],
                          inp[[paste0(id, '-CanOther')]])
      }
      
      # check for CBC
      hadCBC <- FALSE
      CBCAge <- NA
      if(inp[[paste0(id, '-Can')]] == "Breast" &
         inp[[paste0(id, "-CBC")]] == "Yes"){
        hadCBC <- TRUE
        CBCAge <- inp[[paste0(id, "-CBCAge")]]
      }
    }
    
    # add CBC as last row of the data frame
    if(hadCBC){
      can.df[nrow(can.df)+1, ] <- c("Contralateral",
                                    CBCAge,
                                    "")
    }
    
    # no cancer entered for this person, create 1 row placeholder data frame
  } else {
    can.df[1, ] <- c("No cancer selected", NA, "")
  }
  
  can.df
}

#' Add one cancerUI module to a relative's cancer hx
#' 
#' @param cr canReactive$canNums
#' @param rel the relative
#' @param inp the shiny input object.
#' @returns a list:
#' - cr: updated copy of canReactive$canNums
#' - trackMax: the unique number associated with the canUI module
#' - id: the full canUI module id
addCancer <- function(cr = canReactive$canNums, rel, inp = input){
  
  # look-up the maximum number of created cancer UI modules for the current
  # relative add the order of active cancer UI modules
  trackInputs <- cr[[rel]]$dict
  trackMax <- cr[[rel]]$mx

  # increase this person's count of cancer input modules by 1
  if(length(trackInputs) == 1 & is.na(trackInputs[1])){
    trackInputs[1] <- trackMax + 1
  } else {
    next.slot <- max(as.numeric(names(trackInputs))) + 1
    trackInputs <- c(trackInputs, setNames(trackMax + 1, next.slot))
  }

  # increase total number of created cancer UI modules for this person
  trackMax <- trackMax + 1

  # update canNums for this person
  cr[[rel]] <- list(dict = trackInputs,
                    mx = trackMax)

  # create the unique module ID and insert the UI module
  id <- paste0("rel", rel, "canModule", trackMax)
  insertUI(
    selector = "#canContainer",
    where = "beforeEnd",
    ui = canUI(id = id, rel = rel)
  )

  # add a server for checking the validity of the entered cancer age
  validateCanAgeServer(id,
                       in.age = inp[[paste0(id, "-CanAge")]],
                       cur.age = inp$Age)

  # add a server for checking the validity of the entered CBC age
  validateCBCAgeServer(id,
                       can = inp[[paste0(id, "-Can")]],
                       cbc.age = inp[[paste0(id, "-CBCAge")]],
                       bc.age = inp[[paste0(id, "-CanAge")]],
                       cur.age = inp$Age)
  
  return(list(cr = cr, trackMax = trackMax, id = id))
}


#' Remove one cancerUI module from a relative's cancer hx
#' 
#' @param cr canReactive$canNums
#' @param rel the relative
#' @param inp the shiny input object.
#' @param ss shiny session
#' @param trackMax a number, the unique number associated with the cancerUI module ID to be deleted
#' @return an updated copy of canReactive$canNums
removeCancer <- function(cr = canReactive$canNums, rel, 
                         inp = input, ss = session, 
                         trackMax = trackMax){
  
  # get current version of active cancer modules
  tmp.trackInputs <- cr[[rel]]$dict
  
  # get the module id
  idd <- paste0("rel", rel, "canModule", trackMax)

  ## re-add deleted cancer choice to dropdown choices of this person's other cancer modules
  # get all of the cancers currently selected
  cans.selected <- as.character()
  if(!(length(tmp.trackInputs) == 1 & is.na(tmp.trackInputs[1]))){
    for(cn in as.numeric(names(tmp.trackInputs))){
      tmp.id <- paste0("rel", rel, "canModule", tmp.trackInputs[cn], "-Can")
      if(!inp[[tmp.id]] %in% c("No cancer selected", "Other") &
         inp[[tmp.id]] != inp[[paste0(idd, "-Can")]]){
        cans.selected <- c(cans.selected, inp[[tmp.id]])
      }
    }

    # update all cancer choices
    for(cn in as.numeric(names(tmp.trackInputs))){
      tmp.id <- paste0("rel", rel,
                       "canModule", tmp.trackInputs[cn], "-Can")

      # get cancer dropdown choices available for this cancer name input
      mod.cans.selected <- cans.selected[which(cans.selected != inp[[tmp.id]])]
      cans.avail <- CANCER.CHOICES$long[which(!CANCER.CHOICES$long %in% mod.cans.selected)]

      # update the input dropdown
      updateSelectInput(ss, tmp.id,
                        choices = cans.avail,
                        selected = inp[[tmp.id]])
    }
  }

  ## re-add deleted OTHER cancer choice to dropdown choices of this person's OTHER cancer modules
  # get all of the cancers currently selected across this person's cancer UI modules
  cans.selected <- as.character()
  if(!(length(tmp.trackInputs) == 1 & is.na(tmp.trackInputs[1]))){
    for(cn in as.numeric(names(tmp.trackInputs))){
      tmp.id <- paste0("rel", rel,
                       "canModule", tmp.trackInputs[cn], "-CanOther")
      if(inp[[tmp.id]] != "Unknown/Not Listed" &
         inp[[tmp.id]] != inp[[paste0(idd, "-CanOther")]]){
        cans.selected <- c(cans.selected, inp[[tmp.id]])
      }
    }

    # update each of this person's cancer UI module OTHER cancer choice dropdowns to exclude the newly selected OTHER cancer
    for(cn in as.numeric(names(tmp.trackInputs))){
      tmp.id <- paste0("rel", rel,
                       "canModule", tmp.trackInputs[cn], "-CanOther")

      # get OTHER cancer dropdown choices available for this OTHER cancer name and input
      mod.cans.selected <- cans.selected[which(cans.selected != inp[[tmp.id]])]
      cans.avail <- OTHER.CANCER.CHOICES[which(!OTHER.CANCER.CHOICES %in% mod.cans.selected)]

      # update the input dropdown
      updateSelectInput(ss, tmp.id,
                        choices = cans.avail,
                        selected = inp[[tmp.id]])
    }
  }

  ## delete the module and UI
  # remove the module from the UI
  removeUI(selector = paste0("#canSubContainer",idd))

  ## remove module's index from the vector of active modules
  ## decrease the active cancer module count by one
  ## shift remaining active cancer modules to different slots
  if(length(tmp.trackInputs) == 1 & !is.na(tmp.trackInputs[1])){
    tmp.trackInputs[1] <- NA
  } else if(!(length(tmp.trackInputs) == 1 & is.na(tmp.trackInputs[1]))){

    # if the input to be removed is not the last one, iterate through the active inputs to update them
    if(which(tmp.trackInputs == trackMax) != length(tmp.trackInputs)){
      for(el in which(tmp.trackInputs == trackMax):(length(tmp.trackInputs) - 1)){
        tmp.trackInputs[el] <- tmp.trackInputs[el+1]
      }
    }
    tmp.trackInputs <- tmp.trackInputs[1:(length(tmp.trackInputs) - 1)]
  }
  cr[[rel]]$dict <- tmp.trackInputs
  
  # return updated cancer reactive
  return(cr)
}


#' Update the selectInput choices for selecting cancer names in the other canUI modules 
#' based on when a cancer is selected in one canUI module
updateCancerDropdowns <- function(cr = canReactive$canNums,
                                  rel,
                                  inp = input,
                                  ss = session){
  
  # get current version of active cancer modules
  tmp.trackInputs <- cr[[rel]]$dict
  
  # get all of the cancers currently selected across this person's cancer UI modules
  cans.selected <- as.character()
  for(cn in as.numeric(names(tmp.trackInputs))){
    tmp.id <- paste0("rel", rel, "canModule", tmp.trackInputs[cn], '-Can')
    if(!inp[[tmp.id]] %in% c("No cancer selected", "Other")){
      cans.selected <- c(cans.selected, inp[[tmp.id]])
    }
  }
  
  # update each of this person's cancer UI module cancer choice dropdowns to exclude the newly selected cancer
  for(cn in as.numeric(names(tmp.trackInputs))){
    tmp.id <- paste0("rel", rel, "canModule", tmp.trackInputs[cn], '-Can')
    
    # get cancer dropdown choices available for this cancer name input
    mod.cans.selected <- cans.selected[which(cans.selected != inp[[tmp.id]])]
    cans.avail <- CANCER.CHOICES$long[which(!CANCER.CHOICES$long %in% mod.cans.selected)]
    
    # update the input dropdown
    updateSelectInput(ss, tmp.id,
                      choices = cans.avail,
                      selected = inp[[tmp.id]])
  }
}

updateOtherCancerDropdowns <- function(cr = canReactive$canNums,
                                       rel,
                                       inp = input,
                                       ss = session){
  
  # get current version of active cancer modules
  tmp.trackInputs <- cr[[rel]]$dict
  
  # get all of the OTHER cancers currently selected across this person's cancer UI modules
  cans.selected <- as.character()
  for(cn in as.numeric(names(tmp.trackInputs))){
    tmp.id <- paste0("rel", rel, "canModule", tmp.trackInputs[cn], '-CanOther')
    if(inp[[tmp.id]] != "Unknown/Not Listed"){
      cans.selected <- c(cans.selected, inp[[tmp.id]])
    }
  }
  
  # update each of this person's cancer UI module OTHER cancer choice dropdowns to exclude the newly selected OTHER cancer
  for(cn in as.numeric(names(tmp.trackInputs))){
    tmp.id <- paste0("rel", rel, "canModule", tmp.trackInputs[cn], '-CanOther')
    mod.cans.selected <- cans.selected[which(cans.selected != inp[[tmp.id]])]
    cans.avail <- OTHER.CANCER.CHOICES[which(!OTHER.CANCER.CHOICES %in% mod.cans.selected)]
    updateSelectInput(ss, tmp.id,
                      choices = cans.avail,
                      selected = inp[[tmp.id]])
  }
}





