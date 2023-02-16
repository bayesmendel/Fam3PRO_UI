#' Combine race and ethnicity into PanelPRO race categories
#' 
#' @param race string, one of the values from `rc.choices`.
#' @param ethnicity string, one of values from `an.choices`.
#' @returns a string with the PanelPRO race category which is one of 
#' `PanelPRO:::RACE_TYPES`
getPPRace <- function(race, ethnicity){
  if(race == "All_Races"){
    if(ethnicity %in% c("Other_Ethnicity", "Non-Hispanic")){
      pp.race <- "All_Races"
    } else if(ethnicity == "Hispanic"){
      pp.race <- "Hispanic"
    }
  } else if(race %in% c("AIAN", "Asian", "Black")){
    if(ethnicity %in% c("Other_Ethnicity", "Non-Hispanic")){
      pp.race <- race
    } else if(ethnicity == "Hispanic"){
      pp.race <- "All_Races"
    }
  } else if(race == "White"){
    if(ethnicity == "Non-Hispanic"){
      pp.race <- "WNH"
    } else if(ethnicity == "Hispanic"){
      pp.race <- "WH"
    } else if(ethnicity == "Other_Ethnicity"){
      pp.race <- "All_Races"
    }
  }
  return(pp.race)
}

#' Combine PanelPRO race categories of parents to get child's race
#' 
#' @param r1 string, parent 1's PanelPRO race categories. 
#' One of `PanelPRO:::RACE_TYPES`. If `NULL`, `"All_Races"` is assumed.
#' @param r2 string, parent 2's PanelPRO race categories. 
#' One of `PanelPRO:::RACE_TYPES`. If `NULL`, `"All_Races"` is assumed.
#' 
#' @returns a string with the combined PanelPRO race categories. One of `PanelPRO:::RACE_TYPES`.
combinePPrace <- function(r1 = NULL, r2 = NULL){
  
  # assume All_Races if inputs are NULL
  if(is.null(r1)){
    r1 <- "All_Races"
  }
  if(is.null(r2)){
    r2 <- "All_Races"
  }
  
  # special case
  hisp.white.cats <- c("Hispanic", "White", "WNH", "WH")
  
  # determine combination
  if(r1 == r2){
    return(r1)
  } else if(r1 == "All_Races" | r2 == "All_Races"){
    return("All_Races")
  } else if(r1 %in% hisp.white.cats & r2 %in% hisp.white.cats){
    return("WH")
  } else {
    return("All_Races")
  }
}

#' Combine AJ and Italian Ancestry into PanelPRO Ancestry categories
#' 
#' @param aj.anc logical, Ashkenazi Jewish ancestry
#' @param it.anc logical, Italian ancestry
#' @returns a string with the PanelPRO Ancestry category which is one of 
#' `PanelPRO:::ANCESTRY_TYPES`
getPPAncestry <- function(aj.anc, it.anc){
  if(aj.anc){
    return("AJ")
  } else if(it.anc){
    return("Italian")
  } else if(!aj.anc & !it.anc){
    return("nonAJ")
  }
}

#' Populate race, ethnicity, and ancestry data for individual using another 
#' relative's information
#' 
#' @param tmp.ped data frame, the pedigree to modify
#' @param assume.from number, the ID number of the person in the pedigree for which 
#' the race, ethnicity, and ancestry information should be referenced. If `NULL` 
#' the proband is referenced.
#' @param id number, the ID number of the subject to populated the information for. 
#' If `NULL` the the last row of the pedigree is the one that is updated.
#' 
#' @returns a modified version of `tmp.ped` with the updated race, ethnicity, and 
#' ancestry information for subject `id`.
#' 
#' @details this is convenience wrapper for `popPersonData()`.
assumeBackground <- function(tmp.ped, assume.from = NULL, id = NULL){
  
  # assume ID to be updated is the last row of the pedigree if `NULL`
  if(is.null(id)){
    id <- tmp.ped$ID[nrow(tmp.ped)]
  }
  
  # assume proband is reference if `NULL`
  if(is.null(assume.from)){
    assume.from <- tmp.ped$ID[which(tmp.ped$isProband == 1)]
  }
  
  # reference proband's race and ancestry info for later
  NPPrace <- tmp.ped$NPPrace[which(tmp.ped$ID == assume.from)]
  NPPeth <- tmp.ped$NPPeth[which(tmp.ped$ID == assume.from)]
  NPPAJ <- tmp.ped$NPPAJ[which(tmp.ped$ID == assume.from)]
  NPPIt <- tmp.ped$NPPIt[which(tmp.ped$ID == assume.from)]
  
  # update the pedigree and return it
  popPersonData(tmp.ped = tmp.ped, id = id, 
                rc = NPPrace, et = NPPeth, 
                an.aj = NPPAJ, an.it = NPPIt)
}

# validate age values are between min.age and m
validateAge <- function(cur.age, oldest.child.age = NA, youngest.parent.age = NA){
  
  # check if CurAge is not NA (NA is acceptable)
  if(!is.na(cur.age)){
    isNum <- is.numeric(cur.age)
      
    # check CurAge is in range and an integer
    if(isNum){
      inRange <- (cur.age >= min.age & cur.age <= max.age)
      isInt <- cur.age %% 1 == 0
      if(inRange & isInt){
        
        # check person is younger than their youngest parent by 10 years or more
        youngerThanParent <- TRUE
        if(!is.na(youngest.parent.age)){
          youngerThanParent <- cur.age <= youngest.parent.age - min.parent.child.age.diff
        }
        
        # check person is older than their oldest child by 10 years or more
        olderThanChild <- TRUE
        if(!is.na(oldest.child.age)){
          olderThanChild <- cur.age >= oldest.child.age + min.parent.child.age.diff
        }
        
        # message if parent or child age rules violated
        if(!youngerThanParent & !olderThanChild){
          need(all(youngerThanParent, olderThanChild),
               paste0("Age must be at least ", min.parent.child.age.diff," years older than their oldest child, ", oldest.child.age,", and 10 years younger than their youngest parent, ",youngest.parent.age,"."))
        } else if(!youngerThanParent){
          need(youngerThanParent,
               paste0("Age must be at least ", min.parent.child.age.diff," years younger than their youngest parent, ", youngest.parent.age,"."))
        } else if(!olderThanChild){
          need(olderThanChild,
               paste0("Age must be at least ", min.parent.child.age.diff," years older than their oldest child, ", oldest.child.age,"."))
        }
        
        # CurAge was not in range or was not an integer
      } else {
        need(all(isInt, inRange), paste0("Age must be an integer from ", min.age," to ",max.age,"."))
      }
    } else {
      need(isNum, paste0("Age must an integer from ", min.age," to ", max.age,"."))
    }
  }
}


