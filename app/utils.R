#' Add New Person to the Pedigree or Create Pedigree
#' 
#' @param relation string, relationship to proband, one of 
#' `c("proband","mother","father","partner","daughter","son","rel.partner","niece","nephew","sister","brother","grandmother","grandfather","aunt","uncle","cousin")`.
#' Note `"rel.partner"` is used to create the partner of someone already in the 
#' pedigree.
#' @param tmp.ped data frame, pedigree. If set to `proband` a three person 
#' pedigree is created with the proband and their two parents.
#' @param ped.id string, 
#' @param m.id number, the person's mother ID. Only required if mother ID cannot 
#' be determined from `relation`. Not required when relation is `"proband"`.
#' @param f.id number, the person's father ID. Only required if father ID cannot 
#' be determined from `relation`. Not required when relation is `"proband"`.
#' @param c.ids numeric vector of person's childrens' IDs. This argument is not 
#' currently required for any relation but may be needed if great relation 
#' functionality is added.
#' @param m.or.p.side character string of either `c("m","p")` for maternal or 
#' paternal. Only required when a side cannot be determined from `relation`.
#' @param partner.of number, the ID of someone in the pedigree for which the new 
#' person had children with. Only required for new people who "married in" to 
#' the family (ie brother's wife or aunt's husband). `relation` argument must be 
#' `"rel.partner"`
#' @param sx binary, person's sex (at birth). `0` for female or `1` for male. 
#' Only required if the sex cannot be determined from `relation`.
#' @returns an updated pedigree data frame with one added row for the new person 
#' where the new person has a correct PedigreeID, ID, relationship, side, Sex, 
#' isProband, MotherID, and FatherID. All other column contain default values.
#' @details 
#' - People must be added to the pedigree in a specific order for this function 
#' to create a correct pedigree. This order will be controlled by another function.
#' - When parents or grandparents are added their childrens' `MotherID` and 
#' `FatherID` columns will automatically update to reflect the new person. 
#' - The `relation` argument is always required and `tmp.ped` is required for 
#' every `relation` except `"proband"`. The other required arguments by `relation` 
#' type are: 
#'   - `"proband"`: `ped.id` and `sx`
#'   - `"mother"`, `"father"`, `"partner"`, `"sister"`, or `"brother"`: no other 
#'   arguments required
#'   - `"son"` or `"daughter"`: `m.id` OR `f.id` of the non-proband parent
#'   - `"rel.partner"`: `partner.of`
#'   - `"niece"` or `"nephew"`: `m.id` AND `f.id`
#'   - `"grandmother"`, `"grandfather"`, `"aunt"`, or `"uncle"`: `m.or.p.side`
#'   - `"cousin"`: `m.id`, `f.id`, and `sx`
#' - Default values by columns:
#'   - `PedigreeID`,`ID`,`relationship`,`Sex`: no defaults used.
#'   - `side`, parent IDs, `CurAge`, prophylactic surgery ages, tumor markers, 
#'   cancer diagnosis ages, and all gene columns: `NA`
#'   - `isProband`,`isDead`,`Twins`, prophylactic surgery statuses, and cancer 
#'   affection : `0`
#'   - `race`: `"All_Races"`
#'   - `Ancestry`: `"nonAJ"`
#' - half, step, and great type relations are not currently supported.
formatNewPerson <- function(relation, tmp.ped = NULL, ped.id = NULL, 
                            m.id = NULL, f.id = NULL, c.ids = NULL, 
                            m.or.p.side = NULL, partner.of = NULL, sx = NULL){
  
  # check inputs are valid
  possible.relations <- c("proband","mother","father","partner","daughter","son",
                          "rel.partner","niece","nephew","sister","brother",
                          "grandmother","grandfather","aunt","uncle","cousin"
                          #, paste0("step.", c("mother","father","daughter","son")),
                          # paste0("half.", c("sister","brother")),
                          # paste0("g.", c("grandmother","grandfather","aunt","uncle"))
                          )
  if(!relation %in% c(possible.relations)){
    stop("argument relation is not recongized.")
  }
  if(!is.null(m.id)){
    if(!is.numeric(m.id) | length(m.id) != 1){
      stop("argument m.id is either not numeric or has length > 1.")
    }
  }
  if(!is.null(f.id)){
    if(!is.numeric(f.id) | length(f.id) != 1){
      stop("argument f.id is either not numeric or has length > 1.")
    }
  }
  if(!is.null(c.ids)){
    if(!is.numeric(c.ids)){
      stop("argument c.ids is not numeric.")
    }
  }
  possible.sides <- c("m","p")
  if(!is.null(m.or.p.side)){
    if(!m.or.p.side %in% possible.sides){
      stop("argument m.or.p.side not recongized.")
    }
  }
  if(!is.null(sx)){
    if(!(is.numeric(sx) & sx %in% c(0,1))){
      stop("argument sx is not a binary 0 or 1 numeric value.")
    }
  }
  
  ## check combination of inputs are valid by relation type
  if(relation == "proband"){
    if(is.null(ped.id) | is.null(sx) | !is.null(tmp.ped) | !is.null(m.id) | !is.null(f.id) | 
       !is.null(c.ids) | !is.null(m.or.p.side) | !is.null(partner.of)){
      stop("For 'proband', provide non-NULL values for 'ped.id' and 'sx' only")
    }
    
    # this is the only requirement for mother, father, sister, brother
  } else if(is.null(tmp.ped) | !is.null(ped.id)){
    stop("For all non-probands, 'tmp.ped' must be non-NULL and 'ped.id' must be NULL")
    
  } else if(relation %in% c("son","daughter")){
    if(!xor(is.null(m.id), is.null(f.id)) | !is.null(c.ids) | !is.null(m.or.p.side) | 
       !is.null(partner.of) | !is.null(sx)){
      stop("For 'daughter' or 'son', provide non-NULL values for 'relation', 'tmp.ped' and either 'm.id' OR 'f.id' only; provide parent ID for the non-proband parent only.")
    }
    
  } else if(relation == "rel.partner"){
    if(is.null(partner.of) | !is.null(sx) | !is.null(m.id) | !is.null(f.id) | 
       !is.null(c.ids) | !is.null(m.or.p.side)){
      stop("For 'rel.partner', provide non-NULL values for 'tmp.ped' and 'partner.of' only")
    }
    
  } else if(relation %in% c("niece","nephew","cousin")){
    if(is.null(m.id) | is.null(f.id)){
      stop("For 'niece', 'nephew' and 'cousin', provide non-NULL values for 'tmp.ped', 'm.id' and 'f.id' only")
    } else if(relation != "cousin" & 
              (!is.null(c.ids) | !is.null(m.or.p.side) | !is.null(partner.of) | !is.null(sx))){
      stop("For 'niece' and 'nephew', provide non-NULL values for 'tmp.ped', 'm.id', and 'f.id' only")
    } else if(relation == "cousin" & (is.null(sx) | !is.null(c.ids) | !is.null(m.or.p.side) | !is.null(partner.of))){
      stop("For 'cousin', provide non-NULL values for 'tmp.ped', 'm.id', 'f.id', and 'sx' only")
    }
    
  } else if(relation %in% c("grandmother","grandfather","aunt","uncle")){
    if(is.null(m.or.p.side) | !is.null(m.id) | !is.null(f.id) | !is.null(c.ids) | 
       !is.null(partner.of) | !is.null(sx)){
      stop("For grandparents, 'aunt' and 'uncle', provide non-NULL values for 'tmp.ped' and 'm.or.p.side' only")
    }
  }
  
  # assign unique id for the person
  # proband is always 1, mother always 2, and father always 3
  # all other ID numbers are the next highest available number
  if(!is.null(tmp.ped)){
    ped.id <- tmp.ped$PedigreeID[1]
    if(relation == "mother"){
      tmp.id <- 2
    } else if (relation == "father"){
      tmp.id <- 3
    } else {
      tmp.id <- max(tmp.ped$ID)+1
    }
  } else {
    tmp.id <- 1
  }
  
  # assign side if needed but not specified and can be implied by relation
  if(relation == "rel.partner"){
    tmp.side <- tmp.ped$side[tmp.ped$ID == partner.of]
  } else if(relation %in% c("niece","nephew","cousin")){
    tmp.side <- tmp.ped$side[which(tmp.ped$ID == m.id)]
  }
  
  # create and format a 1 row data frame with default values
  tmp.person <- as.data.frame(matrix(NA, nrow = 1, ncol = length(ped.cols))) 
  colnames(tmp.person) <- ped.cols
  tmp.person <-
    tmp.person %>%
    mutate(across(everything(), ~is.numeric(.))) %>%
    mutate(across(.cols = c(race, Ancestry, NPP.race, NPP.eth, panel.name), 
                  ~is.character(.))) %>%
    mutate(PedigreeID = ped.id) %>%
    mutate(ID = tmp.id) %>%
    mutate(relationship = ifelse(relation == "rel.partner", paste0("partner.of.",partner.of), relation)) %>%
    mutate(side = ifelse(grepl(pattern = "partner.of", relationship) | relationship %in% c("niece","nephew","cousin"), tmp.side, 
                         ifelse(!is.null(m.or.p.side), m.or.p.side, side))) %>%
    mutate(race = "All_Races") %>%
    mutate(Ancestry = "nonAJ") %>%
    mutate(NPP.race = "All_Races") %>%
    mutate(NPP.eth = "Other_Ethnicity") %>%
    mutate(panel.name = "none") %>%
    mutate(across(.cols = c(isProband, isDead, Twins, NPP.AJ, NPP.It,
                            starts_with("riskmod"), starts_with("isAff")), 
                  ~ 0)) %>%
    mutate(across(.cols = where(is.logical), ~as.numeric(NA)))
  
  # if proband, create the pedigree, and return it
  if(relation == "proband"){
    tmp.ped <- tmp.person %>% mutate(isProband = 1, Sex = sx)
    return(tmp.ped)
  } 
  
  ## for non-probands
  # assign sex, using implied sex when possible
  if(relation == "partner"){
    partner.sex <- ifelse(tmp.ped$Sex[tmp.ped$isProband == 1] == 0, 1, 0)
    tmp.person <- tmp.person %>% mutate(Sex = partner.sex)
  } else if(relation %in% c("aunt","niece") | grepl(pattern = "mother|sister|daughter", relation)){
    tmp.person <- tmp.person %>% mutate(Sex = 0)
  } else if(relation %in% c("uncle","nephew") | grepl(pattern = "father|brother|son", relation)){
    tmp.person <- tmp.person %>% mutate(Sex = 1)
  } else if(relation == "rel.partner"){
    rel.partner.sex <- ifelse(tmp.ped$Sex[tmp.ped$ID == partner.of] == 0, 1, 0)
    tmp.person <- tmp.person %>% mutate(Sex = rel.partner.sex)
  } else {
    tmp.person <- tmp.person %>% mutate(Sex = sx)
  }
  
  # assign mother and father id, using implied mothers and fathers when possible
  if(relation %in% c("sister","brother")){
    tmp.person$MotherID <- tmp.ped$MotherID[tmp.ped$isProband == 1]
    tmp.person$FatherID <- tmp.ped$FatherID[tmp.ped$isProband == 1]
  } else if(relation %in% c("daughter","son")){
    if(tmp.ped$Sex[tmp.ped$isProband == 1] == 0){
      tmp.person$MotherID <- tmp.ped$ID[tmp.ped$isProband == 1]
      tmp.person$FatherID <- f.id
    } else if(tmp.ped$Sex[tmp.ped$isProband == 1] == 1){
      tmp.person$FatherID <- tmp.ped$ID[tmp.ped$isProband == 1]
      tmp.person$MotherID <- m.id
    }
  } else if(relation %in% c("aunt","uncle")){
    if(m.or.p.side == "m"){
      tmp.person$MotherID <- tmp.ped$MotherID[tmp.ped$ID == tmp.ped$MotherID[tmp.ped$isProband == 1]]
      tmp.person$FatherID <- tmp.ped$FatherID[tmp.ped$ID == tmp.ped$MotherID[tmp.ped$isProband == 1]]
    } else if(m.or.p.side == "p"){
      tmp.person$MotherID <- tmp.ped$MotherID[tmp.ped$ID == tmp.ped$FatherID[tmp.ped$isProband == 1]]
      tmp.person$FatherID <- tmp.ped$FatherID[tmp.ped$ID == tmp.ped$FatherID[tmp.ped$isProband == 1]]
    }
  } else if(!is.null(m.id) & !is.null(f.id)){
    tmp.person$FatherID <- f.id
    tmp.person$MotherID <- m.id
  }
  
  # assign mother or father id to children of the person, if children are already
  # in the pedigree but one or more parents are not
  if(relation == "mother"){
    tmp.ped$MotherID[tmp.ped$isProband == 1] <- tmp.id
  } else if(relation == "father"){
    tmp.ped$FatherID[tmp.ped$isProband == 1] <- tmp.id
  } else if(relation == "grandmother"){
    if(m.or.p.side == "m"){
      tmp.ped$MotherID[which(tmp.ped$ID == tmp.ped$MotherID[tmp.ped$isProband == 1])] <- tmp.id
    } else if(m.or.p.side == "p"){
      tmp.ped$MotherID[which(tmp.ped$ID == tmp.ped$FatherID[tmp.ped$isProband == 1])] <- tmp.id
    }
  } else if(relation == "grandfather"){
    if(m.or.p.side == "m"){
      tmp.ped$FatherID[which(tmp.ped$ID == tmp.ped$MotherID[tmp.ped$isProband == 1])] <- tmp.id
    } else if(m.or.p.side == "p"){
      tmp.ped$FatherID[which(tmp.ped$ID == tmp.ped$FatherID[tmp.ped$isProband == 1])] <- tmp.id
    }
  } else if(!is.null(c.ids) & !is.null(sx)){
    for(ci in c.ids){
      if(sx == 0){
        tmp.ped$MotherID[which(tmp.ped$ID == ci)] <- tmp.id
      } else if(sx == 1){
        tmp.ped$FatherID[which(tmp.ped$ID == ci)] <- tmp.id
      }
    }
  }
  
  # bind new person to bottom of pedigree and return
  tmp.ped <- rbind(tmp.ped, tmp.person)
  return(tmp.ped)
}


#' Label Identical/Monozygotic Twins in a Pedigree
#' 
#' @param tmp.ped a pedigree data frame
#' @param twins a numeric vector of the two ID numbers for a set of twins in the 
#' pedigree.
#' @returns a modified `tmp.ped` where the pair of `twins` have a unique number in 
#' the `Twins` column.
#' @details `PanelPRO` does not currently support triplets or other higher order 
#' monozygotic relationships.
labelTwins <- function(tmp.ped, twins){
  if(length(twins) != 2){
    stop("twins argument must be of length 2")
  }
  tmp.ped$Twins[which(tmp.ped$ID %in% twins)] <- max(tmp.ped$Twins) + 1
  return(tmp.ped)
}


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


#' Populate Data of Modify Data for a Person in a Pedigree
#' 
#' @param tmp.ped a pedigree data frame
#' @param id number, person's ID number in the pedigree. Must be provided unless 
#' `is.proband` is `TRUE`.
#' @param is.proband logical, `TRUE` if the person is the proband, `FALSE` otherwise. 
#' Default is `FALSE`.
#' @param cur.age number, the person's current age, if alive, or age of death
#' @param is.dead binary indicating if person is dead, 0 if alive, 1 if dead.
#' @param rc string, race, one of `rc.choices`.
#' @param et string, ethnicity, one of `et.choices`.
#' @param an.aj logical, Ashkenazi Jewish Ancestry.
#' @param an.it logical, Italian Ancestry.
#' @param m.id number, mother's pedigree ID.
#' @param f.id number, father's pedigree ID.
#' @param twin number, twin pair number.
#' @param riskmods.and.ages list of prophylactic surgery intervention statuses 
#' and surgery ages for mastectomies, hysterectomies and oophorectomies with 
#' two componenets
#' - `$riskmod`: named binary vector of length 3 with surgery statues where names 
#' are `c("mast","hyst","ooph")`.
#' - `$interAge`: named numeric vector of with the same names as `$riskmod`. 
#' If the corresponding surgery did not occur, the age value is `NA`, otherwise 
#' the value is an age between `min.age` and `max.age`.
#' @param cancers.and.ages list of cancer affection statuses and diagnosis ages 
#' with two components: 
#' - `$isAff`: named binary vector of cancer affection statuses with names: 
#' `PanelPRO:::CANCER_NAME_MAP$short` and of length `length(PanelPRO:::CANCER_NAME_MAP$short)`.
#' Values are either `0` for not affected or `1` for affected.
#' - `$Age`: named numeric vector of cancer diagnosis ages where names are the 
#' same as `$isAff` and values are ages from `min.age` to `max.age`.
#' @param er string, ER tumor marker testing status, one of `c("Not Tested", "Positive", "Negative)`
#' @param pr string, PR tumor marker testing status, one of `c("Not Tested", "Positive", "Negative)`
#' @param her2 string, HER2 tumor marker testing status, one of `c("Not Tested", "Positive", "Negative)`
#' @param ck5.6 string, CK5.6 tumor marker testing status, one of `c("Not Tested", "Positive", "Negative)`
#' @param ck14 string, CK14 tumor marker testing status, one of `c("Not Tested", "Positive", "Negative)`
#' @param msi string, MSI tumor marker testing status, one of `c("Not Tested", "Positive", "Negative)`
#' @param gene.results named numeric vector of gene test results with names 
#' `PanelPRO:::GENE_TYPES` and length `length(PanelPRO:::GENE_TYPES)`. The values 
#' are `NA` for no test, `0` for negative/B/LP, `1` for P/LP, and `2` for VUS.  
#' Must be provided when `panel.name` is not `NULL`.
#' @param panel.name string, name of the panel tested. Must be provided when 
#' `gene.results` is not `NULL`.
#' @returns a modified version of `tmp.ped` where the person with `id` has updated 
#' values based on the arguments supplied.
#' @details This function cannot be used to modify `PedigreeID`, a person's `ID`,
#' who the proband is (`isProband`) or linkage information such as `Twins`,`side`, 
#' `relationship`, `Sex`, `MotherID`, or `FatherID`. Use the `modLinkInfo`, 
#' `labelTwins`, or `changePedID` functions for those changes instead.
popPersonData <- function(tmp.ped, 
                          id = NULL, 
                          is.proband = FALSE, 
                          cur.age = NULL, 
                          is.dead = NULL, 
                          rc = NULL, 
                          et = NULL, 
                          an.aj = NULL,
                          an.it = NULL,
                          riskmods.and.ages = NULL,
                          cancers.and.ages = NULL,
                          er = NULL,
                          pr = NULL,
                          her2 = NULL,
                          ck5.6 = NULL,
                          ck14 = NULL,
                          msi = NULL,
                          gene.results = NULL,
                          panel.name = NULL){
  
  # check for a pedigree
  if(is.null(tmp.ped)){
    stop("argument tmp.ped cannot be NULL")
  } else if(!is.data.frame(tmp.ped)){
    stop("value supplied for tmp.ped is not a data frame")
  }
  
  # get proband's ID if is.proband == TRUE
  if(is.null(id) & !is.null(is.proband)){
    if(is.proband){
      id <- tmp.ped$ID[which(tmp.ped$isProband == 1)]
    }
  }
  
  # demographics
  if(!is.null(cur.age)){ tmp.ped$CurAge[which(tmp.ped$ID == id)] <- cur.age }
  if(!is.null(is.dead)){ tmp.ped$isDead[which(tmp.ped$ID == id)] <- is.dead }
  if(!is.null(rc) & !is.null(et)){ 
    tmp.ped$race[which(tmp.ped$ID == id)] <- getPPRace(rc, et) 
    tmp.ped$NPP.race[which(tmp.ped$ID == id)] <- rc
    tmp.ped$NPP.eth[which(tmp.ped$ID == id)] <- et
  }
  if(!is.null(an.aj) & !is.null(an.it)){ 
    tmp.ped$Ancestry[which(tmp.ped$ID == id)] <- getPPAncestry(an.aj, an.it) 
    tmp.ped$NPP.AJ[which(tmp.ped$ID == id)] <- an.aj
    tmp.ped$NPP.It[which(tmp.ped$ID == id)] <- an.it
  }
  
  # surgical hx
  if(!is.null(riskmods.and.ages)){
    
    # surgery status
    tmp.ped$riskmodMast[which(tmp.ped$ID == id)] <- 
      riskmods.and.ages$riskmod[which(names(riskmods.and.ages$riskmod) == "mast")]
    tmp.ped$riskmodHyst[which(tmp.ped$ID == id)] <- 
      riskmods.and.ages$riskmod[which(names(riskmods.and.ages$riskmod) == "hyst")]
    tmp.ped$riskmodOoph[which(tmp.ped$ID == id)] <- 
      riskmods.and.ages$riskmod[which(names(riskmods.and.ages$riskmod) == "ooph")]
    
    # surgery age
    tmp.ped$interAgeMast[which(tmp.ped$ID == id)] <- 
      riskmods.and.ages$interAge[which(names(riskmods.and.ages$interAge) == "mast")]
    tmp.ped$interAgeHyst[which(tmp.ped$ID == id)] <- 
      riskmods.and.ages$interAge[which(names(riskmods.and.ages$interAge) == "hyst")]
    tmp.ped$interAgeOoph[which(tmp.ped$ID == id)] <- 
      riskmods.and.ages$interAge[which(names(riskmods.and.ages$interAge) == "ooph")]
  }
  
  # cancer hx
  if(!is.null(cancers.and.ages)){
    
    # clear existing values
    tmp.ped[which(tmp.ped$ID == id), paste0("isAff", setdiff(CANCER.CHOICES$short, c("No cancer selected", "Other")))] <- 0
    tmp.ped[which(tmp.ped$ID == id), paste0("Age", setdiff(CANCER.CHOICES$short, c("No cancer selected", "Other")))] <- NA
    tmp.ped[which(tmp.ped$ID == id), "NPP.isAffX.AgeX"] <- NA
    
    # create two data frames: 1) PanelPRO cancers 2) non-PanelPRO ("Other") cancers
    pp.cans.df <- cancers.and.ages[which(!cancers.and.ages$Cancer %in% c("No cancer selected","Other")),]
    other.can.df <- cancers.and.ages[which(cancers.and.ages$Cancer == "Other"),]
    
    # iterate through PanelPRO cancers
    if(nrow(pp.cans.df) > 0){
      for(row in 1:nrow(pp.cans.df)){
        c.short <- CANCER.CHOICES$short[which(CANCER.CHOICES$long == pp.cans.df$Cancer[row])]
        tmp.ped[which(tmp.ped$ID == id), paste0("isAff", c.short)] <- 1
        tmp.ped[which(tmp.ped$ID == id), paste0("Age", c.short)] <- pp.cans.df$Age[row]
      }
    }
    
    # make a json string for the other cancers
    if(nrow(other.can.df)){
      other.can.df <-
        other.can.df %>%
        mutate(String = paste0(ifelse(Other == "", "UnkType", Other), ":'", Age,"'"))
      other.cans <- paste0("{", paste0(other.can.df$String, collapse = ", "), "}")
      tmp.ped$NPP.isAffX.AgeX[which(tmp.ped$ID == id)] <- other.cans
    }
    
    # reset cancer specific tumor marker values
    if(tmp.ped$isAffBC[which(tmp.ped$ID == id)] == 0){
      tmp.ped[which(tmp.ped$ID == id), PanelPRO:::MARKER_TESTING$BC$MARKERS] <- NA
    }
    if(tmp.ped$isAffCOL[which(tmp.ped$ID == id)] == 0){
      tmp.ped[which(tmp.ped$ID == id), PanelPRO:::MARKER_TESTING$COL$MARKERS] <- NA
    }
  }
  
  # tumor markers
  bc.mark.vec <- c(ER = er, PR = pr, HER2 = her2, CK5.6 = ck5.6, CK14 = ck14)
  crc.mark.vec <- c(MSI = msi)
  mark.vec <- c(bc.mark.vec, crc.mark.vec)
  if(any(!is.null(mark.vec))){
    
    # clear existing values
    tmp.ped[which(tmp.ped$ID == id), c(PanelPRO:::MARKER_TESTING$BC$MARKERS, PanelPRO:::MARKER_TESTING$COL$MARKERS)] <- NA
    
    # populate new values
    for(m in 1:length(mark.vec)){
      tmp.ped[which(tmp.ped$ID == id), names(mark.vec)[m]] <- ifelse(mark.vec[m] == "Positive", 1,
                                                                     ifelse(mark.vec[m] == "Negative", 0, NA))
    }
  }
  
  # gene results
  if(!is.null(gene.results) & !is.null(panel.name)){
    
    # clear existing values
    tmp.ped[which(tmp.ped$ID == id), PanelPRO:::GENE_TYPES] <- NA
    tmp.ped[which(tmp.ped$ID == id), "panel.name"] <- "none"
    tmp.ped[which(tmp.ped$ID == id), "PP.gene.info"] <- NA
    tmp.ped[which(tmp.ped$ID == id), "NPP.gene.info"] <- NA
    
    # populate panel name
    tmp.ped$panel.name[which(tmp.ped$ID == id)] <- panel.name
    
    # remove slash from result codings and add json string column by result type
    gene.results <-
      gene.results %>%
      mutate(Result = ifelse(Result == "P/LP", "PLP",
                             ifelse(Result == "B/LP", "BLP", Result))) %>%
      mutate(Var.Prot.Zyg = paste0(Result, ":{",
                                   "var:'", Variants, "',",
                                   "prot:'", Proteins, "',",
                                   "zyg:'", Zygosity, "'}"))
      
    # combine duplicate entries of the gene with different result types
    # only retain most severe result type for each gene
    if(any(table(gene.results$Gene) > 1)){
      dups <- names(table(gene.results$Gene)[which(table(gene.results$Gene) > 1)])
      for(d in dups){
        tmp.df <- gene.results %>% filter(Gene == d)
        tmp.vpz <- paste0(tmp.df$Var.Prot.Zyg, collapse = ",")
        gene.results$Var.Prot.Zyg[which(gene.results$Gene == d)] <- tmp.vpz
        result.types <- unique(tmp.df$Result)
        if(any(result.types == "PLP")){
          gene.results <- gene.results[which(!(gene.results$Gene == d & gene.results$Result != "PLP")),]
        } else if(any(result.types == "VUS")) {
          gene.results <- gene.results[which(!which(gene.results$Gene == d & gene.results$Result != "VUS")),]
        } else if(any(result.types == "BLB")) {
          gene.results <- gene.results[which(!which(gene.results$Gene == d & gene.results$Result != "BLB")),]
        }
      }
    }
    
    # add json string column by gene
    gene.results <-
      gene.results %>%
      mutate(Gene.JSON = ifelse(Result == "Neg", paste0(Gene, ":'Neg'"), paste0(Gene, ":{", Var.Prot.Zyg, "}")))
    
    # separate PanelPRO genes from non-PanelPRO genes
    pp.genes.df <- gene.results[which(gene.results$Gene %in% PanelPRO:::GENE_TYPES),]
    npp.genes.df <- gene.results[which(gene.results$Gene %in% non.pp.genes),]
      
    # PanelPRO genes
    if(nrow(pp.genes.df) > 0){
      
      # iterate through PanelPRO genes to assign results status (1 or 0 only)
      # in PanelPRO gene columns
      for(row in 1:nrow(pp.genes.df)){
        for.PP <- TRUE
        if(pp.genes.df$Result[row] == "PLP"){
          
          ### check for P/LP genes that have variants, proteins, or zygosity not handled by PanelPRO
          ### assume missing values are compatible with the PanelPRO
          ## Genes where the variant type matters
          # CHEK2
          if(pp.genes.df$Gene[row] == "CHEK2"){
            tmp.vars <- tolower(strsplit(pp.genes.df$Variants[row], split = ", ")[[1]])
            if(all(!tmp.vars %in% c("",tolower(varCHEK2plp)))){ for.PP <- FALSE }
            
            # NBN
          } else if(pp.genes.df$Gene[row] == "NBN"){
            tmp.vars <- tolower(strsplit(pp.genes.df$Variants[row], split = ", ")[[1]])
            if(all(!tmp.vars %in% c("",tolower(varNBNplp)))){ for.PP <- FALSE }
              
            ## genes where the protein matters
            #CDKN2A
          } else if(pp.genes.df$Gene[row] == "CDKN2A"){
            tmp.vars <- tolower(strsplit(pp.genes.df$Proteins[row], split = ", ")[[1]])
            if(all(!tmp.vars %in% c("",tolower(varNBNplp)))){ for.PP <- FALSE }
            
            ## genes where the zygosity matters
            # MUTYH
          } else if(pp.genes.df$Gene[row] == "MUTYH"){
            if(!pp.genes.df$Zygosity[row] %in% c("Unk",zygMUTYHplp)){ for.PP <- FALSE }
          }
          
          # mark PanelPRO gene columns in pedigree with results
          if(for.PP){
            tmp.ped[which(tmp.ped$ID == id), pp.genes.df$Gene[row]] <- 1
          } else {
            tmp.ped[which(tmp.ped$ID == id), pp.genes.df$Gene[row]] <- 0
          }
          
          # for PanelPRO, mark all VUS, B/LB, and negative as negative
        } else {
          tmp.ped[which(tmp.ped$ID == id), pp.genes.df$Gene[row]] <- 0
        }
      }
      
      # record all PanelPRO gene information in JSON string by gene and result type 
      # (VUS, B/LB, and Neg are differentiated in this data structure)
      pp.gene.json <- paste0("{",paste0(pp.genes.df$Gene.JSON, collapse = ","),"}")
      tmp.ped$PP.gene.info[which(tmp.ped$ID == id)] <- pp.gene.json
    }
    
    # record all non-PanelPRO gene information in JSON string by gene and result type 
    if(nrow(npp.genes.df)){
      npp.gene.json <- paste0("{",paste0(npp.genes.df$Gene.JSON, collapse = ","),"}")
      tmp.ped$NPP.gene.info[which(tmp.ped$ID == id)] <- npp.gene.json
    }
  }
  
  return(tmp.ped)
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
  
  # assume proband is referenced if `NULL`
  if(is.null(assume.from)){
    assume.from <- tmp.ped$ID[which(tmp.ped$isProband == 1)]
  }
  
  # reference proband's race and ancestry info for later
  NPP.race <- tmp.ped$NPP.race[which(tmp.ped$ID == assume.from)]
  NPP.eth <- tmp.ped$NPP.eth[which(tmp.ped$ID == assume.from)]
  NPP.AJ <- tmp.ped$NPP.AJ[which(tmp.ped$ID == assume.from)]
  NPP.It <- tmp.ped$NPP.It[which(tmp.ped$ID == assume.from)]
  
  # update the pedigree and return it
  popPersonData(tmp.ped = tmp.ped, id = id, 
                rc = NPP.race, et = NPP.eth, 
                an.aj = NPP.AJ, an.it = NPP.It)
}


#' Initialize New Pedigree
#' 
#' @param pedigree.id string, pedigree ID
#' @param pb.sex binary, proband's sex, `0` for female and `1` for male
#' @returns an initial 3 person pedigree with proband and proband's parents containing 
#' the PedigreeID, proband's CurAge, Sex, race, and Ancestry and the proband's parents with 
#' Sex, race, and Ancestry.
initPed <- function(pedigree.id, pb.sex){
  t.ped <- formatNewPerson(relation = "proband", ped.id = pedigree.id, sx = pb.sex)
  t.ped <- formatNewPerson(relation = "mother", tmp.ped = t.ped)
  t.ped <- formatNewPerson(relation = "father", tmp.ped = t.ped)
  return(t.ped)
} 


#' Add Children, Siblings (FDRs), Aunts/Uncles, and Grandparents to Pedigree
#' 
#' @param t.ped a pedigree data frame.
#' @param num.dau number of proband's daughters. Default is `0`.
#' @param num.son number of proband's sons. Default is `0`.
#' @param num.sis number of proband's sisters. Default is `0`.
#' @param num.bro number of proband's brothers. Default is `0`.
#' @param num.m.aun number of proband's maternal aunts. Default is `0`.
#' @param num.m.unc number of proband's maternal uncles. Default is `0`.
#' @param num.p.aun number of proband's paternal aunts. Default is `0`.
#' @param num.p.unc number of proband's paternal uncles. Default is `0`.
#' @returns a modified pedigree with children, siblings, aunts, uncles, 
#' and grandparents added.
#' @details 
#' - This function enlarges the pedigree with link and sex information 
#' for the new people only. All other data for the new people will have default 
#' values. See `formatNewPerson`.
#' - One partner of the proband is created if the proband has any children and 
#' this partner is assumed to be the father or mother of all of the proband's 
#' children.
#' - One set of maternal grandparents is created only if one or more maternal 
#' aunts or uncles are specified. The same applies for the paternal side.
#' - This function does not consider half relationships, ie multiple child producing 
#' partners for the proband, proband's parents, or proband's grand parents.
#' - This function does not add nieces, nephews, cousins, or greats.
addFDRPlus <- function(t.ped, num.dau = 0, num.son = 0, num.sis = 0, num.bro = 0, 
                       num.m.aun = 0, num.m.unc = 0, num.p.aun = 0, num.p.unc = 0){
  
  # add proband partner, daughters and sons
  if(num.dau > 0 | num.son > 0){
    t.ped <- formatNewPerson(relation = "partner", tmp.ped = t.ped)
    part.id <- t.ped$ID[nrow(t.ped)]
    if(num.dau > 0){
      for(n in 1:num.dau){
        if(t.ped$Sex[which(t.ped$isProband == 1)] == 0){
          t.ped <- formatNewPerson(relation = "daughter", tmp.ped = t.ped, f.id = part.id)
        } else if(t.ped$Sex[which(t.ped$isProband == 1)] == 1){
          t.ped <- formatNewPerson(relation = "daughter", tmp.ped = t.ped, m.id = part.id)
        }
      }
    }
    if(num.son > 0){
      for(n in 1:num.son){
        if(t.ped$Sex[which(t.ped$isProband == 1)] == 0){
          t.ped <- formatNewPerson(relation = "son", tmp.ped = t.ped, f.id = part.id)
        } else if(t.ped$Sex[which(t.ped$isProband == 1)] == 1){
          t.ped <- formatNewPerson(relation = "son", tmp.ped = t.ped, m.id = part.id)
        }
      }
    }
  }
  
  # add sisters and brothers
  if(num.sis > 0){
    for(n in 1:num.sis){
      t.ped <- formatNewPerson(relation = "sister", tmp.ped = t.ped)
    }
  }
  if(num.bro > 0){
    for(n in 1:num.bro){
      t.ped <- formatNewPerson(relation = "brother", tmp.ped = t.ped)
    }
  }
  
  # add maternal grandparents, aunts, and uncles
  if(num.m.aun > 0 | num.m.unc > 0){
    t.ped <- formatNewPerson(relation = "grandmother", tmp.ped = t.ped, m.or.p.side = "m")
    t.ped <- formatNewPerson(relation = "grandfather", tmp.ped = t.ped, m.or.p.side = "m")
    if(num.m.aun > 0){
      for(n in 1:num.m.aun){
        t.ped <- formatNewPerson(relation = "aunt", tmp.ped = t.ped, m.or.p.side = "m")
      }
    }
    if(num.m.unc > 0){
      for(n in 1:num.m.unc){
        t.ped <- formatNewPerson(relation = "uncle", tmp.ped = t.ped, m.or.p.side = "m")
      }
    }
  }
  
  # add paternal grandparents, aunts, and uncles
  if(num.p.aun > 0 | num.p.unc > 0){
    t.ped <- formatNewPerson(relation = "grandmother", tmp.ped = t.ped, m.or.p.side = "p")
    t.ped <- formatNewPerson(relation = "grandfather", tmp.ped = t.ped, m.or.p.side = "p")
    if(num.p.aun > 0){
      for(n in 1:num.p.aun){
        t.ped <- formatNewPerson(relation = "aunt", tmp.ped = t.ped, m.or.p.side = "p")
      }
    }
    if(num.p.unc > 0){
      for(n in 1:num.p.unc){
        t.ped <- formatNewPerson(relation = "uncle", tmp.ped = t.ped, m.or.p.side = "p")
      }
    }
  }
  
  return(t.ped)
}


# change pedigree ID number
changePedID <- function(tmp.ped, new.ped.id){
  tmp.ped$PedigreeID <- new.ped.id
  return(tmp.ped)
}


#' Modify ID and Link Information in Pedigree
#' 
#' THIS FUNCTION IS INCOMPLETE - DO NOT USE
#' 
#' @param tmp.ped a pedigree data frame
#' @param id number, person's ID number in the pedigree. Must be provided unless 
#' `is.proband` is `TRUE`.
#' @param is.proband logical indicating the person is the new proband (`isProband`) 
#' if `TRUE`. Default is `FALSE`.
#' @param new.id number, the new ID to replace `id`.
#' @param side.of.fam a one character string of either `c("m","p")` for maternal 
#' side or paternal side.
#' @param rel.to.proband a string desribing how the person relates to the program. 
#' See the `relation` argument documentation for the `formatNewPerson` function 
#' documentation for a list of valid values.
#' @param sex binary indicating the person's updated `Sex` value, `0` if female 
#' and `1` if male.
#' @param m.id number indicated the person's updated `MotherID`.
#' @param f.id number indicating the person's updated `FatherID`.
#' @returns a pedigree with the updated information provided
#' @details This function should be applied carefully so that links in the pedigree 
#' are not broken.
modLinkInfo <- function(tmp.ped, id, is.proband = FALSE, new.id = NULL, 
                        side.of.fam = NULL, rel.to.proband = NULL, sex = NULL, 
                        m.id = NULL, f.id = NULL){
  return(tmp.ped)
}

# validate age values are between min.age and m
validateAge <- function(in.age, cur.age){
  if(!is.na(in.age) & !is.na(cur.age)){
    isNum <- is.numeric(in.age)
    need(isNum, paste0("Ages must be integers from ",min.age," to ",max.age,"."))
    if(isNum){
      inRange <- (in.age >= min.age & in.age <= max.age)
      isInt <- in.age %% 1 == 0
      noConflict <- in.age <= cur.age
      if(!noConflict){
        need(noConflict, paste0("Ages must be at or below the person's current age of ", cur.age))
      } else {
        need(all(isInt, inRange), paste0("Ages must be integers from ", min.age," to ",max.age,"."))
      }
    }
  }
}

#' Removes shiny inputs from memory
#' 
#' @param id string, input name to remove from memory
#' @param .input ???
#' 
#' @details from https://www.r-bloggers.com/2020/02/shiny-add-removing-modules-dynamically/
remove_shiny_inputs <- function(id, .input) {
  invisible(
    lapply(grep(id, names(.input), value = TRUE), function(i) {
      .subset2(.input, "impl")$.values$remove(i)
    })
  )
}