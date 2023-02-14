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
    mutate(across(.cols = c(name, race, Ancestry, 
                            NPPrace, NPPeth, panelNames), 
                  ~is.character(.))) %>%
    mutate(PedigreeID = ped.id) %>%
    mutate(ID = tmp.id) %>%
    mutate(relationship = ifelse(relation == "rel.partner", paste0("partner.of.",partner.of), relation)) %>%
    mutate(side = ifelse(grepl(pattern = "partner.of", relationship) | relationship %in% c("niece","nephew","cousin"), tmp.side, 
                         ifelse(!is.null(m.or.p.side), m.or.p.side, side))) %>%
    mutate(race = "All_Races") %>%
    mutate(Ancestry = "nonAJ") %>%
    mutate(NPPrace = "All_Races") %>%
    mutate(NPPeth = "Other_Ethnicity") %>%
    mutate(panelNames = "none") %>%
    mutate(across(.cols = c(isProband, isDead, Twins, NPPAJ, NPPIt,
                            starts_with("riskmod"), starts_with("isAff")), 
                  ~ 0)) %>%
    mutate(across(.cols = where(is.logical), ~as.numeric(NA))) 
  
  # name column (function not compatible with dplyr)
  if(is.na(tmp.person$side)){
    tmp.person$name <- stringi::stri_trans_totitle(tmp.person$relationship)
  } else {
    tmp.person$name <- stringi::stri_trans_totitle(paste0(ifelse(tmp.person$side == "m", "Mat.", 
                                                                 ifelse(tmp.person$side == "p", "Pat.", "?")), 
                                                          " ", tmp.person$relationship))
  }
  
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

#' Populate Data or Modify Data for a Person in a Pedigree
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
#' @param sx a string, one of "Male" or "Female".
#' @param cancers.and.ages a data frame with three columns: Cancer, Age, and Other.
#' @param cbc.info a named list of CBC risk related values with names `cbcrisk.cols`.
#' @param er string, ER tumor marker testing status, one of `c("Not Tested", "Positive", "Negative)`
#' @param pr string, PR tumor marker testing status, one of `c("Not Tested", "Positive", "Negative)`
#' @param her2 string, HER2 tumor marker testing status, one of `c("Not Tested", "Positive", "Negative)`
#' @param ck5.6 string, CK5.6 tumor marker testing status, one of `c("Not Tested", "Positive", "Negative)`
#' @param ck14 string, CK14 tumor marker testing status, one of `c("Not Tested", "Positive", "Negative)`
#' @param msi string, MSI tumor marker testing status, one of `c("Not Tested", "Positive", "Negative)`
#' @param gene.results data frame that contains a summary of all panel test results 
#' with six columns: Gene, Result, Nucleotide, Protein, Zygosity, and Panel.
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
                          sx = NULL,
                          cancers.and.ages = NULL,
                          cbc.info = NULL,
                          er = NULL,
                          pr = NULL,
                          her2 = NULL,
                          ck5.6 = NULL,
                          ck14 = NULL,
                          msi = NULL,
                          gene.results = NULL){
  
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
    tmp.ped$NPPrace[which(tmp.ped$ID == id)] <- rc
    tmp.ped$NPPeth[which(tmp.ped$ID == id)] <- et
  }
  if(!is.null(an.aj) & !is.null(an.it)){ 
    tmp.ped$Ancestry[which(tmp.ped$ID == id)] <- getPPAncestry(an.aj, an.it) 
    tmp.ped$NPPAJ[which(tmp.ped$ID == id)] <- an.aj
    tmp.ped$NPPIt[which(tmp.ped$ID == id)] <- an.it
  }
  
  # cancer hx
  if(!is.null(cancers.and.ages) & !is.null(sx)){
    
    # clear existing values
    mod.can.choices.short <- setdiff(c(CANCER.CHOICES$short, "CBC"), c("No cancer selected", "Other"))
    tmp.ped[which(tmp.ped$ID == id), paste0("isAff", mod.can.choices.short)] <- 0
    tmp.ped[which(tmp.ped$ID == id), paste0("Age", mod.can.choices.short)] <- NA
    tmp.ped[which(tmp.ped$ID == id), "cancersJSON"] <- NA
    
    # remove any cancers that do not match the sex
    if(sx == "Male"){
      cancers.and.ages <- filter(cancers.and.ages, !Cancer %in% FEMALE.CANCERS)
    } else if(sx == "Female"){
      cancers.and.ages <- filter(cancers.and.ages, !Cancer %in% MALE.CANCERS)
    }
    
    # iterate through PanelPRO cancers and populate the PanelPRO cancer columns
    pp.cans.df <- cancers.and.ages[which(!cancers.and.ages$Cancer %in% c("No cancer selected","Other")),]
    if(nrow(pp.cans.df) > 0){
      for(row in 1:nrow(pp.cans.df)){
        c.short <- c(CANCER.CHOICES$short, "CBC")[which(c(CANCER.CHOICES$long, "Contralateral") == pp.cans.df$Cancer[row])]
        tmp.ped[which(tmp.ped$ID == id), paste0("isAff", c.short)] <- 1
        tmp.ped[which(tmp.ped$ID == id), paste0("Age", c.short)] <- pp.cans.df$Age[row]
      }
    }
    
    # make a json string for all cancers
    all.can.df <- cancers.and.ages[which(cancers.and.ages$Cancer != "No cancer selected"),]
    if(nrow(all.can.df) > 0){
      all.can.df <-
        all.can.df %>%
        mutate(Other = ifelse(Cancer == "Other" & Other == "Unknown/Not Listed", "UnkType", Other)) %>%
        mutate(String = paste0("{'cancer':'", Cancer, "','age':'", Age,"','other':'", Other, "'}"))
      
      # ensure CBC is after BC
      if(any(all.can.df$Cancer == "Breast") & any(all.can.df$Cancer == "Contralateral")){
        cbc.row <- which(all.can.df$Cancer == "Contralateral")
        other.rows <- seq(1, nrow(all.can.df))[which(seq(1, nrow(all.can.df)) != cbc.row)]
        all.can.df <- all.can.df[c(other.rows, cbc.row),]
      }
      all.cans <- paste0("[", paste0(all.can.df$String, collapse = ","), "]")
      tmp.ped$cancersJSON[which(tmp.ped$ID == id)] <- all.cans
    }
    
    # reset cancer specific tumor marker values if the relevant cancers are not in the cancer hx
    if(tmp.ped$isAffBC[which(tmp.ped$ID == id)] == 0){
      tmp.ped[which(tmp.ped$ID == id), PanelPRO:::MARKER_TESTING$BC$MARKERS] <- NA
    }
    if(tmp.ped$isAffCOL[which(tmp.ped$ID == id)] == 0){
      tmp.ped[which(tmp.ped$ID == id), PanelPRO:::MARKER_TESTING$COL$MARKERS] <- NA
    }
    
    # reset additional CBC risk columns if the this condition is not met: has BC but does not have CBC
    if(!(tmp.ped$isAffBC[which(tmp.ped$ID == id)] == 1 & 
         tmp.ped$isAffCBC[which(tmp.ped$ID == id)] == 0)){
      for(cbc.var in c("FirstBCType", "AntiEstrogen", "HRPreneoplasia", 
                       "BreastDensity", "FirstBCTumorSize")){
        tmp.ped[which(tmp.ped$ID == id), cbc.var] <- NA
      }
    }
  } # end of if statement for cancer hx
  
  # CBC information
  if(!is.null(cbc.info)){
    tmp.ped$FirstBCType[which(tmp.ped$ID == id)] <- 
      ifelse(cbc.info$FirstBCType == "NA", NA, cbc.info$FirstBCType)
    tmp.ped$AntiEstrogen[which(tmp.ped$ID == id)] <- 
      ifelse(cbc.info$AntiEstrogen == "NA", NA, cbc.info$AntiEstrogen)
    tmp.ped$HRPreneoplasia[which(tmp.ped$ID == id)] <- 
      ifelse(cbc.info$HRPreneoplasia == "NA", NA, cbc.info$HRPreneoplasia)
    tmp.ped$BreastDensity[which(tmp.ped$ID == id)] <- 
      ifelse(cbc.info$BreastDensity == "NA", NA, cbc.info$BreastDensity)
    tmp.ped$FirstBCTumorSize[which(tmp.ped$ID == id)] <- 
      ifelse(cbc.info$FirstBCTumorSize == "NA", NA, cbc.info$FirstBCTumorSize)
  } 
  
  # tumor markers
  bc.mark.vec <- c(ER = er, PR = pr, HER2 = her2, CK5.6 = ck5.6, CK14 = ck14)
  crc.mark.vec <- c(MSI = msi)
  mark.vec <- c(bc.mark.vec, crc.mark.vec)
  if(any(!is.null(mark.vec))){
    
    # clear existing values
    tmp.ped[which(tmp.ped$ID == id), c(PanelPRO:::MARKER_TESTING$BC$MARKERS, 
                                       PanelPRO:::MARKER_TESTING$COL$MARKERS)] <- NA
    
    # populate new values
    for(m in 1:length(mark.vec)){
      tmp.ped[which(tmp.ped$ID == id), names(mark.vec)[m]] <- 
        ifelse(mark.vec[m] == "Positive", 1,
               ifelse(mark.vec[m] == "Negative", 0, NA))
    }
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
  
  # gene results
  if(!is.null(gene.results)){
    
    # clear existing values
    tmp.ped[which(tmp.ped$ID == id), PanelPRO:::GENE_TYPES] <- NA
    tmp.ped[which(tmp.ped$ID == id), "panelNames"] <- "none"
    tmp.ped[which(tmp.ped$ID == id), "genesJSON"] <- NA
    
    # populate pedigree data
    if(gene.results$Gene[1] != "" & nrow(gene.results) > 0){
      
      # update panel name
      tmp.ped$panelNames[which(tmp.ped$ID == id)] <- paste0(unique(gene.results$Panel), collapse = ", ")
      
      # remove slash from result codings and add json string column by result type
      gene.results <-
        gene.results %>%
        mutate(Result = ifelse(Result == "P/LP", "PLP",
                               ifelse(Result == "B/LB", "BLB", Result)))
      
      ## populate json strings gene data
      # iterate through the panels first
      all.jsons <- as.character()
      rtypes <- c("PLP","VUS","BLB","Neg")
      pnames <- unique(gene.results$Panel)
      for(pname in pnames){
        pan.df <- filter(gene.results, Panel == pname)
        
        # iterate through unique genes
        u.genes <- unique(pan.df$Gene)
        for(gcnt in 1:length(u.genes)){
          
          # subset the data frame for all rows matching the gene
          gene.df <- filter(pan.df, Gene == u.genes[gcnt])
          rtypes.gene <- rtypes[which(rtypes %in% unique(gene.df$Result))]
          for(rtype in rtypes.gene){
            
            # subset the data frame again for all rows matching the gene AND the result type
            rtype.df <- filter(gene.df, Result == rtype)
            
            # iterate through the rows to get the JSON sub-string section specific to 1 variant
            for(vcnt in 1:nrow(rtype.df)){
              v.json <- paste0("{",
                                  "'panel':'", pname, "',",
                                  "'gene':'", u.genes[gcnt], "',",
                                  "'result':'", rtype, "',",
                                  "'nuc':'" , rtype.df$Nucleotide[vcnt], "',",
                                  "'prot':'", rtype.df$Protein[vcnt]  , "',",
                                  "'zyg':'" , rtype.df$Zygosity[vcnt]  , "'",
                               "}")
              
              all.jsons <- c(all.jsons, v.json)
            } # end of for loop for variants
          } # end of for loop for PLP, VUS, or BLB result type
        } # end of for loop for genes
      } # end of for loop for panels
        
      # enter results in the pedigree
      tmp.ped$genesJSON[which(tmp.ped$ID == id)] <- paste0("[", paste0(all.jsons, collapse = ","), "]")
      
      ## populate PanelPRO gene columns, PLP is 1, all other results are 0
      pp.genes.df <- gene.results[which(gene.results$Gene %in% PanelPRO:::GENE_TYPES),]
      if(nrow(pp.genes.df) > 0){

        # only consider PLP genes
        plp.pp.genes.df <- filter(pp.genes.df, Result == "PLP")
        
        # initialize list of unique P/LP PanelPRO genes
        pp.mark.pos <- as.character()
        
        # iterate through the unique genes P/LP genes in the data frame
        u.plp <- unique(plp.pp.genes.df$Gene)
        for(ug in u.plp){
          
          # start by assuming the gene should not be marked as positive
          add.ug <- FALSE
          
          ### check for P/LP genes that have nucleotides, proteins, or zygosity 
          ### not handled by PanelPRO, assume missing values are compatible with the PanelPRO
          special.plp.cases <- c("CHEK2", "NBN", "CDKN2A", "MUTYH")
          if(ug %in% special.plp.cases){
            sc.df <- filter(plp.pp.genes.df, Gene == ug)
            if(ug == "CHEK2" & (any(sc.df$Nucleotide == nucCHEK2plp) | any(sc.df$Nucleotide == ""))){
              add.ug <- TRUE
            } else if(ug == "NBN" & (any(sc.df$Nucleotide == nucNBNplp) | any(sc.df$Nucleotide == ""))){
              add.ug <- TRUE
            } else if(ug == "CDKN2A" & (any(sc.df$Protein == protCDKN2Aplp) | any(sc.df$Protein == ""))){
              add.ug <- TRUE
            } else if(ug == "MUTYH" & (any(sc.df$Zygosity == zygMUTYHplp) | any(sc.df$Zygosity == "Unk"))){
              add.ug <- TRUE
            }
            
            # not a special case, so mark it as positive
          } else {
            add.ug <- TRUE
          }
          
          # add qualified gene to the list of positive genes
          if(add.ug){ pp.mark.pos <- c(pp.mark.pos, ug) }
        }
        
        # mark genes as positive in the pedigree
        if(length(pp.mark.pos) > 0){
          tmp.ped[which(tmp.ped$ID == id), pp.mark.pos] <- 1
        }
        
        # mark the remaining genes in the panel as negative
        pp.mark.neg <- setdiff(intersect(gene.results$Gene, PanelPRO:::GENE_TYPES) , pp.mark.pos)
        if(length(pp.mark.neg) > 0){
          tmp.ped[which(tmp.ped$ID == id), pp.mark.neg] <- 0
        }
      } # end of if statement for if there were PanelPRO genes in the panel
    } # end of if statement for if a panel was selected
  } # end of section for adding gene information to the pedigree
  return(tmp.ped)
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

#' Update pedigree with a relative's data based on the currently selected pedTabs tab
#' 
#' @param tped a pedigree data frame. 
#' @param rel the relative for which to save the data.
#' @param inp the shiny input object.
#' @param cr canReactive$canNums
#' @param sr surgReactive$lst
#' @param gr geneReactive$GeneNums
#' @param dupResultGene `dupResultGene()`
#' @return updated pedigree
saveRelDatCurTab <- function(tped, rel, inp, cr, sr, gr, dupResultGene, sx){
  
  # demographics
  if(inp$pedTabs == "Demographics"){
    return(popPersonData(tmp.ped = tped, id = rel, cur.age = inp$Age, 
                      rc = inp$race, et = inp$eth, 
                      an.aj = inp$ancAJ, an.it = inp$ancIt)
    )
    
    # cancer hx
  } else if(inp$pedTabs == "Cancer Hx"){
    can.df <- makeCancerDF(rel = rel, cr = cr, inp = inp)
    return(popPersonData(tmp.ped = tped, id = rel, sx = sx, cancers.and.ages = can.df))
    
    # cbc
  } else if(inp$pedTabs == "CBC Risk"){
    return(
      popPersonData(tmp.ped = tped, id = rel, 
                    cbc.info = list(FirstBCType = inp$FirstBCType,
                                    AntiEstrogen = inp$AntiEstrogen,
                                    HRPreneoplasia = inp$HRPreneoplasia,
                                    BreastDensity = inp$BreastDensity,
                                    FirstBCTumorSize = inp$FirstBCTumorSize)
      )
    )
    
    # tumor markers
  } else if(inp$pedTabs == "Tumor Markers"){
    return(popPersonData(tmp.ped = tped, id = rel, 
                         er = inp$ER, pr = inp$PR, her2 = inp$HER2,
                         ck5.6 = inp$CK56, ck14 = inp$CK14, 
                         msi = inp$MSI))
    
    # surgical hx
  } else if(inp$pedTabs == "Surgical Hx"){
    return(popPersonData(tmp.ped = tped, id = rel, riskmods.and.ages = sr))
    
    # genes
  } else if(inp$pedTabs == "Genes"){
    gene.df <- makeGeneDF(rel = rel, gr = gr, 
                          dupResultGene = dupResultGene,
                          inp = inp)$df
    return(popPersonData(tmp.ped = tped, id = rel,
                         gene.results = gene.df))
    
    # if none of these tabs were selected in pedTabs
  } else {
    return(tped)
  }
}

#' Update all inputs for a relative
#' 
#' @param rel.info a one row data frame containing the pedigree information for the relative.
#' @param ss shiny session
#' @return nothing
updateRelInputs <- function(rel.info, ss){
  
  ## Demographics 
  # sex
  new.sex <- ifelse(rel.info$Sex == 0, "Female",
                    ifelse(rel.info$Sex == 1, "Male", NA))
  updateSelectInput(ss, "Sex", selected = new.sex, choices = sex.choices)
  
  # age
  updateNumericInput(ss, "Age", value = rel.info$CurAge)
  
  # Non-PanelPRO races, ethnicity, and ancestries
  updateSelectInput(ss, "race", selected = rel.info$NPPrace)
  updateSelectInput(ss, "eth", selected = rel.info$NPPeth)
  updateCheckboxInput(ss, "ancAJ", value = rel.info$NPPAJ)
  updateCheckboxInput(ss, "ancIt", value = rel.info$NPPIt)
  
  # cbc
  for(cbc.var in c("FirstBCType", "AntiEstrogen", "HRPreneoplasia", 
                   "BreastDensity", "FirstBCTumorSize")){
    updateSelectInput(ss, cbc.var, 
                      selected = ifelse(is.na(rel.info[[cbc.var]]), "NA", rel.info[[cbc.var]]))
  }
  
  ## Tumor Markers 
  marks <- c(PanelPRO:::MARKER_TESTING$BC$MARKERS, PanelPRO:::MARKER_TESTING$COL$MARKERS)
  for(m in marks){
    mval <- ifelse(is.na(rel.info[1,m]), "Not Tested",
                   ifelse(rel.info[1,m] == 1, "Positive",
                          ifelse(rel.info[1,m] == 0, "Negative", "Not Tested")))
    m <- ifelse(m == "CK5.6", "CK56", m)
    updateSelectInput(ss, m, selected = mval)
  }
  
  ## Surgical Hx
  for(sg in c("Mast", "Ooph", "Hyst")){
    updateCheckboxInput(ss, sg, value = rel.info[[paste0("riskmod", sg)]])
    updateNumericInput(ss, paste0(sg,"Age"), value = rel.info[[paste0("interAge", sg)]])
  }
  
  ## Genes
  # update the panel data for the new person
  if(rel.info$panelNames != "none"){
    updateSelectInput(ss, "existingPanels", 
                      choices = all.panel.names[which(!all.panel.names %in% 
                                                        strsplit(rel.info$panelNames, 
                                                                 split = ", ")[[1]])],
                      selected = "No panel selected")
  } else {
    updateSelectInput(ss, "existingPanels", 
                      choices = all.panel.names,
                      selected = "No panel selected")
  }
}

#' Validate quantities of relatives to be added
validateRelNums <- function(num.rels){
  if(is.na(num.rels)){
    notNA <- FALSE
    need(notNA, "Quantity cannot be blank or NA, it must be greater than or equal to 0.")
  } else {
    isNum <- is.numeric(num.rels)
    if(isNum){
      inRange <- num.rels >= 0
      isInt <- num.rels %% 1 == 0
      need(all(isInt, inRange), "Quantity must be an integer greater than or equal to 0.")
    } else {
      need(isNum, "Quantity must be numeric.")
    }
  }
}
