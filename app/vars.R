#### Set-up ####

# load non-PanelPRO cancers list - modified version of list found at 
# https://www.dana-farber.org/for-patients-and-families/care-and-treatment/cancer-types/
non.pp.cancers <- as.character(read.csv("./non.pp.cancer.list.csv")$cancer)

#### Pedigree ####
#pedigree column names
ped.cols <- c("PedigreeID", "ID", "side", "relationship", "Twins", "Sex", 
              "MotherID", "FatherID", "isProband", "CurAge", "isDead", 
              "race", "Ancestry", "NPP.race", "NPP.eth", "NPP.AJ", "NPP.It",
              paste0("riskmod", c("Mast","Hyst","Ooph")),
              paste0("interAge", c("Mast","Hyst","Ooph")),
              "ER", "PR", "CK14", "CK5.6", "HER2", "MSI",
              paste0("isAff", PanelPRO:::CANCER_NAME_MAP$short),
              paste0("Age", PanelPRO:::CANCER_NAME_MAP$short),
              "NPP.isAffX.AgeX",
              PanelPRO:::GENE_TYPES,"panel.names","PP.gene.info", "NPP.gene.info"
              )

#### Demographics ####
# age range, although PanelPRO can handle ages up to 94, we cannot store ages above 89 for privacy reasons
max.age <- 89
min.age <- 1

# sex choices
sex.choices <- c(" "=" ","Female"="Female","Male"="Male")

# race choices (different from PanelPRO's race choices)
rc.choices <- c("Other/Unreported/Mixed Race" = "All_Races",
                "American Indian/Alaskan Native" = "AIAN",
                "Asian/Pacific Islander" = "Asian",
                "Black" = "Black",
                "White" = "White")
# ethnicity choices
et.choices <- c("Unreported/Both" = "Other_Ethnicity",
                "Hispanic" = "Hispanic", 
                "Non-Hispanic" = "Non-Hispanic")

#### Cancers ####
# cancer choices from PanelPRO
CANCER.CHOICES <- PanelPRO:::CANCER_NAME_MAP
CANCER.CHOICES$short <- c("No cancer selected", setdiff(CANCER.CHOICES$short, "CBC"), "Other")
CANCER.CHOICES$long <- c("No cancer selected", setdiff(CANCER.CHOICES$long, "Contralateral"), "Other")

# see the non-PanelPRO cancers loaded as a csv at the top of this file
OTHER.CANCER.CHOICES <- c("Unknown/Not Listed", non.pp.cancers)

# template data frame for storing cancer history
cancer.inputs.store <- as.data.frame(matrix(, nrow = 0, ncol = 3))
colnames(cancer.inputs.store) <- c("Cancer","Age","Other")

# template list for storing cancer module numbers / number of cancers by subject
# list names are relative ID numbers in the pedigree
# each element of the list contains a named vector where the values are the index number
# of a cancer UI module and the names are a set of numbers 1, 2, 3, ... where the names 
# represent the counts of the cancer UI modules
# the highest named element of the vector represents the number of cancer input modules
# a relative currently has active.
# when the length of a named vector is one and the value is NA then there are 0 cancer UI modules

# list of lists for keeping track of cancer UI modules for each relative
# the top level names are the ID numbers of the relatives 
# each relative has a list with two elements:
# 1) dict: a named numeric vector where each element is the index number of an active (not previously deleted) 
# cancer UI module for the person and the names are an enumerated set of numbers 1,2,3,... 
# indicating the order of the active cancer UI modules. If there are three active UI modules then 
# dict will be length 3 with names 1, 2, and 3. If there is just one active UI module then dict will 
# be length 1 with element name of "1". If there are no active UI modules then a special case occurs 
# where the length is 1 but the value of the single element is NA.
# 2) mx: a number indicating the all time maximum number of cancer UI modules created for a relative.
# mx includes even deleted/removed UI modules where dict only tracks active UI modules
trackCans.init <- list("1" = list(dict = setNames(c(NA),1),
                                  mx = 0),
                       "2" = list(dict = setNames(c(NA),1),
                                  mx = 0),
                       "3" = list(dict = setNames(c(NA),1),
                                  mx = 0))


#### Tumor Markers ####
# result choices
marker.result.choices <- c("Not Tested", "Positive", "Negative")


#### Surgeries ####
RISKMOD.TYPES <- c("mast","hyst","ooph")

# template list of prophylactic surgery statuses and ages
riskmods.inputs.store <- list(riskmod = setNames(rep(0, length(RISKMOD.TYPES)), RISKMOD.TYPES),
                              interAge = setNames(rep(NA, length(RISKMOD.TYPES)), RISKMOD.TYPES))


#### Genes ####

## Gene module tracker template: next list with 6 levels
# The first level names are relative IDs in the pedigree; 
# every relative will have one even if they have no panels.
# The first level elements are nested lists of length three that contain the aggregate 
# panel information for a relative.
# The second level nested list names are 'dict', 'mx', and 'panels':
# - 'dict' is a named vector where the names of the vector numbers corresponding to 
#  the order of the active panelUIs for the relative (1,2,3,...).
#  The values of 'dict' are unique numbers that correspond to unique id numbers of the 
#  panelUI module that the panel corresponds to. 
#  A special case of 'dict' occurs when a relative has no active panels in which case 'dict' 
#  will be of length 1 where the names(dict)[1] is '1' and dict[1] is NA.
# - 'mx' is a number between 0 to infinity that contains the id number of the last
#  panelUI module created for this relative. 
#  It strictly increases by 1 every time a new panelUI is inserted.
# - 'panels' contains a third level nested list where each element contains panel-specific
#  information for 1 panel of the relative. The names of this third level of list follow 
#  the pattern panel1, panel2, panel3, ... For each panel there is a fourth level nested 
#  list with three named elements: 'name', 'genes', 'results':
#  - 'name' contains the panel name. A special case of 'dict' occurs when a 
#   relative has no active panels in which case the panel1 name element will take the 
#   value 'No panel selected'
#  - 'genes' contains a unique vector of gene names that make up the panel. A special 
#   case of 'genes' occurs when a relative has no active panels, in which case the panel1
#   genes element will be an empty vector.
#  - 'results' is a fifth level nested list of length three with names: 'PLP', 'VUS', and 'BLB'. 
#   The structure of each of the three elements is identical and each element is used to track 
#   the modularized geneUI inputs for the PLP, VUS, and BLB result types, respectively. Each 
#   contains a 6th level nested list with two elements named 'dict' and 'mx':
#   - 'dict' is a named vector where the names of the vector numbers corresponding to 
#    the order of the active geneUI for this panel and relative (1,2,3,...).
#    The values of 'dict' are unique numbers that correspond to unique id numbers of the 
#    geneUI module for this panel and relative. 
#    A special case of 'dict' occurs when a relative has no active panels in which 'dict' 
#    will be of length 1 where the names(dict)[1] is '1' and dict[1] is NA.
#   - 'mx' is a number between 0 to infinity that contains the id number of the last
#    geneUI module created for this relative and panel. 
#    It strictly increases by 1 every time a new geneUI is inserted. 
#    A special case of 'mx' occurs when the relative has no panel in which case the panel1
#    'mx' element will be 0.
new.dict <- setNames(c(NA),1)
new.mx <- 0

# 5th level contains an element for each result type (excluding negative)
# 6th level contains information for managing the geneUI modeles for each result type
geneResultsTemplate <- list(PLP = list(dict = new.dict,
                                       mx = new.mx),
                            VUS = list(dict = new.dict,
                                       mx = new.mx),
                            BLB = list(dict = new.dict,
                                       mx = new.mx))

# 2nd level contains the aggregated panel information for tracking panelUI modules
# 3rd level contains an entry for each panel
# 4th level contains information specific to one panel
relTemplate.trackGenes <- list(dict = new.dict,
                               mx = new.mx,
                               panels = list(panel1 = list(name = "No panel selected",
                                                           genes = as.character(),
                                                           results = geneResultsTemplate)))

# 1st level of relatives
trackGenes.init <- list("1" = relTemplate.trackGenes,
                        "2" = relTemplate.trackGenes,
                        "3" = relTemplate.trackGenes)

# master genes lists
all.genes <- c('AIP', 'ALK', 'APC', 'ATM', 'AXIN2', 'BAP1', 'BARD1', 'BLM', 'BMPR1A', 
               'BRCA1', 'BRCA2', 'BRIP1', 'CASR', 'CDC73', 'CDH1', 'CDK4', 'CDKN1B', 
               'CDKN1C', 'CDKN2A', 'CEBPA', 'CHEK2', 'CTNNA1', 'DICER1', 'DIS3L2', 
               'EGFR', 'EPCAM', 'FH', 'FLCN', 'GATA2', 'GPC3', 'GREM1', 'HOXB13', 
               'HRAS', 'KIT', 'MAX', 'MEN1', 'MET', 'MITF', 'MLH1', 'MSH2', 'MSH3', 
               'MSH6', 'MUTYH', 'NBN', 'NF1', 'NF2', 'NTHL1', 'PALB2', 'PDGFRA', 
               'PHOX2B', 'PMS2', 'POLD1', 'POLE', 'POT1', 'PRKAR1A', 'PTCH1', 'PTEN', 
               'RAD50', 'RAD51C', 'RAD51D', 'RB1', 'RECQL4', 'RET', 'RUNX1', 'SDHA', 
               'SDHAF2', 'SDHB', 'SDHC', 'SDHD', 'SMAD4', 'SMARCA4', 'SMARCB1', 
               'SMARCE1', 'STK11', 'SUFU', 'TERC', 'TERT', 'TMEM127', 'TP53', 'TSC1', 
               'TSC2', 'VHL', 'WRN', 'WT1')
hboc.genes <- c("ATM","BRCA1","BRCA2","CDH1","CHEK2","PALB2","PTEN")
lynch.genes <- c("MLH1","MSH2","MSH6","PMS2","EPCAM")
non.pp.genes <- setdiff(all.genes, PanelPRO:::GENE_TYPES)

# master panel list
all.panels <- list("HBOC" = sort(hboc.genes),
                   "Lynch" = sort(lynch.genes),
                   "GB Rossi Pancreatic Cancer Study" = sort(all.genes))
all.panel.names <- c("No panel selected", "Create new", sort(names(all.panels)))

# genes with specific nucleotides
nucCHEK2plp <- "1100delC"
nucNBNplp <- "657del5"

# genes with specific proteins
protCDKN2Aplp <- "p16"

# genes where zygosity matters
zygMUTYHplp <- "Hetero"

# zygosity choices
zyg.choices <- c("Unk","Homo","Hetero")



