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
              PanelPRO:::GENE_TYPES,"panel.name","PP.gene.info", "NPP.gene.info"
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

#### Surgeries ####
RISKMOD.TYPES <- c("mast","hyst","ooph")

# template list of prophylactic surgery statuses and ages
riskmods.inputs.store <- list(riskmod = setNames(rep(0, length(RISKMOD.TYPES)), RISKMOD.TYPES),
                              interAge = setNames(rep(NA, length(RISKMOD.TYPES)), RISKMOD.TYPES))

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


#### Tumor Markers ####
# result choices
marker.result.choices <- c("Not Tested", "Positive", "Negative")

#### Genes ####

# template data frame for storing gene results
gene.inputs.store <- data.frame(Gene     = rep(""   , 1000),
                                Variants = rep(""   , 1000),
                                Proteins = rep(""   , 1000),
                                Zygosity = rep("Unk", 1000))

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

# genes with specific variants
varCHEK2plp <- "1100delC"
varNBNplp <- "657del5"

# genes with specific proteins
protCDKN2Aplp <- "p16"

# genes where zygosity matters
zygMUTYHplp <- "Hetero"

# zygosity choices
zyg.choices <- c("Unk","Homo","Hetero")



