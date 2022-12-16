#### Set-up ####

# load non-PanelPRO cancers list - modified version of list found at 
# https://www.dana-farber.org/for-patients-and-families/care-and-treatment/cancer-types/
non.pp.cancers <- as.character(read.csv("./non.pp.cancer.list.csv")$cancer)

#### Pedigree ####
#pedigree column names
ped.cols <- c("PedigreeID", "ID", "side", "relationship", "Twins", "Sex", 
              "MotherID", "FatherID", "isProband", "CurAge", "isDead", "race", 
              "Ancestry", 
              paste0("riskmod", c("Mast","Hyst","Ooph")),
              paste0("interAge", c("Mast","Hyst","Ooph")),
              "ER", "PR", "CK14", "CK5.6", "HER2", "MSI",
              paste0("isAff", PanelPRO:::CANCER_NAME_MAP$short),
              paste0("Age", PanelPRO:::CANCER_NAME_MAP$short),
              PanelPRO:::GENE_TYPES, 
              "NPP.isAffX.AgeX", "NPP.gene.results")

#### Demographics ####
# age range, although PanelPRO can handle ages up to 94, we cannot store ages above 89 for privacy reasons
max.age <- 89
min.age <- 1

# race choices (different from PanelPRO's race choices)
rc.choices <- c("Other or Unreported" = "All_Races1",
                "Mixed Race" = "All_Races2",
                "American Indian/Alaskan Native" = "AIAN",
                "Asian/Pacific Islander" = "Asian",
                "Black" = "Black",
                "White" = "White")
# ethnicity choices
et.choices <- c("Other or Unreported" = "Other_Ethnicity1",
                "Mixed Ethnicity" = "Other_Ethnicity2",
                "Hispanic" = "Hispanic", 
                "Non-Hispanic" = "Non-Hispanic")

# ancestry choices
an.choices <- c("Other or Unreported" = "nonAJ1",
                "Not Ashkenazi Jewish or Italian" = "nonAJ2",
                "Ashkenazi Jewish" = "AJ", 
                "Italian" = "Italian")

#### Surgeries ####
RISKMOD.TYPES <- c("mast","hyst","ooph")

# template list of prophylactic surgery statuses and ages
init.riskmods.and.ages <- list(riskmod = setNames(rep(0, length(RISKMOD.TYPES)), RISKMOD.TYPES),
                               interAge = setNames(rep(NA, length(RISKMOD.TYPES)), RISKMOD.TYPES))

#### Tumor Markers ####
MARKER.TYPES <- c("No marker selected","ER","PR","CK14","CK5.6","HER2","MSI")

# result choices
marker.result.choices <- c("Not Tested" = "Not Tested", "Positive" = "Positive", "Negative" = "Negative")

# template data frame of tumor markers, temporary storage
tmark.inputs.store <- data.frame(Mark = rep("No marker selected", length(MARKER.TYPES)-1),
                                 Result = rep("Not Tested", length(MARKER.TYPES)-1))

# template vector of tumor markers, persistent storage
init.t.markers <- setNames(object = rep(NA, length(MARKER.TYPES)-1), nm = setdiff(MARKER.TYPES, "No marker selected"))

#### Cancers ####
# cancer choices from PanelPRO
CANCER.CHOICES <- PanelPRO:::CANCER_NAME_MAP
CANCER.CHOICES$short <- c("No cancer selected", CANCER.CHOICES$short, "Other")
CANCER.CHOICES$long <- c("No cancer selected", CANCER.CHOICES$long, "Other")

# see the non-PanelPRO cancers loaded as a csv at the top of this file

# template data frame of cancers, temporary storage
cancer.inputs.store <- data.frame(Cancer = rep("No cancer selected", 100),
                                  Age = rep(NA, 100),
                                  Other = rep("", 100))

# template list of cancer affection statuses and diagnosis ages, permanent storage
init.cancers.and.ages <- list(isAff = setNames(rep(0, length(PanelPRO:::CANCER_NAME_MAP$short)), PanelPRO:::CANCER_NAME_MAP$short),
                              Age = setNames(rep(NA, length(PanelPRO:::CANCER_NAME_MAP$short)), PanelPRO:::CANCER_NAME_MAP$short))

#### Genes ####
# template vector of gene results, permanent storage (good for populating the pedigree)
# only contains PanelPRO genes
init.gene.results <- list(Result      = setNames(rep(NA, length(PanelPRO:::GENE_TYPES)), PanelPRO:::GENE_TYPES),
                          Variants    = setNames(rep(NA, length(PanelPRO:::GENE_TYPES)), PanelPRO:::GENE_TYPES),
                          Proteins    = setNames(rep(NA, length(PanelPRO:::GENE_TYPES)), PanelPRO:::GENE_TYPES),
                          Zygosity = setNames(rep(NA, length(PanelPRO:::GENE_TYPES)), PanelPRO:::GENE_TYPES))


# template data frame of gene results, temporary storage (hold both PanelPRO and other genes)
gene.inputs.store <- data.frame(Gene        = rep("", 1000),
                                Variants     = rep("", 1000),
                                Proteins     = rep("", 1000),
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
pp.genes <- PanelPRO:::GENE_TYPES
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

# gene where zygousity matters
zygMUTYH <- c("homozygous","heterozygous")

# zygous choices
zyg.choices <- c("Unk","Homo","Hetero")



