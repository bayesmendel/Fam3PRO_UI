# README

This directory contains 8 files that contain PanelPRO results for a single pedigree, along with the input data. The ending of each file name is the PedigreeID number of the pedigree analyzed. Files that contains tables will be in either .csv or .rds format depening on what was chosen at the time of download.

## Input data: 

1. pedigree-[PedigreeID].[csv or rds]: The pedigree table analyzed, coded and named for compatibility with PanelPRO and PPI.

2. cancer-details-[PedigreeID].[csv or rds]: a table indexed by PedigreeID, ID, and cancer type that contains cancer diagnoses and ages of diagnoses. This information was extracted from the cancersJSON column in the pedigree file.

3. panel-details-[PedigreeID].[csv or rds]: a table indexed by PedigreeID, ID, gene, result type, nucleotide, protein, zygosity and panel name. This information was extracted from the genesJSON column in the pedigree file.

4. run-settings-[PedigreeID].[csv or rds]: a table of the PanelPRO function argument settings (see the PanelPRO R function documentation in the data-dictionary directory).
 
## Result data:

5. posterior-probs-[PedigreeID].[csv or rds]: a table of carrier probabilities for the proband indexed by gene or combination of genes.

6. cancer-risk-[PedigreeID].[csv or rds]: a table of future cancer risk estimates for the proband indexed by cancer type and future age.

7. posterior-probs-[PedigreeID].png: a plot of the data in carrier probabilities table.

8. cancer-risk-[PedigreeID].png: a plot of the data in the future cancer risk estimates table.

For more details, see the "data-dictionary" directory.