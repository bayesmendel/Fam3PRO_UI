# README

This directory contains three files which will either be in .csv or .rds format, depending on the chosen file type at download:

1. "pedigrees": contains all data needed to run PanelPRO and additional columns required for compatibility with PPI.

2. "cancer-details": a table indexed by PedigreeID, ID, and cancer type that contains cancer diagnoses and ages of diagnoses. This information was extracted from the cancersJSON column in the "pedigrees" file.

3. "panel-details": a table indexed by PedigreeID, ID, gene, result type, nucleotide, protein, zygosity and panel name. This information was extracted from the genesJSON column in the "pedigrees" file.

If only one pedigree was downloaded, then a fourth file will be included:

4. "pedigree-image.png": image of the pedigree tree. Note that this image can only display up to four different PanelPRO cancers. If you prefer a higher quality image that can display more cancers then you can manually download the image of the interactive pedigree tree from the Create/Modify Pedigree tab.

See the "data-dictionary" directory for more information.
