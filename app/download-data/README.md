# README

This directory contains three files which will either be in .csv or .rds format, depending on the chosen file type at download:

1. "pedigrees": contains all data needed to run PanelPRO and additional columns required for compatibility with PPI.

2. "cancer-details": a table indexed by PedigreeID, ID, and cancer type that contains cancer diagnoses and ages of diagnoses. This information was extracted from the cancersJSON column in the "pedigrees" file.

3. "panel-details": a table indexed by PedigreeID, ID, gene, result type, nucleotide, protein, zygosity and panel name. This information was extracted from the genesJSON column in the "pedigrees" file.

See the "data-dictionary" directory for more information.
