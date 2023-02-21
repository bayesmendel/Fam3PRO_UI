# README

This directory contains three files which will either be in .csv or .rds format, depending on the chose file type:

1. "pedigrees": this contains add data needed to run PanelPRO and for compatibility with PPI.

2. "cancer-details": a table indexed by PedigreeID, ID, and cancer type with cancer diagnoses and ages of diagnoses. This information was extracted from the cancersJSON column in the "pedigrees" file.

3. "panel-details": a table indexed by PedigreeID, ID, gene, result type, nucleotide, protein, zygosity and panel name. This information was extracted from the genesJSON column in the "pedigrees" file.

See the "data-dictionary" directory for more information.
