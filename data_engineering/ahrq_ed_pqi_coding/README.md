# README

Th `ahrq_ed_pqi_coding.py` script reads the Excel files from the zip file directly from AHRQ.
It then ingests the codes from each Excel/tab combination and labels the 
information in alignment with the AHRQ inclusion and exclusion criteria.

After consolidating all of the codes for each of the 5 PQIs, the code
applies the decimal logic.

This file only needs to be updated when AHRQ updates their logic.

AHRQ PQE Technical Specifications site: https://qualityindicators.ahrq.gov/measures/ED_PQI_TechSpec

`requirements.txt` included. Executed in Python 3.10.
