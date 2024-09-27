'''
This code reads the Excel files from the zip file directly from AHRQ.
It then ingests the codes from each Excel/tab combination and labels the 
information in alignment with the AHRQ inclusion and exclusion criteria.

After consolidating all of the codes for each of the 5 PQIs, the code
applies the decimal logic.

This file only needs to be updated when AHRQ updates their logic.

AHRQ PQE Technical Specifications site: https://qualityindicators.ahrq.gov/measures/ED_PQI_TechSpec

requirements.txt included in this directory. Executed in Python 3.10.
'''

# online path to AHRQ Excel files
path = 'https://qualityindicators.ahrq.gov/Downloads/Modules/ED_PQI/V2023/TechSpecs/ED_PQI_2023_ICD10_techspecs_excel.zip'


import urllib.request
from zipfile import ZipFile
from io import BytesIO
import tempfile
import os
import numpy as np
import pandas as pd
import openpyxl
import shutil
import re
import datetime



# Un-zip the Excel files in a temporary file location
####################################################################

url_loc = urllib.request.urlopen(path)
zfile = ZipFile(BytesIO(url_loc.read()))
# Create the temporary directory to store the zip file's content
temp_dir = tempfile.TemporaryDirectory()

# Extract the zip file's content into the temporary directory
zfile.extractall(temp_dir.name)

# Print the files names in the directory
os.listdir(temp_dir.name)



# Use the technical specifications to identify the inclusion and exclusion sheets of each PQE
####################################################################

# Define a function to loop through all relevant sheets for each PQE
def pqe_sheet_consolidation(pqe, file_name, sheet_list, dx_type):
  
    temp_df = pd.DataFrame()
    
    # looping function to get the diagnoses out of each sheet
    for sheet in sheet_list:

        sheet_inclusion = pd.read_excel(temp_dir.name+'/'+file_name,
                                        sheet_name=sheet,
                                        engine='openpyxl',
                                        skiprows = 1)

        # Create a list out of the first and third colums
        sheet_inclusion_list = sheet_inclusion.iloc[:,0].tolist() + sheet_inclusion.iloc[:,3].tolist()

        # Remove nan values
        sheet_inclusion_list = [i for i in sheet_inclusion_list if not pd.isna(i)]

        # Create dataframe
        sheet_inclusion = pd.DataFrame({'dx_code': sheet_inclusion_list})
        sheet_inclusion['pqe'] = pqe
        sheet_inclusion['type'] = dx_type
        sheet_inclusion['category'] = sheet

        # Join to the other Chronic ASC conditions
        temp_df = pd.concat([temp_df, sheet_inclusion], axis=0)
        
    return temp_df.reset_index(drop=True)
      


# Use the function above to pull in all relevant dx codes for each PQE
#
pqe01_inclusion = pqe_sheet_consolidation(
  pqe='Dental Visit',
  file_name = 'PQE_01_Visits_for_Dental_Conditions.xlsx',
  sheet_list = ['DENTALVISIT'],
  dx_type = 'Inclusion'
)

pqe01_exclusion = pqe_sheet_consolidation(
  pqe='Dental Visit',
  file_name = 'PQE_01_Visits_for_Dental_Conditions.xlsx',
  sheet_list = ['TRAUMATOFACE'],
  dx_type = 'Exclusion'
)

pqe02_inclusion = pqe_sheet_consolidation(
  pqe='Chronic ASC',
  file_name = 'PQE_02_Visits_for_Chronic_Conditions.xlsx',
  sheet_list = ['ASTHMA', 'COPD', 'HEARTFAILURE', 'DMSTCX', 'CKD', 'LOWERRESPINFECTION'],
  dx_type = 'Inclusion'
)

pqe03_inclusion = pqe_sheet_consolidation(
  pqe='Acute ASC',
  file_name = 'PQE_03_Visits_for_Acute_Conditions.xlsx',
  sheet_list = ['UTI_NONCX', 'UPPERRESPINFECTION', 'INFLUENZA', 'CELLULITIS'],
  dx_type = 'Inclusion'
)

pqe03_exclusion = pqe_sheet_consolidation(
  pqe='Acute ASC',
  file_name = 'PQE_03_Visits_for_Acute_Conditions.xlsx',
  sheet_list = ['IMMUNOCOMPROMISED', 'DIABETES', 'QE03EXC_UTI', 'QE03EXC_UTM'],
  dx_type = 'Exclusion'
)

pqe04_inclusion = pqe_sheet_consolidation(
  pqe='Asthma',
  file_name = 'PQE_04_Visits_for_Asthma.xlsx',
  sheet_list = ['ASTHMA', 'QE4BRONCHITIS'],
  dx_type = 'Inclusion'
)

pqe04_exclusion = pqe_sheet_consolidation(
  pqe='Asthma',
  file_name = 'PQE_04_Visits_for_Asthma.xlsx',
  sheet_list = ['CYSTICFIBROSIS', 'RESPIRATORYANOMALIES', 'QE4EXC_PNEUMONIA'],
  dx_type = 'Exclusion'
)

pqe05_inclusion = pqe_sheet_consolidation(
  pqe='Back Pain',
  file_name = 'PQE_05_Visits_for_BackPain.xlsx',
  sheet_list = ['BACKPAIN'],
  dx_type = 'Inclusion'
)

pqe05_exclusion = pqe_sheet_consolidation(
  pqe='Back Pain',
  file_name = 'PQE_05_Visits_for_BackPain.xlsx',
  sheet_list = ['BPEXCLUDEUTI', 'BPEXCLUDEFEVER', 'BPEXCLUDECES'],
  dx_type = 'Exclusion'
)

pqe05_exclusion_appendix_a = pqe_sheet_consolidation(
  pqe='Back Pain',
  file_name = 'ED_PQI_Appendix_A.xlsx',
  sheet_list = ['APPENDIX A'],
  dx_type = 'Exclusion'
)

pqe05_exclusion_appendix_b = pqe_sheet_consolidation(
  pqe='Back Pain',
  file_name = 'ED_PQI_Appendix_B.xlsx',
  sheet_list = ['APPENDIX B '],
  dx_type = 'Exclusion'
)


# Consolidate the files
pqe_tables = [pqe01_inclusion, pqe01_exclusion,
              pqe02_inclusion,
              pqe03_inclusion, pqe03_exclusion,
              pqe04_inclusion, pqe04_exclusion,
              pqe05_inclusion, pqe05_exclusion, 
              pqe05_exclusion_appendix_a, pqe05_exclusion_appendix_b]
pqe_codes = pd.concat(pqe_tables, axis=0).reset_index(drop=True)


# Format the dx codes to include decimals
pqe_codes['dx_code'] = np.where(
  pqe_codes['dx_code'].str.len() > 3,
  pqe_codes['dx_code'].str.slice(start=0, stop=3)+'.'+pqe_codes['dx_code'].str.slice(start=3),
  pqe_codes['dx_code']
)

# Strip any unnecessary whitespace from the category field
pqe_codes['category'] = pqe_codes['category'].str.strip()
