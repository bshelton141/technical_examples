# README

The `github_phi_scanning.py` script contains a function that exhaustively scans a specified GitHub repository 
for specified PHI syntax, through regular expressions. 

Scanning operations:
  1. Scans all files with the following suffix values from each remote branch of the specified
     GitHub repository, identifying potential PHI values: 
     '.py', '.r', '.R', '.hql', '.sql', '.HQL', '.SQL', '.csv', '.txt'
  2. Scans all open and closed Issues and Pull Requests included within the remote GitHub
     repository to:
     a. identifying potential PHI values.
     b. identify screenshots.
     
Function outputs:
  1. `branches_df`: This provides a list of all 
     potential PHI values contained in each file, within each remaining branch of 
     the GitHub repository.
  2. `issues_df`: This provides a list of 
     all potential PHI text information contained in every Issue and Pull Request (open
     or closed) of the GitHub repository.
  3. `images_df`: This provides a list of 
     the Issues and Pull Request of the GitHub repository that contain a screenshot. The 
     screenshot may not contain PHI, but each one needs to be manually reviewed and validated.
     

Access Requirements:
  1. The ability to clone the target remote repository through an SSH connection.
  2. A GitHub username and password that has access to the target remore repository.
  

Notes:
  - This script's `patterns` objects defines the potential PHI being identified. Each
    organization will have different member and claim syntaxes, and the patterns need
    to be updated appropriately.
  - The repositories being scanned must have "Issues" enabled in order to scan the Issues.

`requirements.txt` included. Excecuted in Python 3.10
