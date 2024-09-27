'''
README

This script contains a function that exhaustively scans a specified GitHub repository 
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
  1. branches_df: This provides a list of all 
     potential PHI values contained in each file, within each remaining branch of 
     the GitHub repository.
  2. issues_df: This provides a list of 
     all potential PHI text information contained in every Issue and Pull Request (open
     or closed) of the GitHub repository.
  3. images_df: This provides a list of 
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

requirements.txt included. Excecuted in Python 3.10
'''

import os
import subprocess
import re
from bs4 import BeautifulSoup
import requests
from time import time
import pytz
from datetime import datetime
import pandas as pd


# ------------------------------------------------------------
# Function to scan a file's text for defined PHI syntax
# ------------------------------------------------------------
# Define the PHI patterns
patterns = {
    'member_identifier': r'\bMEM\d{5,}\b|\bXEM\d{5,}\b',
    'cin_no': r'\b\d{8}[A-Z]\b',
    'claim_id': r'\b\d{5}[A-Z]\d{6}',
    'enc_util_id': r'\b\d{9}\b',
    'SSN': r'\b\d{3}-\d{2}-\d{4}',
    'Phone Number': r'\b\d{3}-\d{4}\b',
}


# Function to scan file for PHI
def scan_file_for_phi(file_path):

    # Read the file content
    with open(file_path, 'r', encoding='ISO 8859-1') as file:
        content = file.read()

    # Dictionary to hold results
    findings = []

    # Scan for each pattern
    for phi_type, pattern in patterns.items():
        matches = re.findall(pattern, content)
        if matches:
            findings.append({phi_type: matches})

    return findings


# ------------------------------------------------------------
# # Function to scan all files in a directory for PHI
# ------------------------------------------------------------  
def scan_directory_for_phi(directory_path):

    results = {}

    # Scan each file in the directory
    for root, _, files in os.walk(directory_path):
        for file_name in files:
            if (file_name.endswith('.py') |\
               file_name.endswith('.R') |\
               file_name.endswith('.r') |\
               file_name.endswith('.hql') |\
               file_name.endswith('.sql') |\
               file_name.endswith('.HQL') |\
               file_name.endswith('.SQL') |\
               file_name.endswith('.md') |\
               file_name.endswith('.txt') |\
               file_name.endswith('.csv')):
                file_path = os.path.join(root, file_name)
                findings = scan_file_for_phi(file_path)
                if findings:
                    results[file_path] = findings

    return results
  

# ------------------------------------------------------------
# Function to identify all remote feature branches of a GitHub repository
# ------------------------------------------------------------
def get_feature_branches():
    # Run the git command and capture the output
    result = subprocess.run(['git', 'ls-remote', '--heads', 'origin'], stdout=subprocess.PIPE, text=True)

    # Split the output into lines
    lines = result.stdout.splitlines()

    # Filter branches that start with 'refs/heads/feature/'
    feature_branches = []
    for line in lines:
        # Split the line to remove the commit hash
        commit_hash, ref = line.split()
        
        # Check if the ref starts with 'refs/heads/feature/'
        if ref.startswith('refs/heads/'):
            # Remove 'refs/heads/' to get the branch name
            branch_name = ref.replace('refs/heads/', '')
            feature_branches.append(branch_name)

    return feature_branches
  
  




# ------------------------------------------------------------
# ------------------------------------------------------------
# # Function to review all GitHub scripts, Issues and Pull Requests
# ------------------------------------------------------------
# ------------------------------------------------------------

def repository_phi_scan(
  repo_ssh_link,
  local_root_directory,
  github_url,
  github_username,
  github_password,
  known_non_phi_list = []
):

    # ------------------------------------------------------------
    # Review GitHub scripts from all remote branches for PHI
    # ------------------------------------------------------------

    # Get the repo path
    focal_owner_repo = repo_ssh_link.split(':')[1].split('.g')[0]
    focal_repo = repo_ssh_link.split('/')[1].split('.g')[0]
    write_out_focal_name = re.sub('/','_',focal_owner_repo)

    # Read in the focal repo from remote GitHub environment
    os.system(f'rm -rf ~/{focal_repo}')
    os.system(f'git clone {repo_ssh_link}')
    os.chdir(focal_repo)


    # Identify all remote branches
    feature_branches = get_feature_branches()


    # Capture all potential PHI information in a single Pandas dataframe
    branches_df = pd.DataFrame()

    for branch in feature_branches:    

        os.system(f'git fetch origin {branch} > /dev/null 2>&1')
        os.system(f'git checkout -b {branch} origin/{branch} > /dev/null 2>&1')
        os.system(f'git pull origin {branch} > /dev/null 2>&1')

        print(f'Scanning {branch} remote branch scripts for potential PHI...')

        scan_results = scan_directory_for_phi(f'{local_root_directory}{focal_repo}')

        # Display and persist the results
        if scan_results:
            for file, findings in scan_results.items():    
                # Initialize an empty list to store rows
                rows = []

                file = re.sub('{local_root_directory}', '', file)
                print(f"\nPotential PHI found in {file}:")
                for finding in findings:
                    print(finding)

                phi_type = list(findings[0].keys())[0]
                phi_values = findings[0][phi_type]

                # remove values that are known to not be PHI, even though they're structured as such
                if (phi_type == 'encounter_id') & (len(known_non_phi_list) > 0):
                    phi_values = [x for x in phi_values if x not in known_non_phi_list]

                # Append each entry as a row in the format (script_path, phi_type, phi_values)
                if len(phi_values) > 0:
                    rows.append([file, phi_type, phi_values])

                # Create the DataFrame
                branch_df = pd.DataFrame(rows, columns=['file_path', 'phi_type', 'phi_values'])
                branch_df['branch'] = branch

                # Append to the larger set of findings
                branches_df = pd.concat([branches_df, branch_df], axis=0).reset_index(drop=True)
        else:
            print("No potential PHI found.")

        # Delete the feature branch reset to the main branch
        os.chdir(f'{local_root_directory}{focal_repo}')
        if 'main' in feature_branches:
            os.system(f'git checkout main > /dev/null 2>&1')
            os.system(f'git reset --hard origin/main > /dev/null 2>&1')
        else:
            os.system(f'git checkout master > /dev/null 2>&1')
            os.system(f'git reset --hard origin/master > /dev/null 2>&1')

        os.system(f'git branch -d {branch} > /dev/null 2>&1')


        print(f'\n...COMPLETED SCAN FOR POTENTIAL PHI IN THE {branch} REMOTE BRANCH SCRIPTS.\n\n\n\n\n')


    # Format the data frame for review
    if len(branches_df) == 0:
        branches_df = pd.DataFrame({'message': ['No suspected PHI found in any script in any branch!']})
    else:
        branches_df = branches_df[['branch', 'file_path', 'phi_type', 'phi_values']]

    os.chdir('..')
    os.system(f'rm -rf ~/{focal_repo}')




    # ------------------------------------------------------------
    # # Review GitHub Issues
    # ------------------------------------------------------------

    # Create a session object
    session = requests.Session()

    # Get the login page
    login_url = f'{github_url}/login'
    response = session.get(login_url)
    soup = BeautifulSoup(response.text, 'html.parser')

    # Find the authenticity token
    token = soup.find('input', {'name': 'authenticity_token'}).get('value')

    # Define login credentials
    payload = {
        'login': github_username,
        'password': github_password,
        'authenticity_token': token
    }


    # Perform login
    session.post(f'{github_url}/session', data=payload)

    # After login, navigate to the issues page
    issues_url = f'{github_url}/{focal_owner_repo}/issues?q='
    response = session.get(issues_url)
    soup = BeautifulSoup(response.text, 'html.parser')

    # Get the number of issue pages to crawl through  
    def page_counter():
        try:
            n = int(
              soup.find_all(class_="paginate-container d-none d-sm-flex flex-sm-justify-center") \
                [0].find('em', class_='current')['data-total-pages']
            )
        except TypeError:
            n = 1

        return n

    total_pages = page_counter()



    # Define dataframes to persist findings
    issues_df = pd.DataFrame()
    images_df = pd.DataFrame()

    # Crawl through each Issue and Pull Request on each webpage of the repo
    for page_number in range(1, total_pages+1):
        issues_page_url = f'{github_url}/{focal_owner_repo}/issues?page={page_number}&q='
        issues_page_response = session.get(issues_page_url)
        page_soup = BeautifulSoup(issues_page_response.text, 'html.parser')

        text_results = {}
        image_results = {}

        # Now scrape the issues
        issues = page_soup.find_all(class_="js-issue-row")
        for issue in issues:
            title = issue.find('a', class_='Link--primary').text.strip()
            print(f"Scanning Issue: {title} for potential PHI...")

            link = issue.find('a', class_='Link--primary')
            href_value = link['href']

            issue_page_url = f'{github_url}/{href_value}'
            issue_page_response = session.get(issue_page_url)
            issue_soup = BeautifulSoup(issue_page_response.text, 'html.parser')
            issue_soup_content = issue_soup.find_all(class_="d-block user-select-contain")


            # Dictionary to hold text finding results
            # -----------------------------------------------------
            text_findings = []

            # Scan for each pattern
            for phi_type, pattern in patterns.items():
                matches = re.findall(pattern, str(issue_soup_content))
                if matches:
                    text_findings.append({phi_type: matches})

            if len(text_findings) > 0:
                text_results[title] = text_findings
                # Initialize an empty list to store rows
                rows = []

                # Loop through the data and unpack the information
                for issue_title, phi_list in text_results.items():
                    for phi in phi_list:
                        for phi_type, phi_values in phi.items():
                            # Append each entry as a row in the format (script_path, phi_type, phi_values)
                            rows.append([issue_title, phi_type, phi_values])

                # Create the DataFrame
                issue_df = pd.DataFrame(rows, columns=['issue_title', 'phi_type', 'phi_values'])                
                issues_df = pd.concat([issues_df, issue_df], axis=0)

                print(f'\t{text_findings}')


            # Dictionary to hold image finding results
            # -----------------------------------------------------
            image_findings = []

            # Scan for each pattern
            image_matches = re.findall('alt="image" src="https://dsghe', str(issue_soup_content))
            if image_matches:
                image_findings = image_findings+image_matches

            if len(image_findings) > 0:
                image_results[title] = image_findings
                # Initialize an empty list to store rows
                rows = []

                # Loop through the data and unpack the information
                for issue_title, image_list in image_results.items():
                    for image in image_list:
                        # Append each entry as a row in the format (script_path, phi_type, phi_values)
                        rows.append([issue_title, image])

                # Create the DataFrame
                issue_images_df = pd.DataFrame(rows, columns=['issue_title', 'image'])                
                images_df = pd.concat([images_df, issue_images_df], axis=0)

                print('\tscreenshot image found')


            print(f'\n')



    # Format the data frames for review
    if len(issues_df) == 0:
        issues_df = pd.DataFrame({'message': ['No suspected PHI found in any Issues or Pull Requests!']})

    if len(images_df) == 0:
        images_df = pd.DataFrame({'message': ['No screenshots found in any Issues or Pull Requests!']})
    else:
        images_df['image_found'] = 'yes'
        images_df = images_df[['issue_title', 'image_found']].drop_duplicates()

    return branches_df, issues_df, images_df


  

# Run the function
# --------------------------------------------------------
script_report, issues_text_report, issues_screenshot_report = repository_phi_scan(
  repo_ssh_link = 'git@github.com:bshelton141/technical_examples.git',
  local_root_directory = '/home/',
  github_url = 'https://github.com',
  github_username = os.getenv('public_github_username'),
  github_password = os.getenv('public_github_password')
)

script_report
issues_text_report
issues_screenshot_report
