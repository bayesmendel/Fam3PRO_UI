# PPI
PanelPRO Interface (PPI) is a user interface for PanelPRO developed in R Shiny. PPI interactively builds and visualizes pedigrees, customizes and runs the PanelPRO model, and displays the model results. Users can save pedigrees to their user account for future retrieval, modification, and analysis.

[Access the site](https://hereditarycancer.dfci.harvard.edu/ppi/)

## Test User Accounts

There are three admin/test accounts for the app:

1. Admin account: has admin privledges that can access all user and mananger accounts by default

  - username: admin
  
  - password: A9@78y4E@rhv
  
2. Test manager account: has manager privledges over the test user account. This account is generally used to test features only available to accounts with manager level permissions that include loading, copying, deleting, and downloading pedigrees from subordinate accounts.

  - username: test_manager
  
  - password: test_manager1A@
  
3. Test user account: primary account used for features.

  - username: test_user
  
  - password: test_user1A@
  
## Branches and Deployment

There are three branches in this repository: main, gb-rossi-panc, and develop. The original intent was to have slightly differnt app versions for the main and gb-rossi-panc branches however, the project did not mature to that point yet. They each have separate published R Connect access URLs (below) but they are linked to the same HeidiSQL database in the backend. 

1. main: this was intended for general access to PPI that was not specific to a study such as the GB Rossi ASIP study. 

  - app name: PanelPRO Interface
  
  - URL: [https://hereditarycancer.dfci.harvard.edu/ppi/](https://hereditarycancer.dfci.harvard.edu/ppi/)
  
  - Access: link is public, but need a user account
  
  - Lauren Flynn has write access
  
2. gb-rossi-panc: this is specific the to AISP study.

  - app name: PanelPRO Interface GB Rossi Pancreatic Cancer Study
  
  - URL: [https://hereditarycancer.dfci.harvard.edu/aisp-pancreatic-cancer-study/](https://hereditarycancer.dfci.harvard.edu/aisp-pancreatic-cancer-study/)
  
  - Access: link is public, but need a user account
  
  - Lauren Flynn has write access
  
3. develop: this is for developing new features and it has its own app URL because some features need to be tested locally and on the server prior to modifying the publically accessible links.

  - app name: PanelPRO Interface Development Testing
  
  - URL: [https://hereditarycancer.dfci.harvard.edu/content/653b1089-6a51-40b9-ad68-6cab2f618def](https://hereditarycancer.dfci.harvard.edu/content/653b1089-6a51-40b9-ad68-6cab2f618def)
  
  - Access: Only Annie Ng, Danielle Braun, and Lauren Flynn (also need a user account)
  
  - Only Lauren Flynn has write access 

## Databases

The pedigree and user account data is stored in a HeidiSQL database which is installed on the hereditarycancer server (the same server as R Connect). To access this data yourself, you will need a local installation of HeidiSQL which you can then use to connect to the data on server when you are on VPN. 

When working with and modifying the app locally, you will also need a local copy of HeidiSQL with the same general structure as the one on the server. If you set-up your local HeidiSQL correctly, you should be able to user the .Renviron file in the same directory as this READMe to run the app locally. Although, depending on how you do this and your OS, you may need to modify those credentials in the .Renviron file. Once you hae HeidiSQL running locally, you can copy the database structure from the server using the instructions found in `./sql-db-dump/README.txt`.

The credentials for both versions of the database can be found in the `secrets.txt` file in the same directory as this README.

## Account recovery email 

The lab's `hereditarycancer@ds.dfci.harvard.edu` gmail account is used to allow users to recover their accounts. The credentials needed for this are also found in `secrets.txt`. The only three credentials for that account that need to be specified in the .Renviron file for local deployments or using the environment variable screen for server deployment are:

- gmail.email

- gmail.noreply.email

- gmail.json.path

A guide to assist with this can be found on [this Basecamp resource page.](https://3.basecamp.com/3348350/buckets/710881/messages/5360891702#__recording_5377000517)

## Google Analytics

The `hereditarycancer@ds.dfci.harvard.edu` account is used to manager Google Analytics for the app. A guide for managing this is found on [this Basecamp resource page.](https://3.basecamp.com/3348350/buckets/710881/messages/5373522902#__recording_5377974815).

The `./app/google-analytics.html` file of the repo is what provides the connection between the deployed app and Google Analytics. On the Google Analytics home page you can see the access statistics by navigating to the app named "panelpro-app".

## App file structure

Various supporting files such as .Renviron, .gitignore, LICENSE.txt, secrets.txt, and this README are in the top level directory. When deploying the app, none of these files will be uploaded to the server, as intended. Note that formatNewPerson-examples.R provides working code on how to utilize the formatNewPerson() function.

Everything in the `./app` directory is deployed to the server. The main file is `app.R` which holds the UI and server. The other files and sub-directories in the `./app` are detailed below. `app.R` itself is well commented and has a strict tree structure to aid in development.

- `google-analytics.html`: connects the app to Google Analytics

- `LICENSE-text.html`: for displaying the contents of the LICENSE.

- `mylynch-app-XXXXXX.json`: this provides the "secrets" needed for the app to connect to the `hereditarycancer@ds.dfci.harvard.edu` account. This must be deployed to the server for it to work.

- `non.pp.cancer.list.csv`: a list of non-PanelPRO cancers from the Dana-Farber website.

- `data-dictionary`: a starter directory which hold files for when a user downloads the data dictionary. Additional files are added upon download and then auto-deleted because they are specific to the user's session/download. A README is included which explains each permanent and temporary file in the directory.

- `download-pedigrees`: a starter directory which hold files for when a user downloads one or more pedigrees. Additional files are added upon download and then auto-deleted because they are specific to the user's session/download. A README is included which explains each permanent and temporary file in the directory.

- `download-results`: a starter directory which hold files for when a user downloads the PanelPRO results for a specific pedigree. Additional files are added upon download and then auto-deleted because they are specific to the user's session/download. A README is included which explains each permanent and temporary file in the directory.

- `rsconnect`: holds the .dfc files for deploying the app to R Connect. There are three, one for each app name as explained in the "Branches and deployment" section of this README.

- `www`: contains all image files, a css style sheet, and all pedigreejs javascript code for the app.

- `www/pedigreejs`: contains the following pedigreejs files:

  + `d3.min.js`: a requirement of pedigreejs, assists in building the tree. No modification necessary, stricly a dependency.
  
  + `Html.html`: the code tells pedigreejs where to display the Tree and it related buttons (ie zoom, download).
  
  + `JS.js`: this is the most important file. This is what connects the app to pedigreejs. It tells pedigreejs the settings you want like which cancers to include, window size information, and more. It also handles the pedigree data transfer to/from R and javascript. There are three functions:
  
    + removeNull: this takes the JSON of pedigree data provided by the R server and removes all NA/NULL values which pedigreejs cannot process.
	
	+ createPedJSHandler: creates the initial pedigreejs tree during a user session. This function contains an important interval function called `getpedigree` which is used by R to constantly scan for updates to the tree structure so that the R version of the pedigree can be updated. 
	
	+ updatePedJSHandler: all subsequent updates to the pedigreejs tree. Even if the user loads a new pedigree in the same session in which createPedJSHandler has been called, updatePedJSHandler will still be used to create the new tree. 
	
  + `pedigreejs-customizations.md`: a file that explains what customizations were made to get pedigreejs to work with PPI. Required as per the pedigreejs license.
  
  + `build/pedigreejs.v2.1.0-rc9-customized-for-PPI-3.js`: the pedigreejs code used in the app with slight modification as described in the above file. 
  
  + `build/pedigreejs.v2.1.0-rc9.js`: the original pedigreejs code as a reference (not used in the app though).
  
  + `build/pedigreejs.v2.1.0-rc9.css`: a style sheet that styles the pedigreejs buttons on each tree node (ie add sibling, delete person, etc)
  
- `R`: contains all scripts that support `app.R`. Each script is well commented.

  + `var.R`: commonly used variables.
  
  + `accnt-utils.R`: utilities for user accounts (account creation, save pedigree to user account, etc.).
  
  + `can-utils.R`: utilities for managing cancer history
  
  + `demo-utils.R`: utilities for managing demographic data pedigrees
  
  + `gene-utils.R`: utilities for manaing genetic testing data for pedigrees
  
  + `modules.R`: contatins UI module functions for cancer history, genetic testing, and managing module memory.
  
  + `ped-utils.R`: utilities that manage the pedigree structure such as creating a new pedigree, adding a new family members, modifying a family member's data. Functions that need to user a combintation of cancer history, genetic testing information, demographics, and/or surgical history are also found here.
  
  + `pp-utils.R`: utilities for running PanelPRO and visualizing its results.
  
  + `surg-utils.R`: utilities for manaing surgical history.
  


