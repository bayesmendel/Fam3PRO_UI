# PPI
PanelPRO Interface (PPI) is a user interface for PanelPRO developed in R Shiny. PPI interactively builds and visualizes pedigrees, customizes and runs the PanelPRO model, and displays the model results. Users can save pedigrees to their user account for future retrieval, modification, and analysis.

[Access the site](https://hereditarycancer.dfci.harvard.edu/ppi/)

## User Accounts

There are three admin/test accounts for the app:

1. admin: has admin privileges that can access all user and manager accounts by default
  
2. test_manager: has manager privileges over the test user account. This account is generally used to test features only available to accounts with manager level permissions that include loading, copying, deleting, and downloading pedigrees from subordinate accounts.
  
3. test_user: primary account used for features.

The passwords are found in `secrets`.

By default, all new user accounts have "standard" permissions which means they cannot access pedigrees from other user accounts. Upgrading a user to have manager level permissions allows them to access pedigrees from a specified set of other user accounts (to include other manager level accounts). This upgrade is done manually using HeidiSQL after they have create their account. It is as simple as changing the permissions field for that user account to say "manager" using the HeidiSQL GUI. See the "Databases" section for more details on how to access this. 

Users, including those with manager level permissions, can give access the pedigrees they create in PPI to other existing user accounts by adding one or more managers to their account. This can either be done when they create their user account or by going into their user account settings once logged in.
  
## Branches and Deployment

There are three branches in this repository: main, gb-rossi-panc, and develop. Main and gb-rossi-panc are identical copies of one another but were originally intended to be different versions of the same app. As the project evolved, the need to have two different app versions on two different branches became obsolete. However, due to the initial approach, each branch was deployed as a separate app to R Connect and therefore there are two different access URLs. Because the branches are identical, the different URLs take the user to the same app that is connected to the same database file. To make things simpler going forward, I recommend deleting the gb-rossi-panc branch and its corresponding R Connect app and asking the GB Rossi study team to stop using that URL and instead use this generic URL: [https://hereditarycancer.dfci.harvard.edu/ppi/](https://hereditarycancer.dfci.harvard.edu/ppi/). Each branch and its corresponding R Connect information is listed below.

1. main: this was intended for general access to PPI for individual researchers and clinicians who are not a part of a specific study, such as the GB Rossi ASIP study. 

  + app name: PanelPRO Interface
  
  + URL: [https://hereditarycancer.dfci.harvard.edu/ppi/](https://hereditarycancer.dfci.harvard.edu/ppi/)
  
  + Access: link is public, but need a user account
  
  + Lauren Flynn has write access
  
2. gb-rossi-panc: this is specific the to AISP study and was intended to have its own database in HeidiSQL. At present there is only one database file in HeidiSQL for all versions/branches of the PPI app. 

  + app name: PanelPRO Interface GB Rossi Pancreatic Cancer Study
  
  + URL: [https://hereditarycancer.dfci.harvard.edu/aisp-pancreatic-cancer-study/](https://hereditarycancer.dfci.harvard.edu/aisp-pancreatic-cancer-study/)
  
  + Access: link is public, but need a user account
  
  + Lauren Flynn has write access
  
3. develop: this is for developing new features and it has its own app URL because some features need to be tested locally and on the server prior to modifying the publically accessible links.

  + app name: PanelPRO Interface Development Testing
  
  + URL: [https://hereditarycancer.dfci.harvard.edu/content/653b1089-6a51-40b9-ad68-6cab2f618def](https://hereditarycancer.dfci.harvard.edu/content/653b1089-6a51-40b9-ad68-6cab2f618def)
  
  + Access: Only Annie Ng, Danielle Braun, and Lauren Flynn (also need a user account)
  
  + Only Lauren Flynn has write access 

## Databases

The pedigree and user account data is stored in a HeidiSQL database which is installed on the hereditarycancer server (the same server as R Connect). To access this data yourself, you will need a local installation of HeidiSQL which you can then use to connect to the data on server when you are on VPN. When working with and modifying the app locally, you will also need a local copy of HeidiSQL with the same general structure as the one on the server (explained below). The credentials for both versions of the database can be found in the `secrets.txt` file in the same directory as this README. If you set-up your local HeidiSQL correctly, you should be able to use the .Renviron file in the same directory as this READMe to run the app locally. Although, depending on how you do this and your OS, you may need to modify those credentials in the .Renviron file. 

Once you have HeidiSQL installed locally, you can copy the database structure from the server using the instructions found in `./sql-db-dump/README.txt`. The critical tables needed for the database to work are:

- user_base: contains the user account log-in info

- managers: a dictionary that links an account to one or more manager accounts. A user account can have multiple managers and an account with manager level permissions can have another manager level account listed as one of their managers therefore, this table contains one row for each combination of manager and subordinate account.

- admin: contains the admin account's pedigrees and serves as the template for creating user accounts.

- test_manager: contains the test_manager's pedigrees.

- test_user: contains the test_user's pedigrees.

- panels: a dictionary that links a panel_name to the unique gene names in that panel. 

As each account is created, a new SQL table is added to the database and the table takes the name of the user account. It will match the structure of the admin table and will be initiated with the `example_pedigree` from the admin table.

All pedigree data for all user accounts can be access through the back-end HeidiSQL app, after logging in, using standard SQL queries. The results of your queries can then be downloaded for further analysis.

## Managing the working pedigree (PED) and pedigreejs

Because adding/removing relatives is easier for the user to do using a drawn family tree than using a 2D table, pedigreejs was utilized by PPI. It prevents the user from having to worry about breaking family links and manually managing mother/father IDs and sexes. However, R is more flexible than pedigreejs and allows for all the detailed information PanelPRO needs to be stored in a strictly formatted data frame. R shiny also provides a much more user friendly and visually appealing UI for data entry. The following approach was used to integrate and leverage the strengths of both of these software.

Terminology note: a currently loaded pedigree which the user can modify and analyze is referred to as the "working pedigree." This is the reactive object is called PED() and it takes the form of a data frame. The working pedigree is the centerpiece of the app which all other features revolve around. This data frame contains all information PanelPRO needs for each relative. 

When the user manually saves the working pedigree or it is auto-saved, a snapshot of the working pedigree is saved in the user's SQL table. All modifications of a relative's data, such as for demographics, cancer hx, gene info, etc. are done using R Shiny inputs and modules, not pedigreejs. Each time the user changes tabs in the app (ie from demographics to cancer history, or from the pedigree editor to the home screen, as examples) or the user manually saves the pedigree using the in-app button, the requested edits on their current screen are used to modify the working pedigree data frame. Then, this pedigree is appended (if new) or over written (if existing) into the user's SQL table. 

When a user initializes a new pedigree by entering a pedigree name, proband sex and proband age, in the background the working pedigree is initialized as a 3 person pedigree with the proband, mother, and father with just their ID, mother/father ID, and sex information (plus age for the proband only). As the user works their way through the editor tabs they are entering information for the proband and the working pedigree is updated each time they advance to another tab. Once the proband information has been entered, the user is given the option to add quantities of other first degree and second degree relatives plus aunts and uncles. Once this information is entered a row for each of these relatives is added to the working pedigree with just their ID/relationship name, implied mother/father ID, and implied sex. Maternal/paternal grandparents are created as needed based on whether maternal/paternal aunts or uncles were specified by the user. At this point, a JSON version of the working pedigree is created with just the bare minimum information pedigreejs needs to draw the pedigree and show the cancer history. This JSON includes ID, mother/father ID, age, sex, and cancer history information only. This JSON is then passed to the createPedJSHandler function which then draws the pedigree via pedigreejs.

At this point, the user must use the pedigreejs interactive tree to add and remove any family members while they use the R shiny inputs and modules to modify the data for each relative. When modifying for example the race information for the mother, this information is saved only to the R data frame version of the pedigree and pedigreejs does not know or need to know this information in order to work. The R server is continually querying the pedigreejs pedigree JSON object every 1 second to detect if the the user has added or deleted a relative. It does this by converting the JSON into a data frame using the jsonlite package and comparing the number of rows from that data frame to the number of rows in the working pedigree data frame. If a difference in rows are detected, then the working pedigree data frame is updated accordingly. R assigns any new person their standardized name (ie MGMom for maternal grandmother), the working pedigree is converted back into JSON, and then updatePedJSHandler is called to update the drawn pedigreejs tree with the new pedigree that contains the new person's name/ID. Alternatively, when R shiny is used to update a sex, age, or cancer history value and this new data is used to modify the working pedigree, the R server knows to make a JSON copy of the working pedigree data frame push it to updatePedJSHandler so that the drawn tree also updates.

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
  
  + `modules.R`: contains UI module functions for cancer history, genetic testing, and managing module memory.
  
  + `ped-utils.R`: utilities that manage the pedigree structure such as creating a new pedigree, adding a new family members, modifying a family member's data. Functions that need to user a combination of cancer history, genetic testing information, demographics, and/or surgical history are also found here.
  
  + `pp-utils.R`: utilities for running PanelPRO and visualizing its results.
  
  + `surg-utils.R`: utilities for managing surgical history.
  
## Current Status of the App

The app is fully functional in its current state with all major features implemented. A history of features can be found in `task-list.xlsx` and that file also contains ideas for future improvements ranked by priority. All very high and high priority tasks were completed.

Historically, the slow connection between HeidiSQL and R Connect was causing the app the crash when loading, copying, deleting, or downloading pedigrees (although modifying a pedigree did not seem to be affected). This is a common problem for shiny apps in general. The problem was exacerbated for manager accounts because manager account first query one of their subordinate tables then a specific pedigree from that table is extracted. I pushed a fix for this on 5/3/23 that forces users to wait until the query operations finish before the user can proceed. I have not received a complaint since then. 

Ideally, users could be able to upload their own pedigrees in a variety of formats however at the moment they must create a pedigree from scratch using PPI. I started coding the function checkUploadPed in the ped-utils.R script to address this however, the feature has not yet been implemented. 

Another very useful feature which has not been implemented yet, would be to add nucleotide and variant coding validation from ClinVar via an API.

There is also a way to modify pedigreejs to show P/LP genes by relative however, this is somewhat complicated and requires more modification of the pedigreejs javascript code to be able to handle all of different PanelPRO genes. 

There are two potential improvements related to pedigreejs:

1. Note that when the user downloads a pedigree, only the kinship2 image is included, not the pedigreejs image. This was due to the difficulty in getting pedigreejs to work with the download feature but I'm sure there is a work around if given more time. The user can still download the image of the pedigreejs tree though using the image download button directly below the pedigreejs tree window.

2. The way in which R is continually scanning for changes to the pedigreejs JSON every 1 second is inefficient and, in the past, has caused the tree display to flicker every 1 second (although I partially fixed this). A better way would be for pedigreejs to inform the R server a relative has been added or deleted however this requires modification of the javascript code.

Loading testing would be great to include if this app is to be published. I began to load test the app using the directory `PanelPRO_ShinyApp\PanelPRO_app_load_test`. The shinyloadtest package unfortunately is not compatible with apps that rely on databases and some of our other complex features like pedigreejs. Instead, I created a test version of the app that only runs the PanelPRO analysis (the most computationally demanding feature). Although not representative of using the real PPI app, it did provide insight into how many simultaneous users could run PanelPRO at the same time using our R Connect server. The answer is around 12 before performance beings to degrade. This is a hardware limitation and we would need to upgrade our server to improve performance. Once the server hardware is optimized and if we still want more capacity we would have to set-up a cluster of R Connect servers to handle increased workload.
