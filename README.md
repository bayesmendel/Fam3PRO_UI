# F3PI
FamePRO Interface (F3PI) is a user interface for Fam3PRO developed in R Shiny. F3PI interactively builds and visualizes pedigrees, customizes and runs the Fam3PRO model, and displays the model results. Users can save pedigrees to their user account for future retrieval, modification, and analysis.

[Access the site](https://hereditarycancer.dfci.harvard.edu/Fam3PRO/)

## User Accounts

There are three admin/test accounts for the app:

1. admin: admin privileges allow this account to access all pedigrees in all user accounts.
  
2. test_manager: has manager privileges over the "test_user" account. This account is generally used to test features only available to accounts with manager level permissions that include loading, copying, deleting, and downloading pedigrees from subordinate accounts.
  
3. test_user: used for general app testing. 

The passwords for each account are found in the `secrets` file.

By default, all new user accounts have "standard" permissions which means they cannot access pedigrees from other user accounts. Upgrading a user to have "manager" level permissions allows them to access pedigrees from a specified set of other user accounts (to include other manager level accounts). To upgrade an account's permissions from "standard" to "manager" (or to "admin"), this must be done manually using the HeidiSQL GUI. Before this can be done, the user must have already created an account themselves using the app. Once they have account, upgrading them is as simple as changing the permissions field in the user_base SQL table for that person's user account to say "manager". See the "Databases" section for more details on how to access this. 

Users, including those with manager level permissions, can give any manager level user account access to the pedigrees they create by specifying those manager level user accounts as managers of the user. This can either be done either when the user creates their account or navigating to their user account settings after logging in.
  
## Branches and Deployment

There are three branches in this repository: main, gb-rossi-panc, and develop. Main and gb-rossi-panc are identical copies of one another but were originally intended to be different versions of the same app. As the project evolved, the need to have two different app versions became obsolete. However, due to this initial approach, each branch was deployed to R Connect as a separate app and therefore, there are two different access URLs. Because the branches are identical, the different URLs take the user to the same app that is connected to the same database file. To make things simpler going forward, I recommend deleting the gb-rossi-panc branch and its corresponding R Connect app and asking the GB Rossi study team to stop using that URL and instead use this generic URL: [https://hereditarycancer.dfci.harvard.edu/F3PI/](https://hereditarycancer.dfci.harvard.edu/F3PI/). Each branch and its corresponding R Connect information is listed below.

1. main: this was intended for general access to F3PI for individual researchers and clinicians who are not a part of a specific study, such as the GB Rossi ASIP study. 

    + app name: Fam3PRO Interface
  
    + URL: [https://hereditarycancer.dfci.harvard.edu/F3PI/](https://hereditarycancer.dfci.harvard.edu/F3PI/)
  
    + Access: link is public, but need a user account
  
    + Lauren Flynn has write access
  
2. gb-rossi-panc: this is specific the to AISP study and was intended to have its own database in HeidiSQL. At present there is only one database file in HeidiSQL for all versions/branches of the F3PI app. 

    + app name: Fam3PRO Interface GB Rossi Pancreatic Cancer Study
  
    + URL: [https://hereditarycancer.dfci.harvard.edu/aisp-pancreatic-cancer-study/](https://hereditarycancer.dfci.harvard.edu/aisp-pancreatic-cancer-study/)
  
    + Access: link is public, but need a user account
  
    + Lauren Flynn has write access
  
3. develop: a standard repo develop branch for developing new features. It has its own app URL because during development some features perform differently when deployed to the server than when they are tested locally. When a new feature has been tested locally and is ready to be deployed, it is recommended that it first be deployed to this test version of the app. Once deployed to this version of the app, test your new feature works as intended and then re-deploy it to one of the two production versions of the app listed above. 

    + app name: Fam3PRO Interface Development Testing
  
    + URL: [https://hereditarycancer.dfci.harvard.edu/content/653b1089-6a51-40b9-ad68-6cab2f618def](https://hereditarycancer.dfci.harvard.edu/content/653b1089-6a51-40b9-ad68-6cab2f618def)
  
    + Access: Only Annie Ng, Danielle Braun, and Lauren Flynn (also need a user account)
  
    + Only Lauren Flynn has write access 

## Databases

The pedigree and user account data is stored in a HeidiSQL database file which is installed on the hereditarycancer server (the same server as R Connect). To access this data yourself, you will need a local installation of HeidiSQL which you can then use to connect to the data on server (you must be connected to DFCI VPN first). When working with and modifying the app locally, you will also need a local copy of HeidiSQL database file with the same general structure as the one on the server (explained below). The credentials for both versions of the database can be found in the `secrets` file in the same directory as this `README`. If you set-up your local HeidiSQL correctly, you should be able to use the `.Renviron` file in the same directory as this `README` to run the app locally. Although, depending on how you do this and your OS, you may need to modify those credentials in the `.Renviron` file (always restart all R Studio windows after modifying a `.Renviron` file). 

Once you have HeidiSQL installed locally, you can copy the database structure from the server using the instructions found in `./sql-db-dump/README.txt`. The critical tables needed for the database to work are:

- `user_base`: contains the user account log-in info and permission level information.

- `managers`: a dictionary that links an account to one or more manager accounts. A user account can have multiple managers and an account with manager level permissions can have another manager level account listed as one of their managers. Therefore, this table contains one row for each combination of manager and subordinate account.

- `admin`: contains the admin account's pedigrees and serves as the template for creating future user accounts.

- `test_manager`: contains the test_manager account's pedigrees.

- `test_user`: contains the test_user account's pedigrees.

- `panels`: a dictionary that links a panel name to the list of genes in that panel. 

Each user account is assigned one and only one table in the database and a user account's table is named the same as the user's account name. Different pedigrees within a user's SQL table are differentiated by the `PedigreeID` column.

As each new user account account is created, a new SQL table is added to the database which takes the name of the new user account. The new table will match the structure of the `admin` table and will be initialized with the `example_pedigree` from the `admin` table.

All pedigree data for all user accounts can be access through the back-end of the app using HeidiSQL. Once logged into the database on the server using the HeidiSQL GUI, you can use SQL queries to gather and export any data you need from any user account.

## Managing the working pedigree (PED) and pedigreejs

Adding and removing relatives from a pedigree is easier using an interactive drawn family tree than it is using a 2D table. Editing the pedigree via an interactive drawn tree prevents the user from having to worry about breaking family links and manually managing mother/father IDs and sexes. [pedigreejs](https://ccge-boadicea.github.io/pedigreejs/) is a javascript web tool which accomplishes this task however, R is more flexible than pedigreejs and allows for all the detailed information Fam3PRO needs to be stored in a strictly formatted data frame. R {shiny} also provides a much more user friendly and visually appealing UI for data entry that pedigreejs. The following approach was used to integrate and leverage the strengths of both pedigreejs, R, and R {shiny}.

Terminology note: the currently loaded pedigree which the user can modify and analyze is referred to as the "working pedigree." This is the reactive value object named `PED()` and it takes the form of an R data frame. The working pedigree is the centerpiece of the app which all other features revolve around. When the user is ready to run Fam3PRO, the working pedigree (`PED()`) is passed to the `pedigree` argument of the `Fam3PRO()` function.

When a user initializes a new pedigree they are asked to enter a pedigree name, proband sex and proband age. Once this happens, `PED()` is initialized as a 3 person pedigree consisting of the proband, the mother, and the father. This initial working pedigree contains blank or default values for each column in `PED()` except for the ID columns and the Sex column. The proband's age is also populated. Next, the user will work their way through entering the data for the proband. This is accomplished by navigating through six different tabs: Demographics, Cancer Hx, CBC Risk, Tumor Markers, Surgical Hx, and Genes. Each time the user advances to the next tab, the data from the previous tab is used to update the working pedigree. 

Once the proband information has been entered, the user comes to a seventh tab named "Add Relatives". Here the user enters quantities by relative type for siblings, children, aunts, and uncles. Once this information is entered a row for each of these relatives is added to the working pedigree with just the ID, relationship name, implied mother/father ID, and implied sex. If any maternal/paternal aunts or uncles were specified, then a corresponding set of maternal/paternal grandparents are created automatically. Next, a JSON copy of the working pedigree is created with just the bare minimum information pedigreejs needs to draw the pedigree and show the cancer history. This JSON includes ID, mother/father ID, age, deceased status, sex, and cancer history information only. This JSON is then passed to the `createPedJSHandler` function which calls on pedigreejs to draw an interactive family tree. 

At this point, the user must use the pedigreejs interactive tree to add and remove any family members. This functionality also allows for the addition of more distant relatives such as cousins, grandchildren, etc. While pedigreejs is used to manage the tree structure and add/remove relatives, the R {shiny} inputs and modules are used to modify the data for each relative. The same six tabs used to enter the proband's data are re-used to enter any relative's data. To switch which relative the user is entering data for, they just need to select the new relative from the drop-down along the top of the screen. 

For example, when modifying the race information for the proband's mother, the user first needs to select "Mother" in the relative selector drop-down and then navigate to the Demographics tab. Next, the user updates the race using the drop-down menu. This information is not updated in the working pedigree until one of these 4 events occurs: 1) the user changes the selected relative, 2) the user changes to another pedigree editor tab (ie to Cancer Hx), 3) the user changes to another navbar Tab (ie the Home tab or the Fam3PRO tab), or 4) the user clicks the "Update and Save Pedigree" button. Every time the working pedigree is updated, a snapshot of `PED()` is saved to the user's SQL table in the database and the previous copy is overwritten. Now because race does not affect the pedigree structure and is not displayed in the interactive family tree, pedigreejs is not informed about this change. However, if the user was to update a current age value, deceased status, or any cancer history, this information is displayed in the interactive family tree and therefore a new JSON copy of the working pedigree is created and passed to pedigreejs via `updatePedJSHandler`. `updatePedJSHandler` tells pedigreejs to redraw the tree with the updated information.

The R server is continually querying the pedigreejs pedigree JSON object via the `getpedigree` function every 1 second to detect if the user has added or deleted any relative using the interactive family tree. The server does this by converting the JSON into a data frame using the {jsonlite} package and comparing the number of rows from that data frame to the number of rows in the working pedigree data frame. If a difference in rows is detected, then `PED()` is updated accordingly. Once `PED()` has been updated, another JSON copy of it is created and passed back to `updatePedJSHandler` to redraw the interactive family tree with either the deleted relative removed or with the new relative(s) added with their standardized IDs and names to match the rest of the family. 


## Account recovery email 

The lab's `hereditarycancer@ds.dfci.harvard.edu` gmail account is used to allow users to recover their accounts. The credentials needed for this are also found in `secrets.txt`. The only three credentials for that account that need to be specified in the .Renviron file for local deployments or using the environment variable screen for server deployment are:

- gmail.email

- gmail.noreply.email

- gmail.json.path

A guide to assist with this can be found on [this Basecamp resource page.](https://3.basecamp.com/3348350/buckets/710881/messages/5360891702#__recording_5377000517)

## Google Analytics

The `hereditarycancer@ds.dfci.harvard.edu` account is used to manager Google Analytics for the app. A guide for managing this is found on [this Basecamp resource page.](https://3.basecamp.com/3348350/buckets/710881/messages/5373522902#__recording_5377974815).

The `./app/google-analytics.html` file of the repo is what provides the connection between the deployed app and Google Analytics. On the Google Analytics home page you can see the access statistics by navigating to the app named "Fam3PRO-app".

## App file structure

Various supporting files such as .Renviron, .gitignore, LICENSE.txt, secrets.txt, and this README are in the top level directory. When deploying the app, none of these files will be uploaded to the server, as intended. Note that formatNewPerson-examples.R provides working code on how to utilize the formatNewPerson() function.

Everything in the `./app` directory is deployed to the server. The main file is `app.R` which holds the UI and server. The other files and sub-directories in the `./app` are detailed below. `app.R` itself is well commented and has a strict tree structure to aid in development.

- `google-analytics.html`: connects the app to Google Analytics

- `LICENSE-text.html`: for displaying the contents of the LICENSE.

- `mylynch-app-XXXXXX.json`: this provides the "secrets" needed for the app to connect to the `hereditarycancer@ds.dfci.harvard.edu` account. This must be deployed to the server for it to work.

- `non.pp.cancer.list.csv`: a list of non-Fam3PRO cancers from the Dana-Farber website.

- `data-dictionary`: a starter directory which hold files for when a user downloads the data dictionary. Additional files are added upon download and then auto-deleted because they are specific to the user's session/download. A README is included which explains each permanent and temporary file in the directory.

- `download-pedigrees`: a starter directory which hold files for when a user downloads one or more pedigrees. Additional files are added upon download and then auto-deleted because they are specific to the user's session/download. A README is included which explains each permanent and temporary file in the directory.

- `download-results`: a starter directory which hold files for when a user downloads the Fam3PRO results for a specific pedigree. Additional files are added upon download and then auto-deleted because they are specific to the user's session/download. A README is included which explains each permanent and temporary file in the directory.

- `rsconnect`: holds the .dfc files for deploying the app to R Connect. There are three, one for each app name as explained in the "Branches and deployment" section of this README.

- `www`: contains all image files, a css style sheet, and all pedigreejs javascript code for the app.

- `www/pedigreejs`: contains the following pedigreejs files:

  + `d3.min.js`: a requirement of pedigreejs, assists in building the tree. No modification necessary, stricly a dependency.
  
  + `Html.html`: the code tells pedigreejs where to display the Tree and it related buttons (ie zoom, download).
  
  + `JS.js`: this is the most important file. This is what connects the app to pedigreejs. It tells pedigreejs the settings you want like which cancers to include, window size information, and more. It also handles the pedigree data transfer to/from R and javascript. There are three functions:
  
    + removeNull: this takes the JSON of pedigree data provided by the R server and removes all NA/NULL values which pedigreejs cannot process.
	
	+ createPedJSHandler: creates the initial pedigreejs tree during a user session. This function contains an important interval function called `getpedigree` which is used by R to constantly scan for updates to the tree structure so that the R version of the pedigree can be updated. 
	
	+ updatePedJSHandler: all subsequent updates to the pedigreejs tree. Even if the user loads a new pedigree in the same session in which createPedJSHandler has been called, updatePedJSHandler will still be used to create the new tree. 
	
  + `pedigreejs-customizations.md`: a file that explains what customizations were made to get pedigreejs to work with F3PI. Required as per the pedigreejs license.
  
  + `build/pedigreejs.v2.1.0-rc9-customized-for-F3PI-3.js`: the pedigreejs code used in the app with slight modification as described in the above file. 
  
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
  
  + `pp-utils.R`: utilities for running Fam3PRO and visualizing its results.
  
  + `surg-utils.R`: utilities for managing surgical history.
  
## Current Status of the App

The app is fully functional in its current state with all major features implemented. A history of features can be found in `task-list.xlsx` and that file also contains ideas for future improvements ranked by priority. All very high and high priority tasks were completed.

Historically, the slow connection between HeidiSQL and R Connect was causing the app the crash when loading, copying, deleting, or downloading pedigrees (although modifying a pedigree did not seem to be affected). This is a common problem for shiny apps in general. The problem was exacerbated for manager accounts because manager account first query one of their subordinate tables then a specific pedigree from that table is extracted. I pushed a fix for this on 5/3/23 that forces users to wait until the query operations finish before the user can proceed. I have not received a complaint since then. 

Ideally, users could be able to upload their own pedigrees in a variety of formats however at the moment they must create a pedigree from scratch using F3PI. I started coding the function checkUploadPed in the ped-utils.R script to address this however, the feature has not yet been implemented. 

Another very useful feature which has not been implemented yet, would be to add nucleotide and variant coding validation from ClinVar via an API.

There is also a way to modify pedigreejs to show P/LP genes by relative however, this is somewhat complicated and requires more modification of the pedigreejs javascript code to be able to handle all of different Fam3PRO genes. 

There are two potential improvements related to pedigreejs:

1. Note that when the user downloads a pedigree, only the kinship2 image is included, not the pedigreejs image. This was due to the difficulty in getting pedigreejs to work with the download feature but I'm sure there is a work around if given more time. The user can still download the image of the pedigreejs tree though using the image download button directly below the pedigreejs tree window.

2. The way in which R is continually scanning for changes to the pedigreejs JSON every 1 second is inefficient and, in the past, has caused the tree display to flicker every 1 second (although I partially fixed this). A better way would be for pedigreejs to inform the R server a relative has been added or deleted however this requires modification of the javascript code.

Loading testing would be great to include if this app is to be published. I began to load test the app using the directory `Fam3PRO_ShinyApp\Fam3PRO_app_load_test`. The shinyloadtest package unfortunately is not compatible with apps that rely on databases and some of our other complex features like pedigreejs. Instead, I created a test version of the app that only runs the Fam3PRO analysis (the most computationally demanding feature). Although not representative of using the real F3PI app, it did provide insight into how many simultaneous users could run Fam3PRO at the same time using our R Connect server. The answer is around 12 before performance beings to degrade. This is a hardware limitation and we would need to upgrade our server to improve performance. Once the server hardware is optimized and if we still want more capacity we would have to set-up a cluster of R Connect servers to handle increased workload.
