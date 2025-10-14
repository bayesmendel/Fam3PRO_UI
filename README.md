# F3PI
FamePRO Interface (F3PI) is a user interface for Fam3PRO developed in R Shiny. F3PI interactively builds and visualizes pedigrees, customizes and runs the Fam3PRO model, and displays the model results. Users can save pedigrees to their user account for future retrieval, modification, and analysis.

[Access the site](https://hereditarycancer.dfci.harvard.edu/fam3pro/)

## User Accounts

By default, all new user accounts have "standard" permissions which means they cannot access pedigrees from other user accounts.
Users, including those with manager level permissions, can give any manager level user account access to the pedigrees they create by specifying those manager level user accounts as managers of the user. This can either be done either when the user creates their account or navigating to their user account settings after logging in.

## Managing the working pedigree (PED) and pedigreejs

Adding and removing relatives from a pedigree is easier using an interactive drawn family tree than it is using a 2D table. Editing the pedigree via an interactive drawn tree prevents the user from having to worry about breaking family links and manually managing mother/father IDs and sexes. [pedigreejs](https://ccge-boadicea.github.io/pedigreejs/) is a javascript web tool which accomplishes this task however, R is more flexible than pedigreejs and allows for all the detailed information Fam3PRO needs to be stored in a strictly formatted data frame. R {shiny} also provides a much more user friendly and visually appealing UI for data entry that pedigreejs. The following approach was used to integrate and leverage the strengths of both pedigreejs, R, and R {shiny}.

Terminology note: the currently loaded pedigree which the user can modify and analyze is referred to as the "working pedigree." This is the reactive value object named `PED()` and it takes the form of an R data frame. The working pedigree is the centerpiece of the app which all other features revolve around. When the user is ready to run Fam3PRO, the working pedigree (`PED()`) is passed to the `pedigree` argument of the `Fam3PRO()` function.

When a user initializes a new pedigree they are asked to enter a pedigree name, proband sex and proband age. Once this happens, `PED()` is initialized as a 3 person pedigree consisting of the proband, the mother, and the father. This initial working pedigree contains blank or default values for each column in `PED()` except for the ID columns and the Sex column. The proband's age is also populated. Next, the user will work their way through entering the data for the proband. This is accomplished by navigating through six different tabs: Demographics, Cancer Hx, CBC Risk, Tumor Markers, Surgical Hx, and Genes. Each time the user advances to the next tab, the data from the previous tab is used to update the working pedigree. 

Once the proband information has been entered, the user comes to a seventh tab named "Add Relatives". Here the user enters quantities by relative type for siblings, children, aunts, and uncles. Once this information is entered a row for each of these relatives is added to the working pedigree with just the ID, relationship name, implied mother/father ID, and implied sex. If any maternal/paternal aunts or uncles were specified, then a corresponding set of maternal/paternal grandparents are created automatically. Next, a JSON copy of the working pedigree is created with just the bare minimum information pedigreejs needs to draw the pedigree and show the cancer history. This JSON includes ID, mother/father ID, age, deceased status, sex, and cancer history information only. This JSON is then passed to the `createPedJSHandler` function which calls on pedigreejs to draw an interactive family tree. 

At this point, the user must use the pedigreejs interactive tree to add and remove any family members. This functionality also allows for the addition of more distant relatives such as cousins, grandchildren, etc. While pedigreejs is used to manage the tree structure and add/remove relatives, the R {shiny} inputs and modules are used to modify the data for each relative. The same six tabs used to enter the proband's data are re-used to enter any relative's data. To switch which relative the user is entering data for, they just need to select the new relative from the drop-down along the top of the screen. 

For example, when modifying the race information for the proband's mother, the user first needs to select "Mother" in the relative selector drop-down and then navigate to the Demographics tab. Next, the user updates the race using the drop-down menu. This information is not updated in the working pedigree until one of these 4 events occurs: 1) the user changes the selected relative, 2) the user changes to another pedigree editor tab (ie to Cancer Hx), 3) the user changes to another navbar Tab (ie the Home tab or the Fam3PRO tab), or 4) the user clicks the "Update and Save Pedigree" button. Every time the working pedigree is updated, a snapshot of `PED()` is saved to the user's SQL table in the database and the previous copy is overwritten. Now because race does not affect the pedigree structure and is not displayed in the interactive family tree, pedigreejs is not informed about this change. However, if the user was to update a current age value, deceased status, or any cancer history, this information is displayed in the interactive family tree and therefore a new JSON copy of the working pedigree is created and passed to pedigreejs via `updatePedJSHandler`. `updatePedJSHandler` tells pedigreejs to redraw the tree with the updated information.

The R server is continually querying the pedigreejs pedigree JSON object via the `getpedigree` function every 1 second to detect if the user has added or deleted any relative using the interactive family tree. The server does this by converting the JSON into a data frame using the {jsonlite} package and comparing the number of rows from that data frame to the number of rows in the working pedigree data frame. If a difference in rows is detected, then `PED()` is updated accordingly. Once `PED()` has been updated, another JSON copy of it is created and passed back to `updatePedJSHandler` to redraw the interactive family tree with either the deleted relative removed or with the new relative(s) added with their standardized IDs and names to match the rest of the family. 

## Fam3PRO R package
The R package itself is not publicly displayed in this repo. If you are planning on embedding the Fam3PRO package into your own tool or if you are pursuing commercial software development that incorporates our software please contact us for information on licensing (https://projects.iq.harvard.edu/bayesmendel/our-software). If you would like to collaborate on research projects using the Fam3PRO R package please contact Giovanni Parmigiani or Danielle Braun for more information or visit our lab website.

## App file structure

Everything in the `./app` directory is deployed to the server. The main file is `app.R` which holds the UI and server. The other files and sub-directories in the `./app` are detailed below. `app.R` itself is well commented and has a strict tree structure to aid in development.

- `LICENSE-text.html`: for displaying the contents of the LICENSE.

- `non.pp.cancer.list.csv`: a list of non-Fam3PRO cancers from the Dana-Farber website.

- `data-dictionary`: a starter directory which hold files for when a user downloads the data dictionary. Additional files are added upon download and then auto-deleted because they are specific to the user's session/download. A README is included which explains each permanent and temporary file in the directory.

- `www`: contains all image files, a css style sheet, and all pedigreejs javascript code for the app.

- `www/pedigreejs`: contains the following pedigreejs files:

  + `d3.min.js`: a requirement of pedigreejs, assists in building the tree. No modification necessary, stricly a dependency.
  
  + `Html.html`: the code tells pedigreejs where to display the Tree and it related buttons (ie zoom, download).
  
  + `JS.js`: this is the most important file. This is what connects the app to pedigreejs. It tells pedigreejs the settings you want like which cancers to include, window size information, and more. It also handles the pedigree data transfer to/from R and javascript. There are three functions:
  
    + removeNull: this takes the JSON of pedigree data provided by the R server and removes all NA/NULL values which pedigreejs cannot process.
	
	+ createPedJSHandler: creates the initial pedigreejs tree during a user session. This function contains an important interval function called `getpedigree` which is used by R to constantly scan for updates to the tree structure so that the R version of the pedigree can be updated. 
	
	+ updatePedJSHandler: all subsequent updates to the pedigreejs tree. Even if the user loads a new pedigree in the same session in which createPedJSHandler has been called, updatePedJSHandler will still be used to create the new tree. 
	
  + `pedigreejs-customizations.md`: a file that explains what customizations were made to get pedigreejs to work with F3PI. Required as per the pedigreejs license.
  
  + `build/pedigreejs.v2.1.0-rc9-customized-for-F3PI-4.js`: the pedigreejs code used in the app with slight modification as described in the above file. 
  
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
  
