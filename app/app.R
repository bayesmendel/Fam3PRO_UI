#### Set-up ####
## libraries
# shiny libraries
library(shiny)
library(shinyBS)    # shiny tool tips
library(shinyjs)    # java script tools
library(shinybusy)  # success notifications and waiting on calc spinner

# pedigrees and models
library(kinship2)   # draws pedigrees (this is temporary only)
library(cbcrisk)    # Swati's cbcrisk model
library(PanelPRO)

# database and user accounts
library(DBI)        # read/write tables in database
library(odbc)       # database connections
library(RMariaDB)   # MariaDB
library(sodium)     # password encryption
library(shinyauthr) # user accounts
library(gmailr)     # sends account recovery emails
library(httr)       # authentication for gmail

# data manipulation
library(tidyverse)
library(rlang)
library(jsonlite)   # convert to/from JSON strings to R data frames
library(stringi)    # convert to title case function: stri_trans_totitle()
library(varhandle)  # check if a character or factor can be safely converted to a number

# html
library(htmltools)  # formatting text

# tables
library(DT)         # displays data tables

# download
library(zip)        # create .zip files for download

# plotting
library(grid)
library(gridExtra)  # combine plots as a grob
library(plotly)

# access function doc string
library(gbRd)

#### UI 
ui <- fixedPage(
  
  # Google analytics
  tags$head(includeHTML(("google-analytics.html"))),
  
  # style sheet
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
  
  # allows shinyjs commands
  useShinyjs(),
  
  # pedigreejs dependencies
  # tags$head(tags$meta(charset="UTF-8")),
  # tags$head(tags$meta(content="IE=edge")),
  # tags$head(tags$meta(name="viewport", content="width=device-width,maximum-scale=2")),
  tags$head(tags$link(rel = "stylesheet", type = "text/css",
                      href = "https://cdn.jsdelivr.net/npm/font-awesome@4.7.0/css/font-awesome.min.css", media="all")),
  # tags$head(tags$link(rel = "stylesheet", href = "www/pedigreejs/build/pedigreejs.v2.1.0-rc9.css")),
  tags$head(includeScript(path="www//pedigreejs//d3.min.js")),
  tags$head(includeScript(path="www//pedigreejs//build//pedigreejs.v2.1.0-rc9-customized-for-PPI-2.js")),
  # tags$head(includeScript(path="www//pedigreejs//build//pedigreejs.v2.1.0-rc9.js")),
  
  # server busy spinner
  shinybusy::add_busy_spinner(spin = "fading-circle", position = "full-page"),
  
  # title and log-out button
  titlePanel(
    title = 
      tagList(
        span(
          app.title,
          div(class = "pull-right", 
            img(src="dana-farber-logo-small-2.PNG", height = "50px"),
            shinyauthr::logoutUI(id = "logout", 
                                 style = "color:white; background-color:red; border-color:grey; padding:5px")
          )
        )
      ),
    windowTitle = app.title
  ),
  
  #### UI: Log-in tabs ####
  conditionalPanel("!output.loggedIn",
    tabsetPanel(id = "loginTabs",
      
      ##### UI: Log-in ####
      tabPanel(title = "Log In",
        shinyauthr::loginUI(id = "login",
          cookie_expiry = 0,
          additional_ui = 
            shiny::tagList(fluidRow(br()),
                           fluidRow(
                             actionButton(inputId = "signUp",
                                          label = "Sign up",
                                          style = "color: black; background-color: white; border-color:grey")),
                           fluidRow(br()),
                           fluidRow(
                             actionButton(inputId = "forgotUnPw",
                                          label = "Forgot Username or Password",
                                          style = "color: black; background-color: white; border-color:grey; padding:4px; font-size:80%"))
          )
        )
      ), # end log-in tab
       
      ##### UI: Sign-up ####
      tabPanel(title = "Sign Up",
        
        # bot check passed
        conditionalPanel(condition = "output.botCheck1",
          h3("Create Account"),
          p("Complete the form below to create a user account."),
          
          # user name
          p("Usernames can only contain lower case letters, numbers, and underscores (_)."),
          textInput(inputId = "newUsername",
                    label = "New Username",
                    placeholder = "Enter a new username"),
          
          # add managers, optionally
          p("Optionally, you can add one or more managers who will be able to view and 
            download any pedigrees you save to your user account. This is useful for 
            studies that have multiple providers enrolling patients into the same study. 
            Your manager(s) must have already created their own user account(s) with 'manager' 
            level permissions."),
          selectizeInput("selManagers1", label = "Enter managers' usernames:",
                         selected = "",
                         choices = "",
                         multiple = TRUE,
                         options = list(create=TRUE)),
          
          # email address
          textInput(inputId = "newEmail",
                    label = "Email Address",
                    placeholder = "Enter your email address"),
          textInput(inputId = "newEmail2",
                    label = "Re-enter your email address",
                    placeholder = "Re-enter your email address"),
           
          # set password
          p("Passwords must be at least 8 character in length and have one of each: 
            lowercase letter, upper case letter, number, symbol."),
          passwordInput(inputId = "newPassword",
                        label = "New Password",
                        placeholder = "Enter a new password"),
          passwordInput(inputId = "newPassword2",
                        label = "Re-enter New Password",
                        placeholder = "Re-enter new password"),
          conditionalPanel(condition = "output.isCredsError",
            h5(textOutput("CredsError"), style = "color: red")
          ),
          
          # submit
          actionButton(inputId = "createNewUser",
                       label = "Submit")
        )
      ), # end sign-up tab

      ##### UI: Forgot Username or Password ####
      tabPanel(title = "Forgot Username or Password",
              
        # bot check passed
        conditionalPanel(condition = "output.botCheck1",
          h3("Account Recovery"),
          p("To recover your username or password, enter your email address below, 
            select which credentials you forgot, then hit submit."),
          textInput(inputId = "recoveryEmail",
                    label = "Email Address",
                    placeholder = "Enter your email address"),
          conditionalPanel(condition = "input.forgotGo",
            actionButton(inputId = "diffEmail",
                         label = "Try a Different Email")               
          ),
          radioButtons(inputId = "forgotWhich",
                       label = "Select which credentials you need to recover:",
                       choices = list("username", "password", "both")),
          actionButton(inputId = "forgotGo",
                       label = "Submit"),
          conditionalPanel(condition = "output.showForgotResponse",
            uiOutput("forgotResponse"),
            conditionalPanel(condition = "output.showRecoveryCodeResponse",
              uiOutput("recoveryCodeResponse"),
              conditionalPanel(condition = "output.showResetPwResponse",
                               uiOutput("resetPassError")
              )
            )
          )
        )
      ), # end forgot username/password tab
      
      ##### UI: Bot Check ####
      tabPanel(title = "Bot Check",
        uiOutput("botCheckUI")
      )
    ) # end of tabsetPanel for log-in tabs
  ), # end of conditionalPanel for logged out status
  
  #### UI: Post Log-in ####
  conditionalPanel("output.loggedIn",
    navbarPage(title = "", id = "navbarTabs",
      
      ##### UI: Home ####
      tabPanel(title = "Home",
        h3("Home"),
        h4("What are PanelPRO and PPI?"),
        p("PanelPRO, created by the BayesMendel Lab at Dana-Farber Cancer Institute,
          is a multi-cancer/multi-gene risk prediction model which utilizes family history
          to estimate the probability that a patient has a pathogenic or likely
          pathogenic variant (P/LP) gene variant on up to 24 different cancer suseptibility genes and,
          estimates a patient's future risk of cancer for up to 18 different cancer types.
          PanelPRO also includes the BRCApro, MMRpro, and MelaPRO risk models and users
          have the ability to create customized models focused on specific cancers and genes.
          The PanelPRO software package was written in the statistical
          programming language R. You can learn more about PanelPRO at the ",
          a("BayesMendel lab's website.", href = "https://projects.iq.harvard.edu/bayesmendel/about")),
        tags$a(tags$img(src = "bm-lab-logo.PNG", height = "150px"), 
               href = "https://projects.iq.harvard.edu/bayesmendel/about"),
        br(),br(),
        p("This website is named the PanelPRO Interface (PPI) because it allows clinicians and researchers to 
          utilize PanelPRO without having to use R or know how write code in R. 
          PPI allows users to create pedigrees formatted for PanelPRO and then run 
          the PanelPRO model, or one of its sub-models, on those pedigrees to obtain 
          carrier probabilities and future cancer risk estimates for their patients or study samples. 
          Users can also save pedigrees they created on this site to their account which can be 
          viewed, updated, downloaded, and (re)analyzed by PanelPRO."),
        br(),
        
        h4("Beta Version"),
        p("This website is a beta version that is still in development and we are working towards adding more 
          features. If you run into bugs or have suggestions to improve PPI, please contact us using the email address 
          at the bottom of the page."),
        br(),
        
        h4("How to Use PPI"),
        p("First, navigate to the 'Manage Pedigrees' tab at the top of the page where you can 
          create a new pedigree, load a pedigree you created previously on this site, or 
          download one or more pedigrees in .csv or .rds format."),
        p("To run PanelPRO, you will first either need to create a new pedigree or load an 
          existing one which you previously saved to your account. Once you do either of these 
          actions you will have access to the 'Create/Modify Pedigree' and the 'PanelPRO' pages."),
        p("When creating a new pedigree, see the privacy section below for rules on naming your pedigrees."),
        p("Once you create a new pedigree and everytime you modify that pedigree, it will automatically save 
          to your user account and you can come back at any time to modify your saved pedigrees 
          and run/re-run them through PanelPRO."),
        br(),
        
        h4("Privacy"),
        p("This website is not authorized to store any identifiable patient data including, but not limited to, 
          patient names (including parts of names or initials), ages above 89, medical record numbers (MRNs), 
          months and days of birth, diagnoses, surgeries, enrollment, or other dates (years are okay), location, contact information, or 
          insurance information. When creating names for your pedigrees, do not use any of these identifiers 
          in whole or in part. The identifiers used to differentiate relatives in each pedigree created using PPI 
          will use generic labels like 'Proband' or 'Sister 1'."),
        p("Website admins have access to all pedigrees saved to all user accounts and if you assigned other users 
          as managers for your account, those accounts can also access all of your saved pedigrees. Admins nor managers 
          have access to your password, however."),
        p("Users can delete any or all pedigrees saved to their account at any time by navigating to 
          the 'Manage Pedigrees' page and then navigating to the 'Delete' tab on that page. Users with manager 
          level permissions may also do this for any user account for which they manage."),
        p("This website uses an https encryption protocol to protect data that is transmitted between our servers 
          and our users and vice versa. This website also uses Google Analytics to help manage compute resources. We have configured 
          Google Analytics to maximize patient and user privacy however, it still records the location 
          of users at the country level and the date and time of access."),
        br(),
        
        h4("Support and Contact Information"),
        p("Please direct all questions to your study or site PI."),
        p("If you find any bugs, have suggestions to improve the website, or are interested in 
          working with us, please email us at: hereditarycancer AT ds.dfci.harvard DOT edu"),
        br(),
        
        h4("Terms and Conditions"),
        div(style = "margin-left:5px",
          actionButton("terms", label = "Terms and Conditions",
                       icon = icon('book'),
                       style = "margin-top:5px")
        )
      ), # end of tab
      
      ##### UI: Manage Pedigrees ####
      tabPanel("Manage Pedigrees",
        h3("Manage My Pedigrees"),
        tabsetPanel(id = "managePedsTab",
                    
          ###### UI: Create or Load Pedigree ####
          tabPanel("Create or Load",
            h4("Create New or Load Existing Pedigree"),
            p(strong("Working pedigree: "), 
              textOutput("currentPed1", inline = T), 
              style = "font-size:17px"),
            p("To get started, you will either need to create a new pedigree using our 
              pedigree builder or load an existing pedigree from your user account."),
            radioButtons("newOrLoad", "Select a start-up option:",
                         choices = c("Create new", "Load existing"),
                         selected = "Create new"),
            
            # if the user wants to load an existing table
            conditionalPanel("input.newOrLoad == 'Load existing'",
              
              # for admins and managers, select the user account to load from first
              conditionalPanel("output.admin | output.manager",
                selectInput(inputId = "selectUser", label = "Select a user account:", 
                            choices = c(""))
              ),
               
              # if there are not pedigrees to load, tell the user
              conditionalPanel(condition = "output.showTblExistsError",
                p("You do not have any saved pedigrees.", style = "color: red;")
              ),
               
              # if there are pedigrees to load, provide a dropdown
              conditionalPanel(condition = "!output.showTblExistsError",
                selectInput("existingPed", "Select a pedigree to load:",
                            choices = "No pedigree selected")
              )
            ),
            
            # show action button only if the user is not trying to load a table that does not exist
            conditionalPanel("(input.newOrLoad == 'Load existing' & !output.showTblExistsError) | input.newOrLoad == 'Create new'",
              conditionalPanel("input.newOrLoad == 'Create new'",
                p("Clicking the button below will take you to the create/edit pedigree screen where you can start a new pedigree."),
              ),
              conditionalPanel("input.newOrLoad == 'Load existing'",
                p("Clicking the button below will load your selected pedigree and will take you to the create/edit pedigree screen to view and/or edit your pedigree."),
              ),
              textOutput("waitLoad"),
              actionButton("goNewOrLoad", label = "Get Started",
                           icon = icon('play'))
            )
          ), # end of tab for loading/creating new pedigree
          
          ###### UI: Preview Pedigree ####
          tabPanel("Preview",
            h4("Preview Current Pedigree"),
            p(strong("Working pedigree: "), 
              textOutput("currentPed2", inline = T), 
              style = "font-size:17px"),
            p("Below you can choose between the tree and table views of the pedigree which is currently loaded."),
            tabsetPanel(id = "pedVisualsViewer",
                        
              # tree
              tabPanel(title = "Tree",
                plotOutput("treePedViewer", width = "750px"),
                h5("Note: the image above will only display a maximum of 4 cancer types.")
              ),
              
              # table
              tabPanel(title = "Table",
                br(),
                DTOutput("tablePedViewer")
              ),
              
              # cancer hx
              tabPanel(title = "Cancers",
                br(),
                DTOutput("cancersTblViewer", width = "75%")
              ),
              
              # genes
              tabPanel(title = "Genes",
                br(),
                DTOutput("genesTblViewer")
              )
            ), # end of tabsetPanel 
            
            # data dictionary available for download
            conditionalPanel("input.pedVisualsViewer != 'Tree'",
              br(),
              p("If you are unfamiliar with PanelPRO formatted pedigree tables, download the data dictionary below to assist you."),
              downloadButton("downloadDD1", label = "Download Data Dictionary",
                             icon = icon('download'))
            )
          ),
          
          ###### UI: Copy Pedigree ####
          tabPanel("Copy",
            h4("Copy A Pedigree"),
            p("This screen allows you to take a pedigree saved to your user account and 
              create a copy of it under a new name."),
            
            # for admins and managers, select the user account to copy from first
            conditionalPanel("output.admin | output.manager",
              selectInput(inputId = "selectUserForCopyFrom", label = "Select a user account to copy from:", 
                          choices = c(""))
            ),
            
            # if there are not pedigrees to copy from the selected user account, tell the user
            conditionalPanel(condition = "output.showTblExistsErrorCopy",
              p("You do not have any saved pedigrees.")
            ),
            
            # if the user account does have saved pedigrees
            conditionalPanel(condition = "!output.showTblExistsErrorCopy",
              selectInput("selectCopyPed", label = "Select a pedigree to copy:",
                          selected = "No pedigree selected", 
                          choices = "No pedigree selected"),
              conditionalPanel("input.selectCopyPed != 'No pedigree selected'",
                               
                # for admins and managers, select the user account to copy from first
                conditionalPanel("output.admin | output.manager",
                  selectInput(inputId = "selectUserForCopyTo", label = "Select a user account to copy to:", 
                              choices = c(""))
                ),
                textInput("newPedName", label = "New pedigree name:"),
                h5("Privacy note: the new pedigree name cannot contain identifying information.", 
                   style = "color:blue"),
                textOutput("validPedID"),
                textOutput("waitCopy"),
                actionButton("copyPed", label = "Copy Pedigree",
                             icon = icon('copy'),
                             style = "margin-top:20px")
              )
            ) # end of conditionalPanel to check if there are tables to download
          ),
        
          ###### UI: Download Pedigrees ####
          tabPanel("Download",
            h4("Download Pedigrees"),
            p("You can download one or more pedigrees in a single .zip file by selecting them below. 
              The .zip file will include two directories (or folders): 'download-pedigrees' and 'data-dictionary'. 
              'download-pedigrees' will include three files: 1) the selected pedigrees, 
              2) detailed cancer history (extracted from the cancerJSON column in the pedigrees file)
              and 3) detailed panel and gene results (extracted from the genesJSON column in the pedigree file). 
              'data-dictionary' will include 3 more files: 1) a description of the each pedigree column, 
              2) PanelPRO cancer abbreviations and 3) a list of PanelPRO genes."),
            
            # for admins and managers, select the user account to load from first
            conditionalPanel("output.admin | output.manager",
              selectInput(inputId = "selectUserForDownload", label = "Select a user account:", 
                          choices = c(""))
            ),
            
            # if there are not pedgirees to load from the selected user account, tell the user
            conditionalPanel(condition = "output.showTblExistsErrorDownloads",
              p("You do not have any saved pedigrees.")
            ),
            
            # if the user account does have saved pedigrees
            conditionalPanel(condition = "!output.showTblExistsErrorDownloads",
              selectInput("selectDownloadPeds", label = "Select which pedigrees to download:",
                          choices = "", 
                          multiple = T),
              div(checkboxInput("selectAllPeds", label = "Select all pedigrees"), style = "margin-left:25px;margin-top:-10px"),
              radioButtons("downloadAs1", label = "Select a file format for download:",
                           choices = c(".csv", ".rds"),
                           selected = ".csv"),
              textOutput("waitDownload"),
                               
              # CSV download button
              conditionalPanel("input.downloadAs1 == '.csv'",
                downloadButton("downloadPedsCSV", label = "Download",
                               icon = icon('download'))
              ),
              
              # RDS download button
              conditionalPanel("input.downloadAs1 == '.rds'",
                downloadButton("downloadPedsRDS", label = "Download",
                               icon = icon('download'))
              )
            ) # end of conditionalPanel to check if there are tables to download
          ), # end of download tab
          
          ###### UI: Delete Pedigrees ####
          tabPanel("Delete",
            h4("Delete Pedigrees"),
            p("Note: once a pedigree is deleted, you cannot undo it."),
            
            # for admins and managers, select the user account to delete from first
            conditionalPanel("output.admin | output.manager",
              selectInput(inputId = "selectUserForDelete", label = "Select a user account:", 
                          choices = c(""))
            ),
            
            # if there are not pedigrees to delete from the selected user account, tell the user
            conditionalPanel(condition = "output.showTblExistsErrorDelete",
              p("You do not have any saved pedigrees.")
            ),
            
            # if the user account does have saved pedigrees
            conditionalPanel(condition = "!output.showTblExistsErrorDelete",
              selectInput("selectDeletePeds", label = "Select which pedigrees to delete:",
                          choices = "", 
                          multiple = T),
              div(style = "margin-left:25px;margin-top:-10px",
                  checkboxInput("selectAllPedsDelete", label = "Select all pedigrees"), 
              ),
              textOutput("waitDelete"),
              actionButton("deletePeds", label = "Delete Pedigrees",
                           icon = icon('trash-o', verify_fa = FALSE),
                           style = "color: white; background-color: red; border-color: grey")
            ) # end of conditionalPanel to check if there are tables to download
          ) # end of tabPanel for deleting pedigrees
        ) # end of tabsetPanels for manage pedigrees
      ), # end of tab for manage pedigrees tab
      
      ##### UI: Create/Modify Pedigree ####
      tabPanel("Create/Modify Pedigree",
        
        # create 2 columns, one for displaying the pedigree (left) and one for data entry (right)
        fluidRow(
          
          # manual save button
          conditionalPanel("output.pedExists",
            div(class = "pull-right", 
              actionButton("manualPedSave", label = "Update and Save Pedigree",
                           icon = icon('save'),
                           style = "margin-top: 0px; margin-bottom: 25px")
            )
          ),
          
          ###### UI: Visualize Pedgiree ####
          # only show pedigree visualization after pedigree has been initialized with all FDR, aunts, and uncles
          conditionalPanel("output.showPed",
            column(width = 6,
                   
              # top row to select which relative is being edited
              fluidRow(
                column(width = 5,
                  div(class = "pull-right",
                    h4("Select a relative to edit:", style = "margin-right:-15px")
                  )
                ),
                column(width = 7,
                  selectInput("relSelect", label = NULL,
                              choices = setNames(c(1), "Proband"), # placeholder, this will be updated once FDR+AU ped initialized
                              width = "300px")
                )
              ),
              
              # pedigree visualization (table and tree)
              tabsetPanel(id = "pedVisualsEditor",
                          
                # pedigreeJS tree
                tabPanel(title = "Tree",
                  br(),
                  bootstrapPage(
                    includeScript(path="www/pedigreejs/JS.js"),
                    includeHTML(path="www/pedigreejs/Html.html")
                  ),
                  h5("Note: the tree above can only display PanelPRO cancers. 
                     It will display any unknown cancer ages as 0."),
                  
                  # create a hidden button to retrieve pedigreeJS pedigree in JSON format
                  # the server needs to simulate a click on this buttons at the 
                  # set interval to sync the master pedigree with pedigreeJS and 
                  # vice-versa
                  br(), br(), br(), br(),
                  tags$a(href="#", onclick = "getpedigree()",
                         id = "GetPedJSButton",
                         class="btn btn-default action-button shiny-bound-input",
                         style="text-align:center;background-color:white;border-color:white;", 
                         label = "")
                  
                  
                  # # FOR TESTING
                  # textOutput("pedJSJSON")
                  
                  
                  
                ),
                          
                # table
                tabPanel(title = "Table",
                  fluidRow(
                    column(width = 12, style = "height:800px; width:100%",
                      br(),
                      DTOutput("tablePedEditor")
                    )
                  )
                ),
                
                # cancer hx
                tabPanel(title = "Cancers",
                  fluidRow(
                    column(width = 8, style = "height:800px; width:100%",
                      br(),
                      DTOutput("cancersTblEditor")
                    )
                  )
                ),
                
                # genes
                tabPanel(title = "Genes",
                  fluidRow(
                    column(width = 10, style = "height:800px; width:100%",
                      br(),
                      DTOutput("genesTblEditor")
                    )
                  )
                )
              ), # end of tabsetPanel for choosing visualization
              
              # download data dictionary
              conditionalPanel("input.pedVisualsEditor != 'Tree' & input.pedVisualsEditor != 'Interactive'",
                fluidRow(
                  p("If you are unfamiliar with PanelPRO formatted pedigree tables, 
                    download the data dictionary below to assist you."),
                  downloadButton("downloadDD2", label = "Download Data Dictionary",
                                 icon = icon('download'))
                )
              )
            ) # end of column for pedigree visualization
          ), # end of conditionalPanel to display pedigree visualization or not
          
          # column for pedigree data entry
          column(width = 6,
            tabsetPanel(id = "pedTabs", 
              
              ###### UI: Demographics ####
              tabPanel("Demographics",
                h3(textOutput("rop1", inline = T), "Demographics"),
                p("Enter the ", textOutput("rop2", inline = T), "demographic information below."),
                
                # PedigreeID
                textInput("pedID", label = h5("Unique Pedigree ID:"),
                          value = "",
                          width = "225px"),
                conditionalPanel("!output.showPed",
                  h5("Privacy note: the ID number cannot contain identifying information.",
                     style = "color:blue")
                ),
                conditionalPanel("output.nonUniqPedID",
                  h5("Your account already has a pedigree with this ID number, 
                     choose another name or delete the existing pedigree first.",
                     style = "color:red")
                ),
                
                # sex, deceased status, age
                selectInput("Sex", label = h5("Sex assigned at birth:"),
                            choices = sex.choices,
                            width = "150px"),
                h5("Check here if deceased:"),
                div(style = "margin-left:25px",
                    checkboxInput("isDead", label = "Deceased")
                ),
                conditionalPanel("input.isDead",
                  h5("Age of Death (1 to 89):")
                ),
                conditionalPanel("!input.isDead",
                  h5("Current Age (1 to 89):")
                ),
                numericInput("Age",
                             label = NULL,
                             value = NA, min = min.age, max = max.age, step = 1,
                             width = "150px"),
                textOutput("validAge"),
                
                # is pedigree does not exist yet, show a create pedigree button
                conditionalPanel("!output.pedExists",
                  actionButton("createPed", label = "Create Pedigree",
                               icon = icon('file'),
                               style = "margin-top:20px")
                ),
                
                # once a pedigree exists, show other demographic options
                conditionalPanel("output.pedExists",
                  
                  # race, ethnicity, and ancestry inputs
                  selectInput("race", label = h5("Race:"),
                              choices = rc.choices,
                              selected = "Other or Unreported",
                              width = "45%"),
                  selectInput("eth", label = h5("Hispanic Ethnicity:"),
                              choices = et.choices,
                              selected = "Other or Unreported",
                              width = "45%"),
                  h5("Ancestry (check all that apply):"),
                  div(style = "margin-left:25px",
                    checkboxInput("ancAJ", label = "Ashkenazi Jewish"),
                    checkboxInput("ancIt", label = "Italian")
                  )
                )
              ), # end of demographics tab
              
              ###### UI: Cancer Hx ####
              tabPanel("Cancer Hx",
                h3(textOutput("rop3", inline = T), "Cancer History"),
                p("List all first primary cancers ", textOutput("rop4", inline = T), 
                  "has or had with the age of diagnosis."),
                
                # cancerUI modules inserted into this container
                tags$div(
                  id = "canContainer",
                  style = "width:100%"
                ),
                actionButton("addCan", label = "Add Cancer",
                             icon = icon('plus'),
                             style = "margin-top: 20px")
              ), # end of cancers tab
              
              ###### UI: CBC Risk ####
              tabPanel("CBC Risk",
                h3(textOutput("rop5", inline = T), "Contralateral Breast Cancer Risk"),
                conditionalPanel("!output.showCBCinputs",
                  p("This tab is only used when ", textOutput("rop6", inline = T), 
                    "is a female and has/had breast cancer but, has not developed contralateral breast cancer.")
                ),
                conditionalPanel("output.showCBCinputs",
                  h5("Was the 1st breast cancer pure invasive, mixed invasive and DCIS, or unknown?", 
                     style = "margin-top:15px"),
                  selectInput("FirstBCType", label = NULL,
                              choices = bc1type.choices,
                              width = "200px"),
                  h5("Was the 1st breast cancer treated with anti-estrogen therapy?"),
                  selectInput("AntiEstrogen", label = NULL,
                              choices = antiest.hrpre.choices,
                              width = "125px"),
                  h5("Is there a personal history of high risk pre-neoplasia (ie atypical hyperplasia or LCIS?)"),
                  selectInput("HRPreneoplasia", label = NULL,
                              choices = antiest.hrpre.choices,
                              width = "125px"),
                  h5("What were the BI-RADS breast density results?"),
                  selectInput("BreastDensity", label = NULL,
                              choices = bdens.choices,
                              width = "325px"),
                  h5("What was the size of the 1st breast tumor?"),
                  selectInput("FirstBCTumorSize", label = NULL,
                              choices = bctsize.choices,
                              width = "125px")
                )
              ),
              
              ###### UI: Tumor Markers ####
              tabPanel("Tumor Markers",
                h3(textOutput("rop7", inline = T), "Tumor Markers"),
                conditionalPanel("!output.showBCMarkers & !output.showCRCMarkers",
                  p("Tumor markers are only applicable if ", textOutput("rop8", inline = T), 
                    "has/had breast cancer or colorectal cancer.")
                ),
                conditionalPanel("output.showBCMarkers | output.showCRCMarkers",
                  p("If ", textOutput("rop9", inline = T), 
                    "was tested for any of the tumor markers related to the cancers below, report the results."),
                  conditionalPanel("output.showBCMarkers",
                    h4("Breast Cancer Tumor Markers"),
                    fluidRow(
                      column(width = 2, h5("ER:", style = "margin-left:25px")),
                      column(width = 4,
                        selectInput("ER", label = NULL, 
                                    choices = marker.result.choices,
                                    width = "150px")
                      )
                    ),
                    fluidRow(
                      column(width = 2, h5("PR:", style = "margin-left:25px")),
                      column(width = 4,
                             selectInput("PR", label = NULL, 
                                         choices = marker.result.choices,
                                         width = "150px")
                      )
                    ),
                    fluidRow(
                      column(width = 2, h5("HER2:", style = "margin-left:25px")),
                      column(width = 4,
                             selectInput("HER2", label = NULL, 
                                         choices = marker.result.choices,
                                         width = "150px")
                      )
                    ),
                    fluidRow(
                      column(width = 2, h5("CK5.6:", style = "margin-left:25px")),
                      column(width = 4,
                             selectInput("CK56", label = NULL, 
                                         choices = marker.result.choices,
                                         width = "150px")
                      )
                    ),
                    fluidRow(
                      column(width = 2, h5("CK14:", style = "margin-left:25px")),
                      column(width = 4,
                             selectInput("CK14", label = NULL, 
                                         choices = marker.result.choices,
                                         width = "150px")
                      )
                    )
                  ), # end of conditionalPanel for BC tumor markers
                  conditionalPanel("output.showCRCMarkers",
                    h4("Colorectal Cancer Tumor Marker"),
                    fluidRow(
                      column(width = 2, h5("MSI:", style = "margin-left:25px")),
                      column(width = 4,
                             selectInput("MSI", label = NULL, 
                                         choices = marker.result.choices,
                                         width = "150px")
                      )
                    )
                  ) # end of conditionalPanel to CRC tumor markers
                ) # end of conditionalPanel to display any tumor markers
              ), # end of tumor marker tab
              
              ###### UI: Surgical Hx ####
              tabPanel("Surgical Hx",
                h3(textOutput("rop10", inline = T), "Prophylactic Surgical History"),
                
                # message for proph surgery is Female sex is not selected
                conditionalPanel("input.Sex != 'Female'",
                  h5("Prophylactic surgery information is only required for females.")
                ),
                 
                # for females
                conditionalPanel("input.Sex == 'Female'",
                  p("Check each surgery ", textOutput("rop11", inline = T), 
                    "has had and enter the age at surgery."),
                    
                  # mastecomties
                  fluidRow(
                    column(width = 6, 
                      checkboxInput("Mast", label = "Bilateral Mastectomy",
                                    width = "150px")
                    ),
                    column(width = 6, 
                      conditionalPanel("input.Mast",
                        div(style = "margin-left:-75px",
                          numericInput("MastAge",
                                       label = h5("Age at Mastectomy:"),
                                       value = NA, min = min.age, max = max.age, step = 1,
                                       width = "150px"),
                          textOutput("validMastAge")
                        )
                      )
                    )
                  ),
                    
                  # hysterectomies
                  fluidRow(
                    column(width = 6, 
                      checkboxInput("Hyst", label = "Hysterectomy",
                                    width = "150px")
                    ),
                    column(width = 6, 
                      conditionalPanel("input.Hyst",
                        div(style = "margin-left:-75px",
                          numericInput("HystAge",
                                       label = h5("Age at Hysterectomy:"),
                                       value = NA, min = min.age, max = max.age, step = 1,
                                       width = "150px"),
                          textOutput("validHystAge")
                        )
                      )
                    )
                  ),
                    
                  # oophorectomies
                  fluidRow(
                    column(width = 6, 
                      checkboxInput("Ooph", label = "Bilateral Oophorectomy",
                                    width = "250px")
                    ),
                    column(width = 6, 
                      conditionalPanel("input.Ooph",
                        div(style = "margin-left:-75px",
                          numericInput("OophAge",
                                       label = h5("Age at Oophorectomy:"),
                                       value = NA, min = min.age, max = max.age, step = 1,
                                       width = "150px"),
                          textOutput("validOophAge")
                        )
                      )
                    )
                  )
                ) # end of female conditionalPanel for surgical history information
              ), # end of surgery tab
              
              ###### UI: Genes ####
              tabPanel("Genes",
                h3(textOutput("rop12", inline = T), "Gene Testing Results"),
                tabsetPanel(id = "geneTabs",
                  
                  tabPanel(title = "Instructions",
                    h4("How to Enter/Edit Germline Genetic Test Results"),
                    p("Use this screen to enter or modify germline genetic test results. 
                      There are three tabs:"),
                    tags$ol(
                      tags$li("'Manage Panels' allows you to add a multi-gene panels or single 
                              gene tests as well as delete one of the existing tests."),
                      tags$li("'Edit Panel Results' allows you to edit gene test 
                              results (if there is at least one panel)."),
                      tags$li("'Summary Table' displays a summary of results of germline 
                              genetic tests by gene, result, nucleotide, protein, and zygosity.")
                    )
                  ),
                  
                  tabPanel(title = "Manage Panels",
                    fluidRow(column(width = 12, 
                      h4(textOutput("rop14", inline = T), "Current Panel Tests"),
                      tags$div(
                        id = "PanCont",
                        style = "width:100%"
                      ),
                      conditionalPanel("!output.atLeastOnePanel",
                        h5(textOutput("rop15", inline = T), "does not have any gene testing results yet.")
                      ),
                      br(),
                      h4("Add a Panel"),
                      p("Using the dropdown, select one of the pre-existing panels and click 'Add Panel' or select 'Create new' 
                        to make a custom panel of genes."),
                      p("Note, do not add or create a new panel until you have received the panel results back from the lab.", 
                        style = "color:blue"),
                      selectInput("existingPanels", label = NULL,
                                  selected = "No panel selected", 
                                  choices = c("No panel selected", "Create new"),
                                  width = "300px"),
                      actionButton("addPanel", label = "Add Panel",
                                   style = "margin-top: 0px; margin-bottom:15px"),
                      
                      # create new panel
                      conditionalPanel("input.existingPanels == 'Create new'",
                        p("Enter the genes in your panel below.
                          When you start typing, the dropdown will filter to genes for you to select.
                          You can also add genes that are not in the dropdown. When done, give it a name and select the
                          'Create Panel' button. You will still need to add the panel above after its created."),
                        textInput("newPanelName", label = h5("Name the new panel:"), width = "250px"),
                        textOutput("validPanelName"),
                        selectizeInput("newPanelGenes", label = h5("Type or select the genes in this panel:"),
                                       choices = all.genes, 
                                       multiple = TRUE, 
                                       options = list(create = TRUE),
                                       width = "500px"),
                        textOutput("waitNewPanel"),
                        actionButton("createPanel", label = "Create Panel",
                                     style = "margin-top: 0px; margin-bottom:15px")
                      )
                    ))
                  ),
                  
                  tabPanel(title = "Edit Panel Results",
                    fluidRow(column(width = 12, 
                      h4("Edit ", textOutput("rop16", inline = T)," Panel Results"),
                      
                      # cannot enter results if no panels exist
                      conditionalPanel("!output.atLeastOnePanel",
                        p("To edit panel results, please add at least one panel on the 'Manage Panels' tab.")
                      ),
                      
                      # enter results by type
                      conditionalPanel("output.atLeastOnePanel",
                        p("Select one of ", textOutput("rop17", inline = T), 
                          " panels to edit then enter the gene results by selecting the three different tabs for ",
                          HTML("<b>pathogenic/likely pathogenic (P/LP), unknown significance (VUS),</b> or <b>
                          benign/likely benign (B/LP)</b>.")," Enter as much information about each gene variant as possible. 
                          Any genes not specified as P/LP, VUS, or B/LB will be recorded as negative."),
                        p("Example nucleotides: c.279del, c.3262dup, c.1817C>T, c.4987-195_4987-194insTT"),
                        p("Example proteins: p.Gln94fs, p.Val1088fs, p.Pro606Leu", 
                          style = "margin-bottom:20px"),
                        selectInput("editPanel", "Select a panel to edit:",
                                    choices = c("No panel selected"), selected = "No panel selected"),
                        
                        # create a tab for each result type to save space
                        conditionalPanel("input.editPanel != 'No panel selected'",
                          tabsetPanel(id = "GeneResultTabs",
                          
                            # P/LP
                            tabPanel("P/LP",
                              wellPanel(style = "background:MistyRose",
                                h4(HTML("<b>Pathogenic/Likely Pathogenic (P/LP) genes</b>"), style = "color:black"),
                                geneHeaderUI(),
                                
                                # PLP gene modules will be added here
                                tags$div(
                                  id = "PLPCont",
                                  style = "width:100%"
                                ),
                                
                                # add a new PLP gene module to the UI
                                actionButton("addPLP", label = "Add P/LP Gene Variant",
                                             icon = icon('plus'),
                                             style = "margin-top: 10px")
                              )
                            ),
                          
                            # VUS
                            tabPanel("VUS",
                              wellPanel(style = "background:LightGreen",
                                h4(HTML("<b>Variant of Unknown Significance (VUS) genes</b>"), style = "color:black"),
                                geneHeaderUI(),
                                
                                # VUS gene modules will be added here
                                tags$div(
                                  id = "VUSCont",
                                  style = "width:100%"
                                ),
                                
                                # add a new VUS gene module to the UI
                                actionButton("addVUS", label = "Add VUS Gene Variant",
                                             icon = icon('plus'),
                                             style = "margin-top: 10px"),
                              )
                            ),
                          
                            # B/LP
                            tabPanel("B/LB",
                              wellPanel(style = "background:AliceBlue",
                                h4(HTML("<b>Benign/Likely Benign (B/LB) genes</b>"), style = "color:black"),
                                geneHeaderUI(),
                                
                                # BLB gene modules will be added here
                                tags$div(
                                  id = "BLBCont",
                                  style = "width:100%"
                                ),
                                
                                # add a new BLB gene module
                                actionButton("addBLB", label = "Add B/LB Gene Variant",
                                             icon = icon('plus'),
                                             style = "margin-top: 10px")
                              )
                            )
                          ) # end of tabsetPanel for gene results by type
                        ) # end of conditionalPanel for entering gene results by type
                      ) # end of conditionalPanel for checking if a panel to edit has been selected
                    )), # end of column and fluidRow
                  ), # end of tab for gene results entry and panel selection 
                  
                  # tab for review panel results
                  tabPanel(title = "Summary Table",
                    h4(textOutput("rop18", inline = T), "Gene Summary"),
                    conditionalPanel("!output.atLeastOnePanel",
                      h5(textOutput("rop19", inline = T), "does not have any panel tests; therefore, a summary table cannot be displayed.")
                    ),
                    conditionalPanel("output.atLeastOnePanel",
                      h5("The table below is a summary of all the genes in all of panels for ", 
                         textOutput("rop20", inline = T), 
                         "Genes are marked a negative until they are recorded as 
                         P/LP, VUS, or B/LP in any of their panels."),
                      
                      # warn user there is at least one gene with different result types recorded
                      conditionalPanel("output.dupResultGene",
                        h5("There is at least one gene for ", 
                           textOutput("rop21", inline = T), "which has multiple different result types recorded. 
                          This is possible but, it could be an error. Please check the table below for accuracy.
                          If one of these result types is P/LP, PanelPRO will treat this gene as P/LP.", 
                          style = "color:blue")
                      ),
                      
                      # data frame with panel summary information
                      DT::DTOutput("panelSum")
                    )
                  ) # end of tab for gene results summary
                ) # end tabsetPanel for gene results screen
              ), # end of gene results tab
              
              ###### UI: Num/Type Rels ####
              tabPanel("Add Relatives",
                h3("Number and Types of Relatives"),
                p("Begin creating the proband's pedigree by entering the number of each relative type below. 
                  The proband's mother and father will be added to the pedigree automatically. 
                  If aunts or uncles are added on this screen, the proband's grandparents will also be added to the pedigree automatically.
                  Relative types not listed on this screen can be added later on."),
                
                fluidRow(
                  column(width = 6,
                    wellPanel(
                      h4("Children"),
                      numericInput("numDau",
                                   label = h5("Daughters:"),
                                   value = 0,
                                   min = 0,
                                   step = 1, 
                                   width = "125px"),
                      textOutput("validDauQty"),
                      
                      numericInput("numSon",
                                   label = h5("Sons:"),
                                   value = 0,
                                   min = 0,
                                   step = 1, 
                                   width = "125px"),
                      textOutput("validSonQty")
                    ),
                    
                    wellPanel(
                      h4("Siblings"),
                      numericInput("numSis",
                                   label = h5("Sisters:"),
                                   value = 0,
                                   min = 0,
                                   step = 1, 
                                   width = "125px"),
                      textOutput("validSisQty"),
                      
                      numericInput("numBro",
                                   label = h5("Brothers:"),
                                   value = 0,
                                   min = 0,
                                   step = 1, 
                                   width = "125px"),
                      textOutput("validBroQty")
                    )
                  ), # end of column for siblings and children
                  
                  column(width = 6,
                    wellPanel(
                      h4("Maternal Relatives"),
                      numericInput("numMAunt",
                                   label = h5("Maternal Aunts:"),
                                   value = 0,
                                   min = 0,
                                   step = 1, 
                                   width = "125px"),
                      textOutput("validMAuntQty"),
                      
                      numericInput("numMUnc",
                                   label = h5("Maternal Uncles:"),
                                   value = 0,
                                   min = 0,
                                   step = 1, 
                                   width = "125px"),
                      textOutput("validMUncQty")
                    ),
                    
                    wellPanel(
                      h4("Paternal Relatives"),
                      numericInput("numPAunt",
                                   label = h5("Paternal Aunts:"),
                                   value = 0,
                                   min = 0,
                                   step = 1, 
                                   width = "125px"),
                      textOutput("validPAuntQty"),
                      
                      numericInput("numPUnc",
                                   label = h5("Paternal Uncles:"),
                                   value = 0,
                                   min = 0,
                                   step = 1, 
                                   width = "125px"),
                      textOutput("validPUncQty")
                    )
                  ) # end of column for aunts and uncles
                ), # end of fluidRow for the entire num/type rel tab
                
                # button to create visual pedigree
                h4("To Continue"),
                h5("Press the button below to display the proband's pedigree."),
                actionButton("showPedButton", label = "Display Pedigree", icon = icon('tv'))
                
              ) # end of number and type of rels tab
            ) # end of tabsetPanel for data entry
          ) # end of column for data entry
        ) # end of fluidRow for create/modify pedigree tab
      ), # end of navbar tab for create/modify pedigree
      
      ##### UI: PanelPRO ####
      tabPanel("PanelPRO",
        h3("PanelPRO", style = "margin-bottom:0px"),
        
        # PanelPRO version
        div(textOutput("ppVersion"), style = "margin-left:10px"),
        
        # show the current pedigree
        p(strong("Working pedigree: "), 
          textOutput("currentPed3", inline = T), 
          style = "font-size:17px;margin-top:5px"),
        
        # split into settings and results tabs
        tabsetPanel(id = "panelproTabs",
          
          ###### UI: Run / Settings ####
          tabPanel(title = "Run PanelPRO",
            h4("Instructions"),
            p("Specify the model parameters below then hit the 'Run PanelPRO' button at the bottom of the page. For more information on the parameters click on the 
              'Show PanelPRO Documentation' button."),
            
            # show panelpro doc string
            actionButton("showPPDocString1", label = "Show PanelPRO Documentation",
                         icon = icon('book'),
                         style = "margin-bottom:25px"),
            
            # basic settings
            h4("Basic Settings"),
            
            # model_spec
            h5("Model Specification ('model_spec'):"),
            selectInput("modelSpec", label = NULL, 
                        selected = formals(PanelPRO::PanelPRO)$model_spec,
                        choices = c("Custom", names(PanelPRO:::MODELPARAMS)),
                        width = "150px"),
            
            # show genes and cancer for a pre-specified model
            conditionalPanel("input.modelSpec != 'Custom'",
              h5("This model specification includes:", style = "margin-left:15px;margin-top:0px"),
              div(style = "margin-left:25px;margin-top:10px",
                textOutput("modSpecCancers")
              ),
              div(style = "margin-left:25px;margin-top:10px",
                textOutput("modSpecGenes")
              )
            ),
            
            # let user select genes and cancers for a custom model
            conditionalPanel("input.modelSpec == 'Custom'",
              div(style = "margin-left:25px",
                  
                #  genes
                h5("Custom Gene List ('genes'):"),
                selectInput("genes", label = NULL,
                            choices = PanelPRO:::GENE_TYPES,
                            multiple = T,
                            width = "400px"),
                div(style = "margin-left:25px;margin-top:-10px",
                  checkboxInput("allGenes", label = "Select all genes",
                                value = F)
                ),
                
                # cancers
                h5("Custom Cancer List ('cancers'):"),
                selectInput("cancers", label = NULL,
                            choices = setdiff(PanelPRO:::CANCER_NAME_MAP$long, "Contralateral"),
                            multiple = T,
                            width = "400px"),
                div(style = "margin-left:25px;margin-top:-10px",
                  checkboxInput("allCancers", label = "Select all cancers",
                                value = F)
                )
              )
            ),
            
            # max.mut
            h5("Maximum simultaneous mutations allowed per individual ('max.mut'):"),
            selectInput("maxMut", label = NULL,
                        choices = c("1","2","3"),
                        selected = "2",
                        width = "150px"),
            
            # age.by
            h5("Year interval for future cancer risk (1 to 10) ('age.by'):"),
            numericInput("ageBy", label = NULL,
                         min = 1, max = 10, step = 1, 
                         value = formals(PanelPRO::PanelPRO)$age.by,
                         width = "150px"),
            textOutput("validYearInterval"),
            br(),
            
            # advanced settings
            bsCollapse(id = "PanelPROAdvanced",
              bsCollapsePanel(title = "Advanced Settings", 
                              
                # not currently supported (default values used): 
                # proband, remove.miss.cancers, parallel, debug, 
                # impute.missing.ages, database, 
                # plusBCRAT, bcrat.vars, rr.bcrat, rr.pop
                
                # unknown.race
                h5("Assume all unknown races to be ('unknown.race'):"),
                selectInput("unknownRace", label = NULL,
                            selected = PanelPRO:::UNKNOWN_RACE,
                            choices = PanelPRO:::RACE_TYPES,
                            width = "150px"),
                
                # unknown.ancestry
                h5("Assume all unknown ancestries to be ('unknown.ancestry'):"),
                selectInput("unknownAncestry", label = NULL,
                            selected = PanelPRO:::UNKNOWN_ANCESTRY,
                            choices = PanelPRO:::ANCESTRY_TYPES,
                            width = "150px"),
                
                # allow.intervention
                h5("Should cancer prevention risk modifiers be considered ('allow.intervention')?"),
                div(style = "margin-left:25px",
                  checkboxInput("allowInter", label = "Allow Interventions", 
                                value = formals(PanelPRO::PanelPRO)$allow.intervention)
                ),
                
                # ignore.proband.germ
                h5("Should the proband's germline testing results be ignored ('ignore.proband.germ')?"),
                div(style = "margin-left:25px",
                  checkboxInput("ignorePbGerm", label = "Ignore Germline Results", 
                                value = formals(PanelPRO::PanelPRO)$ignore.proband.germ)
                ),
                
                # iterations
                h5("The number of iterations to be run when multiply imputing missing ages (1 to 100; excludes failed iterations; 'iterations'):"),
                numericInput("missAgeIters", label = NULL, 
                             min = 1, max = 100, step = 1, 
                             value = formals(PanelPRO::PanelPRO)$iterations,
                             width = "150px"),
                
                # max.iter.tries
                h5("The maximum number of iterations, including failed iterations, to be run when multiply imputing missing ages (1 to 500; 'max.iter.tries'):"),
                numericInput("missAgeMaxIters", label = NULL, 
                             min = 1, max = 500, step = 1, 
                             value = formals(PanelPRO::PanelPRO)$iterations * 5,
                             width = "150px"),
                
                # random.seed
                h5("Random seed for age imputation: (>0; 'random.seed')"),
                numericInput("randomSeed", label = NULL,
                             min = 1, max = NA, step = 1,
                             value = formals(PanelPRO::PanelPRO)$random.seed,
                             width = "150px"),
                
                # net
                h5("Return net or crude future risk estimates ('net')?",
                   style = "margin-bottom: 20px"),
                div(style = "margin-left:25px",
                  radioButtons("netOrCrude", label = NULL,
                               selected = "Crude",
                               choices = c("Crude", "Net"))
                ),
                
                # reset settings to defaults
                h5("Reset all settings to PanelPRO default values"),
                actionButton("resetPanelPROInputs", label = "Reset to Defaults",
                             icon = icon('undo'),
                             style = "margin-bottom:25px")
              )
            ),
            
            # RUN MODEL
            br(),
            actionButton("runPP", label = "Run PanelPRO",
                         icon = icon('play'))
            
          ), # end of "Run" tab for PanelPRO
          
          ###### UI: Results ####
          tabPanel(title = "PanelPRO Results",
            
            # top row for instructions and download button
            fluidRow(div(style = "margin-top:10px",
              column(width = 8,
                p("View the results in one of the tabs below or download them using the 'Download Results' button.")
              ),
              column(width = 4, align = "right",
                actionButton("downloadResults1", label = "Download Results",
                             icon = icon('download'),
                             style = "margin-top: 0px")
              )
            )),
            
            tabsetPanel(id = "ppResultTabs",
              
              ## Carrier Probs
              # plot
              tabPanel(title = "Carrier Prob. Plot",
                h4("Posterior Carrier Probabilities"),
                radioButtons("ppCPPlotView", label = "Select a Y-axis Scale:",
                             choices = c("Full (0 to 1)", "Zoomed"),
                             inline = T),
                conditionalPanel(condition = "input.ppCPPlotView == 'Zoomed'",
                  plotly::plotlyOutput("ppCPPlotZoom", width = "1000px", height = "500px")
                ),
                conditionalPanel(condition = "input.ppCPPlotView != 'Zoomed'",
                  plotly::plotlyOutput("ppCPPlotFull", width = "1000px", height = "500px")
                )
              ),
              
              # table
              tabPanel(title = "Carrier Prob. Table",
                h4("Posterior Carrier Probabilities"),
                DT::DTOutput("ppCPTbl", width = "500px")
              ),
              
              ## Cancer Risk
              # plot
              tabPanel(title = "Cancer Risk Plots",
                h4("Future Cancer Risk"),
                radioButtons("ppFRPlotView1", label = "Select a plot type:",
                             choices = c("Standard", "Zoomed Y-axis", "Compare risks to an average person"),
                             inline = F),
                conditionalPanel(condition = "input.ppFRPlotView1 == 'Zoomed Y-axis'",
                  plotly::plotlyOutput("ppFRPlotZoom", width = "1000px", height = "750px")
                ),
                conditionalPanel(condition = "input.ppFRPlotView1 == 'Standard'",
                  plotly::plotlyOutput("ppFRPlotFull", width = "1000px", height = "750px")
                ),
                conditionalPanel(condition = "input.ppFRPlotView1 == 'Compare risks to an average person'",
                  uiOutput("comparisonPlotH")
                )
              ),
              
              # table
              tabPanel(title = "Cancer Risk Table",
                h4("Future Cancer Risk"),
                DT::DTOutput("ppFRTbl", width = "450px")
              ),
              
              ## Settings
              tabPanel(title = "Run Settings",
                h4("Run Settings"),
                p("The PanelPRO results were obtained using the setting listed below. For a detailed explanation of 
                  the settings, click the 'Show PanelPRO Documentation' button at the bottom of the screen."),
                tableOutput("ppRunSettings"),
                actionButton("showPPDocString2", label = "Show PanelPRO Documentation",
                             icon = icon('book'))
              ),
              
              ## Console Output
              tabPanel(title = "Console Output",
                h4("PanelPRO Function R Console Output"),
                tags$div(
                  id = "ppMessageContainer",
                  style = "width:100%"
                ),
              )
            ) # end of ppResultTabs tabsetPanel
          ) # end of PanelPRO Results Tab in panelproTabs tabsetPanel
        ) # end of panelproTabs tabsetPanel
      ), # end of PanelPRO tab in navbarTabs
      
      ##### UI: My Account ####
      tabPanel("My Account",
        h3("Account Management"),
        
        h4("Username"),
        textOutput("userIs"),
        br(),
        
        h4("Password Reset"),
        p("To reset your password, log-out then select the 'Forgot Username or Password' button 
          on the log-in screen. On the 'Account Recovery' screen, select 'password' and then 
          follow the prompts to reset  your password."),
        br(),
        
        h4("Add Managers"),
        conditionalPanel("output.admin",
          p("Admin accounts cannot add managers, you already have access to all user accounts.") 
        ),
        conditionalPanel("!output.admin",
          conditionalPanel("output.manager",
            p("Accounts like yours, which have manager level permissions, can still add managers.")
          ),
          p("Optionally, you can add one or more managers who will be able to load, run, view, edit, save, 
              download and delete pedigrees you save to your user account. This is useful for 
              when multiple providers are enrolling patients into the same study. 
              Your manager(s) must have already created their own user account(s) with 'manager' 
              level permissions."),
          selectizeInput("selManagers2", label = "Enter managers' usernames",
                         selected = "",
                         choices = "",
                         multiple = TRUE,
                         options = list(create=TRUE)),
          conditionalPanel("output.isManagerWarning",
            h5(textOutput("ManagerWarning"), style = "color: red")
          ),
          actionButton("addManagersButton", label = "Add Selected Managers",
                       icon = icon('plus'),
                       style = "margin-top: 0px; margin-bottom: 25px")
        ),
        br(),
        
        h4("Permissions"),
        conditionalPanel("output.admin",
          p("Your user account has admin level permissions.")
        ),
        conditionalPanel("output.manager",
          p("Your user account has manager level permissions. This means you can load, run, view, edit, save, 
            download and delete pedigrees from any user account which listed your username as one of their managers."),
          p("You currently manage the following accounts:"),
          tagAppendAttributes(textOutput("managedAccts"), style="white-space:pre-wrap;")
        ),
        conditionalPanel("!output.manager & !output.admin",
          p("Your account has user level permissions. This means you can load, run, view, edit, save, 
            download and delete pedigrees using your account and only your account. To be able to perform 
            these functions on user accounts other than your own, you must request manager 
            level permissions by contacting your study PI.")
        )
      ) # end of My Account tab
    ) # end of NavBarPage
  ), # end of conditionalPanel for loggedIn status
  
  #### UI: Footer ####
  br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),

) # end of UI


server <- function(input, output, session) {
  
  #### Connect/Disconnect Database ####
  conn <- dbConnect(drv = RMariaDB::MariaDB(),
                    username = Sys.getenv('maria.un'),
                    password = Sys.getenv('maria.pw'),
                    host = getHost(),
                    port = 3306,
                    dbname = Sys.getenv('dbname'))
  onStop(function(){ dbDisconnect(conn) })
  
  #### User Accounts ####
  ##### Credentials ####
  # the current user's credentials
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = getUserbase(my_conn = conn),
    user_col = user,
    pwd_col = password,
    sodium_hashed = TRUE,
    log_out = reactive(logout_init())
  )
  
  # show log-out button only if logged in
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
  
  # reset app on log-out to clear all fields from previous user
  observeEvent(input$'logout-button', { shinyjs::refresh() })
  
  # permissions message
  output$permMessage <- renderUI({
    paste("You have", credentials()$info[["permissions"]],"permissions")
  })
  
  # admin flag for conditionally showing content
  admin <- reactive({
    if(credentials()$user_auth){
      if(credentials()$info[["permissions"]] == "admin"){
        return(TRUE)
      } else {
        return(FALSE)
      }
    } else {
      return(FALSE)
    }
  })
  output$admin <- reactive({ admin() })
  outputOptions(output, 'admin', suspendWhenHidden = FALSE)
  
  # manager flag for conditionally showing content
  manager <- reactive({
    if(credentials()$user_auth){
      if(credentials()$info[["permissions"]] == "manager"){
        return(TRUE)
      } else {
        return(FALSE)
      }
    } else {
      return(FALSE)
    }
  })
  output$manager <- reactive({ manager() })
  outputOptions(output, 'manager', suspendWhenHidden = FALSE)
  
  # log in status for conditional panels
  loggedIn <- reactive(req(credentials()$user_auth))
  output$loggedIn <- reactive({loggedIn()})
  outputOptions(output, 'loggedIn', suspendWhenHidden = FALSE)
  
  ##### Bot Check ####
  botCheckNums <- reactiveVal(NULL)
  botCheckMsg <- reactiveVal(NULL)
  showFailBotCheck1 <- reactiveVal(FALSE)
  botCheck1 <- reactiveVal(FALSE)
  
  # auto switch/hide tabs
  observeEvent(input$loginTabs, {
    # bot check if user clicks create account for forgot account tabs (only once)
    if(input$loginTabs %in% c("Sign Up", "Forgot Username or Password") &
       !botCheck1()){
      updateTabsetPanel(session, inputId = "loginTabs", selected = "Bot Check")
    }
    
    # hide bot check tab if bot check passed
    if(botCheck1()){
      hideTab(session, inputId = "loginTabs", target = "Bot Check")
    }
  })
  
  # bot check UI
  output$botCheckUI <- renderUI({
    shiny::tagList(
      h3("Bot Check"),
      p("First, let's check if you're human."),
      h4(textOutput("myNumbers1")),
      checkboxGroupInput(inputId = "botCheck1",
                         label = "Check the numbers listed above and hit submit.",
                         choices = 1:6,
                         inline = TRUE),
      actionButton(inputId = "submitBotCheck1",
                   label = "Submit"),
      conditionalPanel(condition = "output.showFailBotCheck1 & !output.botCheck1",
        h5(textOutput("failBotCheck1"), style = "color: red;"),
        actionButton(inputId = "newBotCheckNums",
                     label = "Try Again")
      ),
      conditionalPanel(condition = "output.botCheck1",
        p("Success, you're not a bot. You can proceed to sign-up or recover your account information.",
          style = "margin-top:20px")
      )
    )
  })
  
  # generate random numbers
  observeEvent(list(input$loginTabs, input$newBotCheckNums), {
    botCheckNums(getBotCheckNums())
    updateCheckboxGroupInput(session,
                             inputId = "botCheck1",
                             choices = 1:6,
                             selected = NULL,
                             inline = TRUE)
  })
  output$myNumbers1 <- renderText({ botCheckNums() })
  
  # check the numbers match
  observeEvent(input$submitBotCheck1, {
    if(length(botCheckNums()) == length(input$botCheck1)){
      ref <- botCheckNums()
      cknums <- as.logical()
      for(cknum in input$botCheck1){
        ck <- cknum %in% ref
        cknums[which(cknum == input$botCheck1)] <- ck
        if(ck){
          ref <- ref[!ref %in% cknum]
        }
      }
      if(length(cknums) > 0){
        if(all(cknums)){
          botCheck1(TRUE)
        }
      }
    }
    
    # failed bot check
    if(!botCheck1()){
      botCheckMsg("You entered an incorrect combination of numbers, try again.")
      showFailBotCheck1(TRUE)
      botCheckNums(NULL)
    }
  })
  
  # bot check passed: show sign-up UI
  output$botCheck1 <- reactive({ botCheck1() })
  outputOptions(output, 'botCheck1', suspendWhenHidden = FALSE)
  
  # bot check failed: show warning message and generate new code button
  output$showFailBotCheck1 <- reactive({ showFailBotCheck1() })
  outputOptions(output, 'showFailBotCheck1', suspendWhenHidden = FALSE)
  output$failBotCheck1 <- renderText({ botCheckMsg() })
  
  ##### Create new account ####
  # switch to sign-up tab if sign-up button clicked
  observeEvent(input$signUp, {
    updateTabsetPanel(session, inputId = "loginTabs", selected = "Sign Up")
  })
  
  # add new user to user database table and refresh the app
  isCredsError <- reactiveVal(FALSE)
  output$isCredsError <- reactive({ isCredsError() })
  outputOptions(output, 'isCredsError', suspendWhenHidden = FALSE)
  CredsError <- reactiveVal("None")
  output$CredsError <- renderText({ CredsError() })
  observeEvent(input$createNewUser, {
    
    # checks for new credentials
    ub <- getUserbase(conn)
    unCheck <- checkNewUsername(input$newUsername, ub$user)
    emCheck <- checkNewEmail(input$newEmail, input$newEmail2, ub$email)
    pwCheck <- checkNewPassword(input$newPassword, input$newPassword2)
    managersCheck <- checkManagers(prop.managers = input$selManagers1,
                                   un = input$newUsername,
                                   my_conn = conn)
    
    # add user if checks passed and refresh to update credentials table
    if(pwCheck == "Success" & unCheck == "Success" & emCheck == "Success" & 
       managersCheck$mssg == "Success"){
      newUser <- data.frame(user = input$newUsername,
                            permissions = "standard",
                            email = input$newEmail,
                            password = sodium::password_store(input$newPassword))
      dbAppendTable(conn = conn,
                    name = "user_base",
                    value = newUser)

      # optionally, add rows to manager table if the user selected managers
      # and issue warning to user if necessary
      if(!is.null(input$selManagers1)){
        valid.managers <- input$selManagers1[which(!input$selManagers1 %in% managersCheck$not.managers)]
        if(length(valid.managers) > 0){
          addManagerDF <- data.frame(user = rep(input$newUsername, length(valid.managers)),
                                     manager = valid.managers)
          dbAppendTable(conn = conn,
                        name = "managers",
                        value = addManagerDF)
        }
      }

      # initialize the user's table with the example_pedigree from the admin's account
      example_pedigree <- dbGetQuery(conn = conn,
                                     statement = "SELECT * FROM admin WHERE PedigreeID='example_pedigree'")
      savePedigreeToDB(conne = conn,
                        user = input$newUsername,
                        tmp_tbl = example_pedigree)
      
      # notify user of successful account creation
      shinybusy::notify_success("Account Created!", position = "center-bottom")
      
      # refresh app and take them to log-in screen
      shinyjs::delay(ms = 3000, shinyjs::refresh())
      
      # problem with proposed credentials
    } else {
      isCredsError(TRUE)

      # email problem
      if(emCheck != "Success"){
        CredsError(emCheck)

        # username and/or password problem
      } else if(pwCheck != "Success" | unCheck != "Success"){
        if(pwCheck != "Success" & unCheck == "Success"){
          CredsError(pwCheck)
        } else if(pwCheck == "Success" & unCheck != "Success"){
          CredsError(unCheck)
        } else if(pwCheck != "Success" & unCheck != "Success"){
          CredsError(paste0(unCheck, " ", pwCheck))
        }
        
        # manager problems
      } else if(managersCheck$mssg != "Success"){
        CredsError(managersCheck$mssg)
      }
    }
  })
  
  ##### Account Recovery ####
  # switch to forgot credentials tab if forgot button clicked
  observeEvent(input$forgotUnPw, {
    updateTabsetPanel(session, inputId = "loginTabs", selected = "Forgot Username or Password")
  })
  
  # response to recovery submit button and send emails
  showForgotResponse <- reactiveVal(FALSE)
  output$showForgotResponse <- reactive({ showForgotResponse() })
  outputOptions(output, 'showForgotResponse', suspendWhenHidden = FALSE)
  
  # instantiate recovery variables
  forgotWhichStatic <- reactiveVal(NULL)
  recoveryCode <- reactiveVal(NULL)
  recoveryTimer <- reactiveVal(NULL)
  recoveredUn <- reactiveVal(NULL)
  recoveryUserbase <- reactiveVal(NULL)
  
  # update selected recovery variables and send username email
  observeEvent(input$forgotGo, {
    forgotWhichStatic(input$forgotWhich)
    showForgotResponse(TRUE)
    recoveryUserbase(getUserbase(conn))
    
    # check that the email isn't blank
    if(nchar(input$recoveryEmail) > 0){
      
      # freeze the email
      shinyjs::disable("recoveryEmail")
      
      # get the username for the recovery account
      if(input$recoveryEmail %in% recoveryUserbase()$email){
        recoveredUn(recoveryUserbase()$user[which(recoveryUserbase()$email == input$recoveryEmail)])
      }
      
      # email the username to the user if that was what was forgotten
      if(input$forgotWhich %in% c("username","both") & !is.null(recoveredUn())){
        emailUser(userEmail = input$recoveryEmail,
                  emailType = "recoverUn",
                  userName = recoveredUn())
      }
    }
  })
  
  # set and send/re-send password recovery code
  # for some reason it will send two emails if input$forgotGo is included
  # in the observe list, and one email will have the previous code
  # assume that the status of input$newRecoveryCode changes when the button
  # appears in a conditionalPanel
  observeEvent(list(input$newRecoveryCode), {
    
    # only send if requested
    if(input$forgotWhich %in% c("password","both")){
      
      # set the code and reset it's expiration timer
      recoveryCode(createRecoveryCode())
      recoveryTimer(expireCode)
      
      # send the recovery code in an email
      emailUser(userEmail = input$recoveryEmail,
                emailType = "recoverPw",
                rCode = recoveryCode())
    }
  })
  
  # expire code after a set time
  observe({
    invalidateLater(1000)
    isolate({
      if(!is.null(recoveryCode())){
        recoveryTimer(recoveryTimer() - 1)
        if(recoveryTimer() < 1){
          recoveryCode(NULL)
          recoveryTimer(expireCode)
        }
      }
    })
  })
  
  # respond to recovery submit button with message and more inputs
  output$forgotResponse <- renderUI({
    if(nchar(input$recoveryEmail) > 0){
      if(forgotWhichStatic() == "username"){
        mssg <- p("Thank you. If the email address provided is in our system then
                   you will receive an email with your username soon.", 
                  style = "margin-top:20px")
      } else if(forgotWhichStatic() == "password"){
        mssg <- p("Thank you. If the email address provided is in our system then
                   you will receive an email with a password recovery code soon.", 
                  style = "margin-top:20px")
      } else if(forgotWhichStatic() == "both"){
        mssg <- p("Thank you. If the email address provided is in our system then
                   you will receive one email with your username and a separate email
                   with a password recovery code.", 
                  style = "margin-top:20px")
      }
      
      if(forgotWhichStatic() == "username"){
        return( shiny::tagList(
          mssg,
          actionButton(inputId = "backToLogin",
                       label = "Go Back to Log-in Screen"))
        )
      } else {
        return( shiny::tagList(
          mssg,
          p("To reset your password, enter the recovery code below and submit it."),
          textInput(inputId = "recoveryCode",
                    label = "Recovery Code"),
          actionButton(inputId = "submitRecoveryCode",
                       label = "Submit Recovery Code"),
          p("If you don't receive the code after two minutes, click the button below.", 
            style = "margin-top:20px"),
          actionButton(inputId = "newRecoveryCode",
                       label = "Send New Code"))
        )
      }
    } else {
      return( h5("You must first enter an email address.", style = "color: red;") )
    }
  })
  
  # go back to log-in screen
  observeEvent(input$backToLogin, {
    updateTabsetPanel(session, inputId = "loginTabs", selected = "Log In")
  })
  
  # check if entered code matches
  goodCode <- reactiveVal(FALSE)
  observeEvent(input$submitRecoveryCode, {
    if(input$submitRecoveryCode != "" & !is.null(input$recoveryCode)){
      if(input$recoveryCode == recoveryCode()){
        goodCode(TRUE)
      } else {
        goodCode(FALSE)
      }
    } else {
      goodCode(FALSE)
    }
  })
  
  # respond to entered recovery code
  showRecoveryCodeResponse <- reactiveVal(FALSE)
  output$showRecoveryCodeResponse <- reactive({ showRecoveryCodeResponse() })
  outputOptions(output, 'showRecoveryCodeResponse', suspendWhenHidden = FALSE)
  observeEvent(input$submitRecoveryCode, { showRecoveryCodeResponse(TRUE) })
  
  output$recoveryCodeResponse <- renderUI({
    if(goodCode()){
      return( shiny::tagList(
        p("Thank you. Please reset your password below."),
        passwordInput(inputId = "resetPw",
                      label = "Enter a new password"),
        passwordInput(inputId = "resetPw2",
                      label = "Re-enter the new password"),
        actionButton(inputId = "resetPwGo",
                     label = "Reset Password"))
      )
    } else {
      return(h5("An incorrect code was entered.", style = "color: red;"))
    }
  })
  
  # respond to new password
  showResetPwResponse <- reactiveVal(FALSE)
  output$showResetPwResponse <- reactive({ showResetPwResponse() })
  outputOptions(output, 'showResetPwResponse', suspendWhenHidden = FALSE)
  
  resetPwCheck <- reactiveVal("None")
  output$resetPassError <- renderText({ resetPwCheck() })
  observeEvent(input$resetPwGo, {
    
    # check new password
    resetPwCheck(checkNewPassword(input$resetPw, input$resetPw2))
    if(resetPwCheck() == "Success"){
      
      # write new password to database
      st <- paste0("UPDATE user_base SET password = '",sodium::password_store(input$resetPw),
                   "' WHERE user = '",recoveredUn(),"';")
      rs <- dbSendStatement(conn = conn, statement = st)
      dbClearResult(rs)
      
      # refresh app so it recognizes new credentials and goes back to log-in
      shinyjs::refresh()
      
      # flag to show what's wrong with the new password
    } else {
      showResetPwResponse(TRUE)
    }
  })
  
  # reset the forgot tab if it is navigated away from or
  # if the user wants to use a different email
  observeEvent(list(input$loginTabs), {
    if(input$loginTabs != "Forgot Username or Password"){
      recoveredUn(NULL)
      recoveryCode(NULL)
      goodCode(FALSE)
      forgotWhichStatic(NULL)
      resetPwCheck("None")
      recoveryUserbase(NULL)
      
      updateTextInput(session, inputId = "recoveryEmail", value = "")
      updateRadioButtons(session, inputId = "forgotWhich", selected = "username")
      updateTextInput(session, inputId = "recoveryCode", value = "")
      updateTextInput(session, inputId = "resetPw", value = "")
      updateTextInput(session, inputId = "resetPw2", value = "")
      
      showForgotResponse(FALSE)
      showRecoveryCodeResponse(FALSE)
      showResetPwResponse(FALSE)
    }
  })
  
  # refresh the app if the user wants to try to recover using a different
  # email address than the one previously entered
  observeEvent(input$diffEmail, { shinyjs::refresh() })
  
  ##### My Account ####
  output$userIs <- renderText(credentials()$info[["user"]])
  
  ###### Add Managers from My Account Tab ####
  # enable/disable add managers button if usernames are in the add managers input or not
  observeEvent(input$selManagers2, {
    if(is.null(input$selManagers2)){
      shinyjs::disable("addManagersButton")
    } else {
      shinyjs::enable("addManagersButton")
    }
  }, ignoreNULL = F)
  
  # add managers to database for the user account, add warning message if necessary
  isManagerWarning <- reactiveVal(FALSE)
  output$isManagerWarning <- reactive({ isManagerWarning() })
  outputOptions(output, 'isManagerWarning', suspendWhenHidden = FALSE)
  ManagerWarning <- reactiveVal("None")
  output$ManagerWarning <- renderText({ ManagerWarning() })
  observeEvent(input$addManagersButton, {
    managersCheck <- checkManagers(prop.managers = input$selManagers2,
                                   un = credentials()$info[["user"]],
                                   my_conn = conn)
    
    # add valid managers to the managers table in the database
    valid.managers <- input$selManagers2[which(!input$selManagers2 %in% managersCheck$not.managers)]
    if(length(valid.managers) > 0){
      addManagerDF <- data.frame(user = rep(credentials()$info[["user"]], length(valid.managers)),
                                 manager = valid.managers)
      dbAppendTable(conn = conn,
                    name = "managers",
                    value = addManagerDF)
    }
    
    # provide warnings if required
    if(managersCheck$mssg != "Success"){
      isManagerWarning(TRUE)
      ManagerWarning(paste0(managersCheck$mssg, 
                            " All usernames in this warning message were ignored but, any other managers you listed were added.")
                     )
    } else {
      isManagerWarning(FALSE)
      ManagerWarning("None")
    }
    
    # reset managers input
    updateSelectizeInput(session, "selManagers2", selected = "", choices = "")
  }, ignoreInit = T)
  
  # reset add manager related inputs when navigating away from the My Account tab
  observeEvent(input$navbarTabs, {
    isManagerWarning(FALSE)
    ManagerWarning("None")
    updateSelectizeInput(session, "selManagers2", selected = "", choices = "")
  }, ignoreInit = TRUE)
  
  ###### Show Managed Accounts ####
  output$managedAccts <- renderText({
    if(manager()){
      maccnts <- dbGetQuery(conn = conn,
                            statement = paste0("SELECT user FROM managers ", 
                                               "WHERE manager='", credentials()$info[["user"]],"';"
                                               ))$user
      return(paste0(maccnts, collapse = "\n"))
    } else {
      return("")
    }
  })
  
  #### Manage User Pedigrees ####
  ## currently loaded pedigree name
  currentPed <- reactive({
    if(!is.null(PED())){
      return(PED()$PedigreeID[1])
    } else {
      return(NULL)
    }
  })
  
  # text for new/load screen
  output$currentPed1 <- renderText({ 
    if(!is.null(currentPed())){
      return(currentPed())
    } else {
      return("no pedigree has been loaded or created yet.")
    }
  })
  
  # text for preview screen
  output$currentPed2 <- renderText({ 
    if(!is.null(currentPed())){
      return(currentPed())
    } else {
      return("no pedigree has been loaded or created yet. Go to the 'Create or Load' tab.")
    }
  })
  
  # text for panelpro screen
  output$currentPed3 <- renderText({ 
    if(!is.null(currentPed())){
      return(currentPed())
    } else {
      return("no pedigree has been loaded or created yet. Go to the 'Create or Load' tab.")
    }
  })
  
  ##### Load/Create New Pedigree ####
  # check that loading a pedigree is possible based on if at least one pedigree is selected
  # and enable/disable download buttons accordingly
  observeEvent(list(input$existingPed, input$newOrLoad), {
    if(input$newOrLoad == "Load existing" & input$existingPed == "No pedigree selected"){
      waitLoad(FALSE)
      shinyjs::disable("goNewOrLoad")
    } else if(input$newOrLoad == "Load existing"){
      waitLoad(TRUE)
      shinyjs::delay(delay_load_ms, {
        shinyjs::enable("goNewOrLoad")
        waitLoad(FALSE)
      })
    } else if(input$newOrLoad == "Create new"){
      waitLoad(FALSE)
      shinyjs::enable("goNewOrLoad")
    }
  }, ignoreInit = T)
  waitLoad <- reactiveVal(FALSE)
  output$waitLoad <- renderText({ need(!waitLoad(), "Please wait...") })
  observeEvent(input$newOrLoad, { shinyjs::reset("existingPed") })
  
  # user's pedigrees that are available for loading
  userPeds <- reactiveVal(NULL)
  
  # reactive to signal if the user account has its own table yet or not
  showTblExistsError <- reactiveVal(FALSE)
  output$showTblExistsError <- reactive({ showTblExistsError() })
  outputOptions(output, 'showTblExistsError', suspendWhenHidden = FALSE)
  
  # keep user tables and pedigrees available for loading updated at all times
  observe({
    if(loggedIn()){
      
      # for admin, make all user tables available
      if(admin() | manager()){
        showTblExistsError(FALSE)
        
        # for admin, start with all usernames
        if(admin()){
          users <- getUserbase(conn)$user
          
          # for a manager, start with all usernames assigned to that manager
        } else if(manager()){
          users <- c(credentials()$info[["user"]],
                     getUsersUnderManager(manager = credentials()$info[["user"]], 
                                          my_conn = conn))
        }
        
        users.with.tbls <- users
        for(usr in users){
          if(!dbExistsTable(conn = conn, name = usr)){
            users.with.tbls <- users.with.tbls[users.with.tbls != usr]
          }
        }
        
        if(length(users.with.tbls) > 0){
          updateSelectInput(session, inputId = "selectUser",
                            choices = users.with.tbls, 
                            selected = credentials()$info[["user"]])
        } else {
          updateSelectInput(session, inputId = "selectUser",
                            choices = credentials()$info[["user"]], 
                            selected = credentials()$info[["user"]])
        }
        
        # for non-admins and non-managers, only show sub-tables in their master table
      } else {
        
        # check if the user has a master table
        hasTbl <- dbExistsTable(conn = conn, name = credentials()$info[["user"]])
        if(hasTbl){
          showTblExistsError(FALSE)
          userPeds(sort(unique(
            dbGetQuery(conn = conn,
                       statement = paste0("SELECT PedigreeID FROM ", 
                                                        credentials()$info[["user"]], 
                                                        ";"))$PedigreeID
          )))
          
          # update the tables available for loading
          updateSelectInput(session, inputId = "existingPed",
                            choices = c("No pedigree selected", userPeds()))
        } else {
          showTblExistsError(TRUE)
        }
      }
    }
  })
  
  # for admins and managers, update the pedigrees available based on the user account selected
  observeEvent(list(input$selectUser, userPeds()), {
    userPeds(sort(unique(
      dbGetQuery(conn = conn,
                        statement = paste0("SELECT PedigreeID FROM ",
                                           ifelse(input$selectUser == "", credentials()$info[["user"]],
                                                  input$selectUser),
                                           ";"))$PedigreeID
    )))
    updateSelectInput(session, inputId = "existingPed",
                      choices = c("No pedigree selected", userPeds()))
  }, ignoreInit = T, ignoreNULL = T)
  
  newOrLoadFlag <- reactiveVal("new")
  observeEvent(input$goNewOrLoad, {
    if(input$newOrLoad == "Load existing" & input$existingPed != "No pedigree selected"){
      newOrLoadFlag("load")
      
      # for admins, load sub-table from the selected user's master table
      if(admin() | manager()){
        selected.user <- ifelse(input$selectUser == "", credentials()$info[["user"]], 
                                input$selectUser)
        
        # for non-admins, load sub-table from the user's master table
      } else {
        selected.user <- credentials()$info[["user"]]
      }
      
      # load the pedigree, replacing the CK5.6 colname first
      tped <- dbGetQuery(conn = conn,
                         statement = paste0("SELECT * FROM ",
                                            selected.user,
                                            " WHERE PedigreeID = '",
                                            input$existingPed,"';"))
      colnames(tped)[which(colnames(tped) == "CK5_6")] <- "CK5.6"
      PED(tped)
    
      # re-populate pedigree editor input widgets with new proband's information
      updateTextInput(session, "pedID", value = PED()$PedigreeID[1])
      shinyjs::disable("pedID")
      proband.id <- as.numeric(PED()$ID[which(PED()$isProband == 1)])
      lastRel(proband.id)
      updateSelectInput(session, "relSelect",
                        choices = setNames(as.character(PED()$ID), PED()$name),
                        selected = as.character(proband.id))
      proband.info <- PED()[which(PED()$ID == proband.id),]
      updateRelInputs(rel.info = proband.info, ss = session, conn = conn)
      shinyjs::disable("Sex")
      
      #### Reset cancer and gene reactives and UI modules and create data frames
      #### for cancer and gene data by
      master.can.df <- NULL
      master.gene.df <- NULL
      for(rl in PED()$ID){
      
        ### 1: RESET
        # CANCERS, iterate through this relative's cancer hx dictionary, if there is at least one cancer
        if(any(names(canReactive$canNums) == as.character(rl))){
          if(!is.na(canReactive$canNums[[as.character(rl)]]$dict[1])){
            for(cMod in sort(as.numeric(names(canReactive$canNums[[as.character(rl)]]$dict)), decreasing = T)){
              
              # update the cancer reactive object, remove the cancerUI module and delete it from memory
              canReactive$canNums <-
                removeCancer(cr = canReactive$canNums,
                             rel = as.character(rl),
                             inp = input,
                             ss = session,
                             trackMax = canReactive$canNums[[as.character(rl)]]$dict[cMod])
              
              # remove the module's inputs from memory
              remove_shiny_inputs(paste0("rel", rl, "canModule", canReactive$canNums[[as.character(rl)]]$dict[cMod]), input)
            }
          }
          
          # else, create an empty cancer tracked for this relative
        } else {
          canReactive$canNums[[as.character(rl)]] <- trackCans.rel
        }
        
        # GENES, iterate through this relative's panel dictionary, if there is at least one panel
        if(any(names(geneReactive$GeneNums) == as.character(rl))){
          if(!is.na(geneReactive$GeneNums[[as.character(rl)]]$dict[1])){
            for(pMod in sort(as.numeric(names(geneReactive$GeneNums[[as.character(rl)]]$dict)), decreasing = T)){
              
              # update the geneReactive and also remove and delete all related UI modules for this panel
              out <-
                removePanel(gr = geneReactive$GeneNums,
                            rel = as.character(rl),
                            pan.name = geneReactive$GeneNums[[as.character(rl)]]$panels[[paste0("panel", pMod)]]$name,
                            panel.module.id.num = geneReactive$GeneNums[[as.character(rl)]]$dict[pMod],
                            inp = input,
                            ss = session,
                            conn = conn)
              geneReactive$GeneNums <- out$gr

              # remove each geneUI module associated with this panel from memory
              for(tmp.geneMod.id in out$panel.geneMod.ids){
                remove_shiny_inputs(tmp.geneMod.id, input)
              }

              # remove the panel module's inputs from memory
              remove_shiny_inputs(paste0("rel", rl, "PanelModule", geneReactive$GeneNums[[as.character(rl)]]$dict[pMod]), input)
            }
          }

          # else initialize gene tracker for this realtive
        } else {
          geneReactive$GeneNums[[as.character(rl)]] <- relTemplate.trackGenes
        }

        ### 2: ORGANIZE JSON DATA TO BE LOADED INTO DATA FRAMES
        ## CANCERS
        if(!is.na(PED()$cancersJSON[which(PED()$ID == rl)])){

          # convert JSON into a data frame of cancers
          mod.can.JSON <- gsub(pattern = "\'", replacement = "\"", PED()$cancersJSON[which(PED()$ID == rl)])
          can.df <- fromJSON(mod.can.JSON, simplifyDataFrame = T)
          can.df <-
            can.df %>%
            mutate(across(.cols = c(cancer, other), ~as.character(.))) %>%
            mutate(rel = rl) %>%
            mutate(sex = as.character(PED()$Sex[which(PED()$ID == rl)])) %>%
            mutate(sex = recode(sex, "0" = "Female", "1" = "Male")) %>%
            mutate(cbc = "No") %>%
            mutate(cbcAge = NA) %>%
            mutate(age = na_if(age, "NA")) %>%
            mutate(across(.cols = c(age, cbcAge), ~as.numeric(.)))

          # combine contralateral into same row as breast and drop the CBC row
          if(any(can.df$cancer == "Breast") & any(can.df$cancer == "Contralateral")){
            can.df$cbc[which(can.df$cancer == "Breast")] <- "Yes"
            can.df$cbcAge[which(can.df$cancer == "Breast")] <- can.df$age[which(can.df$cancer == "Contralateral")]
            can.df <- can.df[which(can.df$cancer != "Contralateral"),]
          }

          # append to data frame of cancers for all relatives
          if(is.null(master.can.df)){
            master.can.df <- can.df
          } else {
            master.can.df <- rbind(master.can.df, can.df)
          }
        } # end of if statement to check if there was any content in cancersJSON for this relative

        ## GENES
        if(!is.na(PED()$genesJSON[which(PED()$ID == rl)])){

          # convert JSON into a data frame of genes
          mod.gene.JSON <- gsub(pattern = "\'", replacement = "\"", PED()$genesJSON[which(PED()$ID == rl)])
          gene.df <- fromJSON(mod.gene.JSON, simplifyDataFrame = T)
          gene.df <-
            gene.df %>%
            mutate(across(.cols = everything(), ~as.character(.))) %>%
            mutate(rel = rl)

          # append to data frame of genes for all relatives
          if(is.null(master.gene.df)){
            master.gene.df <- gene.df
          } else {
            master.gene.df <- rbind(master.gene.df, gene.df)
          }
        } # end of if statement to check if there was any content in genesJSON for this relative
      } # end of for loop for relatives that resets and re-populates cancer and gene UI modules and reactives
      
      ### 3: CREATE AND POPULATE MODULES
      ## CANCER
      # create and populate canUI modules
      if(!is.null(master.can.df)){
        lapply(1:nrow(master.can.df), function(x){
          
          # create the canUI modules with pre-populated inputs
          out <- addCancer(cr = canReactive$canNums,
                           rel = master.can.df$rel[x],
                           inp = input,
                           values = list(can = master.can.df$cancer[x],
                                         age = master.can.df$age[x],
                                         other = master.can.df$other[x],
                                         cbc = master.can.df$cbc[x],
                                         cbcAge = master.can.df$cbcAge[x]),
                           sex = master.can.df$sex[x])
          canReactive$canNums <- out$cr
          trackMax <- out$trackMax
          id <- out$id
          
          # add a server for checking the validity of the entered cancer age
          observeEvent(list(input[[paste0(id, "-CanAge")]], input$Age), {
            validateCanAgeServer(id,
                                 in.age = input[[paste0(id, "-CanAge")]],
                                 cur.age = PED()$CurAge[which(PED()$ID == master.can.df$rel[x])])
          })
          
          # add a server for checking the validity of the entered CBC age
          observeEvent(list(input[[paste0(id, "-CBCAge")]], 
                            input$Age,
                            input[[paste0(id, "-Can")]], 
                            input[[paste0(id, "-CanAge")]]), {
                              validateCBCAgeServer(id,
                                                   can = input[[paste0(id, "-Can")]],
                                                   cbc.age = input[[paste0(id, "-CBCAge")]],
                                                   bc.age = input[[paste0(id, "-CanAge")]],
                                                   cur.age = PED()$CurAge[which(PED()$ID == master.can.df$rel[x])])
                            })

          # create a remove module button observer for each UI module created
          observeEvent(input[[paste0(id, '-removeCan')]], {
            canReactive$canNums <- removeCancer(cr = canReactive$canNums,
                                                rel = master.can.df$rel[x],
                                                inp = input,
                                                ss = session,
                                                trackMax = trackMax)

            # remove the module's inputs from memory
            remove_shiny_inputs(id, input)
          })

          ## observe for BC and CBC
          observeEvent(list(input[[paste0(id, '-Can')]], input[[paste0(id, '-CBC')]]), {

            # reset CBC inputs if cancer is not BC
            if(input[[paste0(id, '-Can')]] != "Breast"){
              shinyjs::reset(id = paste0(id, '-CBC'))
              shinyjs::reset(id = paste0(id, '-CBCAge'))
            }

            # reset CBC age if CBC is no longer selected
            if(input[[paste0(id, '-CBC')]] == "No"){
              shinyjs::reset(id = paste0(id, '-CBCAge'))
            }
          }, ignoreInit = T, ignoreNULL = T)

          # create a cancer selection observer which will trigger an update of all of the cancer dropdown
          # choices for each of the person's cancer UI modules
          observeEvent(input[[paste0(id, '-Can')]], {
            updateCancerDropdowns(cr = canReactive$canNums,
                                  rel = master.can.df$rel[x],
                                  inp = input,
                                  ss = session,
                                  type = "cancer")
          })
        })
      }
      
      ## GENES
      # create and populate panelUI modules
      if(!is.null(master.gene.df)){
        lapply(1:nrow(master.gene.df), function(x){
  
          # prevent duplication of panels
          newPan <- FALSE
          if(x == 1){
            newPan <- TRUE
          } else if(master.gene.df$panel[x] != master.gene.df$panel[x-1]){
            newPan <- TRUE
          }
          if(newPan){
  
            # create the panelUI module with pre-populated input values
            out <- addPanel(gr = geneReactive$GeneNums,
                            rel = master.gene.df$rel[x],
                            inp = input,
                            ss = session,
                            pan.name = master.gene.df$panel[x],
                            conn = conn)
            geneReactive$GeneNums <- out$gr
            panel.module.id.num <- out$panel.module.id.num
            panMod.id <- out$panMod.id
  
            # create a remove module button observer for each panelUI module created
            observeEvent(input[[paste0(panMod.id, '-removePanel')]], {
              out.rm <- removePanel(gr = geneReactive$GeneNums,
                                    rel = master.gene.df$rel[x],
                                    pan.name = master.gene.df$panel[x],
                                    panel.module.id.num = panel.module.id.num,
                                    inp = input,
                                    ss = session,
                                    conn = conn)
              geneReactive$GeneNums <- out.rm$gr
  
              # remove each geneUI module associated with this panel from memory
              for(tmp.geneMod.id in out.rm$panel.geneMod.ids){
                remove_shiny_inputs(tmp.geneMod.id, input)
              }
  
              # remove the panel module's inputs from memory
              remove_shiny_inputs(panMod.id, input)
  
            }) # end of removePanel observeEvent
          } # end of if statement to prevent duplication panels
        }) # end of lapply for creating panelUI modules
  
        ## create the geneUI modules by iterating through the rows of the master table of gene results
        lapply(1:nrow(master.gene.df), function(y){
          rtype <- master.gene.df$result[y]
  
          # only create geneUI modules for non-Neg results
          if(rtype != "Neg"){
            out <- addGene(gr = geneReactive$GeneNums,
                           rel = master.gene.df$rel[y],
                           inp = input,
                           p.name = master.gene.df$panel[y],
                           rtype = rtype,
                           vals = list(gene = master.gene.df$gene[y],
                                       nuc = master.gene.df$nuc[y],
                                       prot = master.gene.df$prot[y],
                                       zyg = master.gene.df$zyg[y]))
            geneReactive$GeneNums <- out$gr
            gene.module.id.num <- out$gene.module.id.num
            geneMod.id <- out$geneMod.id
  
            # create a remove module button observer for each UI module created
            observeEvent(input[[paste0(geneMod.id, '-removeGene')]], {
              geneReactive$GeneNums <-
                removeGene(gr = geneReactive$GeneNums,
                           rel = master.gene.df$rel[y],
                           inp = input,
                           p.name = master.gene.df$panel[y],
                           gene.module.id.num = gene.module.id.num,
                           geneMod.id = geneMod.id,
                           rtype = rtype)
  
              # remove the module's inputs from memory
              remove_shiny_inputs(geneMod.id, input)
            })
          } # end of if statement checking if the result type is non-Negative
        }) # end of lapply for creating geneUI modules
      } # end of if statement to check if master.gene.df is not NULL

      # hide add relatives tab and visualize the pedigree
      hideTab("pedTabs", target = "Add Relatives", session = session)
      shinyjs::click("showPedButton")
      
      # reset the pedigree loading selector
      shinyjs::reset("existingPed")
      
      ###### CREATE NEW PEDIGREE
    } else if(input$newOrLoad == "Create new"){
      newOrLoadFlag("new")
      
      ## reset inputs and input reactives
      shinyjs::reset("relSelect")
      lastRel(1)
      PED(NULL)
      
      # demo
      shinyjs::enable("pedID")
      shinyjs::enable("Sex")
      for(demo.var in c("pedID", "Sex", "Age", "isDead", "race", "eth", "ancAJ", "ancIt")){
        shinyjs::reset(demo.var)
      }
      
      # cancers
      # reset modules and inputs
      for(rl in as.numeric(names(canReactive$canNums))){
        
        # iterate through this relative's cancer hx dictionary, if there is at least one cancer
        if(!is.na(canReactive$canNums[[as.character(rl)]]$dict[1])){
          for(cMod in sort(as.numeric(names(canReactive$canNums[[as.character(rl)]]$dict)), decreasing = T)){
            
            # update the cancer reactive object, remove the cancerUI module and delete it from memory
            canReactive$canNums <-
              removeCancer(cr = canReactive$canNums,
                           rel = as.character(rl),
                           inp = input,
                           ss = session,
                           trackMax = canReactive$canNums[[as.character(rl)]]$dict[cMod])
            
            # remove the module's inputs from memory
            remove_shiny_inputs(paste0("rel", rl, "canModule", canReactive$canNums[[as.character(rl)]]$dict[cMod]), input)
          }
        }
      }
      canReactive$canNums <- trackCans.init
      
      # cbc
      for(cbc.var in cbcrisk.cols){
        shinyjs::reset(cbc.var)
      }
      
      # markers
      for(mtype in c(PanelPRO:::MARKER_TESTING$BC$MARKERS, PanelPRO:::MARKER_TESTING$COL$MARKERS)){
        m <- ifelse(mtype == "CK5.6", "CK56", mtype)
        shinyjs::reset(m)
      }
      
      # surg
      for(stype in c("Mast", "Hyst", "Ooph")){
        shinyjs::reset(stype)
        shinyjs::reset(paste0(stype, "Age"))
      }
      
      # genes
      shinyjs::reset("existingPanels")
      shinyjs::reset("editPanel")
      
      # reset modules and inputs
      for(rl in as.numeric(names(geneReactive$GeneNums))){
        
        # iterate through this relative's panel dictionary, if there is at least one panel
        if(!is.na(geneReactive$GeneNums[[as.character(rl)]]$dict[1])){
          for(pMod in sort(as.numeric(names(geneReactive$GeneNums[[as.character(rl)]]$dict)), decreasing = T)){
            
            # update the geneReactive and also remove and delete all related UI modules for this panel
            out <-
              removePanel(gr = geneReactive$GeneNums,
                          rel = as.character(rl),
                          pan.name = geneReactive$GeneNums[[as.character(rl)]]$panels[[paste0("panel", pMod)]]$name,
                          panel.module.id.num = geneReactive$GeneNums[[as.character(rl)]]$dict[pMod],
                          inp = input,
                          ss = session,
                          conn = conn)
            geneReactive$GeneNums <- out$gr
            
            # remove each geneUI module associated with this panel from memory
            for(tmp.geneMod.id in out$panel.geneMod.ids){
              remove_shiny_inputs(tmp.geneMod.id, input)
            }
            
            # remove the panel module's inputs from memory
            remove_shiny_inputs(paste0("rel", rl, 
                                       "PanelModule", geneReactive$GeneNums[[as.character(rl)]]$dict[pMod]), 
                                input)
          }
        }
      }
      geneReactive$GeneNums <- trackGenes.init
      
      # show "add relatives" tab and don't display the pedigree tree & table
      showPed(FALSE)
      
    } # end of else for creating a new pedigree
    
    # execute actions relevant to create new and load existing
    if(input$newOrLoad == "Create new" | 
       (input$newOrLoad == "Load existing" & input$existingPed != "No pedigree selected")){
            
      # reset add relative counts
      for(relation in c("Dau", "Son", "Sis", "Bro", "MAunt", "MUnc", "PAunt", "PUnc")){
        shinyjs::reset(paste0("num", relation))
      }
      
      # update selected users for loading, downloading, and deleting a pedigree
      updateSelectInput(session, "selectUser", selected = credentials()$info[["user"]])
      updateSelectInput(session, "selectUserForDownload", selected = credentials()$info[["user"]])
      updateSelectInput(session, "selectUserForCopyTo", selected = credentials()$info[["user"]])
      updateSelectInput(session, "selectUserForCopyFrom", selected = credentials()$info[["user"]])
      updateSelectInput(session, "selectUserForDelete", selected = credentials()$info[["user"]])
      
      # show the pedigree edit/create tab when the button is clicked the first time
      showTab("navbarTabs", target = "Create/Modify Pedigree", session = session)
      
      # update selected tabs
      updateTabsetPanel(session, "pedTabs", selected = "Demographics")
      updateTabsetPanel(session, "geneTabs", selected = "Instructions")
      updateTabsetPanel(session, "geneResultTabs", selected = "P/LP")
      updateTabsetPanel(session, "pedVisualsEditor", selected = "Tree")
      updateTabsetPanel(session, "pedVisualsViewer", selected = "Tree")
      updateTabsetPanel(session, "panelproTabs", selected = "Run PanelPRO")
      updateTabsetPanel(session, "ppResultTabs", selected = "Carrier Prob. Plot")
    }
    
    # take user to the pedigree editor if they chose the create new option
    if(input$newOrLoad == "Create new"){
      updateNavlistPanel(session, "navbarTabs", selected = "Create/Modify Pedigree")
    }
    
    # notify user of load success
    if(input$newOrLoad == "Load existing"){
      shinybusy::notify_success("Load Successful!", position = "center-bottom")
    }
  }, ignoreInit = T)
  
  ##### Copy Pedigree ####
  # keep dropdowns of user tables and pedigrees available for copy updated at all times
  userPedsForCopyFrom <- reactiveVal(NULL)
  userPedsForCopyTo <- reactiveVal(NULL)
  showTblExistsErrorCopy <- reactiveVal(FALSE)
  output$showTblExistsErrorCopy <- reactive({ showTblExistsErrorCopy() })
  outputOptions(output, 'showTblExistsErrorCopy', suspendWhenHidden = FALSE)
  observe({
    if(loggedIn()){
      
      # for admin, make all user tables available
      if(admin() | manager()){
        showTblExistsErrorCopy(FALSE)
        
        # for admin, start with all usernames
        if(admin()){
          users <- getUserbase(conn)$user
          
          # for a manager, start with all usernames assigned to that manager
        } else if(manager()){
          users <- c(credentials()$info[["user"]],
                     getUsersUnderManager(manager = credentials()$info[["user"]], 
                                          my_conn = conn))
        }
        
        users.with.tbls <- users
        for(usr in users){
          if(!dbExistsTable(conn = conn, name = usr)){
            users.with.tbls <- users.with.tbls[users.with.tbls != usr]
          }
        }
        
        if(length(users.with.tbls) > 0){
          updateSelectInput(session, inputId = "selectUserForCopyFrom",
                            choices = users.with.tbls, 
                            selected = credentials()$info[["user"]])
          updateSelectInput(session, inputId = "selectUserForCopyTo",
                            choices = users.with.tbls, 
                            selected = credentials()$info[["user"]])
        } else {
          updateSelectInput(session, inputId = "selectUserForCopyFrom",
                            choices = credentials()$info[["user"]], 
                            selected = credentials()$info[["user"]])
          updateSelectInput(session, inputId = "selectUserForCopyTo",
                            choices = credentials()$info[["user"]], 
                            selected = credentials()$info[["user"]])
        }
        
        # for non-admins and non-managers, only show sub-tables in their master table
      } else {
        
        # check if the user has a master table
        hasTbl <- dbExistsTable(conn = conn, name = credentials()$info[["user"]])
        if(hasTbl){
          showTblExistsErrorCopy(FALSE)
          userPedsForCopyFrom(sort(unique(
            dbGetQuery(conn = conn,
                       statement = paste0("SELECT PedigreeID FROM ", 
                                          credentials()$info[["user"]], 
                                          ";"))$PedigreeID
          )))
          userPedsForCopyTo(userPedsForCopyFrom())
          
          # update the tables available for copy
          updateSelectInput(session, inputId = "selectCopyPed",
                            selected = "No pedigree selected",
                            choices = c("No pedigree selected", userPedsForCopyFrom()))
        } else {
          showTblExistsErrorCopy(TRUE)
        }
      }
    }
  })
  
  # for admins and managers, update the pedigrees available to copy from based on the user account selected
  observeEvent(list(input$selectUserForCopyFrom, userPedsForCopyFrom()), {
    userPedsForCopyFrom(sort(unique(
      dbGetQuery(conn = conn,
                 statement = paste0("SELECT PedigreeID FROM ",
                                    ifelse(input$selectUserForCopyFrom == "", credentials()$info[["user"]],
                                           input$selectUserForCopyFrom),
                                    ";"))$PedigreeID
    )))
    updateSelectInput(session, inputId = "selectCopyPed",
                      choices = c("No pedigree selected", userPedsForCopyFrom()))
  }, ignoreInit = T, ignoreNULL = T)
  
  # for admins and managers, update the pedigrees in the copy to account
  observeEvent(input$selectUserForCopyTo, {
    userPedsForCopyTo(sort(unique(
      dbGetQuery(conn = conn,
                 statement = paste0("SELECT PedigreeID FROM ",
                                    ifelse(input$selectUserForCopyTo == "", credentials()$info[["user"]],
                                           input$selectUserForCopyTo),
                                    ";"))$PedigreeID
    )))
    
  }, ignoreInit = T, ignoreNULL = T)
  
  # validate the new pedigree name
  validPedID <- reactive({
    paste(
      need(all(userPedsForCopyTo() != input$newPedName),
           "Your account already has a pedigree with this ID number, choose another name."),
      need(input$newPedName != "example_pedigree",
           "'example_pedigree' is a reserved pedigree name, pick another name."),
      need(input$newPedName != input$selectCopyPed,
           "The copy's name cannot be the same as the original."),
      need(input$newPedName != "",
           "Provide a name for the new pedigree.")
    )
  })
  output$validPedID <- renderText({ shiny::validate(validPedID()) })
  
  # check that copy is possible based on if at least one pedigree is selected
  # and enable/disable copy button accordingly
  observeEvent(list(input$selectCopyPed, validPedID()), {
    if(input$selectCopyPed != "No pedigree selected" & length(validPedID()) == 0){
      waitCopy(TRUE)
      shinyjs::delay(delay_copy_ms, {
        shinyjs::enable("copyPed")
        waitCopy(FALSE)
      })
    } else {
      waitCopy(FALSE)
      shinyjs::disable("copyPed")
    }
  }, ignoreInit = F, ignoreNULL = T)
  waitCopy <- reactiveVal(FALSE)
  output$waitCopy <- renderText({ need(!waitCopy(), "Please wait...") })
  
  # copy the selected pedigrees
  observeEvent(input$copyPed, {
    if(length(validPedID()) == 0){
      if(admin() | manager()){
        fromAcct <- ifelse(input$selectUserForCopyFrom == "", credentials()$info[["user"]],
                           input$selectUserForCopyFrom)
        toAcct <- ifelse(input$selectUserForCopyTo == "", credentials()$info[["user"]],
                         input$selectUserForCopyTo)
      } else {
        fromAcct <- credentials()$info[["user"]]
        toAcct <- credentials()$info[["user"]]
      }
      
      # retrieve and rename the pedigree
      tmp.ped <- 
        dbGetQuery(conn = conn,
                   statement = paste0("SELECT * FROM ", fromAcct, 
                                      " WHERE PedigreeID='", input$selectCopyPed, "';"))
      tmp.ped$PedigreeID <- input$newPedName
      
      # save to new pedigree to database
      savePedigreeToDB(conne = conn,
                       user = toAcct,
                       tmp_tbl = tmp.ped)
      
      # update the list of pedigrees that can be loaded, downloaded, and deleted
      updated.peds <- sort(unique(
        dbGetQuery(conn = conn,
                   statement = paste0("SELECT PedigreeID FROM ", toAcct, ";"))$PedigreeID
      ))
      userPeds(updated.peds)
      userPedsForDownload(updated.peds)
      userPedsForDelete(updated.peds)
      userPedsForCopyFrom(updated.peds)
      userPedsForCopyTo(updated.peds)
      
      # reset pedigree name input
      updateTextInput(session, "newPedName", value = "")
      
      # notify user of copy success
      shinybusy::notify_success("Copy Successful!", position = "center-bottom")
    }
  }, ignoreInit = T)
  
  ##### Download Pedigrees ####
  # keep drop-downs of user tables and pedigrees available for downloading updated at all times
  userPedsForDownload <- reactiveVal(NULL)
  showTblExistsErrorDownload <- reactiveVal(FALSE)
  output$showTblExistsErrorDownload <- reactive({ showTblExistsErrorDownload() })
  outputOptions(output, 'showTblExistsErrorDownload', suspendWhenHidden = FALSE)
  observe({
    if(loggedIn()){
      
      # for admin, make all user tables available
      if(admin() | manager()){
        showTblExistsErrorDownload(FALSE)
        
        # for admin, start with all usernames
        if(admin()){
          users <- getUserbase(conn)$user
          
          # for a manager, start with all usernames assigned to that manager
        } else if(manager()){
          users <- c(credentials()$info[["user"]],
                     getUsersUnderManager(manager = credentials()$info[["user"]], 
                                          my_conn = conn))
        }
        users.with.tbls <- users
        for(usr in users){
          if(!dbExistsTable(conn = conn, name = usr)){
            users.with.tbls <- users.with.tbls[users.with.tbls != usr]
          }
        }
        
        # update users with pedigrees that can be downloaded
        if(length(users.with.tbls) > 0){
          updateSelectInput(session, inputId = "selectUserForDownload",
                            choices = users.with.tbls,
                            selected = credentials()$info[["user"]])
        } else {
          updateSelectInput(session, inputId = "selectUserForDownload",
                            choices = credentials()$info[["user"]],
                            selected = credentials()$info[["user"]])
        }
        
        # for non-admins and non-managers, only show sub-tables in their master table
      } else {
        
        # check if the user has a master table
        hasTbl <- dbExistsTable(conn = conn, name = credentials()$info[["user"]])
        if(hasTbl){
          showTblExistsErrorDownload(FALSE)
          userPedsForDownload(sort(unique(
            dbGetQuery(conn = conn,
                       statement = paste0("SELECT PedigreeID FROM ", 
                                          credentials()$info[["user"]], 
                                          ";"))$PedigreeID
          )))
          
          # update the tables available for loading
          updateSelectInput(session, inputId = "selectDownloadPeds",
                            choices = userPedsForDownload())
        } else {
          showTblExistsErrorDownload(TRUE)
        }
      }
    }
  })
  
  # for admins and managers, update the pedigrees available based on the user account selected
  observeEvent(list(input$selectUserForDownload, userPedsForDownload()), {
    userPedsForDownload(sort(unique(
      dbGetQuery(conn = conn,
                 statement = paste0("SELECT PedigreeID FROM ",
                                    ifelse(input$selectUserForDownload == "", credentials()$info[["user"]],
                                           input$selectUserForDownload),
                                    ";"))$PedigreeID
    )))
    updateSelectInput(session, inputId = "selectDownloadPeds",
                      choices = userPedsForDownload())
  }, ignoreInit = T, ignoreNULL = T)
  
  # select or de-select all pedigrees in a user account for downloading
  observeEvent(input$selectAllPeds, {
    if(input$selectAllPeds){
      updateSelectInput(session, "selectDownloadPeds",
                        selected = userPedsForDownload(),
                        choices = userPedsForDownload())
    } else {
      updateSelectInput(session, "selectDownloadPeds",
                        choices = userPedsForDownload())
    }
  }, ignoreInit = T)
  
  # check that download is possible based on if at least one pedigree is selected
  # and enable/disable download buttons accordingly
  observeEvent(input$selectDownloadPeds, {
    if(!is.null(input$selectDownloadPeds)){
      waitDownload(TRUE)
      shinyjs::delay(delay_download_ms, {
        shinyjs::enable("downloadPedsCSV")
        shinyjs::enable("downloadPedsRDS")
        waitDownload(FALSE)
      })
    } else {
      waitDownload(FALSE)
      shinyjs::disable("downloadPedsCSV")
      shinyjs::disable("downloadPedsRDS")
    }
  }, ignoreNULL = F)
  waitDownload <- reactiveVal(FALSE)
  output$waitDownload <- renderText({ need(!waitDownload(), "Please wait...") })
  
  # prepare the table of pedigrees to be downloaded from the "Manage Pedigrees" tab
  downloadPedsTable <- reactive({
    if(admin() | manager()){
      fromAcct <- ifelse(input$selectUserForDownload == "", credentials()$info[["user"]], 
                         input$selectUserForDownload)
    } else {
      fromAcct <- credentials()$info[["user"]]
    }
    dbGetQuery(conn = conn, 
               statement = paste0("SELECT * FROM ", fromAcct, " ",
                                  "WHERE PedigreeID IN ('", 
                                  paste0(input$selectDownloadPeds, collapse = "','"), 
                                  "');")
    )
  })
  
  # prepare the table of detailed cancer hx (includes non-PanelPRO cancers)
  downloadCanDetails <- reactive({
    
    # get the cancers JSONs for all pedigrees selected for download
    if(admin() | manager()){
      fromAcct <- ifelse(input$selectUserForDownload == "", credentials()$info[["user"]], 
                         input$selectUserForDownload)
    } else {
      fromAcct <- credentials()$info[["user"]]
    }
    peds <- 
      dbGetQuery(conn = conn, 
                 statement = paste0("SELECT PedigreeID, ID, cancersJSON FROM ", fromAcct, " ",
                                    "WHERE PedigreeID IN ('", 
                                    paste0(input$selectDownloadPeds, collapse = "','"), 
                                    "');"))
    
    # convert JSONs into a data frame of cancer hx indexed by PedigreeID and ID
    can.df <- NULL
    for(pi in unique(peds$PedigreeID)){
      this.ped <- peds[which(peds$PedigreeID == pi),]
      for(row in 1:nrow(this.ped)){
        if(!is.na(this.ped$cancersJSON[row])){
          mod.can.JSON <- gsub(pattern = "\'", replacement = "\"", this.ped$cancersJSON[row])
          rel.can.df <- fromJSON(mod.can.JSON, simplifyDataFrame = T)
          rel.can.df <- cbind(data.frame(PedigreeID = rep(pi, nrow(rel.can.df)), 
                                         ID = rep(this.ped$ID[row], nrow(rel.can.df))), 
                              rel.can.df)
          if(is.null(can.df)){
            can.df <- rel.can.df
          } else {
            can.df <- rbind(can.df, rel.can.df)
          }
        }
      }
    }
    
    # if there was no cancer hx in any of the selected pedigrees return a 0 row data frame
    if(is.null(can.df)){
      return(setNames(as.data.frame(matrix(NA, nrow = 0, ncol = 2 + length(colnames(cancer.inputs.store)))), 
                      c("PedigreeID", "ID", colnames(cancer.inputs.store))))
    } else {
      return(can.df)
    }
  })
  
  # prepare the table of detailed panel (includes non-PanelPRO genes, all result types, nuc/prot/zyg information)
  downloadPanelDetails <- reactive({
    
    # get the cancers JSONs for all pedigrees selected for download
    if(admin() | manager()){
      fromAcct <- ifelse(input$selectUserForDownload == "", credentials()$info[["user"]], 
                         input$selectUserForDownload)
    } else {
      fromAcct <- credentials()$info[["user"]]
    }
    peds <- 
      dbGetQuery(conn = conn, 
                 statement = paste0("SELECT PedigreeID, ID, genesJSON FROM ", fromAcct, " ",
                                    "WHERE PedigreeID IN ('", 
                                    paste0(input$selectDownloadPeds, collapse = "','"), 
                                    "');"))
    
    # convert JSONs into a data frame of gene results indexed by PedigreeID and ID
    gene.df <- NULL
    for(pi in unique(peds$PedigreeID)){
      this.ped <- peds[which(peds$PedigreeID == pi),]
      for(row in 1:nrow(this.ped)){
        if(!is.na(this.ped$genesJSON[row])){
          mod.gene.JSON <- gsub(pattern = "\'", replacement = "\"", this.ped$genesJSON[row])
          rel.gene.df <- fromJSON(mod.gene.JSON, simplifyDataFrame = T)
          rel.gene.df <- cbind(data.frame(PedigreeID = rep(pi, nrow(rel.gene.df)), 
                                          ID = rep(this.ped$ID[row], nrow(rel.gene.df))), 
                               rel.gene.df)
          if(is.null(gene.df)){
            gene.df <- rel.gene.df
          } else {
            gene.df <- rbind(gene.df, rel.gene.df)
          }
        }
      }
    }
    
    # if there was no gene results in any of the selected pedigrees return a 0 row data frame
    if(is.null(gene.df)){
      return(setNames(as.data.frame(matrix(NA, nrow = 0, ncol = 2 + length(gene.df.colnames))), 
                      c("PedigreeID", "ID", gene.df.colnames)))
    } else {
      return(gene.df)
    }
  })
  
  # table of PanelPRO cancers dictionary to link abbreviations to cancer names
  ppCancersDict <- reactive({
    as.data.frame(PanelPRO:::CANCER_NAME_MAP)
  })
  
  # table of PanelPRO gene names
  ppGenes <- reactive({
    data.frame(PanelPRO_genes = PanelPRO:::GENE_TYPES)
  })
  
  # download one or more pedigrees from the user account from the "Manage Pedigrees" tab
  # as a zip file of CSV files
  output$downloadPedsCSV <- shiny::downloadHandler(
    filename = function(){
      paste0("PanelPRO-pedigrees-", Sys.Date(), ".zip")
    },
    content = function(file){
      
      # create .csv files and zip them together
      write.csv(downloadPedsTable(), file = "download-pedigrees/pedigrees.csv", row.names = F)
      write.csv(downloadCanDetails(), file = "download-pedigrees/cancer-details.csv", row.names = F)
      write.csv(downloadPanelDetails(), file = "download-pedigrees/panel-details.csv", row.names = F)
      write.csv(ppCancersDict(), file = "data-dictionary/panelpro-cancer-abbreviations.csv", row.names = F)
      write.csv(ppGenes(), file = "data-dictionary/panelpro-gene-list.csv", row.names = F)
      
      # include a png if 1 and only 1 pedigree is being downloaded
      if(length(unique(downloadPedsTable()$PedigreeID)) == 1){
        
        # check if there are any cancers which determines if the legend should be included
        cans <- select(downloadPedsTable(), starts_with("isAff"))
        
        # create the png image
        png(filename = "download-pedigrees/pedigree-image.png")
        print(plot(
          kinship2.ped(downloadPedsTable())[paste0(unique(downloadPedsTable()$PedigreeID))]
        ))
        if(sum(cans) > 0){
          print(
            pedigree.legend(kinship2.ped(downloadPedsTable())[paste0(unique(downloadPedsTable()$PedigreeID))], 
                            location = "bottomright", radius=0.1)
          )
        }
        dev.off()
      }
      
      new.files <- c(
        "download-pedigrees/pedigrees.csv",
        "download-pedigrees/cancer-details.csv",
        "download-pedigrees/panel-details.csv",
        "data-dictionary/panelpro-cancer-abbreviations.csv",
        "data-dictionary/panelpro-gene-list.csv",
        "download-pedigrees/pedigree-image.png"
      )
      
      tmp.zip <- 
        zip::zip(zipfile = file, 
                 files = c("data-dictionary/columns-and-codings-dictionary.csv",
                           "data-dictionary/README.md",
                           "download-pedigrees/README.md",
                           new.files))
      
      # remove the created files
      file.remove(new.files)
      
      return(tmp.zip)
    },
    contentType = "application/zip"
  )
  
  # download one or more pedigrees from the user account from the "Manage Pedigrees" tab
  # as a .rds
  output$downloadPedsRDS <- downloadHandler(
    filename = function(){
      paste0("PanelPRO-pedigrees-", Sys.Date(), ".zip")
    },
    content = function(file){
      
      # create .rds files and zip them together
      saveRDS(downloadPedsTable(), file = "download-pedigrees/pedigrees.rds")
      saveRDS(downloadCanDetails(), file = "download-pedigrees/cancer-details.rds")
      saveRDS(downloadPanelDetails(), file = "download-pedigrees/panel-details.rds")
      saveRDS(read.csv(file = "data-dictionary/columns-and-codings-dictionary.csv"), 
              file = "data-dictionary/columns-and-codings-dictionary.rds")
      saveRDS(ppCancersDict(), file = "data-dictionary/panelpro-cancer-abbreviations.rds")
      saveRDS(ppGenes(), file = "data-dictionary/panelpro-gene-list.rds")
      
      # include a png if 1 and only 1 pedigree is being downloaded
      if(length(unique(downloadPedsTable()$PedigreeID)) == 1){
        
        # check if there are any cancers which determines if the legend should be included
        cans <- select(downloadPedsTable(), starts_with("isAff"))
        
        # create the png image
        png(filename = "download-pedigrees/pedigree-image.png")
        print(plot(
          kinship2.ped(downloadPedsTable())[paste0(unique(downloadPedsTable()$PedigreeID))]
        ))
        if(sum(cans) > 0){
          print(
            pedigree.legend(kinship2.ped(downloadPedsTable())[paste0(unique(downloadPedsTable()$PedigreeID))], 
                            location = "bottomright", radius=0.1)
          )
        }
        dev.off()
      }
      
      # filenames of all the temporary files
      new.files <- c(
        "download-pedigrees/pedigrees.rds",
        "download-pedigrees/cancer-details.rds",
        "download-pedigrees/panel-details.rds",
        "data-dictionary/columns-and-codings-dictionary.rds",
        "data-dictionary/panelpro-cancer-abbreviations.rds",
        "data-dictionary/panelpro-gene-list.rds",
        "download-pedigrees/pedigree-image.png"
      )
      
      tmp.zip <- 
        zip::zip(zipfile = file, 
                 files = c("data-dictionary/README.md",
                           "download-pedigrees/README.md",
                           new.files))
      
      # remove the created files
      file.remove(new.files)
      
      return(tmp.zip)
    },
    contentType = "application/zip"
  )
  
  ##### Delete Pedigrees ####
  # keep dropdowns of user tables and pedigrees available for deletion updated at all times
  userPedsForDelete <- reactiveVal(NULL)
  showTblExistsErrorDelete <- reactiveVal(FALSE)
  output$showTblExistsErrorDelete <- reactive({ showTblExistsErrorDelete() })
  outputOptions(output, 'showTblExistsErrorDelete', suspendWhenHidden = FALSE)
  observe({
    if(loggedIn()){
      
      # for admin, make all user tables available
      if(admin() | manager()){
        showTblExistsErrorDelete(FALSE)
        
        # for admin, start with all usernames
        if(admin()){
          users <- getUserbase(conn)$user
          
          # for a manager, start with all usernames assigned to that manager
        } else if(manager()){
          users <- c(credentials()$info[["user"]],
                     getUsersUnderManager(manager = credentials()$info[["user"]], 
                                          my_conn = conn))
        }
        
        users.with.tbls <- users
        for(usr in users){
          if(!dbExistsTable(conn = conn, name = usr)){
            users.with.tbls <- users.with.tbls[users.with.tbls != usr]
          }
        }
        
        if(length(users.with.tbls) > 0){
          updateSelectInput(session, inputId = "selectUserForDelete",
                            choices = users.with.tbls, 
                            selected = credentials()$info[["user"]])
        } else {
          updateSelectInput(session, inputId = "selectUserForDelete",
                            choices = credentials()$info[["user"]], 
                            selected = credentials()$info[["user"]])
        }
        
        # for non-admins and non-managers, only show sub-tables in their master table
      } else {
        
        # check if the user has a master table
        hasTbl <- dbExistsTable(conn = conn, name = credentials()$info[["user"]])
        if(hasTbl){
          showTblExistsErrorDelete(FALSE)
          userPedsForDelete(sort(unique(
            dbGetQuery(conn = conn,
                       statement = paste0("SELECT PedigreeID FROM ", 
                                          credentials()$info[["user"]], 
                                          ";"))$PedigreeID
          )))
          
          # update the tables available for deletion
          updateSelectInput(session, inputId = "selectDeletePeds",
                            choices = userPedsForDelete())
        } else {
          showTblExistsErrorDelete(TRUE)
        }
      }
    }
  })
  
  # for admins and managers, update the pedigrees available based on the user account selected
  observeEvent(list(input$selectUserForDelete, userPedsForDelete()), {
    userPedsForDelete(sort(unique(
      dbGetQuery(conn = conn,
                 statement = paste0("SELECT PedigreeID FROM ",
                                    ifelse(input$selectUserForDelete == "", credentials()$info[["user"]],
                                           input$selectUserForDelete),
                                    ";"))$PedigreeID
    )))
    updateSelectInput(session, inputId = "selectDeletePeds",
                      choices = userPedsForDelete())
  }, ignoreInit = T, ignoreNULL = T)

  # select or de-select all pedigrees in a user account for deletion
  observeEvent(list(userPedsForDelete(), input$selectAllPedsDelete), {
    if(input$selectAllPedsDelete){
      updateSelectInput(session, "selectDeletePeds",
                        selected = userPedsForDelete(),
                        choices = userPedsForDelete())
    } else {
      updateSelectInput(session, "selectDeletePeds",
                        choices = userPedsForDelete())
    }
  }, ignoreInit = T)

  # check that deletion is possible based on if at least one pedigree is selected
  # and enable/disable deletePeds button accordingly
  observeEvent(input$selectDeletePeds, {
    if(!is.null(input$selectDeletePeds)){
      waitDelete(TRUE)
      shinyjs::delay(delay_delete_ms, {
        shinyjs::enable("deletePeds")
        waitDelete(FALSE)
      })
    } else {
      waitDelete(FALSE)
      shinyjs::disable("deletePeds")
    }
  }, ignoreNULL = F)
  waitDelete <- reactiveVal(FALSE)
  output$waitDelete <- renderText({ need(!waitDelete(), "Please wait...") })
  
  # user confirms deletion
  observeEvent(input$deletePeds, {
    showModal(modalDialog(
      tagList(p("Are you sure you want to delete the selected pedigrees from your account? 
                You cannot undo this action.")),
      title = "Deletion Confirmation",
      footer = tagList(
        actionButton("confirmDeletePeds", "Delete"),
        modalButton("Cancel")
      )
    ))
  })
  
  # delete the selected pedigrees
  observeEvent(input$confirmDeletePeds, {
    if(admin() | manager()){
      fromAcct <- ifelse(input$selectUserForDelete == "", credentials()$info[["user"]],
                         input$selectUserForDelete)
    } else {
      fromAcct <- credentials()$info[["user"]]
    }
    
    # if the pedigree to be deleted is the one currently being edited, remove it first
    # and force the user to either create a new pedigree or load an existing one to continue
    if(input$pedID %in% input$selectDeletePeds){
      PED(NULL)
      hideTab("navbarTabs", target = "Create/Modify Pedigree", session = session)
    }
    
    dbExecute(conn = conn,
              statement = paste0("DELETE FROM ", fromAcct, 
                                 " WHERE PedigreeID IN ('", 
                                 paste0(input$selectDeletePeds, collapse = "','"), 
                                 "');"))
    
    # update the list of pedigrees that can be loaded, downloaded, and deleted
    updated.peds <- sort(unique(
      dbGetQuery(conn = conn,
                 statement = paste0("SELECT PedigreeID FROM ", fromAcct, ";"))$PedigreeID
    ))
    userPeds(updated.peds)
    userPedsForDownload(updated.peds)
    userPedsForDelete(updated.peds)
    userPedsForCopyFrom(updated.peds)
    userPedsForCopyTo(updated.peds)
    
    # remove the confirmation pop-up window
    removeModal()
    
    # notify user of delete success
    shinybusy::notify_success("Deletion Successful!", position = "center-bottom")
    
  }, ignoreInit = T)
  
  #### Edit Pedigree ####
  # manually save the pedigree, useful for pre-logout save and to update the pedigree viewers
  # with information just entered on an editor tab without requiring the user to change tabs
  observeEvent(input$manualPedSave, {
    if(!is.null(PED())){
      PED(saveRelDatCurTab(tped = PED(), rel = input$relSelect, inp = input,
                           cr = canReactive$canNums,
                           sr = surgReactive$lst,
                           gr = geneReactive$GeneNums,
                           dupResultGene = dupResultGene(),
                           sx = ifelse(input$relSelect == "Female", 0, 1))
      )
      
      # if tab is switched prior to pedigree being visualized, when the parent's
      # race/eth/anc cannot be individually set, assume those values from the proband
      if(input$pedTabs == "Demographics" & !showPed() & newOrLoadFlag() == "new"){
        PED(assumeBackground(PED(), id = PED()$MotherID[which(PED()$isProband == 1)]))
        PED(assumeBackground(PED(), id = PED()$FatherID[which(PED()$isProband == 1)]))
      }
      
      # save pedigree to database
      savePedigreeToDB(conne = conn,
                       user = credentials()$info[["user"]],
                       tmp_tbl = PED())
      
      # avoid bug when a pedigree is loaded when the tabs that are displaying a data table are selected
      updateTabsetPanel(session, "geneTabs", selected = "Instructions")
      updateTabsetPanel(session, "pedVisualsViewer", selected = "Tree")
      updateTabsetPanel(session, "pedVisualsEditor", selected = "Tree")
      updateTabsetPanel(session, "ppResultTabs", selected = "Carrier Prob. Plot")
      
      # send pedigree to pedigreeJS
      session$sendCustomMessage("updatePedJSHandler", prepPedJSON(PED()))
    }
  }, ignoreInit = TRUE)
  
  ##### Demographics / Create Pedigree ####
  # user confirms proband is deceased
  observeEvent(list(input$isDead, PED()), {
    popup <- FALSE
    if(!is.null(PED())){
      if(PED()$isProband[which(PED()$ID == as.numeric(input$relSelect))] ==  1 & 
         PED()$isDead[which(PED()$ID == as.numeric(input$relSelect))] == 0 & 
         input$isDead){
        popup <- TRUE
      }
    } else if(is.null(PED()) & input$isDead){
      popup <- TRUE
    }
    
    if(popup){
      showModal(modalDialog(
        tagList(p("Are you sure that the proband is deceased?")),
        title = "Deceased Proband Confirmation",
        footer = tagList(
          actionButton("notDead", "Not Deceased"),
          modalButton("Deceased")
        )
      ))
    }
  }, ignoreNULL = F, ignoreInit = T)
  
  # undo deceased status based on user input to popup
  observeEvent(input$notDead, {
    updateCheckboxInput(session, "isDead", value = FALSE)
    removeModal()
  }, ignoreInit = T)
  
  # validate current age
  validAge <- reactive({
    if(is.null(PED()) | input$isDead){
      return(shiny::validate(validateAge(input$Age)))
    } else {
      
      # check for parents and get parent's ages
      mom.age <- PED()$CurAge[which(PED()$ID == PED()$MotherID[which(PED()$ID == as.numeric(input$relSelect))])]
      dad.age <- PED()$CurAge[which(PED()$ID == PED()$FatherID[which(PED()$ID == as.numeric(input$relSelect))])]
      
      # check for deceased status and eliminate those from error checking
      mom.dead <- PED()$isDead[which(PED()$ID == PED()$MotherID[which(PED()$ID == as.numeric(input$relSelect))])] == 1
      dad.dead <- PED()$isDead[which(PED()$ID == PED()$FatherID[which(PED()$ID == as.numeric(input$relSelect))])] == 1
      
      # determine age that should be used for each parent
      if(length(mom.age) == 0){
        mom.age <- NA
      } else if(mom.dead){
        mom.age <- NA
      }
      if(length(dad.age) == 0){
        dad.age <- NA
      } else if(dad.dead){
        dad.age <- NA
      }
      
      # determine youngest parent
      if(all(is.na(c(mom.age, dad.age)))){
        youngest.parent.age <- NA
      } else {
        youngest.parent.age <- min(c(mom.age, dad.age), na.rm = T)
      }
      
      # check for children and get their ages
      children.ids <- c(PED()$ID[which(PED()$MotherID == as.numeric(input$relSelect))],
                        PED()$ID[which(PED()$FatherID == as.numeric(input$relSelect))])
      if(length(children.ids) > 0){
        
        # get ages of living children only 
        child.ages <- PED()$CurAge[which(PED()$ID %in% children.ids & PED()$isDead == 0)]
        if(length(child.ages) > 0){
          if(all(is.na(child.ages))){
            oldest.child.age <- NA
          } else {
            oldest.child.age <- max(child.ages, na.rm = T)
          }
        } else {
          oldest.child.age <- NA
        }
      } else {
        oldest.child.age <- NA
      }
      
      return(shiny::validate(validateAge(input$Age, oldest.child.age, youngest.parent.age)))
    }
  })
  output$validAge <- renderText({ validAge() })
  
  # enable sex input if the selected relative is not the proband and if the relative has children
  observeEvent(input$relSelect, {
    if(pedExists()){
      if(PED()$Sex[which(PED()$ID == as.numeric(input$relSelect))] == 0){
        rel.kids <- PED()$ID[which(PED()$MotherID == as.numeric(input$relSelect))]
      } else {
        rel.kids <- PED()$ID[which(PED()$FatherID == as.numeric(input$relSelect))]
      }
      if(!PED()$isProband[which(PED()$ID == as.numeric(input$relSelect))] & 
         length(rel.kids) == 0){
        shinyjs::enable(id = "Sex")
      } else {
        shinyjs::disable(id = "Sex")
      }
    }
  }, ignoreNULL = T, ignoreInit = T)
  
  # do not allow user to move to other pedTabs if there is not enough information to make the pedigree
  # also disable/enable create pedigree button based on presense/absense of minimum necessary information
  pbMinInfo <- reactiveVal(FALSE)
  output$pbMinInfo <- reactive({ pbMinInfo() })
  outputOptions(output, 'pbMinInfo', suspendWhenHidden = FALSE)
  observeEvent(list(input$pedID, input$Sex, input$Age, validAge(), PED()), {
    
    # check if minimum information has been entered/conditions met to start a new pedigree
    if(is.null(PED()) &
       newOrLoadFlag() == "new" & 
       input$pedID != "" & 
       input$Sex != " " & 
       !is.na(input$Age) & 
       is.null(validAge()) & 
       !nonUniqPedID()){
      pbMinInfo(TRUE)
    } else if(!is.null(PED())){
      pbMinInfo(TRUE)
    } else {
      pbMinInfo(FALSE)
    }
    
    # enable/disable create pedigree button
    if(pbMinInfo()){
      shinyjs::enable("createPed")
    } else {
      shinyjs::disable("createPed")
    }
  }, ignoreInit = F)
  
  # warn user if the pedigreeID they are trying to use is already a pedigree in the user's table
  nonUniqPedID <- reactiveVal(FALSE)
  output$nonUniqPedID <- reactive({ nonUniqPedID() })
  outputOptions(output, 'nonUniqPedID', suspendWhenHidden = FALSE)
  observeEvent(list(userPeds(), input$pedID, PED()), {
    if(!is.null(userPeds()) & newOrLoadFlag() != "load" & is.null(PED())){
      if(any(userPeds() == input$pedID) | input$pedID == "example_pedigree"){
        nonUniqPedID(TRUE)
      } else {
        nonUniqPedID(FALSE)
      }
    } else {
      nonUniqPedID(FALSE)
    }
  }, ignoreNULL = F, ignoreInit = T)
  
  # hide/show tabs if the minimum information to create a pedigree is present or not and the pedigree has been created
  observeEvent(list(pbMinInfo(), PED()), {
    if(pbMinInfo() & !is.null(PED())){
      showTab("pedTabs", "Cancer Hx", select = FALSE, session)
      showTab("pedTabs", "CBC Risk", select = FALSE, session)
      showTab("pedTabs", "Surgical Hx", select = FALSE, session)
      showTab("pedTabs", "Tumor Markers", select = FALSE, session)
      showTab("pedTabs", "Genes", select = FALSE, session)
      
      # only show add relatives tab if it is a pedigree being created, not loaded
      if(newOrLoadFlag() == "new" & !showPed()){
        showTab("pedTabs", "Add Relatives", select = FALSE, session)
      } 
    } else {
      hideTab("pedTabs", "Cancer Hx", session)
      hideTab("pedTabs", "CBC Risk", session)
      hideTab("pedTabs", "Surgical Hx", session)
      hideTab("pedTabs", "Tumor Markers", session)
      hideTab("pedTabs", "Genes", session)
      hideTab("pedTabs", "Add Relatives", session)
    }
  })
  
  # instantiate the pedigree reactive
  PED <- reactiveVal(NULL)
  pedExists <- reactiveVal(FALSE)
  output$pedExists <- reactive({ pedExists() })
  outputOptions(output, 'pedExists', suspendWhenHidden = FALSE)
  observeEvent(PED(), {
    if(!is.null(PED())){
      pedExists(TRUE)
      
      
      
      # # FOR TESTING
      # View(PED())
      
      
      
      
    } else {
      pedExists(FALSE)
    }
  }, ignoreInit = T, ignoreNULL = F)
  
  # initialize the pedigree when the user clicks the "Create pedigree" buttons
  observeEvent(input$createPed, {
    
    # lock fields
    shinyjs::disable("pedID")
    shinyjs::disable("Sex")
    
    # initialize new pedigree with proband and parents
    if(input$Sex == "Female"){
      ps <- 0
    } else if(input$Sex == "Male"){
      ps <- 1
    } else {
      ps <- NA
    }
    PED(initPed(pedigree.id = input$pedID, pb.sex = ps))
    
    # populate proband's age and deceased status
    PED(popPersonData(tmp.ped = PED(), id = input$relSelect, cur.age = input$Age, is.dead = input$isDead))
    
    # save to database
    savePedigreeToDB(conne = conn,
                     user = credentials()$info[["user"]],
                     tmp_tbl = PED())
    
    # update the list of pedigrees that can be loaded, downloaded, and deleted
    updated.peds <- sort(unique(
      dbGetQuery(conn = conn,
                 statement = paste0("SELECT PedigreeID FROM ", credentials()$info[["user"]], ";"))$PedigreeID
    ))
    userPeds(updated.peds)
    userPedsForDownload(updated.peds)
    userPedsForDelete(updated.peds)
    userPedsForCopyFrom(updated.peds)
    userPedsForCopyTo(updated.peds)
  })
  
  # populate the age, race, ethnicity, and ancestry values in the pedigree whenever the user leaves the demographics tab
  onDemoTab <- reactiveVal(TRUE)
  observeEvent(input$pedTabs, {
    
    # execute if the previous tab was the proband demographics tab and the current tab is different
    if(onDemoTab() & input$pedTabs != "Demographics" & !is.null(PED())){
      
      # populate proband's demographics data and PedigreeID
      PED(popPersonData(tmp.ped = PED(), id = input$relSelect, 
                        cur.age = input$Age, is.dead = input$isDead,
                        sx = input$Sex,
                        rc = input$race, et = input$eth, 
                        an.aj = input$ancAJ, an.it = input$ancIt))
      
      # if the mother and father's information cannot be set individually yet (pedigree has not been visualized)
      # then assume they have the same background as the proband
      if(!showPed()){
        PED(assumeBackground(PED(), id = PED()$MotherID[which(PED()$isProband == 1)]))
        PED(assumeBackground(PED(), id = PED()$FatherID[which(PED()$isProband == 1)]))
      }
      
      # save to database
      savePedigreeToDB(conne = conn,
                       user = credentials()$info[["user"]],
                       tmp_tbl = PED())
      
      # update pedigreeJS
      if(showPed()){
        session$sendCustomMessage("updatePedJSHandler", prepPedJSON(PED()))
      }
      
    } # if statement to check whether the pedigree could be created based on the tab and minimum info required
    
    # update the reactive value to detect if the current tab is the target tab
    if(input$pedTabs == "Demographics"){
      onDemoTab(TRUE)
    } else {
      onDemoTab(FALSE)
    }
  }, ignoreInit = TRUE)
  
  ##### Cancer History ####
  # save the number of cancers for each person in the pedigree
  canReactive <- reactiveValues(canNums = trackCans.init)
  
  # add a cancer UI module on button click and advance the module counter
  observeEvent(input$addCan, {
    rel <- input$relSelect
    sex <- input$Sex
    out <- addCancer(cr = canReactive$canNums, 
                     rel = rel, 
                     inp = input,
                     sex = sex)
    canReactive$canNums <- out$cr
    trackMax <- out$trackMax
    id <- out$id
    
    # add a server for checking the validity of the entered cancer age
    observeEvent(list(input[[paste0(id, "-CanAge")]], input$Age), {
      validateCanAgeServer(id,
                           in.age = input[[paste0(id, "-CanAge")]],
                           cur.age = PED()$CurAge[which(PED()$ID == rel)])
    })
    
    # add a server for checking the validity of the entered CBC age
    observeEvent(list(input[[paste0(id, "-CBCAge")]], 
                      input$Age,
                      input[[paste0(id, "-Can")]], 
                      input[[paste0(id, "-CanAge")]]), {
      validateCBCAgeServer(id,
                           can = input[[paste0(id, "-Can")]],
                           cbc.age = input[[paste0(id, "-CBCAge")]],
                           bc.age = input[[paste0(id, "-CanAge")]],
                           cur.age = PED()$CurAge[which(PED()$ID == rel)])
    })
    
    ### Cancer UI Remove Observer
    # create a remove module button observer for each UI module created
    observeEvent(input[[paste0(id, '-removeCan')]], {
      canReactive$canNums <- removeCancer(cr = canReactive$canNums,
                                          rel = rel,
                                          inp = input,
                                          ss = session,
                                          trackMax = trackMax)

      # remove the module's inputs from memory
      remove_shiny_inputs(id, input)
    })
    
    ## observe for BC and CBC
    observeEvent(list(input[[paste0(id, '-Can')]], input[[paste0(id, '-CBC')]]), {
      
      # reset CBC inputs if cancer is not BC
      if(input[[paste0(id, '-Can')]] != "Breast"){
        shinyjs::reset(id = paste0(id, '-CBC'))
        shinyjs::reset(id = paste0(id, '-CBCAge'))
      }
      
      # reset CBC age if CBC is no longer selected
      if(input[[paste0(id, '-CBC')]] == "No"){
        shinyjs::reset(id = paste0(id, '-CBCAge'))
      }
    }, ignoreInit = T, ignoreNULL = T)
    

    ## create a cancer selection observer which will trigger an update of all of the cancer dropdown
    ## choices for each of the person's cancer UI modules
    observeEvent(input[[paste0(id, '-Can')]], {
      updateCancerDropdowns(cr = canReactive$canNums,
                            rel = rel,
                            inp = input,
                            ss = session,
                            type = "cancer")
    })

    ## create an OTHER cancer selection observer which will trigger an update of all of the OTHER cancer dropdown
    ## choices for each of the person's cancer UI modules
    observeEvent(input[[paste0(id, '-CanOther')]], {
      updateCancerDropdowns(cr = canReactive$canNums,
                            rel = rel,
                            inp = input,
                            ss = session,
                            type = "other")
    })
  })
  
  # add data to pedigree when user navigates off of the tab
  onCanTab <- reactiveVal(FALSE)
  observeEvent(input$pedTabs, {
    
    # consolidate cancer info into a data frame
    can.df <- makeCancerDF(rel = input$relSelect, cr = canReactive$canNums, inp = input)
    
    # transfer information to the pedigree
    if(onCanTab() & input$pedTabs != "Cancer Hx" & !is.null(PED())){
      PED(popPersonData(tmp.ped = PED(), id = input$relSelect, sx = input$Sex, cancers.and.ages = can.df))
      
      # save pedigree to database
      savePedigreeToDB(conne = conn,
                       user = credentials()$info[["user"]],
                       tmp_tbl = PED())
      
      # update pedigreeJS
      if(showPed()){
        session$sendCustomMessage("updatePedJSHandler", prepPedJSON(PED()))
      }
    }
    
    # update the reactive value to detect if the current tab is the target tab
    if(input$pedTabs == "Cancer Hx"){
      onCanTab(TRUE)
    } else {
      onCanTab(FALSE)
    }
  }, ignoreInit = TRUE)
  
  ##### CBC Risk ####
  # keep track of whether CBC inputs should be shown or not
  showCBCinputs <- reactiveVal(FALSE)
  output$showCBCinputs <- reactive({ showCBCinputs() })
  outputOptions(output, 'showCBCinputs', suspendWhenHidden = FALSE)
  observeEvent(list(PED(), input$relSelect, canReactive$canNums), {
    if(is.null(PED())){
      showCBCinputs(FALSE)
    } else {
      
      # check updated cancers list for presence of BC and CBC
      if(PED()$isAffBC[which(PED()$ID == as.numeric(input$relSelect))] == 1 & 
         PED()$isAffCBC[which(PED()$ID == as.numeric(input$relSelect))] == 0 &
         PED()$Sex[which(PED()$ID == as.numeric(input$relSelect))] == 0){ 
        showCBCinputs(TRUE)
      } else if(showCBCinputs()){
        showCBCinputs(FALSE)
        for(cbc.var in cbcrisk.cols){
          shinyjs::reset(cbc.var)
        }
      } else {
        showCBCinputs(FALSE)
      }
      
      # check if any previously recorded CBC related inputs need to be removed and update them
      if(!showCBCinputs() & 
         any(!is.na(PED()[which(PED()$ID == as.numeric(input$relSelect)), cbcrisk.cols]))){
        PED(popPersonData(tmp.ped = PED(), id = input$relSelect, 
                          cbc.info = list(FirstBCType = input$FirstBCType,
                                          AntiEstrogen = input$AntiEstrogen,
                                          HRPreneoplasia = input$HRPreneoplasia,
                                          BreastDensity = input$BreastDensity,
                                          FirstBCTumorSize = input$FirstBCTumorSize)))
        
        # save pedigree to database
        savePedigreeToDB(conne = conn,
                         user = credentials()$info[["user"]],
                         tmp_tbl = PED())
      }
    }
  })
  
  # add data to pedigree when user navigates off of the tab
  onCBCTab <- reactiveVal(FALSE)
  observeEvent(input$pedTabs, {
    
    # consolidate cancer info into a data frame to check for BC and CBC
    can.df <- makeCancerDF(rel = input$relSelect, cr = canReactive$canNums, inp = input)
    
    # transfer information to the pedigree if conditions are met
    if(onCBCTab() & input$pedTabs != "CBC Risk" & !is.null(PED()) & 
       any(can.df$Cancer == "Breast") & all(can.df$Cancer != "Contralateral") & input$Sex == "Female"){
      PED(popPersonData(tmp.ped = PED(), id = input$relSelect, 
                        cbc.info = list(FirstBCType = input$FirstBCType,
                                        AntiEstrogen = input$AntiEstrogen,
                                        HRPreneoplasia = input$HRPreneoplasia,
                                        BreastDensity = input$BreastDensity,
                                        FirstBCTumorSize = input$FirstBCTumorSize)
                        )
          )
      
      # save pedigree to database
      savePedigreeToDB(conne = conn,
                       user = credentials()$info[["user"]],
                       tmp_tbl = PED())
    }
    
    # update the reactive value to detect if the current tab is the target tab
    if(input$pedTabs == "CBC Risk"){
      onCBCTab(TRUE)
    } else {
      onCBCTab(FALSE)
    }
  }, ignoreInit = TRUE)
  
  ##### Tumor Markers ####
  # condition to inform UI whether to display tumor markers or not, based on cancer hx
  showBCMarkers <- reactiveVal(FALSE)
  showCRCMarkers <- reactiveVal(FALSE)
  output$showBCMarkers <- reactive({ showBCMarkers() })
  output$showCRCMarkers <- reactive({ showCRCMarkers() })
  outputOptions(output, 'showBCMarkers', suspendWhenHidden = FALSE)
  outputOptions(output, 'showCRCMarkers', suspendWhenHidden = FALSE)
  observeEvent(list(PED(), input$relSelect), {
    if(is.null(PED())){
      showBCMarkers(FALSE)
      showCRCMarkers(FALSE)
    } else {
    
      # check updated cancers list for presence of tumor marker related cancers
      if(PED()$isAffBC[which(PED()$ID == as.numeric(input$relSelect))] == 1){ 
        hadBC <- TRUE 
        showBCMarkers(TRUE)
      } else {
        hadBC <- FALSE
        showBCMarkers(FALSE)
      }
      
      
      if(PED()$isAffCOL[which(PED()$ID == as.numeric(input$relSelect))] == 1){ 
        hadCRC <- TRUE 
        showCRCMarkers(TRUE)
      } else {
        hadCRC <- FALSE
        showCRCMarkers(FALSE)
      }
      
      # check if any previously recorded markers need to be removed and update the inputs
      rmBCmarks <- FALSE
      rmCRCmarks <- FALSE
      if(!hadBC & any(!is.na(PED()[which(PED()$ID == as.numeric(input$relSelect)), PanelPRO:::MARKER_TESTING$BC$MARKERS]))){
        rmBCmarks <- TRUE
        for(m in PanelPRO:::MARKER_TESTING$BC$MARKERS){
          m <- ifelse(m == "CK5.6", "CK56", m)
          updateSelectInput(session, m, selected = "Not Tested")
        }
      }
      
      if(!hadCRC & any(!is.na(PED()[which(PED()$ID == as.numeric(input$relSelect)), PanelPRO:::MARKER_TESTING$COL$MARKERS]))){
        rmCRCmarks <- TRUE
        for(m in PanelPRO:::MARKER_TESTING$COL$MARKERS){
          updateSelectInput(session, m, selected = "Not Tested")
        }
      }
      
      # update tumor markers in pedigree if required
      if(rmBCmarks | rmCRCmarks){
        PED(popPersonData(tmp.ped = PED(), id = input$relSelect, 
                          er = input$ER, pr = input$PR, her2 = input$HER2,
                          ck5.6 = input$CK56, ck14 = input$CK14, msi = input$MSI))
        
        # save pedigree to database
        savePedigreeToDB(conne = conn,
                         user = credentials()$info[["user"]],
                         tmp_tbl = PED())
      }
    }
  })
  
  # add data to pedigree when user navigates off of the tab
  onMarkerTab <- reactiveVal(FALSE)
  observeEvent(list(input$pedTabs), {
    if(onMarkerTab() & input$pedTabs != "Tumor Markers" & !is.null(PED())){
      PED(popPersonData(tmp.ped = PED(), id = input$relSelect, 
                        er = input$ER, pr = input$PR, her2 = input$HER2,
                        ck5.6 = input$CK56, ck14 = input$CK14, msi = input$MSI))
      
      # save pedigree to database
      savePedigreeToDB(conne = conn,
                       user = credentials()$info[["user"]],
                       tmp_tbl = PED())
    }
    
    # update the reactive value to detect if the current tab is the target tab
    if(input$pedTabs == "Tumor Markers"){
      onMarkerTab(TRUE)
    } else {
      onMarkerTab(FALSE)
    }
  }, ignoreInit = TRUE, ignoreNULL = FALSE)
  
  
  ##### Surgical History ####
  # store for prophylactic surgeries
  surgReactive <- reactiveValues(lst = riskmods.inputs.store)
  observeEvent(list(input$Mast, input$MastAge), {
    surgReactive$lst[["riskmod"]][which(names(surgReactive$lst[["riskmod"]]) == "mast")] <- input$Mast
    surgReactive$lst[["interAge"]][which(names(surgReactive$lst[["interAge"]]) == "mast")] <- input$MastAge
  }, ignoreInit = TRUE)
  observeEvent(list(input$Hyst, input$HystAge), {
    surgReactive$lst[["riskmod"]][which(names(surgReactive$lst[["riskmod"]]) == "hyst")] <- input$Hyst
    surgReactive$lst[["interAge"]][which(names(surgReactive$lst[["interAge"]]) == "hyst")] <- input$HystAge
  }, ignoreInit = TRUE)
  observeEvent(list(input$Ooph, input$OophAge), {
    surgReactive$lst[["riskmod"]][which(names(surgReactive$lst[["riskmod"]]) == "ooph")] <- input$Ooph
    surgReactive$lst[["interAge"]][which(names(surgReactive$lst[["interAge"]]) == "ooph")] <- input$OophAge
  }, ignoreInit = TRUE)
  
  ## if a surgery is unchecked, reset the surgery age value
  # Mast
  observeEvent(input$Mast, {
    if(!input$Mast){
      updateNumericInput(session, "MastAge", value = NA)
    }
  })
  
  # Ooph
  observeEvent(input$Ooph, {
    if(!input$Ooph){
      updateNumericInput(session, "OophAge", value = NA)
    }
  })
  
  # Hyst
  observeEvent(input$Hyst, {
    if(!input$Hyst){
      updateNumericInput(session, "HystAge", value = NA)
    }
  })
  
  # if sex is changed from female to male, clear all surgical data from inputs and ped
  observeEvent(list(input$Sex), {
    if(!is.null(PED())){
      if(PED()$Sex[which(PED()$ID == as.numeric(input$relSelect))] == 0){
        if(input$Sex == "Male"){
          for(sg in c("Mast", "Hyst", "Ooph")){
            updateCheckboxInput(session, sg, value = FALSE)
            updateNumericInput(session, paste0(sg,"Age"), value = NA)
            tmp.ped <- PED()
            tmp.ped[[paste0("riskmod", sg)]][which(tmp.ped$ID == input$relSelect)] <- 0
            tmp.ped[[paste0("interAge", sg)]][which(tmp.ped$ID == input$relSelect)] <- NA
            PED(tmp.ped)
          }
          
          # save pedigree to database
          savePedigreeToDB(conne = conn,
                           user = credentials()$info[["user"]],
                           tmp_tbl = PED())
        }
      }
    }
  })
  
  ## validate surgery ages
  # Oophorectomy age
  validOophAge <- reactive({
    shiny::validate(validateSurgAge(input$OophAge, input$Age, PED()$AgeOC[which(PED()$ID == as.numeric(input$relSelect))]))
  })
  output$validOophAge <- renderText({ validOophAge() })
  
  # Mastectomy age
  validMastAge <- reactive({
    shiny::validate(validateSurgAge(input$MastAge, input$Age, PED()$AgeCBC[which(PED()$ID == as.numeric(input$relSelect))]))
  })
  output$validMastAge <- renderText({ validMastAge() })
  
  # Hysterectomy age
  validHystAge <- reactive({
    shiny::validate(validateSurgAge(input$HystAge, input$Age, PED()$AgeENDO[which(PED()$ID == as.numeric(input$relSelect))]))
  })
  output$validHystAge <- renderText({ validHystAge() })
  
  # add data to pedigree when user navigates off of the tab
  onSurgTab <- reactiveVal(FALSE)
  observeEvent(input$pedTabs, {
    if(onSurgTab() & input$pedTabs != "Surgical Hx" & !is.null(PED())){
      PED(popPersonData(tmp.ped = PED(), id = input$relSelect, riskmods.and.ages = surgReactive$lst))
      
      # save pedigree to database
      savePedigreeToDB(conne = conn,
                       user = credentials()$info[["user"]],
                       tmp_tbl = PED())
    }
    
    # update the reactive value to detect if the current tab is the target tab
    if(input$pedTabs == "Surgical Hx"){
      onSurgTab(TRUE)
    } else {
      onSurgTab(FALSE)
    }
  }, ignoreInit = TRUE)
  
  
  ##### Genes ####
  # track the panel and gene UI modules
  geneReactive <- reactiveValues(GeneNums = trackGenes.init)
  
  ###### Panels ####
  # get panel name choices from the database
  observe({
    all.pans <- 
      sort(dbGetQuery(conn = conn,
                      statement = "SELECT panel_name FROM panels")$panel_name)
    
    # update the panel selector
    updateSelectInput(session, "existingPanels", choices = c("No panel selected", "Create new", all.pans))
  })
  
  # create new panel
  observeEvent(input$createPanel, {
    all.pans <- 
      dbGetQuery(conn = conn,
                 statement = "SELECT panel_name FROM panels")$panel_name
    
    if(input$newPanelName != "" &
       !input$newPanelName %in% all.pans &
       !is.null(input$newPanelGenes)){
      pan.genes <- paste0(input$newPanelGenes, collapse = ",")
      dbExecute(conn = conn,
                statement = paste0("INSERT INTO panels (panel_name,genes) VALUES ('", 
                                   input$newPanelName,"','", pan.genes,"');"))
      
      # reset/update inputs
      shinyjs::reset("newPanelName")
      shinyjs::reset("newPanelGenes")
      all.pans <- 
        sort(dbGetQuery(conn = conn,
                        statement = "SELECT panel_name FROM panels")$panel_name)
      updateSelectInput(session, "existingPanels", 
                        choices = c("No panel selected", "Create new", all.pans))
      updateTextInput(session, "newPanelName", value = "")
      updateSelectizeInput(session, "newPanelGenes", selected = NULL)
    }
  }, ignoreInit = T)

  # disable Create Panel button if conditions are not met
  observeEvent(list(input$newPanelName, input$newPanelGenes), {
    all.pans <- 
      dbGetQuery(conn = conn,
                 statement = "SELECT panel_name FROM panels")$panel_name
  
    if(input$newPanelName == "" |
       input$newPanelName %in% all.pans |
       is.null(input$newPanelGenes)){
      waitNewPanel(FALSE)
      shinyjs::disable("createPanel")
      if(input$newPanelName %in% all.pans){
        panelNameUnique(FALSE)
      } else {
        panelNameUnique(TRUE)
      }
    } else {
      panelNameUnique(TRUE)
      waitNewPanel(TRUE)
      shinyjs::delay(delay_insert_ms, {
        shinyjs::enable("createPanel")
        waitNewPanel(FALSE)
      })
    }
  }, ignoreNULL = F, ignoreInit = F)
  waitNewPanel <- reactiveVal(FALSE)
  output$waitNewPanel <- renderText({ need(!waitNewPanel(), "Please wait...") })
  
  # warning message to user about duplicate panel name
  panelNameUnique <- reactiveVal(TRUE)
  output$validPanelName <- renderText({
    shiny::validate(need(panelNameUnique(), "This panel name is already taken, please choose another."))
  })
  
  ### check if the current relative has a least one panel
  atLeastOnePanel <- reactiveVal(FALSE)
  output$atLeastOnePanel <- reactive({ atLeastOnePanel() })
  outputOptions(output, 'atLeastOnePanel', suspendWhenHidden = FALSE)
  observeEvent(list(geneReactive$GeneNums, input$relSelect), {
    
    # if statement to address bug fix that occurs when resetting inputs during create new pedigree process 
    # while previous relSelect was not the proband, mother, or father
    if(any(names(geneReactive$GeneNums) == input$relSelect)){
      if(geneReactive$GeneNums[[input$relSelect]]$panels$panel1$name == "No panel selected"){
        atLeastOnePanel(FALSE)
      } else {
        atLeastOnePanel(TRUE)
      }
    } else {
      atLeastOnePanel(FALSE)
    }
  })

  ### add an existing panel
  observeEvent(input$addPanel, {
    
    # check that the selected panel is an actual panel
    if(!input$existingPanels %in% c("No panel selected", "Create new")){
      rel <- input$relSelect
      pan.name <- input$existingPanels
      out <- addPanel(gr = geneReactive$GeneNums,
                      rel = rel,
                      inp = input,
                      ss = session,
                      pan.name = pan.name,
                      conn = conn)
      geneReactive$GeneNums <- out$gr
      panel.module.id.num <- out$panel.module.id.num
      panMod.id <- out$panMod.id
      
      ## panelUI Remove Observer
      # create a remove module button observer for each panelUI module created
      observeEvent(input[[paste0(panMod.id, '-removePanel')]], {
        out.rm <- removePanel(gr = geneReactive$GeneNums,
                              rel = rel,
                              pan.name = pan.name,
                              panel.module.id.num = panel.module.id.num,
                              inp = input,
                              ss = session,
                              conn = conn)
        geneReactive$GeneNums <- out.rm$gr
        
        # remove each geneUI module associated with this panel from memory
        for(tmp.geneMod.id in out.rm$panel.geneMod.ids){
          remove_shiny_inputs(tmp.geneMod.id, input)
        }
        
        # remove the panel module's inputs from memory
        remove_shiny_inputs(panMod.id, input)
        
      }) # end of removePanel observeEvent
    } # end of if statement to check if the request to create the new panel had a valid panel name
  }) # end of observeEvent for creating a new panelUI module

  ###### PLP Genes ####
  # add a PLP geneUI module on button click and advance the module counter
  # note, a conditionalPanel ensures the button is only displayed if the relative
  # has a least on panel
  observeEvent(input$addPLP, {
    if(input$editPanel != "No panel selected"){
      rel <- input$relSelect
      p.name <- input$editPanel
      rtype <- "PLP"
      out <- addGene(gr = geneReactive$GeneNums,
                     rel = rel,
                     inp = input,
                     p.name = p.name,
                     rtype = rtype)
      geneReactive$GeneNums <- out$gr
      gene.module.id.num <- out$gene.module.id.num
      geneMod.id <- out$geneMod.id

      # create a remove module button observer for each UI module created
      observeEvent(input[[paste0(geneMod.id, '-removeGene')]], {
        geneReactive$GeneNums <-
          removeGene(gr = geneReactive$GeneNums,
                     rel = rel,
                     inp = input,
                     p.name = p.name,
                     gene.module.id.num = gene.module.id.num,
                     geneMod.id = geneMod.id,
                     rtype = rtype)

        # remove the module's inputs from memory
        remove_shiny_inputs(geneMod.id, input)
      })
    }
  }, ignoreInit = TRUE)


  ###### VUS Genes ####
  # add a VUS geneUI module on button click and advance the module counter
  # note, a conditionalPanel ensures the button is only displayed if the relative
  # has a least on panel
  observeEvent(input$addVUS, {
    if(input$editPanel != "No panel selected"){
      rel <- input$relSelect
      p.name <- input$editPanel
      rtype <- "VUS"
      out <- addGene(gr = geneReactive$GeneNums,
                     rel = rel,
                     inp = input,
                     p.name = p.name,
                     rtype = rtype)
      geneReactive$GeneNums <- out$gr
      gene.module.id.num <- out$gene.module.id.num
      geneMod.id <- out$geneMod.id

      # create a remove module button observer for each UI module created
      observeEvent(input[[paste0(geneMod.id, '-removeGene')]], {
        geneReactive$GeneNums <-
          removeGene(gr = geneReactive$GeneNums,
                     rel = rel,
                     inp = input,
                     p.name = p.name,
                     gene.module.id.num = gene.module.id.num,
                     geneMod.id = geneMod.id,
                     rtype = rtype)

        # remove the module's inputs from memory
        remove_shiny_inputs(geneMod.id, input)
      })
    }
  }, ignoreInit = TRUE)


  ###### BLB Genes ####
  # add a BLB geneUI module on button click and advance the module counter
  # note, a conditionalPanel ensures the button is only displayed if the relative
  # has a least on panel
  observeEvent(input$addBLB, {
    if(input$editPanel != "No panel selected"){
      rel <- input$relSelect
      p.name <- input$editPanel
      rtype <- "BLB"
      out <- addGene(gr = geneReactive$GeneNums,
                     rel = rel,
                     inp = input,
                     p.name = p.name,
                     rtype = rtype)
      geneReactive$GeneNums <- out$gr
      gene.module.id.num <- out$gene.module.id.num
      geneMod.id <- out$geneMod.id

      # create a remove module button observer for each UI module created
      observeEvent(input[[paste0(geneMod.id, '-removeGene')]], {
        geneReactive$GeneNums <-
          removeGene(gr = geneReactive$GeneNums,
                     rel = rel,
                     inp = input,
                     p.name = p.name,
                     gene.module.id.num = gene.module.id.num,
                     geneMod.id = geneMod.id,
                     rtype = rtype)

        # remove the module's inputs from memory
        remove_shiny_inputs(geneMod.id, input)
      })
    }
  }, ignoreInit = TRUE)

  ###### Summary Table & Store ####
  # indicator same gene is listed in more than one result category
  dupResultGene <- reactiveVal(FALSE)
  output$dupResultGene <- reactive({ dupResultGene() })
  outputOptions(output, 'dupResultGene', suspendWhenHidden = FALSE)

  # panel summary table
  panelSum <- reactive({
    out <- makeGeneDF(rel = input$relSelect, gr = geneReactive$GeneNums,
                      dupResultGene = dupResultGene(),
                      inp = input)
    dupResultGene(out$dupResultGene)
    out$df
  })

  # output formatted data table colored by result type
  output$panelSum <- DT::renderDT({
    if(!is.null(panelSum())){
      datatable(panelSum()) %>%
        formatStyle('Result',
                    backgroundColor = styleEqual(c("P/LP","VUS","B/LB","Neg"),
                                                 c("MistyRose","LightGreen","AliceBlue","white")),
                    fontWeight = 'bold')
    }
  }, server = F)

  # add data to pedigree when user navigates off of the tab
  onGeneTab <- reactiveVal(FALSE)
  observeEvent(input$pedTabs, {
    if(onGeneTab() & input$pedTabs != "Genes" & !is.null(PED())){
      PED(popPersonData(tmp.ped = PED(), id = input$relSelect,
                        gene.results = panelSum()))
      
      # save pedigree to database
      savePedigreeToDB(conne = conn,
                       user = credentials()$info[["user"]],
                       tmp_tbl = PED())
    }
    
    # update the reactive value to detect if the current tab is the target tab
    if(input$pedTabs == "Genes"){
      onGeneTab(TRUE)
    } else {
      onGeneTab(FALSE)
    }
  }, ignoreInit = TRUE)
  
  ##### Add Kids, Sibs, Aunts/Uncles ####
  # validate quantities of relatives
  validDauQty <- reactive({
    shiny::validate(validateRelNums(input$numDau))
  })
  output$validDauQty <- renderText({ validDauQty() })
  
  validSonQty <- reactive({
    shiny::validate(validateRelNums(input$numSon))
  })
  output$validSonQty <- renderText({ validSonQty() })
  
  validSisQty <- reactive({
    shiny::validate(validateRelNums(input$numSis))
  })
  output$validSisQty <- renderText({ validSisQty() })
  
  validBroQty <- reactive({
    shiny::validate(validateRelNums(input$numBro))
  })
  output$validBroQty <- renderText({ validBroQty() })
  
  validMAuntQty <- reactive({
    shiny::validate(validateRelNums(input$numMAunt))
  })
  output$validMAuntQty <- renderText({ validMAuntQty() })
  
  validMUncQty <- reactive({
    shiny::validate(validateRelNums(input$numMUnc))
  })
  output$validMUncQty <- renderText({ validMUncQty() })
  
  validPAuntQty <- reactive({
    shiny::validate(validateRelNums(input$numPAunt))
  })
  output$validPAuntQty <- renderText({ validPAuntQty() })
  
  validPUncQty <- reactive({
    shiny::validate(validateRelNums(input$numPUnc))
  })
  output$validPUncQty <- renderText({ validPUncQty() })
  
  # disable showPed button if any relative quantities are invalid
  observe({
    total.rel.qty.errors <- sum(
      length(validateRelNums(input$numDau)),
      length(validateRelNums(input$numSon)),
      length(validateRelNums(input$numSis)),
      length(validateRelNums(input$numBro)),
      length(validateRelNums(input$numMAunt)),
      length(validateRelNums(input$numMUnc)),
      length(validateRelNums(input$numPAunt)),
      length(validateRelNums(input$numPUnc))
    )
    if(total.rel.qty.errors > 0){
      shinyjs::disable("showPedButton")
    } else if(total.rel.qty.errors == 0){
      shinyjs::enable("showPedButton")
    }
  })
  
  
  # add relatives to the pedigree when the user click the button at bottom of screen
  # populate assumed races and ancestries based on proband's mother and father info
  showPedButtonCnt <- reactiveVal(0)
  showPed <- reactiveVal(FALSE)
  output$showPed <- reactive({ showPed() })
  outputOptions(output, 'showPed', suspendWhenHidden = FALSE)
  observeEvent(input$showPedButton, {
    
    # update reactive value which triggers showing/hiding the visualized pedigree
    showPed(TRUE)
    
    # keep track of the 1st time a pedigree is shown that pedigreeJS can be either 
    # updated or created from scratch
    if(showPedButtonCnt() < 2){
      showPedButtonCnt(showPedButtonCnt()+1)
    }
    
    # only add family members if this is a new pedigree
    if(newOrLoadFlag() == "new"){
      
      # add children
      if(input$numDau > 0 | input$numSon > 0){
        
        # first, add partner to pedigree
        PED(formatNewPerson(relation = "partner", tmp.ped = PED()))
        parnter.id <- PED()$ID[nrow(PED())]
        t.ped <- PED()
        t.ped$name[nrow(t.ped)] <- "Proband Partner 1"
        PED(t.ped)
        
        # add daughters iteratively
        if(input$numDau > 0){
          for(i in 1:input$numDau){
            if(PED()$Sex[which(PED()$isProband == 1)] == 0){
              PED(formatNewPerson(relation = "daughter", tmp.ped = PED(), f.id = parnter.id))
            } else if(PED()$Sex[which(PED()$isProband == 1)] == 1){
              PED(formatNewPerson(relation = "daughter", tmp.ped = PED(), m.id = parnter.id))
            }
            
            # add unique number to name field
            t.ped <- PED()
            t.ped$name[nrow(t.ped)] <- paste0(t.ped$name[nrow(t.ped)], " ", i)
            PED(t.ped)
          }
        }
        # add sons iteratively
        if(input$numSon > 0){
          for(i in 1:input$numSon){
            if(PED()$Sex[which(PED()$isProband == 1)] == 0){
              PED(formatNewPerson(relation = "son", tmp.ped = PED(), f.id = parnter.id))
            } else if(PED()$Sex[which(PED()$isProband == 1)] == 1){
              PED(formatNewPerson(relation = "son", tmp.ped = PED(), m.id = parnter.id))
            }
            
            # add unique number to name field
            t.ped <- PED()
            t.ped$name[nrow(t.ped)] <- paste0(t.ped$name[nrow(t.ped)], " ", i)
            PED(t.ped)
          }
        }
      }
      
      # add sisters iteratively
      if(input$numSis > 0){
        for(i in 1:input$numSis){
          PED(formatNewPerson(relation = "sister", tmp.ped = PED()))
          
          # add unique number to name field
          t.ped <- PED()
          t.ped$name[nrow(t.ped)] <- paste0(t.ped$name[nrow(t.ped)], " ", i)
          PED(t.ped)
        }
      }
      # add brothers iteratively
      if(input$numBro > 0){
        for(i in 1:input$numBro){
          PED(formatNewPerson(relation = "brother", tmp.ped = PED()))
          
          
          # add unique number to name field
          t.ped <- PED()
          t.ped$name[nrow(t.ped)] <- paste0(t.ped$name[nrow(t.ped)], " ", i)
          PED(t.ped)
        }
      }
      
      # add maternal aunts and uncles
      if(input$numMAunt > 0 | input$numMUnc > 0){
        
        # first, create maternal grandparents, add a cancer counter for each
        # assume, initially, race/eth/ancestry match the proband's mother
        PED(formatNewPerson(relation = "grandmother", tmp.ped = PED(), m.or.p.side = "m"))
        PED(formatNewPerson(relation = "grandfather", tmp.ped = PED(), m.or.p.side = "m"))
        
        # add maternal aunts iteratively
        if(input$numMAunt > 0){
          for(i in 1:input$numMAunt){
            PED(formatNewPerson(relation = "aunt", tmp.ped = PED(), m.or.p.side = "m"))
            
            # add unique number to name field
            t.ped <- PED()
            t.ped$name[nrow(t.ped)] <- paste0(t.ped$name[nrow(t.ped)], " ", i)
            PED(t.ped)
          }
        }
        # add maternal uncles iteratively
        if(input$numMUnc > 0){
          for(i in 1:input$numMUnc){
            PED(formatNewPerson(relation = "uncle", tmp.ped = PED(), m.or.p.side = "m"))
            
            # add unique number to name field
            t.ped <- PED()
            t.ped$name[nrow(t.ped)] <- paste0(t.ped$name[nrow(t.ped)], " ", i)
            PED(t.ped)
          }
        }
      }
      
      # add paternal aunts and uncles
      if(input$numPAunt > 0 | input$numPUnc > 0){
        
        # first, create paternal grandparents, add a cancer counter,
        # assume, initially, race/eth/ancestry match the proband's father
        PED(formatNewPerson(relation = "grandmother", tmp.ped = PED(), m.or.p.side = "p"))
        PED(formatNewPerson(relation = "grandfather", tmp.ped = PED(), m.or.p.side = "p"))
        
        # add paternal aunts iteratively
        if(input$numPAunt > 0){
          for(i in 1:input$numPAunt){
            PED(formatNewPerson(relation = "aunt", tmp.ped = PED(), m.or.p.side = "p"))
            
            # add unique number to name field
            t.ped <- PED()
            t.ped$name[nrow(t.ped)] <- paste0(t.ped$name[nrow(t.ped)], " ", i)
            PED(t.ped)
          }
        }
        # add paternal uncles iteratively
        if(input$numPUnc > 0){
          for(i in 1:input$numPUnc){
            PED(formatNewPerson(relation = "uncle", tmp.ped = PED(), m.or.p.side = "p"))
            
            # add unique number to name field
            t.ped <- PED()
            t.ped$name[nrow(t.ped)] <- paste0(t.ped$name[nrow(t.ped)], " ", i)
            PED(t.ped)
          }
        }
      }
      
      # assume race/eth/ancestry and create module tracking for each new relative
      for(np in PED()$ID[3:nrow(PED())]){
        PED(assumeBackground(PED(), id = np))
        canReactive$canNums[[as.character(np)]] <- trackCans.rel
        geneReactive$GeneNums[[as.character(np)]] <- relTemplate.trackGenes
      }
      
      # save pedigree to database
      savePedigreeToDB(conne = conn,
                       user = credentials()$info[["user"]],
                       tmp_tbl = PED())
      
      # get names and abbreviated names
      selector.names <- 
        PED() %>%
        select(ID, name) %>%
        mutate(long_name = name)
      selector.names <- abb.Relations(selector.names)
      selector.names <- 
        selector.names %>%
        mutate(combined_name = paste0(long_name, " (", name, ")"))
      
      # update relative selector with all relatives in the pedigree
      updateSelectInput(session = session, inputId = "relSelect", 
                        choices = setNames(selector.names$ID, selector.names$combined_name), 
                        selected = PED()$ID[which(PED()$isProband == 1)])
      
    } # end of if statement for confirming the pedigree is a new creation
    
    # hide initialize pedigree tab and reset inputs
    for(relation in c("Dau", "Son", "Sis", "Bro", "MAunt", "MUnc", "PAunt", "PUnc")){
      updateNumericInput(session, paste0("num", relation), value = 0)
    }
    hideTab("pedTabs", target = "Add Relatives", session = session)
    
    # create interactive pedigeejs pedigree if a any pedigree, even a different family,
    # has not been displayed yet, otherwise update pedigreeJS instead
    if(showPedButtonCnt() == 1){
      session$sendCustomMessage("createPedJSHandler", prepPedJSON(PED()))
    } else if(showPedButtonCnt() > 1) {
      session$sendCustomMessage("updatePedJSHandler", prepPedJSON(PED()))
    }
  }, ignoreInit = TRUE)
  
  ##### Visualize Pedigree ####
  ###### PedigreeJS ####
  
  
  
  
  # # FOR TESTING - diplay the pedigreeJS JSON as a string
  # output$pedJSJSON <- renderText(input$pedJSJSON)
  
  
  
  
  # at a specified time interval, refresh the JSON pedigree
  autoInvalidate <- reactiveTimer(intervalMs = 1000)
  observe({
    autoInvalidate()
    shinyjs::click("GetPedJSButton")
  })
  
  # when the JSON pedigree updates:
  # 1: update the R pedigree (PED()) by removing any deleted relatives and 
  #    changing the pedigreeJS 'name' values to match the formatting of the R 
  #    pedigree 'ID' values.
  # 2: if any pedigreeJS 'name'/R 'ID' values were changed, pass an updated JSON
  #    string back to pedigreeJS
  observeEvent(input$pedJSJSON, {
    
    # only do this after the pedigree has been visualized
    if(showPed()){
      
      # convert JSON pedigree object from pedigreeJS to data frame
      pjs <- fromJSON(input$pedJSJSON, simplifyDataFrame = T)
      
      View(pjs)
      
      # get the numeric ids from the pedigreeJS pedigree
      num.ids <- as.numeric(pjs$name[which(varhandle::check.numeric(pjs$name))])
      
      # get a copy of the R data frame master pedigree
      r.ped <- PED()

      # check if relatives were added or deleted
      if(nrow(pjs) != nrow(r.ped)){

        # check if there were deletions of relatives and update the pedigree in R
        if(nrow(pjs) < nrow(r.ped)){
          del.rels <- r.ped$ID[which(!as.character(r.ped$ID) %in% pjs$name)]
          for(dr in del.rels){
            r.ped <- subset(r.ped, ID != dr)
  
            # update master pedigree
            PED(r.ped)
  
            # remove relative from panel and cancer hx tracking
            canReactive$canNums[[as.character(dr)]] <- NULL
            geneReactive$GeneNums[[as.character(dr)]] <- NULL
          }

          # not a deletion, therefore it was an addition, so
          # update the pedigree in R,
          # and change the new names, display_names, and statuses in pedigreeJS
        } else {

          # check if one relative was added
          # this means either a sibling or child, not necessarily a relative to the proband,
          # was added in pedigreeJS
          if(nrow(pjs) == nrow(r.ped)+1){
            
            # change the ID/name
            # note that the ID column is R is equivalent to the name property in pedigreeJS
            # and note that the name column in R is equivalend to the display_name property in pedigreeJS
            new.id <- max(num.ids)+1
            target.rels <- as.character(new.id)
            pjs$name[which(!varhandle::check.numeric(pjs$name))] <- as.character(new.id)
            
            # updated R and pedigreeJS pedigrees
            out <- addPJSrel(pjs = pjs,
                             r.ped = r.ped,
                             target.rel = target.rels,
                             type = "sib-child")
            
            # check if two relatives were added
            # (this means either a set of parents or a partner and child were added in pedigreeJS)
          } else if(nrow(pjs) == nrow(r.ped)+2){
            
            # identify the two new additions
            added.rels <- pjs$name[which(!varhandle::check.numeric(pjs$name))]
            
            # check if the two additions were parents
            if((isTRUE(pjs$top_level[which(pjs$name == added.rels[1])]) &
               isTRUE(pjs$top_level[which(pjs$name == added.rels[2])])) |
              (isTRUE(pjs$noparents[which(pjs$name == added.rels[1])]) &
               isTRUE(pjs$noparents[which(pjs$name == added.rels[2])]))){
              
              # identify the male/female parents
              if(pjs$sex[which(pjs$name == added.rels[1])] == "M"){
                added.malep <- added.rels[1]
                added.femalep <- added.rels[2]
              } else {
                added.malep <- added.rels[2]
                added.femalep <- added.rels[1]
              }
              
              # change string ids for the new parents to numeric names/IDs
              new.fp.id <- max(num.ids)+1
              new.mp.id <- max(num.ids)+2
              target.rels <- as.character(c(new.fp.id, new.mp.id))
              pjs$name[which(pjs$name == added.femalep)] <- as.character(new.fp.id)
              pjs$name[which(pjs$name == added.malep)] <- as.character(new.mp.id)
              pjs$mother[which(pjs$mother == added.femalep)] <- as.character(new.fp.id)
              pjs$father[which(pjs$father == added.malep)] <- as.character(new.mp.id)

              # add the female parent then the male parent
              out.f <- addPJSrel(pjs = pjs[which(pjs$name != added.malep),],
                                 r.ped = r.ped,
                                 target.rel = as.character(new.fp.id),
                                 type = "parent",
                                 pjs.full = pjs)
              out <- addPJSrel(pjs = out.f$pjs_updated,
                               r.ped = out.f$r.ped_updated,
                               target.rel = as.character(new.mp.id),
                               type = "parent",
                               pjs.full = out.f$pjs_updated)
            
              # they were not parents, the two additions were a partner and a child
            } else {
              
              # identify which is the partner and which is the child
              if(isTRUE(pjs$noparents[which(pjs$name == added.rels[1])])){
                added.partner <- added.rels[1]
                added.child <- added.rels[2]
              } else {
                added.partner <- added.rels[2]
                added.child <- added.rels[1]
              }
              
              # change string ids for the new parents to numeric names/IDs
              new.par.id <- max(num.ids)+1
              new.child.id <- max(num.ids)+2
              target.rels <- as.character(c(new.par.id, new.child.id))
              pjs$name[which(pjs$name == added.partner)] <- as.character(new.par.id)
              pjs$name[which(pjs$name == added.child)] <- as.character(new.child.id)
              if(pjs$sex[which(pjs$name == as.character(new.par.id))] == "F"){
                pjs$mother[which(pjs$mother == added.partner)] <- as.character(new.par.id)
              } else if(pjs$sex[which(pjs$name == as.character(new.par.id))] == "M"){
                pjs$father[which(pjs$father == added.partner)] <- as.character(new.par.id)
              }
              
              # identify the partner's partner
              partner.of <- 
                as.character(
                  pjs[which(pjs$name == as.character(new.child.id)), c("mother","father")]
                )
              partner.of <- partner.of[which(partner.of != as.character(new.par.id))]
              
              # add the partner then the child
              out.part <- addPJSrel(pjs = pjs,
                                    r.ped = r.ped,
                                    target.rel = as.character(new.par.id),
                                    type = "partner",
                                    partner.of = partner.of)
              out <- addPJSrel(pjs = out.part$pjs_updated,
                               r.ped = out.part$r.ped_updated,
                               target.rel = as.character(new.child.id),
                               type = "sib-child")
            }
            
            # warn if more than two relatives were added
          } else if(nrow(pjs) > nrow(r.ped)+2){
            base::message("More than two relatives were added between pedigreeJS pedigree JSON refreshes.")
          }
          
          # update master pedigree
          PED(out$r.ped_updated)
          
          # assume race/eth/ancestry and create module tracking for each new relative
          for(tr in as.numeric(target.rels)){
            PED(assumeBackground(PED(), id = tr))
            canReactive$canNums[[as.character(tr)]] <- trackCans.rel
            geneReactive$GeneNums[[as.character(tr)]] <- relTemplate.trackGenes
          }
          
          # push the updated pedigree back to pedigreeJS
          pjs.json <- toJSON(out$pjs_updated, dataframe = "rows", na = "null", pretty = TRUE)
          session$sendCustomMessage("updatePedJSHandler", pjs.json)
          
        } # end of else where relatives were added
        
        # save pedigree to database
        savePedigreeToDB(conne = conn,
                         user = credentials()$info[["user"]],
                         tmp_tbl = PED())
        
        # get names and abbreviated names
        selector.names <- 
          PED() %>%
          select(ID, name) %>%
          mutate(long_name = name)
        selector.names <- abb.Relations(selector.names)
        selector.names <- 
          selector.names %>%
          mutate(combined_name = paste0(long_name, " (", name, ")"))
        
        # update relative selector with all relatives in the pedigree
        # if the selected person was deleted then make the proband the selected relativee
        if(all(selector.names$ID != input$relSelect)){
          updateSelectInput(session = session, inputId = "relSelect",
                            choices = setNames(selector.names$ID, selector.names$combined_name),
                            selected = PED()$ID[which(PED()$isProband == 1)])
          
          # otherwise keep the current selection
        } else {
          updateSelectInput(session = session, inputId = "relSelect",
                            choices = setNames(selector.names$ID, selector.names$combined_name))
        }
      } # end of if statement to check if pedigrees have the same number of rows
    } # end of if statement to check if pedigree has been displayed yet (showPed())
  }, ignoreInit = T, ignoreNULL = T)
  
  ###### kinship2 Tree ####
  ## draw pedigree static image using kinship2
  # prepare the data
  treePed <- reactive({
    if(!is.null(PED())){
      return(kinship2.ped(PED()))
    } else {
      return(NULL)
    }
  })
  
  # display in the pedigree viewer
  output$treePedViewer <- renderPlot({
    shiny::validate(
      need(!is.null(treePed()) & !is.null(PED()), "No pedigree has been loaded or created yet."),
    )
    
    # check if there are any cancers which determines if the legend should be included
    cans <- select(PED(), starts_with("isAff"))
    
    # plot
    plot(treePed()[paste0(input$pedID)])
    if(sum(cans) > 0){
      pedigree.legend(treePed()[paste0(input$pedID)], location = "bottomright", radius=0.1)
    }
  })
  
  ###### Pedigree Table ####
  # prepare the data frame
  tablePed <- reactive({
    
    if(!is.null(PED())){
      t.ped <- 
        PED() %>% 
        select(-c("PedigreeID", 
                  "side", 
                  "relationship", 
                  "isProband",
                  starts_with("isAff"),
                  starts_with("Age"),
                  "cancersJSON",
                  "panelNames",
                  "genesJSON")) %>%
        select(-any_of(PanelPRO:::GENE_TYPES)) %>%
        relocate(name, .after = "ID") %>%
        relocate(Sex, .after = "name") %>%
        relocate(Twins, .after = "isDead")
        
      return(
        DT::datatable(data = t.ped,
                      rownames = FALSE,
                      class = list("nowrap", "stripe", "compact", "cell-border"),
                      extensions = "FixedColumns",
                      options = list(pageLength = 20,
                                     scrollX = TRUE,
                                     fixedColumns = list(leftColumns = 2)))
      )
    } else {
      return(NULL)
    }
  })
  
  # display in the pedigree editor
  output$tablePedEditor <- DT::renderDT({
    shiny::validate(
      need(!is.null(tablePed()) & !is.null(PED()), "No pedigree has been loaded or created yet."),
    )
    tablePed()
  }, server = F)
  
  # display in the pedigree viewer
  output$tablePedViewer <- DT::renderDT({
    shiny::validate(
      need(!is.null(tablePed()) & !is.null(PED()), "No pedigree has been loaded or created yet."),
    )
    tablePed()
  }, server = F)
  
  ###### Cancer Details ####
  ## cancer details, convert JSONs into a data frame of cancer hx indexed by ID
  cancersTbl <- reactive({
    can.df <- NULL
    if(!is.null(PED())){
      for(row in 1:nrow(PED())){
        if(!is.na(PED()$cancersJSON[row])){
          mod.can.JSON <- gsub(pattern = "\'", replacement = "\"", PED()$cancersJSON[row])
          rel.can.df <- fromJSON(mod.can.JSON, simplifyDataFrame = T)
          colnames(rel.can.df) <- stri_trans_totitle(colnames(rel.can.df))
          rel.can.df <- cbind(data.frame(ID = rep(PED()$ID[row], nrow(rel.can.df)),
                                         Name = rep(PED()$name[row], nrow(rel.can.df))), 
                              rel.can.df)
          if(is.null(can.df)){
            can.df <- rel.can.df
          } else {
            can.df <- rbind(can.df, rel.can.df)
          }
        }
      }
    }
    
    # if there was no cancer hx for any relatives return a 0 row data frame
    if(is.null(can.df)){
      return(setNames(as.data.frame(matrix(NA, nrow = 0, ncol = 2 + length(colnames(cancer.inputs.store)))), 
                      c("ID", "Name", stri_trans_totitle(colnames(cancer.inputs.store)))))
    } else {
      return(can.df)
    }
  })
  
  # display in the pedigree editor
  output$cancersTblEditor <- DT::renderDT({
    shiny::validate(
      need(!is.null(cancersTbl()) & !is.null(PED()), "No pedigree has been loaded or created yet."),
    )
    cancersTbl()
  }, server = F)
  
  # display in the pedigree viewer
  output$cancersTblViewer <- DT::renderDT({
    shiny::validate(
      need(!is.null(cancersTbl()) & !is.null(PED()), "No pedigree has been loaded or created yet."),
    )
    cancersTbl()
  }, server = F)
  
  ###### Panel Details ####
  ## panel details, convert JSONs into a data frame of panel and gene data indexed by ID
  genesTbl <- reactive({
    gene.df <- NULL
    if(!is.null(PED())){
      for(row in 1:nrow(PED())){
        if(!is.na(PED()$genesJSON[row])){
          mod.gene.JSON <- gsub(pattern = "\'", replacement = "\"", PED()$genesJSON[row])
          rel.gene.df <- fromJSON(mod.gene.JSON, simplifyDataFrame = T)
          colnames(rel.gene.df) <- stri_trans_totitle(colnames(rel.gene.df))
          rel.gene.df <- cbind(data.frame(ID   = rep(PED()$ID[row], nrow(rel.gene.df)),
                                          Name = rep(PED()$name[row], nrow(rel.gene.df))), 
                               rel.gene.df)
          if(is.null(gene.df)){
            gene.df <- rel.gene.df
          } else {
            gene.df <- rbind(gene.df, rel.gene.df)
          }
        }
      }
    }
    
    # if there was no gene info for any relatives return a 0 row data frame
    if(is.null(gene.df)){
      return(setNames(as.data.frame(matrix(NA, nrow = 0, ncol = 2 + length(gene.df.colnames))), 
                      c("ID", "Name", stri_trans_totitle(gene.df.colnames))))
    } else {
      return(gene.df)
    }
  })
  
  # display in the pedigree editor
  output$genesTblEditor <- DT::renderDT({
    shiny::validate(
      need(!is.null(genesTbl()) & !is.null(PED()), "No pedigree has been loaded or created yet."),
    )
    
    # save space by abbreviating column names and removing row number
    gt <- genesTbl()
    colnames(gt) <- c("ID", "Name", "Panel", "Gene", "Res", "Nuc", "Prot", "Zyg")
    gt <- datatable(gt, rownames = F)
    gt
  }, server = F)
  
  # display in the pedigree viewer
  output$genesTblViewer <- DT::renderDT({
    shiny::validate(
      need(!is.null(genesTbl()) & !is.null(PED()), "No pedigree has been loaded or created yet."),
    )
    genesTbl()
  }, server = F)
  
  ###### Download Data Dictionary ####
  ## download just the data dictionary zip file
  # pedigree previewer
  output$downloadDD1 <- shiny::downloadHandler(
    filename = function(){
      paste0("PanelPRO-data-dictionary-", Sys.Date(), ".zip")
    },
    content = function(file){
      
      # create .csv files and zip them together
      write.csv(ppCancersDict(), file = "data-dictionary/panelpro-cancer-abbreviations.csv", row.names = F)
      write.csv(ppGenes(), file = "data-dictionary/panelpro-gene-list.csv", row.names = F)
      new.files <- c(
        "data-dictionary/panelpro-cancer-abbreviations.csv",
        "data-dictionary/panelpro-gene-list.csv"
      )
      tmp.zip <- 
        zip::zip(zipfile = file, 
                 files = c("data-dictionary/columns-and-codings-dictionary.csv",
                           "data-dictionary/README.md",
                           new.files))
      
      # remove the created files
      file.remove(new.files)
      
      return(tmp.zip)
    },
    contentType = "application/zip"
  )
  
  # pedigree editor viewer
  output$downloadDD2 <- shiny::downloadHandler(
    filename = function(){
      paste0("PanelPRO-data-dictionary-", Sys.Date(), ".zip")
    },
    content = function(file){
      
      # create .csv files and zip them together
      write.csv(ppCancersDict(), file = "data-dictionary/panelpro-cancer-abbreviations.csv", row.names = F)
      write.csv(ppGenes(), file = "data-dictionary/panelpro-gene-list.csv", row.names = F)
      new.files <- c(
        "data-dictionary/panelpro-cancer-abbreviations.csv",
        "data-dictionary/panelpro-gene-list.csv"
      )
      tmp.zip <- 
        zip::zip(zipfile = file, 
                 files = c("data-dictionary/columns-and-codings-dictionary.csv",
                           "data-dictionary/README.md",
                           new.files))
      
      # remove the created files
      file.remove(new.files)
      
      return(tmp.zip)
    },
    contentType = "application/zip"
  )
  
  # Save data to pedigree when navbarTabs change or one of the viewer tabs in the editor
  observeEvent(list(input$navbarTabs), {
    if(!is.null(PED())){
      PED(saveRelDatCurTab(tped = PED(), rel = input$relSelect, inp = input,
                           cr = canReactive$canNums,
                           sr = surgReactive$lst,
                           gr = geneReactive$GeneNums,
                           dupResultGene = dupResultGene(),
                           sx = ifelse(input$relSelect == "Female", 0, 1))
      )
      
      # if tab is switched prior to pedigree being visualized, when the parent's
      # race/eth/anc cannot be individually set, assume those values from the proband
      if(input$pedTabs == "Demographics" & !showPed() & newOrLoadFlag() == "new"){
        PED(assumeBackground(PED(), id = PED()$MotherID[which(PED()$isProband == 1)]))
        PED(assumeBackground(PED(), id = PED()$FatherID[which(PED()$isProband == 1)]))
      }
      
      # save pedigree to database
      savePedigreeToDB(conne = conn,
                       user = credentials()$info[["user"]],
                       tmp_tbl = PED())
      
      # avoid bug when a pedigree is loaded when the tabs that are displaying a data table are selected
      updateTabsetPanel(session, "geneTabs", selected = "Instructions")
      updateTabsetPanel(session, "pedVisualsViewer", selected = "Tree")
      updateTabsetPanel(session, "pedVisualsEditor", selected = "Tree")
      updateTabsetPanel(session, "ppResultTabs", selected = "Carrier Prob. Plot")
    }
  }, ignoreInit = TRUE)
  
  ##### Switch Selected Relative ####
  # initialize the ID of the last relative selected with proband
  lastRel <- reactiveVal(1)
  
  # 1) save data to pedigree when the relative is switched or if navbarTabs change
  # 2) repopulate inputs with new relative's data from the pedigree
  observeEvent(input$relSelect, {
    
    # only execute if a pedigree has been created
    if(!is.null(PED())){
      
      # save data for previously selected relative to the pedigree
      PED(saveRelDatCurTab(tped = PED(), rel = lastRel(), inp = input,
                           cr = canReactive$canNums,
                           sr = surgReactive$lst,
                           gr = geneReactive$GeneNums,
                           dupResultGene = dupResultGene(),
                           sx = PED()$Sex[which(PED()$ID == lastRel())])
          )

      # save pedigree to database
      savePedigreeToDB(conne = conn,
                       user = credentials()$info[["user"]],
                       tmp_tbl = PED())
      
      # update the last relative selected
      lastRel(as.numeric(input$relSelect))
      
      # Re-populate data for new person
      rel.info <- PED()[which(PED()$ID == as.numeric(input$relSelect)),]
      updateRelInputs(rel.info = rel.info, ss = session, conn = conn)
      
      # reset the selected tabs
      updateTabsetPanel(session, "geneTabs", selected = "Instructions")
      updateTabsetPanel(session, "geneResultTabs", selected = "P/LP")
    }
  }, ignoreInit = TRUE)
  
  # change instructions and headers based on the selected relative
  relOrProband <- reactive({
    if(is.null(PED())){
      return("Proband")
    } else {
      return(PED()$name[which(PED()$ID == input$relSelect)])
    }
  })
  output$rop1 <- renderText({ paste0(relOrProband(),"'s") })          # Demographics heading
  output$rop2 <- renderText({ paste0(tolower(relOrProband()),"'s") }) # Demographics instructions
  output$rop3 <- renderText({ paste0(relOrProband(),"'s") })          # Cancer Hx heading
  output$rop4 <- renderText({                                         # Cancer Hx instructions
    if(grepl(pattern = "[[:digit:]]$", relOrProband())){
      return(tolower(relOrProband()))
    } else {
      return(paste0("the ", tolower(relOrProband())))
    }
  })     
  output$rop5 <- renderText({ paste0(relOrProband(),"'s") })          # CBC Risk heading
  output$rop6 <- renderText({                                         # CBC Risk cannot display warning
    if(grepl(pattern = "[[:digit:]]$", relOrProband())){
      return(tolower(relOrProband()))
    } else {
      return(paste0("the ", tolower(relOrProband())))
    }
  })    
  output$rop7 <- renderText({ paste0(relOrProband(),"'s") })          # Tumor Marker heading
  output$rop8 <- renderText({                                         # Tumor Marker cannot display warning
    if(grepl(pattern = "[[:digit:]]$", relOrProband())){
      return(tolower(relOrProband()))
    } else {
      return(paste0("the ", tolower(relOrProband())))
    }
  })
  output$rop9 <- renderText({                                         # Tumor Marker instructions
    if(grepl(pattern = "[[:digit:]]$", relOrProband())){
      return(tolower(relOrProband()))
    } else {
      return(paste0("the ", tolower(relOrProband())))
    }
  })
  output$rop10 <- renderText({ paste0(relOrProband(),"'s") })          # Surgical Hx heading
  output$rop11 <- renderText({                                         # Surgical Hx instructions
    if(grepl(pattern = "[[:digit:]]$", relOrProband())){
      return(tolower(relOrProband()))
    } else {
      return(paste0("the ", tolower(relOrProband())))
    }
  })
  
  # Gene/Panel relative titles
  output$rop12 <- renderText({ paste0(relOrProband(),"'s") })
  output$rop13 <- renderText({                                         
    if(grepl(pattern = "[[:digit:]]$", relOrProband())){
      return(paste0(tolower(relOrProband()), "."))
    } else {
      return(paste0("the ", tolower(relOrProband()), "."))
    }
  })
  output$rop14 <- renderText({ paste0(relOrProband(),"'s") })
  output$rop15 <- renderText({                                         
    if(grepl(pattern = "[[:digit:]]$", relOrProband())){
      return(relOrProband())
    } else {
      return(paste0("The ", tolower(relOrProband())))
    }
  })
  output$rop16 <- renderText({  paste0(relOrProband(),"'s") })
  output$rop17 <- renderText({                                         
    if(grepl(pattern = "[[:digit:]]$", relOrProband())){
      return(paste0(tolower(relOrProband()), "'s"))
    } else {
      return(paste0("the ", tolower(relOrProband()), "'s"))
    }
  })
  output$rop18 <- renderText({  paste0(relOrProband(),"'s") })
  output$rop19 <- renderText({                                         
    if(grepl(pattern = "[[:digit:]]$", relOrProband())){
      return(relOrProband())
    } else {
      return(paste0("The ", tolower(relOrProband())))
    }
  })
  output$rop20 <- renderText({                                         
    if(grepl(pattern = "[[:digit:]]$", relOrProband())){
      return(paste0(tolower(relOrProband()), "."))
    } else {
      return(paste0("the ", tolower(relOrProband()), "."))
    }
  })
  output$rop21 <- renderText({                                         
    if(grepl(pattern = "[[:digit:]]$", relOrProband())){
      return(tolower(relOrProband()))
    } else {
      return(paste0("the ", tolower(relOrProband())))
    }
  })
  
  #### Manage navbarTabs ####
  # on start-up, hide the non-Home navbarTabs (should not show until a pedigree is create as new or loaded)
  observe({
    hideTab("navbarTabs", target = "Create/Modify Pedigree", session = session)
    hideTab("navbarTabs", target = "PanelPRO", session = session)
  })
  
  # only show PanelPRO tab when a pedigree has been created or loaded
  observeEvent(PED(), {
    if(!is.null(PED())){
      showTab("navbarTabs", target = "PanelPRO", session = session)
    } else {
      hideTab("navbarTabs", target = "PanelPRO", session = session)
    }
  }, ignoreNULL = F)
  
  # Save data to pedigree when one of the viewer tabs in the editor
  observeEvent(input$pedVisualsEditor, {
    if(!is.null(PED())){
      PED(saveRelDatCurTab(tped = PED(), rel = input$relSelect, inp = input,
                           cr = canReactive$canNums,
                           sr = surgReactive$lst,
                           gr = geneReactive$GeneNums,
                           dupResultGene = dupResultGene(),
                           sx = ifelse(input$relSelect == "Female", 0, 1))
      )
      
      # if tab is switched prior to pedigree being visualized, when the parent's
      # race/eth/anc cannot be individually set, assume those values from the proband
      if(input$pedTabs == "Demographics" & !showPed() & newOrLoadFlag() == "new"){
        PED(assumeBackground(PED(), id = PED()$MotherID[which(PED()$isProband == 1)]))
        PED(assumeBackground(PED(), id = PED()$FatherID[which(PED()$isProband == 1)]))
      }
      
      # save pedigree to database
      savePedigreeToDB(conne = conn,
                       user = credentials()$info[["user"]],
                       tmp_tbl = PED())
      
      # avoid bug when a pedigree is loaded when the tabs that are displaying a data table are selected
      updateTabsetPanel(session, "geneTabs", selected = "Instructions")
      updateTabsetPanel(session, "pedVisualsViewer", selected = "Tree")
      updateTabsetPanel(session, "ppResultTabs", selected = "Carrier Prob. Plot")
    }
  }, ignoreInit = TRUE)
  
  #### PanelPRO ####
  # panelpro version number
  output$ppVersion <- renderText({
    paste0("version: ", packageVersion("PanelPRO"))
  })
  
  ##### Settings ####
  # reset to default input values
  observeEvent(input$resetPanelPROInputs, {
    settingInputs <- c("modelSpec", "genes", "cancers",
                       "maxMut", "ageBy",
                       "unknownRace", "unknownAncestry",
                       "allowInter", "ignorePbGerm",
                       "missAgeIters", "missAgeMaxIters",
                       "randomSeed", "netOrCrude")
    for(si in settingInputs){ shinyjs::reset(si) }
  })
  
  # clear genes and cancers when the model specification is not custom
  observeEvent(input$modelSpec, {
    if(input$modelSpec != "Custom"){
      shinyjs::reset("genes")
      shinyjs::reset("cancers")
      shinyjs::reset("allGenes")
      shinyjs::reset("allCancers")
    }
  }, ignoreInit = T)
  
  # show cancers and genes in each of the model specifications
  output$modSpecCancers <- renderText({
    if(input$modelSpec == "Custom"){
      return(NULL)
    } else {
      mcans <- PanelPRO:::MODELPARAMS[[input$modelSpec]]$CANCERS
      ncans <- length(mcans)
      return(paste(ncans, "Cancers:", paste0(mcans, collapse = ", ")))
    }
  })
  
  # show cancers and genes in each of the model specifications
  output$modSpecGenes <- renderText({
    if(input$modelSpec == "Custom"){
      return(NULL)
    } else {
      mgenes <- PanelPRO:::MODELPARAMS[[input$modelSpec]]$GENES
      ngenes <- length(mgenes)
      return(paste(ngenes, "Genes:", paste0(mgenes, collapse = ", ")))
    }
  })
  
  # select all genes
  observeEvent(input$allGenes, {
    if(input$allGenes){
      updateSelectInput(session, "genes", 
                        selected = PanelPRO:::GENE_TYPES, 
                        choices = PanelPRO:::GENE_TYPES)
    } else {
      updateSelectInput(session, "genes", 
                        choices = PanelPRO:::GENE_TYPES)
    }
  })
  
  # select all cancers
  observeEvent(input$allCancers, {
    if(input$allCancers){
      updateSelectInput(session, "cancers", 
                        selected = setdiff(PanelPRO:::CANCER_NAME_MAP$long, "Contralateral"),
                        choices = setdiff(PanelPRO:::CANCER_NAME_MAP$long, "Contralateral"))
    } else {
      updateSelectInput(session, "cancers", 
                        choices = setdiff(PanelPRO:::CANCER_NAME_MAP$long, "Contralateral"))
    }
  })
  
  # validate year interval for future cancer risk
  output$validYearInterval <- renderText({ 
    shiny::validate(
      need(input$ageBy %% 1 == 0, "Year interval must be an integer."),
      need(input$ageBy >= 1, "Year interval must be greater than or equal to 1."),
      need(input$ageBy <= 10, "Year interval must be less than or equal to 10.")
    )
  })
  
  # enable/disable run button if conditions are not met
  observeEvent(PED(), {
    if(is.null(PED())){
      shinyjs::disable("runPP")
    } else {
      shinyjs::enable("runPP")
    }
  }, ignoreNULL = F, ignoreInit = F)
  
  ##### Results ####
  ppReactive <- reactiveValues(cpTbl = NULL, frTbl = NULL, 
                               cpTblDF = NULL, frTblDF = NULL,
                               cpPlotZoom = NULL, cpPlotFull = NULL,
                               frPlotZoom = NULL, frPlotFull = NULL, 
                               comparisonPlot = NULL, grows = NULL, gcols = NULL,
                               cpAndfrPlots = NULL,
                               cpPlotStaticZoom = NULL, cpPlotStaticFull = NULL, 
                               frPlotStaticZoom = NULL, frPlotStaticFull = NULL,
                               settingsTbl = NULL)
  
  observeEvent(input$runPP, {
    if(!is.null(PED())){
      
      # validate numeric settings
      if(is.na(input$maxMut)){
        maxMut <- 2
      } else {
        maxMut <- as.numeric(input$maxMut)
      }
      if(is.na(input$ageBy)){
        ageBy <- as.numeric(formals(PanelPRO::PanelPRO)$age.by)
      } else {
        ageBy <- as.numeric(input$ageBy)
      }
      if(is.na(input$missAgeIters)){
        missAgeIters <- as.numeric(formals(PanelPRO::PanelPRO)$iterations)
      } else {
        missAgeIters <- as.numeric(input$missAgeIters)
      }
      if(is.na(input$missAgeMaxIters)){
        missAgeMaxIters <- as.numeric(formals(PanelPRO::PanelPRO)$max.iter.tries) * 5
      } else {
        missAgeMaxIters <- as.numeric(input$missAgeMaxIters)
      }
      if(is.na(input$randomSeed)){
        randomSeed <- as.numeric(formals(PanelPRO::PanelPRO)$random.seed)
      } else {
        randomSeed <- as.numeric(input$randomSeed)
      }
      
      # convert net vs crude future risk to logical indicating whether net should be returned
      if(input$netOrCrude == "Net"){
        net.logical <- TRUE
      } else {
        net.logical <- FALSE
      }
      
      # take user to results
      updateTabsetPanel(session, "panelproTabs", selected = "PanelPRO Results")
      
      # run the model, get the console output and check for warnings and error
      out <- 
        ppResultsAndConsole(
          pedigree = PED(),
          model_spec = input$modelSpec,
          genes = input$genes,
          cancers = input$cancers,
          max.mut = maxMut,
          age.by = ageBy,
          unknown.race = input$unknownRace,
          unknown.ancestry = input$unknownAncestry,
          allow.intervention = input$allowInter,
          ignore.proband.germ = input$ignorePbGerm,
          iterations = missAgeIters,
          max.iter.tries = missAgeMaxIters,
          random.seed = randomSeed,
          net = net.logical
        )
      
      # get the proband
      pb <- as.character(PED()$ID[which(PED()$isProband == 1)])
      
      # settings table
      settings <- c("PedigreeID" = "pedigree",
                    "Proband ID" = "proband",
                    "Model Spec" = "model_spec",
                    "Num. Cancers" = NA,
                    "Cancers" = "cancers",
                    "Num. Genes" = NA,
                    "Genes" = "genes",
                    "Max. Mutations" = "max.mut",
                    "Future Risk Year Interval" = "age.by",
                    "Assume Missing Race As" = "unknown.race",
                    "Assume Missing Ancestry As" = "unknown.ancestry",
                    "Allow surgical interventions?" = "allow.intervention",
                    "Ignore Proband's germline testing results?" = "ignore.proband.germ",
                    "Imputation iterations"= "iterations",
                    "Max. iteration tries" = "max.iter.tries",
                    "Random seed for imputing missing ages" = "random.seed",
                    "Provide net, instead of crude, future risk estimates?" = "net")
      settingsTbl <- data.frame(Setting = names(settings),
                                Argument = unname(settings),
                                Value = rep(NA, length(settings)))
      settingsTbl$Value[which(settingsTbl$Setting == "PedigreeID")] <- PED()$PedigreeID[1]
      settingsTbl$Value[which(settingsTbl$Setting == "Proband ID")] <- PED()$ID[which(PED()$isProband == 1)]
      if(input$modelSpec != "Custom"){
        settingsTbl$Value[which(settingsTbl$Setting == "Model Spec")] <- input$modelSpec
        settingsTbl$Value[which(settingsTbl$Setting == "Num. Cancers")] <-
          length(PanelPRO:::MODELPARAMS[[input$modelSpec]]$CANCERS)
        settingsTbl$Value[which(settingsTbl$Setting == "Cancers")] <-
          paste0(PanelPRO:::MODELPARAMS[[input$modelSpec]]$CANCERS, collapse = ", ")
        settingsTbl$Value[which(settingsTbl$Setting == "Num. Genes")] <-
          length(PanelPRO:::MODELPARAMS[[input$modelSpec]]$GENES)
        settingsTbl$Value[which(settingsTbl$Setting == "Genes")] <-
          paste0(PanelPRO:::MODELPARAMS[[input$modelSpec]]$GENES, collapse = ", ")
      } else {
        settingsTbl$Value[which(settingsTbl$Setting == "Model Spec")] <- NA
        settingsTbl$Value[which(settingsTbl$Setting == "Num. Cancers")] <-
          length(input$cancers)
        settingsTbl$Value[which(settingsTbl$Setting == "Cancers")] <-
          paste0(input$cancers, collapse = ", ")
        settingsTbl$Value[which(settingsTbl$Setting == "Num. Genes")] <-
          length(input$genes)
        settingsTbl$Value[which(settingsTbl$Setting == "Genes")] <-
          paste0(input$genes, collapse = ", ")
      }
      settingsTbl$Value[which(settingsTbl$Setting == "Max. Mutations")] <- maxMut
      settingsTbl$Value[which(settingsTbl$Setting == "Future Risk Year Interval")] <- ageBy
      settingsTbl$Value[which(settingsTbl$Setting == "Assume Missing Race As")] <- input$unknownRace
      settingsTbl$Value[which(settingsTbl$Setting == "Assume Missing Ancestry As")] <- input$unknownAncestry
      settingsTbl$Value[which(settingsTbl$Setting == "Allow surgical interventions?")] <- input$allowInter
      settingsTbl$Value[which(settingsTbl$Setting == "Ignore Proband's germline testing results?")] <- input$ignorePbGerm
      settingsTbl$Value[which(settingsTbl$Setting == "Imputation iterations")] <- missAgeIters
      settingsTbl$Value[which(settingsTbl$Setting == "Max. iteration tries")] <- missAgeMaxIters
      settingsTbl$Value[which(settingsTbl$Setting == "Random seed for imputing missing ages")] <- randomSeed
      settingsTbl$Value[which(settingsTbl$Setting == "Provide net, instead of crude, future risk estimates?")] <- net.logical
      
      # use PanelPRO defaults to populate the remainder of the table
      def.vals <- formals(PanelPRO::PanelPRO)
      for(st in settings[which(!is.na(settings))]){
        def.vals[[st]] <- NULL
      }
      add.settings <- data.frame(Setting = rep(NA, length(def.vals)),
                                 Argument = names(def.vals),
                                 Value = rep(NA, length(def.vals)))
      def.setting.names <- c("Impute missing ages?" = "impute.missing.ages",
                             "Remove missing cancers from the model?" = "remove.miss.cancers",
                             "Database" = "database",
                             "Parallelize age imputation?" = "parallel",
                             "Provide debugging messages?" = "debug",
                             "Use BRCAPRO+BCRAT model?" = "plusBCRAT",
                             "Data frame of BCRAT covariates" = "bcrat.vars",
                             "Data frame of BCRAT relative risks" = "rr.bcrat",
                             "Data frame of race-specific relative risks" = "rr.pop")
      for(ln in names(def.vals)){
        if(is.null(def.vals[[ln]])){
          tmp.val <- "NULL"
        } else if(is.symbol(def.vals[[ln]])){
          tmp.val <- rlang::as_string(def.vals[[ln]])
        } else if(ln == "remove.miss.cancers"){
          tmp.val <- "Default is TRUE but not applicable when run on PPI."
        } else {
          tmp.val <- def.vals[[ln]]
        }
        add.settings$Value[which(add.settings$Argument == ln)] <- tmp.val
        add.settings$Setting[which(add.settings$Argument == ln)] <-
          names(def.setting.names)[which(def.setting.names == ln)]
      }
      
      # combine user specified settings and PanelPRO default settings
      tmp.tbl <- rbind(settingsTbl, add.settings)
      allow.age.impute.row <- which(tmp.tbl$Argument == "impute.missing.ages")
      iterations.row <- which(tmp.tbl$Argument == "iterations")
      tmp.tbl <- tmp.tbl[c(1:(iterations.row-1),
                           allow.age.impute.row,
                           iterations.row:(allow.age.impute.row-1),
                           (allow.age.impute.row+1):nrow(tmp.tbl)),]
      ppReactive$settingsTbl <- tmp.tbl
      
      ## create tables and graphs from results
      # only execute if the result was not an error
      if(!is.null(out)){
        
        ## table of posterior probabilities
        if(class(out$posterior.prob[[pb]]) == "data.frame"){
          cpTbl <-
            out$posterior.prob[[pb]] %>%
            mutate(NumMuts = 1 + stringr::str_count(genes, pattern = "\\."), .after = "genes") %>%
            mutate(genes = PanelPRO:::formatGeneNames(gene_names = genes, format = "drop_hetero_anyPV")) %>%
            mutate(genes = ifelse(grepl(pattern = "\\.", genes),
                                  sub(pattern = "\\.", replacement = " & ", genes),
                                  genes)) %>%
            mutate(genes = ifelse(genes == "noncarrier", "Non-carrier", genes))
            
          ## get the risk any PV gene
          # estimate
          prob.anyPVdf <- filter(cpTbl, !grepl(pattern = " & ", genes))
          prob.anyPVdf <- filter(prob.anyPVdf, genes != "Non-carrier")
          prob.anyPV <- sum(prob.anyPVdf$estimate, na.rm = T)
          cpTbl[nrow(cpTbl)+1,] <- rep(NA, 5)
          cpTbl$NumMuts[nrow(cpTbl)] <- 1
          cpTbl$genes[nrow(cpTbl)] <- "Any Gene"
          cpTbl$estimate[nrow(cpTbl)] <- prob.anyPV
          
          # lower and upper prediction intervals
          if(!(all(is.na(prob.anyPVdf$lower)) && 
               all(is.na(prob.anyPVdf$upper)))){
            pred.mat <- NULL
            for(gn in unique(prob.anyPVdf$genes)){
              g.preds <- runif(n = missAgeIters, 
                               min = prob.anyPVdf$lower[which(prob.anyPVdf$genes == gn)],
                               max = prob.anyPVdf$upper[which(prob.anyPVdf$genes == gn)])
              if(is.null(pred.mat)){
                pred.mat <- g.preds
              } else {
                pred.mat <- cbind(pred.mat, g.preds)
              }
            }
            any.preds <- apply(pred.mat, 1, sum, simplify = T)
            cpTbl$lower[nrow(cpTbl)] <- min(any.preds, na.rm = T)
            cpTbl$upper[nrow(cpTbl)] <- max(any.preds, na.rm = T)
          }
          
          ## format table
          cpTbl <-
            cpTbl %>%
            mutate(across(.cols = -genes, ~as.numeric(.))) %>%
            arrange(desc(estimate)) %>%
            rename("Num. Muts." = "NumMuts",
                   "Genes" = "genes",
                   "Estimate" = "estimate",
                   "Lower" = "lower",
                   "Upper" = "upper")
  
          # save data frame version for download
          ppReactive$cpTblDF <- cpTbl
  
          # format as a data.table
          cpTbl <-
            cpTbl %>%
            mutate(across(.cols = c(Estimate, Lower, Upper), ~ round(., digits = 6))) %>%
            mutate(across(.cols = c(Estimate, Lower, Upper), ~ ifelse(is.na(.), NA,
                                                                      ifelse(.<0.000001, "<1E-6", .)))) %>%
            DT::datatable(rownames = FALSE,
                          class = list("nowrap", "stripe", "compact"),
                          options = list(pageLength = 20)) %>%
            DT::formatStyle(columns = c("Estimate", "Lower", "Upper"),
                            textAlign = "right")
          ppReactive$cpTbl <- cpTbl
          
        } else {
          ppReactive$cpTblDF <- NULL
          ppReactive$cpTbl <- NULL
        }

        ## table of cancer risks
        if(!any(lapply(out$future.risk[[pb]], class) == "character")){
          frByCancer <- out$future.risk[[pb]]
          frTbl <- NULL
          for(cn in names(frByCancer)){
            can.df <-
              frByCancer[[cn]] %>%
              mutate(Cancer = cn, .before = "ByAge") %>%
              mutate(Who = "Proband", .after = "Cancer")
            if(is.null(frTbl)){
              frTbl <- can.df
            } else {
              frTbl <- rbind(frTbl, can.df)
            }
          }
          frTbl <-
            frTbl %>%
            rename("Estimate" = "estimate",
                   "Lower" = "lower",
                   "Upper" = "upper")
          
          ## get average person risks
          nc.pens <-
            as.data.frame(
              PanelPRO::PanelPRODatabase$Penetrance[, # Cancer
                                                    "SEER", # Gene
                                                    ifelse(is.na(PED()$race[which(PED()$isProband == 1)]), "All_Races", 
                                                           PED()$race[which(PED()$isProband == 1)]), #Race
                                                    ifelse(PED()$Sex[which(PED()$isProband == 1)] == 0, "Female", 
                                                           "Male"), #Sex
                                                    , # Age
                                                    ifelse(net.logical, "Net", "Crude") # PenetType
                                                    ]
            ) %>%
            tibble::rownames_to_column("Cancer") %>%
            pivot_longer(cols = -Cancer, names_to = "ByAge", values_to = "Penetrance") %>%
            mutate(Who = "Average Person", .after = "Cancer") %>%
            mutate(Estimate = 0, .after = "ByAge") %>%
            mutate(Lower = NA, .after = "Estimate") %>%
            mutate(Upper = NA, .after = "Lower") %>%
            group_by(Cancer) %>%
            mutate(Survival = 1 - cumsum(Penetrance)) %>%
            mutate(across(.cols = c(Survival, Estimate), ~as.numeric(.)))
          pb.age <- PED()$CurAge[which(PED()$isProband == 1)]
          for(c in unique(nc.pens$Cancer)){
            tmp.pens <- nc.pens[which(nc.pens$Cancer == c),]
            for(a in (pb.age+1):(PanelPRO:::MAXAGE)){
              nc.pens$Estimate[which(nc.pens$Cancer == c & nc.pens$ByAge == a)] <- 
                sum(tmp.pens$Penetrance[(pb.age+1):a]) / nc.pens$Survival[pb.age]
            }
          }
          nc.pens <- 
            nc.pens %>% 
            select(-c(Penetrance, Survival)) %>%
            mutate(ByAge = as.numeric(ByAge)) %>%
            filter(ByAge %in% frTbl$ByAge)
          
          frTbl <- 
            rbind(frTbl, nc.pens) %>%
            arrange(Cancer, desc(Who), ByAge) %>%
            rename("By Age" = "ByAge")

          # save data frame version for download
          ppReactive$frTblDF <- frTbl

          # format as a data.table
          frTbl <-
            frTbl %>%
            mutate(across(.cols = c(Estimate, Lower, Upper), ~ round(., digits = 6))) %>%
            mutate(across(.cols = c(Estimate, Lower, Upper), ~ ifelse(is.na(.), NA,
                                                                      ifelse(.<0.000001, "<1E-6", .)))) %>%
            DT::datatable(rownames = FALSE,
                          class = list("nowrap", "stripe", "compact"),
                          options = list(pageLength = 20)) %>%
            DT::formatStyle(columns = c("Estimate", "Lower", "Upper"),
                            textAlign = "right")
          ppReactive$frTbl <- frTbl

          # PanelPRO did not return a cancer risk table
        } else {
          ppReactive$frTblDF <- NULL
          ppReactive$frTbl <- NULL
        }

        ### plots
        vr.plots <- visRiskPPI(pp_output = out,
                               markdown = NULL,
                               return_obj = TRUE,
                               prob_threshold = 0.01,
                               show_fr_ci = FALSE,
                               race = PED()$race[which(PED()$isProband == 1)],
                               sex = PED()$Sex[which(PED()$isProband == 1)],
                               cur.age = PED()$CurAge[which(PED()$isProband == 1)],
                               net = net.logical,
                               missAgeIters = missAgeIters)
        
        ## carrier prob plots
        if(class(out$posterior.prob[[pb]]) == "data.frame"){
          ppReactive$cpPlotZoom <- vr.plots$cp.zoom
          ppReactive$cpPlotFull <- vr.plots$cp.full
          ppReactive$cpPlotStaticZoom <- vr.plots$cpStatic.zoom     # for download
          ppReactive$cpPlotStaticFull <- vr.plots$cpStatic.full     # for download
        } else {
          ppReactive$cpPlotZoom <- NULL
          ppReactive$cpPlotFull <- NULL
          ppReactive$cpPlotStaticZoom <- NULL
          ppReactive$cpPlotStaticFull <- NULL
        }
        
        ## cancer risk plots
        if(!any(lapply(out$future.risk[[pb]], class) == "character")){
          ppReactive$frPlotZoom <- vr.plots$fr.zoom
          ppReactive$frPlotFull <- vr.plots$fr.full
          ppReactive$frPlotStaticZoom <- vr.plots$frStatic.zoom   # for download
          ppReactive$frPlotStaticFull <- vr.plots$frStatic.full   # for download
          ppReactive$comparisonPlot <- vr.plots$comparison
          ppReactive$grows <- vr.plots$grows
          ppReactive$gcols <- vr.plots$gcols
        } else {
          ppReactive$frPlotZoom <- NULL
          ppReactive$frPlotFull <- NULL
          ppReactive$frPlotStaticZoom <- NULL
          ppReactive$frPlotStaticFull <- NULL
          ppReactive$comparisonPlot <- NULL
          ppReactive$grows <- NULL
          ppReactive$gcols <- NULL
        }
        
        ## combined prob and cancer risk plot
        ppReactive$cpAndfrPlots <- vr.plots$both
        
        # end of if statement to confirm PanelPRO output was not an error
      } else {
        cpTbl <- NULL
        frTbl <- NULL
        cpTblDF <- NULL
        frTblDF <- NULL
        cpPlotZoom <- NULL
        cpPlotFull <- NULL
        frPlotZoom <- NULL
        frPlotFull <- NULL
        comparisonPlot <- NULL
        grows <- NULL
        gcols <- NULL
        cpAndfrPlots <- NULL
        cpPlotStaticZoom <- NULL
        cpPlotStaticFull <- NULL
        frPlotStaticZoom <- NULL
        frPlotStaticFull <- NULL
      }
    } # end of if statement to check if pedigree was present
  }, ignoreNULL = F, ignoreInit = T)
  
  # carrier probabilities table
  output$ppCPTbl <- renderDT({
    shiny::validate(
      need(!is.null(PED()), "No pedigree has been loaded or created yet."),
      need(!is.null(ppReactive$cpTbl), "A carrier probability table could not be generated.")
    )
    return(ppReactive$cpTbl)
  }, server = F)
  
  # cancer risk table
  output$ppFRTbl <- renderDT({
    shiny::validate(
      need(!is.null(PED()), "No pedigree has been loaded or created yet."),
      need(!is.null(ppReactive$frTbl), "A cancer risk table could not be generated.")
    )
    return(ppReactive$frTbl)
  }, server = F)
  
  # carrier prob. plot, zoomed y-axis
  output$ppCPPlotZoom <- plotly::renderPlotly({
    shiny::validate(
      need(!is.null(PED()), "No pedigree has been loaded or created yet."),
      need(!is.null(ppReactive$cpPlotZoom), "A carrier probability plot could not be generated.")
    )
    return(ppReactive$cpPlotZoom)
  })
  
  # carrier prob. plot, full y-axis
  output$ppCPPlotFull <- plotly::renderPlotly({
    shiny::validate(
      need(!is.null(PED()), "No pedigree has been loaded or created yet."),
      need(!is.null(ppReactive$cpPlotFull), "A carrier probability plot could not be generated.")
    )
    return(ppReactive$cpPlotFull)
  })
  
  # carrier prob. plot
  output$ppFRPlotZoom <- plotly::renderPlotly({
    shiny::validate(
      need(!is.null(PED()), "No pedigree has been loaded or created yet."),
      need(!is.null(ppReactive$frPlotZoom), "A cancer risk plot could not be generated.")
    )
    return(ppReactive$frPlotZoom)
  })
  
  # carrier prob. plot
  output$ppFRPlotFull <- plotly::renderPlotly({
    shiny::validate(
      need(!is.null(PED()), "No pedigree has been loaded or created yet."),
      need(!is.null(ppReactive$frPlotFull), "A cancer risk plot could not be generated.")
    )
    return(ppReactive$frPlotFull)
  })
  
  # comparison to average person cancer risks
  output$comparisonPlot <- renderPlot({
    shiny::validate(
      need(!is.null(PED()), "No pedigree has been loaded or created yet."),
      need(!is.null(ppReactive$comparisonPlot), "A cancer risk plot could not be generated.")
    )
    return(grid::grid.draw(ppReactive$comparisonPlot))
  })
  output$comparisonPlotH <- renderUI({
    h <- paste0(300*as.numeric(ppReactive$grows), "px")
    w <- paste0(360*as.numeric(ppReactive$gcols), "px")
    plotOutput("comparisonPlot", height = h, width = w)
  })
  
  # run settings table
  output$ppRunSettings <- renderTable({
    shiny::validate(
      need(!is.null(PED()), "No pedigree has been loaded or created yet."),
      need(!is.null(ppReactive$settingsTbl), "Run PanelPRO to see the results.")
    )
    return(ppReactive$settingsTbl)
  }, striped = T)
  
  # PanelPRO function doc string
  observeEvent(list(input$showPPDocString1, input$showPPDocString2), {
    showModal(modalDialog(
      tagList(htmlOutput("ppDocString")),
      title = "PanelPRO Function R Documentation",
      footer = tagList(
        modalButton("Close")
      ),
      easyClose = T
    ))
  }, ignoreInit = T)
  ppDocString <- reactive({
    temp = Rd2HTML(Rd_fun("PanelPRO"), out = tempfile("docs"))
    content = read_file(temp)
    file.remove(temp)
    content
  })
  output$ppDocString <- renderText({
    ppDocString()
  })
  
  ##### Download ####
  # disable download button if PanelPRO has not been run yet
  observeEvent(ppReactive$cpTbl, {
    if(!is.null(ppReactive$cpTbl)){
      shinyjs::enable('downloadResults1')
    } else {
      shinyjs::disable('downloadResults1')
    }
  }, ignoreInit = F, ignoreNULL = F)
  
  # prepare cancersJSON for download as its own table
  canJSONToDF <- reactive({
    
    # convert JSONs into a data frame of cancer hx indexed by PedigreeID and ID
    can.df <- NULL
    for(row in 1:nrow(PED())){
      if(!is.na(PED()$cancersJSON[row])){
        mod.can.JSON <- gsub(pattern = "\'", replacement = "\"", PED()$cancersJSON[row])
        rel.can.df <- fromJSON(mod.can.JSON, simplifyDataFrame = T)
        rel.can.df <- cbind(data.frame(PedigreeID = rep(PED()$PedigreeID[1], nrow(rel.can.df)), 
                                       ID = rep(PED()$ID[row], nrow(rel.can.df))), 
                            rel.can.df)
        if(is.null(can.df)){
          can.df <- rel.can.df
        } else {
          can.df <- rbind(can.df, rel.can.df)
        }
      }
    }
    
    # if there was no cancer hx in any of the selected pedigrees return a 0 row data frame
    if(is.null(can.df)){
      return(setNames(as.data.frame(matrix(NA, nrow = 0, ncol = 2 + length(colnames(cancer.inputs.store)))), 
                      c("PedigreeID", "ID", colnames(cancer.inputs.store))))
    } else {
      return(can.df)
    }
  })
  
  # prepare genesJSON for download as its own table
  genesJSONToDF <- reactive({
  
    # convert JSONs into a data frame of gene results indexed by PedigreeID and ID
    gene.df <- NULL
    for(row in 1:nrow(PED())){
      if(!is.na(PED()$genesJSON[row])){
        mod.gene.JSON <- gsub(pattern = "\'", replacement = "\"", PED()$genesJSON[row])
        rel.gene.df <- fromJSON(mod.gene.JSON, simplifyDataFrame = T)
        rel.gene.df <- cbind(data.frame(PedigreeID = rep(PED()$PedigreeID[1], nrow(rel.gene.df)), 
                                        ID = rep(PED()$ID[row], nrow(rel.gene.df))), 
                             rel.gene.df)
        if(is.null(gene.df)){
          gene.df <- rel.gene.df
        } else {
          gene.df <- rbind(gene.df, rel.gene.df)
        }
      }
    }
    
    # if there was no gene results in any of the selected pedigrees return a 0 row data frame
    if(is.null(gene.df)){
      return(setNames(as.data.frame(matrix(NA, nrow = 0, ncol = 2 + length(gene.df.colnames))), 
                      c("PedigreeID", "ID", gene.df.colnames)))
    } else {
      return(gene.df)
    }
  })
  
  # model for user to select file format
  observeEvent(input$downloadResults1, {
    showModal(modalDialog(
      tagList(
        h5("Choose a file format for the tables:"),
        radioButtons("downloadResultsAs", label = NULL,
                     choices = c(".csv", ".rds"),
                     selected = ".csv"),
      ),
      title = "Download File Format",
      footer = tagList(
        conditionalPanel("input.downloadResultsAs == '.csv'",
          downloadButton("downloadResultsCSV", label = "Download",
                         icon = icon('download')),
        ),
        conditionalPanel("input.downloadResultsAs == '.rds'",
          downloadButton("downloadResultsRDS", label = "Download",
                         icon = icon('download')),
        ),
        modalButton("Cancel")
      )
    ))
  })
  
  # download as .csv
  output$downloadResultsCSV <- shiny::downloadHandler(
    filename = function(){
      paste0("PanelPRO-results-", 
             ppReactive$settingsTbl$Value[which(ppReactive$settingsTbl$Setting == "PedigreeID")], 
             "-",
             Sys.Date(), ".zip")
    },
    content = function(file){
      
      # remove modal when done
      on.exit(removeModal())
      
      # pedigreeID
      pedID <- ppReactive$settingsTbl$Value[which(ppReactive$settingsTbl$Setting == "PedigreeID")]
      
      # data dictionary files
      write.csv(ppCancersDict(), file = "data-dictionary/panelpro-cancer-abbreviations.csv", row.names = F)
      write.csv(ppGenes(), file = "data-dictionary/panelpro-gene-list.csv", row.names = F)
      Rd2HTML(Rd_fun("PanelPRO"), out = "data-dictionary/panelpro-function-documentation.html")
      
      # pedigree files
      write.csv(PED(), file = paste0("download-results/pedigree-", pedID, ".csv"), row.names = F)
      write.csv(canJSONToDF(), file = paste0("download-results/cancer-details-", pedID, ".csv"), row.names = F)
      write.csv(genesJSONToDF(), file = paste0("download-results/panel-details-", pedID, ".csv"), row.names = F)
      
      # create the png image
      cans <- select(PED(), starts_with("isAff"))
      png(filename = paste0("download-results/pedigree-image-", pedID,".png"))
      print(plot(
        kinship2.ped(PED())[paste0(unique(PED()$PedigreeID))]
      ))
      if(sum(cans) > 0){
        print(
          pedigree.legend(kinship2.ped(PED())[paste0(unique(PED()$PedigreeID))], 
                          location = "bottomright", radius=0.1)
        )
      }
      dev.off()
      
      # run settings table
      write.csv(ppReactive$settingsTbl, file = paste0("download-results/run-settings-", pedID, ".csv"), row.names = F)
      
      # result tables
      if(!is.null(ppReactive$cpTblDF)){
        write.csv(ppReactive$cpTblDF, file = paste0("download-results/posterior-probs-", pedID, ".csv"), row.names = F)
      }
      if(!is.null(ppReactive$frTblDF)){
        write.csv(ppReactive$frTblDF, file = paste0("download-results/cancer-risks-", pedID, ".csv"), row.names = F)
      }
      
      # result images and other
      if(!is.null(ppReactive$cpPlotStaticZoom)){
        ggsave(plot = ppReactive$cpPlotStaticZoom, 
               path = "./download-results", 
               filename = paste0("posterior-probs-zoom-", pedID, ".png"))
      }
      if(!is.null(ppReactive$cpPlotStaticFull)){
        ggsave(plot = ppReactive$cpPlotStaticFull, 
               path = "./download-results", 
               filename = paste0("posterior-probs-full-", pedID, ".png"))
      }
      if(!is.null(ppReactive$frPlotStaticZoom)){
        ggsave(plot = ppReactive$frPlotStaticZoom, 
               path = "./download-results", 
               filename = paste0("cancer-risks-zoomed-y-axis-", pedID, ".png"))
      }
      if(!is.null(ppReactive$frPlotStaticFull)){
        ggsave(plot = ppReactive$frPlotStaticFull, 
               path = "./download-results", 
               filename = paste0("cancer-risks-standard-", pedID, ".png"))
      }
      if(!is.null(ppReactive$comparisonPlot)){
        ggsave(plot = ppReactive$comparisonPlot,
               width = 1667*as.numeric(ppReactive$gcols),
               height = 1250*as.numeric(ppReactive$grows),
               units = "px",
               path = "./download-results", 
               filename = paste0("cancer-risks-compared-to-ave-person-", pedID, ".png"))
      }
      
      # zip them all together
      new.files <- c(
        "data-dictionary/panelpro-cancer-abbreviations.csv",
        "data-dictionary/panelpro-gene-list.csv",
        "data-dictionary/panelpro-function-documentation.html",
        paste0("download-results/pedigree-", pedID, ".csv"),
        paste0("download-results/cancer-details-", pedID, ".csv"),
        paste0("download-results/panel-details-", pedID, ".csv"),
        paste0("download-results/pedigree-image-", pedID,".png"),
        paste0("download-results/run-settings-", pedID, ".csv")
      )
      if(!is.null(ppReactive$cpTblDF)){
        new.files <- c(new.files, paste0("download-results/posterior-probs-", pedID, ".csv"))
      }
      if(!is.null(ppReactive$cpPlotStaticZoom)){
        new.files <- c(new.files, paste0("download-results/posterior-probs-zoom-", pedID, ".png"))
      }
      if(!is.null(ppReactive$cpPlotStaticFull)){
        new.files <- c(new.files, paste0("download-results/posterior-probs-full-", pedID, ".png"))
      }
      if(!is.null(ppReactive$frTblDF)){
        new.files <- c(new.files, paste0("download-results/cancer-risks-", pedID, ".csv"))
      }
      if(!is.null(ppReactive$frPlotStaticZoom)){
        new.files <- c(new.files, paste0("download-results/cancer-risks-zoomed-y-axis-", pedID, ".png"))
      }
      if(!is.null(ppReactive$frPlotStaticFull)){
        new.files <- c(new.files, paste0("download-results/cancer-risks-standard-", pedID, ".png"))
      }
      if(!is.null(ppReactive$comparisonPlot)){
        new.files <- c(new.files, paste0("download-results/cancer-risks-compared-to-ave-person-", pedID, ".png"))
      }
      
      tmp.zip <- 
        zip::zip(zipfile = file, 
                 files = c("data-dictionary/columns-and-codings-dictionary.csv",
                           "data-dictionary/README.md",
                           "download-results/README.md",
                           new.files))
      
      # remove the created files
      file.remove(new.files)
      
      return(tmp.zip)
    },
    contentType = "application/zip"
  )
  
  # download as .rds
  output$downloadResultsRDS <- shiny::downloadHandler(
    filename = function(){
      paste0("PanelPRO-results-", 
             ppReactive$settingsTbl$Value[which(ppReactive$settingsTbl$Setting == "PedigreeID")], 
             "-",
             Sys.Date(), ".zip")
    },
    content = function(file){
      
      # remove modal when done
      on.exit(removeModal())
      
      # pedigreeID
      pedID <- ppReactive$settingsTbl$Value[which(ppReactive$settingsTbl$Setting == "PedigreeID")]
      
      # data dictionary files
      saveRDS(read.csv(file = "data-dictionary/columns-and-codings-dictionary.csv"), 
              file = "data-dictionary/columns-and-codings-dictionary.rds")
      saveRDS(ppCancersDict(), file = "data-dictionary/panelpro-cancer-abbreviations.rds")
      saveRDS(ppGenes(), file = "data-dictionary/panelpro-gene-list.rds")
      Rd2HTML(Rd_fun("PanelPRO"), out = "data-dictionary/panelpro-function-documentation.html")
      
      # pedigree files
      saveRDS(PED(), file = paste0("download-results/pedigree-", pedID, ".rds"))
      saveRDS(canJSONToDF(), file = paste0("download-results/cancer-details-", pedID, ".rds"))
      saveRDS(genesJSONToDF(), file = paste0("download-results/panel-details-", pedID, ".rds"))
      
      # create the png image
      cans <- select(PED(), starts_with("isAff"))
      png(filename = paste0("download-results/pedigree-image-", pedID,".png"))
      print(plot(
        kinship2.ped(PED())[paste0(unique(PED()$PedigreeID))]
      ))
      if(sum(cans) > 0){
        print(
          pedigree.legend(kinship2.ped(PED())[paste0(unique(PED()$PedigreeID))], 
                          location = "bottomright", radius=0.1)
        )
      }
      dev.off()
      
      # run settings table
      saveRDS(ppReactive$settingsTbl, file = paste0("download-results/run-settings-", pedID, ".rds"))
      
      # result tables
      if(!is.null(ppReactive$cpTblDF)){
        saveRDS(ppReactive$cpTblDF, file = paste0("download-results/posterior-probs-", pedID, ".rds"))
      }
      if(!is.null(ppReactive$frTblDF)){
        saveRDS(ppReactive$frTblDF, file = paste0("download-results/cancer-risks-", pedID, ".rds"))
      }
      
      # result images and other
      if(!is.null(ppReactive$cpPlotStaticZoom)){
        ggsave(plot = ppReactive$cpPlotStaticZoom, 
               path = "./download-results", 
               filename = paste0("posterior-probs-zoom-", pedID, ".png"))
      }
      if(!is.null(ppReactive$cpPlotStaticFull)){
        ggsave(plot = ppReactive$cpPlotStaticFull, 
               path = "./download-results", 
               filename = paste0("posterior-probs-full-", pedID, ".png"))
      }
      if(!is.null(ppReactive$frPlotStaticZoom)){
        ggsave(plot = ppReactive$frPlotStaticZoom, 
               path = "./download-results", 
               filename = paste0("cancer-risks-zoomed-y-axis-", pedID, ".png"))
      }
      if(!is.null(ppReactive$frPlotStaticFull)){
        ggsave(plot = ppReactive$frPlotStaticFull, 
               path = "./download-results", 
               filename = paste0("cancer-risks-standard-", pedID, ".png"))
      }
      if(!is.null(ppReactive$comparisonPlot)){
        ggsave(plot = ppReactive$comparisonPlot,
               width = 1667*as.numeric(ppReactive$gcols),
               height = 1250*as.numeric(ppReactive$grows),
               units = "px",
               path = "./download-results", 
               filename = paste0("cancer-risks-compared-to-ave-person-", pedID, ".png"))
      }
      
      # zip them all together
      new.files <- c(
        "data-dictionary/columns-and-codings-dictionary.rds",
        "data-dictionary/panelpro-cancer-abbreviations.rds",
        "data-dictionary/panelpro-gene-list.rds",
        "data-dictionary/panelpro-function-documentation.html",
        paste0("download-results/pedigree-", pedID, ".rds"),
        paste0("download-results/cancer-details-", pedID, ".rds"),
        paste0("download-results/panel-details-", pedID, ".rds"),
        paste0("download-results/pedigree-image-", pedID,".png"),
        paste0("download-results/run-settings-", pedID, ".rds")
      )
      if(!is.null(ppReactive$cpTblDF)){
        new.files <- c(new.files, paste0("download-results/posterior-probs-", pedID, ".rds"))
      }
      if(!is.null(ppReactive$cpPlotStaticZoom)){
        new.files <- c(new.files, paste0("download-results/posterior-probs-zoom-", pedID, ".png"))
      }
      if(!is.null(ppReactive$cpPlotStaticFull)){
        new.files <- c(new.files, paste0("download-results/posterior-probs-full-", pedID, ".png"))
      }
      if(!is.null(ppReactive$frTblDF)){
        new.files <- c(new.files, paste0("download-results/cancer-risks-", pedID, ".rds"))
      }
      if(!is.null(ppReactive$frPlotStaticZoom)){
        new.files <- c(new.files, paste0("download-results/cancer-risks-zoomed-y-axis-", pedID, ".png"))
      }
      if(!is.null(ppReactive$frPlotStaticFull)){
        new.files <- c(new.files, paste0("download-results/cancer-risks-standard-", pedID, ".png"))
      }
      if(!is.null(ppReactive$comparisonPlot)){
        new.files <- c(new.files, paste0("download-results/cancer-risks-compared-to-ave-person-", pedID, ".png"))
      }
      
      tmp.zip <- 
        zip::zip(zipfile = file, 
                 files = c("data-dictionary/README.md",
                           "download-results/README.md",
                           new.files))
      
      # remove the created files
      file.remove(new.files)
      
      return(tmp.zip)
    },
    contentType = "application/zip"
  )
  
  #### Terms and Conditions ####
  output$termsOut <- renderText({
    lines <- read_lines(system.file("LICENSE", package="PanelPRO"))
    paste0(lines[which(lines != "")], collapse = "\n\n")
  })
  observeEvent(input$terms, {
    showModal(modalDialog(
      tagList(tagAppendAttributes(textOutput("termsOut"), style="white-space:pre-wrap;")),
      title = "Terms and Conditions",
      footer = tagList(
        modalButton("Close")
      ),
      easyClose = T
    ))
  })
  
} # end of server

# Run the application 
shinyApp(ui = ui, server = server)
