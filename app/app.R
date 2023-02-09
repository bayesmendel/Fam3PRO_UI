#### Set-up ####
## libraries
# shiny libraries
library(shiny)
library(shinyBS)    # shiny tool tips
library(shinyjs)    # java script tools

# pedigrees
library(kinship2)   # draws pedigrees (this is temporary only)
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
library(jsonlite)
library(stringi)    # toTitleCase

# html
library(htmltools)

# tables
library(DT)

## load data


## utility functions and variables
source("./vars.R")
source("./ped-utils.R")
source("./demo-utils.R")
source("./can-utils.R")
source("./surg-utils.R")
source("./gene-utils.R")
source("./modules.R")
source("./accnt-utils.R")


ui <- fixedPage(
  
  # adjust all tab's padding for all tabSetPanels
  tags$style(HTML(".tabbable > .nav > li > a {padding:5px;}")),
  
  # title and log-out button
  titlePanel(
    tagList(
      span("PPI: PanelPRO Interface",
           div(class = "pull-right", shinyauthr::logoutUI(id = "logout")))
    )
  ),
  
  # Google analytics
  # tags$head(includeHTML(("google-analytics.html"))),
  
  # allows shinyjs commands like disable (and others?)
  useShinyjs(),
  
  #### Log-in tabs ####
  conditionalPanel("!output.loggedIn",
    tabsetPanel(id = "loginTabs",
      
      ##### Log-in ####
      tabPanel(title = "Log In",
        shinyauthr::loginUI(id = "login",
          cookie_expiry = 0,
          additional_ui = 
            shiny::tagList(fluidRow(br()),
                           fluidRow(
                             actionButton(inputId = "signUp",
                                          label = "Sign up")),
                           fluidRow(br()),
                           fluidRow(
                             actionButton(inputId = "forgotUnPw",
                                          label = "Forgot Username or Password",
                                          style = "padding:4px; font-size:80%"))
          )
        )
      ), # end log-in tab
       
      ##### Sign-up ####
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
          selectizeInput("selManagers1", label = "Enter managers' usernames",
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

      ##### Forgot Username or Password ####
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
      
      ##### Bot Check ####
      tabPanel(title = "Bot Check",
        uiOutput("botCheckUI")
      )
    ) # end of tabsetPanel for log-in tabs
  ), # end of conditionalPanel for logged out status
  
  #### Post Log-in ####
  conditionalPanel("output.loggedIn",
    navbarPage(title = "", id = "navbarTabs",
      
      ##### Home ####
      tabPanel(title = "Home",
        h3("PPI Home"),
        h4("What is PPI?"),
        p("The PanelPRO Interface (PPI) is a website where users can interact with 
          the PanelPRO software. PanelPRO was create by the BayesMendel Lab at 
          Dana-Farber Cancer Institute and is a multi-cancer/multi-gene risk prediction model 
          which utilizes family history to estimate the probability a patient has a 
          a pathogenic or likely pathogenic variant of one or more of their genes as well 
          as their future risk of a variety of different cancer types."),
        
        h4("How to Use PPI"),
        
        h4("Support and Contact Information"),
        
      ), # end of tab
      
      ##### Manage Pedigrees ####
      tabPanel("Manage Pedigrees",
        h3("Manage My Pedigrees"),
        tabsetPanel(id = "managePedsTab",
                    
          ###### Create or Load Pedigree ####
          tabPanel("Create or Load",
            h4("Create New or Load Existing Pedigree"),
            p("To get started, you will either need to create a new pedigree using our 
              pedigree builder or load an existing pedigree from your user account."),
            radioButtons("newOrLoad", "Select an start-up option:",
                         choices = c("Create new", "Load existing"),
                         selected = "Create new"),
            
            # if the user wants to load an existing table
            conditionalPanel("input.newOrLoad == 'Load existing'",
              
              # for admins and managers, select the user account to load from first
              conditionalPanel("output.admin | output.manager",
                selectInput(inputId = "selectUser", label = "Select a user account:", 
                            choices = c(""))
              ),
               
              # if there are not pedgirees to load, tell the user
              conditionalPanel(condition = "output.showTblExistsError",
                p("You do not have any saved pedigrees.", style = "color: red;")
              ),
               
              # if there are pedigrees to load, provide a dropdown
              conditionalPanel(condition = "!output.showTblExistsError",
                selectInput("existingPed", "Select a pedigree to load:",
                            choices = "")
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
              actionButton("goNewOrLoad", label = "Get Started",
                           icon = icon('play'),
                           style = "color: white; background-color: #10699B; border-color: #10699B")
            )
          ),
        
          ###### Download Pedigrees ####
          tabPanel("Download",
            h4("Download Pedigrees"),
            
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
                               
              # CSV download button
              conditionalPanel("input.downloadAs1 == '.csv'",
                downloadButton("downloadPedsCSV", label = "Download",
                               icon = icon('download'),
                               style = "color: white; background-color: #10699B; border-color: #10699B")
              ),
              
              # RDS download button
              conditionalPanel("input.downloadAs1 == '.rds'",
                downloadButton("downloadPedsRDS", label = "Download",
                               icon = icon('download'),
                               style = "color: white; background-color: #10699B; border-color: #10699B")
              )
            ) # end of conditionalPanel to check if there are tables to download
          ), # end of download tab
          
          ###### Delete Pedigrees ####
          tabPanel("Delete",
            h4("Delete Pedigrees"),
            
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
              actionButton("deletePeds", label = "Delete Pedigrees",
                           icon = icon('trash'),
                           style = "color: white; background-color: #10699B; border-color: #10699B")
            ) # end of conditionalPanel to check if there are tables to download
          ) # end of tabPanel for deleting pedigrees
        ) # end of tabsetPanels for manage pedigrees
      ), # end of tab for manage pedigrees tab
      
      ##### Create/Modify Pedigree ####
      tabPanel("Create/Modify Pedigree",
        
        # top row to hold save pedigree button
        fluidRow(
           
          # select which relative is being edited, only show after pedigree is visualized
          column(width = 6, align = "left",
            conditionalPanel("input.visPed",
              fluidRow(
                column(width = 5,
                  h4("Select a relative to edit:")
                ),
                column(width = 7,
                  selectInput("relSelect", label = NULL,
                              choices = setNames(c(1), "Proband"), # placeholder, this will be updated once FDR+AU ped initialized
                              width = "200px")
                )
              )
            )
          ),
          
          # save pedigree
          column(width = 6, align = "right",
            actionButton("savePed", label = "Save Pedigree",
                         icon = icon('save'),
                         style = "color: white; background-color: #10699B; border-color: #10699B; margin-top:0px; margin-bottom:0px;")
          )
        ),
        
        # create 2 columns, one for displaying the pedigree (left) and one for data entry (right)
        fluidRow(
          
          # only show pedigree visualization after pedigree has been initialized with all FDR, aunts, and uncles
          conditionalPanel("input.visPed",
            column(width = 6,
              plotOutput("drawPed")
            )
          ),
          
          # column for pedigree data entry
          column(width = 6,
            tabsetPanel(id = "pedTabs", 
              
              ###### Demographics ####
              tabPanel("Demographics",
                h3("Demographics"),
                p("Enter the person's demographic information below. Inputs with an 
                  astrick(*) require a response to continue to the next screen."),
                
                # PedigreeID
                textInput("pedID", label = h5("*Unique Pedigree ID:"),
                          value = "",
                          width = "225px"),
                conditionalPanel("!input.visPed",
                  h5("Privacy note: the ID number cannot contain identifying information.",
                     style = "color:blue")
                ),
                conditionalPanel("output.nonUniqPedID",
                  h5("Your account already has a pedigree with this ID number, 
                     choose another name or delete the existing pedigree first.",
                     style = "color:red")
                ),
                
                # sex and age
                selectInput("Sex", label = h5("*Sex assigned at birth:"),
                            choices = sex.choices,
                            width = "150px"),
                numericInput("Age",
                             label = h5("*Current Age (1 to 89):"),
                             value = NA, min = min.age, max = max.age, step = 1,
                             width = "150px"),
                textOutput("validAge"),
                tags$head(tags$style("#validAge{color: red;}")),
                
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
              ), # end of demographics tab
              
              ###### Cancer Hx ####
              tabPanel("Cancer Hx",
                h3("Cancer History"),
                p("List all first primary cancers the person has or had with the age of diagnosis."),
                
                # cancerUI modules inserted into this container
                tags$div(
                  id = "canContainer",
                  style = "width:100%"
                ),
                actionButton("addCan", label = "Add Cancer",
                             icon = icon('plus'),
                             style = "color: white; background-color: #10699B; border-color: #10699B; margin-top: 20px")
              ), # end of cancers tab
              
              ###### CBC Risk ####
              tabPanel("CBC Risk",
                h3("Contralateral Breast Cancer Risk"),
                conditionalPanel("!output.showCBCinputs",
                  p("This tab is only used when the relative has/had breast cancer but has not developed contralateral breast cancer.")
                ),
                conditionalPanel("output.showCBCinputs",
                  h5("Was the 1st breast cancer pure invasive, mixed invasive and DCIS, or unknown?"),
                  selectInput("FirstBCType", label = NULL,
                              choices = bc1type.choices,
                              width = "200px"),
                  h5("Was the 1st breast cancer treated with anti-estrogen therapy?"),
                  selectInput("AntiEstrogen", label = NULL,
                              choices = antiest.hrpre.choices,
                              width = "125px"),
                  h5("Does the relative have a history of high risk pre-neoplasia (ie atypical hyperplasia or LCIS?)"),
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
              
              ###### Tumor Markers ####
              tabPanel("Tumor Markers",
                h3("Tumor Markers"),
                conditionalPanel("!output.showBCMarkers & !output.showCRCMarkers",
                  p("Tumor markers are only applicable if the person has/had breast cancer or colorectal cancer")
                ),
                conditionalPanel("output.showBCMarkers | output.showCRCMarkers",
                  p("If the person was tested for any of the tumor markers related to the cancers below, report the results."),
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
              
              ###### Surgical Hx ####
              tabPanel("Surgical Hx",
                h3("Prophylactic Surgical History"),
                
                # message for proph surgery is Female sex is not selected
                conditionalPanel("input.Sex != 'Female'",
                  h5("Prophylactic surgery information is only required for females.")
                ),
                 
                # for females
                conditionalPanel("input.Sex == 'Female'",
                  p("Check each surgery the person has had and enter the age at surgery."),
                    
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
                          textOutput("validMastAge"),
                          tags$head(tags$style("#validMastAge{color: red;}"))
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
                          textOutput("validHystAge"),
                          tags$head(tags$style("#validHystAge{color: red;}"))
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
                          textOutput("validOophAge"),
                          tags$head(tags$style("#validOophAge{color: red;}"))
                        )
                      )
                    )
                  )
                ) # end of female conditionalPanel for surgical history information
              ), # end of surgery tab
              
              ###### Genes ####
              tabPanel("Genes",
                h3("Gene Testing Results"),
                tabsetPanel(id = "geneTabs",
                            
                  tabPanel(title = "Instructions",
                    h4("How to Enter/Edit Germline Genetic Test Results"),
                    p("Use this screen to enter or modify germline genetic test results 
                      for the currently selected relative. There are three tabs: 
                      1) 'Manage Panels' allows you to add a multi-gene panel or single 
                      gene test for the currently selected relative or delete one of their tests; 
                      2) 'Edit Panel' allows you to edit this relative's gene test 
                      results, if they have at least one test; 
                      3) 'Summary Table' displays a summary of results from all of the 
                      relative's germline genetic tests by gene, result, nucleotide, protein, and zygosity.", 
                      style = "margin-bottom:10px")
                  ),
                            
                  tabPanel(title = "Manage Panels",
                    fluidRow(column(width = 12, 
                      h4("Current Panel Tests"),
                      tags$div(
                        id = "PanCont",
                        style = "width:100%"
                      ),
                      conditionalPanel("!output.atLeastOnePanel",
                        h5("This relative does not have any gene testing results yet.")
                      ),
                      br(),
                      h4("Add a Panel"),
                      p("Using the dropdown, select one of the pre-existing panels and click 'Add Panel' or select 'Create new' 
                        to make a custom panel of genes."),
                      selectInput("existingPanels", label = NULL,
                                  choices = all.panel.names, selected = "No panel selected",
                                  width = "300px"),
                      actionButton("addPanel", label = "Add Panel",
                                   style = "color: white; background-color: #10699B; border-color: #10699B; margin-top: 0px; margin-bottom:15px"),
                      
                      # create new panel
                      conditionalPanel("input.existingPanels == 'Create new'",
                        p("Enter the genes in your panel below. 
                          When you start typing, the dropdown will filter to genes for you to select. 
                          You can also add genes that are not in the dropdown. When done, give it a name and select the 
                          'Create and Add Panel' button."),
                        textInput("newPanelName", label = h5("Name the new panel:"), width = "250px"),
                        selectizeInput("newPanelGenes", label = h5("Type or select the genes in this panel:"),
                                       choices = all.genes, multiple = TRUE,
                                       width = "500px"),
                        actionButton("createPanel", label = "Create and Add Panel",
                                     style = "color: white; background-color: #10699B; border-color: #10699B; margin-top: 0px; margin-bottom:15px")
                      )
                    ))
                  ),
                  
                  tabPanel(title = "Edit Panel",
                    fluidRow(column(width = 12, 
                      h4("Edit Panel Results"),
                                    
                      # cannot enter results if no panels exist
                      conditionalPanel("!output.atLeastOnePanel",
                        p("To edit panel test results, please add at least one panel on the 'Manage Panels' tab.")
                      ),
                         
                      # enter results by type
                      conditionalPanel("output.atLeastOnePanel",
                        p("Select one of the subject's panels to edit then enter the gene results by selecting the three different tabs for ",
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
                                             style = "color: white; background-color: #10699B; border-color: #10699B; margin-top: 10px")
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
                                             style = "color: white; background-color: #10699B; border-color: #10699B; margin-top: 10px"),
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
                                             style = "color: white; background-color: #10699B; border-color: #10699B; margin-top: 10px")
                              )
                            )
                          ) # end of tabsetPanel for gene results by type
                        ) # end of conditionalPanel for entering gene results by type
                      ) # end of conditionalPanel for checking if a panel to edit has been selected
                    )), # end of column and fluidRow
                  ), # end of tab for gene results entry and panel selection 
                  
                  # tab for review panel results
                  tabPanel(title = "Summary Table",
                    h4("Gene Summary"),
                    conditionalPanel("!output.atLeastOnePanel",
                      h5("This relative does not have any panel tests; therefore, a summary table cannot be displayed.")
                    ),
                    conditionalPanel("output.atLeastOnePanel",
                      h5("The table below is a summary of all the genes in all of panels for this individual. 
                         Genes are marked a negative until they are recorded as 
                         P/LP, VUS, or B/LP in any of their panels."),
                      
                      # warn user there is at least one gene with different result types recorded
                      conditionalPanel("output.dupResultGene",
                        h5("There is at least one gene for this relative which has multiple different result types recorded. 
                          This is possible but, it could be an error. Please check the table below for accuracy.
                          If one of these result types is P/LP, PanelPRO will treat this gene as P/LP.", 
                          style = "color:red")
                      ),
                      
                      # data frame with panel summary information
                      DT::dataTableOutput("panelSum")
                    )
                  ) # end of tab for gene results summary
                ) # end tabsetPanel for gene results screen
              ), # end of gene results tab
              
              ###### Num/Type Rels ####
              tabPanel("Add Relatives",
                h3("Number and Types of Relatives"),
                p("Begin creating the proband's pedigree by entering the number of 
                  each relative type below. Relative types not listed on this screen 
                  can be added later on."),
                
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
                      numericInput("numSon",
                                   label = h5("Sons:"),
                                   value = 0,
                                   min = 0,
                                   step = 1, 
                                   width = "125px")
                    ),
                    
                    wellPanel(
                      h4("Siblings"),
                      numericInput("numSis",
                                   label = h5("Sisters:"),
                                   value = 0,
                                   min = 0,
                                   step = 1, 
                                   width = "125px"),
                      numericInput("numBro",
                                   label = h5("Brothers:"),
                                   value = 0,
                                   min = 0,
                                   step = 1, 
                                   width = "125px")
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
                      numericInput("numMUnc",
                                   label = h5("Maternal Uncles:"),
                                   value = 0,
                                   min = 0,
                                   step = 1, 
                                   width = "125px")
                    ),
                    
                    wellPanel(
                      h4("Paternal Relatives"),
                      numericInput("numPAunt",
                                   label = h5("Paternal Aunts:"),
                                   value = 0,
                                   min = 0,
                                   step = 1, 
                                   width = "125px"),
                      numericInput("numPUnc",
                                   label = h5("Paternal Uncles:"),
                                   value = 0,
                                   min = 0,
                                   step = 1, 
                                   width = "125px")
                    )
                  ) # end of column for aunts and uncles
                ), # end of fluidRow for the entire num/type rel tab
                
                # button to create visual pedigree
                h4("To Continue"),
                h5("Press the button below to create the proband's pedigree."),
                actionButton("visPed", label = "Create Pedigree", icon = icon('play'),
                             style = "color: white; background-color: #10699B; border-color: #10699B")
                
              ) # end of number and type of rels tab
            ) # end of tabsetPanel for data entry
          ), # end of column for data entry
        ) # end of fluidRow for create/modify pedigree tab
      ), # end of tab for create/modify pedigree
      
      ##### Run PanelPRO ####
      tabPanel("Run PanelPRO",
        h3("Run PanelPRO")
      ), # end of PanelPRO tab
      
      ##### My Account ####
      tabPanel("My Account",
        h3("Account Management"),
        
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
                       style = "color: white; background-color: #10699B; border-color: #10699B; margin-top: 0px; margin-bottom: 25px")
        ),
        br(),
        
        h4("Permissions"),
        conditionalPanel("output.admin",
          p("Your user account has admin level permissions.")
        ),
        conditionalPanel("output.manager",
          p("Your user account has manager level permissions. This means you can load, run, view, edit, save, 
            download and delete pedigrees from any user account which listed your username as one of their managers.")
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
  
  #### Footer ####
  br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
  
  
  #### Tab Switching Tags #####
  
  # automatically go to top of tab when selecting a tab
  tags$script(" $(document).ready(function () {
         $('#navbarTabs a[data-toggle=\"tab\"]').on('click', function (e) {
          window.scrollTo(0, 0)
               });
               });"
  ),
  # automatically go to top of tab when selecting a tab
  tags$script(" $(document).ready(function () {
         $('#pedTabs a[data-toggle=\"tab\"]').on('click', function (e) {
          window.scrollTo(0, 0)
               });
               });"
  )
  
    
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
      mod.ped.cols <- ped.cols
      mod.ped.cols[which(ped.cols == "CK5.6")] <- "CK5_6"
      saveTableToMaster(conne = conn,
                        user = input$newUsername,
                        tmp_tbl = example_pedigree,
                        col.info = setNames(ped.col.dtypes, mod.ped.cols))
      
      shinyjs::refresh()
      
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
  
  
  ##### Add Managers from My Account Tab ####
  
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
  
  
  #### Manage User Pedigrees ####
  ##### Load/Create New Pedigree ####
  # on start-up, hide the non-Home navbarTabs (should not show until a pedigree is create as new or loaded)
  observe({
    hideTab("navbarTabs", target = "Create/Modify Pedigree", session = session)
    hideTab("navbarTabs", target = "Run PanelPRO", session = session)
  })
  
  # check that loading a pedigree is possible based on if at least one pedigree is selected
  # and enable/disable download buttons accordingly
  observeEvent(list(input$existingPed, input$newOrLoad), {
    if(input$newOrLoad == "Load existing" & input$existingPed == "No pedigree selected"){
      shinyjs::disable("goNewOrLoad")
    } else {
      shinyjs::enable("goNewOrLoad")
    }
  }, ignoreInit = T)
  
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
          userPeds(unique(dbGetQuery(conn = conn,
                                     statement = paste0("SELECT PedigreeID FROM ", 
                                                        credentials()$info[["user"]], 
                                                        ";"))$PedigreeID))
          
          # update the tables available for loading
          updateSelectInput(session, inputId = "existingPed",
                            choices = userPeds())
        } else {
          showTblExistsError(TRUE)
        }
      }
    }
  })
  
  # update drop-down for selecting a pedigree for the admin and managers when the selected user account changes
  observeEvent(list(input$selectUser, userPeds()), {
    if(loggedIn()){
      if(admin() | manager()){
        userPeds(unique(dbGetQuery(conn = conn,
                                   statement = paste0("SELECT PedigreeID FROM ", 
                                                      ifelse(input$selectUser == "", credentials()$info[["user"]], 
                                                             input$selectUser), 
                                                      ";"))$PedigreeID))
        updateSelectInput(session, inputId = "existingPed",
                          choices = c("No pedigree selected", userPeds()))
      }
    }
  })
  
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
      updateRelInputs(rel.info = proband.info, ss = session)
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
                            ss = session)
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
        observeEvent(list(input[[paste0(id, '-Can')]], input[[paste0(id, '-CBC')]], input$relSelect), {
          
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
      
      ## GENES
      # create and populate panelUI modules
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
                          pan.name = master.gene.df$panel[x])
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
                                  ss = session)
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
      
      # hide add relatives tab and visualize the pedigree
      hideTab("pedTabs", target = "Add Relatives", session = session)
      shinyjs::click("visPed")
      
      ###### CREATE NEW PEDIGREE
    } else if(input$newOrLoad == "Create new"){
      newOrLoadFlag("new")
      PED(NULL)
      
      ## reset inputs and input reactives
      shinyjs::reset("relSelect")
      lastRel(1)
      
      # demo
      shinyjs::enable("pedID")
      shinyjs::enable("Sex")
      for(demo.var in c("pedID", "Sex", "Age", "race", "eth", "ancAJ", "ancIt")){
        shinyjs::reset(demo.var)
      }
      
      # cancers
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
      geneReactive$GeneNums <- trackGenes.init
      
      # add relatives
      visPed(FALSE)
      
    } # end of else for creating a new pedigree
    
    # execute actions relevant to create new and load existing
    if(input$newOrLoad == "Create new" | 
       (input$newOrLoad == "Load existing" & input$existingPed != "No pedigree selected")){
      
      # reset add relative counts
      for(relation in c("Dau", "Son", "Sis", "Bro", "MAunt", "MUnc", "PAunt", "PUnc")){
        shinyjs::reset(paste0("num", relation))
      }
      
      updateSelectInput(session, "selectUser", selected = credentials()$info[["user"]])
      updateSelectInput(session, "selectUserForDownload", selected = credentials()$info[["user"]])
      
      # show the pedigree and panelpro tabs when the button is clicked the first time
      showTab("navbarTabs", target = "Create/Modify Pedigree", session = session)
      showTab("navbarTabs", target = "Run PanelPRO", session = session)
      
      # update selected tabs
      updateTabsetPanel(session, "pedTabs", selected = "Demographics")
      updateTabsetPanel(session, "geneTabs", selected = "Instructions")
      updateTabsetPanel(session, "geneResultTabs", selected = "P/LP")
      updateNavlistPanel(session, "navbarTabs", selected = "Create/Modify Pedigree")
    }
  }, ignoreInit = T)
  
  ##### Download Pedigrees ####
  # keep dropdowns of user tables and pedigrees available for downloading updated at all times
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
          userPedsForDownload(
            unique(dbGetQuery(conn = conn,
                              statement = paste0("SELECT PedigreeID FROM ", 
                                                 credentials()$info[["user"]], 
                                                 ";"))$PedigreeID)
          )
          
          # update the tables available for loading
          updateSelectInput(session, inputId = "selectDownloadPeds",
                            choices = userPedsForDownload())
        } else {
          showTblExistsErrorDownload(TRUE)
        }
      }
    }
  })
  
  # update drop-down for selecting a pedigrees to download for the admin and managers
  # when the selected user account changes
  observeEvent(input$selectUserForDownload, {
    if(loggedIn()){
      if(admin() | manager()){
        userPedsForDownload(
          unique(dbGetQuery(conn = conn,
                            statement = paste0("SELECT PedigreeID FROM ",
                                               ifelse(input$selectUserForDownload == "", credentials()$info[["user"]],
                                                      input$selectUserForDownload),
                                               ";"))$PedigreeID))
        updateSelectInput(session, inputId = "selectDownloadPeds",
                          choices = userPedsForDownload())
      }
    }
  })
  
  # select or de-select all pedigrees in a user account for downloading
  observeEvent(list(userPedsForDownload(), input$selectAllPeds), {
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
      shinyjs::enable("downloadPedsCSV")
      shinyjs::enable("downloadPedsRDS")
    } else {
      shinyjs::disable("downloadPedsCSV")
      shinyjs::disable("downloadPedsRDS")
    }
  }, ignoreNULL = F)
  
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
  
  # download one or more pedigrees from the user account from the "Manage Pedigrees" tab
  # as a .csv
  output$downloadPedsCSV <- shiny::downloadHandler(
    filename = function(){
      paste0("PanelPRO-pedigrees-", Sys.Date(), input$downloadAs1)
    },
    content = function(file){
      write.csv(downloadPedsTable(), file, row.names = F)
    }
  )
  
  # download one or more pedigrees from the user account from the "Manage Pedigrees" tab
  # as a .rds
  output$downloadPedsRDS <- downloadHandler(
    filename = function(){
      paste0("PanelPRO-pedigrees-", Sys.Date(), input$downloadAs1)
    },
    content = function(file){
      saveRDS(downloadPedsTable(), file)
    }
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
          userPedsForDelete(
            unique(dbGetQuery(conn = conn,
                              statement = paste0("SELECT PedigreeID FROM ", 
                                                 credentials()$info[["user"]], 
                                                 ";"))$PedigreeID)
          )
          
          # update the tables available for deletion
          updateSelectInput(session, inputId = "selectDeletePeds",
                            choices = userPedsForDelete())
        } else {
          showTblExistsErrorDelete(TRUE)
        }
      }
    }
  })
  
  # update drop-down for selecting pedigrees to delete for the admin and managers
  # when the selected user account changes
  observeEvent(input$selectUserForDelete, {
    if(loggedIn()){
      if(admin() | manager()){
        userPedsForDelete(
          unique(dbGetQuery(conn = conn,
                            statement = paste0("SELECT PedigreeID FROM ",
                                               ifelse(input$selectUserForDelete == "", credentials()$info[["user"]],
                                                      input$selectUserForDelete),
                                               ";"))$PedigreeID))
        updateSelectInput(session, inputId = "selectDeletePeds",
                          choices = userPedsForDelete())
      }
    }
  })

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
      shinyjs::enable("deletePeds")
    } else {
      shinyjs::disable("deletePeds")
    }
  }, ignoreNULL = F)
  
  # delete the selected pedigrees
  observeEvent(input$deletePeds, {
    if(admin() | manager()){
      fromAcct <- ifelse(input$selectUserForDelete == "", credentials()$info[["user"]],
                         input$selectUserForDelete)
    } else {
      fromAcct <- credentials()$info[["user"]]
    }
    dbExecute(conn = conn,
              statement = paste0("DELETE FROM ", fromAcct, 
                                 " WHERE PedigreeID IN ('", 
                                 paste0(input$selectDeletePeds, collapse = "','"), 
                                 "';"))
  }, ignoreInit = T)
  
  ##### Save Pedigree #######
  observeEvent(input$savePed, {
    if(!is.null(PED())){
      
      # save data for currently selected relative to the pedigree prior to saving pedigree to database
      PED(saveRelDatCurTab(tped = PED(), rel = input$relSelect, inp = input,
                           cr = canReactive$canNums,
                           sr = surgReactive$lst,
                           gr = geneReactive$GeneNums,
                           dupResultGene = dupResultGene(),
                           sx = input$Sex)
      )
      
      # the database cannot have columns with . in the name
      s.ped <- PED()
      colnames(s.ped)[which(colnames(s.ped) == "CK5.6")] <- "CK5_6"
      mod.ped.cols <- ped.cols
      mod.ped.cols[which(ped.cols == "CK5.6")] <- "CK5_6"
      
      # save
      saveTableToMaster(conne = conn,
                        user = credentials()$info[["user"]],
                        tmp_tbl = s.ped,
                        col.info = setNames(ped.col.dtypes, mod.ped.cols))
      
      # update the list of pedigrees that can be loaded, downloaded, and deleted
      updated.peds <- unique(dbGetQuery(conn = conn,
                                        statement = paste0("SELECT PedigreeID FROM ", 
                                                           credentials()$info[["user"]], 
                                                           ";"))$PedigreeID)
      userPeds(updated.peds)
      userPedsForDownload(updated.peds)
      userPedsForDelete(updated.peds)
    }
  }, ignoreInit = T)
  
  #### Demographics / Create Pedigree ####
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
  
  # validate current age
  validAge <- reactive({
    shiny::validate(validateAge(input$Age))
  })
  output$validAge <- renderText({ validAge() })
  
  # do not allow user to move to other pedTabs if there is not enough information to make the pedigree
  pbMinInfo <- reactiveVal(FALSE)
  output$pbMinInfo <- reactive({ pbMinInfo() })
  outputOptions(output, 'pbMinInfo', suspendWhenHidden = FALSE)
  observeEvent(list(input$pedID, input$Sex, input$Age, validAge(), PED()), {
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
  }, ignoreInit = T)
  
  # hide/show tabs if on the demographics tab based on if minimum information to create
  # a pedigree is present or not
  observeEvent(pbMinInfo(), {
    if(!pbMinInfo()){
      hideTab("pedTabs", "Cancer Hx", session)
      hideTab("pedTabs", "CBC Risk", session)
      hideTab("pedTabs", "Surgical Hx", session)
      hideTab("pedTabs", "Tumor Markers", session)
      hideTab("pedTabs", "Genes", session)
      hideTab("pedTabs", "Add Relatives", session)
      hideTab("pedTabs", "Family Tree and Relative Information", session)
    } else if(pbMinInfo()){
      showTab("pedTabs", "Cancer Hx", select = FALSE, session)
      showTab("pedTabs", "CBC Risk", select = FALSE, session)
      showTab("pedTabs", "Surgical Hx", select = FALSE, session)
      showTab("pedTabs", "Tumor Markers", select = FALSE, session)
      showTab("pedTabs", "Genes", select = FALSE, session)
      showTab("pedTabs", "Add Relatives", select = FALSE, session)
      showTab("pedTabs", "Family Tree and Relative Information", select = FALSE, session)
    }
  })
  
  # instantiate the pedigree reactive
  PED <- reactiveVal(NULL)
  
  # check if ped exists for conditionalPanels
  pedExists <- reactiveVal(FALSE)
  output$pedExists <- reactive({ pedExists() })
  outputOptions(output, 'pedExists', suspendWhenHidden = FALSE)
  observeEvent(PED(), {
    if(is.null(PED())){
      pedExists(FALSE)
      shinyjs::disable("savePed")
    } else {
      pedExists(TRUE)
      shinyjs::enable("savePed")
    }
  }, ignoreNULL = F)
  
  # initialize the pedigree when user leave the proband demographics tab
  onDemoTab <- reactiveVal(TRUE)
  observeEvent(input$pedTabs, {
    
    # execute if the previous tab was the proband demographics tab and the current tab is different
    if(onDemoTab() & input$pedTabs != "Demographics" & pbMinInfo()){
      
      # lock fields
      shinyjs::disable("pedID")
      shinyjs::disable("Sex")
      
      # initialize new pedigree with proband and parents if no pedigree exists
      create.parents <- FALSE
      if(is.null(PED())){
        create.parents <- TRUE
        if(input$Sex == "Female"){
          ps <- 0
        } else if(input$Sex == "Male"){
          ps <- 1
        } else {
          ps <- NA
        }
        PED(initPed(pedigree.id = input$pedID, pb.sex = ps))
      }
      
      # populate proband's demographics data and PedigreeID
      t.ped <- PED()
      t.ped <- popPersonData(tmp.ped = t.ped, id = input$relSelect, cur.age = input$Age, 
                             rc = input$race, et = input$eth, 
                             an.aj = input$ancAJ, an.it = input$ancIt)
      
      # if this is this is the initialization of the three person pedigree, add the parents
      if(create.parents){
        
        # populate mother's race and Ancestry information
        t.ped <- popPersonData(tmp.ped = t.ped, id = t.ped$MotherID[which(t.ped$isProband == 1)], 
                               rc = input$race, et = input$eth, 
                               an.aj = input$ancAJ, an.it = input$ancIt)
        
        # populate father's race and Ancestry information
        t.ped <- popPersonData(tmp.ped = t.ped, id = t.ped$FatherID[which(t.ped$isProband == 1)], 
                               rc = input$race, et = input$eth, 
                               an.aj = input$ancAJ, an.it = input$ancIt)
      }
      
      # update the pedigree
      PED(t.ped)
      
    } # if statement to check whether the pedigree could be created based on the tab and minimum info required
    
    # update the reactive value to detect if the current tab is the target tab
    if(input$pedTabs == "Demographics"){
      onDemoTab(TRUE)
    } else {
      onDemoTab(FALSE)
    }
  }, ignoreInit = TRUE)
  
  
  
  
  # FOR TESTING: VIEW PEDIGREE EVERY TIME IT CHANGES
  observeEvent(PED(), { View(PED()) }, ignoreNULL = T)
  
  
  
  
  #### Cancer History ####
  # save the number of cancers for each person in the pedigree
  canReactive <- reactiveValues(canNums = trackCans.init)
  
  
  
  # # FOR TESTING
  # observeEvent(canReactive$canNums, { View(canReactive$canNums )})
  
  
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
    observeEvent(list(input[[paste0(id, '-Can')]], input[[paste0(id, '-CBC')]], input$relSelect), {
      
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
    }
    
    # update the reactive value to detect if the current tab is the target tab
    if(input$pedTabs == "Cancer Hx"){
      onCanTab(TRUE)
    } else {
      onCanTab(FALSE)
    }
  }, ignoreInit = TRUE)
  
  #### CBC Risk ####
  # keep track of whether CBC inputs should be shown or not
  showCBCinputs <- reactiveVal(FALSE)
  output$showCBCinputs <- reactive({ showCBCinputs() })
  outputOptions(output, 'showCBCinputs', suspendWhenHidden = FALSE)
  observeEvent(list(PED(), input$relSelect), {
    if(is.null(PED())){
      showCBCinputs(FALSE)
    } else {
      
      # check updated cancers list for presence of BC and CBC
      if(PED()$isAffBC[which(PED()$ID == as.numeric(input$relSelect))] == 1 & 
         PED()$isAffCBC[which(PED()$ID == as.numeric(input$relSelect))] == 0){ 
        showCBCinputs(TRUE)
      } else {
        showCBCinputs(FALSE)
      }
      
      # check if any previously recorded CBC related inputs need to be removed and update them
      rmCBCinputs <- FALSE
      if(!showCBCinputs() & 
         any(!is.na(PED()[which(PED()$ID == input$relSelect), cbcrisk.cols]))){
        rmCBCinputs <- TRUE
        for(cbc.var in cbcrisk.cols){
          shinyjs::reset(cbc.var)
        }
      }
      
      # update CBC risk columns in pedigree if required
      if(rmCBCinputs){
        PED(popPersonData(tmp.ped = PED(), id = input$relSelect, 
                          cbc.info = list(FirstBCType = input$FirstBCType,
                                          AntiEstrogen = input$AntiEstrogen,
                                          HRPreneoplasia = input$HRPreneoplasia,
                                          BreastDensity = input$BreastDensity,
                                          FirstBCTumorSize = input$FirstBCTumorSize)))
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
    }
    
    # update the reactive value to detect if the current tab is the target tab
    if(input$pedTabs == "CBC Risk"){
      onCBCTab(TRUE)
    } else {
      onCBCTab(FALSE)
    }
  }, ignoreInit = TRUE)
  
  #### Tumor Markers ####
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
      if(!hadBC & any(!is.na(PED()[which(PED()$ID == input$relSelect), PanelPRO:::MARKER_TESTING$BC$MARKERS]))){
        rmBCmarks <- TRUE
        for(m in PanelPRO:::MARKER_TESTING$BC$MARKERS){
          m <- ifelse(m == "CK5.6", "CK56", m)
          updateSelectInput(session, m, selected = "Not Tested")
        }
      }
      if(!hadCRC & any(!is.na(PED()[which(PED()$ID == input$relSelect), PanelPRO:::MARKER_TESTING$COL$MARKERS]))){
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
    }
    
    # update the reactive value to detect if the current tab is the target tab
    if(input$pedTabs == "Tumor Markers"){
      onMarkerTab(TRUE)
    } else {
      onMarkerTab(FALSE)
    }
  }, ignoreInit = TRUE, ignoreNULL = FALSE)
  
  
  #### Surgical History ####
  
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
      if(PED()$Sex[which(PED()$ID == input$relSelect)] == 0){
        if(input$Sex == "Male"){
          for(sg in c("Mast", "Hyst", "Ooph")){
            updateCheckboxInput(session, sg, value = FALSE)
            updateNumericInput(session, paste0(sg,"Age"), value = NA)
            tmp.ped <- PED()
            tmp.ped[[paste0("riskmod", sg)]][which(tmp.ped$ID == input$relSelect)] <- 0
            tmp.ped[[paste0("interAge", sg)]][which(tmp.ped$ID == input$relSelect)] <- NA
            PED(tmp.ped)
          }
        }
      }
    }
  })
  
  ## validate surgery ages
  # Oophorectomy age
  validOophAge <- reactive({
    shiny::validate(validateSurgAge(input$OophAge, input$Age, PED()$AgeOC[which(PED()$ID == input$relSelect)]))
  })
  output$validOophAge <- renderText({ validOophAge() })
  
  # Mastectomy age
  validMastAge <- reactive({
    shiny::validate(validateSurgAge(input$MastAge, input$Age, PED()$AgeCBC[which(PED()$ID == input$relSelect)]))
  })
  output$validMastAge <- renderText({ validMastAge() })
  
  # Hysterectomy age
  validHystAge <- reactive({
    shiny::validate(validateSurgAge(input$HystAge, input$Age, PED()$AgeENDO[which(PED()$ID == input$relSelect)]))
  })
  output$validHystAge <- renderText({ validHystAge() })
  
  # add data to pedigree when user navigates off of the tab
  onSurgTab <- reactiveVal(FALSE)
  observeEvent(input$pedTabs, {
    if(onSurgTab() & input$pedTabs != "Surgical Hx" & !is.null(PED())){
      PED(popPersonData(tmp.ped = PED(), id = input$relSelect, riskmods.and.ages = surgReactive$lst))
    }
    
    # update the reactive value to detect if the current tab is the target tab
    if(input$pedTabs == "Surgical Hx"){
      onSurgTab(TRUE)
    } else {
      onSurgTab(FALSE)
    }
  }, ignoreInit = TRUE)
  
  
  #### Genes ####
  # track the panel and gene UI modules
  geneReactive <- reactiveValues(GeneNums = trackGenes.init)
  
  
  # # FOR TESTING
  # observeEvent(geneReactive$GeneNums, { View(geneReactive$GeneNums )})
  
  
  
  ##### Panels ####
  
  ### check if the current relative has a least one panel
  atLeastOnePanel <- reactiveVal(FALSE)
  output$atLeastOnePanel <- reactive({ atLeastOnePanel() })
  outputOptions(output, 'atLeastOnePanel', suspendWhenHidden = FALSE)
  observeEvent(list(geneReactive$GeneNums, input$relSelect), {
    if(geneReactive$GeneNums[[input$relSelect]]$panels$panel1$name == "No panel selected"){
      atLeastOnePanel(FALSE)
    } else {
      atLeastOnePanel(TRUE)
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
                      pan.name = pan.name)
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
                              ss = session)
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
  
  
  ### create new panel (PLACEHOLDER)
  
  
  ##### PLP Genes ####
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
  
  
  ##### VUS Genes ####
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


  ##### BLB Genes ####
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
  
  
  ##### Summary Table & Store ####

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
  output$panelSum <- DT::renderDataTable({
    if(!is.null(panelSum())){
      datatable(panelSum()) %>%
        formatStyle('Result',
                    backgroundColor = styleEqual(c("P/LP","VUS","B/LB","Neg"),
                                                 c("MistyRose","LightGreen","AliceBlue","white")),
                    fontWeight = 'bold')
    }
  })

  # add data to pedigree when user navigates off of the tab
  onGeneTab <- reactiveVal(FALSE)
  observeEvent(input$pedTabs, {
    if(onGeneTab() & input$pedTabs != "Genes" & !is.null(PED())){
      PED(popPersonData(tmp.ped = PED(), id = input$relSelect,
                        gene.results = panelSum()))
    }
    
    # update the reactive value to detect if the current tab is the target tab
    if(input$pedTabs == "Genes"){
      onGeneTab(TRUE)
    } else {
      onGeneTab(FALSE)
    }
  }, ignoreInit = TRUE)
  
  #### Add Children, Siblings, Aunts/Uncles ####
  
  # add relatives to the pedigree when the user click the button at bottom of screen
  # populate assumed races and ancestries based on proband's mother and father info
  visPed <- reactiveVal(FALSE)
  observeEvent(input$visPed, {
    
    # update reactive value
    visPed(TRUE)
    
    # only add family members if this is a new pedigree
    if(newOrLoadFlag() == "new"){
      
      # add children
      if(input$numDau > 0 | input$numSon > 0){
        
        # first, add partner to pedigree
        PED(formatNewPerson(relation = "partner", tmp.ped = PED()))
        parnter.id <- PED()$ID[nrow(PED())]
        
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
        for(i in 1:input$numDau){
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
      
      # update relative selector with all relatives in the pedigree
      updateSelectInput(session = session, inputId = "relSelect", 
                        choices = setNames(PED()$ID, PED()$name), 
                        selected = PED()$ID[which(PED()$isProband == 1)])
      
      # hide initialize pedigree tab and reset inputs
      for(relation in c("Dau", "Son", "Sis", "Bro", "MAunt", "MUnc", "PAunt", "PUnc")){
        updateNumericInput(session, paste0("num", relation), value = 0)
      }
      hideTab("pedTabs", target = "Add Relatives", session = session)
      
    } # end of if statement for confirming the pedigree is a new creation
  }, ignoreInit = TRUE)
  
  #### Visualize Pedigree ####
  # temporarily: draw pedigree in kinship2
  # replace with pedigreejs
  output$drawPed <- renderPlot({
    plot_fam <-
      PED() %>%
      mutate(Sex = ifelse(Sex == 0, 2, Sex)) %>%
      mutate(across(.cols = c(MotherID, FatherID), ~ ifelse(is.na(.), 0, .))) %>%
      mutate(name = sub(pattern = "Daughter", replacement = "Dau", name)) %>%
      mutate(name = sub(pattern = "Sister", replacement = "Sis", name)) %>%
      mutate(name = sub(pattern = "Brother", replacement = "Bro", name)) %>%
      mutate(name = sub(pattern = "Uncle", replacement = "Unc", name)) %>%
      mutate(name = sub(pattern = "Grandmother", replacement = "GMom", name)) %>%
      mutate(name = sub(pattern = "Grandfather", replacement = "GDad", name)) %>%
      mutate(name = sub(pattern = "Mother", replacement = "Mom", name)) %>%
      mutate(name = sub(pattern = "Father", replacement = "Dad", name)) %>%
      mutate(name = sub(pattern = "Mat. ", replacement = "M", name)) %>%
      mutate(name = sub(pattern = "Pat. ", replacement = "P", name)) %>%
      mutate(name = gsub(pattern = " ", replacement = "", name)) %>%
      mutate(nameMother = "") %>%
      mutate(nameFather = "") %>%
      select(PedigreeID, ID, name, MotherID, nameMother, FatherID, nameFather, Sex)
    
    # replace mother and father ID numbers with names
    for(uid in unique(plot_fam$MotherID)){
      if(uid != 0){
        pname <- plot_fam$name[which(plot_fam$ID == uid)]
        plot_fam$nameMother[which(plot_fam$MotherID == uid)] <- pname
      }
    }
    for(uid in unique(plot_fam$FatherID)){
      if(uid != 0){
        pname <- plot_fam$name[which(plot_fam$ID == uid)]
        plot_fam$nameFather[which(plot_fam$FatherID == uid)] <- pname
      }
    }
    
    
    
    
    View(plot_fam)
    
    
    
    
    # plot
    dped <- pedigree(id = plot_fam$name,
                     momid = plot_fam$nameMother,
                     dadid = plot_fam$nameFather,
                     sex = plot_fam$Sex,
                     famid = plot_fam$PedigreeID)
    plot(dped[paste0(input$pedID)])
  })
  
  #### Switch Selected Relative ####
  # initialize the ID of the last relative selected with proband
  lastRel <- reactiveVal(1)
  
  # 1) save data to pedigree when the relative is switched or if navbarTabs change
  # 2) repopulate inputs with new relative's data from the pedigree
  observeEvent(list(input$relSelect, input$navbarTabs), {
    
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
      
      # update the last relative selected
      lastRel(as.numeric(input$relSelect))
      
      # Re-populate data for new person
      rel.info <- PED()[which(PED()$ID == as.numeric(input$relSelect)),]
      updateRelInputs(rel.info = rel.info, ss = session)
      
      # reset the selected tabs
      updateTabsetPanel(session, "geneTabs", selected = "Instructions")
      updateTabsetPanel(session, "geneResultTabs", selected = "P/LP")
      
    } # end of if statement for input$visPed == TRUE
  }, ignoreInit = TRUE)
  
  #### PanelPRO ####

  
}

# Run the application 
shinyApp(ui = ui, server = server)
