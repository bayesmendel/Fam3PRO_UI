# libraries
library(shiny)
library(shinyBS) # shiny tool tips
library(shinyjs) # java script tools
library(kinship2) # draws pedigrees (this is temporary only)
library(PanelPRO)

# data manipulation
library(tidyverse)
library(rlang)

# html
library(htmltools)

# line and bar plots
library(ggplot2)
library(plotly)
library(ggplotify)
library(ggpattern)

# email
library(gmailr)
library(httr)

# tables
library(DT)

# load data


# utils and variables
source("./vars.R")
source("./utils.R")
source("./modules.R")

#### UI ####
ui <- fixedPage(
  
  titlePanel("PPI: PanelPRO Interface"),
  
  # Google analytics
  # tags$head(includeHTML(("google-analytics.html"))),
  
  # allows shinyjs commands like disable (and others?)
  useShinyjs(),
  
  navbarPage(title = "", id = "navbarTabs",
    
    ##### Home ####
    tabPanel(title = "Home",
      h3("What is PPI?"),
      
      h3("How to Use PPI"),
      
      h3("Support and Contact Information"),
      
    ), # end of tab
    
    ##### Create/Modify Pedigree ####
    tabPanel("Create/Modify Pedigree",
             
      # create 2 columns, one for displaying the pedigree (left) and one for data entry (right)
      fluidRow(
        
        # only show pedigree visualization after pedigree has been initialized with all FDR, aunts, and uncles
        conditionalPanel("input.visPed",
          column(width = 6,
            plotOutput("drawPed")
          )
        ),
        
        # column for pedigree data entry, full width at first, then 1/2 width once pedigree is visualized
        column(width = 6, 
               
          # select which relative is being edited, only show after pedigree is visualized
          conditionalPanel("input.visPed",
            selectInput("relSelect", label = h4("Select a relative to edit:"),
                        choices = c(1), # placeholder, this will be updated once FDR+AU ped initialized
                        width = "200px")
          ),
               
          tabsetPanel(id = "pedTabs", type = "pills",
            
            ###### Demographics ####
            tabPanel("Demographics",
              h3("Demographics"),
              p("Enter the person's demographic information below. Inputs with an 
                astrick(*) require a response to continue to the next screen."),
              textInput("pedID", label = h5("*Unique Proband or Pedigree ID:"),
                        value = "",
                        width = "225px"),
              conditionalPanel("!input.visPed",
                h5("The ID number above must not contain any identifying information. 
                   It also cannot be the proband's MRN.",
                   style = "color:red")
              ),
              selectInput("Sex", label = h5("*Sex assigned at birth:"),
                          choices = sex.choices,
                          width = "150px"),
              numericInput("Age",
                           label = h5("*Current Age (1 to 89):"),
                           value = NA, min = min.age, max = max.age, step = 1,
                           width = "150px"),
              textOutput("validAge"),
              tags$head(tags$style("#validAge{color: red;}")),
              
              # create subject's individual race, ethnicity, and ancestry inputs
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
              
              # enter cancers
              tags$div(
                id = "canContainer",
                style = "width:100%"
              ),
              actionButton("addCan", label = "Add Cancer",
                           icon = icon('plus'),
                           style = "color: white; background-color: #10699B; border-color: #10699B; margin-top: 20px")
            ), # end of cancers tab
            
            ###### Tumor Markers ####
            tabPanel("Tumor Markers",
              h3("Tumor Markers"),
              conditionalPanel("!output.showBCMarkers & !output.showCRCMarkers",
                p("Tumor markers are only applicable if the person has/had breast cancer or colorectal cancer")
              ),
              conditionalPanel("output.showBCMarkers | output.showCRCMarkers",
                p("If the person was tested for any of the tumor markers related to the cancers below, report the results."),
                br(),
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
                  ),
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
                  ),
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
                        benign/likely benign (B/LP)</b>.")," Any genes not specified as P/LP, VUS, or B/LB
                        will be recorded as negative.", 
                        style = "margin-bottom:25px"),
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
                      p("There is at least one gene for this relative which has multiple different result types recorded. 
                        This is possible but, it could be an error. Please check the table below for accuracy.
                        If one of these result types is P/LP, PanelPRO will treat this gene as P/LP.", 
                        style = "color:red")
                    ),
                    
                    # data frame with panel summary information
                    dataTableOutput("panelSum")
                  )
                ) # end of tab for gene results summary
              ) # end tabsetPanel for gene results screen
            ), # end of gene results tab
            
            ###### Num/Type Rels ####
            tabPanel("Initialize Pedigree",
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
  ),
  
  ##### Footer ####
  br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
  
  
  ##### Tab Switching Tags #####
  
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

#### Server ####
server <- function(input, output, session) {
  
  #### Demographics / Create Pedigree ####
  
  # validate current age
  validAge <- reactive({
    validate(validateAge(input$Age))
  })
  output$validAge <- renderText({ validAge() })
  
  # do not allow user to move to other pedTabs if there is not enough information to make the pedigree
  pbMinInfo <- reactiveVal(FALSE)
  observeEvent(list(input$pedID, input$Sex, input$Age, validAge(), PED()), {
    if(!input$visPed & input$pedID != "" & input$Sex != " " & !is.na(input$Age) & is.null(validAge())){
      pbMinInfo(TRUE)
    } else if(input$visPed){
      pbMinInfo(TRUE)
    } else {
      pbMinInfo(FALSE)
    }
  })
  output$pbMinInfo <- reactive({ pbMinInfo() })
  outputOptions(output, 'pbMinInfo', suspendWhenHidden = FALSE)
  
  # hide/show tabs if on the demographics tab based on if minimum information to create
  # a pedigree is present or not
  observeEvent(list(input$navbarTabs, pbMinInfo()), {
    if(input$pedTabs == "Demographics"){
      if(!pbMinInfo()){
        hideTab("pedTabs", "Surgical Hx", session)
        hideTab("pedTabs", "Tumor Markers", session)
        hideTab("pedTabs", "Cancer Hx", session)
        hideTab("pedTabs", "Genes", session)
        hideTab("pedTabs", "Initialize Pedigree", session)
        hideTab("pedTabs", "Family Tree and Relative Information", session)
      } else if(pbMinInfo()){
        showTab("pedTabs", "Surgical Hx", select = FALSE, session)
        showTab("pedTabs", "Tumor Markers", select = FALSE, session)
        showTab("pedTabs", "Cancer Hx", select = FALSE, session)
        showTab("pedTabs", "Genes", select = FALSE, session)
        showTab("pedTabs", "Initialize Pedigree", select = FALSE, session)
        showTab("pedTabs", "Family Tree and Relative Information", select = FALSE, session)
      }
    }
  })
  
  # initialize the pedigree when user leave the proband demographics tab
  PED <- reactiveVal(NULL)
  onDemoTab <- reactiveVal(TRUE)
  observeEvent(input$pedTabs, {
    
    # lock the sex field - sex's will be handled by pedigreejs
    shinyjs::disable("Sex")
    
    # execute if the previous tab was the proband demographics tab and the current tab is different
    if(onDemoTab() & input$pedTabs != "Demographics" & pbMinInfo()){
      
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
    }
    
    # update the reactive value to detect if the current tab is the target tab
    if(input$pedTabs == "Demographics"){
      onDemoTab(TRUE)
    } else {
      onDemoTab(FALSE)
    }
  }, ignoreInit = TRUE)
  
  
  
  
  # FOR TESTING: VIEW PEDIGREE EVERY TIME IT CHANGES
  observeEvent(PED(), { View(PED()) })
  
  
  #### Cancer History ####
  # save the number of cancers for each person in the pedigree
  canReactive <- reactiveValues(canNums = trackCans.init)
  
  # add a cancer UI module on button click and advance the module counter
  observeEvent(input$addCan, {
    
    # look-up the maximum number of created cancer UI modules for the current 
    # relative add the order of active cancer UI modules
    trackInputs <- canReactive$canNums[[input$relSelect]]$dict
    trackMax <- canReactive$canNums[[input$relSelect]]$mx
    
    # increase this person's count of cancer input modules by 1
    if(length(trackInputs) == 1 & is.na(trackInputs[1])){
      trackInputs[1] <- trackMax + 1
    } else {
      next.slot <- max(as.numeric(names(trackInputs))) + 1
      trackInputs <- c(trackInputs, setNames(trackMax + 1, next.slot))
    }
    
    # increase total number of created cancer UI modules for this person
    trackMax <- trackMax + 1
    
    # update canNums for this person
    canReactive$canNums[[input$relSelect]] <- list(dict = trackInputs,
                                                   mx = trackMax)
    
    # create the unique module ID and insert the UI module
    id <- paste0("rel", input$relSelect, "canModule", trackMax)
    insertUI(
      selector = "#canContainer",
      where = "beforeEnd",
      ui = canUI(id = id, rel = reactive(input$relSelect))
    )
    
    # add a server for checking the validity of the entered cancer age
    validateCanAgeServer(id,
                         in.age = reactive(input[[paste0(id, "-CanAge")]]),
                         cur.age = reactive(input$Age))
    
    # add a server for checking the validity of the entered CBC age
    validateCBCAgeServer(id,
                         can = reactive(input[[paste0(id, "-Can")]]),
                         cbc.age = reactive(input[[paste0(id, "-CBCAge")]]),
                         bc.age = reactive(input[[paste0(id, "-CanAge")]]),
                         cur.age = reactive(input$Age))
    
    ### Cancer UI Remove Observer
    # create a remove module button observer for each UI module created
    observeEvent(input[[paste0(id, '-removeCan')]], {
      
      # get current version of active cancer modules
      tmp.trackInputs <- canReactive$canNums[[input$relSelect]]$dict
      
      ## re-add deleted cancer choice to dropdown choices of this person's other cancer modules
      # get all of the cancers currently selected
      cans.selected <- as.character()
      if(!(length(tmp.trackInputs) == 1 & is.na(tmp.trackInputs[1]))){
        for(cn in as.numeric(names(tmp.trackInputs))){
          tmp.id <- paste0("rel", input$relSelect, "canModule", tmp.trackInputs[cn], "-Can")
          if(!input[[tmp.id]] %in% c("No cancer selected", "Other") &
             input[[tmp.id]] != input[[paste0(id, "-Can")]]){
            cans.selected <- c(cans.selected, input[[tmp.id]])
          }
        }
        
        
        # update all cancer choices
        for(cn in as.numeric(names(tmp.trackInputs))){
          tmp.id <- paste0("rel", input$relSelect, "canModule", tmp.trackInputs[cn], "-Can")
          
          # get cancer dropdown choices available for this cancer name input
          mod.cans.selected <- cans.selected[which(cans.selected != input[[tmp.id]])]
          cans.avail <- CANCER.CHOICES$long[which(!CANCER.CHOICES$long %in% mod.cans.selected)]
          
          # update the input dropdown
          updateSelectInput(session, tmp.id, 
                            choices = cans.avail, 
                            selected = input[[tmp.id]])
        }
      }
      
      ## re-add deleted OTHER cancer choice to dropdown choices of this person's OTHER cancer modules
      # get all of the cancers currently selected across this person's cancer UI modules
      cans.selected <- as.character()
      if(!(length(tmp.trackInputs) == 1 & is.na(tmp.trackInputs[1]))){
        for(cn in as.numeric(names(tmp.trackInputs))){
          tmp.id <- paste0("rel", input$relSelect, "canModule", tmp.trackInputs[cn], "-CanOther")
          if(input[[tmp.id]] != "Unknown/Not Listed" &
             input[[tmp.id]] != input[[paste0(id, "-CanOther")]]){
            cans.selected <- c(cans.selected, input[[tmp.id]])
          }
        }
        
        # update each of this person's cancer UI module OTHER cancer choice dropdowns to exclude the newly selected OTHER cancer
        for(cn in as.numeric(names(tmp.trackInputs))){
          tmp.id <- paste0("rel", input$relSelect, "canModule", tmp.trackInputs[cn], "-CanOther")
          
          # get OTHER cancer dropdown choices available for this OTHER cancer name and input
          mod.cans.selected <- cans.selected[which(cans.selected != input[[tmp.id]])]
          cans.avail <- OTHER.CANCER.CHOICES[which(!OTHER.CANCER.CHOICES %in% mod.cans.selected)]
          
          # update the input dropdown
          updateSelectInput(session, tmp.id, 
                            choices = cans.avail, 
                            selected = input[[tmp.id]])
        }
      }
      
      ## delete the module and UI
      # remove the module from the UI
      removeUI(selector = paste0("#canSubContainer",id))
      
      # remove the module's inputs from memory
      remove_shiny_inputs(id, input)
      
      ## remove module's index from the vector of active modules
      ## decrease the active cancer module count by one
      ## shift remaining active cancer modules to different slots
      if(length(tmp.trackInputs) == 1 & !is.na(tmp.trackInputs[1])){
        tmp.trackInputs[1] <- NA
      } else if(!(length(tmp.trackInputs) == 1 & is.na(tmp.trackInputs[1]))){
        
        # if the input to be removed is not the last one, iterate through the active inputs to update them
        if(which(tmp.trackInputs == trackMax) != length(tmp.trackInputs)){
          for(el in which(tmp.trackInputs == trackMax):(length(tmp.trackInputs) - 1)){
            tmp.trackInputs[el] <- tmp.trackInputs[el+1]
          }
        }
        tmp.trackInputs <- tmp.trackInputs[1:(length(tmp.trackInputs) - 1)]
      } 
      canReactive$canNums[[input$relSelect]]$dict <- tmp.trackInputs
      
    })
    
    ## create a cancer selection observer which will trigger an update of all of the cancer dropdown
    ## choices for each of the person's cancer UI modules
    observeEvent(input[[paste0(id, '-Can')]], {
      
      # get current version of active cancer modules
      tmp.trackInputs <- canReactive$canNums[[input$relSelect]]$dict
      
      # get all of the cancers currently selected across this person's cancer UI modules
      cans.selected <- as.character()
      for(cn in as.numeric(names(tmp.trackInputs))){
        tmp.id <- paste0("rel", input$relSelect, "canModule", tmp.trackInputs[cn], '-Can')
        if(!input[[tmp.id]] %in% c("No cancer selected", "Other")){
          cans.selected <- c(cans.selected, input[[tmp.id]])
        }
      }
      
      # update each of this person's cancer UI module cancer choice dropdowns to exclude the newly selected cancer
      for(cn in as.numeric(names(tmp.trackInputs))){
        tmp.id <- paste0("rel", input$relSelect, "canModule", tmp.trackInputs[cn], '-Can')
      
        # get cancer dropdown choices available for this cancer name input
        mod.cans.selected <- cans.selected[which(cans.selected != input[[tmp.id]])]
        cans.avail <- CANCER.CHOICES$long[which(!CANCER.CHOICES$long %in% mod.cans.selected)]

        # update the input dropdown
        updateSelectInput(session, tmp.id,
                          choices = cans.avail,
                          selected = input[[tmp.id]])
      }
    })
    
    ## create an OTHER cancer selection observer which will trigger an update of all of the OTHER cancer dropdown
    ## choices for each of the person's cancer UI modules
    observeEvent(input[[paste0(id, '-CanOther')]], {
      
      # get current version of active cancer modules
      tmp.trackInputs <- canReactive$canNums[[input$relSelect]]$dict
      
      # get all of the OTHER cancers currently selected across this person's cancer UI modules
      cans.selected <- as.character()
      for(cn in as.numeric(names(tmp.trackInputs))){
        tmp.id <- paste0("rel", input$relSelect, "canModule", tmp.trackInputs[cn], '-CanOther')
        if(input[[tmp.id]] != "Unknown/Not Listed"){
          cans.selected <- c(cans.selected, input[[tmp.id]])
        }
      }
      
      # update each of this person's cancer UI module OTHER cancer choice dropdowns to exclude the newly selected OTHER cancer
      for(cn in as.numeric(names(tmp.trackInputs))){
        tmp.id <- paste0("rel", input$relSelect, "canModule", tmp.trackInputs[cn], '-CanOther')
        mod.cans.selected <- cans.selected[which(cans.selected != input[[tmp.id]])]
        cans.avail <- OTHER.CANCER.CHOICES[which(!OTHER.CANCER.CHOICES %in% mod.cans.selected)]
        updateSelectInput(session, tmp.id,
                          choices = cans.avail,
                          selected = input[[tmp.id]])
      }
    })
    
  })
  
  # add data to pedigree when user navigates off of the tab
  onCanTab <- reactiveVal(FALSE)
  observeEvent(list(input$pedTabs), {
    
    can.df <- makeCancerDF(rel = input$relSelect, cr = canReactive$canNums, inp = input)

    # # consolidate all cancer inputs into a single data frame by looping through each exiting module
    # can.df <- cancer.inputs.store
    # trackInputs <- canReactive$canNums[[input$relSelect]]$dict
    # if(!(length(trackInputs) == 1 & is.na(trackInputs[1]))){
    #   for(cn in as.numeric(names(trackInputs))){
    #     id <- paste0("rel", input$relSelect, "canModule", trackInputs[cn])
    #     if(input[[paste0(id, '-Can')]] != "No cancer selected"){
    #       can.df[cn, ] <- c(input[[paste0(id, '-Can')]],
    #                         input[[paste0(id, '-CanAge')]],
    #                         input[[paste0(id, '-CanOther')]])
    #     }
    #     
    #     # check for CBC
    #     hadCBC <- FALSE
    #     CBCAge <- NA
    #     if(input[[paste0(id, '-Can')]] == "Breast" &
    #        input[[paste0(id, "-CBC")]] == "Yes"){
    #       hadCBC <- TRUE
    #       CBCAge <- input[[paste0(id, "-CBCAge")]]
    #     }
    #   }
    #   
    #   # add CBC as last row of the data frame
    #   if(hadCBC){
    #     can.df[nrow(can.df)+1, ] <- c("Contralateral",
    #                                   CBCAge,
    #                                   "")
    #   }
    #   
    #   # no cancer entered for this person, create 1 row placeholder data frame
    # } else {
    #   can.df[1, ] <- c("No cancer selected", NA, "")
    # }
    
    # transfer information to the pedigree
    if(onCanTab() & input$pedTabs != "Cancer Hx"){
      PED(popPersonData(tmp.ped = PED(), id = input$relSelect, cancers.and.ages = can.df))
    }
    
    # update the reactive value to detect if the current tab is the target tab
    if(input$pedTabs == "Cancer Hx"){
      onCanTab(TRUE)
    } else {
      onCanTab(FALSE)
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
  observeEvent(PED(), {
    
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
  })
  
  # add data to pedigree when user navigates off of the tab
  onMarkerTab <- reactiveVal(FALSE)
  observeEvent(list(input$pedTabs), {
    if(onMarkerTab() & input$pedTabs != "Tumor Markers"){
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
  }, ignoreInit = TRUE)
  
  
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
    validate(validateSurgAge(input$OophAge, input$Age, PED()$AgeOC[which(PED()$ID == input$relSelect)]))
  })
  output$validOophAge <- renderText({ validOophAge() })
  # Mastectomy age
  validMastAge <- reactive({
    validate(validateSurgAge(input$MastAge, input$Age, PED()$AgeCBC[which(PED()$ID == input$relSelect)]))
  })
  output$validMastAge <- renderText({ validMastAge() })
  # Hysterectomy age
  validHystAge <- reactive({
    validate(validateSurgAge(input$HystAge, input$Age, PED()$AgeENDO[which(PED()$ID == input$relSelect)]))
  })
  output$validHystAge <- renderText({ validHystAge() })
  
  # add data to pedigree when user navigates off of the tab
  onSurgTab <- reactiveVal(FALSE)
  observeEvent(input$pedTabs, {
    if(onSurgTab() & input$pedTabs != "Surgical Hx"){
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
  
  # save the number of genes for each person, by result type
  # also store the list of genes in the currently selected panel
  geneReactive <- reactiveValues(GeneNums = trackGenes.init)
  
  ##### Panels ####
  
  ### check if the current relative has a least one panel
  atLeastOnePanel <- reactiveVal(FALSE)
  observeEvent(list(geneReactive$GeneNums, input$relSelect), {
    if(geneReactive$GeneNums[[input$relSelect]]$panels$panel1$name == "No panel selected"){
      atLeastOnePanel(FALSE)
    } else {
      atLeastOnePanel(TRUE)
    }
  })
  output$atLeastOnePanel <- reactive({ atLeastOnePanel() })
  outputOptions(output, 'atLeastOnePanel', suspendWhenHidden = FALSE)
  
  ### add an existing panel
  observeEvent(input$addPanel, {
    
    # check that the selected panel is an actual panel
    if(!input$existingPanels %in% c("No panel selected", "Create new")){
      
      ## store initial inputs as variables to be called later
      rel <- input$relSelect
      gr <- geneReactive$GeneNums[[rel]]
      pan.name <- input$existingPanels
      panel.module.id.num <- gr$mx + 1
      if(gr$panels$panel1$name == "No panel selected"){
        next.dict.slot <- 1
      } else {
        next.dict.slot <- max(as.numeric(names(gr$dict))) + 1
      }
      new.pan <- paste0("panel", next.dict.slot)
      
      ## update gene/panel input tracking information
      # update panel aggregate tracking information
      gr$mx <- panel.module.id.num
      if(gr$panels$panel1$name == "No panel selected"){
        gr$dict <- setNames(c(panel.module.id.num), next.dict.slot)
      } else {
        gr$dict <- c(gr$dict, setNames(c(panel.module.id.num), next.dict.slot))
      }
      
      # update panel specific information
      gr$panels[[new.pan]]$name <- pan.name
      gr$panels[[new.pan]]$genes <- all.panels[[pan.name]]
      gr$panels[[new.pan]]$results <- geneResultsTemplate
      
      # update the reactive
      geneReactive$GeneNums[[input$relSelect]] <- gr
      
      ## insert UI
      # create the unique panelUI module ID with its own container
      panMod.id <- paste0("rel", rel, "PanelModule", panel.module.id.num)
      insertUI(
        selector = "#PanCont",
        where = "beforeEnd",
        ui = panelUI(id = panMod.id, 
                     rel = rel,
                     panelName = pan.name)
      )
      
      ## update related dropdown choices
      # remove added panel from dropdown choices for adding a new panel
      cur.panels <- as.character()
      for(pn in names(gr$dict)){
        cur.panels <- c(cur.panels, gr$panels[[paste0("panel", pn)]]$name)
      }
      updateSelectInput(session, "existingPanels",
                        choices = all.panel.names[which(!all.panel.names %in% cur.panels)],
                        selected = "No panel selected")
      
      # add panel name to dropdown choices for editing an active panel
      updateSelectInput(session, "editPanel",
                        choices = c("No panel selected", cur.panels),
                        selected = "No panel selected")
      
      ## panelUI Remove Observer
      # create a remove module button observer for each panelUI module created
      observeEvent(input[[paste0(panMod.id, '-removePanel')]], {
        
        ## remove the panel's input information from reactive used to track gene/panel inputs
        # get an updated copy of the reactive because gr likely changed between creation and removal of the UI
        rm.gr <- geneReactive$GeneNums[[rel]]
        
        # find the panelUI's order number in the dictionary
        for(rm.pnum in as.numeric(names(rm.gr$dict))){
          rm.pnum.name <- paste0("panel", rm.pnum)
          if(rm.gr$panels[[rm.pnum.name]]$name == pan.name){
            break
          }
        }
        
        ## remove all related geneUI's for each result type
        for(rtype in c("PLP", "VUS", "BLB")){
          rm.gr.rtype <- rm.gr$panels[[rm.pnum.name]]$results[[rtype]]
          for(gnum in as.numeric(names(rm.gr.rtype$dict))){
            gene.module.id.num <- rm.gr.rtype$dict[gnum]
            
            # remove the module from the UI
            geneMod.id <- paste0("rel", rel, "Pan", panel.module.id.num, rtype, "GeneModule", gene.module.id.num)
            removeUI(selector = paste0("#", geneMod.id, "Cont"))
            
            # remove the module's inputs from memory
            remove_shiny_inputs(geneMod.id, input)
          }
        }
        
        ## delete the panelUI module
        # remove the module from the UI
        removeUI(selector = paste0("#", panMod.id, "Cont"))
        
        # remove the module's inputs from memory
        remove_shiny_inputs(panMod.id, input)
        
        ## update the panel/gene tracking information
        # clear panel1 information if there was only 1 panel
        if(length(rm.gr$dict) == 1){
          rm.gr$dict[1] <- NA
          rm.gr$panels$panel1$name <- "No panel selected"
          rm.gr$panels$panel1$genes <- as.character()
          rm.gr$panels$panel1$results <- geneResultsTemplate
          
          # if there was more than 1 panel
        } else {
          
          # delete the panel's information
          rm.gr$panels[[rm.pnum.name]] <- NULL
          
          ## modify the relative's panel dictionary and panel names
          # case where the panel to be deleted is not the last panel added
          if(rm.pnum != names(rm.gr$dict)[length(rm.gr$dict)]){
            
            # re-shift all panel1, panel2, ... names
            names(rm.gr$panels)[rm.pnum:length(rm.gr$panels)] <- 
              paste0("panel", rm.pnum:length(rm.gr$panels))
            
            # iterate through the active panelUI modules to update the panel dictionary
            for(el in rm.pnum:(length(rm.gr$dict) - 1)){
              rm.gr$dict[el] <- rm.gr$dict[el+1]
            }
          }
          
          # remove the last entry from the panel dictionary
          rm.gr$dict <- rm.gr$dict[1:(length(rm.gr$dict) - 1)]
        }
        
        # update reactive
        geneReactive$GeneNums[[rel]] <- rm.gr
        
        ## update related dropdown choices
        # add removed panel back to dropdown choices for adding a new panel
        rm.cur.panels <- as.character()
        if(!is.na(rm.gr$dict[1])){
          for(pnum in names(rm.gr$dict)){
            rm.cur.panels <- c(rm.cur.panels, rm.gr$panels[[paste0("panel", pnum)]]$name)
          }
        }
        updateSelectInput(session, "existingPanels",
                          choices = all.panel.names[which(!all.panel.names %in% rm.cur.panels)],
                          selected = "No panel selected")
        
        # remove panel name from dropdown choices for editing an active panel
        updateSelectInput(session, "editPanel",
                          choices = c("No panel selected", rm.cur.panels),
                          selected = "No panel selected")
        
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
      
      # assign variables to be called later
      rel <- input$relSelect
      gr <- geneReactive$GeneNums[[rel]]
      for(pnum in as.numeric(names(gr$dict))){
        pnum.name <- paste0("panel", pnum)
        if(gr$panels[[pnum.name]]$name == input$editPanel){
          panel.module.id.num <- gr$dict[pnum]
          p.name <- gr$panels[[pnum.name]]$name
          p.genes <- gr$panels[[pnum.name]]$genes
          grplp <- gr$panels[[pnum.name]]$results$PLP
          gene.module.id.num <- grplp$mx + 1
          if(is.na(grplp$dict[1])){
            next.dict.slot <- 1
          } else {
            next.dict.slot <- max(as.numeric(names(grplp$dict))) + 1
          }
          break
        }
      }

      # update PLP geneUI module tracking information for this relative and panel
      grplp$mx <- gene.module.id.num
      if(is.na(grplp$dict[1])){
        grplp$dict <- setNames(c(gene.module.id.num), next.dict.slot)
      } else {
        grplp$dict <- c(grplp$dict, setNames(c(gene.module.id.num), next.dict.slot))
      }
      geneReactive$GeneNums[[rel]]$panels[[pnum.name]]$results$PLP <- grplp

      # create the unique geneUI module ID and insert the module into its own container
      geneMod.id <- paste0("rel", rel, "Pan", panel.module.id.num, "PLPGeneModule", gene.module.id.num)
      insertUI(
        selector = "#PLPCont",
        where = "beforeEnd",
        ui = geneUI(id = geneMod.id,
                    rel = rel,
                    panelName = p.name,
                    panelGenes = p.genes
             )
      )

      ### Gene UI Remove Observer
      # create a remove module button observer for each UI module created
      observeEvent(input[[paste0(geneMod.id, '-removeGene')]], {
        
        ## delete the geneUI module
        # remove the module from the UI
        removeUI(selector = paste0("#", geneMod.id, "Cont"))
        
        # remove the module's inputs from memory
        remove_shiny_inputs(geneMod.id, input)
        
        ## remove the gene's input information from reactive used to track gene/panel inputs
        # get an updated copy of the reactive because gr likely changed between creation and removal of the UI
        for(rm.pnum in as.numeric(names(geneReactive$GeneNums[[rel]]$dict))){
          rm.pnum.name <- paste0("panel", rm.pnum)
          if(geneReactive$GeneNums[[rel]]$panels[[rm.pnum.name]]$name == input$editPanel){
            break
          }
        }
        rm.grplp <- geneReactive$GeneNums[[rel]]$panels[[rm.pnum.name]]$results$PLP
        del.dict.slot <- which(rm.grplp$dict == gene.module.id.num)
        
        # clear gene module information
        if(length(rm.grplp$dict) == 1){
          rm.grplp$dict[1] <- NA
        } else {
          
          # case where the gene module to be deleted is not the last gene module added
          if(del.dict.slot != names(rm.grplp$dict)[length(rm.grplp$dict)]){
            
            # iterate through the active geneUI modules to update the dictionary
            for(el in del.dict.slot:(length(rm.grplp$dict) - 1)){
              rm.grplp$dict[el] <- rm.grplp$dict[el+1]
            }
          }
          
          # remove the last entry from the dictionary
          rm.grplp$dict <- rm.grplp$dict[1:(length(rm.grplp$dict) - 1)]
        }
        
        # update reactive
        geneReactive$GeneNums[[rel]]$panels[[rm.pnum.name]]$results$PLP <- rm.grplp
      })
    }
  }, ignoreInit = TRUE)
  
  
  ##### VUS Genes ####
  # add a VUS geneUI module on button click and advance the module counter
  # note, a conditionalPanel ensures the button is only displayed if the relative
  # has a least on panel
  observeEvent(input$addVUS, {
    if(input$editPanel != "No panel selected"){

      # assign variables to be called later
      rel <- input$relSelect
      gr <- geneReactive$GeneNums[[rel]]
      for(pnum in as.numeric(names(gr$dict))){
        pnum.name <- paste0("panel", pnum)
        if(gr$panels[[pnum.name]]$name == input$editPanel){
          panel.module.id.num <- gr$dict[pnum]
          p.name <- gr$panels[[pnum.name]]$name
          p.genes <- gr$panels[[pnum.name]]$genes
          grvus <- gr$panels[[pnum.name]]$results$VUS
          gene.module.id.num <- grvus$mx + 1
          if(is.na(grvus$dict[1])){
            next.dict.slot <- 1
          } else {
            next.dict.slot <- max(as.numeric(names(grvus$dict))) + 1
          }
          break
        }
      }

      # update VUS geneUI module tracking information for this relative and panel
      grvus$mx <- gene.module.id.num
      if(is.na(grvus$dict[1])){
        grvus$dict <- setNames(c(gene.module.id.num), next.dict.slot)
      } else {
        grvus$dict <- c(grvus$dict, setNames(c(gene.module.id.num), next.dict.slot))
      }
      geneReactive$GeneNums[[rel]]$panels[[pnum.name]]$results$VUS <- grvus

      # create the unique geneUI module ID and insert the module into its own container
      geneMod.id <- paste0("rel", rel, "Pan", panel.module.id.num, "VUSGeneModule", gene.module.id.num)
      insertUI(
        selector = "#VUSCont",
        where = "beforeEnd",
        ui = geneUI(id = geneMod.id,
                    rel = rel,
                    panelName = p.name,
                    panelGenes = p.genes
        )
      )

      ### Gene UI Remove Observer
      # create a remove module button observer for each UI module created
      observeEvent(input[[paste0(geneMod.id, '-removeGene')]], {

        ## delete the geneUI module
        # remove the module from the UI
        removeUI(selector = paste0("#", geneMod.id, "Cont"))

        # remove the module's inputs from memory
        remove_shiny_inputs(geneMod.id, input)

        ## remove the gene's input information from reactive used to track gene/panel inputs
        # get an updated copy of the reactive because gr likely changed between creation and removal of the UI
        for(rm.pnum in as.numeric(names(geneReactive$GeneNums[[rel]]$dict))){
          rm.pnum.name <- paste0("panel", rm.pnum)
          if(geneReactive$GeneNums[[rel]]$panels[[rm.pnum.name]]$name == input$editPanel){
            break
          }
        }
        rm.grvus <- geneReactive$GeneNums[[rel]]$panels[[rm.pnum.name]]$results$VUS
        del.dict.slot <- which(rm.grvus$dict == gene.module.id.num)

        # clear gene module information
        if(length(rm.grvus$dict) == 1){
          rm.grvus$dict[1] <- NA
        } else {

          # case where the gene module to be deleted is not the last gene module added
          if(del.dict.slot != names(rm.grvus$dict)[length(rm.grvus$dict)]){

            # iterate through the active geneUI modules to update the dictionary
            for(el in del.dict.slot:(length(rm.grvus$dict) - 1)){
              rm.grvus$dict[el] <- rm.grvus$dict[el+1]
            }
          }

          # remove the last entry from the dictionary
          rm.grvus$dict <- rm.grvus$dict[1:(length(rm.grvus$dict) - 1)]
        }

        # update reactive
        geneReactive$GeneNums[[rel]]$panels[[rm.pnum.name]]$results$VUS <- rm.grvus
      })
    }
  }, ignoreInit = TRUE)


  ##### BLB Genes ####
  # add a BLB geneUI module on button click and advance the module counter
  # note, a conditionalPanel ensures the button is only displayed if the relative
  # has a least on panel
  observeEvent(input$addBLB, {
    if(input$editPanel != "No panel selected"){

      # assign variables to be called later
      rel <- input$relSelect
      gr <- geneReactive$GeneNums[[rel]]
      for(pnum in as.numeric(names(gr$dict))){
        pnum.name <- paste0("panel", pnum)
        if(gr$panels[[pnum.name]]$name == input$editPanel){
          panel.module.id.num <- gr$dict[pnum]
          p.name <- gr$panels[[pnum.name]]$name
          p.genes <- gr$panels[[pnum.name]]$genes
          grblb <- gr$panels[[pnum.name]]$results$BLB
          gene.module.id.num <- grblb$mx + 1
          if(is.na(grblb$dict[1])){
            next.dict.slot <- 1
          } else {
            next.dict.slot <- max(as.numeric(names(grblb$dict))) + 1
          }
          break
        }
      }

      # update BLB geneUI module tracking information for this relative and panel
      grblb$mx <- gene.module.id.num
      if(is.na(grblb$dict[1])){
        grblb$dict <- setNames(c(gene.module.id.num), next.dict.slot)
      } else {
        grblb$dict <- c(grblb$dict, setNames(c(gene.module.id.num), next.dict.slot))
      }
      geneReactive$GeneNums[[rel]]$panels[[pnum.name]]$results$BLB <- grblb

      # create the unique geneUI module ID and insert the module into its own container
      geneMod.id <- paste0("rel", rel, "Pan", panel.module.id.num, "BLBGeneModule", gene.module.id.num)
      insertUI(
        selector = "#BLBCont",
        where = "beforeEnd",
        ui = geneUI(id = geneMod.id,
                    rel = rel,
                    panelName = p.name,
                    panelGenes = p.genes
        )
      )

      ### Gene UI Remove Observer
      # create a remove module button observer for each UI module created
      observeEvent(input[[paste0(geneMod.id, '-removeGene')]], {

        ## delete the geneUI module
        # remove the module from the UI
        removeUI(selector = paste0("#", geneMod.id, "Cont"))

        # remove the module's inputs from memory
        remove_shiny_inputs(geneMod.id, input)

        ## remove the gene's input information from reactive used to track gene/panel inputs
        # get an updated copy of the reactive because gr likely changed between creation and removal of the UI
        for(rm.pnum in as.numeric(names(geneReactive$GeneNums[[rel]]$dict))){
          rm.pnum.name <- paste0("panel", rm.pnum)
          if(geneReactive$GeneNums[[rel]]$panels[[rm.pnum.name]]$name == input$editPanel){
            break
          }
        }
        rm.grblb <- geneReactive$GeneNums[[rel]]$panels[[rm.pnum.name]]$results$BLB
        del.dict.slot <- which(rm.grblb$dict == gene.module.id.num)

        # clear gene module information
        if(length(rm.grblb$dict) == 1){
          rm.grblb$dict[1] <- NA
        } else {

          # case where the gene module to be deleted is not the last gene module added
          if(del.dict.slot != names(rm.grblb$dict)[length(rm.grblb$dict)]){

            # iterate through the active geneUI modules to update the dictionary
            for(el in del.dict.slot:(length(rm.grblb$dict) - 1)){
              rm.grblb$dict[el] <- rm.grblb$dict[el+1]
            }
          }

          # remove the last entry from the dictionary
          rm.grblb$dict <- rm.grblb$dict[1:(length(rm.grblb$dict) - 1)]
        }

        # update reactive
        geneReactive$GeneNums[[rel]]$panels[[rm.pnum.name]]$results$BLB <- rm.grblb
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
  output$panelSum <- renderDataTable({
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
    if(onGeneTab() & input$pedTabs != "Genes"){
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
    
    # add children
    if(input$numDau > 0 | input$numSon > 0){
      
      # first, add partner to pedigree
      PED(formatNewPerson(relation = "partner", tmp.ped = PED()))
      parnter.id <- PED()$ID[nrow(PED())]
      
      # assume, initially, that partner's race/eth/ancestry match the proband's
      PED(assumeBackground(PED()))
      
      # add daughters iteratively
      if(input$numDau > 0){
        for(i in 1:input$numDau){
          if(PED()$Sex[which(PED()$isProband == 1)] == 0){
            PED(formatNewPerson(relation = "daughter", tmp.ped = PED(), f.id = parnter.id))
          } else if(PED()$Sex[which(PED()$isProband == 1)] == 1){
            PED(formatNewPerson(relation = "daughter", tmp.ped = PED(), m.id = parnter.id))
          }
          
          # assume, initially, daughter's race/eth/ancestry match the proband's
          PED(assumeBackground(PED()))
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
          
          # assume, initially, son's race/eth/ancestry match the proband's
          PED(assumeBackground(PED()))
        }
      }
    }
    
    # add sisters iteratively
    if(input$numSis > 0){
      for(i in 1:input$numDau){
        PED(formatNewPerson(relation = "sister", tmp.ped = PED()))
        
        # assume, initially, sister's race/eth/ancestry match the proband's
        PED(assumeBackground(PED()))
      }
    }
    # add brothers iteratively
    if(input$numBro > 0){
      for(i in 1:input$numBro){
        PED(formatNewPerson(relation = "brother", tmp.ped = PED()))
        
        # assume, initially, brother's race/eth/ancestry match the proband's
        PED(assumeBackground(PED()))
      }
    }
    
    # add maternal aunts and uncles
    if(input$numMAunt > 0 | input$numMUnc > 0){
      
      # first, create maternal grandparents, add a cancer counter for each
      # assume, initially, race/eth/ancestry match the proband's mother
      PED(formatNewPerson(relation = "grandmother", tmp.ped = PED(), m.or.p.side = "m"))
      PED(assumeBackground(PED()))
      
      PED(formatNewPerson(relation = "grandfather", tmp.ped = PED(), m.or.p.side = "m"))
      PED(assumeBackground(PED()))
      
      # add maternal aunts iteratively
      if(input$numMAunt > 0){
        for(i in 1:input$numMAunt){
          PED(formatNewPerson(relation = "aunt", tmp.ped = PED(), m.or.p.side = "m"))
          
          # assume, initially, aunt's race/eth/ancestry match the proband's mother
          PED(assumeBackground(PED()))
        }
      }
      # add maternal uncles iteratively
      if(input$numMUnc > 0){
        for(i in 1:input$numMUnc){
          PED(formatNewPerson(relation = "uncle", tmp.ped = PED(), m.or.p.side = "m"))
          
          # assume, initially, uncle's race/eth/ancestry match the proband's mother
          PED(assumeBackground(PED()))
        }
      }
    }
    
    # add paternal aunts and uncles
    if(input$numPAunt > 0 | input$numPUnc > 0){
      
      # first, create paternal grandparents, add a cancer counter,
      # assume, initially, race/eth/ancestry match the proband's father
      PED(formatNewPerson(relation = "grandmother", tmp.ped = PED(), m.or.p.side = "p"))
      PED(assumeBackground(PED()))
      
      PED(formatNewPerson(relation = "grandfather", tmp.ped = PED(), m.or.p.side = "p"))
      PED(assumeBackground(PED()))
      
      # add paternal aunts iteratively
      if(input$numPAunt > 0){
        for(i in 1:input$numPAunt){
          PED(formatNewPerson(relation = "aunt", tmp.ped = PED(), m.or.p.side = "p"))
          
          # assume, initially, aunt's race/eth/ancestry match the proband's father
          PED(assumeBackground(PED()))
        }
      }
      # add paternal uncles iteratively
      if(input$numPUnc > 0){
        for(i in 1:input$numPUnc){
          PED(formatNewPerson(relation = "uncle", tmp.ped = PED(), m.or.p.side = "p"))
          
          # assume, initially, uncle's race/eth/ancestry match the proband's father
          PED(assumeBackground(PED()))
        }
      }
    }
    
    # initialize cancer counter for each new person added to the pedigree
    for(np in PED()$ID[3:nrow(PED())]){
      canReactive$canNums[[as.character(np)]] <- list(dict = setNames(c(NA),1), mx = 0)
    }
    
    # update relative selector with all relatives in the pedigree
    updateSelectInput(session = session, inputId = "relSelect", 
                      choices = PED()$ID, selected = PED()$ID[which(PED()$isProband == 1)])
    
    # hide initialize pedigree tab
    hideTab("pedTabs", target = "Initialize Pedigree", session = session)
    
    # disable the pedigree name field
    shinyjs::disable("pedID")
  }, ignoreInit = TRUE)
  
  #### Visualize Pedigree ####
  # temporarily: draw pedigree in kinship2
  # replace with pedigreejs
  output$drawPed <- renderPlot({
    plot_fam <-
      PED() %>%
      mutate(Sex = ifelse(Sex == 0, 2, Sex)) %>%
      mutate(across(.cols = c(MotherID, FatherID), ~ ifelse(is.na(.), 0, .))) %>%
      select(PedigreeID, ID, MotherID, FatherID, Sex)
    dped <- pedigree(id = plot_fam$ID,
                     momid = plot_fam$MotherID,
                     dadid = plot_fam$FatherID,
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
    
    # only execute when pedigree has been visualized
    if(input$visPed){
      
      ##### Save Data for Previous Relative ####
      ## save data for the previously selected relative to pedigree
      # demographics
      if(input$pedTabs == "Demographics"){
        PED(popPersonData(tmp.ped = PED(), id = lastRel(), cur.age = input$Age, 
                          rc = input$race, et = input$eth, 
                          an.aj = input$ancAJ, an.it = input$ancIt)
            )
        
        # cancer hx
      } else if(input$pedTabs == "Cancer Hx"){
        can.df <- makeCancerDF(rel = lastRel(), cr = canReactive$canNums, inp = input)
        PED(popPersonData(tmp.ped = PED(), id = lastRel(), cancers.and.ages = can.df))
        
        # tumor markers
      } else if(input$pedTabs == "Tumor Markers"){
        PED(popPersonData(tmp.ped = PED(), id = lastRel(), 
                          er = input$ER, pr = input$PR, her2 = input$HER2,
                          ck5.6 = input$CK56, ck14 = input$CK14, msi = input$MSI))
        
        # surgical hx
      } else if(input$pedTabs == "Surgical Hx"){
        PED(popPersonData(tmp.ped = PED(), id = lastRel(), riskmods.and.ages = surgReactive$lst))
        
        # genes
      } else if(input$pedTabs == "Genes"){
        gene.df <- makeGeneDF(rel = lastRel(), gr = geneReactive$GeneNums, 
                              dupResultGene = dupResultGene(),
                              inp = input)$df
        PED(popPersonData(tmp.ped = PED(), id = lastRel(),
                          gene.results = gene.df))
      }
      
      # update the last relative selected
      lastRel(as.numeric(input$relSelect))
      
      #### Re-populate data for new person ####
      rel.info <- PED()[which(PED()$ID == as.numeric(input$relSelect)),]
      
      ## Demographics 
      # sex
      new.sex <- ifelse(rel.info$Sex == 0, "Female",
                        ifelse(rel.info$Sex == 1, "Male", NA))
      updateSelectInput(session, "Sex", selected = new.sex, choices = sex.choices)
      
      # age
      updateNumericInput(session, "Age", value = rel.info$CurAge)
      
      # Non-PanelPRO races, ethnicity, and ancestries
      updateSelectInput(session, "race", selected = rel.info$NPP.race)
      updateSelectInput(session, "eth", selected = rel.info$NPP.eth)
      updateCheckboxInput(session, "ancAJ", value = rel.info$NPP.AJ)
      updateCheckboxInput(session, "ancIt", value = rel.info$NPP.It)
      
      ## Tumor Markers 
      marks <- c(PanelPRO:::MARKER_TESTING$BC$MARKERS, PanelPRO:::MARKER_TESTING$COL$MARKERS)
      for(m in marks){
        mval <- ifelse(is.na(rel.info[1,m]), "Not Tested",
                       ifelse(rel.info[1,m] == 1, "Positive",
                              ifelse(rel.info[1,m] == 0, "Negative", "Not Tested")))
        m <- ifelse(m == "CK5.6", "CK56", m)
        updateSelectInput(session, m, selected = mval)
      }
      
      ## Surgical Hx
      for(sg in c("Mast", "Ooph", "Hyst")){
        updateCheckboxInput(session, sg, value = rel.info[[paste0("riskmod", sg)]])
        updateNumericInput(session, paste0(sg,"Age"), value = rel.info[[paste0("interAge", sg)]])
      }
      
      ## Genes
      # update the panel data for the new person
      if(rel.info$panel.names != "none"){
        updateSelectInput(session, "existingPanels", 
                          choices = all.panel.names[which(!all.panel.names %in% 
                                                            strsplit(rel.info$panel.names, 
                                                                     split = ", ")[[1]])],
                          selected = "No panel selected")
      } else {
        updateSelectInput(session, "existingPanels", 
                          choices = all.panel.names,
                          selected = "No panel selected")
      }

      # reset the selected gene tabs
      updateTabsetPanel(session, "geneTabs", selected = "Instructions")
      updateTabsetPanel(session, "geneResultTabs", selected = "P/LP")
      
    } # end of if statement for input$visPed == TRUE
  }, ignoreInit = TRUE)
  
  #### PanelPRO ####

  
}

# Run the application 
shinyApp(ui = ui, server = server)
