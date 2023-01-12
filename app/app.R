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

# talbes
library(DT)

# load data


# utils and variables
source("./vars.R")
source("./utils.R")

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
              
              # create maternal and paternal race, ethnicity, and ancestry columns
              # for the proband (before pedigree is visualized)
              conditionalPanel("!input.visPed",
                fluidRow(
                  
                  # maternal race, ethnicity, ancestry column
                  column(width = 6, 
                    selectInput("RaceM", label = h5("Mother's Race:"),
                                choices = rc.choices,
                                selected = "Other or Unreported",
                                width = "95%"),
                    selectInput("EthM", label = h5("Mother's Hispanic Ethnicity:"),
                                choices = et.choices,
                                selected = "Other or Unreported",
                                width = "95%"),
                    h5("Mother's Ancestry (check all that apply):"),
                    div(style = "margin-left:25px",
                      checkboxInput("AncAJM", label = "Ashkenazi Jewish"),
                      checkboxInput("AncItM", label = "Italian")
                    )
                  ),
                  
                  # paternal race, ethnicity, ancestry column
                  column(width = 6, 
                    selectInput("RaceF", label = h5("Father's Race:"),
                                choices = rc.choices,
                                selected = "Other or Unreported",
                                width = "95%"),
                    selectInput("EthF", label = h5("Father's Hispanic Ethnicity:"),
                                choices = et.choices,
                                selected = "Other or Unreported",
                                width = "95%"),
                    h5("Father's Ancestry (check all that apply):"),
                    div(style = "margin-left:25px",
                      checkboxInput("AncAJF", label = "Ashkenazi Jewish"),
                      checkboxInput("AncItF", label = "Italian")
                    )
                  ),
                ) # end of fluidRow for race and ancestry
              ), # end of conditionalPanel for when pedigree is not visualized
              
              # create subject's individual race, ethnicity, and ancestry inputs
              # after pedigree is visualized this is for all relatives and the proband
              conditionalPanel("input.visPed",
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
            
            ###### Cancer Hx ####
            tabPanel("Cancer Hx",
              h3("Cancer History"),
              p("List all primary cancers the person has or had with the age of diagnosis. If a cancer reoccurred in the same 
                organ, list that cancer only once and only provide the first diagnosis age. However, if the patient was diagnosed with 
                contralateral breast cancer (CBC) then enter two cancers: one as 'Breast' with the first 
                diagnosis age and a second as 'Contralateral' with the CBC diagnosis age."),
              
              # issue warning if the same cancer is listed more than once
              conditionalPanel("output.dupCancers",
                h5("You have the same cancer listed more than once, please fix this.", style = "color:red")
              ),
              
              # issue warning if any cancer age is not valid
              textOutput("validCanAges"),
              tags$head(tags$style("#validCanAges{color: red;}")),
              
              # enter cancers
              uiOutput("CanInputs"),
              actionButton("addCan", label = "Add Cancer",
                           icon = icon('plus'),
                           style = "color: white; background-color: #10699B; border-color: #10699B; margin-top: 20px"),
              actionButton("removeCan", label = "Remove Last Cancer",
                           icon = icon('trash'),
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
            
            ###### Genes ####
            tabPanel("Genes",
              h3("Gene Testing Results"),
              p("First, specify the panel of genes tested and then enter the test results by type. All data is 
                entered on the first tab and a summary is displayed on the second tab.", 
                style = "margin-bottom:10px"),
              
              # create two tabs, one for entering data and one for displaying a data frame summary
              tabsetPanel(id = "geneTabs",
                
                tabPanel(title = "1. Select Panel & Enter Results",
                  fluidRow(column(width = 12, 
                       
                    # select existing panel
                    h4("Step 1a: Specify the Panel of Genes Tested"),
                    p("You can select an existing panel of genes from the drop down. If you need to create a 
                      custom panel select 'Create new'."),
                    selectInput("existingPanels", label = NULL,
                                choices = all.panel.names, selected = "No panel selected",
                                width = "300px"),
                    
                    # create new panel
                    conditionalPanel("input.existingPanels == 'Create new'",
                      p("Enter the genes in your panel below. 
                        When you start typing, the dropdown will filter to genes for you to select. 
                        You can also add genes that are not in the dropdown. When done, select the 
                        'Create Panel' button."),
                      textInput("newPanelName", label = h5("Name the new panel:"), width = "250px"),
                      selectizeInput("newPanelGenes", label = h5("Type or select the genes in this panel:"),
                                     choices = all.genes, multiple = TRUE,
                                     width = "500px"),
                      actionButton("createPanel", label = "Create Panel",
                                   style = "color: white; background-color: #10699B; border-color: #10699B; margin-top: 0px; margin-bottom:15px")
                    ),
                    
                    # enter results by type
                    conditionalPanel("input.existingPanels != 'No panel selected' & input.existingPanels != 'Create new'",
                      h4("Step 1b: Enter Results by Type"),
                      p("Enter the gene results by selecting the three different tabs for ",
                        HTML("<b>pathogenic/likely pathogenic (P/LP), unknown significance (VUS),</b> or <b>
                        benign/likely benign (B/LP)</b>.")," Any genes not specified as P/LP, VUS, or B/LB
                        will be recorded as negative.", 
                        style = "margin-bottom:25px"),
                      conditionalPanel("output.dupResultGene",
                        h5("Warning: you have the same gene listed in more than one result category, this possible but not common. 
                           Check for errors in the information you entered for gene results.", style = "color:red")
                      ),
                      conditionalPanel("output.dupPLP",
                        h5("You have the same gene listed more than once as P/LP, please fix this.", style = "color:red")
                      ),
                      conditionalPanel("output.dupVUS",
                        h5("You have the same gene listed more than once as VUS, please fix this.", style = "color:red")
                      ),
                      conditionalPanel("output.dupBLB",
                        h5("You have the same gene listed more than once as B/LB, please fix this.", style = "color:red")
                      ),
                      
                      # create a tab for each result type to save space
                      tabsetPanel(id = "GeneResultTabs",
                      
                        # P/LP
                        tabPanel("P/LP",
                          wellPanel(style = "background:MistyRose",
                            h4(HTML("<b>Pathogenic/Likely Pathogenic (P/LP) genes</b>"), style = "color:black"),
                            fluidRow(
                              column(width = 3,
                                     h5(HTML("<b>Gene</b>"), style = "margin-left:0px;margin-bottom:10px;margin-top:0px")
                              ),
                              column(width = 3, 
                                     h5(HTML("<b>Variants</b>"), style = "margin-left:-25px;margin-bottom:10px;margin-top:0px")
                              ),
                              column(width = 3,
                                     h5(HTML("<b>Proteins</b>"), style = "margin-left:-25px;margin-bottom:10px;margin-top:0px")
                              ),
                              column(width = 3,
                                     h5(HTML("<b>Zygosity</b>"), style = "margin-left:-25px;margin-bottom:10px;margin-top:0px")
                              ),
                            ),
                            uiOutput("plpGeneInfo"),
                            actionButton("addPLP", label = "Add P/LP Gene",
                                         icon = icon('plus'),
                                         style = "color: white; background-color: #10699B; border-color: #10699B; margin-top: 10px"),
                            actionButton("removePLP", label = "Remove Last P/LP Gene",
                                         icon = icon('trash'),
                                         style = "color: white; background-color: #10699B; border-color: #10699B; margin-top: 10px")
                          )
                        ),
                      
                        # VUS
                        tabPanel("VUS",
                          wellPanel(style = "background:LightGreen",
                            h4(HTML("<b>Variant of Unknown Significance (VUS) genes</b>"), style = "color:black"),
                            fluidRow(
                              column(width = 3,
                                     h5(HTML("<b>Gene</b>"), style = "margin-left:0px;margin-bottom:10px;margin-top:0px")
                              ),
                              column(width = 3, 
                                     h5(HTML("<b>Variants</b>"), style = "margin-left:-25px;margin-bottom:10px;margin-top:0px")
                              ),
                              column(width = 3,
                                     h5(HTML("<b>Proteins</b>"), style = "margin-left:-25px;margin-bottom:10px;margin-top:0px")
                              ),
                              column(width = 3,
                                     h5(HTML("<b>Zygosity</b>"), style = "margin-left:-25px;margin-bottom:10px;margin-top:0px")
                              ),
                            ),
                            uiOutput("vusGeneInfo"),
                            actionButton("addVUS", label = "Add VUS Gene",
                                         icon = icon('plus'),
                                         style = "color: white; background-color: #10699B; border-color: #10699B; margin-top: 10px"),
                            actionButton("removeVUS", label = "Remove Last VUS Gene",
                                         icon = icon('trash'),
                                         style = "color: white; background-color: #10699B; border-color: #10699B; margin-top: 10px")
                          )
                        ),
                      
                        # B/LP
                        tabPanel("B/LB",
                          wellPanel(style = "background:AliceBlue",
                            h4(HTML("<b>Benign/Likely Benign (B/LB) genes</b>"), style = "color:black"),
                            fluidRow(
                              column(width = 3,
                                     h5(HTML("<b>Gene</b>"), style = "margin-left:0px;margin-bottom:10px;margin-top:0px")
                              ),
                              column(width = 3, 
                                     h5(HTML("<b>Variants</b>"), style = "margin-left:-25px;margin-bottom:10px;margin-top:0px")
                              ),
                              column(width = 3,
                                     h5(HTML("<b>Proteins</b>"), style = "margin-left:-25px;margin-bottom:10px;margin-top:0px")
                              ),
                              column(width = 3,
                                     h5(HTML("<b>Zygosity</b>"), style = "margin-left:-25px;margin-bottom:10px;margin-top:0px")
                              ),
                            ),
                            uiOutput("blbGeneInfo"),
                            actionButton("addBLB", label = "Add B/LB Gene",
                                         icon = icon('plus'),
                                         style = "color: white; background-color: #10699B; border-color: #10699B; margin-top: 10px"),
                            actionButton("removeBLB", label = "Remove Last BLB Gene",
                                         icon = icon('trash'),
                                         style = "color: white; background-color: #10699B; border-color: #10699B; margin-top: 10px")
                          )
                        )
                      ) # end of tabsetPanel for gene results by type
                    ) # end of conditionalPanel for entering gene results by type
                  )), # end of column and fluidRow
                ), # end of tab for gene results entry and panel selection 
                
                # tab for review panel results
                tabPanel(title = "2. Panel Summary",
                  h4("Step 2: Review Panel Results"),
                  conditionalPanel("input.existingPanels == 'No panel selected' | input.existingPanels == 'Create new'",
                    h5("Select a panel or create a new one to display the panel genes.")
                  ),
                  conditionalPanel("input.existingPanels != 'No panel selected' & input.existingPanels != 'Create new'",
                    h5("The table below is a summary of the result you have entered so far. In only lists the 
                       genes in the panel you specified. Genes are marked a negative until they are recorded as 
                       P/LP, VUS, or B/LP on the left."),
                    
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
  
  #### Validate Age Inputs ####
  
  ## age
  validAge <- reactive({
    validate(validateAge(input$Age, input$Age))
  })
  output$validAge <- renderText({ validAge() })
  
  ## surgery ages
  # Oophorectomy age
  validOophAge <- reactive({
    validate(validateAge(input$OophAge, input$Age))
  })
  output$validOophAge <- renderText({ validOophAge() })
  # Mastectomy age
  validMastAge <- reactive({
    validate(validateAge(input$MastAge, input$Age))
  })
  output$validMastAge <- renderText({ validMastAge() })
  # Hysterectomy age
  validHystAge <- reactive({
    validate(validateAge(input$HystAge, input$Age))
  })
  output$validHystAge <- renderText({ validHystAge() })
  
  ## cancer ages
  validCanAges <- reactive({
    v.age <- canReactive$df$Age[which(canReactive$df$Cancer != "No cancer selected")]
    validate(unlist(lapply(v.age, validateAge, cur.age = input$Age))[1])
  })
  output$validCanAges <- renderText({ validCanAges() })
  
  #### Demographics / Create Pedigree ####
  
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
      if(is.null(PED())){
        if(input$Sex == "Female"){
          ps <- 0
        } else if(input$Sex == "Male"){
          ps <- 1
        } else {
          ps <- NA
        }
        PED(initPed(pedigree.id = input$pedID, pb.sex = ps))
      }
      
      # combine proband's mother and father race, ethnicity, and ancestry information
      if(input$RaceM != input$RaceF){
        pb.rc <- "All_Races"
      } else if(input$RaceM == input$RaceF){
        pb.rc <- input$RaceM
      }
      if(input$EthM == input$EthF){
        pb.et <- input$EthM
      } else if(all(c(input$EthM, input$EthP) != "Other_Ethnicity")){
        pb.et <- "Hispanic"
      } else {
        pb.et <- "Other_Ethnicity"
      }
      if(input$AncAJM | input$AncAJF){
        pb.an.aj <- TRUE
      } else {
        pb.an.aj <- FALSE
      }
      if(input$AncItM | input$AncItF){
        pb.an.it <- TRUE
      } else {
        pb.an.it <- FALSE
      }
      
      # populate proband's demographics data and PedigreeID
      t.ped <- PED()
      t.ped <- popPersonData(tmp.ped = t.ped, id = input$relSelect, cur.age = input$Age, 
                             rc = pb.rc, et = pb.et, an.aj = pb.an.aj, an.it = pb.an.it)
      
      # populate mother's race and Ancestry information
      t.ped <- popPersonData(tmp.ped = t.ped, id = t.ped$MotherID[which(t.ped$isProband == 1)], 
                             rc = input$RaceM, et = input$EthM, 
                             an.aj = input$AncAJM, an.it = input$AncItM)
      
      # populate father's race and Ancestry information
      t.ped <- popPersonData(tmp.ped = t.ped, id = t.ped$FatherID[which(t.ped$isProband == 1)], 
                             rc = input$RaceF, et = input$EthF, 
                             an.aj = input$AncAJF, an.it = input$AncItF)
      PED(t.ped)
      
      # update the race, ethnicity, and ancestry inputs for the proband
      # which will be displayed after the pedigree is visualized (vs the ones for their mother and father)
      updateSelectInput(session, "race", selected = pb.rc)
      updateSelectInput(session, "eth", selected = pb.et)
      updateCheckboxInput(session, "ancAJ", value = pb.an.aj)
      updateCheckboxInput(session, "ancIt", value = pb.an.it)
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
  
  
  #### Cancer History ####
  
  ##### UI ####
  
  
  
  # for testing:
  observeEvent(CanCnt(), {
    print(paste0("CanCnt(): ", CanCnt()))
  })
  
  
  
  # count number of cancers
  CanCnt <- reactiveVal(1)
  observeEvent(input$addCan, {
    CanCnt(CanCnt()+1)
  })
  observeEvent(input$removeCan, {
    if(CanCnt() > 1){
      
      # update count, cannot go below 1
      CanCnt(CanCnt()-1)
      
      # index of cancer inputs to remove from memory
      ind <- CanCnt()+1
    } else if(CanCnt() == 1){
      ind <- 1
    }
    
    # remove UIs from memory to prevent duplication if they are recreated
    # see https://appsilon.com/how-to-safely-remove-a-dynamic-shiny-module/
    remove_shiny_inputs(id = paste0("#Can", ind), input)
    remove_shiny_inputs(id = paste0("#CanOther", ind), input)
    remove_shiny_inputs(id = paste0("#CanAge", ind), input)
    
    # remove values from temporary storage data frame
    canReactive$df$Cancer[ind] <- "No cancer selected"
    canReactive$df$Age[ind]    <- NA
    canReactive$df$Other[ind]  <- ""
  })
  
  # cancer history UI
  output$CanInputs <- renderUI({
    lapply(if(CanCnt() > 0){1:CanCnt()}else{1}, function(CanNum){
      fluidRow(
        column(width = 6, 
          selectInput(paste0('Can', CanNum), h5(paste0('Cancer ', CanNum,':')),
                      choices = CANCER.CHOICES$long,
                      width = "200px"),
          conditionalPanel(paste0('input.Can', CanNum, " == 'Other'"),
            fluidRow(
              column(6, h5("Other cancer:", style = "margin-left:25px")),
              column(6, 
                div(selectizeInput(paste0('CanOther', CanNum), label = NULL,
                                   choices = c("", non.pp.cancers), selected = "",
                                   multiple = FALSE, options = list(create=TRUE),
                                   width = "225px"),
                    style = "margin-left:-25px;margin-right:-100px"
                )
              )
            )
          )
        ),
        conditionalPanel(paste0("input.Can", CanNum, " != 'No cancer selected'"),
          column(width = 6,
            div(numericInput(paste0('CanAge', CanNum), h5("Diagnosis Age:"),
                             min = min.age, max = max.age, step = 1, value = NA,
                             width = "100px"),
                style = "margin-left:-50px"
            ),
          )
        )
      )
    })
  })
  
  # check for cancer duplicate entries
  dupCancers <- reactiveVal(FALSE)
  observeEvent(canReactive$df, {
    cs <- canReactive$df$Cancer[which(!canReactive$df$Cancer %in% c("Other","No cancer selected"))]
    if(length(cs) > 1){
      if(any(table(cs) > 1)){
        dupCancers(TRUE)
      } else {
        dupCancers(FALSE)
      }
    }
  }, ignoreInit = TRUE)
  output$dupCancers <- reactive({ dupCancers() })
  outputOptions(output, 'dupCancers', suspendWhenHidden = FALSE)
  
  
  ##### Storage ####
  canReactive <- reactiveValues(df = cancer.inputs.store)
  
  
  
  # FOR TESTING ONLY: observe df every time it changes
  observeEvent(canReactive$df, {
    View(canReactive$df)
  })
  
  
  
  # store cancer names in the order they are entered
  observe(
    lapply(1:CanCnt(), 
           function(cc){
             observeEvent(input[[paste0("Can",cc)]], {
               canReactive$df$Cancer[cc] <- input[[paste0("Can",cc)]]
             }, ignoreInit = TRUE)
           }
    )
  )
  
  # store cancer ages in the order they are entered
  observe(
    lapply(1:CanCnt(), 
           function(cc){
             observeEvent(input[[paste0("CanAge",cc)]], {
               canReactive$df$Age[cc] <- input[[paste0("CanAge",cc)]]
             }, ignoreInit = TRUE)
           }
    )
  )
  
  # store other cancer names in the order they are entered
  observe(
    lapply(1:CanCnt(), 
           function(cc){
             observeEvent(input[[paste0("CanOther",cc)]], {
               canReactive$df$Other[cc] <- input[[paste0("CanOther",cc)]]
             }, ignoreInit = TRUE)
           }
    )
  )
  
  # repopulate cancer inputs when a cancer is added or deleted and
  # remove previously selected cancers from dropdown choices
  observeEvent(list(input$addCan, input$removeCan, canReactive$df), {
    unselected.choices <- CANCER.CHOICES$long[which(!CANCER.CHOICES$long %in%
                                                      setdiff(canReactive$df$Cancer[1:CanCnt()],
                                                              c("No cancer selected", "Other")))]
    for(cc in 1:CanCnt()){
      if(length(unselected.choices) > 0){
        u.choices <- unique(c(unselected.choices, input[[paste0("Can", cc)]]))
        u.choices <- CANCER.CHOICES$long[which(CANCER.CHOICES$long %in% u.choices)] # keep the order consistent
      } else {
        u.choices <- CANCER.CHOICES$long
      }
      updateSelectInput(session, paste0("Can",cc), selected = canReactive$df$Cancer[cc], choices = u.choices)
      updateSelectInput(session, paste0("CanAge",cc), selected = canReactive$df$Age[cc])
      updateSelectInput(session, paste0("CanOther",cc), selected = canReactive$df$Other[cc])
    }
  }, ignoreInit = TRUE)
  
  # add data to pedigree when user navigates off of the tab
  onCanTab <- reactiveVal(FALSE)
  observeEvent(input$pedTabs, {
    if(onCanTab() & input$pedTabs != "Cancer Hx"){
      PED(popPersonData(tmp.ped = PED(), id = input$relSelect, cancers.and.ages = canReactive$df))
    }
    
    # update the reactive value to detect if the current tab is the target tab
    if(input$pedTabs == "Cancer Hx"){
      onCanTab(TRUE)
    } else {
      onCanTab(FALSE)
    }
  }, ignoreInit = TRUE)
  
  
  #### Tumor Markers ####
  
  # condition to inform UI whether to display tumor markers or not, based on  cancer hx
  showBCMarkers <- reactiveVal(FALSE)
  showCRCMarkers <- reactiveVal(FALSE)
  output$showBCMarkers <- reactive({ showBCMarkers() })
  output$showCRCMarkers <- reactive({ showCRCMarkers() })
  outputOptions(output, 'showBCMarkers', suspendWhenHidden = FALSE)
  outputOptions(output, 'showCRCMarkers', suspendWhenHidden = FALSE)
  observeEvent(canReactive$df, {
    
    # check updated cancers list for presence of tumor marker related cancers
    if("Breast" %in% canReactive$df$Cancer){ 
      hadBC <- TRUE 
      showBCMarkers(TRUE)
    } else {
      hadBC <- FALSE
      showBCMarkers(FALSE)
    }
    if("Colorectal" %in% canReactive$df$Cancer){ 
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
  
  
  #### Genes ####
  
  # create storage for each result type and for the genes in the selected panel
  geneReactive <- reactiveValues(plp.df = gene.inputs.store,
                                 vus.df = gene.inputs.store,
                                 blb.df = gene.inputs.store,
                                 panel.genes = as.character())
  
  ##### Panel ####
  
  ### existing panel
  # reset storage contains when the panel changes
  observeEvent(input$existingPanels, {
    geneReactive$plp.df <- gene.inputs.store
    geneReactive$vus.df <- gene.inputs.store
    geneReactive$blb.df <- gene.inputs.store
    geneReactive$panel.genes <- all.panels[[input$existingPanels]]
  }, ignoreInit = TRUE)
  
  
  ### create new panel (PLACEHOLDER)
  
  
  ##### UI ####
  
  ## count number of genes by result type
  # PLP
  PLPCnt <- reactiveVal(1)
  observeEvent(input$addPLP, {
    PLPCnt(PLPCnt()+1)
  })
  observeEvent(input$removePLP, {
    if(PLPCnt() > 0){
      
      # update count
      PLPCnt(PLPCnt()-1)
      
      # remove UIs from memory to prevent duplication if they are recreated
      # see https://appsilon.com/how-to-safely-remove-a-dynamic-shiny-module/
      remove_shiny_inputs(id = paste0("#PLPGene",PLPCnt()+1), input)
      remove_shiny_inputs(id = paste0("#PLPVarInfo",PLPCnt()+1), input)
      remove_shiny_inputs(id = paste0("#PLPProtInfo",PLPCnt()+1), input)
      remove_shiny_inputs(id = paste0("#PLPZygInfo",PLPCnt()+1), input)
    }
  })
  
  # VUS
  VUSCnt <- reactiveVal(1)
  observeEvent(input$addVUS, {
    VUSCnt(VUSCnt()+1)
  })
  observeEvent(input$removeVUS, {
    if(VUSCnt() > 0){
      
      # update count
      VUSCnt(VUSCnt()-1)
      
      # remove UIs from memory to prevent duplication if they are recreated
      # see https://appsilon.com/how-to-safely-remove-a-dynamic-shiny-module/
      remove_shiny_inputs(id = paste0("#VUSGene",VUSCnt()+1), input)
      remove_shiny_inputs(id = paste0("#VUSVarInfo",VUSCnt()+1), input)
      remove_shiny_inputs(id = paste0("#VUSProtInfo",VUSCnt()+1), input)
      remove_shiny_inputs(id = paste0("#VUSZygInfo",VUSCnt()+1), input)
    }
  })
  
  # BLB
  BLBCnt <- reactiveVal(1)
  observeEvent(input$addBLB, {
    BLBCnt(BLBCnt()+1)
  })
  observeEvent(input$removeBLB, {
    if(BLBCnt() > 0){
      
      # update count
      BLBCnt(BLBCnt()-1)
      
      # remove UIs from memory to prevent duplication if they are recreated
      # see https://appsilon.com/how-to-safely-remove-a-dynamic-shiny-module/
      remove_shiny_inputs(id = paste0("#BLBGene",BLBCnt()+1), input)
      remove_shiny_inputs(id = paste0("#BLBVarInfo",BLBCnt()+1), input)
      remove_shiny_inputs(id = paste0("#BLBProtInfo",BLBCnt()+1), input)
      remove_shiny_inputs(id = paste0("#BLBZygInfo",BLBCnt()+1), input)
    }
  })
  
  ## dynamic data inputs for each gene by result type
  # PLP
  output$plpGeneInfo <- renderUI({
      lapply(if(PLPCnt() > 0){1:PLPCnt()}else{1}, function(plpNum){
        fluidRow(
          column(width = 3,
                 div(style = "margin-left:0px;margin-top:0px;margin-bottom:-10px",
                     selectInput(paste0('PLPGene', plpNum),
                                 label = NULL, 
                                 choices = c("", geneReactive$panel.genes),
                                 selected = "",
                                 width = "125px")
                     
                 )
          ),
          conditionalPanel(paste0("input.PLPGene", plpNum, " != ''"),
            column(width = 3,
                   div(style = "margin-left:-25px;margin-right:0px;margin-top:0px;margin-bottom:-10px",
                       selectizeInput(paste0('PLPVarInfo', plpNum), 
                                      label = NULL,
                                      choices = "", 
                                      selected = "",
                                      multiple = TRUE, 
                                      options = list(create=TRUE),
                                      width = "130px")
                   )
            ),
            column(width = 3,
                   div(style = "margin-left:-25px;margin-right:0px;margin-top:0px;margin-bottom:-10px",
                       selectizeInput(paste0('PLPProtInfo', plpNum),
                                      label = NULL,
                                      choices = "",
                                      selected = "",
                                      multiple = TRUE,
                                      options = list(create=TRUE),
                                      width = "130px")
                   )
            ),
            column(width = 3,
                   div(style = "margin-left:-25px;margin-right:-50px;margin-top:0px;margin-bottom:-10px",
                       selectInput(paste0('PLPZygInfo', plpNum),
                                   label = NULL,
                                   choices = zyg.choices,
                                   selected = "Unk",
                                   width = "100px")
                   )
            )
          )
        )
      })
  })
  
  # VUS
  output$vusGeneInfo <- renderUI({
    lapply(if(VUSCnt() > 0){1:VUSCnt()}else{1}, function(vusNum){
      fluidRow(
        column(width = 3,
               div(style = "margin-left:0px;margin-top:0px;margin-bottom:-10px",
                   selectInput(paste0('VUSGene', vusNum),
                               label = NULL, 
                               choices = c("", geneReactive$panel.genes),
                               selected = "",
                               width = "125px")
                   
               )
        ),
        conditionalPanel(paste0("input.VUSGene", vusNum, " != ''"),
                         column(width = 3,
                                div(style = "margin-left:-25px;margin-right:0px;margin-top:0px;margin-bottom:-10px",
                                    selectizeInput(paste0('VUSVarInfo', vusNum), 
                                                   label = NULL,
                                                   choices = "", 
                                                   selected = "",
                                                   multiple = TRUE, 
                                                   options = list(create=TRUE),
                                                   width = "130px")
                                )
                         ),
                         column(width = 3,
                                div(style = "margin-left:-25px;margin-right:0px;margin-top:0px;margin-bottom:-10px",
                                    selectizeInput(paste0('VUSProtInfo', vusNum),
                                                   label = NULL,
                                                   choices = "",
                                                   selected = "",
                                                   multiple = TRUE,
                                                   options = list(create=TRUE),
                                                   width = "130px")
                                )
                         ),
                         column(width = 3,
                                div(style = "margin-left:-25px;margin-right:-50px;margin-top:0px;margin-bottom:-10px",
                                    selectInput(paste0('VUSZygInfo', vusNum),
                                                label = NULL,
                                                choices = zyg.choices,
                                                selected = "Unk",
                                                width = "100px")
                                )
                         )
        )
      )
    })
  })
  
  # BLB
  output$blbGeneInfo <- renderUI({
    lapply(if(BLBCnt() > 0){1:BLBCnt()}else{1}, function(blbNum){
      fluidRow(
        column(width = 3,
               div(style = "margin-left:0px;margin-top:0px;margin-bottom:-10px",
                   selectInput(paste0('BLBGene', blbNum),
                               label = NULL, 
                               choices = c("", geneReactive$panel.genes),
                               selected = "",
                               width = "125px")
                   
               )
        ),
        conditionalPanel(paste0("input.BLBGene", blbNum, " != ''"),
                         column(width = 3,
                                div(style = "margin-left:-25px;margin-right:0px;margin-top:0px;margin-bottom:-10px",
                                    selectizeInput(paste0('BLBVarInfo', blbNum), 
                                                   label = NULL,
                                                   choices = "", 
                                                   selected = "",
                                                   multiple = TRUE, 
                                                   options = list(create=TRUE),
                                                   width = "130px")
                                )
                         ),
                         column(width = 3,
                                div(style = "margin-left:-25px;margin-right:0px;margin-top:0px;margin-bottom:-10px",
                                    selectizeInput(paste0('BLBProtInfo', blbNum),
                                                   label = NULL,
                                                   choices = "",
                                                   selected = "",
                                                   multiple = TRUE,
                                                   options = list(create=TRUE),
                                                   width = "130px")
                                )
                         ),
                         column(width = 3,
                                div(style = "margin-left:-25px;margin-right:-50px;margin-top:0px;margin-bottom:-10px",
                                    selectInput(paste0('BLBZygInfo', blbNum),
                                                label = NULL,
                                                choices = zyg.choices,
                                                selected = "Unk",
                                                width = "100px")
                                )
                         )
        )
      )
    })
  })
  
  ## check for gene duplicate entries
  # PLP
  dupPLP <- reactiveVal(FALSE)
  observeEvent(geneReactive$plp.df, {
    gs <- geneReactive$plp.df$Gene[which(geneReactive$plp.df$Gene != "")]
    if(length(gs) > 1){
      if(any(table(gs) > 1)){
        dupPLP(TRUE)
      } else {
        dupPLP(FALSE)
      }
    }
  }, ignoreInit = TRUE)
  output$dupPLP <- reactive({ dupPLP() })
  outputOptions(output, 'dupPLP', suspendWhenHidden = FALSE)
  
  # VUS
  dupVUS <- reactiveVal(FALSE)
  observeEvent(geneReactive$vus.df, {
    gs <- geneReactive$vus.df$Gene[which(geneReactive$vus.df$Gene != "")]
    if(length(gs) > 1){
      if(any(table(gs) > 1)){
        dupVUS(TRUE)
      } else {
        dupVUS(FALSE)
      }
    }
  }, ignoreInit = TRUE)
  output$dupVUS <- reactive({ dupVUS() })
  outputOptions(output, 'dupVUS', suspendWhenHidden = FALSE)
  
  # BLB
  dupBLB <- reactiveVal(FALSE)
  observeEvent(geneReactive$blb.df, {
    gs <- geneReactive$blb.df$Gene[which(geneReactive$blb.df$Gene != "")]
    if(length(gs) > 1){
      if(any(table(gs) > 1)){
        dupBLB(TRUE)
      } else {
        dupBLB(FALSE)
      }
    }
  }, ignoreInit = TRUE)
  output$dupBLB <- reactive({ dupBLB() })
  outputOptions(output, 'dupBLB', suspendWhenHidden = FALSE)
  
  # warn user if a gene is listed in more than one result category (possible, but rare)
  dupResultGene <- reactiveVal(FALSE)
  observeEvent(list(geneReactive$plp.df, geneReactive$vus.df$Gene, geneReactive$blb.df$Gene), {
    plp.g <- setdiff(geneReactive$plp.df$Gene, "")
    vus.g <- setdiff(geneReactive$vus.df$Gene, "")
    blb.g <- setdiff(geneReactive$blb.df$Gene, "")
    
    # if a gene is double listed in more than one result category, then warn the user
    if(any(length(intersect(plp.g, vus.g)) > 0,
           length(intersect(plp.g, blb.g)) > 0,
           length(intersect(vus.g, blb.g)) > 0)){
      dupResultGene(TRUE)
    } else {
      dupResultGene(FALSE)
    }
  }, ignoreInit = TRUE)
  output$dupResultGene <- reactive({ dupResultGene() })
  outputOptions(output, 'dupResultGene', suspendWhenHidden = FALSE)
  
  
  # panel summary table
  panelSum <- reactive({
    tmp.plp <- 
      geneReactive$plp.df[which(geneReactive$plp.df$Gene != ""),] %>%
      mutate(Result = "P/LP", .after = "Gene") %>%
      arrange(Gene)
    tmp.vus <- 
      geneReactive$vus.df[which(geneReactive$vus.df$Gene != ""),] %>%
      mutate(Result = "VUS", .after = "Gene") %>%
      arrange(Gene)
    tmp.blb <- 
      geneReactive$blb.df[which(geneReactive$blb.df$Gene != ""),] %>%
      mutate(Result = "B/LB", .after = "Gene") %>%
      arrange(Gene)
    
    # genes not listed in a result category are assumed negative
    neg.g <- setdiff(geneReactive$panel.genes, c(tmp.plp$Gene, tmp.vus$Gene, tmp.blb$Gene))
    tmp.neg <- data.frame(Gene = neg.g,
                          Result = rep("Neg", length(neg.g)),
                          Variants = rep("", length(neg.g)),
                          Proteins = rep("", length(neg.g)),
                          Zygosity = rep("", length(neg.g)))
    
    # combine all result types
    sum.df <- 
      tmp.plp %>%
      bind_rows(tmp.vus) %>%
      bind_rows(tmp.blb) %>%
      bind_rows(tmp.neg) 
    
    # ensure genes with multiple result types are stacked in the summary
    if(dupResultGene()){
      all.results <- c(tmp.plp$Gene, tmp.vus$Gene, tmp.blb$Gene)
      results.tbl <- table(all.results)
      dups <- names(results.tbl)[which(results.tbl > 1)]
      for(d in 1:length(dups)){
        d.rows <- which(sum.df$Gene == dups[d])
        move.rows <- d.rows[2:length(d.rows)]
        other.rows <- setdiff(1:nrow(sum.df), c(1:d.rows[1], move.rows))
        sum.df <- sum.df[c(1:(d.rows[1]), move.rows, other.rows),]
      }
    }
    sum.df
  })
  
  # output formatted data table colored by result type
  output$panelSum <- renderDataTable({
    datatable(panelSum()) %>%
      formatStyle('Result',
                  backgroundColor = styleEqual(c("P/LP","VUS","B/LB","Neg"),
                                               c("MistyRose","LightGreen","AliceBlue","white")),
                  fontWeight = 'bold')
  })
  
  
  ##### Storage ####
  
  ## store gene names in the order they are entered
  # PLP
  observe(
    lapply(1:PLPCnt(), 
           function(plpc){
             observeEvent(input[[paste0("PLPGene",plpc)]], {
               if(input[[paste0("PLPGene",plpc)]] != ""){
                 geneReactive$plp.df$Gene[plpc] <- input[[paste0("PLPGene",plpc)]]
               }
             }, ignoreInit = TRUE)
           }
    )
  )
  
  # VUS
  observe(
    lapply(1:VUSCnt(), 
           function(vusc){
             observeEvent(input[[paste0("VUSGene",vusc)]], {
               if(input[[paste0("VUSGene",vusc)]] != ""){
                 geneReactive$vus.df$Gene[vusc] <- input[[paste0("VUSGene",vusc)]]
               }
             }, ignoreInit = TRUE)
           }
    )
  )
  
  # BLB
  observe(
    lapply(1:BLBCnt(), 
           function(blbc){
             observeEvent(input[[paste0("BLBGene",blbc)]], {
               if(input[[paste0("BLBGene",blbc)]] != ""){
                 geneReactive$blb.df$Gene[blbc] <- input[[paste0("BLBGene",blbc)]]
               }
             }, ignoreInit = TRUE)
           }
    )
  )
  
  ## store variants in the order they are entered
  # PLP
  observe(
    lapply(1:PLPCnt(), 
           function(plpc){
             observeEvent(input[[paste0("PLPVarInfo",plpc)]], {
               geneReactive$plp.df$Variants[plpc] <- paste0(input[[paste0("PLPVarInfo",plpc)]], collapse = ", ")
             }, ignoreInit = TRUE, ignoreNULL = FALSE)
           }
    )
  )
  
  # VUS
  observe(
    lapply(1:VUSCnt(), 
           function(vusc){
             observeEvent(input[[paste0("VUSVarInfo",vusc)]], {
               geneReactive$vus.df$Variants[vusc] <- paste0(input[[paste0("VUSVarInfo",vusc)]], collapse = ", ")
             }, ignoreInit = TRUE, ignoreNULL = FALSE)
           }
    )
  )
  
  # BLB
  observe(
    lapply(1:BLBCnt(), 
           function(blbc){
             observeEvent(input[[paste0("BLBVarInfo",blbc)]], {
               geneReactive$blb.df$Variants[blbc] <- paste0(input[[paste0("BLBVarInfo",blbc)]], collapse = ", ")
             }, ignoreInit = TRUE, ignoreNULL = FALSE)
           }
    )
  )
  
  ## store proteins in the order they are entered
  # PLP
  observe(
    lapply(1:PLPCnt(), 
           function(plpc){
             observeEvent(input[[paste0("PLPProtInfo",plpc)]], {
               geneReactive$plp.df$Proteins[plpc] <- paste0(input[[paste0("PLPProtInfo",plpc)]], collapse = ", ")
             }, ignoreInit = TRUE, ignoreNULL = FALSE)
           }
    )
  )
  
  # VUS
  observe(
    lapply(1:VUSCnt(), 
           function(vusc){
             observeEvent(input[[paste0("VUSProtInfo",vusc)]], {
               geneReactive$vus.df$Proteins[vusc] <- paste0(input[[paste0("VUSProtInfo",vusc)]], collapse = ", ")
             }, ignoreInit = TRUE, ignoreNULL = FALSE)
           }
    )
  )
  
  # BLB
  observe(
    lapply(1:BLBCnt(), 
           function(blbc){
             observeEvent(input[[paste0("BLBProtInfo",blbc)]], {
               geneReactive$blb.df$Proteins[blbc] <- paste0(input[[paste0("BLBProtInfo",blbc)]], collapse = ", ")
             }, ignoreInit = TRUE, ignoreNULL = FALSE)
           }
    )
  )
  
  ## store homo/hetero in the order they are entered
  # PLP
  observe(
    lapply(1:PLPCnt(), 
           function(plpc){
             observeEvent(input[[paste0("PLPZygInfo",plpc)]], {
               geneReactive$plp.df$Zygosity[plpc] <- input[[paste0("PLPZygInfo",plpc)]]
             }, ignoreInit = TRUE)
           }
    )
  )
  
  # VUS
  observe(
    lapply(1:VUSCnt(), 
           function(vusc){
             observeEvent(input[[paste0("VUSZygInfo",vusc)]], {
               geneReactive$vus.df$Zygosity[vusc] <- input[[paste0("VUSZygInfo",vusc)]]
             }, ignoreInit = TRUE)
           }
    )
  )
  
  # BLB
  observe(
    lapply(1:BLBCnt(), 
           function(blbc){
             observeEvent(input[[paste0("BLBZygInfo",blbc)]], {
               geneReactive$blb.df$Zygosity[blbc] <- input[[paste0("BLBZygInfo",blbc)]]
             }, ignoreInit = TRUE)
           }
    )
  )
  
  ## repopulate gene inputs when a gene is added or deleted and remove previously selected genes from dropdown choices
  # PLP
  observeEvent(list(input$addPLP, input$removePLP, geneReactive$plp.df), {
    unselected.choices <- geneReactive$panel.genes[which(!geneReactive$panel.genes %in% 
                                                      setdiff(geneReactive$plp.df$Gene[1:PLPCnt()], ""))]
    if(PLPCnt() > 0){
      for(plpc in 1:PLPCnt()){
        
        # genes
        if(length(unselected.choices) > 0){
          u.choices <- unique(c(unselected.choices, input[[paste0("PLPGene", plpc)]]))
          u.choices <- geneReactive$panel.genes[which(geneReactive$panel.genes %in% u.choices)] # keep the order consistent
        } else {
          u.choices <- geneReactive$panel.genes
        }
        updateSelectInput(session, paste0("PLPGene",plpc), selected = geneReactive$plp.df$Gene[plpc], choices = u.choices)
        
        # variant/protein
        if(geneReactive$plp.df$Gene[plpc] != ""){
          tmp.var.prot <-
            strsplit(c(geneReactive$plp.df$Variants[plpc],
                       geneReactive$plp.df$Proteins[plpc]),
                     split = ", ")

          # replace missing variant/protein values with blank
          for(el in 1:length(tmp.var.prot)){
            if(length(tmp.var.prot[[el]]) == 0){
              tmp.var.prot[[el]] <- ""
            }
          }
        } else {
          tmp.var.prot <- list("","")
        }

        updateSelectInput(session, paste0("PLPVarInfo",plpc), selected = tmp.var.prot[[1]], choices = tmp.var.prot[[1]])
        updateSelectInput(session, paste0("PLPProtInfo",plpc), selected = tmp.var.prot[[2]], choices = tmp.var.prot[[2]])
        
        # homo/hetero
        updateSelectInput(session, paste0("PLPZygInfo",plpc), selected = geneReactive$plp.df$Zygosity[plpc])
      }
    }
  }, ignoreInit = TRUE)
  
  # VUS
  observeEvent(list(input$addVUS, input$removeVUS, geneReactive$vus.df), {
    unselected.choices <- geneReactive$panel.genes[which(!geneReactive$panel.genes %in% 
                                                           setdiff(geneReactive$vus.df$Gene[1:VUSCnt()], ""))]
    if(VUSCnt() > 0){
      for(vusc in 1:VUSCnt()){
        
        # genes
        if(length(unselected.choices) > 0){
          u.choices <- unique(c(unselected.choices, input[[paste0("VUSGene", vusc)]]))
          u.choices <- geneReactive$panel.genes[which(geneReactive$panel.genes %in% u.choices)] # keep the order consistent
        } else {
          u.choices <- geneReactive$panel.genes
        }
        updateSelectInput(session, paste0("VUSGene",vusc), selected = geneReactive$vus.df$Gene[vusc], choices = u.choices)
        
        # variant/protein
        if(geneReactive$vus.df$Gene[vusc] != ""){
          tmp.var.prot <-
            strsplit(c(geneReactive$vus.df$Variants[vusc],
                       geneReactive$vus.df$Proteins[vusc]),
                     split = ", ")
          
          # replace missing variant/protein values with blank
          for(el in 1:length(tmp.var.prot)){
            if(length(tmp.var.prot[[el]]) == 0){
              tmp.var.prot[[el]] <- ""
            }
          }
        } else {
          tmp.var.prot <- list("","")
        }
        
        updateSelectInput(session, paste0("VUSVarInfo",vusc), selected = tmp.var.prot[[1]], choices = tmp.var.prot[[1]])
        updateSelectInput(session, paste0("VUSProtInfo",vusc), selected = tmp.var.prot[[2]], choices = tmp.var.prot[[2]])
        
        # homo/hetero
        updateSelectInput(session, paste0("VUSZygInfo",vusc), selected = geneReactive$vus.df$Zygosity[vusc])
      }
    }
  }, ignoreInit = TRUE)
  
  # BLB
  observeEvent(list(input$addBLB, input$removeBLB, geneReactive$blb.df), {
    unselected.choices <- geneReactive$panel.genes[which(!geneReactive$panel.genes %in% 
                                                           setdiff(geneReactive$blb.df$Gene[1:BLBCnt()], ""))]
    if(BLBCnt() > 0){
      for(blbc in 1:BLBCnt()){
        
        # genes
        if(length(unselected.choices) > 0){
          u.choices <- unique(c(unselected.choices, input[[paste0("BLBGene", blbc)]]))
          u.choices <- geneReactive$panel.genes[which(geneReactive$panel.genes %in% u.choices)] # keep the order consistent
        } else {
          u.choices <- geneReactive$panel.genes
        }
        updateSelectInput(session, paste0("BLBGene",blbc), selected = geneReactive$blb.df$Gene[blbc], choices = u.choices)
        
        # variant/protein
        if(geneReactive$blb.df$Gene[blbc] != ""){
          tmp.var.prot <-
            strsplit(c(geneReactive$blb.df$Variants[blbc],
                       geneReactive$blb.df$Proteins[blbc]),
                     split = ", ")
          
          # replace missing variant/protein values with blank
          for(el in 1:length(tmp.var.prot)){
            if(length(tmp.var.prot[[el]]) == 0){
              tmp.var.prot[[el]] <- ""
            }
          }
        } else {
          tmp.var.prot <- list("","")
        }
        
        updateSelectInput(session, paste0("BLBVarInfo",blbc), selected = tmp.var.prot[[1]], choices = tmp.var.prot[[1]])
        updateSelectInput(session, paste0("BLBProtInfo",blbc), selected = tmp.var.prot[[2]], choices = tmp.var.prot[[2]])
        
        # homo/hetero
        updateSelectInput(session, paste0("BLBZygInfo",blbc), selected = geneReactive$blb.df$Zygosity[blbc])
      }
    }
  }, ignoreInit = TRUE)
  
  ## remove gene from temporary storage when the input is deleted
  # PLP
  observeEvent(input$removePLP, {
    geneReactive$plp.df$Gene[PLPCnt()+1] <- ""
    geneReactive$plp.df$Variants[PLPCnt()+1] <- ""
    geneReactive$plp.df$Proteins[PLPCnt()+1]  <- ""
    geneReactive$plp.df$Zygosity[PLPCnt()+1]  <- "Unk"
  }, ignoreInit = TRUE)
  
  # VUS
  observeEvent(input$removeVUS, {
    geneReactive$vus.df$Gene[VUSCnt()+1] <- ""
    geneReactive$vus.df$Variants[VUSCnt()+1] <- ""
    geneReactive$vus.df$Proteins[VUSCnt()+1]  <- ""
    geneReactive$vus.df$Zygosity[VUSCnt()+1]  <- "Unk"
  }, ignoreInit = TRUE)
  
  # BLB
  observeEvent(input$removeBLB, {
    geneReactive$blb.df$Gene[BLBCnt()+1] <- ""
    geneReactive$blb.df$Variants[BLBCnt()+1] <- ""
    geneReactive$blb.df$Proteins[BLBCnt()+1]  <- ""
    geneReactive$blb.df$Zygosity[BLBCnt()+1]  <- "Unk"
  }, ignoreInit = TRUE)
  
  ### store variant/protein/homo-hetero information by result type
  # PLP
  observe(
    if(length(input$PLPGenes) > 0 & !(all(input$PLPGenes == ""))){
      lapply(1:length(input$PLPGenes),
             function(plpNum){
               plp.g.name <- input$PLPGenes[plpNum]
               
               # variants: store variants as one string
               observeEvent(input[[paste0('PLPVarInfo', plpNum)]], {
                 if(is.null(input[[paste0('PLPVarInfo', plpNum)]])){
                   tmp.var <- ""
                 } else {
                   tmp.var <- input[[paste0('PLPVarInfo', plpNum)]]
                 }
                 geneReactive$plp.df$Variants[which(geneReactive$plp.df$Gene == plp.g.name & geneReactive$plp.df$Result == 1)] <- 
                   paste0(unique(tmp.var), collapse = ", ")
               }, ignoreInit = TRUE, ignoreNULL = FALSE)
               
               # proteins: store proteins as one string
               observeEvent(input[[paste0('PLPProtInfo', plpNum)]], {
                 if(is.null(input[[paste0('PLPProtInfo', plpNum)]])){
                   tmp.prot <- ""
                 } else {
                   tmp.prot <- input[[paste0('PLPProtInfo', plpNum)]]
                 }
                 geneReactive$plp.df$Proteins[which(geneReactive$plp.df$Gene == plp.g.name & geneReactive$plp.df$Result == 1)] <- 
                   paste0(unique(tmp.prot), collapse = ", ")
               }, ignoreInit = TRUE, ignoreNULL = FALSE)
               
               # homo/hetero
               observeEvent(input[[paste0('PLPZygInfo', plpNum)]], {
                 geneReactive$plp.df$Zygosity[which(geneReactive$plp.df$Gene == plp.g.name & geneReactive$plp.df$Result == 1)] <- 
                   paste0(input[[paste0('PLPZygInfo', plpNum)]], collapse = ", ")
               }, ignoreInit = TRUE, ignoreNULL = FALSE)
             })
    }
  )
  
  # VUS
  observe(
    if(length(input$VUSGenes) > 0 & !(all(input$VUSGenes == ""))){
      lapply(1:length(input$VUSGenes),
             function(vusNum){
               vus.g.name <- input$VUSGenes[vusNum]
               
               # variants: store variants as one string
               observeEvent(input[[paste0('VUSVarInfo', vusNum)]], {
                 if(is.null(input[[paste0('VUSVarInfo', vusNum)]])){
                   tmp.var <- ""
                 } else {
                   tmp.var <- input[[paste0('VUSVarInfo', vusNum)]]
                 }
                 geneReactive$vus.df$Variants[which(geneReactive$vus.df$Gene == vus.g.name & geneReactive$vus.df$Result == 1)] <- 
                   paste0(unique(tmp.var), collapse = ", ")
               }, ignoreInit = TRUE, ignoreNULL = FALSE)
               
               # proteins: store proteins as one string
               observeEvent(input[[paste0('VUSProtInfo', vusNum)]], {
                 if(is.null(input[[paste0('VUSProtInfo', vusNum)]])){
                   tmp.prot <- ""
                 } else {
                   tmp.prot <- input[[paste0('VUSProtInfo', vusNum)]]
                 }
                 geneReactive$vus.df$Proteins[which(geneReactive$vus.df$Gene == vus.g.name & geneReactive$vus.df$Result == 1)] <- 
                   paste0(unique(tmp.prot), collapse = ", ")
               }, ignoreInit = TRUE, ignoreNULL = FALSE)
               
               # homo/hetero
               observeEvent(input[[paste0('VUSZygInfo', vusNum)]], {
                 geneReactive$vus.df$Zygosity[which(geneReactive$vus.df$Gene == vus.g.name & geneReactive$vus.df$Result == 1)] <- 
                   paste0(input[[paste0('VUSZygInfo', vusNum)]], collapse = ", ")
               }, ignoreInit = TRUE, ignoreNULL = FALSE)
             })
    }
  )
  
  # BLB
  observe(
    if(length(input$BLBGenes) > 0 & !(all(input$BLBGenes == ""))){
      lapply(1:length(input$BLBGenes),
             function(blbNum){
               blb.g.name <- input$BLBGenes[blbNum]
               
               # variants: store variants as one string
               observeEvent(input[[paste0('BLBVarInfo', blbNum)]], {
                 if(is.null(input[[paste0('BLBVarInfo', blbNum)]])){
                   tmp.var <- ""
                 } else {
                   tmp.var <- input[[paste0('BLBVarInfo', blbNum)]]
                 }
                 geneReactive$blb.df$Variants[which(geneReactive$blb.df$Gene == blb.g.name & geneReactive$blb.df$Result == 1)] <- 
                   paste0(unique(tmp.var), collapse = ", ")
               }, ignoreInit = TRUE, ignoreNULL = FALSE)
               
               # proteins: store proteins as one string
               observeEvent(input[[paste0('BLBProtInfo', blbNum)]], {
                 if(is.null(input[[paste0('BLBProtInfo', blbNum)]])){
                   tmp.prot <- ""
                 } else {
                   tmp.prot <- input[[paste0('BLBProtInfo', blbNum)]]
                 }
                 geneReactive$blb.df$Proteins[which(geneReactive$blb.df$Gene == blb.g.name & geneReactive$blb.df$Result == 1)] <- 
                   paste0(unique(tmp.prot), collapse = ", ")
               }, ignoreInit = TRUE, ignoreNULL = FALSE)
               
               # homo/hetero
               observeEvent(input[[paste0('BLBZygInfo', blbNum)]], {
                 geneReactive$blb.df$Zygosity[which(geneReactive$blb.df$Gene == blb.g.name & geneReactive$blb.df$Result == 1)] <- 
                   paste0(input[[paste0('BLBZygInfo', blbNum)]], collapse = ", ")
               }, ignoreInit = TRUE, ignoreNULL = FALSE)
             })
    }
  )
  
  # add data to pedigree when user navigates off of the tab
  onGeneTab <- reactiveVal(FALSE)
  observeEvent(input$pedTabs, {
    if(onGeneTab() & input$pedTabs != "Genes"){
      PED(popPersonData(tmp.ped = PED(), id = input$relSelect, 
                        gene.results = panelSum(), 
                        panel.name = input$existingPanels))
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
      }
      
      # assume, initially, brother's race/eth/ancestry match the proband's
      PED(assumeBackground(PED()))
    }
    
    # add maternal aunts and uncles
    if(input$numMAunt > 0 | input$numMUnc > 0){
      
      # first, create maternal grandparents
      # assume, initially, race/eth/ancestry match the proband's mother
      PED(formatNewPerson(relation = "grandmother", tmp.ped = PED(), m.or.p.side = "m"))
      PED(assumeBackground(PED(), PED()$ID[which(PED()$relation == "mother")]))
      PED(formatNewPerson(relation = "grandfather", tmp.ped = PED(), m.or.p.side = "m"))
      PED(assumeBackground(PED(), PED()$ID[which(PED()$relation == "mother")]))
      
      # add maternal aunts iteratively
      if(input$numMAunt > 0){
        for(i in 1:input$numMAunt){
          PED(formatNewPerson(relation = "aunt", tmp.ped = PED(), m.or.p.side = "m"))
          
          # assume, initially, aunt's race/eth/ancestry match the proband's mother
          PED(assumeBackground(PED(), PED()$ID[which(PED()$relation == "mother")]))
        }
      }
      # add maternal uncles iteratively
      if(input$numMUnc > 0){
        for(i in 1:input$numMUnc){
          PED(formatNewPerson(relation = "uncle", tmp.ped = PED(), m.or.p.side = "m"))
          
          # assume, initially, uncle's race/eth/ancestry match the proband's mother
          PED(assumeBackground(PED(), PED()$ID[which(PED()$relation == "mother")]))
        }
      }
    }
    
    # add paternal aunts and uncles
    if(input$numPAunt > 0 | input$numPUnc > 0){
      
      # first, create paternal grandparents
      # assume, initially, race/eth/ancestry match the proband's father
      PED(formatNewPerson(relation = "grandmother", tmp.ped = PED(), m.or.p.side = "p"))
      PED(assumeBackground(PED(), PED()$ID[which(PED()$relation == "father")]))
      PED(formatNewPerson(relation = "grandfather", tmp.ped = PED(), m.or.p.side = "p"))
      PED(assumeBackground(PED(), PED()$ID[which(PED()$relation == "father")]))
      
      # add paternal aunts iteratively
      if(input$numPAunt > 0){
        for(i in 1:input$numPAunt){
          PED(formatNewPerson(relation = "aunt", tmp.ped = PED(), m.or.p.side = "p"))
          
          # assume, initially, aunt's race/eth/ancestry match the proband's father
          PED(assumeBackground(PED(), PED()$ID[which(PED()$relation == "father")]))
        }
      }
      # add paternal uncles iteratively
      if(input$numPUnc > 0){
        for(i in 1:input$numPUnc){
          PED(formatNewPerson(relation = "uncle", tmp.ped = PED(), m.or.p.side = "p"))
          
          # assume, initially, uncle's race/eth/ancestry match the proband's father
          PED(assumeBackground(PED(), PED()$ID[which(PED()$relation == "father")]))
        }
      }
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
        PED(popPersonData(tmp.ped = t.ped, id = lastRel(), cur.age = input$Age, 
                          rc = input$race, et = input$eth, 
                          an.aj = input$ancAJ, an.it = input$ancIt)
            )
        
        # surgical hx
      } else if(input$pedTabs == "Surgical Hx"){
        PED(popPersonData(tmp.ped = PED(), id = lastRel(), riskmods.and.ages = surgReactive$lst))
        
        # tumor markers
      } else if(input$pedTabs == "Tumor Markers"){
        PED(popPersonData(tmp.ped = PED(), id = lastRel(), 
                          er = input$ER, pr = input$PR, her2 = input$HER2,
                          ck5.6 = input$CK56, ck14 = input$CK14, msi = input$MSI))
        
        # cancer hx
      } else if(input$pedTabs == "Cancer Hx"){
        PED(popPersonData(tmp.ped = PED(), id = lastRel(), cancers.and.ages = canReactive$df))
        
        # genes
      } else if(input$pedTabs == "Genes"){
        PED(popPersonData(tmp.ped = PED(), id = lastRel(), 
                          gene.results = panelSum(), 
                          panel.name = input$existingPanels))
      }
      
      # update the last relative selected
      lastRel(as.numeric(input$relSelect))
      
      #### Re-populate data for new person ####
      rel.info <- PED()[which(PED()$ID == as.numeric(input$relSelect)),]
      
      ##### Demographics ####
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
      
      ## surgical hx
      for(sg in c("Mast", "Ooph", "Hyst")){
        updateCheckboxInput(session, sg, value = rel.info[[paste0("riskmod", sg)]])
        updateNumericInput(session, paste0(sg,"Age"), value = rel.info[[paste0("interAge", sg)]])
      }
      
      ##### Cancer Hx ####
      # clear inputs and data frame
      for(i in 1:CanCnt()){
        
        # update count of inputs
        if(CanCnt() > 1){
          CanCnt(CanCnt()-1)
        }
        
        # remove UIs from memory to prevent duplication if they are recreated
        # see https://appsilon.com/how-to-safely-remove-a-dynamic-shiny-module/
        remove_shiny_inputs(id = paste0("#Can",CanCnt()+1), input)
        remove_shiny_inputs(id = paste0("#CanAge",CanCnt()+1), input)
        remove_shiny_inputs(id = paste0("#CanOther",CanCnt()+1), input)
      }
      canReactive$df <- cancer.inputs.store
      
      # retrieve results for this person from the pedigree
      can.aff.info <- rel.info %>% select(starts_with("isAff"))
      cans.had.short <- sub(pattern = "isAff", replacement = "", colnames(can.aff.info)[which(can.aff.info == 1)])
      # if(!is.na(rel.info$NPP.isAffX.AgeX)){
      #   cans.had.short <- c(cans.had.short, "Other")
      # }
      cans.had.long <- CANCER.CHOICES$long[which(CANCER.CHOICES$short %in% cans.had.short)]
      
      # if there were results in the pedigree, loop through them
      if(length(cans.had.short) > 0){

        ## update data frame
        # PanelPRO cancers
        canReactive$df$Cancer[1:length(cans.had.long)] <- cans.had.long
        can.ages <-
          rel.info %>%
          select(all_of(paste0("Age", cans.had.short)))
        can.ages <- as.numeric(can.ages)
        canReactive$df$Age[1:length(cans.had.long)] <- can.ages
        
        # non-PanelPRO (Other) cancers
        
        
        # set choices based on what has already been selected
        unselected.choices <-
          CANCER.CHOICES$long[which(!CANCER.CHOICES$long %in% cans.had.long)]
        for(cc in 1:length(cans.had.short)){
          
          # add another set of inputs
          CanCnt(CanCnt()+1)
          
          # update inputs
          if(length(unselected.choices) > 0){
            u.choices <- unique(c(unselected.choices, cans.had.long[cc]))
            u.choices <- CANCER.CHOICES$long[which(CANCER.CHOICES$long %in% u.choices)] # keep the order consistent
          } else {
            u.choices <- CANCER.CHOICES$long
          }
          updateSelectInput(session, paste0("Can",cc),
                            choices = u.choices, selected = cans.had.long[cc])
          updateNumericInput(session, paste0("CanAge",cc),
                             value = can.ages[cc])
        }
      }
      
      ##### Tumor Markers ####
      marks <- c(PanelPRO:::MARKER_TESTING$BC$MARKERS, PanelPRO:::MARKER_TESTING$COL$MARKERS)
      for(m in marks){
        mval <- ifelse(is.na(rel.info[1,m]), "Not Tested",
                       ifelse(rel.info[1,m] == 1, "Positive",
                              ifelse(rel.info[1,m] == 0, "Negative", "Not Tested")))
        m <- ifelse(m == "CK5.6", "CK56", m)
        updateSelectInput(session, m, selected = mval)
      }
    } # end of if statement for input$visPed == TRUE
  }, ignoreInit = TRUE)
  
  #### PanelPRO ####

  
}

# Run the application 
shinyApp(ui = ui, server = server)
