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
        column(width = 12, class = "tosix",
               
          # select which relative is being edited, only show after pedigree is visualized
          conditionalPanel("input.visPed",
            selectInput("relSelect", label = h4("Select a relative to edit:"),
                        choices = c(1),
                        width = "150px")
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
              selectInput("pbSex", label = h5("*Sex assigned at birth:"),
                          choices = c(" "=" ","Female"="Female","Male"="Male"),
                          width = "150px"),
              numericInput("pbAge",
                           label = h5("*Current Age (1 to 89):"),
                           value = NA, min = min.age, max = max.age, step = 1,
                           width = "150px"),
              textOutput("validPbAge"),
              tags$head(tags$style("#validPbAge{color: red;}")),
              
              # create maternal and paternal race, ethnicity, and ancestry columns
              # for the proband (before pedigree is visualized)
              conditionalPanel("!input.visPed",
                fluidRow(
                  
                  # maternal race, ethnicity, ancestry column
                  column(width = 3, class = "tosix",
                    selectInput("pbRaceM", label = h5("Mother's Race:"),
                                choices = rc.choices,
                                selected = "Other or Unreported",
                                width = "95%"),
                    selectInput("pbEthM", label = h5("Mother's Hispanic Ethnicity:"),
                                choices = et.choices,
                                selected = "Other or Unreported",
                                width = "95%"),
                    h5("Mother's Ancestry (check all that apply):"),
                    div(style = "margin-left:25px",
                      checkboxInput("pbAncAJM", label = "Ashkenazi Jewish"),
                      checkboxInput("pbAncItM", label = "Italian")
                    )
                  ),
                  
                  # paternal race, ethnicity, ancestry column
                  column(width = 3, class = "tosix",
                    selectInput("pbRaceF", label = h5("Father's Race:"),
                                choices = rc.choices,
                                selected = "Other or Unreported",
                                width = "95%"),
                    selectInput("pbEthF", label = h5("Father's Hispanic Ethnicity:"),
                                choices = et.choices,
                                selected = "Other or Unreported",
                                width = "95%"),
                    h5("Father's Ancestry (check all that apply):"),
                    div(style = "margin-left:25px",
                      checkboxInput("pbAncAJF", label = "Ashkenazi Jewish"),
                      checkboxInput("pbAncItF", label = "Italian")
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
              conditionalPanel("input.pbSex != 'Female'",
                h5("Prophylactic surgery information is only required for females.")
              ),
              
              # for females
              conditionalPanel("input.pbSex == 'Female'",
                p("Check each surgery the person has had and enter the age at surgery."),
                
                # mastecomties
                fluidRow(
                  column(width = 3, class = "tosix",
                    checkboxInput("pbMast", label = "Bilateral Mastectomy",
                                  width = "150px")
                  ),
                  column(width = 3, class = "tosix",
                    conditionalPanel("input.pbMast",
                      div(style = "margin-left:-75px",
                        numericInput("pbMastAge",
                                     label = h5("Age at Mastectomy:"),
                                     value = NA, min = min.age, max = max.age, step = 1,
                                     width = "150px"),
                        textOutput("validpbMastAge"),
                        tags$head(tags$style("#validpbMastAge{color: red;}"))
                      )
                    )
                  ),
                ),
                
                # hysterectomies
                fluidRow(
                  column(width = 3, class = "tosix",
                    checkboxInput("pbHyst", label = "Hysterectomy",
                                  width = "150px")
                  ),
                  column(width = 3, class = "tosix",
                    conditionalPanel("input.pbHyst",
                      div(style = "margin-left:-75px",
                        numericInput("pbHystAge",
                                     label = h5("Age at Hysterectomy:"),
                                     value = NA, min = min.age, max = max.age, step = 1,
                                     width = "150px"),
                        textOutput("validpbHystAge"),
                        tags$head(tags$style("#validpbHystAge{color: red;}"))
                      )
                    )
                  ),
                ),
                
                # oophorectomies
                fluidRow(
                  column(width = 3, class = "tosix",
                    checkboxInput("pbOoph", label = "Bilateral Oophorectomy",
                                  width = "250px")
                  ),
                  column(width = 3, class = "tosix",
                    conditionalPanel("input.pbOoph",
                      div(style = "margin-left:-75px",
                        numericInput("pbOophAge",
                                     label = h5("Age at Oophorectomy:"),
                                     value = NA, min = min.age, max = max.age, step = 1,
                                     width = "150px"),
                        textOutput("validpbOophAge"),
                        tags$head(tags$style("#validpbOophAge{color: red;}"))
                      )
                    )
                  )
                )
              ) # end of female conditionalPanel for surgical history information
            ), # end of surgery tab
            
            ###### Tumor Markers ####
            tabPanel("Tumor Markers",
              h3("Tumor Markers"),
              p("If the person was tested for any of tumor markers options below, report the results."),
              conditionalPanel("output.dupMarkers",
                h5("You have the same tumor marker listed more than once, please fix this.", style = "color:red")
              ),
              uiOutput("pbMarkInputs"),
              actionButton("addPbMark", label = "Add Marker",
                           icon = icon('plus'),
                           style = "color: white; background-color: #10699B; border-color: #10699B; margin-top: 20px"),
              actionButton("removePbMark", label = "Remove Last Marker",
                           icon = icon('trash'),
                           style = "color: white; background-color: #10699B; border-color: #10699B; margin-top: 20px")
            ), # end of tumor marker tab
            
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
              textOutput("validpbCanAges"),
              tags$head(tags$style("#validpbCanAges{color: red;}")),
              
              # enter cancers
              uiOutput("pbCanInputs"),
              actionButton("addPbCan", label = "Add Cancer",
                           icon = icon('plus'),
                           style = "color: white; background-color: #10699B; border-color: #10699B; margin-top: 20px"),
              actionButton("removePbCan", label = "Remove Last Cancer",
                           icon = icon('trash'),
                           style = "color: white; background-color: #10699B; border-color: #10699B; margin-top: 20px")
            ), # end of cancers tab
            
            ###### Genes ####
            tabPanel("Genes",
              h3("Gene Testing Results"),
              p("First, specify the panel of genes tested and then enter the test results by type. All data is 
                entered on the first tab and a summary is displayed on the second tab.", 
                style = "margin-bottom:10px"),
              
              # create two tabs, one for entering data and one for displaying a data frame summary
              tabsetPanel(id = "geneTabs",
                
                tabPanel(title = "1. Select Panel & Enter Results",
                  fluidRow(column(width = 6, class = "totwelve",
                       
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
                      
                      # create a tab for each result type to save space
                      tabsetPanel(id = "pbGeneResultTabs",
                      
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
                            actionButton("addPbPLP", label = "Add P/LP Gene",
                                         icon = icon('plus'),
                                         style = "color: white; background-color: #10699B; border-color: #10699B; margin-top: 10px"),
                            actionButton("removePbPLP", label = "Remove Last P/LP Gene",
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
                            actionButton("addPbVUS", label = "Add VUS Gene",
                                         icon = icon('plus'),
                                         style = "color: white; background-color: #10699B; border-color: #10699B; margin-top: 10px"),
                            actionButton("removePbVUS", label = "Remove Last VUS Gene",
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
                            actionButton("addPbBLB", label = "Add B/LB Gene",
                                         icon = icon('plus'),
                                         style = "color: white; background-color: #10699B; border-color: #10699B; margin-top: 10px"),
                            actionButton("removePbBLB", label = "Remove Last BLB Gene",
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
                column(width = 3,
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
                
                column(width = 3,
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
        
        # only show pedigree visualization after pedigree has been initialized with all FDR, aunts, and uncles
        tags$script(
          type = "text/javascript",
          "
            const btn = document.getElementById('visPed');
            btn.addEventListener('click', function(event) {

                // update class
                const columns = document.querySelectorAll('.tosix');
                const columns1 = document.querySelectorAll('.totwelve');
                columns.forEach(function(column) {
                    column.className = 'col-sm-' + 6 + ' tosix';
                });
                columns1.forEach(function(column) {
                    column.className = 'col-sm-' + 12 + ' totwelve';
                });
            })
          "
        ),
        
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
  
  ## proband's age
  validPbAge <- reactive({
    validate(validAge(input$pbAge, input$pbAge))
  })
  output$validPbAge <- renderText({ validPbAge() })
  
  ## proband's surgery ages
  # Oophorectomy age
  validpbOophAge <- reactive({
    validate(validAge(input$pbOophAge, input$pbAge))
  })
  output$validpbOophAge <- renderText({ validpbOophAge() })
  # Mastectomy age
  validpbMastAge <- reactive({
    validate(validAge(input$pbMastAge, input$pbAge))
  })
  output$validpbMastAge <- renderText({ validpbMastAge() })
  # Hysterectomy age
  validpbHystAge <- reactive({
    validate(validAge(input$pbHystAge, input$pbAge))
  })
  output$validpbHystAge <- renderText({ validpbHystAge() })
  
  ## proband's cancer ages
  validpbCanAges <- reactive({
    v.age <- canReactive$df$Age[which(canReactive$df$Cancer != "No cancer selected")]
    validate(unlist(lapply(v.age, validAge, cur.age = input$pbAge))[1])
  })
  output$validpbCanAges <- renderText({ validpbCanAges() })
  
  #### Demographics / Create Pedigree ####
  
  # proband's sex, convert to binary
  pb.Sex <- reactiveVal(NA)
  observeEvent(input$pbSex, {
    if(input$pbSex == "Female"){
      pb.Sex(0)
    } else if(input$pbSex == "Male"){
      pb.Sex(1)
    } else {
      pb.Sex(NA)
    }
  })
  
  # do not allow user to move to other pedTabs if there is not enough information to make the pedigree
  pbMinInfo <- reactiveVal(FALSE)
  observeEvent(list(input$pedID, input$pbSex, input$pbAge, validPbAge()), {
    if(input$pedID != "" & input$pbSex != " " & !is.na(input$pbAge) & is.null(validPbAge())){
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
  onPbDemoTab <- reactiveVal(TRUE)
  observeEvent(input$pedTabs, {
    
    # execute if the previous tab was the proband demographics tab and the current tab is different
    if(onPbDemoTab() & input$pedTabs != "Demographics" & pbMinInfo()){
      
      # initialize new pedigree with proband and parents if no pedigree exists
      if(is.null(PED())){
        PED(initPed(pedigree.id = input$pedID, pb.sex = pb.Sex()))
      }
      # combine proband's mother and father race, ethnicity, and ancestry information
      if(input$pbRaceM != input$pbRaceF){
        pb.rc <- "All_Races"
      } else if(input$pbRaceM == input$pbRaceF){
        pb.rc <- input$pbRaceM
      }
      if(input$pbEthM == input$pbEthF){
        pb.et <- input$pbEthM
      } else if(all(c(input$pbEthM, input$pbEthP) != "Other_Ethnicity")){
        pb.et <- "Hispanic"
      } else {
        pb.et <- "Other_Ethnicity"
      }
      if(input$pbAncAJM | input$pbAncAJF){
        pb.an.aj <- TRUE
      } else {
        pb.an.aj <- FALSE
      }
      if(input$pbAncItM | input$pbAncItF){
        pb.an.it <- TRUE
      } else {
        pb.an.it <- FALSE
      }
      
      # populate proband's demographics data and PedigreeID
      t.ped <- PED()
      t.ped <- popPersonData(tmp.ped = t.ped, is.proband = TRUE, cur.age = input$pbAge, 
                             rc = pb.rc, et = pb.et, an.aj = pb.an.aj, an.it = pb.an.it)
      
      # populate mother's race and Ancestry information
      t.ped <- popPersonData(tmp.ped = t.ped, id = t.ped$MotherID[which(t.ped$isProband == 1)], 
                             rc = input$pbRaceM, et = input$pbEthM, 
                             an.aj = input$pbAncAJM, an.it = input$pbAncItM)
      
      # populate father's race and Ancestry information
      t.ped <- popPersonData(tmp.ped = t.ped, id = t.ped$FatherID[which(t.ped$isProband == 1)], 
                             rc = input$pbRaceF, et = input$pbEthF, 
                             an.aj = input$pbAncAJF, an.it = input$pbAncItF)
      PED(t.ped)
      
      
      
      View(PED())
      
      
      
    }
    
    # update the reactive value to detect if the current tab is the target tab
    if(input$pedTabs == "Demographics"){
      onPbDemoTab(TRUE)
    } else {
      onPbDemoTab(FALSE)
    }
  }, ignoreInit = TRUE)
  
  #### Surgical History ####
  
  # store for prophylactic surgeries
  surgReactive <- reactiveValues(lst = riskmods.inputs.store)
  observeEvent(list(input$pbMast, input$pbMastAge), {
    surgReactive$lst[["riskmod"]][which(names(surgReactive$lst[["riskmod"]]) == "mast")] <- input$pbMast
    surgReactive$lst[["interAge"]][which(names(surgReactive$lst[["interAge"]]) == "mast")] <- input$pbMastAge
  }, ignoreInit = TRUE)
  observeEvent(list(input$pbHyst, input$pbHystAge), {
    surgReactive$lst[["riskmod"]][which(names(surgReactive$lst[["riskmod"]]) == "hyst")] <- input$pbHyst
    surgReactive$lst[["interAge"]][which(names(surgReactive$lst[["interAge"]]) == "hyst")] <- input$pbHystAge
  }, ignoreInit = TRUE)
  observeEvent(list(input$pbOoph, input$pbOophAge), {
    surgReactive$lst[["riskmod"]][which(names(surgReactive$lst[["riskmod"]]) == "ooph")] <- input$pbOoph
    surgReactive$lst[["interAge"]][which(names(surgReactive$lst[["interAge"]]) == "ooph")] <- input$pbOophAge
  }, ignoreInit = TRUE)
  
  ## if a surgery is unchecked, reset the surgery age value
  # Mast
  observeEvent(input$pbMast, {
    if(!input$pbMast){
      updateNumericInput(session, "pbMastAge", value = NA)
    }
  })
  
  # Ooph
  observeEvent(input$pbOoph, {
    if(!input$pbOoph){
      updateNumericInput(session, "pbOophAge", value = NA)
    }
  })
  
  # Hyst
  observeEvent(input$pbHyst, {
    if(!input$pbHyst){
      updateNumericInput(session, "pbHystAge", value = NA)
    }
  })
  
  # add data to pedigree when user navigates off of the tab
  onPbSurgTab <- reactiveVal(FALSE)
  observeEvent(input$pedTabs, {
    if(onPbSurgTab() & input$pedTabs != "Surgical Hx"){
      PED(popPersonData(tmp.ped = PED(), is.proband = TRUE, riskmods.and.ages = surgReactive$lst))
      
      
      View(PED())
      
      
      
    }
    
    # update the reactive value to detect if the current tab is the target tab
    if(input$pedTabs == "Surgical Hx"){
      onPbSurgTab(TRUE)
    } else {
      onPbSurgTab(FALSE)
    }
  }, ignoreInit = TRUE)
  
  
  #### Tumor Markers ####
  
  ##### UI ####
  
  # count number of markers
  pbMarkCnt <- reactiveVal(1)
  observeEvent(input$addPbMark, {
    if(pbMarkCnt() < length(MARKER.TYPES)-1){
      pbMarkCnt(pbMarkCnt()+1)
    }
  })
  observeEvent(input$removePbMark, {
    if(pbMarkCnt() > 0){
      pbMarkCnt(pbMarkCnt()-1)
    }
  })
  
  # tumor marker UI for proband
  output$pbMarkInputs <- renderUI({
    
    # dynamically change columns widths when pedigree is/is not displayed
    col.widths <- ifelse(input$visPed, 6, 3)
    
    lapply(if(pbMarkCnt() > 0){1:pbMarkCnt()}else{1}, function(pbMarkNum){
      fluidRow(
        column(width = col.widths, 
            selectInput(paste0('pbMark', pbMarkNum), h5(paste0('Marker ', pbMarkNum,':')),
                        choices = MARKER.TYPES,
                        width = "65%")
        ),
        conditionalPanel(paste0("input.pbMark", pbMarkNum, " != 'No marker selected'"),
          column(width = col.widths, 
            div(
              selectInput(paste0('pbMarkResult', pbMarkNum), h5("Test Result:"),
                          choices = marker.result.choices,
                          width = "125px"),
              style = "margin-left:-100px",
            )
          )
        )
      )
    })
  })
  
  # check for marker duplicate entries
  dupMarkers <- reactiveVal(FALSE)
  observeEvent(markReactive$df, {
    tms <- markReactive$df$Mark[which(markReactive$df$Mark != "No marker selected")]
    if(length(tms) > 1){
      if(any(table(tms) > 1)){
        dupMarkers(TRUE)
      } else {
        dupMarkers(FALSE)
      }
    }
  }, ignoreInit = TRUE)
  output$dupMarkers <- reactive({ dupMarkers() })
  outputOptions(output, 'dupMarkers', suspendWhenHidden = FALSE)
  
  # store tumor marker names in the order they are entered
  observe(
    lapply(1:pbMarkCnt(), 
           function(mc){
             observeEvent(input[[paste0("pbMark",mc)]], {
               if(input[[paste0("pbMark",mc)]] != "No marker selected"){
                 markReactive$df$Mark[mc] <- input[[paste0("pbMark",mc)]]
               }
             }, ignoreInit = TRUE)
           }
    )
  )
  
  ##### Storage ####
  
  # store proband tumor marker inputs in the order they are entered
  markReactive <- reactiveValues(df = tmark.inputs.store)
  
  # store tumor marker results in the order they are entered
  observe(
    lapply(1:pbMarkCnt(), 
           function(mc){
             observeEvent(input[[paste0("pbMarkResult",mc)]], {
               if(input[[paste0("pbMarkResult",mc)]] != "Not Tested"){
                  markReactive$df$Result[mc] <- input[[paste0("pbMarkResult",mc)]]
               }
             }, ignoreInit = TRUE)
           }
    )
  )
  
  # repopulate tumor marker inputs when a marker is added or deleted and 
  # remove previously selected tumor markers from dropdown choices
  observeEvent(list(input$addPbMark, input$removePbMark), {
    for(mc in 1:pbMarkCnt()){
      if(mc > 1){
        u.choices <- MARKER.TYPES[which(!MARKER.TYPES %in% setdiff(markReactive$df$Mark[1:(mc-1)], "No marker selected"))]
      } else {
        u.choices <- MARKER.TYPES
      }
      updateSelectInput(session, paste0("pbMark",mc), selected = markReactive$df$Mark[mc], choices = u.choices)
      updateSelectInput(session, paste0("pbMarkResult",mc), selected = markReactive$df$Result[mc])
    }
  }, ignoreInit = TRUE)
  
  # remove marker from temporary storage when the input is deleted
  observeEvent(input$removePbMark, {
    markReactive$df$Mark[pbMarkCnt()+1] <- "No marker selected"
    markReactive$df$Result[pbMarkCnt()+1] <- "Not Tested"
  }, ignoreInit = TRUE)
  
  # add data to pedigree when user navigates off of the tab
  onPbMarkerTab <- reactiveVal(FALSE)
  observeEvent(input$pedTabs, {
    if(onPbMarkerTab() & input$pedTabs != "Tumor Markers"){
      PED(popPersonData(tmp.ped = PED(), is.proband = TRUE, t.markers = markReactive$df))
      
      
      
      View(PED())
      
      
      
    }
    
    # update the reactive value to detect if the current tab is the target tab
    if(input$pedTabs == "Tumor Markers"){
      onPbMarkerTab(TRUE)
    } else {
      onPbMarkerTab(FALSE)
    }
  }, ignoreInit = TRUE)
  
  
  #### Cancer History ####
  
  ##### UI ####
  
  # count number of cancers
  pbCanCnt <- reactiveVal(1)
  observeEvent(input$addPbCan, {
    pbCanCnt(pbCanCnt()+1)
  })
  observeEvent(input$removePbCan, {
    if(pbCanCnt() > 0){
      pbCanCnt(pbCanCnt()-1)
    }
  })
  
  # cancer history UI for proband
  output$pbCanInputs <- renderUI({
    
    # dynamically change columns widths when pedigree is/is not displayed
    col.widths <- ifelse(input$visPed, 6, 3)
    
    lapply(if(pbCanCnt() > 0){1:pbCanCnt()}else{1}, function(pbCanNum){
      fluidRow(
        column(width = col.widths, 
          selectInput(paste0('pbCan', pbCanNum), h5(paste0('Cancer ', pbCanNum,':')),
                      choices = CANCER.CHOICES$long,
                      width = "200px"),
          conditionalPanel(paste0('input.pbCan', pbCanNum, " == 'Other'"),
            fluidRow(
              column(6, h5("Other cancer:", style = "margin-left:25px")),
              column(6, 
                div(selectizeInput(paste0('pbCanOther', pbCanNum), label = NULL,
                                   choices = c("", non.pp.cancers), selected = "",
                                   multiple = FALSE, options = list(create=TRUE),
                                   width = "225px"),
                    style = "margin-left:-25px;margin-right:-100px"
                )
              )
            )
          )
        ),
        conditionalPanel(paste0("input.pbCan", pbCanNum, " != 'No cancer selected'"),
          column(width = col.widths,
            div(numericInput(paste0('pbCanAge', pbCanNum), h5("Diagnosis Age:"),
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
  
  # store proband cancer inputs in the order they are entered
  canReactive <- reactiveValues(df = cancer.inputs.store)
  
  # store cancer names in the order they are entered
  observe(
    lapply(1:pbCanCnt(), 
           function(cc){
             observeEvent(input[[paste0("pbCan",cc)]], {
               canReactive$df$Cancer[cc] <- input[[paste0("pbCan",cc)]]
             }, ignoreInit = TRUE)
           }
    )
  )
  
  # store cancer ages in the order they are entered
  observe(
    lapply(1:pbCanCnt(), 
           function(cc){
             observeEvent(input[[paste0("pbCanAge",cc)]], {
               canReactive$df$Age[cc] <- input[[paste0("pbCanAge",cc)]]
             }, ignoreInit = TRUE)
           }
    )
  )
  
  # store other cancer names in the order they are entered
  observe(
    lapply(1:pbCanCnt(), 
           function(cc){
             observeEvent(input[[paste0("pbCanOther",cc)]], {
               canReactive$df$Other[cc] <- input[[paste0("pbCanOther",cc)]]
             }, ignoreInit = TRUE)
           }
    )
  )
  
  # repopulate cancer inputs when a cancer is added or deleted and 
  # remove previously selected cancers from dropdown choices
  observeEvent(list(input$addPbCan, input$removePbCan), {
    for(cc in 1:pbCanCnt()){
      if(cc > 1){
        u.choices <- CANCER.CHOICES$long[which(!CANCER.CHOICES$long %in% setdiff(canReactive$df$Cancer[1:(cc-1)], c("Other", "No cancer selected")))]
      } else {
        u.choices <- CANCER.CHOICES$long
      }
      updateSelectInput(session, paste0("pbCan",cc), selected = canReactive$df$Cancer[cc], choices = u.choices)
      updateSelectInput(session, paste0("pbCanAge",cc), selected = canReactive$df$Age[cc])
      updateSelectInput(session, paste0("pbCanOther",cc), selected = canReactive$df$Other[cc])
    }
  }, ignoreInit = TRUE)
  
  # remove cancer from temporary storage which the input is deleted
  observeEvent(input$removePbCan, {
    canReactive$df$Cancer[pbCanCnt()+1] <- "No cancer selected"
    canReactive$df$Age[pbCanCnt()+1]    <- NA
    canReactive$df$Other[pbCanCnt()+1]  <- ""
  }, ignoreInit = TRUE)
  
  # add data to pedigree when user navigates off of the tab
  onPbCanTab <- reactiveVal(FALSE)
  observeEvent(input$pedTabs, {
    if(onPbCanTab() & input$pedTabs != "Cancer Hx"){
      PED(popPersonData(tmp.ped = PED(), is.proband = TRUE, cancers.and.ages = canReactive$df))
      

      
      View(PED())
      
      
      
    }
    
    # update the reactive value to detect if the current tab is the target tab
    if(input$pedTabs == "Cancer Hx"){
      onPbCanTab(TRUE)
    } else {
      onPbCanTab(FALSE)
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
  pbPLPCnt <- reactiveVal(1)
  observeEvent(input$addPbPLP, {
    pbPLPCnt(pbPLPCnt()+1)
  })
  observeEvent(input$removePbPLP, {
    if(pbPLPCnt() > 0){
      pbPLPCnt(pbPLPCnt()-1)
    }
  })
  
  # VUS
  pbVUSCnt <- reactiveVal(1)
  observeEvent(input$addPbVUS, {
    pbVUSCnt(pbVUSCnt()+1)
  })
  observeEvent(input$removePbVUS, {
    if(pbVUSCnt() > 0){
      pbVUSCnt(pbVUSCnt()-1)
    }
  })
  
  # BLB
  pbBLBCnt <- reactiveVal(1)
  observeEvent(input$addPbBLB, {
    pbBLBCnt(pbBLBCnt()+1)
  })
  observeEvent(input$removePbBLB, {
    if(pbBLBCnt() > 0){
      pbBLBCnt(pbBLBCnt()-1)
    }
  })
  
  ## dynamic data inputs for each gene by result type
  # PLP
  output$plpGeneInfo <- renderUI({
      lapply(if(pbPLPCnt() > 0){1:pbPLPCnt()}else{1}, function(plpNum){
        fluidRow(
          column(width = 3,
                 div(style = "margin-left:0px;margin-top:0px;margin-bottom:-10px",
                     selectInput(paste0('pbPLPGene', plpNum),
                                 label = NULL, 
                                 choices = c("", geneReactive$panel.genes),
                                 selected = "",
                                 width = "125px")
                     
                 )
          ),
          conditionalPanel(paste0("input.pbPLPGene", plpNum, " != ''"),
            column(width = 3,
                   div(style = "margin-left:-25px;margin-right:0px;margin-top:0px;margin-bottom:-10px",
                       selectizeInput(paste0('pbPLPVarInfo', plpNum), 
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
                       selectizeInput(paste0('pbPLPProtInfo', plpNum),
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
                       selectInput(paste0('pbPLPZygInfo', plpNum),
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
    lapply(if(pbVUSCnt() > 0){1:pbVUSCnt()}else{1}, function(vusNum){
      fluidRow(
        column(width = 3,
               div(style = "margin-left:0px;margin-top:0px;margin-bottom:-10px",
                   selectInput(paste0('pbVUSGene', vusNum),
                               label = NULL, 
                               choices = c("", geneReactive$panel.genes),
                               selected = "",
                               width = "125px")
                   
               )
        ),
        conditionalPanel(paste0("input.pbVUSGene", vusNum, " != ''"),
                         column(width = 3,
                                div(style = "margin-left:-25px;margin-right:0px;margin-top:0px;margin-bottom:-10px",
                                    selectizeInput(paste0('pbVUSVarInfo', vusNum), 
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
                                    selectizeInput(paste0('pbVUSProtInfo', vusNum),
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
                                    selectInput(paste0('pbVUSZygInfo', vusNum),
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
    lapply(if(pbBLBCnt() > 0){1:pbBLBCnt()}else{1}, function(blbNum){
      fluidRow(
        column(width = 3,
               div(style = "margin-left:0px;margin-top:0px;margin-bottom:-10px",
                   selectInput(paste0('pbBLBGene', blbNum),
                               label = NULL, 
                               choices = c("", geneReactive$panel.genes),
                               selected = "",
                               width = "125px")
                   
               )
        ),
        conditionalPanel(paste0("input.pbBLBGene", blbNum, " != ''"),
                         column(width = 3,
                                div(style = "margin-left:-25px;margin-right:0px;margin-top:0px;margin-bottom:-10px",
                                    selectizeInput(paste0('pbBLBVarInfo', blbNum), 
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
                                    selectizeInput(paste0('pbBLBProtInfo', blbNum),
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
                                    selectInput(paste0('pbBLBZygInfo', blbNum),
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
    gs <- geneReactive$plp.df$Gene[which(geneReactive$plp.df$Gene != "No cancer selected")]
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
    gs <- geneReactive$vus.df$Gene[which(geneReactive$vus.df$Gene != "No cancer selected")]
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
    gs <- geneReactive$blb.df$Gene[which(geneReactive$blb.df$Gene != "No cancer selected")]
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
    lapply(1:pbPLPCnt(), 
           function(plpc){
             observeEvent(input[[paste0("pbPLPGene",plpc)]], {
               if(input[[paste0("pbPLPGene",plpc)]] != ""){
                 geneReactive$plp.df$Gene[plpc] <- input[[paste0("pbPLPGene",plpc)]]
               }
             }, ignoreInit = TRUE)
           }
    )
  )
  
  # VUS
  observe(
    lapply(1:pbVUSCnt(), 
           function(vusc){
             observeEvent(input[[paste0("pbVUSGene",vusc)]], {
               if(input[[paste0("pbVUSGene",vusc)]] != ""){
                 geneReactive$vus.df$Gene[vusc] <- input[[paste0("pbVUSGene",vusc)]]
               }
             }, ignoreInit = TRUE)
           }
    )
  )
  
  # BLB
  observe(
    lapply(1:pbBLBCnt(), 
           function(blbc){
             observeEvent(input[[paste0("pbBLBGene",blbc)]], {
               if(input[[paste0("pbBLBGene",blbc)]] != ""){
                 geneReactive$blb.df$Gene[blbc] <- input[[paste0("pbBLBGene",blbc)]]
               }
             }, ignoreInit = TRUE)
           }
    )
  )
  
  ## store variants in the order they are entered
  # PLP
  observe(
    lapply(1:pbPLPCnt(), 
           function(plpc){
             observeEvent(input[[paste0("pbPLPVarInfo",plpc)]], {
               geneReactive$plp.df$Variants[plpc] <- paste0(input[[paste0("pbPLPVarInfo",plpc)]], collapse = ", ")
             }, ignoreInit = TRUE, ignoreNULL = FALSE)
           }
    )
  )
  
  # VUS
  observe(
    lapply(1:pbVUSCnt(), 
           function(vusc){
             observeEvent(input[[paste0("pbVUSVarInfo",vusc)]], {
               geneReactive$vus.df$Variants[vusc] <- paste0(input[[paste0("pbVUSVarInfo",vusc)]], collapse = ", ")
             }, ignoreInit = TRUE, ignoreNULL = FALSE)
           }
    )
  )
  
  # BLB
  observe(
    lapply(1:pbBLBCnt(), 
           function(blbc){
             observeEvent(input[[paste0("pbBLBVarInfo",blbc)]], {
               geneReactive$blb.df$Variants[blbc] <- paste0(input[[paste0("pbBLBVarInfo",blbc)]], collapse = ", ")
             }, ignoreInit = TRUE, ignoreNULL = FALSE)
           }
    )
  )
  
  ## store proteins in the order they are entered
  # PLP
  observe(
    lapply(1:pbPLPCnt(), 
           function(plpc){
             observeEvent(input[[paste0("pbPLPProtInfo",plpc)]], {
               geneReactive$plp.df$Proteins[plpc] <- paste0(input[[paste0("pbPLPProtInfo",plpc)]], collapse = ", ")
             }, ignoreInit = TRUE, ignoreNULL = FALSE)
           }
    )
  )
  
  # VUS
  observe(
    lapply(1:pbVUSCnt(), 
           function(vusc){
             observeEvent(input[[paste0("pbVUSProtInfo",vusc)]], {
               geneReactive$vus.df$Proteins[vusc] <- paste0(input[[paste0("pbVUSProtInfo",vusc)]], collapse = ", ")
             }, ignoreInit = TRUE, ignoreNULL = FALSE)
           }
    )
  )
  
  # BLB
  observe(
    lapply(1:pbBLBCnt(), 
           function(blbc){
             observeEvent(input[[paste0("pbBLBProtInfo",blbc)]], {
               geneReactive$blb.df$Proteins[blbc] <- paste0(input[[paste0("pbBLBProtInfo",blbc)]], collapse = ", ")
             }, ignoreInit = TRUE, ignoreNULL = FALSE)
           }
    )
  )
  
  ## store homo/hetero in the order they are entered
  # PLP
  observe(
    lapply(1:pbPLPCnt(), 
           function(plpc){
             observeEvent(input[[paste0("pbPLPZygInfo",plpc)]], {
               geneReactive$plp.df$Zygosity[plpc] <- input[[paste0("pbPLPZygInfo",plpc)]]
             }, ignoreInit = TRUE)
           }
    )
  )
  
  # VUS
  observe(
    lapply(1:pbVUSCnt(), 
           function(vusc){
             observeEvent(input[[paste0("pbVUSZygInfo",vusc)]], {
               geneReactive$vus.df$Zygosity[vusc] <- input[[paste0("pbVUSZygInfo",vusc)]]
             }, ignoreInit = TRUE)
           }
    )
  )
  
  # BLB
  observe(
    lapply(1:pbBLBCnt(), 
           function(blbc){
             observeEvent(input[[paste0("pbBLBZygInfo",blbc)]], {
               geneReactive$blb.df$Zygosity[blbc] <- input[[paste0("pbBLBZygInfo",blbc)]]
             }, ignoreInit = TRUE)
           }
    )
  )
  
  ## repopulate gene inputs when a gene is added or deleted and remove previously selected genes from dropdown choices
  # PLP
  observeEvent(list(input$addPbPLP, input$removePbPLP), {
    if(pbPLPCnt() > 0){
      for(plpc in 1:pbPLPCnt()){
        
        # genes
        if(plpc > 1){
          u.choices <- geneReactive$panel.genes[which(!geneReactive$panel.genes %in% 
                                                        setdiff(geneReactive$plp.df$Gene[1:(plpc-1)], ""))]
        } else {
          u.choices <- geneReactive$panel.genes
        }
        updateSelectInput(session, paste0("pbPLPGene",plpc), selected = geneReactive$plp.df$Gene[plpc], choices = u.choices)
        
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

        updateSelectInput(session, paste0("pbPLPVarInfo",plpc), selected = tmp.var.prot[[1]], choices = tmp.var.prot[[1]])
        updateSelectInput(session, paste0("pbPLPProtInfo",plpc), selected = tmp.var.prot[[2]], choices = tmp.var.prot[[2]])
        
        # homo/hetero
        updateSelectInput(session, paste0("pbPLPZygInfo",plpc), selected = geneReactive$plp.df$Zygosity[plpc])
      }
    }
  }, ignoreInit = TRUE)
  
  # VUS
  observeEvent(list(input$addPbVUS, input$removePbVUS), {
    if(pbVUSCnt() > 0){
      for(vusc in 1:pbVUSCnt()){
        
        # genes
        if(vusc > 1){
          u.choices <- geneReactive$panel.genes[which(!geneReactive$panel.genes %in% 
                                                        setdiff(geneReactive$vus.df$Gene[1:(vusc-1)], ""))]
        } else {
          u.choices <- geneReactive$panel.genes
        }
        updateSelectInput(session, paste0("pbVUSGene",vusc), selected = geneReactive$vus.df$Gene[vusc], choices = u.choices)
        
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
        
        updateSelectInput(session, paste0("pbVUSVarInfo",vusc), selected = tmp.var.prot[[1]], choices = tmp.var.prot[[1]])
        updateSelectInput(session, paste0("pbVUSProtInfo",vusc), selected = tmp.var.prot[[2]], choices = tmp.var.prot[[2]])
        
        # homo/hetero
        updateSelectInput(session, paste0("pbVUSZygInfo",vusc), selected = geneReactive$vus.df$Zygosity[vusc])
      }
    }
  }, ignoreInit = TRUE)
  
  # BLB
  observeEvent(list(input$addPbBLB, input$removePbBLB), {
    if(pbBLBCnt() > 0){
      for(blbc in 1:pbBLBCnt()){
        
        # genes
        if(blbc > 1){
          u.choices <- geneReactive$panel.genes[which(!geneReactive$panel.genes %in% 
                                                        setdiff(geneReactive$blb.df$Gene[1:(blbc-1)], ""))]
        } else {
          u.choices <- geneReactive$panel.genes
        }
        updateSelectInput(session, paste0("pbBLBGene",blbc), selected = geneReactive$blb.df$Gene[blbc], choices = u.choices)
        
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
        
        updateSelectInput(session, paste0("pbBLBVarInfo",blbc), selected = tmp.var.prot[[1]], choices = tmp.var.prot[[1]])
        updateSelectInput(session, paste0("pbBLBProtInfo",blbc), selected = tmp.var.prot[[2]], choices = tmp.var.prot[[2]])
        
        # homo/hetero
        updateSelectInput(session, paste0("pbBLBZygInfo",blbc), selected = geneReactive$blb.df$Zygosity[blbc])
      }
    }
  }, ignoreInit = TRUE)
  
  ## remove gene from temporary storage when the input is deleted
  # PLP
  observeEvent(input$removePbPLP, {
    geneReactive$plp.df$Gene[pbPLPCnt()+1] <- ""
    geneReactive$plp.df$Variants[pbPLPCnt()+1] <- ""
    geneReactive$plp.df$Proteins[pbPLPCnt()+1]  <- ""
    geneReactive$plp.df$Zygosity[pbPLPCnt()+1]  <- "Unk"
  }, ignoreInit = TRUE)
  
  # VUS
  observeEvent(input$removePbVUS, {
    geneReactive$vus.df$Gene[pbVUSCnt()+1] <- ""
    geneReactive$vus.df$Variants[pbVUSCnt()+1] <- ""
    geneReactive$vus.df$Proteins[pbVUSCnt()+1]  <- ""
    geneReactive$vus.df$Zygosity[pbVUSCnt()+1]  <- "Unk"
  }, ignoreInit = TRUE)
  
  # BLB
  observeEvent(input$removePbBLB, {
    geneReactive$blb.df$Gene[pbBLBCnt()+1] <- ""
    geneReactive$blb.df$Variants[pbBLBCnt()+1] <- ""
    geneReactive$blb.df$Proteins[pbBLBCnt()+1]  <- ""
    geneReactive$blb.df$Zygosity[pbBLBCnt()+1]  <- "Unk"
  }, ignoreInit = TRUE)
  
  ### store variant/protein/homo-hetero information by result type
  # PLP
  observe(
    if(length(input$pbPLPGenes) > 0 & !(all(input$pbPLPGenes == ""))){
      lapply(1:length(input$pbPLPGenes),
             function(plpNum){
               plp.g.name <- input$pbPLPGenes[plpNum]
               
               # variants: store variants as one string
               observeEvent(input[[paste0('pbPLPVarInfo', plpNum)]], {
                 if(is.null(input[[paste0('pbPLPVarInfo', plpNum)]])){
                   tmp.var <- ""
                 } else {
                   tmp.var <- input[[paste0('pbPLPVarInfo', plpNum)]]
                 }
                 geneReactive$plp.df$Variants[which(geneReactive$plp.df$Gene == plp.g.name & geneReactive$plp.df$Result == 1)] <- 
                   paste0(unique(tmp.var), collapse = ", ")
               }, ignoreInit = TRUE, ignoreNULL = FALSE)
               
               # proteins: store proteins as one string
               observeEvent(input[[paste0('pbPLPProtInfo', plpNum)]], {
                 if(is.null(input[[paste0('pbPLPProtInfo', plpNum)]])){
                   tmp.prot <- ""
                 } else {
                   tmp.prot <- input[[paste0('pbPLPProtInfo', plpNum)]]
                 }
                 geneReactive$plp.df$Proteins[which(geneReactive$plp.df$Gene == plp.g.name & geneReactive$plp.df$Result == 1)] <- 
                   paste0(unique(tmp.prot), collapse = ", ")
               }, ignoreInit = TRUE, ignoreNULL = FALSE)
               
               # homo/hetero
               observeEvent(input[[paste0('pbPLPZygInfo', plpNum)]], {
                 geneReactive$plp.df$Zygosity[which(geneReactive$plp.df$Gene == plp.g.name & geneReactive$plp.df$Result == 1)] <- 
                   paste0(input[[paste0('pbPLPZygInfo', plpNum)]], collapse = ", ")
               }, ignoreInit = TRUE, ignoreNULL = FALSE)
             })
    }
  )
  
  # VUS
  observe(
    if(length(input$pbVUSGenes) > 0 & !(all(input$pbVUSGenes == ""))){
      lapply(1:length(input$pbVUSGenes),
             function(vusNum){
               vus.g.name <- input$pbVUSGenes[vusNum]
               
               # variants: store variants as one string
               observeEvent(input[[paste0('pbVUSVarInfo', vusNum)]], {
                 if(is.null(input[[paste0('pbVUSVarInfo', vusNum)]])){
                   tmp.var <- ""
                 } else {
                   tmp.var <- input[[paste0('pbVUSVarInfo', vusNum)]]
                 }
                 geneReactive$vus.df$Variants[which(geneReactive$vus.df$Gene == vus.g.name & geneReactive$vus.df$Result == 1)] <- 
                   paste0(unique(tmp.var), collapse = ", ")
               }, ignoreInit = TRUE, ignoreNULL = FALSE)
               
               # proteins: store proteins as one string
               observeEvent(input[[paste0('pbVUSProtInfo', vusNum)]], {
                 if(is.null(input[[paste0('pbVUSProtInfo', vusNum)]])){
                   tmp.prot <- ""
                 } else {
                   tmp.prot <- input[[paste0('pbVUSProtInfo', vusNum)]]
                 }
                 geneReactive$vus.df$Proteins[which(geneReactive$vus.df$Gene == vus.g.name & geneReactive$vus.df$Result == 1)] <- 
                   paste0(unique(tmp.prot), collapse = ", ")
               }, ignoreInit = TRUE, ignoreNULL = FALSE)
               
               # homo/hetero
               observeEvent(input[[paste0('pbVUSZygInfo', vusNum)]], {
                 geneReactive$vus.df$Zygosity[which(geneReactive$vus.df$Gene == vus.g.name & geneReactive$vus.df$Result == 1)] <- 
                   paste0(input[[paste0('pbVUSZygInfo', vusNum)]], collapse = ", ")
               }, ignoreInit = TRUE, ignoreNULL = FALSE)
             })
    }
  )
  
  # BLB
  observe(
    if(length(input$pbBLBGenes) > 0 & !(all(input$pbBLBGenes == ""))){
      lapply(1:length(input$pbBLBGenes),
             function(blbNum){
               blb.g.name <- input$pbBLBGenes[blbNum]
               
               # variants: store variants as one string
               observeEvent(input[[paste0('pbBLBVarInfo', blbNum)]], {
                 if(is.null(input[[paste0('pbBLBVarInfo', blbNum)]])){
                   tmp.var <- ""
                 } else {
                   tmp.var <- input[[paste0('pbBLBVarInfo', blbNum)]]
                 }
                 geneReactive$blb.df$Variants[which(geneReactive$blb.df$Gene == blb.g.name & geneReactive$blb.df$Result == 1)] <- 
                   paste0(unique(tmp.var), collapse = ", ")
               }, ignoreInit = TRUE, ignoreNULL = FALSE)
               
               # proteins: store proteins as one string
               observeEvent(input[[paste0('pbBLBProtInfo', blbNum)]], {
                 if(is.null(input[[paste0('pbBLBProtInfo', blbNum)]])){
                   tmp.prot <- ""
                 } else {
                   tmp.prot <- input[[paste0('pbBLBProtInfo', blbNum)]]
                 }
                 geneReactive$blb.df$Proteins[which(geneReactive$blb.df$Gene == blb.g.name & geneReactive$blb.df$Result == 1)] <- 
                   paste0(unique(tmp.prot), collapse = ", ")
               }, ignoreInit = TRUE, ignoreNULL = FALSE)
               
               # homo/hetero
               observeEvent(input[[paste0('pbBLBZygInfo', blbNum)]], {
                 geneReactive$blb.df$Zygosity[which(geneReactive$blb.df$Gene == blb.g.name & geneReactive$blb.df$Result == 1)] <- 
                   paste0(input[[paste0('pbBLBZygInfo', blbNum)]], collapse = ", ")
               }, ignoreInit = TRUE, ignoreNULL = FALSE)
             })
    }
  )
  
  # add data to pedigree when user navigates off of the tab
  onPbGeneTab <- reactiveVal(FALSE)
  observeEvent(input$pedTabs, {
    if(onPbGeneTab() & input$pedTabs != "Genes"){
      PED(popPersonData(tmp.ped = PED(), is.proband = TRUE, gene.results = panelSum()))
      
      
      
      View(PED())
      
      
      
    }
    
    # update the reactive value to detect if the current tab is the target tab
    if(input$pedTabs == "Genes"){
      onPbGeneTab(TRUE)
    } else {
      onPbGeneTab(FALSE)
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
    
    
    
    View(PED())
    
    
    
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
  
  #### PanelPRO ####
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
