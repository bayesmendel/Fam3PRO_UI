#### Cancer Hx ####

# add/delete/edit a cancer in a relative's cancer history
canUI <- function(id, rel, vals, sex){
  
  # reserve a local namespace for cancer hx
  ns <- NS(id)
  
  # if initial values are provided feed them to the inputs, otherwise use initial values
  if(!is.null(vals)){
    can <- vals$can
    age <- vals$age
    other <- ifelse(vals$other == "UnkType", "Unknown/Not Listed", vals$other)
    cbc <- vals$cbc
    cbcAge <- vals$cbcAge
  } else {
    can <- "No cancer selected"
    age <- NA
    other <- "Unknown/Not Listed"
    cbc <- "No"
    cbcAge <- NA
  }
  
  # filter possible cancers and "other" cancers by sex
  if(sex == "Male"){
    can.choices <- CANCER.CHOICES$long[which(!CANCER.CHOICES$long %in% FEMALE.CANCERS)]
    other.choices <- OTHER.CANCER.CHOICES[which(!OTHER.CANCER.CHOICES %in% FEMALE.CANCERS)]
  } else if(sex == "Female"){
    can.choices <- CANCER.CHOICES$long[which(!CANCER.CHOICES$long %in% MALE.CANCERS)]
    other.choices <- OTHER.CANCER.CHOICES[which(!OTHER.CANCER.CHOICES %in% MALE.CANCERS)]
  }
  
  
  # UI
  tags$div(id = paste0("canSubContainer", id),
    tagList(
      
      # only show module's inputs for the currently selected relative
      conditionalPanel(paste0("input.relSelect == '", rel, "'"),
                       
        # top row: cancer name, cancer age, and delete module button
        fluidRow(
          
          # cancer name
          column(width = 6, 
            selectInput(inputId = ns("Can"), 
                        label = h5("Cancer:"),
                        choices = can.choices,
                        selected = can,
                        width = "200px")
          ),
          
          # cancer age
          column(width = 4,
            conditionalPanel(sprintf("input['%s'] != 'No cancer selected'", ns("Can")),
              div(numericInput(inputId = ns("CanAge"), 
                               label = h5("Diagnosis Age:"),
                               min = min.age, max = max.age, step = 1, value = age,
                               width = "100px"),
                  style = "margin-left:-75px"
              ),
              
              # issue warning if the cancer age is not valid
              textOutput(ns("valCanAge")),
              tags$head(tags$style(paste0("#",ns("valCanAge"),"{color: red;margin-left:-250px}")))
            )
          ),
          
          # delete button
          column(width = 2,
            actionButton(inputId = ns("removeCan"),
                         label = NULL,
                         icon = icon('trash'),
                         style = "color: #FF2800; background-color: white; border-color: grey; margin-left:-150px; margin-top:40px")
          )
        ),
        
        # if "other" cancer selected above, show the selectInput to enter the name of the OTHER cancer
        conditionalPanel(sprintf("input['%s'] == 'Other'", ns("Can")),
          fluidRow(
             column(5, h5("Other cancer:", style = "margin-left:25px")),
             column(7, 
              div(selectizeInput(inputId = ns("CanOther"), 
                                 label = NULL,
                                 choices = other.choices, 
                                 selected = other,
                                 multiple = FALSE, 
                                 options = list(create=TRUE),
                                 width = "225px"),
                  style = "margin-left:-125px;"
              )
            )
          )
        ), # end of conditionalPanel for other cancers
        
        # if breast cancer was selected above, ask if person had CBC and the CBC diagnosis age
        conditionalPanel(sprintf("input['%s'] == 'Breast'", ns("Can")),
          fluidRow(
            h5("Also had contralateral breast cancer?", 
               style = "margin-left:40px"),
            
            # indicate if had CBC
            column(3, 
              div(selectInput(inputId = ns("CBC"),
                              label = NULL,
                              selected = cbc,
                              choices = c("No", "Yes"),
                              width = "100px"),
                  style = "margin-left:25px"
              )
            ),
            
            # if CBC indicated, provide input for the CBC diagnosis age
            conditionalPanel(sprintf("input['%s'] == 'Yes'", ns("CBC")),
              column(width = 3,
                h5("Diagnosis Age:",
                   style = "margin-left:0px") 
              ),
              column(width = 3,
                  div(numericInput(inputId = ns("CBCAge"), 
                                   label = NULL,
                                   min = min.age, max = max.age, step = 1, value = cbcAge,
                                   width = "100px"),
                      style = "margin-left:-40px"
                  ),
                  
                  # issue warning if the cancer age is not valid
                  textOutput(ns("valCBCAge")),
                  tags$head(tags$style(paste0("#",ns("valCBCAge"),"{color: red;margin-left:-250px}")))
              )
            ) # end of conditionalPanel for CBC age 
          ) # end of fluidRow for CBC
        ) # end of conditionalPanel for contralateral breast cancer
      ) # end of conditionalPanel for person's inputs
    ) # end of tagList
  ) # end of div
}

# display the age validation message for the cancer age in the canUI module
validateCanAgeServer <- function(id, in.age, cur.age) {
  moduleServer(
    id,
    function(input, output, session){
      output$valCanAge <- renderText(shiny::validate(validateCanAge(in.age, cur.age)))
    }
  )
}

# display the age validation message for the CBC age in the canUI module
validateCBCAgeServer <- function(id, can, cbc.age, bc.age, cur.age) {
  moduleServer(
    id,
    function(input, output, session){
      output$valCBCAge <- renderText(shiny::validate(validateCBCAge(can, cbc.age, bc.age, cur.age)))
    }
  )
}


#### Genes ####

# header for entering gene variants, re-used across PLP, VUS, and BLB gene tabs
geneHeaderUI <- function(){
  tagList(
    fluidRow(
      column(width = 3,
        h5(HTML("<b>Gene</b>"), style = "margin-left:0px;margin-bottom:10px;margin-top:0px")
      ),
      column(width = 3, 
        h5(HTML("<b>Nucleotide</b>"), style = "margin-left:-25px;margin-bottom:10px;margin-top:0px")
      ),
      column(width = 3,
        h5(HTML("<b>Protein</b>"), style = "margin-left:-25px;margin-bottom:10px;margin-top:0px")
      ),
      column(width = 3,
        h5(HTML("<b>Zygosity</b>"), style = "margin-left:-25px;margin-bottom:10px;margin-top:0px")
      )
    )
  )
}

# add/delete/display a relative's panels
panelUI <- function(id, rel, panelName){
  
  # reserve local namespace for gene results
  ns <- NS(id)
  
  tags$div(id = paste0(id, "Cont"),
    tagList(
       
      # only show module's inputs for the currently selected relative
      conditionalPanel(paste0("input.relSelect == '", rel, "'"),
        fluidRow(
          column(offset = 1, width = 5,
            h5(panelName)
          ),
          column(width = 2,
            actionButton(inputId = ns("removePanel"),
                         label = NULL,
                         icon = icon('trash'),
                         style = "color: #FF2800; background-color: white; border-color: grey; margin-left:-20px; margin-top:0px")
          )
        )
      )
    )
  )
}

# add/delete/edit gene variants for a specific relative and a panel
geneUI <- function(id, rel, panelName, panelGenes, vals){
  
  # reserve local namespace for gene results
  ns <- NS(id)
  
  # if initial values are provided feed them to the inputs, otherwise use initial values
  if(!is.null(vals)){
    gene <- vals$gene
    nuc <- vals$nuc
    prot <- vals$prot
    zyg <- vals$zyg
  } else {
    gene <- nuc <- prot <- ""
    zyg <- "Unk"
  }
  
  tags$div(id = paste0(id, "Cont"),
    tagList(
      
      # only show module's inputs for the currently selected relative and panel
      conditionalPanel(paste0("input.relSelect == '", rel, "' & input.editPanel == '", panelName,"'"),
        
        fluidRow(
          column(width = 3,
            div(style = "margin-left:0px;margin-top:0px;margin-bottom:-10px",
                selectInput(ns('Gene'),
                            label = NULL, 
                            choices = c("", panelGenes),
                            selected = gene,
                            width = "125px")
            )
          ),
          conditionalPanel(sprintf("input['%s'] != ''", ns('Gene')),
            column(width = 3,
              div(style = "margin-left:-25px;margin-right:0px;margin-top:0px;margin-bottom:-10px",
                  selectizeInput(ns('NucInfo'), 
                                 label = NULL,
                                 choices = "", 
                                 selected = nuc,
                                 multiple = FALSE, 
                                 options = list(create=TRUE),
                                 width = "130px")
              )
            ),
            column(width = 3,
              div(style = "margin-left:-25px;margin-right:0px;margin-top:0px;margin-bottom:-10px",
                  selectizeInput(ns('ProtInfo'),
                                 label = NULL,
                                 choices = "",
                                 selected = prot,
                                 multiple = FALSE,
                                 options = list(create=TRUE),
                                 width = "130px")
              )
            ),
            column(width = 2,
              div(style = "margin-left:-25px;margin-right:0px;margin-top:0px;margin-bottom:-10px",
                  selectInput(ns('ZygInfo'),
                              label = NULL,
                              choices = zyg.choices,
                              selected = zyg,
                              width = "85px")
              )
            )
          ), # end of conditionalPanel for displaying nucleotide, protein, and zygous inputs
          
          # insert spacer between gene and delete button if no gene is selected
          conditionalPanel(sprintf("input['%s'] == ''", ns('Gene')),
            column(width = 8)
          ),
          
          # delete button
          column(width = 1,
            actionButton(inputId = ns("removeGene"),
                         label = NULL,
                         icon = icon('trash'),
                         style = "color: #FF2800; background-color: white; border-color: grey; margin-left:-20px; margin-top:0px; margin-right:0px")
          )
        ) # end of fluidRow for all inputs
      ) # end of conditionalPanel for relative's panel data
    ) # end of tagList
  ) # end of tags$div
}

#### Module Memory Management ####

#' Removes shiny inputs from memory
#' 
#' @param id string, input name to remove from memory
#' @param .input the shiny master input list
#' 
#' @details from https://www.r-bloggers.com/2020/02/shiny-add-removing-modules-dynamically/
remove_shiny_inputs <- function(id, .input) {
  invisible(
    lapply(grep(id, names(.input), value = TRUE), function(i) {
      .subset2(.input, "impl")$.values$remove(i)
    })
  )
}













