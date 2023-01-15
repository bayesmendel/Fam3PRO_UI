#### Cancer Hx ####

# module to enter data for one cancer
canUI <- function(id, rel, cancer = NULL, age = NULL, other = NULL){
  
  # reserve a local namespace for cancer hx
  ns <- NS(id)
  
  # initial input values
  can.select <- "No cancer selected"
  can.age <- NA
  can.other <- "Unknown/Not Listed"
  if(!is.null(cancer)){
    can.select <- cancer
  } 
  if(!is.null(age)){
    can.age <- age
  }
  if(!is.null(other)){
    can.other <- other
  }
  
  # UI
  tags$div(id = paste0("canSubContainer", id),
    tagList(
      
      # only show module's inputs for the currently selected relative
      conditionalPanel(paste0("input.relSelect == ", rel()),
                       
        # top row: cancer name, cancer age, and delete module button
        fluidRow(
          
          # cancer name
          column(width = 6, 
            selectInput(inputId = ns("Can"), 
                        label = h5(paste0("Cancer:")),
                        choices = CANCER.CHOICES$long,
                        selected = can.select,
                        width = "200px")
          ),
          
          # cancer age
          column(width = 4,
            conditionalPanel(sprintf("input['%s'] != 'No cancer selected'", ns("Can")),
              div(numericInput(inputId = ns("CanAge"), 
                               label = h5("Diagnosis Age:"),
                               min = min.age, max = max.age, step = 1, value = can.age,
                               width = "100px"),
                  style = "margin-left:-75px"
              ),
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
                                 choices = OTHER.CANCER.CHOICES, 
                                 selected = can.other,
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
                                   min = min.age, max = max.age, step = 1, value = can.age,
                                   width = "100px"),
                      style = "margin-left:-40px"
                  ),
              )
            ) # end of conditionalPanel for CBC age 
          ) # end of fluidRow for CBC
        ) # end of conditionalPanel for contralateral breast cancer
      ) # end of conditionalPanel for person's inputs
    ) # end of tagList
  ) # end of div
}



















