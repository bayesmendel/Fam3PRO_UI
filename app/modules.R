#### Cancer Hx ####

# module to enter data for one cancer
canUI <- function(id, canNum, rel, cancer = NULL, age = NULL, other = NULL){
  
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
      conditionalPanel(paste0("input.relSelect == ", rel()),
        fluidRow(
          column(width = 6, 
            selectInput(inputId = ns("Can"), 
                        label = h5(paste0("Cancer ",canNum,":")),
                        choices = CANCER.CHOICES$long,
                        selected = can.select,
                        width = "200px")
          ),
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
          # remove this set of cancer inputs
          column(width = 2,
            actionButton(inputId = ns("removeCan"),
                         label = NULL,
                         icon = icon('trash'),
                         style = "color: #FF2800; background-color: white; border-color: grey; margin-left:-150px; margin-top:40px")
          )
        ),
        conditionalPanel(sprintf("input['%s'] == 'Other'", ns("Can")),
          fluidRow(
             column(5, h5("Other cancer:", style = "margin-left:25px")),
             column(7, 
              div(selectizeInput(inputId = ns("CanOther"), 
                                 label = NULL,
                                 choices = c("Unknown/Not Listed", non.pp.cancers), 
                                 selected = can.other,
                                 multiple = FALSE, 
                                 options = list(create=TRUE),
                                 width = "225px"),
                  style = "margin-left:-125px;"
              )
            )
          )
        ) # end of conditionalPanel for other cancers
      ) # end of div
    )
  ) # end of tagList
}




















