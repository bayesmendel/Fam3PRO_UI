#' Create data frame of genetic testing results
#' 
#' @param gr a list of the panel and gene module tracking information of the format 
#' `trackGenes.init`.
#' @param rel a number, the ID number of the relative for which the data frame 
#' is to be created.
#' @param inp, the shiny input list
#' @returns a data frame of genetic test results with columns: Gene, Result, Nucleotide, 
#' Protein, and Panel.
makeGeneDF <- function(rel, gr = geneReactive$GeneNums, 
                       dupResultGene,
                       inp = input){
  
  if(is.numeric(rel)){
    rel <- as.character(rel)
  }
  
  # specify the relative
  gr <- gr[[rel]]
  
  # initialize the data frame
  tmp.r <- gene.inputs.store
  
  # storage for all negative genes 
  neg.g <- as.character()
  
  # verify the relative has at least one panel
  if(!is.na(gr$dict[1])){
    
    # iterate through the panels for this relative to get data frame of PLP, VUS, and BLB results
    for(p.num in as.numeric(names(gr$dict))){
      panel.module.id.num <- gr$dict[p.num]
      grp <- gr$panels[[paste0("panel", p.num)]]
      p.name <- grp$name
      p.genes <- grp$genes
      
      # iterate through the result types
      for(rtype in c("PLP", "VUS", "BLB")){
        grpr <- grp$results[[rtype]]
        
        # iterate through the gene modules for this relative, panel and result type
        if(!is.na(grpr$dict[1])){
          for(g.num in as.numeric(names(grpr$dict))){
            gene.module.id.num <- grpr$dict[g.num]
            geneMod.id <- paste0("rel", rel, "Pan", panel.module.id.num, rtype, "GeneModule", gene.module.id.num)
            if(inp[[paste0(geneMod.id,"-Gene")]] != ""){
              tmp.r[nrow(tmp.r)+1,] <- c(inp[[paste0(geneMod.id,"-Gene")]],                         # Gene
                                         ifelse(rtype == "PLP", "P/LP", 
                                                ifelse(rtype == "BLB", "B/LB", rtype)),             # Result
                                         ifelse(is.null(inp[[paste0(geneMod.id,"-NucInfo")]]), "",
                                                inp[[paste0(geneMod.id,"-NucInfo")]]),              # Nucleotide 
                                         ifelse(is.null(inp[[paste0(geneMod.id,"-ProtInfo")]]), "",
                                                inp[[paste0(geneMod.id,"-ProtInfo")]]),             # Protein
                                         inp[[paste0(geneMod.id,"-ZygInfo")]],                      # Zygosity
                                         p.name)                                                    # Panel Name
            }
          }
        }
      }
      
      # add negative genes for the panel which are any genes that were not PLP, VUS, or BLB
      neg.g <- setdiff(p.genes, tmp.r$Gene[which(tmp.r$Panel == p.name)])
      p.neg.g <- setNames(as.data.frame(matrix("", nrow = length(neg.g), ncol = length(gene.df.colnames))),
                          gene.df.colnames)
      if(nrow(p.neg.g) > 0){
        p.neg.g$Gene <- neg.g
        p.neg.g$Result <- "Neg"
        p.neg.g$Zygosity <- "Unk"
        p.neg.g$Panel <- p.name
      }
      tmp.r <- rbind(tmp.r, p.neg.g)
    }
    
    # check if any genes are listed in more than one category which will warn the user
    if(length(intersect(tmp.r$Gene[which(tmp.r$Result == "P/LP")], tmp.r$Gene[which(tmp.r$Result == "VUS") ])) > 0 |
       length(intersect(tmp.r$Gene[which(tmp.r$Result == "P/LP")], tmp.r$Gene[which(tmp.r$Result == "B/LB")])) > 0 |
       length(intersect(tmp.r$Gene[which(tmp.r$Result == "P/LP")], tmp.r$Gene[which(tmp.r$Result == "Neg") ])) > 0 |
       length(intersect(tmp.r$Gene[which(tmp.r$Result == "VUS" )], tmp.r$Gene[which(tmp.r$Result == "B/LB")])) > 0 |
       length(intersect(tmp.r$Gene[which(tmp.r$Result == "VUS" )], tmp.r$Gene[which(tmp.r$Result == "Neg") ])) > 0 |
       length(intersect(tmp.r$Gene[which(tmp.r$Result == "B/LB")], tmp.r$Gene[which(tmp.r$Result == "Neg") ])) > 0
    ){
      dupResultGene <- TRUE
    } else {
      dupResultGene <- FALSE
    }
    
    # order the table
    tmp.r <- arrange(tmp.r, 
                     factor(Result, levels = c("P/LP", "VUS", "B/LB", "Neg")), 
                     Gene, Panel, Nucleotide, Protein, Zygosity)
    
    # ensure genes with multiple result types are stacked in the summary
    check.dups <- table(tmp.r$Gene)
    if(any(check.dups > 1)){
      dup.genes <- names(check.dups[which(check.dups > 1)])
      for(g in dup.genes){
        dup.results <- unique(tmp.r$Result[which(tmp.r$Gene == g)])
        if(length(dup.results) > 1){
          d.rows <- which(tmp.r$Gene == g)
          move.rows <- d.rows[2:length(d.rows)]
          other.rows <- setdiff(1:nrow(tmp.r), c(1:d.rows[1], move.rows))
          tmp.r <- tmp.r[c(1:(d.rows[1]), move.rows, other.rows),]
        }
      }
      
      # re-do rownames
      rownames(tmp.r) <- 1:nrow(tmp.r)
    }
  } else {
    tmp.r <- NULL
  }
  
  return.list <- list(df = tmp.r, dupResultGene = dupResultGene)
  return.list
}

#' Add a panelUI module
#' 
#' @param gr geneReactive$GeneNums
#' @param rel the relative for which the remove the panel
#' @param inp the shiny input object
#' @param ss shiny session
#' @param pan.name name of the panel to add
#' @param conn a database connection
#' @returns a list:
#' - gr: updated copy of geneReactive$GeneNums
#' - panel.module.id.num: the unique number associated with the panelUI module
#' - panMod.id: the full panelUI module id
addPanel <- function(gr = geneReactive$GeneNums, rel, inp = input, ss = session,
                     pan.name, conn){
  
  if(is.numeric(rel)){
    rel <- as.character(rel)
  }

  ## store initial inputs as variables to be called later
  add.gr <- gr[[rel]]
  panel.module.id.num <- add.gr$mx + 1
  if(add.gr$panels$panel1$name == "No panel selected"){
    next.dict.slot <- 1
  } else {
    next.dict.slot <- max(as.numeric(names(add.gr$dict))) + 1
  }
  new.pan <- paste0("panel", next.dict.slot)
  
  ## update gene/panel input tracking information
  # update panel aggregate tracking information
  add.gr$mx <- panel.module.id.num
  if(add.gr$panels$panel1$name == "No panel selected"){
    add.gr$dict <- setNames(c(panel.module.id.num), next.dict.slot)
  } else {
    add.gr$dict <- c(add.gr$dict, setNames(c(panel.module.id.num), next.dict.slot))
  }
  
  # update panel specific information
  add.gr$panels[[new.pan]]$name <- pan.name
  add.gr$panels[[new.pan]]$genes <- 
    strsplit(
      dbGetQuery(conn = conn,
                 statement = paste0("SELECT genes FROM panels WHERE panel_name='", pan.name,"';"))$genes,
      split = ","
    )[[1]]
  add.gr$panels[[new.pan]]$results <- geneResultsTemplate
  
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
  for(pn in names(add.gr$dict)){
    cur.panels <- c(cur.panels, add.gr$panels[[paste0("panel", pn)]]$name)
  }
  all.pans <- 
    sort(dbGetQuery(conn = conn,
                    statement = "SELECT panel_name FROM panels")$panel_name)
  updateSelectInput(ss, "existingPanels",
                    choices = all.pans[which(!all.pans %in% cur.panels)],
                    selected = "No panel selected")
  
  # add panel name to dropdown choices for editing an active panel
  updateSelectInput(ss, "editPanel",
                    choices = c("No panel selected", cur.panels),
                    selected = "No panel selected")
  
  # update full geneReactive$GeneNums
  gr[[rel]] <- add.gr
  
  # return list
  return(list(gr = gr, 
              panel.module.id.num = panel.module.id.num, 
              panMod.id = panMod.id))
}

#' Remove a panelUI module
#' 
#' @param gr geneReactive$GeneNums.
#' @param rel the relative for which the remove the panel (as a character string with the relative's ID number)
#' @param inp the shiny input object
#' @param ss shiny session
#' @param pan.name name of the panel to remove/delete
#' @param panel.module.id.number number, the unique numeric id number of the panelUI module
#' @param conn a database connection
#' @returns a list:
#' - an updated copy of geneReactive$GeneNums
#' - a character vector with all of the geneUI module id's associated with the panel (so they can be removed from memory)
removePanel <- function(gr = geneReactive$GeneNums, 
                        rel, 
                        inp = input, 
                        ss = session, 
                        pan.name, 
                        panel.module.id.num,
                        conn){
  
  if(is.numeric(rel)){
    rel <- as.character(rel)
  }
  
  ## remove the panel's input information from reactive used to track gene/panel inputs
  # get an updated copy of the reactive because gr likely changed between creation and removal of the UI
  rm.gr <- gr[[rel]]
  
  # get this module's full id name
  panMod.idd <- paste0("rel", rel, "PanelModule", panel.module.id.num)
  
  # find the panelUI's order number in the dictionary
  for(rm.pnum in as.numeric(names(rm.gr$dict))){
    rm.pnum.name <- paste0("panel", rm.pnum)
    if(rm.gr$panels[[rm.pnum.name]]$name == pan.name){
      break
    }
  }
  
  ## remove all related geneUI's for each result type
  panel.geneMod.ids <- as.character() # this will be passed out of the function for deleting these modules from memory
  for(rtype in c("PLP", "VUS", "BLB")){
    rm.gr.rtype <- rm.gr$panels[[rm.pnum.name]]$results[[rtype]]
    for(gnum in as.numeric(names(rm.gr.rtype$dict))){
      gene.module.id.num <- rm.gr.rtype$dict[gnum]
      
      # remove the module from the UI
      geneMod.id <- paste0("rel", rel, 
                           "Pan", panel.module.id.num, 
                           rtype, "GeneModule", gene.module.id.num)
      panel.geneMod.ids <- c(panel.geneMod.ids, geneMod.id)
      removeUI(selector = paste0("#", geneMod.id, "Cont"))
    }
  }
  
  ## delete the panelUI module
  # remove the module from the UI
  removeUI(selector = paste0("#", panMod.idd, "Cont"))
  
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
  gr[[rel]] <- rm.gr
  
  ## update related dropdown choices
  # add removed panel back to dropdown choices for adding a new panel
  rm.cur.panels <- as.character()
  if(!is.na(rm.gr$dict[1])){
    for(pnum in names(rm.gr$dict)){
      rm.cur.panels <- c(rm.cur.panels, rm.gr$panels[[paste0("panel", pnum)]]$name)
    }
  }
  all.pans <- 
    sort(dbGetQuery(conn = conn,
                    statement = "SELECT panel_name FROM panels")$panel_name)
  updateSelectInput(ss, "existingPanels",
                    choices = all.pans[which(!all.pans %in% rm.cur.panels)],
                    selected = "No panel selected")
  
  # remove panel name from dropdown choices for editing an active panel
  updateSelectInput(ss, "editPanel",
                    choices = c("No panel selected", rm.cur.panels),
                    selected = "No panel selected")
  
  # return the updated copy of geneReactive#GeneNums
  return(list(gr = gr, panel.geneMod.ids = panel.geneMod.ids))
}

#' Add a geneUI module
#' 
#' @param gr geneReactive$GeneNums
#' @param rel the relative for which the remove the panel
#' @param inp the shiny input object
#' @param p.name name of the panel to add
#' @param rtype one of c("PLP", "VUS", "BLB")
#' @returns a list:
#' - gr: updated copy of geneReactive$GeneNums
#' - gene.module.id.num: the unique number associated with the geneUI module
#' - panMod.id: the full geneUI module id
addGene <- function(gr = geneReactive$GeneNums, rel, inp = input, p.name, rtype, vals = NULL){
  
  if(is.numeric(rel)){
    rel <- as.character(rel)
  }
  
  # assign variables to be called later
  add.gr <- gr[[rel]]
  for(pnum in as.numeric(names(add.gr$dict))){
    pnum.name <- paste0("panel", pnum)
    if(add.gr$panels[[pnum.name]]$name == p.name){
      panel.module.id.num <- add.gr$dict[pnum]
      p.genes <- add.gr$panels[[pnum.name]]$genes
      gr.result <- add.gr$panels[[pnum.name]]$results[[rtype]]
      gene.module.id.num <- gr.result$mx + 1
      if(is.na(gr.result$dict[1])){
        next.dict.slot <- 1
      } else {
        next.dict.slot <- max(as.numeric(names(gr.result$dict))) + 1
      }
      break
    }
  }
  
  # update this result type's geneUI module tracking information for this relative and panel
  gr.result$mx <- gene.module.id.num
  if(is.na(gr.result$dict[1])){
    gr.result$dict <- setNames(c(gene.module.id.num), next.dict.slot)
  } else {
    gr.result$dict <- c(gr.result$dict, setNames(c(gene.module.id.num), next.dict.slot))
  }
  gr[[rel]]$panels[[pnum.name]]$results[[rtype]] <- gr.result
  
  # create the unique geneUI module ID and insert the module into its own container
  geneMod.id <- paste0("rel", rel, "Pan", panel.module.id.num, rtype, "GeneModule", gene.module.id.num)
  insertUI(
    selector = paste0("#", rtype, "Cont"),
    where = "beforeEnd",
    ui = geneUI(id = geneMod.id,
                rel = rel,
                panelName = p.name,
                panelGenes = p.genes,
                vals = vals)
  )
  
  return(list(gr = gr,
              gene.module.id.num = gene.module.id.num,
              geneMod.id = geneMod.id))
}

#' Remove a geneUI
#' 
#' @param gr geneReactive$GeneNums.
#' @param rel the relative for which the remove the panel
#' @param inp the shiny input object
#' @param p.name name of the associated panel
#' @param gene.module.id.num number, the unique numeric id number of the geneUI module
#' @param geneMod.id the unique module id of the geneUI
#' @param rtype one of c("PLP", "VUS", "BLB")
#' @returns an updated copy of geneReactive$GeneNums
removeGene <- function(gr = geneReactive$GeneNums, 
                       rel, 
                       inp = input, 
                       p.name, 
                       gene.module.id.num,
                       geneMod.id,
                       rtype){
  
  if(is.numeric(rel)){
    rel <- as.character(rel)
  }
  
  # get the section of the reactive for this relative
  rm.gr <- gr[[rel]]
  
  # remove the module from the UI
  removeUI(selector = paste0("#", geneMod.id, "Cont"))
  
  # remove the gene's input information from reactive used to track gene/panel inputs
  for(rm.pnum in as.numeric(names(rm.gr$dict))){
    rm.pnum.name <- paste0("panel", rm.pnum)
    if(rm.gr$panels[[rm.pnum.name]]$name == p.name){
      break
    }
  }
  rm.gr.result <- rm.gr$panels[[rm.pnum.name]]$results[[rtype]]
  del.dict.slot <- which(rm.gr.result$dict == gene.module.id.num)
  
  # clear gene module information
  if(length(rm.gr.result$dict) == 1){
    rm.gr.result$dict[1] <- NA
  } else {
    
    # case where the gene module to be deleted is not the last gene module added
    if(del.dict.slot != names(rm.gr.result$dict)[length(rm.gr.result$dict)]){
      
      # iterate through the active geneUI modules to update the dictionary
      for(el in del.dict.slot:(length(rm.gr.result$dict) - 1)){
        rm.gr.result$dict[el] <- rm.gr.result$dict[el+1]
      }
    }
    
    # remove the last entry from the dictionary
    rm.gr.result$dict <- rm.gr.result$dict[1:(length(rm.gr.result$dict) - 1)]
  }
  
  # update reactive and return it
  rm.gr$panels[[rm.pnum.name]]$results[[rtype]] <- rm.gr.result
  gr[[rel]] <- rm.gr
  return(gr)
}


