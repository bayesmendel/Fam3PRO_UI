#' Mutation probability and future cancer risk plots for the PanelPRO Interface (PPI)
#'
#' Visualizes the carrier probabilities and future risk estimates returned by 
#' the PanelPRO function for the probands. A modified version of PanelPRO::visRisk().
#' 
#' @param pp_output A list of results returned by PanelPRO. 
#' @param markdown A logical value specifying whether the output requested is 
#' in an R Markdown file. If this is set to `TRUE`, then the height and width 
#' of the final plot will not be resizable. For use in the RStudio console, set 
#' `markdown` to the default value, `NULL`.  
#' @param return_obj A logical value; the default is `FALSE`, in which case the 
#' visualization will be plotted in the console and not returned. If set to 
#' `TRUE`, the visualization will be returned as a Plotly object and not 
#' plotted. Setting this value to TRUE will override the `markdown` argument. 
#' @param prob_threshold A numeric value. For genotypes with multiple 
#' simultaneous mutations, only those with an estimated probability of above 
#' `prob_threshold` will be shown. The default is `0.01`. 
#' @param show_fr_ci A logical value specifying whether confidence intervals 
#' for future risk plots should be shown when multiple imputations were run. 
#' The default is `FALSE`. 
#' @param height The height of the final plot (per proband) in px. The default 
#' is `450`. 
#' @param width The width of the final plot (per proband) in px. The default is 
#' `700`. 
#' @return A series of interactive Plotly plots. 
#' @examples
#' # run PanelPRO main function
#' output <- PanelPRO(test_fam_2, 
#'                    cancers = c("Endometrial", "Pancreas", "Small Intestine"),
#'                    genes = c("PALB2", "BRCA2"),
#'                    parallel = FALSE)
#' # Render plots
#' visRisk(output)
visRiskPPI <- function(pp_output, markdown = NULL, return_obj = FALSE, 
                       prob_threshold = 0.01, show_fr_ci = FALSE,
                       height = 450, width = 700) {
  # Get the number of probands in the output
  nProbands <- length(pp_output$posterior.prob)
  
  # Get the IDs of the probands
  probandIDs <- names(pp_output$posterior.prob)
  
  # Verify that there are the same number of probands in the future.risk part
  stopifnot(nProbands == length(pp_output$future.risk))
  
  # Get the cancer names
  cancers <- names(pp_output$future.risk[[1]])
  
  # Initialize list of figures
  figs <- list()
  
  # Loop over the probands
  for (i in seq_len(nProbands)) {
    
    # which plots to omit
    omit.plots <- "none"
    
    # Check that the selected proband isn't dead, such that future risk is NA
    if (any(grepl("dead", pp_output$future.risk[[i]]))) {
      rlang::inform(paste0("Proband ID: ", probandIDs[i], 
                           " is dead. Not outputting plots for this person."),
                    class = "DeadProband")
      omit.plots <- "fr"
      # next
      # Check that the selected proband's age doesn't exceed MAXAGE
    } else if (any(grepl("maximum age support", pp_output$future.risk[[i]]))) {
      rlang::inform(paste0("Proband ID: ", probandIDs[i],
                           " has current age equal to or above the", 
                           "maximum age supported, ", PanelPRO:::MAXAGE,
                           ". Not outputting plots for this person."))
      omit.plots <- "fr"
      # next
      # Check that the selected proband has carrier probability results
    } else if (any(grepl("No carrier probabilities were requested by the model specification.", 
                         pp_output$posterior.prob[[i]]))) {
      rlang::inform(paste0("Proband ID: ", probandIDs[i], 
                           " has no carrier probability estimates because there are no genes in the model. Not outputting plots for this person."))
      omit.plots <- "both"
      # next
      # Check that the selected proband has future risk results
    } else if (any(grepl("No future risk estimates were requested by the model specification.", 
                         pp_output$future.risk[[i]]))) {
      rlang::inform(paste0("Proband ID: ", probandIDs[i], 
                           " has no future risk estimates because there are no cancers in the model. Not outputting future risk plots for this person."))
      omit.plots <- "fr"
      # next
    }
    
    # Put future risk data together
    if(omit.plots != "both" & omit.plots != "fr"){
      future_risk_data <- cbind(
        cancer = rep(cancers, sapply(pp_output$future.risk[[i]], nrow)),
        do.call(rbind, pp_output$future.risk[[i]])
      )
      
      # Use capitalized name
      names(future_risk_data)[names(future_risk_data) == "cancer"] <- "Cancer"
      
      # Plot of future risk estimates
      p1 <- ggplot2::ggplot(future_risk_data, 
                            ggplot2::aes(ByAge, estimate, colour = Cancer)) +
        ggplot2::geom_point() +
        ggplot2::geom_line() +
        ggplot2::theme_minimal()
      
      # Check whether there were imputations
      pedigree_full <- all(is.na(future_risk_data$lower)) && 
        all(is.na(future_risk_data$upper))
      
      if (!pedigree_full && show_fr_ci) {
        # There was an imputation and we want to show them
        p1 <- p1 + ggplot2::geom_ribbon(ggplot2::aes(x = ByAge, 
                                                     ymin = lower, ymax = upper), 
                                        alpha = 0.3, linetype = 0)
      }
      
      # Future risk title information
      upper_plot_title <- list(
        text = "Future cancer risks",
        font = list(size = 20),
        xref = "paper",
        yref = "paper",
        yanchor = "bottom",
        xanchor = "center",
        align = "center",
        x = 0.5,
        y = 1.0,
        showarrow = FALSE
      )
      
      # If markdown not set, do normal resizable ggplotly
      # otherwise set the heights and widths
      if (is.null(markdown)) {
        gg1 <- plotly::ggplotly(p1)
      } else {
        gg1 <- plotly::ggplotly(p1, height = height, width = width)
      }
      
      # create static ggplot version for download
      p1 <- 
        p1 +
        ggplot2::facet_wrap(vars(Cancer)) +
        ggplot2::labs(title = "Future cancer risks", y = "Cumulative cancer risk", x = "Age",
                      caption = 
                      "Variability in estimates may arise from an imputation process for missing ages.\nThe range of estimates is indicated by error bars or (lower, upper) estimates.\n\nIf the hetero/homogeneity or variant of the gene has not been specified,\nit is assumed to be heterogeneous and of any pathogenic variant."
                      ) +
        ggplot2::theme_bw() +
        ggplot2::theme(plot.title = element_text(hjust = 0.5),
                       plot.caption = element_text(size = 7, hjust = 0),
                       strip.text.x = element_text(size = 5))
      
      # Set up Plotly layout for future risk plot
      pp1 <- plotly::layout(gg1,
                            yaxis = list(
                              title = "Cumulative cancer risk",
                              titlefont = list(size = 16)
                            ),
                            xaxis = list(
                              title = "Age",
                              titlefont = list(size = 16)
                            ),
                            showlegend = TRUE,
                            margin = 1,
                            annotations = upper_plot_title
      )
      
      # for stand-alone cancer risk plot, add same annotations as the other plot
      pp1a <- pp1 %>% plotly::add_annotations(text = 
        "      Variability in estimates may arise from an imputation process for missing ages. 
        The range of estimates is indicated by error bars or (lower, upper) estimates. 
        
        If the hetero/homogeneity or variant of the gene has not been specified, 
        it is assumed to be heterogeneous and of any pathogenic variant.",
        xref = "paper",
        yref = "paper",
        yanchor = "bottom",
        xanchor = "center",
        align = "left",
        x = 0.2,
        y = 1.05,
        font = list(size = 9),
        showarrow = FALSE
      ) %>% plotly::config(modeBarButtonsToRemove = c("lasso2d","select2d","pan2d",
                                                      "autoScale2d",
                                                      "zoomIn2d","zoomOut2d"),
                           displaylogo = FALSE)
    } else {
      pp1a <- NULL
      p1 <- NULL
    }
    
    # Re-order such that non-carrier, 1 at a time ... appears
    if(omit.plots != "both"){
      current_pp <- pp_output$posterior.prob[[i]]
      current_pp$genes <- factor(current_pp$genes, levels = current_pp$genes)
      
      # Get the row numbers of the noncarrier, single and multiple gene positions
      # These have no period (full stop) in them
      nc_position <- grepl("noncarrier", current_pp$genes)
      single_positions <- !grepl("\\.|noncarrier", current_pp$genes)
      multiple_positions <- !(nc_position | single_positions)
      multiple_gene_probs <- current_pp[multiple_positions, ]
      
      # Combine the single gene probs and any multiple
      # gene probs which are above the threshold
      probs_to_show <- rbind(
        current_pp[single_positions, ],
        multiple_gene_probs[multiple_gene_probs$estimate > prob_threshold, ]
      )
      
      # Reduce full gene names
      probs_to_show$genes <- 
        as.factor(PanelPRO:::formatGeneNames(as.character(probs_to_show$genes)))
      
      # Now plot the single gene carrier probability plots
      p2 <- ggplot2::ggplot(probs_to_show, 
                            ggplot2::aes(x = genes, y = estimate)) +
        ggplot2::geom_point() +
        ggplot2::theme_minimal()
      
      # Check whether there were imputations
      pedigree_full <- all(is.na(probs_to_show$lower)) && 
        all(is.na(probs_to_show$upper))
      
      if (!pedigree_full) {
        # There was an imputation
        p2 <- p2 + 
          ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper),
                                 width = .2,
                                 position = ggplot2::position_dodge(0.05)
          )
      }
      
      # If markdown not set, do normal resizable ggplotly
      # otherwise set the heights and widths
      if (is.null(markdown)) {
        gg2 <- plotly::ggplotly(p2)
      } else {
        gg2 <- plotly::ggplotly(p2, height = height, width = width)
      }
      
      # create static ggplot version for download
      p2 <- 
        p2 +
        ggplot2::labs(title = "Mutation probabilities", y = "Mutation probability", x = "Gene",
                      caption = 
                        "Variability in estimates may arise from an imputation process for missing ages.\nThe range of estimates is indicated by error bars or (lower, upper) estimates.\n\nIf the hetero/homogeneity or variant of the gene has not been specified,\nit is assumed to be heterogeneous and of any pathogenic variant."
                      ) +
        ggplot2::theme_bw() +
        ggplot2::theme(plot.title = element_text(hjust = 0.5), 
                       axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
                       plot.caption = element_text(size = 7, hjust = 0))
      
      # Set up Plotly layout for carrier probability plot
      pp2 <- plotly::layout(gg2,
                            yaxis = list(
                              title = "Mutation probability",
                              titlefont = list(size = 14)
                            ),
                            xaxis = list(
                              title = "Gene",
                              titlefont = list(size = 14),
                              tickangle = -45),
                            margin = 1
      )
      
      # Plot of carrier probability estimates
      pp2 <- pp2 %>% plotly::add_annotations(text = "Mutation probabilities",
                                             font = list(size = 18),
                                             xref = "paper",
                                             yref = "paper",
                                             yanchor = "bottom",
                                             xanchor = "center",
                                             align = "center",
                                             x = 0.5,
                                             y = 1,
                                             showarrow = FALSE
      ) %>% plotly::add_annotations(text =
        "      Variability in estimates may arise from an imputation process for missing ages.
        The range of estimates is indicated by error bars or (lower, upper) estimates.
  
        If the hetero/homogeneity or variant of the gene has not been specified,
        it is assumed to be heterogeneous and of any pathogenic variant.",
       xref = "paper",
       yref = "paper",
       yanchor = "bottom",
       xanchor = "center",
       align = "left",
       x = 0.2,
       y = 1.1,
       font = list(size = 9),
       showarrow = FALSE
      ) %>% plotly::add_annotations(text =
                                      paste0("Non-carrier probability: ", round(current_pp[nc_position, ]$estimate,
                                                                                digits = 3)),
                                    xref = "paper",
                                    yref = "paper",
                                    yanchor = "bottom",
                                    xanchor = "center",
                                    align = "center",
                                    x = 0.9,
                                    y = 1.0,
                                    font = list(size = 10),
                                    showarrow = FALSE
      ) %>% plotly::config(modeBarButtonsToRemove = c("lasso2d","select2d","pan2d",
                                                      "autoScale2d",
                                                      "zoomIn2d","zoomOut2d",
                                                      "hoverClosestCartesian",
                                                      "hoverCompareCartesian",
                                                      "toggleSpikelines"),
                           displaylogo = FALSE)
      
      # Add in the confidence intervals if needed
      if (!pedigree_full) {
        pp2 <- pp2 %>% plotly::add_annotations(text = 
                                                 paste0("(", round(current_pp[nc_position, ]$lower, digits = 3), ", ",
                                                        round(current_pp[nc_position, ]$upper, digits = 3), ")"),
                                               xref = "paper",
                                               yref = "paper",
                                               yanchor = "bottom",
                                               xanchor = "center",
                                               align = "center",
                                               x = 0.9,
                                               y = 0.875,
                                               font = list(size = 10),
                                               showarrow = FALSE
        )
      }
    } else {
      p2 <- NULL
      pp2 <- NULL
    }
    
    # Final Plotly output
    if(omit.plots != "both" & omit.plots != "fr"){
      fig <- plotly::layout(plotly::subplot(pp1, pp2, nrows = 2, 
                                            titleY = TRUE, titleX = TRUE, 
                                            margin = 0.2,
                                            heights = c(0.5, 0.5)),
                            title = paste0("Cancer Risk and Mutation Risk Profile: ID ", 
                                           probandIDs[i]),
                            modebar = list(orientation = "h"),
                            margin = 0.2,
                            showlegend = TRUE,
                            legend = list(font = list(size = 9))
      )
      
      figs[[i]] <- fig
      
      if (return_obj == FALSE) {
        print(figs[[i]])
      }
    } else {
      figs <- NULL
    }
  }
  
  if (return_obj == TRUE) {
    return(list(
      both = figs,
      cp = pp2,
      fr = pp1a,
      cpStatic = p2,
      frStatic = p1
    ))
  }
  
  if (!is.null(markdown) & isTRUE(markdown)) {
    htmltools::tagList(figs)
  }
}


#' Evaluate PanelPRO and Handle Errors and Warnings
#' 
#' Arguments are the same as the PanelPRO function but fewer
#' @returns either the typical PanelPRO result or a warning or error message
eval_PanelPRO <- function(pedigree,
                          model_spec = NULL,
                          genes = NULL,
                          cancers = NULL,
                          max.mut,
                          age.by,
                          unknown.race,
                          unknown.ancestry,
                          allow.intervention,
                          ignore.proband.germ,
                          iterations,
                          max.iter.tries,
                          random.seed,
                          net){
  tryCatch(
    
    # try to run PanelPRO
    {
      if(!is.null(model_spec)){
        out <- PanelPRO(pedigree = pedigree,
                        model_spec = model_spec,
                        max.mut = max.mut,
                        age.by = age.by,
                        unknown.race = unknown.race,
                        unknown.ancestry = unknown.ancestry,
                        allow.intervention = allow.intervention,
                        ignore.proband.germ = ignore.proband.germ,
                        iterations = iterations,
                        max.iter.tries = max.iter.tries,
                        random.seed = random.seed,
                        net = net)
      } else if(!is.null(genes) & !is.null(cancers)){
        out <- PanelPRO(pedigree = pedigree,
                        genes = genes,
                        cancers = cancers,
                        max.mut = max.mut,
                        age.by = age.by,
                        unknown.race = unknown.race,
                        unknown.ancestry = unknown.ancestry,
                        allow.intervention = allow.intervention,
                        ignore.proband.germ = ignore.proband.germ,
                        iterations = iterations,
                        max.iter.tries = max.iter.tries,
                        random.seed = random.seed,
                        net = net)
      }
    },
    
    # if an error occurs, tell the user
    error = function(e){
      return(e)
    },
    
    # if a warning occurs, tell the user
    warning = function(w){
      return(w)
    }
  )
}

#' Capture PanelPRO console output and send to UI
#' adapted from: https://gist.github.com/jcheng5/3830244757f8ca25d4b00ce389ea41b3
ppResultsAndConsole <- function(pedigree,
                                model_spec,
                                genes,
                                cancers,
                                max.mut,
                                age.by,
                                unknown.race,
                                unknown.ancestry,
                                allow.intervention,
                                ignore.proband.germ,
                                iterations,
                                max.iter.tries,
                                random.seed,
                                net) {
  
  ## safely run PanelPRO to check for errors and warnings
  # pre-defined model case
  if(model_spec != "Custom"){
    ppErrorProof <- eval_PanelPRO(pedigree = pedigree,
                                  model_spec = model_spec,
                                  max.mut = max.mut,
                                  age.by = age.by,
                                  unknown.race = unknown.race,
                                  unknown.ancestry = unknown.ancestry,
                                  allow.intervention = allow.intervention,
                                  ignore.proband.germ = ignore.proband.germ,
                                  iterations = iterations,
                                  max.iter.tries = max.iter.tries,
                                  random.seed = random.seed,
                                  net = net)
    
    # custom model
  } else {
    ppErrorProof <- eval_PanelPRO(pedigree = pedigree,
                                  genes = genes,
                                  cancers = cancers,
                                  max.mut = max.mut,
                                  age.by = age.by,
                                  unknown.race = unknown.race,
                                  unknown.ancestry = unknown.ancestry,
                                  allow.intervention = allow.intervention,
                                  ignore.proband.germ = ignore.proband.germ,
                                  iterations = iterations,
                                  max.iter.tries = max.iter.tries,
                                  random.seed = random.seed,
                                  net = net)
  }
  
  ## take different actions depending on if an error was thrown
  # not an error so re-run to capture PanelPRO results and console output
  if(!"error" %in% class(ppErrorProof)){
    
    # different run settings based on if a pre-defined vs custom model was selected
    if(model_spec != "Custom"){
      txt <- 
        capture.output(
          results <- 
            PanelPRO(
              pedigree = pedigree,
              model_spec = model_spec,
              max.mut = max.mut,
              age.by = age.by,
              unknown.race = unknown.race,
              unknown.ancestry = unknown.ancestry,
              allow.intervention = allow.intervention,
              ignore.proband.germ = ignore.proband.germ,
              iterations = iterations,
              max.iter.tries = max.iter.tries,
              random.seed = random.seed,
              net = net
            ), 
          type = "message"
        )
    } else {
      txt <- 
        capture.output(
          results <- 
            PanelPRO(
              pedigree = pedigree,
              genes = genes,
              cancers = cancers,
              max.mut = max.mut,
              age.by = age.by,
              unknown.race = unknown.race,
              unknown.ancestry = unknown.ancestry,
              allow.intervention = allow.intervention,
              ignore.proband.germ = ignore.proband.germ,
              iterations = iterations,
              max.iter.tries = max.iter.tries,
              random.seed = random.seed,
              net = net
            ), 
          type = "message"
        )
    }
    
    # format the console output
    txt <- paste0(txt, "\n", collapse = "")
    
    # error - return error message
  } else {
    call <- paste0(deparse(ppErrorProof$call), collapse = "\n")
    mssg <- paste0(ppErrorProof$message, collapse = "\n")
    txt <- paste0("Error:\n\ncall:\n", call, "\nmessage:\n", mssg)
    results <- NULL
  }
  
  # change text color based on message type
  if("error" %in% class(ppErrorProof)){
    mssg.color <- "red"
  } else {
    mssg.color <- "black"
  }
  
  # persistent console output for one of the results tabs
  removeUI(selector = "#outputContainer") # remove old outputs if they are there
  insertUI("#ppMessageContainer", 
           where = "beforeEnd",
           ui = div(id = "outputContainer", 
                    p(txt, style = paste0("white-space:pre-wrap;color:", mssg.color))))
  
  # create pop-up only if there were warning or errors
  if(any(c("error", "warning") %in% class(ppErrorProof))){
    
    # change formatting based on message type
    if("error" %in% class(ppErrorProof)){
      modal.title <- "PanelPRO Issued an Error"
      mssg.intro <- "The error message is below:"
    } else {
      modal.title <- "PanelPRO Issued a Warning"
      mssg.intro <- "The R console output is below which includes the warning message:"
    }
    
    # create modal
    showModal(modalDialog(
      tagList(p(mssg.intro),
              p(txt, style = paste0("white-space:pre-wrap;color:", mssg.color, ";")),
              p("After closing this pop-up, you can view it again on the 'Console Output' tab."),
              p("Check the PanelPRO Documentation for more information.")),
      title = modal.title,
      footer = tagList(modalButton("OK")),
      easyClose = T
    ))
  }
  
  return(results)
}
