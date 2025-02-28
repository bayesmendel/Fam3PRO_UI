#' Mutation probability and future cancer risk plots for the Fam3PRO Interface (F3PI)
#'
#' Visualizes the carrier probabilities and future risk estimates returned by 
#' the Fam3PRO function for the probands. A modified version of Fam3PRO::visRisk().
#' 
#' @param f3p_output A list of results returned by Fam3PRO 
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
#' @param race string containing the proband's Fam3PRO race category
#' @param sex binary, 0 if female and 1 if male
#' @param cur.age numeric value, age of the proband
#' @param net logical value, if TRUE net penetrances are used, otherwise crude penetrances are used
#' @param missAgeIters number of iterations allowed for age imputation for Fam3PRO as defined by 
#' Fam3PRO argument `iterations`.
#' @return A series of interactive Plotly plots. 
#' @examples
#' # run Fam3PRO main function
#' output <- Fam3PRO(test_fam_2, 
#'                    cancers = c("Endometrial", "Pancreas", "Small Intestine"),
#'                    genes = c("PALB2", "BRCA2"),
#'                    parallel = FALSE)
#' # Render plots
#' visRisk(output)
visRiskF3PI <- function(f3p_output, markdown = NULL, return_obj = FALSE, 
                       prob_threshold = 0.01, show_fr_ci = FALSE,
                       height = 450, width = 700, race, sex, cur.age, net,
                       missAgeIters){
  
  # Get the number of probands in the output
  nProbands <- length(f3p_output$posterior.prob)
  
  # Get the IDs of the probands
  probandIDs <- names(f3p_output$posterior.prob)
  
  # Verify that there are the same number of probands in the future.risk part
  stopifnot(nProbands == length(f3p_output$future.risk))
  
  # Get the cancer names
  cancers <- names(f3p_output$future.risk[[1]])
  
  # Initialize list of figures
  figs <- list()
  
  # Loop over the probands
  for (i in seq_len(nProbands)) {
    
    # which plots to omit
    omit.plots <- "none"
    
    # Check that the selected proband isn't dead, such that future risk is NA
    if (any(grepl("dead", f3p_output$future.risk[[i]]))) {
      rlang::inform(paste0("Proband ID: ", probandIDs[i], 
                           " is dead. Not outputting plots for this person."),
                    class = "DeadProband")
      omit.plots <- "fr"
      
      # Check that the selected proband's age doesn't exceed MAXAGE
    } else if (any(grepl("maximum age support", f3p_output$future.risk[[i]]))) {
      rlang::inform(paste0("Proband ID: ", probandIDs[i],
                           " has current age equal to or above the", 
                           "maximum age supported, ", Fam3PRO:::MAXAGE,
                           ". Not outputting plots for this person."))
      omit.plots <- "fr"
  
      # Check that the selected proband has carrier probability results
    } else if (any(grepl("No carrier probabilities were requested by the model specification.", 
                         f3p_output$posterior.prob[[i]]))) {
      rlang::inform(paste0("Proband ID: ", probandIDs[i], 
                           " has no carrier probability estimates because there are no genes in the model. Not outputting plots for this person."))
      omit.plots <- "both"

      # Check that the selected proband has future risk results
    } else if (any(grepl("No future risk estimates were requested by the model specification.", 
                         f3p_output$future.risk[[i]]))) {
      rlang::inform(paste0("Proband ID: ", probandIDs[i], 
                           " has no future risk estimates because there are no cancers in the model. Not outputting future risk plots for this person."))
      omit.plots <- "fr"
    }
    
    #### Future Risk ####
    
    # Put future risk data together
    if(omit.plots != "both" & omit.plots != "fr"){
      future_risk_data <- cbind(
        cancer = rep(cancers, sapply(f3p_output$future.risk[[i]], nrow)),
        do.call(rbind, f3p_output$future.risk[[i]])
      )
      
      # Use capitalized name
      names(future_risk_data)[names(future_risk_data) == "cancer"] <- "Cancer"
      future_risk_data <- 
        future_risk_data %>%
        mutate(Who = "Proband", .after = "Cancer")
      
      ## get average person risks
      nc.pens <-
        as.data.frame(
          Fam3PRO::Fam3PRODatabase$Penetrance[, # Cancer
                                                "SEER", # Gene
                                                ifelse(is.na(race), "All_Races", race), #Race
                                                ifelse(sex == 0, "Female", "Male"), #Sex
                                                , # Age
                                                ifelse(net, "Net", "Crude") # PenetType
          ]
        ) %>%
        tibble::rownames_to_column("Cancer") %>%
        filter(Cancer %in% unique(future_risk_data$Cancer)) %>%
        pivot_longer(cols = -Cancer, names_to = "ByAge", values_to = "Penetrance") %>%
        mutate(Who = "Average Person", .after = "Cancer") %>%
        mutate(estimate = 0, .after = "ByAge") %>%
        mutate(lower = NA, .after = "estimate") %>%
        mutate(upper = NA, .after = "lower") %>%
        group_by(Cancer) %>%
        mutate(Survival = 1 - cumsum(Penetrance)) %>%
        mutate(across(.cols = c(Survival, estimate), ~as.numeric(.)))
      for(c in unique(nc.pens$Cancer)){
        tmp.pens <- nc.pens[which(nc.pens$Cancer == c),]
        for(a in (cur.age+1):(Fam3PRO:::MAXAGE)){
          nc.pens$estimate[which(nc.pens$Cancer == c & nc.pens$ByAge == a)] <-
            sum(tmp.pens$Penetrance[(cur.age+1):a]) / nc.pens$Survival[cur.age]
        }
      }
      nc.pens <-
        nc.pens %>%
        select(-c(Penetrance, Survival)) %>%
        mutate(ByAge = as.numeric(ByAge)) %>%
        filter(ByAge %in% future_risk_data$ByAge)
      future_risk_data <- rbind(future_risk_data, nc.pens)
      
      ##### GGPlot Version ####
      ###### Zoomed ####
      
      ## Plot of future risk estimates
      # 1st set of plots: include average person risk where plots are facetted by cancer type
      # create a grob a plots
      plot.list <- list()
      cans.to.plot <- unique(future_risk_data$Cancer)
      num.cans <- length(cans.to.plot)
      
      # get number of rows for the grob/facet
      if(num.cans %in% seq(1,3)){
        grows <- 1
      } else if(num.cans == 4){
        grows <- 2
      } else {
        grows <- ceiling(num.cans / 3)
      }
      gcols <- ceiling(num.cans / grows)
      
      # cancer colors
      color.scheme <- grDevices::rainbow(n = num.cans, s = 1, v = .85)
      cancer.colors <- stats::setNames(color.scheme, cans.to.plot)
      
      # loop through the cancers
      for(this.cancer in cans.to.plot){
        this.plot <- make.ggline(data = future_risk_data[which(future_risk_data$Cancer == this.cancer),],
                                 cancer = this.cancer,
                                 cancer.colors = cancer.colors,
                                 age = cur.age,
                                 grows = grows)
        cancer.num <- which(unique(future_risk_data$Cancer) == this.cancer)
        plot.list[[cancer.num]] <- this.plot
      }
      
      # prepare the grid of individual cancer plots
      p0 <- arrangeGrob(grobs = plot.list,
                         top = textGrob("Future cancer risks", gp = gpar(fontsize = 24)),
                         left = textGrob("Cumulative cancer risk", rot = 90, gp = gpar(fontsize = 16)),
                         bottom = textGrob("Age", gp = gpar(fontsize = 16)),
                         nrow = grows)
      
      # create 2 static ggplot version for download, one with y-axis at 1, and the other at the maximum cancer risk
      p1.zoom.fap <-
        ggplot2::ggplot(subset(future_risk_data, Who == "Proband"),
                        ggplot2::aes(ByAge, estimate,
                                     color = Cancer)) +
        ggplot2::geom_point() +
        ggplot2::geom_line() +
        ggplot2::facet_wrap(vars(Cancer), labeller = label_wrap_gen()) +
        ggplot2::labs(title = "Future cancer risks", y = "Cumulative cancer risk", x = "Age",
                      caption =
                        "Variability in estimates may arise from an imputation process for missing ages.\nThe range of estimates is indicated by error bars or (lower, upper) estimates.\n\nIf the hetero/homogeneity or variant of the gene has not been specified,\nit is assumed to be heterogeneous and of any pathogenic variant."
        ) +
        ggplot2::theme_bw() +
        ggplot2::theme(plot.title = element_text(hjust = 0.5),
                       plot.caption = element_text(size = 7, hjust = 0),
                       strip.text.x = element_text(size = 4))
      
      ###### Full Y-axis ####
      p1.full.fap <-
        p1.zoom.fap +
        ggplot2::scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.2))
      
      ##### Plotly Version ####
      ###### Zoomed ####
      # 2nd set of plots: no average person risk and no facetting
      p1.zoom <- 
        ggplot2::ggplot(subset(future_risk_data, Who == "Proband"), 
                        ggplot2::aes(ByAge, estimate, colour = Cancer)) +
        ggplot2::geom_point() +
        ggplot2::geom_line() +
        ggplot2::theme_minimal()
      
      ###### Full Y-axis ####
      p1.full <- 
        p1.zoom +
        ggplot2::scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.2))
      
      # Check whether there were imputations
      pedigree_full <- all(is.na(future_risk_data$lower)) && 
        all(is.na(future_risk_data$upper))
      
      if (!pedigree_full && show_fr_ci) {
        # There was an imputation and we want to show them
        p1.zoom <- p1.zoom + ggplot2::geom_ribbon(ggplot2::aes(x = ByAge, 
                                                     ymin = lower, ymax = upper), 
                                        alpha = 0.3, linetype = 0)
        p1.full <- p1.full + ggplot2::geom_ribbon(ggplot2::aes(x = ByAge, 
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
        gg1.zoom <- plotly::ggplotly(p1.zoom, height = height+150, width = width)
        gg1.full <- plotly::ggplotly(p1.full, height = height+150, width = width)
      } else {
        gg1 <- plotly::ggplotly(p1, height = height, width = width)
      }
      
      # Set up Plotly layout for future risk plot
      f3p1.zoom <- plotly::layout(gg1.zoom,
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
      f3p1.full <- plotly::layout(gg1.full,
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
      f3p1a.zoom <- f3p1.zoom %>% plotly::add_annotations(text = 
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
      
      f3p1a.full <- f3p1.full %>% plotly::add_annotations(text = 
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
                           displaylogo = FALSE
      ) %>% plotly::layout(yaxis = list(range = list(0,1)))
      
    } else {
      f3p1a.zoom <- NULL
      f3p1a.full <- NULL
      f3p1.zoom <- NULL
      f3p1.full <- NULL
    }
    
    #### Carrier Probs ####
    
    # Re-order such that non-carrier, 1 at a time ... appears
    if(omit.plots != "both"){
      current_f3p <- f3p_output$posterior.prob[[i]]
      
      # get risk of any gene having a P/LP
      prob.anyPVdf <- filter(current_f3p, !grepl(pattern = "\\.|noncarrier", genes))
      prob.anyPV <- sum(prob.anyPVdf$estimate, na.rm = T)
      current_f3p[nrow(current_f3p)+1,] <- rep(NA, 4)
      current_f3p$genes[nrow(current_f3p)] <- "Any Gene"
      current_f3p$estimate[nrow(current_f3p)] <- prob.anyPV
      
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
        current_f3p$lower[nrow(current_f3p)] <- min(any.preds, na.rm = T)
        current_f3p$upper[nrow(current_f3p)] <- max(any.preds, na.rm = T)
      } 
      
      
      current_f3p$genes <- factor(current_f3p$genes, levels = current_f3p$genes)
      
      # Get the row numbers of the noncarrier, single and multiple gene positions
      # These have no period (full stop) in them
      nc_position <- grepl("noncarrier", current_f3p$genes)
      single_positions <- !grepl("\\.|noncarrier", current_f3p$genes)
      multiple_positions <- !(nc_position | single_positions)
      multiple_gene_probs <- current_f3p[multiple_positions, ]
      
      # Combine the single gene probs and any multiple
      # gene probs which are above the threshold
      probs_to_show <- rbind(
        current_f3p[single_positions, ],
        multiple_gene_probs[multiple_gene_probs$estimate > prob_threshold, ]
      )
      
      # Reduce full gene names
      probs_to_show$genes <- 
        as.factor(Fam3PRO:::formatGeneNames(as.character(probs_to_show$genes)))
      # Now plot the single gene carrier probability plots
      
      
      p2.zoom <- 
        ggplot2::ggplot(probs_to_show, ggplot2::aes(x = genes, y = estimate)) +
        ggplot2::geom_point() +
        ggplot2::geom_hline(yintercept = 0.025, color = "red", lty = "dashed")
        ggplot2::theme_minimal()
        
      p2.full <- 
        p2.zoom +
        ggplot2::scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.2))

      # Check whether there were imputations
      pedigree_full <- all(is.na(probs_to_show$lower)) && 
        all(is.na(probs_to_show$upper))
      
      if (!pedigree_full) {
        # There was an imputation
        p2.zoom <- p2.zoom + 
          ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper),
                                 width = .2,
                                 position = ggplot2::position_dodge(0.05)
          )
        p2.full <- p2.full + 
          ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper),
                                 width = .2,
                                 position = ggplot2::position_dodge(0.05)
          )
      }
      
      # If markdown not set, do normal resizable ggplotly
      # otherwise set the heights and widths
      if (is.null(markdown)) {
        gg2.zoom <- plotly::ggplotly(p2.zoom, height = height, width = width)
        gg2.full <- plotly::ggplotly(p2.full, height = height, width = width)
      } else {
        gg2 <- plotly::ggplotly(p2, height = height, width = width)
      }
      
      # create static ggplot version for download
      p2.zoom <- 
        p2.zoom +
        ggplot2::labs(title = "Mutation probabilities", y = "Mutation probability", x = "Gene",
                      caption = 
                        "Variability in estimates may arise from an imputation process for missing ages.\nThe range of estimates is indicated by error bars or (lower, upper) estimates.\n\nIf the hetero/homogeneity or variant of the gene has not been specified,\nit is assumed to be heterogeneous and of any pathogenic variant."
                      ) +
        ggplot2::annotate("text", x = 5, y = 0.0255, label = "0.025 prob. threshold", color = "red", size = 2) +
        ggplot2::theme_bw() +
        ggplot2::theme(plot.title = element_text(hjust = 0.5), 
                       axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
                       plot.caption = element_text(size = 7, hjust = 0))
      p2.full <- 
        p2.full +
        ggplot2::labs(title = "Mutation probabilities", y = "Mutation probability", x = "Gene",
                      caption = 
                        "Variability in estimates may arise from an imputation process for missing ages.\nThe range of estimates is indicated by error bars or (lower, upper) estimates.\n\nIf the hetero/homogeneity or variant of the gene has not been specified,\nit is assumed to be heterogeneous and of any pathogenic variant."
        ) +
        ggplot2::annotate("text", x = 5, y = 0.05, label = "0.025 prob. threshold", color = "red", size = 2) +
        ggplot2::theme_bw() +
        ggplot2::theme(plot.title = element_text(hjust = 0.5), 
                       axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
                       plot.caption = element_text(size = 7, hjust = 0))
      
      # Set up Plotly layout for carrier probability plot
      f3p2.zoom <- plotly::layout(gg2.zoom,
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
      f3p2.full <- plotly::layout(gg2.full,
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
      f3p2.zoom <- f3p2.zoom %>% plotly::add_annotations(text = "Mutation probabilities",
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
                                      paste0("Non-carrier probability: ", round(current_f3p[nc_position, ]$estimate,
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
      ) %>% plotly::add_annotations(text = "0.025 prob. threshold",
                                    xref = "x",
                                    yref = "y",
                                    align = "center",
                                    x = 5,
                                    y = 0.024,
                                    font = list(size = 10,
                                                color = "red"),
                                    showarrow = FALSE
      ) %>% plotly::config(modeBarButtonsToRemove = c("lasso2d","select2d","pan2d",
                                                      "autoScale2d",
                                                      "zoomIn2d","zoomOut2d",
                                                      "hoverClosestCartesian",
                                                      "hoverCompareCartesian",
                                                      "toggleSpikelines"),
                           displaylogo = FALSE)
      
      f3p2.full <- f3p2.full %>% plotly::add_annotations(text = "Mutation probabilities",
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
                                      paste0("Non-carrier probability: ", round(current_f3p[nc_position, ]$estimate,
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
      ) %>% plotly::add_annotations(text = "0.025 prob. threshold",
                                    xref = "x",
                                    yref = "y",
                                    align = "center",
                                    x = 5,
                                    y = 0.055,
                                    font = list(size = 10,
                                                color = "red"),
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
        f3p2.zoom <- f3p2.zoom %>% plotly::add_annotations(text = 
                                                 paste0("(", round(current_f3p[nc_position, ]$lower, digits = 3), ", ",
                                                        round(current_f3p[nc_position, ]$upper, digits = 3), ")"),
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
        f3p2.full <- f3p2.full %>% plotly::add_annotations(text = 
                                                           paste0("(", round(current_f3p[nc_position, ]$lower, digits = 3), ", ",
                                                                  round(current_f3p[nc_position, ]$upper, digits = 3), ")"),
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
      p2.zoom <- NULL
      p2.full <- NULL
      f3p2.zoom <- NULL
      f3p2.full <- NULL
    }
    
    # Final Plotly output
    if(omit.plots != "both" & omit.plots != "fr"){
      fig <- plotly::layout(plotly::subplot(f3p1.zoom, f3p2.zoom, nrows = 2, 
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
      both = figs,             # plotly contains both carrier probs and cancer risks with zoomed y-axis
      cp.zoom = f3p2.zoom,      # plotly carrier probs zoomed y-axis
      cp.full = f3p2.full,      # plotly carrier probs full y-axis
      fr.zoom = f3p1a.zoom,     # plotly cancer risk zoomed y-axis
      fr.full = f3p1a.full,     # plotly cancer risk full y-axis
      cpStatic.zoom = p2.zoom, # ggplot carrier prob zoomed y-axis
      cpStatic.full = p2.full, # ggplot carrier prob full y-axis
      frStatic.zoom = p1.zoom.fap, # ggplot cancer risks facetted by cancer type and zoomed y-axis
      frStatic.full = p1.full.fap,  # ggplot cancer risks facetted by cancer type and full y-axis
      comparison = p0,         # ggplot cancer risks facetted by cancer type with average person risks and full y-axis
      grows = grows,           # number of grid columns in p0
      gcols = gcols            # number of grid rows in p0
    ))
  }
  
  if (!is.null(markdown) & isTRUE(markdown)) {
    htmltools::tagList(figs)
  }
}

#' Create a ggplot line graph for one cancer that contains a line for the proband's 
#' risk and another for the average person's risk
make.ggline <- function(data, cancer, cancer.colors, age, grows){
  
  # line colors
  gen.pop.color <- "grey"
  user.color <- cancer.colors[which(names(cancer.colors) == cancer)]
  these.colors <- c(user.color, gen.pop.color)
  names(these.colors) <- c(paste0(cancer,".Proband"),
                           paste0(cancer,".Average Person"))

  # point type
  ptType <- 19

  # x-axis ticks for line graphs
  x.ticks <- seq(age, Fam3PRO:::MAXAGE, 10)
  if(Fam3PRO:::MAXAGE - as.numeric(age) <= 5){
    x.ticks <- seq(age, Fam3PRO:::MAXAGE, 1)
  } else if(Fam3PRO:::MAXAGE - as.numeric(age) < 15){
    x.ticks <- seq(age, Fam3PRO:::MAXAGE, 2)
  } else if(Fam3PRO:::MAXAGE - as.numeric(age) < 30){
    x.ticks <- seq(age, Fam3PRO:::MAXAGE, 5)
  }

  # axis text size
  axis.text.size <- 12
  axis.title.size <- 16

  ## legend formatting
  legend.h.pos <- 0.2
  legend.v.pos <- 0.85
  legend.text.size <- 10
  
  # set-up the plot
  p <- 
    ggplot(data = data, aes(x = ByAge,
                            y = estimate,
                            color = interaction(Cancer, Who),
                            shape = Who)) +
    geom_point(size = 3) + 
    geom_line(linewidth = 1.5) +
    scale_x_continuous(breaks = x.ticks) +
    scale_y_continuous(limits = c(0,1)) +
    scale_color_manual(values = these.colors,
                       labels = c("Average Person","Proband")) +
    scale_shape_manual(values = c(NA,ptType),
                       guide = "none") +
    labs(title = element_blank(), x = element_blank(), y = element_blank()) +
    facet_wrap(~ Cancer) +
    theme_bw() +
    theme(axis.text = element_text(size = axis.title.size),
          legend.position = c(legend.h.pos,legend.v.pos),
          legend.text = element_text(size = legend.text.size),
          legend.title = element_blank(),
          strip.text = element_text(size = axis.text.size))
  p
}


#' Evaluate Fam3PRO and Handle Errors and Warnings
#' 
#' Arguments are the same as the Fam3PRO function but fewer
#' @returns either the typical Fam3PRO result or a warning or error message
eval_Fam3PRO <- function(pedigree,
                          model_spec = NULL,
                          genes = NULL,
                          cancers = NULL,
                          max.mut,
                          age.by,
                          use.mult.variants,
                          breakloop,
                          unknown.race,
                          unknown.ancestry,
                          allow.intervention,
                          ignore.proband.germ,
                          iterations,
                          max.iter.tries,
                          random.seed,
                          net){
  tryCatch(
    
    # try to run Fam3PRO
    {
      if(!is.null(model_spec)){
        out <- Fam3PRO(pedigree = pedigree,
                        model_spec = model_spec,
                        max.mut = max.mut,
                        age.by = age.by,
                        use.mult.variants = use.mult.variants,
                        breakloop = breakloop,
                        unknown.race = unknown.race,
                        unknown.ancestry = unknown.ancestry,
                        allow.intervention = allow.intervention,
                        ignore.proband.germ = ignore.proband.germ,
                        iterations = iterations,
                        max.iter.tries = max.iter.tries,
                        random.seed = random.seed,
                        net = net)
      } else if(!is.null(genes) & !is.null(cancers)){
        out <- Fam3PRO(pedigree = pedigree,
                        genes = genes,
                        cancers = cancers,
                        max.mut = max.mut,
                        age.by = age.by,
                        use.mult.variants = use.mult.variants,
                        breakloop = breakloop,
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

#' Capture Fam3PRO console output and send to UI
#' adapted from: https://gist.github.com/jcheng5/3830244757f8ca25d4b00ce389ea41b3
f3pResultsAndConsole <- function(pedigree,
                                model_spec,
                                genes,
                                cancers,
                                max.mut,
                                age.by,
                                use.mult.variants,
                                breakloop,
                                unknown.race,
                                unknown.ancestry,
                                allow.intervention,
                                ignore.proband.germ,
                                iterations,
                                max.iter.tries,
                                random.seed,
                                net) {
  
  ## safely run Fam3PRO to check for errors and warnings
  # pre-defined model case
  if(model_spec != "Custom"){
    f3pErrorProof <- eval_Fam3PRO(pedigree = pedigree,
                                  model_spec = model_spec,
                                  max.mut = max.mut,
                                  age.by = age.by,
                                  use.mult.variants = use.mult.variants,
                                  breakloop = breakloop,
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
    f3pErrorProof <- eval_Fam3PRO(pedigree = pedigree,
                                  genes = genes,
                                  cancers = cancers,
                                  max.mut = max.mut,
                                  age.by = age.by,
                                  use.mult.variants = use.mult.variants,
                                  breakloop = breakloop,
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
  # not an error so re-run to capture Fam3PRO results and console output
  if(!"error" %in% class(f3pErrorProof)){
    
    # different run settings based on if a pre-defined vs custom model was selected
    if(model_spec != "Custom"){
      txt <- 
        capture.output(
          results <- 
            Fam3PRO(
              pedigree = pedigree,
              model_spec = model_spec,
              max.mut = max.mut,
              age.by = age.by,
              use.mult.variants = use.mult.variants,
              breakloop = breakloop,
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
            Fam3PRO(
              pedigree = pedigree,
              genes = genes,
              cancers = cancers,
              max.mut = max.mut,
              age.by = age.by,
              use.mult.variants = use.mult.variants,
              breakloop = breakloop,
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
    call <- paste0(deparse(f3pErrorProof$call), collapse = "\n")
    mssg <- paste0(f3pErrorProof$message, collapse = "\n")
    txt <- paste0("Error:\n\ncall:\n", call, "\nmessage:\n", mssg)
    results <- NULL
  }
  
  # change text color based on message type
  if("error" %in% class(f3pErrorProof)){
    mssg.color <- "red"
  } else {
    mssg.color <- "black"
  }
  
  # persistent console output for one of the results tabs
  removeUI(selector = "#outputContainer") # remove old outputs if they are there
  insertUI("#f3pMessageContainer", 
           where = "beforeEnd",
           ui = div(id = "outputContainer", 
                    p(txt, style = paste0("white-space:pre-wrap;color:", mssg.color))))
  
  # create pop-up only if there were warning or errors
  if(any(c("error", "warning") %in% class(f3pErrorProof))){
    
    # change formatting based on message type
    if("error" %in% class(f3pErrorProof)){
      modal.title <- "Fam3PRO Issued an Error"
      mssg.intro <- "The error message is below:"
    } else {
      modal.title <- "Fam3PRO Issued a Warning"
      mssg.intro <- "The R console output is below which includes the warning message:"
    }
    
    # create modal
    showModal(modalDialog(
      tagList(p(mssg.intro),
              p(txt, style = paste0("white-space:pre-wrap;color:", mssg.color, ";")),
              p("After closing this pop-up, you can view it again on the 'Console Output' tab."),
              p("Check the Fam3PRO Documentation for more information.")),
      title = modal.title,
      footer = tagList(modalButton("OK")),
      easyClose = T
    ))
  }
  
  return(results)
}

