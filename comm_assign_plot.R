# DESCRIPTION:
#     Make community assignent plot (square plot)
# 
# Inputs:   comm,         data frame of community assignment (each column is an assignment)
#           commlabel,    data frame of community and color label
#           ref_col,      column name of reference solumn to be sorted by 
#           col_order,    colnames in the order that should be on the x-axis
#           comm_info,    logical, TRUE (Default), show communtiy info on top of graph
#           show_legend,  logical, TRUE (Default), show community color legend
#
# Ouput:    ggplot object
#
# Example Usage:
#        comm %>%
#          select(-par_id, -par_vertex) %>%
#          comm_assign_plot(comm = .,
#                           commlabel = commlabel,
#                           ref_col = 'power_label',
#                           col_order = c(rev(names(c)[3:7]), as.character(seq(.1,10,.1)), 'power_label'))
# #########################################################################
# myc, 2019/09/04 - add Example Usage
# myc, UTD 2019/08/29- initial
# #########################################################################
comm_assign_plot = function(comm, commlabel, ref_col, col_order, comm_info=TRUE, show_legend=TRUE) {
  library(ggplot2)
  library(tidyverse)

  ################
  # Setup data
  ################
  sorting_order <- order(comm[[ref_col]]) 
  sc <- comm[sorting_order,]
  sc$ReorderRef <- order(sc[[ref_col]]) # extract order of reordered reference and save it as a column
  
  # wide to long format
  scc <- sc %>%
    gather("td", "CommID", -ReorderRef)
  
  scc$td <- factor(scc$td, levels = col_order) # Order the Edge Density factor levels
  
  scc$CommID_reassigned <- scc$CommID 
  scc$CommID_reassigned[scc$CommID>41] <- 1 # unassigne
  scc$CommID_reassigned <- factor(scc$CommID_reassigned)
  
  
  ################
  # Setup color
  ################
  comm_in_sub <- is.element(commlabel$CommID, scc$CommID_reassigned)
  syscolor <- as.character(commlabel$hex[comm_in_sub])
  names(syscolor) <- as.character(commlabel$CommID[comm_in_sub])
  
  ########
  # Plot #  
  ########
  g <- scc %>%
    ggplot(aes(x = td, y = ReorderRef)) +
      geom_raster(aes(fill=CommID_reassigned)) +
      scale_y_reverse() +
      scale_x_discrete(limits=rev(levels(scc$td))) +
      xlab("Edge Density (%) / Label") +
      ylab(sprintf("Parcel ordered by %s",ref_col)) +
      scale_fill_manual(values = syscolor) +
      ggtitle(sprintf("Sorted by %s",ref_col)) +
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 90, hjust = 1, size=8))
  
  if(comm_info){
    u_comm <- scc %>%
      group_by(td) %>%
      summarise(count=n_distinct(CommID))
      u_comm$y <- -8 
      
      u_comm_5p <- data.frame(td=levels(scc$td), 
                              y=-33,
                              count=sapply(X = levels(scc$td),
                                           FUN = function(x){sum(table(scc$CommID[scc$td==x]) >= 5)}))
      
      u_comm_1 <- data.frame(td=levels(scc$td),
                             y=-53, 
                             count=sapply(X = levels(scc$td),
                                          FUN = function(x){sum(table(scc$CommID[scc$td==x]) == 1)}))
      
      clabel <- data.frame(y=c(-8,-33,-53), 
                           td=11, 
                           label=c("N Comm","N Comm >=5","N Comm 1 node"))
    
    g <- g + 
      # add community count info on top
      geom_text(data=u_comm, aes(x=td, y=y, label=count), angle = 90, size=3) +
      geom_text(data=u_comm_5p, aes(x=td, y=y, label=count), angle = 90, size=3) +
      geom_text(data=u_comm_1, aes(x=td, y=y, label=count), angle = 90, size=3) +
      geom_hline(yintercept = c(-23,-43), color="grey") + 
      geom_text(data=clabel, aes(y=y, label=label), x=0,
                hjust = 1,
                size = 3) +  
      coord_cartesian(# xlim = c(0.1, 10), # This focuses the x-axis on the range of interest
        clip = 'off') +   # This keeps the labels from disappearing
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 90, hjust = 1, size=8),
            plot.margin = unit(c(1,1,1,3), "lines")) # This widens the left margin
  }
  
  if(show_legend){
    g <- suppressMessages(g + scale_fill_manual(values = syscolor, 
                                 name = "Community",
                                 labels = as.character(commlabel$ShortName[comm_in_sub]))) +
      theme(legend.position = "right")
    
  }
  
  return(g)
}