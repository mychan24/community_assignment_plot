############################
# Example 1 - Minimal plot #
############################
rm(list=ls())
library(tidyverse)
source("./comm_assign_plot.R")

subject_comm <- read.csv('./data/sample_sub_sorted_community_mode.txt', header=F)
syslabel <- read.csv("./data/systemlabel.txt")

# Rename subject's conmmunity column
colnames(subject_comm)[1:ncol(subject_comm)] <- seq(.1,10,.1) #  .1% to 10%

g <- comm_assign_plot(subject_comm,
              commlabel =  syslabel, 
              ref_col = "10", 
              col_order = as.character(seq(.1,10,.1)), 
              comm_info=FALSE, 
              show_legend=FALSE)

print(g)

###################################################
# Example 2 - Plot with community info and legend #
###################################################
rm(list=ls())
source("./comm_assign_plot.R")

subject_comm <- read.csv('./data/sample_sub_sorted_community_mode.txt', header=F)
subject_sparse_mode <- read.table("./data/parcel_community.txt", header=T, sep="\t")
syslabel <- read.csv("./data/systemlabel.txt")

subject_sparse_mode[subject_sparse_mode==0] <- 1 # Liang name unassign to 0, but it should be named as 1 for coloring
colnames(subject_comm)[1:ncol(subject_comm)] <- seq(.1,10,.1) #  .1% to 10%

# Combine 0.1%-10% and sparsest & mode
c <- cbind(subject_sparse_mode, subject_comm)

g <- c %>% 
  select(-par_id, -par_vertex) %>%
  comm_assign_plot(comm = ., 
           commlabel = syslabel, 
           ref_col='power_label', 
           col_order = c(rev(names(c)[3:7]), as.character(seq(.1,10,.1)), 'power_label'), 
           comm_info=TRUE, 
           show_legend=TRUE)

print(g)