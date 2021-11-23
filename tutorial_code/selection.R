# R Code: Lode node and edge data frames
library(tidyverse)
library(igraph)
library(amen)

edge = read_csv("../data/edge_selection.csv")
node = read_csv("../data/node_selection.csv")

# R Code: Calculate adjacently matrix and relational covariates 
G = graph.data.frame(edge) 
adj_matrix = as.matrix(get.adjacency(G,attr = "approach_count_1"))
adj_matrix_0 = as.matrix(get.adjacency(G,attr = "approach_count_0"))
x_d = as.matrix(dist(node$negative_affect))
x_d2 = array(cbind(adj_matrix_0, x_d), dim = c(dim(adj_matrix),2))
             
# R Code: run ame model without latent factors as a baseline
# use "nscan" parameter in ame function to reduce the run time
# if necessary

fit_SRM_dyad_only = ame(adj_matrix, Xd=x_d, family="nrm") 
fit_SRM = ame(adj_matrix, Xr = node$negative_affect, Xc = node$negative_affect, Xd=x_d, family="nrm") 
fit_SRM_ar = ame(adj_matrix, Xr = node$negative_affect, Xc = node$negative_affect, Xd=x_d2, family="nrm") 
fit_LFM = ame(adj_matrix, Xr = node$negative_affect, Xc = node$negative_affect, Xd=x_d2, family="nrm", R=2) 

# R Code: run ame model without latent factors as a baseline
summary(fit_SRM_dyad_only)
summary(fit_SRM)
summary(fit_SRM_ar)
summary(fit_LFM)

