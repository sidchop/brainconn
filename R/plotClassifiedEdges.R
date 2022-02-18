################
# This function is useful for understanding the results of NBS output.
# Given a classification of nodes into a subset of networks (e.g., DMN,
#                                                            % FPN, CON, etc), it counts the number of edges present in a network both
# within and between networks and optionally plots the output as a matrix.
#
# The results are counted only at the level of binary topology
#
# -------
# % INPUTS:
# % -------
# % adj       - binary N*N adjacency matrix, where N is number of nodes. This matrix
#           could be the output of an NBS analysis; i.e., the matrix stored
#           in nbs.NBS.con_may{1}.
#
# ids       - N*1 vector of network ids. Each network should be represented
#           as a unique number. Each row is a different node.
#
# plotFig   - set to 1 if you want to plot out; 2 for outPC; 3 for outNorm;
#           0 otherwise. Default is 0.
#
# labels    - M*1 cell, where M is the number of different networks. Each
#             cell contains a string representing the
#            network name. This is used to label the axes of the plot.
#
# -------
# % OUTPUTS:
# % -------
# % out       - M*M adjacency matrix indicating the number of edges between
#           each network pair. Values along diagonal represent edges within
#           a network.
#           NOTE: sum(sum(triu(out))) = sum(sum(adj))/2; in other
#           words, the number of unique edges in adj equals the number of
#           edges in the upper triangle (including the diagonal) of out.
#
# outPC     - The out matrix normalized by the number of unqiue edge to
#           yield the proportion of edges for each category.
#
# outNorm   - this matrix is normalized separetly for each pair of regions
#           by the total number of edges between them. The values thus
#           represent the connection density of the subgraph of nodes
#           belonging to a given pair of modules. This normalization
#           accounts for differences in the size of modules, which can bias
#           the results of outPC.
#
#
# Alex Fornito, Monash University, Oct 2016
# Sidhant Chopra, Monash University, Oct 2020
# Updates:
# % Feb 2017 added outNorm as output; options to plot different output
# matrices
#
# May 2020 (Sidhant Chopra)
# 1) Converted function from matlab to r
#
#2)Fixed the normalisation factor for outNorm matrix. Previously, the normalisaion
# was taking into account total number of edges between AND within two nodes (i.e. nSubTot = length(inds_i) + length(inds_j) ;
# (nSubTot^2 - nSubTot)/2). For between network connections, the normFactor should probably be computed by multiplying the numbers of regions
# in each pair of networks: length(inds_i)*length(inds_j) and for within network connections using length(inds_i)^2)-(length(inds_i))/2.

#=========================================================================#

plotClassifiedEdges <- function(adj, ids, labels){


  N <- dim(adj)[1]   # number of nodes
  unq <- unique(ids) # number of unique networks
  colnames(unq) <- NULL
  #initialise the three output mats
  out <- outPC <- outNorm <- matrix(nrow=dim(unq)[1], ncol=dim(unq)[1])


  #  sort edges
  diag(adj) <- 0

  for (i in 1:dim(unq)[1]) {
    for (j in i:dim(unq)[1]) {
      index_i  <- which(ids==i)
      index_j <- which(ids==j)

      tempsum <- 0
      tempsum <-  sum(adj[c(index_i),  c(index_j)])
      if(i==j) {tempsum <- tempsum/2}

      out[i,j] <- tempsum
      out[j,i] <-   tempsum


      outPC[i,j] <- tempsum/sum(adj[upper.tri(adj, diag = T)])
      outPC[j,i] <- tempsum/sum(adj[upper.tri(adj, diag = T)])

      normFactor <- length(index_i)*length(index_j)

      if(i==j) {normFactor <- ((length(index_i)^2)-length(index_i))/2} #remove diagnal elements with computing within network connections

      outNorm[i,j] <- tempsum/normFactor
      outNorm[j,i] <- tempsum/normFactor

    }
  }
  output <- list()
  output[[1]] <- out
  output[[2]] <- outPC
  output[[3]] <- outNorm
  rownames(output[[1]]) <-  colnames(output[[1]]) <- t(labels)
  rownames(output[[2]]) <-  colnames(output[[2]]) <- t(labels)
  rownames(output[[3]]) <-  colnames(output[[3]]) <- t(labels)#add in labels
  return(output)
}


