## ----setup, include=F---------------------------------------------------------
knitr::opts_chunk$set(eval=TRUE)
devtools::load_all(".")
knitr::opts_knit$set(root.dir = '..')

## ---- out.width='50%'---------------------------------------------------------
library(brainconn)
x <- example_unweighted_undirected
brainconn(atlas ="schaefer300_n7", conmat=x, node.size = 3, view="ortho")

## ---- out.width='50%', warning=FALSE, message=FALSE---------------------------
brainconn(atlas ="schaefer300_n7", conmat=example_unweighted_undirected, view="left", node.size = 3, node.color = "hotpink", edge.width = 2, edge.color="darkblue", edge.alpha = 0.8, all.nodes = T, show.legend = F)

## ---- out.width='50%',  warning=FALSE, message=FALSE--------------------------
x <- example_weighted_undirected

brainconn(atlas ="schaefer300_n7", conmat=x, node.size = 5,view="bottom", scale.edge.width = c(1,3), background.alpha = 0.4, show.legend = F)

## ---- out.width='50%',  warning=FALSE, message=FALSE--------------------------

brainconn(atlas ="schaefer300_n7", conmat=example_weighted_undirected, node.size = 7,view="bottom", edge.width = 2, edge.color.weighted = T, show.legend = T)

## ---- out.width='50%',  warning=FALSE, message=FALSE--------------------------
x <- example_unweighted_directed

brainconn(atlas ="schaefer300_n7", conmat=x, node.size = 4, view="right", edge.alpha = 0.6)

## ---- out.width='50%',  warning=FALSE, message=FALSE--------------------------
x <- example_weighted_directed

brainconn(atlas ="schaefer300_n7", conmat=x, view="front", edge.color.weighted=T)

## ---- out.width='50%',  warning=FALSE, message=FALSE--------------------------
#x <- read.csv("/Users/sidchopra/Dropbox/Sid/R_files/brainconn/data/example/example_unweighted_undirected.txt", header = F)

#brainconn(atlas ="Stages_melbBrain", conmat=x, labels = T, label.size = 2)

## ---- out.width='50%',  warning=FALSE, message=FALSE--------------------------
#x_igraph <- igraph::graph_from_adjacency_matrix(as.matrix(x)) #convert connectivity matrix into an graph object.
#d <- igraph::degree(x_igraph) #use igraph::degree to obtain a vector of nodal degree
#d <- d[d != 0] #remove nodes with no edges 
#brainconn(atlas ="Stages_melbBrain", conmat=x, node.size = d)

## ---- out.width='50%',  warning=FALSE, message=FALSE--------------------------
#p <- brainconn3D(atlas ="Stages_melbBrain", conmat=x, show.legend = F)
#p

## ---- out.width='50%',  warning=FALSE, message=FALSE--------------------------
#p <- brainconn3D(atlas ="Stages_melbBrain", conmat=x, edge.width = 6, edge.color = "green", node.size = 6, node.color = "red", show.legend = F)
#p

## ---- out.width='50%',  warning=FALSE, message=FALSE--------------------------
#p <- brainconn3D(atlas ="Stages_melbBrain", conmat=x, edge.width = 3, edge.color = "brown", node.size = 3, d.factor = 1.3, all.nodes = T, opacity = 0.1, show.legend = F)
#p

