
library(ggconn)
#library(ggraph)
nparc <- 300

z <- igraph::erdos.renyi.game(300, 0.009, type="gnp")
plot(z)

y <- matrix(sample(0:10,nparc*nparc, replace=TRUE, prob=c(0.99999,.00001,.00001,.00001,.00001,.00001,.00001,.00001,.00001,.00001,.00001)),nparc,nparc)
diag(x) <- 0



write.csv(as.matrix(y), file = "data/example/example_weighted_directed.txt", row.names = F)

require(Matrix)
x <- forceSymmetric(as.matrix(x))

isSymmetric(as.matrix(x))
rownames(x) <- colnames(x)
x <- read.csv("data/example/temp", header = T)
x <- read.csv("../STAGES_fmri/data/swe_validation/tfnbs/clust_TFCE_a2.txt", header = F)
pheatmap::pheatmap(x, cluster_rows = FALSE,
                   cluster_cols = FALSE)

diag(y) <- 0
isSymmetric.matrix(as.matrix(x))

x[lower.tri(x)] = t(x)[lower.tri(x)]
x <- Matrix::forceSymmetric(as.matrix(x))

#degree for aspree
#d <- c(2,7,3,2,7,1,9,4,1,2,7,3,1,2,2,1,4,1,5,3,1,1,1,2,1,3,1,2,2,2,2,2,5,1,1)
#d_adj <- d+5
#d_scale <- scale(d)+4
nparc <- 1000
x <- matrix(sample(0:1,nparc*nparc, replace=TRUE, prob=c(0.9999,.0001)),nparc,nparc)
y <- matrix(sample(0:10,nparc*nparc, replace=TRUE, prob=c(0.99999,.00001,.00001,.00001,.00001,.00001,.00001,.00001,.00001,.00001,.00001)),nparc,nparc)

ggconn(atlas ="schafer1000_n7",
       conmat=x,
             all.nodes = T,
       #broken        interactive = F,
#       node.color = "",
       node.size= 1,
             edge.color.weighted=T,
          edge.width=0.3,
       #scale.edge.width = c(1,2),
       edge.alpha=1,
       view = "top",
       #         labels = F,
                show.legend = T,
       #         label.size = 2,
       #          background.alpha = 0.4
)


ggconn3D(atlas = "Stages_melbBrain",
         conmat = x,

        node.color = "network",
        show.legend = T
          )


