
###IGNORE THIS SCRIPT - JUST FOR TESTING CODE
##TEST#
schafer300_17_cog_lang <- read.csv("Schafer300_17_cog_lang.csv")
usethis::use_data(schafer300_17_cog_lang, schafer300_17_cog_lang)

library(brainconn)
#library(ggraph)
vignette("brainconn")

nparc <- 300

z <- igraph::erdos.renyi.game(59, 0.009, type="gnp")
plot(z)

y <- matrix(sample(0:10,nparc*nparc, replace=TRUE, prob=c(0.99999,.00001,.00001,.00001,.00001,.00001,.00001,.00001,.00001,.00001,.00001)),nparc,nparc)
diag(x) <- 0



write.csv(as.matrix(y), file = "data/example/example_weighted_directed.txt", row.names = F)

require(Matrix)
x <- forceSymmetric(as.matrix(x))

isSymmetric(as.matrix(x))
rownames(x) <- colnames(x)
x <- read.csv("data/example/temp", header = T)
x <- read.csv("../STAGES_fmri/data/swe_validation_contrasts/gmr/a2_illness_effect/clust_gmr_illness_0.999_a2.txt", header = F)
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
nparc <- 59
x <- matrix(sample(0:1,nparc*nparc, replace=TRUE, prob=c(0.99,.01)),nparc,nparc)
y <- matrix(sample(0:10,nparc*nparc, replace=TRUE, prob=c(0.99999,.00001,.00001,.00001,.00001,.00001,.00001,.00001,.00001,.00001,.00001)),nparc,nparc)


x  <- read.csv(
"~/Dropbox/Sid/R_files/STAGES_fmri/data/swe_validation_contrasts/gmr/fwe_contrast_corrected/a3/medication_effect_conj/1.301_obs_comp.csv",
              header = F)
#as.igraph(qgraph(y))
#x <- vec_2_mat(x, 316, 0)
#y <- binarize(x = x, threshold = 15)

brainconn(atlas ="schafer300_17_cog_lang",
          conmat=x,
          all.nodes = F,
          #broken        interactive = F,
          #       node.color = "",
          node.size= 3,
          #      edge.color.weighted=T,
          edge.color = "black",
          edge.width=0.3,
          #scale.edge.width = c(1,2),
          edge.alpha=1,
          view = "top",
          labels = F,
          show.legend = T,
                  label.size = 3,
          #          background.alpha = 0.4
)

as.igraph(qgraph::qgraph(x))
x <- read.csv("data/example/example_unweighted_undirected.txt", header = F)
p <- brainconn3D(atlas ="Stages_melbBrain", conmat=x, show.legend = F)
p


x_igraph <- igraph::graph_from_adjacency_matrix(as.matrix(x))

degree <- igraph::degree(x_igraph)

degree <- degree[degree != 0]
degree <- log(degree)*2
load("~/Dropbox/Sid/R_files/brainconn/data/Stages_melbBrain.rda")

View(degree)


brainconn3D(atlas = "Stages_melbBrain",
         conmat = x,
        node.color = "network",
        show.legend = T
          )
devtools::use_vignettes()
devtools::build_vignettes()
