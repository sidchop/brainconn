
###IGNORE THIS SCRIPT - JUST FOR TESTING CODE
##TEST#
custom <- read.csv("data/custom_atlas_example.csv")
check_atlas(custom)
colnames(custom) <- c("ROI.Name", "x.mni", "y.mni", "z.mni", "network", "hemi", "index")



load("data/stages_study.rda")
write.csv(stages_study, "data/custom_atlas_example.csv", row.names = F, quote = F)

library(brainconn)
#library(ggraph)
vignette("brainconn")

nparc <- 316

z <- igraph::erdos.renyi.game(59, 0.009, type="gnp")
plot(z)

y <- matrix(sample(0:10,nparc*nparc, replace=TRUE, prob=c(0.99999,.00001,.00001,.00001,.00001,.00001,.00001,.00001,.00001,.00001,.00001)),nparc,nparc)
diag(y) <- 0



write.csv(as.matrix(y), file = "data/example/example_weighted_directed.txt", row.names = F)

require(Matrix)
y <- forceSymmetric(as.matrix(y))
diag(x) <- 0
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
nparc <- 90
x <- matrix(sample(0:1,nparc*nparc, replace=TRUE, prob=c(0.99,.01)),nparc,nparc)
y <- matrix(sample(0:10,nparc*nparc, replace=TRUE, prob=c(0.99999,.00001,.00001,.00001,.00001,.00001,.00001,.00001,.00001,.00001,.00001)),nparc,nparc)
nparc <- 4
x <- matrix(sample(0:1,nparc*nparc, replace=TRUE, prob=c(0.10,.90)),nparc,nparc)

x  <- read.csv(
"~/Dropbox/Sid/R_files/STAGES_fmri/data/swe_validation_contrasts/gmr/fwe_contrast_corrected/illness_effect_age_long_fixed/3_obs_comp.csv",
              header = F)
#as.igraph(qgraph(y))
#x <- vec_2_mat(x, 316, 0)
#y <- binarize(x = x, threshold = 15)

brainconn(atlas = custom,
          conmat=y,
          all.nodes = F,
          #       node.color = "",
          node.size= 2,
                edge.color.weighted=T,
          edge.color = "black",
          edge.width=2,
          #scale.edge.width = c(1,2),
          edge.alpha=1,
          view = "ortho",
          labels = F,
          show.legend = F,
                  label.size = 3,
                 background.alpha = 0.2
)

as.igraph(qgraph::qgraph(x))
x <- read.csv("data/example/example_unweighted_undirected.txt", header = F)
p <- brainconn3D(atlas ="Stages_melbBrain", conmat=x, show.legend = F)
p


x_igraph <- igraph::graph_from_adjacency_matrix(as.matrix(x), mode = c("undirected"))

igraph::gsize(x_igraph)
degree <- igraph::degree(x_igraph)

degree <- degree[degree != 0]
degree <- log(degree)*2
load("~/Dropbox/Sid/R_files/brainconn/data/stages_study.rda")

View(degree)


brainconn3D(atlas = custom,
         conmat = y,
        node.color = "network",
        show.legend = F,
        edge.width = 2

          )

devtools::use_vignettes()
devtools::build_vignettes()

list_atlases()


#####test adding a atlas
library(brainconn)
ROI.Name <- c("boosap", "bdoop", "abeep", "bodop")
x.mni <- as.integer(c(-38.65, -23.91, 8.16, 0.43))
y.mni <- as.integer(c(-5.68, 3.86, 51.67, 32.73))
z.mni <-  as.integer(c(50.94, 2.40, -7.13, 35.46))
network <- c("cat", "cat", "dog", "dog")


catlas_plus <- data.frame(ROI.Name, x.mni, y.mni, z.mni, network)

catlas_plus <- as.data.frame(cbind(ROI.Name, x.mni, y.mni, z.mni, network))


colnames(catlas_plus) <- c("ROI.Name", "x.mni", "y.mni", "z.mni", "network")
names(catlas_plus)

add_atlas(catlas_plus)
list_atlases()

data()



