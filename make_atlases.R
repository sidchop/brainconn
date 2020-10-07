
r <- c(100, 200, 300, 400, 500, 600, 700, 800, 900,1000)
n <- c(7,17)

for (i in 1:length(r)) {
  for (ii in 1:length(n)) {
    x <- read.csv(paste0("https://raw.githubusercontent.com/ThomasYeoLab/CBIG/master/stable_projects/brain_parcellation/Schaefer2018_LocalGlobal/Parcellations/MNI/Centroid_coordinates/Schaefer2018_",r[i],"Parcels_",n[ii],"Networks_order_FSLMNI152_1mm.Centroid_RAS.csv"))
    centroid <- x[,3:5]
    roi <- x$ROI.Name
    roi <- stringr::str_split(roi, pattern = "_", simplify = T)
    ROI.Name <- stringr::str_c(roi[,3],roi[,4], roi[,5], sep = "_", collapse = NULL)
    ROI.Name <- gsub('^_|_$', '',  ROI.Name)
    atlas <- cbind(ROI.Name, centroid, roi[,3], roi[,2], 1:length(ROI.Name))
    colnames(atlas) <- colnames(schaefer300_n7)
    assign(paste0("schaefer",r[i],"_n",n[ii]), atlas)
  }
}


usethis::use_data(schaefer1000_n17, overwrite = T)

x <- matrix(rbinom(1000 * 1000, 1, 0.0001), ncol = 1000, nrow = 1000)
x <- Matrix::forceSymmetric(x)
diag(x) <- 0
brainconn::brainconn(atlas = "schaefer1000_n17", conmat = x,
                     view = "ortho",
                     node.size = 0.2,
                     all.nodes = T)
