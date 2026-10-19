# Simulated functional connectivity matrix for the schaefer300_n7 atlas, used
# in the brainconn_matrix() and brainconn_circle() examples. It is NOT real
# data: values are network block means (loosely based on typical resting-state
# patterns) plus noise, with same-network pairs slightly weaker between
# hemispheres than within a hemisphere.
devtools::load_all()
set.seed(2024)
atlas <- schaefer300_n7
nets <- c("Vis", "SomMot", "DorsAttn", "SalVentAttn", "Limbic", "Cont", "Default")
block <- matrix(0.05, 7, 7, dimnames = list(nets, nets))
diag(block) <- c(0.55, 0.50, 0.40, 0.40, 0.30, 0.35, 0.45)
pair <- function(a, b, v) { block[a, b] <<- v; block[b, a] <<- v }
pair("Vis", "SomMot", 0.20)
pair("Vis", "DorsAttn", 0.25)
pair("SomMot", "SalVentAttn", 0.25)
pair("DorsAttn", "SalVentAttn", 0.20)
pair("DorsAttn", "Cont", 0.15)
pair("SalVentAttn", "Cont", 0.20)
pair("Cont", "Default", 0.15)
pair("Limbic", "Default", 0.15)
pair("Default", "DorsAttn", -0.25)
pair("Default", "SalVentAttn", -0.20)
pair("Default", "SomMot", -0.10)
pair("Default", "Vis", -0.10)

n <- nrow(atlas)
net <- match(atlas$network, nets)
fc <- block[net, net] + matrix(rnorm(n * n, sd = 0.12), n, n)
fc[outer(atlas$hemi, atlas$hemi, "!=") & outer(net, net, "==")] <-
  fc[outer(atlas$hemi, atlas$hemi, "!=") & outer(net, net, "==")] - 0.08
fc <- (fc + t(fc)) / 2
fc <- pmin(pmax(fc, -1), 1)
diag(fc) <- 1
example_fc_schaefer300 <- round(fc, 2)
usethis::use_data(example_fc_schaefer300, overwrite = TRUE, compress = "xz")
