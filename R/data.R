#' Simulated functional connectivity matrix (Schaefer 300, 7 networks)
#'
#' A simulated (not real) 300 x 300 functional connectivity matrix for the
#' \code{schaefer300_n7} atlas, for trying out \code{\link{brainconn_matrix}},
#' \code{\link{network_matrix}} and other functions on a dense, signed matrix.
#' Values are network block means, loosely based on typical resting-state
#' patterns (e.g. the default network is anti-correlated with the attention
#' networks), plus noise. The diagonal is 1. See \code{data-raw/} in the
#' package source for how it was made.
#'
#' @format A 300 x 300 symmetric numeric matrix with values between -1 and 1,
#'   in the region order of \code{schaefer300_n7}.
"example_fc_schaefer300"

#' Schaefer cortical parcellations
#'
#' Cortical parcellations of Schaefer et al. (2018) with 100 to 1000 regions,
#' each region labelled with one of the 7 or 17 Yeo et al. (2011) resting-state
#' networks (\code{_n7} and \code{_n17} versions). \code{schaefer800_n17} is not
#' included. Coordinates are region centroids in MNI space.
#'
#' @format A data.frame with one row per region and columns:
#' \describe{
#'   \item{ROI.Name}{region name}
#'   \item{x.mni, y.mni, z.mni}{centroid in MNI space (mm)}
#'   \item{network}{Yeo 7 or 17 network}
#'   \item{hemi}{hemisphere, "LH" or "RH"}
#'   \item{index}{region number}
#'   \item{network7}{(17-network versions only) the Yeo 7 network each 17-network
#'     belongs to, e.g. "DefaultA" is in "Default"; useful as
#'     \code{brainconn_circle(group = "network7")}}
#' }
#' @source Schaefer A, et al. (2018). Local-global parcellation of the human
#'   cerebral cortex from intrinsic functional connectivity MRI. Cerebral Cortex,
#'   28(9), 3095-3114. Yeo BTT, et al. (2011). The organization of the human
#'   cerebral cortex estimated by intrinsic functional connectivity. Journal of
#'   Neurophysiology, 106(3), 1125-1165.
#' @name schaefer_atlases
#' @aliases schaefer100_n7
#' @aliases schaefer100_n17
#' @aliases schaefer200_n7
#' @aliases schaefer200_n17
#' @aliases schaefer300_n7
#' @aliases schaefer300_n17
#' @aliases schaefer400_n7
#' @aliases schaefer400_n17
#' @aliases schaefer500_n7
#' @aliases schaefer500_n17
#' @aliases schaefer600_n7
#' @aliases schaefer600_n17
#' @aliases schaefer700_n7
#' @aliases schaefer700_n17
#' @aliases schaefer800_n7
#' @aliases schaefer900_n7
#' @aliases schaefer900_n17
#' @aliases schaefer1000_n7
#' @aliases schaefer1000_n17
#' @docType data
#' @keywords datasets
NULL

#' Schaefer 300 (17 networks): cognition and language regions
#'
#' A 59-region subset of the \code{schaefer300_n17} parcellation: the regions of
#' the Salience/Ventral Attention A, Control A and Temporal Parietal networks.
#'
#' @format A data.frame with 59 rows and columns \code{ROI.Name},
#'   \code{x.mni}, \code{y.mni}, \code{z.mni}, \code{network}, \code{hemi} and
#'   \code{index}; see \code{\link{schaefer_atlases}}.
#' @source See \code{\link{schaefer_atlases}}.
"schaefer300_17_cog_lang"

#' Automated Anatomical Labeling (AAL) atlases
#'
#' The AAL atlas of Tzourio-Mazoyer et al. (2002): \code{aal116} has all 116
#' regions, including 26 cerebellar regions; \code{aal90} has the 90 cerebral
#' regions. Regions are grouped into lobes in the \code{network} column.
#'
#' @format A data.frame with one row per region and columns \code{ROI.Name},
#'   \code{x.mni}, \code{y.mni}, \code{z.mni} (MNI centroid, mm), \code{network}
#'   (lobe), \code{hemi} and \code{index}.
#' @source Tzourio-Mazoyer N, et al. (2002). Automated anatomical labeling of
#'   activations in SPM using a macroscopic anatomical parcellation of the MNI
#'   MRI single-subject brain. NeuroImage, 15(1), 273-289.
#' @name aal_atlases
#' @aliases aal90 aal116
#' @docType data
#' @keywords datasets
NULL

#' Desikan-Killiany atlases
#'
#' \code{dk68}: the 68 cortical regions of the Desikan-Killiany atlas
#' (Desikan et al., 2006). \code{dkt62}: the 62 cortical regions of the
#' Desikan-Killiany-Tourville atlas (Klein & Tourville, 2012).
#' \code{dk82_aspree}: the 68 Desikan-Killiany regions plus 14 subcortical
#' regions, with Yeo network labels, as used in the ASPREE study; it also has
#' an \code{X} column and a \code{Degree_AllCovars} column from that study.
#'
#' @format A data.frame with one row per region and columns \code{ROI.Name},
#'   \code{x.mni}, \code{y.mni}, \code{z.mni} (MNI centroid, mm), \code{network},
#'   \code{hemi} and \code{index}.
#' @source Desikan RS, et al. (2006). An automated labeling system for
#'   subdividing the human cerebral cortex on MRI scans into gyral based regions
#'   of interest. NeuroImage, 31(3), 968-980. Klein A, Tourville J (2012). 101
#'   labeled brain images and a consistent human cortical labeling protocol.
#'   Frontiers in Neuroscience, 6, 171.
#' @name dk_atlases
#' @aliases dk68 dkt62 dk82_aspree
#' @docType data
#' @keywords datasets
NULL

#' Craddock 200-region atlas
#'
#' The 200-region functional parcellation of Craddock et al. (2012), with
#' regions grouped by lobe or structure in the \code{network} column.
#'
#' @format A data.frame with 200 rows and columns \code{ROI.Name},
#'   \code{x.mni}, \code{y.mni}, \code{z.mni} (MNI centroid, mm), \code{network},
#'   \code{hemi} and \code{index}.
#' @source Craddock RC, et al. (2012). A whole brain fMRI atlas generated via
#'   spatially constrained spectral clustering. Human Brain Mapping, 33(8),
#'   1914-1928.
"craddock200"

#' Gordon 333-region atlas
#'
#' The 333-region cortical parcellation of Gordon et al. (2016), with the
#' community (network) of each region.
#'
#' @format A data.frame with 333 rows and columns \code{ROI.Name},
#'   \code{x.mni}, \code{y.mni}, \code{z.mni} (MNI centroid, mm), \code{network}
#'   and \code{index}.
#' @source Gordon EM, et al. (2016). Generation and evaluation of a cortical area
#'   parcellation from resting-state correlations. Cerebral Cortex, 26(1),
#'   288-303.
"gordon_333"

#' Shen atlases
#'
#' \code{shen_268}: the 268-region whole-brain functional parcellation of Shen
#' et al. (2013), with network numbers in the \code{network} column.
#' \code{shen_368}: a 368-region version, with regions labelled by hemisphere,
#' subcortex and cerebellum in the \code{network} column.
#'
#' @format A data.frame with one row per region and columns \code{ROI.Name},
#'   \code{x.mni}, \code{y.mni}, \code{z.mni} (MNI centroid, mm), \code{network}
#'   and \code{index}.
#' @source Shen X, Tokoglu F, Papademetris X, Constable RT (2013).
#'   Groupwise whole-brain parcellation from resting-state fMRI data for network
#'   node identification. NeuroImage, 82, 403-415.
#' @name shen_atlases
#' @aliases shen_268 shen_368
#' @docType data
#' @keywords datasets
NULL

#' STAGES study atlas
#'
#' A custom 316-region parcellation made for the STAGES study (Chopra et al.,
#' JAMA Psychiatry): 284 cortical regions labelled with the 7 Yeo networks,
#' and the 32 subcortical regions of the Melbourne Subcortex Atlas at scale II
#' (Tian et al., 2020).
#'
#' @format A data.frame with 316 rows and columns \code{ROI.Name},
#'   \code{x.mni}, \code{y.mni}, \code{z.mni} (MNI centroid, mm), \code{network},
#'   \code{hemi} and \code{index}.
#' @source Tian Y, Margulies DS, Breakspear M, Zalesky A (2020). Topographic
#'   organization of the human subcortex unveiled with functional connectivity
#'   gradients. Nature Neuroscience, 23, 1421-1432.
"stages_study"

#' Example custom atlas
#'
#' An example of a custom atlas with only the required columns, for use with
#' \code{\link{check_atlas}} and the plotting functions (see
#' \code{vignette("brainconn")}).
#'
#' @format A data.frame with 300 rows and columns \code{ROI.Name},
#'   \code{x.mni}, \code{y.mni} and \code{z.mni}.
"custom_atlas_example"

#' Example connectivity matrices
#'
#' Small example connectivity matrices for the \code{schaefer300_n7} atlas:
#' binary or weighted (weights 1 to 10), and undirected (symmetric) or
#' directed. Each has only a few edges (11 to 26). For a dense matrix, see
#' \code{\link{example_fc_schaefer300}}.
#'
#' @format A 300 x 300 data.frame, in the region order of \code{schaefer300_n7}.
#' @name example_matrices
#' @aliases example_unweighted_undirected
#' @aliases example_weighted_undirected
#' @aliases example_unweighted_directed
#' @aliases example_weighted_directed
#' @docType data
#' @keywords datasets
NULL

#' ICBM152 brain surface and background images
#'
#' \code{ICBM152_mesh_vb} and \code{ICBM152_mesh_it} are the vertices and
#' triangles of the ICBM152 brain surface mesh distributed with BrainNet Viewer
#' (81924 vertices, 163840 triangles), used as the default brain surface by
#' \code{\link{brainconn}} and \code{\link{brainconn3D}}. The
#' \code{ICBM152_top}, \code{_bottom}, \code{_left}, \code{_right},
#' \code{_front} and \code{_back} images are the backgrounds of
#' \code{\link{brainconn2d}}.
#'
#' @format \code{ICBM152_mesh_vb}: a 4 x 81924 matrix of vertex coordinates
#'   (x, y, z in MNI space, and 1). \code{ICBM152_mesh_it}: a 3 x 163840 matrix
#'   of 1-based vertex indices, one triangle per column. The images are RGBA
#'   arrays (height x width x 4) with values from 0 to 1.
#' @source Xia M, Wang J, He Y (2013). BrainNet Viewer: a network visualization
#'   tool for human brain connectomics. PLoS ONE, 8(7), e68910.
#' @name ICBM152
#' @aliases ICBM152_mesh_vb
#' @aliases ICBM152_mesh_it
#' @aliases ICBM152_top
#' @aliases ICBM152_bottom
#' @aliases ICBM152_left
#' @aliases ICBM152_right
#' @aliases ICBM152_front
#' @aliases ICBM152_back
#' @docType data
#' @keywords datasets
NULL

#' Rat brain: surface mesh and atlas (Waxholm Space)
#'
#' A rat brain surface and a 66-region atlas in Waxholm Space, from the
#' Waxholm Space atlas of the Sprague Dawley rat brain, version 4 (Kleven et
#' al. 2023). Use them together, e.g.
#' \code{brainconn(rat_atlas, conmat, mesh = rat_mesh)}.
#'
#' \code{rat_mesh} was made from the atlas labels (at 78 micrometre resolution,
#' without the cranial nerves, spinal cord and inner ear), smoothed and
#' simplified to 40,000 triangles. \code{rat_atlas} has 33 regions per
#' hemisphere: cortical areas, the hippocampal region, olfactory bulb,
#' amygdala and septum, basal ganglia, thalamic groups, hypothalamus, midbrain,
#' pons and cerebellum; each region's coordinates are the centre of its surface
#' in each hemisphere. See \code{data-raw/animal_meshes.R} in the package
#' source for how they were made.
#'
#' Coordinates are in Waxholm Space: millimetres, with x to the right, y to the
#' front and z up, and the origin at the decussation of the anterior commissure.
#'
#' @format \code{rat_mesh}: a mesh (see \code{\link{read_brain_mesh}}) with
#'   \code{vertices} and \code{faces}. \code{rat_atlas}: a data.frame with 66
#'   rows and columns \code{ROI.Name} (e.g. "L_M" for the left motor cortex),
#'   \code{x.mni}, \code{y.mni}, \code{z.mni} (Waxholm Space, mm),
#'   \code{network} (anatomical group), \code{hemi}, \code{name} (full region
#'   name), \code{whs_id} (Waxholm structure id) and \code{index}.
#' @source Kleven H, et al. (2023). Waxholm Space atlas of the rat brain: a 3D
#'   atlas supporting data analysis and integration. Nature Methods, 20,
#'   1822-1829. \doi{10.1038/s41592-023-02034-3}. Papp EA, et al. (2014).
#'   Waxholm Space atlas of the Sprague Dawley rat brain. NeuroImage, 97,
#'   374-386. \doi{10.1016/j.neuroimage.2014.04.001}. Licence: CC BY 4.0.
#' @name rat_brain
#' @aliases rat_mesh rat_atlas
#' @docType data
#' @keywords datasets
NULL

#' Macaque brain: surface mesh and atlas (NMT v2, CHARM)
#'
#' A rhesus macaque brain surface and a 34-region atlas in the space of the
#' NIMH Macaque Template v2 (NMT v2, symmetric), with the regions of level 2 of
#' the Cortical Hierarchy Atlas of the Rhesus Macaque (CHARM; Jung et al. 2021).
#' Use them together, e.g.
#' \code{brainconn(macaque_atlas, conmat, mesh = macaque_mesh)}.
#'
#' \code{macaque_mesh} is the NMT v2 pial surface of both hemispheres,
#' simplified to 25,000 triangles each. \code{macaque_atlas} has the 17 CHARM
#' level 2 regions in each hemisphere, grouped into the four lobes of CHARM
#' level 1 (\code{network}); coordinates are region centroids. See
#' \code{data-raw/animal_meshes.R} in the package source for how they were made.
#'
#' Coordinates are in NMT v2 space: millimetres, with x to the right, y to the
#' front and z up.
#'
#' @format \code{macaque_mesh}: a mesh (see \code{\link{read_brain_mesh}}) with
#'   \code{vertices} and \code{faces}. \code{macaque_atlas}: a data.frame with 34
#'   rows and columns \code{ROI.Name} (e.g. "L_V1"), \code{x.mni}, \code{y.mni},
#'   \code{z.mni} (NMT v2 space, mm), \code{network} (lobe), \code{hemi},
#'   \code{name} (full region name), \code{charm_id} (CHARM label) and
#'   \code{index}.
#' @source Jung B, et al. (2021). A comprehensive macaque fMRI pipeline and
#'   hierarchical atlas. NeuroImage, 235, 117997.
#'   \doi{10.1016/j.neuroimage.2021.117997}. Files from TemplateFlow
#'   (tpl-NMT31Sym). Licence: public domain (CC0).
#' @name macaque_brain
#' @aliases macaque_mesh macaque_atlas
#' @docType data
#' @keywords datasets
NULL
