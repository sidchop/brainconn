# brainconn 1.0.0

## Breaking changes

* `brainconn()` now draws static 3D renders in the style of BrainNet Viewer:
  shiny sphere nodes, tube edges and a translucent glass brain, rendered with
  the rayvertex software renderer (no OpenGL needed). It returns a ggplot
  object. The previous 2D function is now `brainconn2d()`, unchanged. Calls to
  `brainconn()` that use arguments only the 2D plot has (`background`,
  `background.alpha`, `bg_*`, `label.edge.weight`, or a ggraph scale as
  `edge.color`) are passed on to `brainconn2d()` with a warning.

## New features

* `brainconn(style = "realistic")` (experimental) draws an opaque,
  tissue-coloured brain with blood vessels on its surface, and the network
  glowing through it, with nodes and edges further from the viewer darker.
  The vessels are illustrative, grown over any brain surface (also the rat
  and macaque brains) the same way every time.
* Background colours other than white in `brainconn()` now come out as given
  (rayvertex darkened them), and legend labels are no longer clipped at the
  right edge. Text is white on dark backgrounds.
* `brainconn()` defaults to `node.size = 8`; "L" and "R" labels are larger.
* `brainconn()` views: `"top"`, `"bottom"`, `"left"`, `"right"`, `"front"`,
  `"back"`, `"ortho"`, `"all"` (six panels), or any camera angle as
  `c(azimuth, elevation)`. Edges can be sized or coloured by weight
  (`edge.palette`, `edge.limits`); directed edges get arrowheads. The brain
  surface can be restyled (`mesh.color`, `mesh.alpha`), removed
  (`mesh = NULL`) or replaced with any mesh in the atlas's space (an rgl
  `mesh3d` or a list of vertices and faces); node and edge sizes scale with
  the mesh.
* `brainconn3D()` draws nodes as shaded spheres that grow and shrink as you
  zoom (`node.size` is now in mm, as in `brainconn()`, default 8). Hovering
  over a node shows its network, degree, strength and strongest connections,
  including nodes inside the brain surface, which plotly's own hover could not
  reach. With `highlight = TRUE`, clicking a node keeps its edges and
  neighbours and fades everything else; click it again, click elsewhere or
  press Escape to show everything. `brainconn3D()` also gains
  `edge.color.weighted` (with `edge.palette` and `edge.limits`) and `mesh`,
  colours networks with `network_colors()`, and uses a simplified brain
  surface, so pages are smaller and rotate faster.
* `brainconn_matrix()` plots connectivity matrices with regions grouped by
  network: coloured network strips and labels, borders between networks,
  within-network smoothing (`smooth = TRUE`), network-level summaries
  (`level = "network"`, with `values = TRUE` to print them), one triangle
  (`triangle`), a second matrix in the other triangle (`conmat2`), and a 45
  degree "diamond" view (`diamond = TRUE`).
* `brainconn_circle()` draws chord diagrams of connectivity within and
  between networks: network sectors in the standard network colours, ribbons
  coloured by value, `top`/`thr` to keep the strongest links, and an optional
  outer ring of network groups (`group`). Palettes include `"bkr"` and
  `"bwr"`, which every brainconn plot now accepts.
* The Schaefer 17-network atlases gain a `network7` column with the Yeo 7
  network of each region, for grouping (e.g.
  `brainconn_circle(group = "network7")`).
* `network_matrix()` summarises a region x region matrix into a network x
  network matrix (mean, sum, count, density, median or any function), or a
  smoothed region x region matrix with `full = TRUE`. It replaces the
  unexported `plotClassifiedEdges()`.
* New dataset `example_fc_schaefer300`: a simulated, dense, signed functional
  connectivity matrix for `schaefer300_n7`.
* Rat and macaque brains are included: `rat_mesh` and `rat_atlas` (Waxholm
  Space atlas of the Sprague Dawley rat, v4; 66 regions) and `macaque_mesh` and
  `macaque_atlas` (NIMH Macaque Template v2 with CHARM level 2; 34 regions).
* `read_brain_mesh()` reads brain surfaces in GIfTI, FreeSurfer, PLY, OBJ,
  STL, VTK, OFF and other formats (via the freesurferformats package), and
  combines separate hemispheres. Mesh files can also be passed straight to
  the `mesh` argument of `brainconn()` and `brainconn3D()`, so connectivity
  can be plotted on any brain (e.g. macaque or rat), as long as the atlas is in
  the same space.
* `network_colors()` returns the standard Yeo 7/17 network colours for
  Yeo/Schaefer network names and common alternatives (e.g. "Frontoparietal"),
  and evenly spaced colours otherwise.

## Bug fixes and data

* `schaefer300_17_cog_lang` can now be plotted (its region name column was
  `ROI.name` instead of `ROI.Name`).
* `stages_study` network names no longer contain stray non-ASCII characters
  and whitespace, and `shen_368` region names are no longer padded.
* `brainconn3D()` pages are about four times smaller (one surface colour,
  shorter coordinates, and no duplicate copy of the plot data).

## Documentation and infrastructure

* Every included dataset is documented, with references for the atlases.
* Four vignettes: "Getting started" (focused on `brainconn()`, with a quick
  look at everything else), "A guide to every function" (step-by-step
  examples), "Custom atlases and other brains" (your own atlas or mesh, and
  the rat and macaque brains), and "Animating brain networks" (GIFs of a
  network diffusion model and a travelling wave). The README has a new gallery,
  including an animation of the interactive plot, and all figures are high
  resolution.
* New R CMD check workflow, an updated pkgdown workflow and a pkgdown
  reference index.

# brainconn 0.2.0

## Bug fixes

* Left/right orientation: the subject's left hemisphere is now on the left of
  the plot in every top, bottom, front and back view (the bottom and front
  views, and their backgrounds, are mirrored). Previously the front view put
  the left hemisphere on the right, which was inconsistent with the top view
  in `view = "ortho"` (#33).
* `"left"` and `"right"` views now draw the hemisphere
  nearest the viewer on top, instead of mixing both hemispheres' nodes. The
  `"right"` view background is now aligned with the nodes.
* New `hemi.labels` argument (default `TRUE`) marks "L" and "R" on the top,
  bottom, front and back views.
* `brainconn3D()` now works with directed matrices, custom atlases without an
  `index` column, and no `conmat`. Thresholds (`thr`, `uthr`) are applied
  before choosing which nodes to show. All traces share a single 3D scene
  with true aspect ratio, and the "No scatter3d mode specified" warning is
  gone (#32).
* `brainconn()` no longer runs a stress layout, which failed with
  "Negative loop in graph" for negative weights (#34) and "No layout function
  defined for objects of class <matrix>" (#31).
* `brainconn()` now works with no `conmat`, with custom atlases that have no
  `network` column, and with negative weights.
* `node.size` vectors work, with one value per atlas region or per plotted
  node.
* `show.legend = FALSE` is respected in `view = "ortho"`.
* The legend title is now "Network" rather than `as.factor(data$network)`.
* A ggraph colour scale passed as `edge.color` now works in every view, not
  just `"ortho"`.
* Clearer errors for unknown atlases, invalid views and mis-sized matrices.

## Other changes

* `check_atlas()` accepts any numeric coordinates, not only integers.
* `list_atlases()` invisibly returns the atlas names.
* Internal clean-up: shared helpers for atlas, matrix and background handling,
  and one table describing every 2D view.
* Expanded tests covering orientation, all views and matrix types, and 3D.
