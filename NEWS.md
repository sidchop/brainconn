# brainconn 0.2.0

## Bug fixes

* Left/right orientation: `"left"` and `"right"` views now draw the hemisphere
  nearest the viewer on top, instead of mixing both hemispheres' nodes. The
  `"right"` view background is now aligned with the nodes.
* New `hemi.labels` argument (default `TRUE`) marks "L" and "R" on the top,
  bottom, front and back views, so the orientation of each view (and of the
  ortho layout) is unambiguous.
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
