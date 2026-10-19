// Hover/click highlighting for brainconn3D(highlight = TRUE).
//
// Hovering a node highlights its edges and neighbours and greys out the other
// nodes and edges. Clicking a node keeps it highlighted until the node is
// clicked again or the brain surface is clicked.
//
// `data` comes from R:
//   nodeTraces: indices of the node traces
//   legendTraces: indices of the legend-only traces (one point each, removed here)
//   edgeTrace:  index of the trace with all edges
//   hlTrace:    index of the (initially empty) trace for highlighted edges
//   neighbors:  for each node, the indices of its neighbours
//   edges:      for each node, {x, y, z, w} arrays of its edges (null between edges)
//   dimColor:   colour for nodes that are not highlighted
//   dimOpacity: opacity of the other edges while a node is highlighted
function(el, x, data) {
  var gd = el;
  var pinned = null;
  var original = {};
  // Per-point attributes of a single point may arrive as a plain value
  function asArray(v, n) {
    if (Array.isArray(v)) return v.slice();
    var out = [];
    for (var k = 0; k < n; k++) out.push(v);
    return out;
  }
  data.nodeTraces.forEach(function(t) {
    var tr = gd.data[t];
    var n = Array.isArray(tr.x) ? tr.x.length : 1;
    original[t] = {
      color: asArray(tr.marker.color, n),
      size: asArray(tr.marker.size, n),
      ids: asArray(tr.customdata, n)
    };
  });
  var edgeOpacity = data.edgeTrace === null ? null : gd.data[data.edgeTrace].opacity;

  // The legend traces only exist for their legend entries
  if (data.legendTraces.length > 0) {
    var gone = data.legendTraces.map(function() { return [null]; });
    Plotly.restyle(gd, {x: gone, y: gone, z: gone}, data.legendTraces);
  }

  function highlight(node) {
    var keep = {};
    keep[node] = true;
    data.neighbors[node].forEach(function(n) { keep[n] = true; });
    var colors = [], sizes = [];
    data.nodeTraces.forEach(function(t) {
      var o = original[t];
      colors.push(o.ids.map(function(id, k) { return keep[id] ? o.color[k] : data.dimColor; }));
      sizes.push(o.ids.map(function(id, k) { return id === node ? o.size[k] * 1.4 : o.size[k]; }));
    });
    Plotly.restyle(gd, {'marker.color': colors, 'marker.size': sizes}, data.nodeTraces);
    if (data.hlTrace !== null) {
      var e = data.edges[node];
      var update = {x: [e.x], y: [e.y], z: [e.z]};
      if (e.w) update['line.color'] = [e.w];
      Plotly.restyle(gd, update, [data.hlTrace]);
      Plotly.restyle(gd, {opacity: data.dimOpacity}, [data.edgeTrace]);
    }
  }

  function reset() {
    var colors = [], sizes = [];
    data.nodeTraces.forEach(function(t) {
      colors.push(original[t].color);
      sizes.push(original[t].size);
    });
    Plotly.restyle(gd, {'marker.color': colors, 'marker.size': sizes}, data.nodeTraces);
    if (data.hlTrace !== null) {
      Plotly.restyle(gd, {x: [[]], y: [[]], z: [[]]}, [data.hlTrace]);
      Plotly.restyle(gd, {opacity: edgeOpacity}, [data.edgeTrace]);
    }
  }

  function nodeOf(ev) {
    var p = ev && ev.points && ev.points[0];
    if (!p || data.nodeTraces.indexOf(p.curveNumber) < 0) return null;
    return p.customdata;
  }

  gd.on('plotly_hover', function(ev) {
    if (pinned !== null) return;
    var node = nodeOf(ev);
    if (node === null) reset(); else highlight(node);
  });
  gd.on('plotly_unhover', function() {
    if (pinned === null) reset();
  });
  gd.on('plotly_click', function(ev) {
    var node = nodeOf(ev);
    if (node === null || node === pinned) {
      pinned = null;
      reset();
    } else {
      pinned = node;
      highlight(node);
    }
  });

  // For scripts and tests: highlight node i (0-based), or reset with null
  el.brainconnHighlight = function(node) {
    pinned = node;
    if (node === null) reset(); else highlight(node);
  };
}
