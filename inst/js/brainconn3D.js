// Node tooltips and click highlighting for brainconn3D().
//
// Plotly can't pick nodes inside the (translucent) brain surface, so the node
// under the mouse is found here: every node centre is projected to the screen
// with the scene's current camera, and the nearest one within its on-screen
// radius wins (the one closest to the viewer, if several overlap).
//
// With data.highlight, clicking a node keeps its edges and neighbours and fades
// everything else; clicking it again, clicking elsewhere or pressing Escape
// shows everything again.
//
// `data` comes from R:
//   centres, radii: node positions and sphere radii (data units)
//   hover:          tooltip HTML for each node
//   nodeTraces:     [{trace, nodes, colors}] node mesh traces, their nodes and colours
//   legendTraces:   one-point traces for the legend, removed from the scene here
//   verticesPerNode: vertices in each node's sphere
//   edgeTrace, hlTrace: indices of the edge trace and the (empty) highlight trace
//   selTrace:       (highlight only) mesh trace for the selected node and neighbours
//   neighbors, edges:  (highlight only) each node's neighbours and edge coordinates
//   nodeOpacity, dimOpacity: opacity of the other nodes and edges while highlighting
function(el, x, data) {
  var gd = el;
  var selected = null;
  var edgeOpacity = data.edgeTrace === null ? null : gd.data[data.edgeTrace].opacity;

  if (data.highlight) Plotly.restyle(gd, {opacity: 0}, [data.selTrace]);

  // The legend traces only exist for their legend entries
  if (data.legendTraces.length > 0) {
    var gone = data.legendTraces.map(function() { return [null]; });
    Plotly.restyle(gd, {x: gone, y: gone, z: gone}, data.legendTraces);
  }

  // Screen position (client pixels), on-screen radius and depth of node i
  function mul(m, v) {
    return [m[0] * v[0] + m[4] * v[1] + m[8] * v[2] + m[12] * v[3],
            m[1] * v[0] + m[5] * v[1] + m[9] * v[2] + m[13] * v[3],
            m[2] * v[0] + m[6] * v[1] + m[10] * v[2] + m[14] * v[3],
            m[3] * v[0] + m[7] * v[1] + m[11] * v[2] + m[15] * v[3]];
  }
  function project(p) {
    var scene = gd._fullLayout.scene._scene;
    var cam = scene.glplot.cameraParams, s = scene.dataScale;
    var rect = scene.glplot.canvas.getBoundingClientRect();
    var v = [p[0] * s[0], p[1] * s[1], p[2] * s[2], 1];
    v = mul(cam.projection, mul(cam.view, mul(cam.model, v)));
    return [rect.left + (v[0] / v[3] * 0.5 + 0.5) * rect.width,
            rect.top + (0.5 - v[1] / v[3] * 0.5) * rect.height,
            v[2] / v[3]];
  }
  function screenNode(i) {
    var c = data.centres[i], r = data.radii[i];
    var p = project(c);
    var q = project([c[0] + r, c[1], c[2]]), q2 = project([c[0], c[1] + r, c[2]]),
        q3 = project([c[0], c[1], c[2] + r]);
    var rad = Math.max(Math.hypot(q[0] - p[0], q[1] - p[1]), Math.hypot(q2[0] - p[0], q2[1] - p[1]),
                       Math.hypot(q3[0] - p[0], q3[1] - p[1]));
    return {x: p[0], y: p[1], r: rad, depth: p[2]};
  }
  function visibleNodes() {
    var out = [];
    data.nodeTraces.forEach(function(nt) {
      var v = gd.data[nt.trace].visible;
      if (v === false || v === 'legendonly') return;
      nt.nodes.forEach(function(i) { out.push(i); });
    });
    return out;
  }
  function nodeAt(cx, cy) {
    var best = null, bestDepth = Infinity;
    visibleNodes().forEach(function(i) {
      var s = screenNode(i);
      if (Math.hypot(cx - s.x, cy - s.y) <= Math.max(s.r, 5) && s.depth < bestDepth) {
        best = i; bestDepth = s.depth;
      }
    });
    return best;
  }

  // Tooltip
  if (getComputedStyle(el).position === 'static') el.style.position = 'relative';
  var tip = document.createElement('div');
  tip.className = 'brainconn-tooltip';
  tip.style.cssText = 'position:absolute;display:none;pointer-events:none;z-index:1001;' +
    'background:white;border:1px solid #888;border-radius:3px;padding:4px 6px;' +
    'font:12px sans-serif;color:#222;box-shadow:0 1px 3px rgba(0,0,0,0.2);white-space:nowrap';
  el.appendChild(tip);
  function showTip(i, cx, cy) {
    var box = el.getBoundingClientRect();
    tip.innerHTML = data.hover[i];
    tip.style.display = 'block';
    tip.style.left = (cx - box.left + 12) + 'px';
    tip.style.top = (cy - box.top + 12) + 'px';
  }
  function hideTip() { tip.style.display = 'none'; }

  // Highlighting: fade the node traces and the other edges, and draw the
  // selected node and its neighbours, opaque, on the selection trace
  var traces = data.nodeTraces.map(function(nt) { return nt.trace; });
  var where = {};   // node -> [node trace, position within it]
  data.nodeTraces.forEach(function(nt) {
    nt.nodes.forEach(function(i, k) { where[i] = [nt, k]; });
  });
  function selectionMesh(nodes) {
    var nv = data.verticesPerNode, m = {x: [], y: [], z: [], i: [], j: [], k: [], vertexcolor: []};
    var first = gd.data[data.nodeTraces[0].trace];
    var nf = first.i.length / data.nodeTraces[0].nodes.length;   // faces per sphere
    nodes.forEach(function(node, n) {
      var nt = where[node][0], k = where[node][1], tr = gd.data[nt.trace];
      for (var v = k * nv; v < (k + 1) * nv; v++) {
        m.x.push(tr.x[v]); m.y.push(tr.y[v]); m.z.push(tr.z[v]);
        m.vertexcolor.push(nt.colors[k]);
      }
      for (var f = 0; f < nf; f++) {
        m.i.push(first.i[f] + n * nv); m.j.push(first.j[f] + n * nv); m.k.push(first.k[f] + n * nv);
      }
    });
    return m;
  }
  function select(i) {
    selected = i;
    var nodes = [i].concat(data.neighbors[i]);
    var m = selectionMesh(nodes);
    Plotly.restyle(gd, {x: [m.x], y: [m.y], z: [m.z], i: [m.i], j: [m.j], k: [m.k],
                        vertexcolor: [m.vertexcolor], opacity: 1}, [data.selTrace]);
    Plotly.restyle(gd, {opacity: data.nodeOpacity}, traces);
    if (data.hlTrace !== null) {
      var e = data.edges[i], update = {x: [e.x], y: [e.y], z: [e.z]};
      if (e.w) update['line.color'] = [e.w];
      Plotly.restyle(gd, update, [data.hlTrace]);
      Plotly.restyle(gd, {opacity: data.dimOpacity}, [data.edgeTrace]);
    }
  }
  function reset() {
    selected = null;
    Plotly.restyle(gd, {opacity: 0}, [data.selTrace]);
    Plotly.restyle(gd, {opacity: 1}, traces);
    if (data.hlTrace !== null) {
      Plotly.restyle(gd, {x: [[]], y: [[]], z: [[]]}, [data.hlTrace]);
      Plotly.restyle(gd, {opacity: edgeOpacity}, [data.edgeTrace]);
    }
  }

  // Mouse: tooltips while moving (not while dragging to rotate); a click
  // without dragging selects a node
  var down = null;
  el.addEventListener('mousedown', function(ev) { down = [ev.clientX, ev.clientY]; hideTip(); });
  el.addEventListener('mousemove', function(ev) {
    if (ev.buttons) return;
    var i = nodeAt(ev.clientX, ev.clientY);
    if (i === null) hideTip(); else showTip(i, ev.clientX, ev.clientY);
  });
  el.addEventListener('mouseleave', hideTip);
  el.addEventListener('mouseup', function(ev) {
    if (!data.highlight || !down) return;
    var moved = Math.hypot(ev.clientX - down[0], ev.clientY - down[1]);
    down = null;
    if (moved > 4) return;
    var i = nodeAt(ev.clientX, ev.clientY);
    if (i === null || i === selected) reset(); else select(i);
  });
  document.addEventListener('keydown', function(ev) {
    if (ev.key === 'Escape' && selected !== null) reset();
  });

  // For scripts and tests
  el.brainconnHighlight = function(i) { if (i === null) reset(); else select(i); };
  el.brainconnSelected = function() { return selected; };
  el.brainconnScreen = function(i) { return screenNode(i); };
  el.brainconnNodeAt = nodeAt;
}
