/* RubberSignal : graphiques SVG sans dépendance.
   Règles : un seul axe par graphique ; traits fins (2 px) ; légende dès deux séries ;
   texte en encre neutre (jamais la couleur de la série) ; lecture au survol ET au clavier ;
   chaque graphique a son tableau de valeurs. */
(function () {
  'use strict';
  var NS = 'http://www.w3.org/2000/svg';
  var RS = window.RS;

  function S(name, attrs, parent) {
    var e = document.createElementNS(NS, name);
    for (var k in attrs) {
      if (!Object.prototype.hasOwnProperty.call(attrs, k)) continue;
      // style posé par le CSSOM : compatible avec une politique de contenu sans style en ligne
      if (k === 'style') e.style.cssText = attrs[k]; else e.setAttribute(k, attrs[k]);
    }
    if (parent) parent.appendChild(e);
    return e;
  }
  function fin(v) { return v !== null && v !== undefined && isFinite(v); }

  function ticks(min, max, n) {
    if (min === max) { min -= 1; max += 1; }
    var raw = (max - min) / n;
    var mag = Math.pow(10, Math.floor(Math.log(raw) / Math.LN10));
    var r = raw / mag;
    var step = (r <= 1 ? 1 : r <= 2 ? 2 : r <= 2.5 ? 2.5 : r <= 5 ? 5 : 10) * mag;
    var lo = Math.floor(min / step + 1e-9) * step;
    var hi = Math.ceil(max / step - 1e-9) * step;
    var out = [];
    for (var v = lo; v <= hi + step * 1e-6; v += step) out.push(Math.round(v / step) * step);
    return { lo: lo, hi: hi, values: out, step: step };
  }

  function legend(host, series, box) {
    var l = RS.el('div', 'legend', null, host);
    series.forEach(function (s) {
      var k = RS.el('span', 'key', null, l);
      var sw = RS.el('span', 'swatch' + (box ? ' box' : ''), null, k);
      if (box) sw.style.background = s.color; else sw.style.borderTopColor = s.color;
      RS.el('span', null, s.name, k);
    });
  }

  // Couche de lecture commune : réticule, info-bulle, flèches du clavier
  function reader(host, svg, g, n, xOf, rowsOf, titleOf, onIndex) {
    var tip = RS.el('div', 'tip', null, host);
    tip.hidden = true;
    var hit = S('rect', {
      x: g.l, y: g.t, width: Math.max(1, g.W - g.l - g.r), height: Math.max(1, g.H - g.t - g.b),
      fill: 'transparent', 'class': 'hit', tabindex: '0', focusable: 'true',
      'aria-label': RS.t('Lecture des valeurs : flèches gauche et droite', 'Read values: left and right arrow keys')
    }, svg);
    var cur = -1;
    function show(i) {
      if (i < 0 || i >= n) return;
      cur = i;
      onIndex(i);
      tip.textContent = '';
      RS.el('div', 't', titleOf(i), tip);
      rowsOf(i).forEach(function (r) {
        if (r.extra) { RS.el('div', 'x', r.extra, tip); return; }
        var row = RS.el('div', 'r', null, tip);
        var nm = RS.el('span', 'n', null, row);
        if (r.color) { var key = RS.el('i', null, null, nm); key.style.borderTopColor = r.color; }
        RS.el('span', null, r.name, nm);
        RS.el('b', null, r.value, row);
      });
      tip.hidden = false;
      var box = svg.getBoundingClientRect();
      var scale = box.width / g.W;
      var px = xOf(i) * scale;
      var w = tip.offsetWidth;
      var left = px + 14;
      if (left + w > box.width) left = px - w - 14;
      tip.style.left = Math.max(0, left) + 'px';
      tip.style.top = (g.t * scale + 4) + 'px';
    }
    function hide() { cur = -1; tip.hidden = true; onIndex(-1); }
    function nearest(ev) {
      var box = svg.getBoundingClientRect();
      var x = (ev.clientX - box.left) * (g.W / box.width);
      var best = 0, d = Infinity;
      for (var i = 0; i < n; i++) { var dd = Math.abs(xOf(i) - x); if (dd < d) { d = dd; best = i; } }
      return best;
    }
    hit.addEventListener('pointermove', function (ev) { show(nearest(ev)); });
    hit.addEventListener('pointerdown', function (ev) { show(nearest(ev)); });
    hit.addEventListener('pointerleave', function () { if (document.activeElement !== hit) hide(); });
    hit.addEventListener('focus', function () { show(cur >= 0 ? cur : n - 1); });
    hit.addEventListener('blur', hide);
    hit.addEventListener('keydown', function (ev) {
      if (ev.key === 'ArrowLeft') { show(Math.max(0, (cur < 0 ? n : cur) - 1)); ev.preventDefault(); }
      else if (ev.key === 'ArrowRight') { show(Math.min(n - 1, cur + 1)); ev.preventDefault(); }
      else if (ev.key === 'Escape') { hit.blur(); }
    });
  }

  function axes(svg, g, tk, ys, yFmt, x, xs, xFmt) {
    tk.values.forEach(function (v) {
      S('line', { x1: g.l, x2: g.W - g.r, y1: ys(v), y2: ys(v), style: 'stroke:var(--grid);stroke-width:1' }, svg);
      S('text', { x: g.l - 8, y: ys(v) + 4, 'text-anchor': 'end' }, svg).textContent = yFmt(v);
    });
    var n = x.length;
    var room = Math.max(2, Math.floor((g.W - g.l - g.r) / 62));
    var every = Math.max(1, Math.ceil(n / room));
    for (var i = n - 1; i >= 0; i -= every) {
      S('text', { x: xs(i), y: g.H - 6, 'text-anchor': 'middle' }, svg).textContent = xFmt(x[i], false);
    }
  }

  /* Courbes. o : { x, xFmt(label, long), series: [{ name, color, values, step }], yFmt, tipFmt,
                    height, zero, endLabels, label, tipExtra(i) } */
  function line(host, o) {
    host.textContent = '';
    host.classList.add('chart');
    var series = o.series.filter(function (s) { return s.values.some(fin); });
    if (!series.length) { RS.el('p', 'note', RS.t('Donnée non disponible.', 'Data not available.'), host); return; }
    if (series.length > 1) legend(host, series, false);
    var g = { W: Math.max(300, host.clientWidth || 640), H: o.height || 260, l: 48, r: o.endLabels === false ? 14 : 60, t: 12, b: 26 };
    var all = [];
    series.forEach(function (s) { s.values.forEach(function (v) { if (fin(v)) all.push(v); }); });
    var mn = Math.min.apply(null, all), mx = Math.max.apply(null, all);
    var pad = (mx - mn) * 0.1 || Math.abs(mx) * 0.05 || 1;
    var tk = o.zero ? ticks(Math.min(0, mn), mx + pad, 4) : ticks(mn - pad, mx + pad, 4);
    var n = o.x.length;
    var xs = function (i) { return g.l + (n === 1 ? (g.W - g.l - g.r) / 2 : i * (g.W - g.l - g.r) / (n - 1)); };
    var ys = function (v) { return g.t + (tk.hi - v) * (g.H - g.t - g.b) / (tk.hi - tk.lo); };
    var svg = S('svg', { viewBox: '0 0 ' + g.W + ' ' + g.H, role: 'img', 'aria-label': o.label || '' }, host);
    axes(svg, g, tk, ys, o.yFmt, o.x, xs, o.xFmt);

    var ends = [];
    series.forEach(function (s, si) {
      var d = '', pen = false, run = 0, last = -1, alone = [];
      s.values.forEach(function (v, i) {
        if (!fin(v)) { if (pen && run === 1) alone.push(last); pen = false; run = 0; return; }
        var X = xs(i).toFixed(1), Y = ys(v).toFixed(1);
        d += pen ? (s.step ? 'H' + X + 'V' + Y : 'L' + X + ' ' + Y) : 'M' + X + ' ' + Y;
        pen = true; run += 1; last = i;
      });
      if (pen && run === 1) alone.push(last);
      S('path', { d: d, fill: 'none', style: 'stroke:' + s.color + ';stroke-width:2;stroke-linejoin:round;stroke-linecap:round' }, svg);
      alone.forEach(function (i) {
        S('circle', { cx: xs(i), cy: ys(s.values[i]), r: 3, style: 'fill:' + s.color }, svg);
      });
      if (last >= 0) {
        S('circle', { cx: xs(last), cy: ys(s.values[last]), r: 4, style: 'fill:' + s.color + ';stroke:var(--surface);stroke-width:2' }, svg);
        ends.push({ si: si, i: last, y: ys(s.values[last]), v: s.values[last] });
      }
    });
    if (o.endLabels !== false) {
      var placed = [];
      ends.forEach(function (e) {          // la première série (le sujet) est servie d'abord
        if (e.i !== n - 1) return;
        if (placed.some(function (y) { return Math.abs(y - e.y) < 15; })) return;
        placed.push(e.y);
        S('text', { x: xs(e.i) + 9, y: e.y + 4, 'class': 'endlabel' }, svg).textContent = (o.tipFmt || o.yFmt)(e.v);
      });
    }

    var cross = S('line', { y1: g.t, y2: g.H - g.b, style: 'stroke:var(--axis);stroke-width:1', visibility: 'hidden' }, svg);
    var dots = series.map(function (s) {
      return S('circle', { r: 4.5, style: 'fill:' + s.color + ';stroke:var(--surface);stroke-width:2', visibility: 'hidden' }, svg);
    });
    reader(host, svg, g, n, xs, function (i) {
      var rows = series.map(function (s) {
        return { name: s.name, color: s.color, value: fin(s.values[i]) ? (o.tipFmt || o.yFmt)(s.values[i]) : '—' };
      });
      var extra = o.tipExtra ? o.tipExtra(i) : null;
      if (extra) rows.push({ extra: extra });
      return rows;
    }, function (i) { return o.xFmt(o.x[i], true); }, function (i) {
      if (i < 0) { cross.setAttribute('visibility', 'hidden'); dots.forEach(function (c) { c.setAttribute('visibility', 'hidden'); }); return; }
      cross.setAttribute('x1', xs(i)); cross.setAttribute('x2', xs(i)); cross.setAttribute('visibility', 'visible');
      series.forEach(function (s, k) {
        if (fin(s.values[i])) { dots[k].setAttribute('cx', xs(i)); dots[k].setAttribute('cy', ys(s.values[i])); dots[k].setAttribute('visibility', 'visible'); }
        else dots[k].setAttribute('visibility', 'hidden');
      });
    });
  }

  /* Barres verticales depuis une ligne de base à zéro.
     o : { x, xFmt, values, color, name, yFmt, tipFmt, height, label, tipExtra(i) } */
  function bars(host, o) {
    host.textContent = '';
    host.classList.add('chart');
    var vals = o.values.filter(fin);
    if (!vals.length) { RS.el('p', 'note', RS.t('Donnée non disponible.', 'Data not available.'), host); return; }
    var g = { W: Math.max(300, host.clientWidth || 640), H: o.height || 220, l: 48, r: 14, t: 18, b: 26 };
    var mn = Math.min(0, Math.min.apply(null, vals)), mx = Math.max(0, Math.max.apply(null, vals));
    var tk = ticks(mn, mx, 4);
    var n = o.x.length;
    var band = (g.W - g.l - g.r) / n;
    var bw = Math.min(24, Math.max(3, band - 2));
    var xs = function (i) { return g.l + band * (i + 0.5); };
    var ys = function (v) { return g.t + (tk.hi - v) * (g.H - g.t - g.b) / (tk.hi - tk.lo); };
    var svg = S('svg', { viewBox: '0 0 ' + g.W + ' ' + g.H, role: 'img', 'aria-label': o.label || '' }, host);
    axes(svg, g, tk, ys, o.yFmt, o.x, xs, o.xFmt);
    var y0 = ys(0), marks = [], lastI = -1;
    o.values.forEach(function (v, i) {
      if (!fin(v)) { marks.push(null); return; }
      lastI = i;
      var x = xs(i) - bw / 2, y1 = ys(v), h = Math.abs(y1 - y0);
      var r = Math.min(4, bw / 2, h), s = v >= 0 ? 1 : -1;
      var d = 'M' + x.toFixed(1) + ' ' + y0.toFixed(1) + 'V' + (y1 + s * r).toFixed(1) +
        'Q' + x.toFixed(1) + ' ' + y1.toFixed(1) + ' ' + (x + r).toFixed(1) + ' ' + y1.toFixed(1) +
        'H' + (x + bw - r).toFixed(1) +
        'Q' + (x + bw).toFixed(1) + ' ' + y1.toFixed(1) + ' ' + (x + bw).toFixed(1) + ' ' + (y1 + s * r).toFixed(1) +
        'V' + y0.toFixed(1) + 'Z';
      marks.push(S('path', { d: d, style: 'fill:' + o.color }, svg));
    });
    S('line', { x1: g.l, x2: g.W - g.r, y1: y0, y2: y0, style: 'stroke:var(--axis);stroke-width:1' }, svg);
    if (lastI >= 0) {
      var lv = o.values[lastI];
      S('text', { x: xs(lastI), y: ys(lv) + (lv >= 0 ? -6 : 14), 'text-anchor': lastI === n - 1 ? 'end' : 'middle', 'class': 'endlabel' }, svg)
        .textContent = (o.tipFmt || o.yFmt)(lv);
    }
    reader(host, svg, g, n, xs, function (i) {
      var rows = [{ name: o.name, color: o.color, value: fin(o.values[i]) ? (o.tipFmt || o.yFmt)(o.values[i]) : '—' }];
      var extra = o.tipExtra ? o.tipExtra(i) : null;
      if (extra) rows.push({ extra: extra });
      return rows;
    }, function (i) { return o.xFmt(o.x[i], true); }, function (i) {
      marks.forEach(function (m, k) { if (m) m.style.opacity = (i < 0 || i === k) ? '1' : '0.5'; });
    });
  }

  /* Barres horizontales classées, une mise en avant. rows : [{ name, value, text, strong }] */
  function ranked(host, rows) {
    host.textContent = '';
    var max = Math.max.apply(null, rows.map(function (r) { return fin(r.value) ? r.value : 0; }));
    var box = RS.el('div', 'ranked', null, host);
    rows.forEach(function (r) {
      var row = RS.el('div', 'rk-row', null, box);
      RS.el('span', 'rk-name', r.name, row);
      var track = RS.el('span', 'rk-track', null, row);
      var bar = RS.el('span', 'rk-bar' + (r.strong ? ' strong' : ''), null, track);
      bar.style.width = (max > 0 && fin(r.value) ? Math.max(1, 100 * r.value / max) : 0) + '%';
      RS.el('span', 'rk-val', r.text, row);
    });
  }

  /* Tableau de valeurs replié sous un graphique. cols : [{ label, left, get(row) }] */
  function table(host, cols, rows, summary, open) {
    var d = RS.el('details', 'tbl', null, host);
    if (open) d.open = true;
    RS.el('summary', null, summary || RS.t('Tableau des valeurs', 'Table of values'), d);
    var sc = RS.el('div', 'scroll', null, d);
    var t = RS.el('table', 'data', null, sc);
    var hr = RS.el('tr', null, null, RS.el('thead', null, null, t));
    cols.forEach(function (c) { var th = RS.el('th', c.left ? 'l' : null, c.label, hr); th.scope = 'col'; });
    var tb = RS.el('tbody', null, null, t);
    rows.forEach(function (r) {
      var tr = RS.el('tr', null, null, tb);
      cols.forEach(function (c) { RS.el('td', (c.left ? 'l' : '') + (c.wrap ? ' wrapc' : ''), c.get(r), tr); });
    });
    return d;
  }

  // Redessin au changement de largeur ou de langue
  var mounted = [];
  function mount(host, draw) {
    var item = { host: host, draw: draw, w: 0 };
    mounted.push(item);
    function go() { item.w = host.clientWidth; draw(); }
    go();
    if (window.ResizeObserver) {
      var pending = false;
      new ResizeObserver(function () {
        if (pending || Math.abs(host.clientWidth - item.w) < 2) return;
        pending = true;
        window.requestAnimationFrame(function () { pending = false; go(); });
      }).observe(host);
    }
  }
  function redrawAll() { mounted.forEach(function (m) { m.w = m.host.clientWidth; m.draw(); }); }

  window.RSCharts = { line: line, bars: bars, ranked: ranked, table: table, mount: mount, redrawAll: redrawAll };
})();
