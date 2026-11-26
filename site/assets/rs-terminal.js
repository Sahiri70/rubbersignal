/* RubberSignal Terminal : mise en page des données publiques (data/terminal.js).
   Lecture seule : aucune écriture, aucun appel réseau, aucune clé. */
(function () {
  'use strict';
  var RS = window.RS, C = window.RSCharts, D = window.RS_DATA;
  var $ = function (id) { return document.getElementById(id); };
  var S1 = 'var(--s1)', S2 = 'var(--s2)', S3 = 'var(--s3)';

  if (!D || !D.rapi || !D.meta) {
    $('erreur').hidden = false;
    $('app').hidden = true;
    return;
  }

  var state = { range: 12, cur: 'usd' };
  var t = RS.t;
  var ok = RS.ok;

  function cut(a) { return state.range ? a.slice(-state.range) : a; }
  function clear(node) { node.textContent = ''; return node; }
  function tag(text, title, parent) { var e = RS.el('span', 'tag', text, parent); if (title) e.title = title; return e; }
  var NATURE = {
    T: ['transaction', 'transaction'], O: ['officiel', 'official'], D: ['déclaration', 'declaration'],
    E: ['estimation calculée', 'calculated estimate'], J: ['jugement', 'judgement']
  };
  function natureTag(code, parent) {
    var n = NATURE[code];
    return tag(code, n ? t(n[0], n[1]) : null, parent);
  }

  // ── Barre d'état ──────────────────────────────────────────
  function drawStatus() {
    var s = clear($('status'));
    var m = D.meta;
    RS.el('span', null, t('Semaine ', 'Week ') + m.semaine + ' · ' + t('données du ', 'data as of ') + RS.date(m.date_collecte), s);
    var v = m.validation || {};
    var txt = v.statut === 'valide' ? t('Validé par l’évaluateur le ', 'Validated by the assessor on ') + RS.date(v.date)
      : v.statut === 'valide_avec_reserves' ? t('Validé avec réserves le ', 'Validated with reservations on ') + RS.date(v.date)
      : v.statut === 'refuse' ? t('Refusé par l’évaluateur : ne pas utiliser', 'Rejected by the assessor: do not use')
      : t('En attente de validation par l’évaluateur', 'Pending validation by the assessor');
    RS.el('span', 'tag status dot', txt, s);
    RS.el('span', null, t('Sources sous licence libre uniquement', 'Openly licensed sources only'), s);
    $('foot-validation').textContent = t('Calculs préparés et contrôlés par des outils automatisés, dont des modèles d’IA. ',
      'Calculations prepared and checked by automated tools, including AI models. ') + txt + ' (' + m.evaluateur + ').';
  }

  // ── Tuiles ────────────────────────────────────────────────
  function kpi(host, label, value, unit, metaText, nature) {
    var k = RS.el('div', 'kpi', null, host);
    RS.el('div', 'label', label, k);
    var v = RS.el('div', 'value', value, k);
    if (unit) RS.el('small', null, unit, v);
    var m = RS.el('div', 'meta', null, k);
    if (nature) natureTag(nature, m);
    RS.el('span', null, metaText, m);
  }
  function lastWith(arr, key) { for (var i = arr.length - 1; i >= 0; i--) if (ok(arr[i][key])) return i; return -1; }

  function drawKpis() {
    var host = clear($('kpis'));
    var mois = D.rapi.mois, i = lastWith(mois, 'ci_usd');
    if (i >= 0) {
      var m = mois[i], p = i > 0 ? mois[i - 1] : null;
      var key = state.cur === 'usd' ? 'ci_usd' : 'ci_eur';
      var varm = p && ok(p[key]) ? (m[key] / p[key] - 1) * 100 : null;
      kpi(host, t('Caoutchouc ivoirien rendu UE', 'Ivorian rubber, delivered EU'), RS.num(m[key], 3),
        state.cur === 'usd' ? 'USD/kg' : '€/kg',
        RS.month(m.p, true) + (ok(varm) ? ' · ' + RS.pct(varm, 1, true) + t(' sur un mois', ' m/m') : ''), 'T');
      kpi(host, t('Écart de prix Côte d’Ivoire / Asie', 'Price gap, Côte d’Ivoire vs Asia'), RS.pct(m.decote, 1), null, RS.month(m.p, true), 'E');
      kpi(host, t('Importations UE depuis la Côte d’Ivoire', 'EU imports from Côte d’Ivoire'), RS.int(m.vol_t), 't', RS.month(m.p, true), 'T');
    }
    var bm = D.references.bm_tsr20;
    if (bm.length) {
      var b = bm[bm.length - 1];
      kpi(host, t('TSR20, Banque mondiale', 'TSR20, World Bank'), RS.num(b.v, 2), 'USD/kg', RS.month(b.p, true), 'T');
    }
    var h = D.rsci.historique, r = lastWith(h, 'rsci_pct');
    if (r >= 0) {
      kpi(host, t('Part du planteur (RSCI)', 'Farmer share (RSCI)'), RS.pct(h[r].rsci_pct, 1), null,
        t('prix de ', 'price of ') + RS.month(h[r].mois, true), 'E');
    }
    if (D.pre_rsi) {
      kpi(host, t('Pré-RSI (analyse)', 'Pre-RSI (analysis)'), RS.int(D.pre_rsi.score), '/100',
        String(t(D.pre_rsi.signal_fr, D.pre_rsi.signal_en) || '').split(' — ')[0], 'E');
    }
  }

  // ── 1. Matrice Afrique de l'Ouest ─────────────────────────
  function flags(m) {
    var f = [];
    if (m.revise) f.push(t('révisé par Eurostat', 'revised by Eurostat'));
    if (m.faible_liquidite) f.push(t('faible liquidité', 'low liquidity'));
    if (m.alerte_variation) f.push(t('variation de plus de 15 %', 'change above 15%'));
    if (!m.asie_complet) f.push(t('composite Asie incomplet', 'Asian composite incomplete'));
    return f.join(', ');
  }

  function drawMatrice() {
    var mois = cut(D.rapi.mois);
    var usd = state.cur === 'usd', unit = usd ? 'USD/kg' : '€/kg';
    var x = mois.map(function (m) { return m.p; });
    var series = [
      { name: t('Côte d’Ivoire', 'Côte d’Ivoire'), color: S1, values: mois.map(function (m) { return usd ? m.ci_usd : m.ci_eur; }) },
      { name: t('Asie (Thaïlande, Indonésie, Malaisie, Vietnam)', 'Asia (Thailand, Indonesia, Malaysia, Vietnam)'), color: S2, values: mois.map(function (m) { return usd ? m.asie_usd : m.asie_eur; }) }
    ];
    if (usd) series.push({ name: t('TSR20 Banque mondiale (cotation, hors indice)', 'World Bank TSR20 (quote, outside the index)'), color: S3, values: mois.map(function (m) { return m.tsr20_bm; }) });
    $('c-prix-sub').textContent = unit + ' · ' + t('douanes de l’UE (Eurostat), mise à jour du ', 'EU customs (Eurostat), updated ') + RS.date(D.rapi.maj_eurostat);
    C.line($('c-prix'), {
      x: x, xFmt: RS.month, series: series, height: 290,
      yFmt: function (v) { return RS.num(v, 2); },
      tipFmt: function (v) { return RS.num(v, 3); },
      tipExtra: function (i) { return flags(mois[i]) || null; },
      label: t('Prix rendu UE par origine', 'EU-delivered price by origin')
    });
    C.bars($('c-decote'), {
      x: x, xFmt: RS.month, values: mois.map(function (m) { return m.decote; }), color: S1,
      name: t('Écart CI / Asie', 'CI / Asia gap'), height: 220,
      yFmt: function (v) { return RS.pct(v, 0); }, tipFmt: function (v) { return RS.pct(v, 1); },
      label: t('Écart de prix Côte d’Ivoire / Asie', 'Price gap Côte d’Ivoire vs Asia')
    });
    C.bars($('c-volume'), {
      x: x, xFmt: RS.month, values: mois.map(function (m) { return m.vol_t; }), color: S1,
      name: t('Volume', 'Volume'), height: 220,
      yFmt: function (v) { return RS.int(v / 1000); }, tipFmt: function (v) { return RS.int(v) + ' t'; },
      tipExtra: function (i) { return mois[i].revise ? t('révisé par Eurostat', 'revised by Eurostat') : null; },
      label: t('Volume importé depuis la Côte d’Ivoire', 'Volume imported from Côte d’Ivoire')
    });

    var host = clear($('t-matrice'));
    var rows = mois.slice().reverse();
    C.table(host, [
      { label: t('Mois', 'Month'), left: true, get: function (m) { return RS.month(m.p, true); } },
      { label: t('Volume CI (t)', 'CI volume (t)'), get: function (m) { return RS.int(m.vol_t); } },
      { label: 'CI €/kg', get: function (m) { return RS.num(m.ci_eur, 3); } },
      { label: 'CI USD/kg', get: function (m) { return RS.num(m.ci_usd, 3); } },
      { label: t('Asie USD/kg', 'Asia USD/kg'), get: function (m) { return RS.num(m.asie_usd, 3); } },
      { label: t('Écart CI / Asie', 'CI / Asia gap'), get: function (m) { return RS.pct(m.decote, 1); } },
      { label: t('TSR20 Banque mondiale', 'World Bank TSR20'), get: function (m) { return RS.num(m.tsr20_bm, 2); } },
      { label: t('Mentions', 'Flags'), left: true, get: function (m) { return flags(m) || '—'; } }
    ], rows, t('Tableau complet de la matrice (prix : nature T ; écart : nature E ; méthode ', 'Full matrix table (prices: nature T; gap: nature E; method ') + D.rapi.version + ')', true);
  }

  function drawOrigines() {
    var o = D.rapi.origines;
    if (!o || !o.pays) { clear($('c-orig')); return; }
    $('c-orig-sub').textContent = t('Tonnes, de ', 'Tonnes, ') + RS.month(o.debut, true) + t(' à ', ' to ') + RS.month(o.fin, true);
    var rows = o.pays.filter(function (p) { return ok(p.vol_t); }).sort(function (a, b) { return b.vol_t - a.vol_t; })
      .map(function (p) {
        return { name: t(p.nom_fr, p.nom_en), value: p.vol_t, strong: p.code === 'CI', text: RS.int(p.vol_t) + ' t · ' + RS.num(p.prix_eur_kg, 3) + ' €/kg' };
      });
    C.ranked($('c-orig'), rows);
    $('c-orig-note').textContent = t('Prix moyen pondéré par les volumes sur la période. Source : Eurostat Comext, NC 4001 22 00.',
      'Volume-weighted average price over the period. Source: Eurostat Comext, CN 4001 22 00.');
  }

  // ── 1 bis. Autres origines africaines (RAPI-Afrique) ──────
  function afFlags(m) {
    var f = [];
    if (m.revise) f.push(t('révisé par Eurostat', 'revised by Eurostat'));
    if (m.faible_liquidite) f.push(t('faible liquidité (moins de ', 'low liquidity (below ') + RS.int(D.afrique.seuil_liquidite_t) + ' t)');
    if (m.alerte_variation) f.push(t('variation de plus de 15 %', 'change above 15%'));
    return f.join(', ');
  }

  function drawAfrique() {
    var A = D.afrique, sec = $('afrique');
    if (!A || !A.origines || !A.origines.length) { sec.hidden = true; return; }
    sec.hidden = false;
    var usd = state.cur === 'usd', unit = usd ? 'USD/kg' : '€/kg';
    var nom = function (o) { return t(o.nom_fr, o.nom_en); };
    var autres = A.origines.filter(function (o) { return !o.reference; });
    var periode = RS.month(A.debut, true) + t(' à ', ' to ') + RS.month(A.fin, true);
    // Phrase de lecture du dernier mois commun, formatée comme le reste du site
    var der = null;
    autres.forEach(function (o) { var i = lastWith(o.mois, 'ecart'); if (i >= 0 && (!der || o.mois[i].p > der)) der = o.mois[i].p; });
    var au = autres.map(function (o) { var m = o.mois.filter(function (x) { return x.p === der; })[0]; return m && ok(m.ecart) ? { o: o, e: m.ecart } : null; })
      .filter(Boolean).sort(function (a, b) { return a.e - b.e; });
    $('af-phrase').textContent = au.length < 2 ? '' :
      t('En ', 'In ') + RS.month(der, true) + t(', l’écart de prix rendu UE avec la Côte d’Ivoire va de ', ', the EU-delivered price gap to Côte d’Ivoire ranges from ') +
      RS.pct(au[0].e, 1, true) + ' (' + nom(au[0].o) + t(') à ', ') to ') + RS.pct(au[au.length - 1].e, 1, true) + ' (' + nom(au[au.length - 1].o) + ').';

    // Classement par volume de caoutchouc technique, prix moyen et écart à la CI
    $('af-rk-sub').textContent = t('Caoutchouc technique (NC 4001 22 00), tonnes et prix moyen, de ', 'Technically specified rubber (CN 4001 22 00), tonnes and average price, ') + periode;
    var tri = A.origines.slice().sort(function (a, b) { return (b.vol_tsnr_t || 0) - (a.vol_tsnr_t || 0); });
    C.ranked($('af-rk'), tri.map(function (o) {
      var prix = usd ? o.prix_usd_kg : o.prix_eur_kg;
      return { name: nom(o), value: o.vol_tsnr_t, strong: o.reference,
        text: RS.int(o.vol_tsnr_t) + ' t · ' + RS.num(prix, 3) + ' ' + unit + (o.reference ? '' : ' · ' + RS.pct(o.ecart_ci_pct, 1, true)) };
    }));
    $('af-rk-note').textContent = t('Prix moyen pondéré par les volumes sur la période ; écart en % avec la Côte d’Ivoire sur la même période. Source : Eurostat Comext.',
      'Volume-weighted average price over the period; gap in % to Côte d’Ivoire over the same period. Source: Eurostat Comext.');

    // Volumes des trois produits
    $('af-prod-sub').textContent = t('Tonnes, de ', 'Tonnes, ') + periode;
    C.table(clear($('af-prod')), [
      { label: t('Origine', 'Origin'), left: true, get: nom },
      { label: t('Caoutchouc technique', 'Technically specified'), get: function (o) { return RS.int(o.vol_tsnr_t); } },
      { label: t('Autres formes sèches', 'Other dry forms'), get: function (o) { return RS.int(o.vol_autres_t); } },
      { label: 'Latex', get: function (o) { return RS.int(o.vol_latex_t); } }
    ], tri, t('NC 4001 22 00, 4001 29 00 et 4001 10 00 (nature T)', 'CN 4001 22 00, 4001 29 00 and 4001 10 00 (nature T)'), true);

    // Petits multiples : écart à la CI, une origine par graphique, échelle commune
    var lo = 0, hi = 0;
    autres.forEach(function (o) { cut(o.mois).forEach(function (m) { if (ok(m.ecart)) { lo = Math.min(lo, m.ecart); hi = Math.max(hi, m.ecart); } }); });
    var host = clear($('af-multiples'));
    autres.forEach(function (o) {
      var mo = cut(o.mois);
      var card = RS.el('div', 'card chart-card', null, host);
      RS.el('h3', null, nom(o), card);
      RS.el('p', 'sub', t('Écart de prix avec la Côte d’Ivoire, en %. Négatif : moins cher que l’ivoirien.', 'Price gap to Côte d’Ivoire, in %. Negative: cheaper than Ivorian rubber.'), card);
      var c = RS.el('div', null, null, card);
      C.line(c, {
        x: mo.map(function (m) { return m.p; }), xFmt: RS.month, height: 200, yDomain: [lo, hi], ref: 0,
        series: [{ name: t('Écart avec la Côte d’Ivoire', 'Gap to Côte d’Ivoire'), color: S1, values: mo.map(function (m) { return m.ecart; }) }],
        yFmt: function (v) { return RS.pct(v, 0); }, tipFmt: function (v) { return RS.pct(v, 1, true); },
        tipExtra: function (i) {
          var m = mo[i], fl = afFlags(m);
          return RS.num(usd ? m.usd : m.eur, 3) + ' ' + unit + ' · ' + RS.int(m.vol_t) + ' t' + (fl ? ' · ' + fl : '');
        },
        label: t('Écart de prix ', 'Price gap ') + nom(o) + t(' / Côte d’Ivoire', ' vs Côte d’Ivoire')
      });
    });

    // Tableau mensuel complet
    var ci = A.origines.filter(function (o) { return o.reference; })[0];
    var by = {};
    A.origines.forEach(function (o) { by[o.code] = {}; o.mois.forEach(function (m) { by[o.code][m.p] = m; }); });
    var mois = cut((ci ? ci.mois : autres[0].mois).map(function (m) { return m.p; })).slice().reverse();
    var px = function (code, p) { var m = by[code][p]; return m ? RS.num(usd ? m.usd : m.eur, 3) : '—'; };
    var cols = [{ label: t('Mois', 'Month'), left: true, get: function (p) { return RS.month(p, true); } },
      { label: t('Côte d’Ivoire ', 'Côte d’Ivoire ') + unit, get: function (p) { return ci ? px(ci.code, p) : '—'; } }];
    autres.forEach(function (o) {
      cols.push({ label: nom(o) + ' ' + unit, get: function (p) { return px(o.code, p); } });
      cols.push({ label: t('écart ', 'gap ') + o.code, get: function (p) { var m = by[o.code][p]; return m ? RS.pct(m.ecart, 1, true) : '—'; } });
    });
    cols.push({ label: t('Mentions', 'Flags'), left: true, get: function (p) {
      var f = [];
      autres.forEach(function (o) { var m = by[o.code][p]; var x = m ? afFlags(m) : ''; if (x) f.push(o.code + ' : ' + x); });
      return f.join(' ; ') || '—';
    } });
    C.table(clear($('t-afrique')), cols, mois,
      t('Tableau mensuel (prix : nature T ; écart : nature E ; méthode ', 'Monthly table (prices: nature T; gap: nature E; method ') + A.version +
      t(' ; douanes mises à jour le ', '; customs updated ') + RS.date(A.maj_eurostat) + ')');
  }

  // ── 1 ter. Zones de production (FAOSTAT, carte, vues satellite) ──
  var prod = { sat: 0, mode: 'image' };
  var SVGNS = 'http://www.w3.org/2000/svg';
  function svgEl(name, attrs, parent) {
    var e = document.createElementNS(SVGNS, name);
    for (var k in attrs) if (Object.prototype.hasOwnProperty.call(attrs, k)) e.setAttribute(k, attrs[k]);
    if (parent) parent.appendChild(e);
    return e;
  }
  function classeProd(t) { return !ok(t) || t <= 0 ? 0 : t >= 1e6 ? 4 : t >= 1e5 ? 3 : t >= 25e3 ? 2 : 1; }
  function tonnes(t) { return t >= 1e6 ? RS.num(t / 1e6, 2) + ' Mt' : t >= 1e4 ? RS.int(t / 1e3) + ' kt' : RS.int(t) + ' t'; }
  function importUE(code) {
    var A = D.afrique; if (!A || !A.origines) return null;
    var o = A.origines.filter(function (x) { return x.code === code; })[0];
    return o ? o.vol_tsnr_t : null;
  }
  function detailPays(p) {
    if (!p) return '';
    var P = D.production, imp = importUE(p.code);
    return t(p.nom_fr, p.nom_en) + ' : ' + RS.int(p.prod_t) + t(' t en ', ' t in ') + P.annee +
      (ok(p.part_afrique_pct) ? ' (' + RS.pct(p.part_afrique_pct, 1) + t(' de l’Afrique)', ' of Africa)') : '') +
      ', ' + RS.int(p.surf_ha) + ' ha, ' + RS.int(p.rend_kg_ha) + ' kg/ha' +
      (ok(imp) ? t('. Importations UE de caoutchouc technique sur 6 mois : ', '. EU imports of technically specified rubber over 6 months: ') + RS.int(imp) + ' t' : '') + '.';
  }

  function drawProduction() {
    var P = D.production, K = window.RS_CARTE, sec = $('production');
    if (!P || !P.pays || !K) { sec.hidden = true; return; }
    sec.hidden = false;
    var byCode = {}; P.pays.forEach(function (p) { byCode[p.code] = p; });
    var afr = P.pays.filter(function (p) { return p.groupe === 'afrique' && ok(p.prod_t); }).sort(function (a, b) { return b.prod_t - a.prod_t; });
    var ci = byCode.CI;
    var monde = P.pays.filter(function (p) { return p.groupe !== 'total' && ok(p.prod_t); }).sort(function (a, b) { return b.prod_t - a.prod_t; });

    // Tuiles
    var k = clear($('prod-kpis'));
    kpi(k, t('Production africaine ', 'African production ') + P.annee, tonnes(P.afrique_t), null,
      RS.pct(100 * P.afrique_t / P.monde_t, 1) + t(' de la production mondiale', ' of world production'), 'O');
    if (ci) kpi(k, t('Côte d’Ivoire ', 'Côte d’Ivoire ') + P.annee, tonnes(ci.prod_t), null,
      t((monde.indexOf(ci) + 1) + 'e producteur mondial · ', 'No. ' + (monde.indexOf(ci) + 1) + ' world producer · ') + RS.pct(ci.part_afrique_pct, 0) + t(' de l’Afrique', ' of Africa'), ci.nature || 'O');
    if (ci && ok(ci.prod_2020_t)) kpi(k, t('Côte d’Ivoire depuis 2020', 'Côte d’Ivoire since 2020'), RS.pct(100 * (ci.prod_t / ci.prod_2020_t - 1), 0, true), null,
      RS.int(ci.prod_2020_t) + ' t → ' + RS.int(ci.prod_t) + ' t', 'O');

    // Carte
    $('carte-titre').textContent = t('Production de caoutchouc naturel par pays, ', 'Natural rubber production by country, ') + P.annee;
    $('carte-sub').textContent = t('FAOSTAT (FAO), mise à jour du ', 'FAOSTAT (FAO), updated ') + RS.date(P.maj_fao) + t(' · fond de carte Natural Earth', ' · base map Natural Earth');
    var host = clear($('carte'));
    var svg = svgEl('svg', { viewBox: '0 0 ' + K.largeur + ' ' + K.hauteur, 'class': 'carte', role: 'img',
      'aria-label': t('Carte de la production de caoutchouc naturel en Afrique de l’Ouest et centrale', 'Map of natural rubber production in West and Central Africa') }, host);
    var detail = $('carte-detail');
    var labels = [];
    K.pays.forEach(function (c) {
      var p = byCode[c.iso2], q = p ? classeProd(p.prod_t) : 0;
      var path = svgEl('path', { d: c.d, 'class': 'pays' + (q ? ' q' + q + ' prod' : '') }, svg);
      var tt = svgEl('title', {}, path); tt.textContent = p ? detailPays(p) : t(c.nom_fr, c.nom_en);
      if (p && q) {
        path.setAttribute('tabindex', '0'); path.setAttribute('role', 'button');
        path.setAttribute('aria-label', detailPays(p));
        var montrer = function () { detail.textContent = detailPays(p); };
        path.addEventListener('pointerenter', montrer); path.addEventListener('focus', montrer); path.addEventListener('click', montrer);
        if (p.prod_t >= 25e3) labels.push({ path: path, txt: t(p.nom_fr, p.nom_en) });
      }
    });
    // Étiquettes au centre du pays, décalées vers le haut si un point satellite les couvre
    var pinsXY = (P.zones || []).map(function (z) { return [(z.lon - K.bbox.xmin) * K.echelle, (K.bbox.ymax - z.lat) * K.echelle]; });
    labels.forEach(function (l) {
      var b = l.path.getBBox(), x = b.x + b.width / 2, y = b.y + b.height / 2 + 4;
      // un point satellite couvre l'étiquette : on la place sous le point (côté mer pour les pays côtiers)
      var pin = pinsXY.filter(function (q) { return Math.abs(q[0] - x) < 60 && Math.abs(q[1] - y + 6) < 22; })[0];
      if (pin) { x = pin[0]; y = pin[1] + 36; }
      var tx = svgEl('text', { x: x.toFixed(1), y: y.toFixed(1), 'class': 'lbl', 'text-anchor': 'middle' }, svg);
      tx.textContent = l.txt;
    });
    (P.zones || []).forEach(function (z, i) {
      var x = (z.lon - K.bbox.xmin) * K.echelle, y = (K.bbox.ymax - z.lat) * K.echelle;
      var g = svgEl('g', { 'class': 'pin' + (i === prod.sat ? ' on' : ''), tabindex: '0', role: 'button',
        'aria-label': t('Vue satellite : ', 'Satellite view: ') + t(z.titre_fr, z.titre_en) }, svg);
      svgEl('circle', { cx: x.toFixed(1), cy: y.toFixed(1), r: 15 }, g);
      svgEl('text', { x: x.toFixed(1), y: (y + 6).toFixed(1) }, g).textContent = String(i + 1);
      var choisir = function () { prod.sat = i; drawSatellite(); drawProductionPins(); };
      g.addEventListener('click', choisir);
      g.addEventListener('keydown', function (ev) { if (ev.key === 'Enter' || ev.key === ' ') { ev.preventDefault(); choisir(); } });
    });
    var lg = clear($('carte-legende'));
    [[4, '≥ 1 Mt'], [3, '100 kt – 1 Mt'], [2, '25 – 100 kt'], [1, '< 25 kt'], [0, t('non producteur ou non suivi', 'non-producer or not tracked')]].forEach(function (x) {
      var s = RS.el('span', null, null, lg); RS.el('i', null, null, s).style.background = 'var(--q' + x[0] + ')'; RS.el('span', null, x[1], s);
    });
    var sp = RS.el('span', null, null, lg); RS.el('i', 'pinkey', null, sp); RS.el('span', null, t('vue satellite', 'satellite view'), sp);
    detail.textContent = ci ? detailPays(ci) : '';

    // Classement mondial (Côte d'Ivoire et pays africains mis en avant)
    $('rang-sub').textContent = t('Production ', 'Production ') + P.annee + t(', tonnes (FAOSTAT). En couleur : pays africains.', ', tonnes (FAOSTAT). Coloured: African countries.');
    C.ranked($('rang'), monde.slice(0, 12).map(function (p) {
      return { name: t(p.nom_fr, p.nom_en), value: p.prod_t, strong: p.groupe === 'afrique', text: tonnes(p.prod_t) + ' · ' + RS.pct(p.part_monde_pct, 1) };
    }));

    // Tableau des producteurs africains
    C.table(clear($('t-production')), [
      { label: t('Pays', 'Country'), left: true, get: function (p) { return t(p.nom_fr, p.nom_en); } },
      { label: t('Production (t)', 'Production (t)'), get: function (p) { return RS.int(p.prod_t); } },
      { label: t('Part de l’Afrique', 'Share of Africa'), get: function (p) { return RS.pct(p.part_afrique_pct, 1); } },
      { label: t('Surface (ha)', 'Area (ha)'), get: function (p) { return RS.int(p.surf_ha); } },
      { label: t('Rendement (kg/ha)', 'Yield (kg/ha)'), get: function (p) { return RS.int(p.rend_kg_ha); } },
      { label: t('Depuis 2020', 'Since 2020'), get: function (p) { return ok(p.prod_2020_t) && p.prod_2020_t > 0 ? RS.pct(100 * (p.prod_t / p.prod_2020_t - 1), 0, true) : '—'; } },
      { label: t('Import UE 6 mois (t)', 'EU imports 6 months (t)'), get: function (p) { var v = importUE(p.code); return ok(v) ? RS.int(v) : '—'; } },
      { label: 'Nature', left: true, get: function (p) { return p.nature || '—'; } }
    ], afr, t('Producteurs africains, ', 'African producers, ') + P.annee + ' (' + P.version + t(', licence CC BY 4.0)', ', CC BY 4.0 licence)'), true);

    drawSatellite();
  }
  function drawProductionPins() {
    var pins = document.querySelectorAll('#carte .pin');
    for (var i = 0; i < pins.length; i++) pins[i].setAttribute('class', 'pin' + (i === prod.sat ? ' on' : ''));
  }
  function drawSatellite() {
    var P = D.production, zs = P.zones || [];
    if (!zs.length) return;
    var z = zs[Math.min(prod.sat, zs.length - 1)];
    var choix = clear($('sat-choix'));
    zs.forEach(function (x, i) {
      var b = RS.el('button', null, (i + 1) + '. ' + t(x.titre_fr, x.titre_en).split(' : ')[0].split(': ')[0], choix);
      b.type = 'button'; b.setAttribute('aria-pressed', i === prod.sat ? 'true' : 'false');
      b.addEventListener('click', function () { prod.sat = i; drawSatellite(); drawProductionPins(); });
    });
    var E = window.RS_EUDR, eb = E && E.bassins ? E.bassins.filter(function (x) { return x.id === z.id; })[0] : null;
    var eudr = prod.mode === 'eudr' && eb;
    var mb = document.querySelectorAll('[data-sat-mode]');
    for (var m = 0; m < mb.length; m++) {
      mb[m].setAttribute('aria-pressed', mb[m].getAttribute('data-sat-mode') === (eudr ? 'eudr' : 'image') ? 'true' : 'false');
      mb[m].disabled = !eb && mb[m].getAttribute('data-sat-mode') === 'eudr';
    }
    // En mode EUDR, nom géographique seulement : l'indicateur ne désigne aucun acteur
    var lieu = t(z.titre_fr, z.titre_en).split(' : ')[0].split(': ')[0];
    $('sat-titre').textContent = (prod.sat + 1) + '. ' + (eudr ? t('Zone de ', 'Area around ') + lieu : t(z.titre_fr, z.titre_en));
    var src = eudr ? z.image.replace(/\.jpg$/, '_eudr.jpg') : z.image;
    var img = $('sat-img');
    img.src = src; img.alt = (eudr ? t('Indicateur EUDR sur vue satellite 2016 : zone de ', 'EUDR indicator on 2016 satellite view: area around ') + lieu
      : t('Vue satellite 2016 : ', '2016 satellite view: ') + t(z.titre_fr, z.titre_en));
    var lien = $('sat-lien'); lien.href = src; lien.title = t('Ouvrir l’image en taille réelle', 'Open the full-size image');
    var lg = clear($('sat-leg-eudr')); lg.hidden = !eudr;
    if (eudr) [['eudr-foret', t('forêt au 31/12/2020 (JRC)', 'forest on 31/12/2020 (JRC)')],
               ['eudr-hevea', t('hévéa en 2024 (Forest Data Partnership)', 'rubber in 2024 (Forest Data Partnership)')],
               ['eudr-rouge', t('perte de couvert après 2020 sur cette forêt', 'tree cover loss after 2020 on that forest')],
               ['eudr-jaune', t('perte après 2020 hors forêt (plantations, cultures)', 'loss after 2020 outside forest (plantations, crops)')]].forEach(function (x) {
      var s = RS.el('span', null, null, lg); RS.el('i', x[0], null, s); RS.el('span', null, x[1], s);
    });
    $('sat-legende').textContent = (eudr ? t('Carré d’environ ', 'Square of about ') + RS.int(eb.surface_ha) + t(' ha ; il dépasse les limites de toute exploitation.', ' ha; it extends beyond any single operation.')
      : t(z.legende_fr, z.legende_en)) + t(' Largeur de l’image : environ ', ' Image width: about ') + RS.num(z.largeur_km, 0) +
      t(' km. Cliquez sur l’image pour la voir en taille réelle.', ' km. Click the image to see it full size.');
    var cr = clear($('sat-credit'));
    RS.el('span', null, t('Image : ', 'Image: ') + P.satellite_credit + (eudr ? '. ' + E.sources.foret + '. ' + E.sources.pertes + (E.sources.hevea ? '. ' + E.sources.hevea : '') : '') +
      t('. Position : ', '. Location: ') + z.coordonnees + '. ', cr);
    if (!eudr) { var a = RS.el('a', null, t('Source sur ce site de production', 'Source on this production site'), cr);
                 a.href = z.source; a.rel = 'noopener noreferrer nofollow'; a.target = '_blank'; }
    drawEudr();
  }

  // Indicateur EUDR des quatre bassins (outil scripts/outils/eudr_bassins.R)
  function drawEudr() {
    var E = window.RS_EUDR, P = D.production, card = $('eudr-card');
    if (!E || !E.bassins || !E.bassins.length || !P) { card.hidden = true; return; }
    card.hidden = false;
    var zs = P.zones || [], z = zs[Math.min(prod.sat, zs.length - 1)];
    var eb = E.bassins.filter(function (x) { return x.id === z.id; })[0] || E.bassins[0];
    var lieu = function (id) { var zz = zs.filter(function (x) { return x.id === id; })[0]; return zz ? t(zz.titre_fr, zz.titre_en).split(' : ')[0].split(': ')[0] : id; };
    $('eudr-titre').textContent = t('Indicateur de risque EUDR : zone de ', 'EUDR risk indicator: area around ') + lieu(eb.id);
    $('eudr-sub').textContent = t('Date de référence EUDR : 31 décembre 2020. Calcul du ', 'EUDR cut-off date: 31 December 2020. Calculated ') + RS.date(E.date_calcul) + ' · ' + E.version;
    var k = clear($('eudr-kpis'));
    kpi(k, t('Forêt au 31/12/2020', 'Forest on 31/12/2020'), RS.int(eb.foret_2020_ha), 'ha', RS.pct(eb.part_foret_pct, 1) + t(' du carré (JRC, 10 m)', ' of the square (JRC, 10 m)'), 'E');
    kpi(k, t('Perte de couvert après 2020, sur cette forêt', 'Tree cover loss after 2020, on that forest'), RS.num(eb.perte_apres_2020_foret_ha, 0), 'ha',
      RS.pct(eb.part_perte_foret_pct, 1) + t(' de la forêt 2020 (Hansen, 2021-2024)', ' of 2020 forest (Hansen, 2021-2024)'), 'E');
    kpi(k, t('Perte après 2020 hors forêt', 'Loss after 2020 outside forest'), RS.num(eb.perte_apres_2020_hors_foret_ha, 0), 'ha',
      t('plantations, cultures, jachères : non visées par l’EUDR comme déforestation', 'plantations, crops, fallows: not EUDR deforestation'), 'E');
    if (eb.hevea_2024_ha != null) {
      kpi(k, t('Hévéa en 2024', 'Rubber in 2024'), RS.int(eb.hevea_2024_ha), 'ha',
        RS.pct(eb.part_hevea_2024_pct, 1) + t(' du carré ; ', ' of the square; ') + RS.int(eb.hevea_2020_ha) + t(' ha en 2020 (FDP)', ' ha in 2020 (FDP)'), 'E');
      kpi(k, t('Nouvel hévéa sur forêt 2020', 'New rubber on 2020 forest'), RS.num(eb.hevea_nouveau_sur_foret_2020_ha, 1), 'ha',
        t('plantation probable sur une forêt après la date de référence EUDR', 'probable planting on forest after the EUDR cut-off date'), 'E');
    }
    var ans = Object.keys(eb.perte_par_an);
    C.bars($('eudr-bars'), {
      x: ans, xFmt: function (a) { return String(a); }, values: ans.map(function (a) { return eb.perte_par_an[a]; }), color: 'var(--s1)',
      name: t('Perte de couvert arboré, tout le carré', 'Tree cover loss, whole square'), height: 200,
      yFmt: function (v) { return RS.int(v); }, tipFmt: function (v) { return RS.int(v) + ' ha'; },
      tipExtra: function (i) { return +ans[i] > 2020 ? t('après la date de référence EUDR', 'after the EUDR cut-off date') : null; },
      label: t('Perte de couvert arboré par an, 2001-2024 (ha)', 'Tree cover loss per year, 2001-2024 (ha)')
    });
    C.table(clear($('t-eudr')), [
      { label: t('Zone', 'Area'), left: true, get: function (b) { return lieu(b.id); } },
      { label: t('Surface du carré (ha)', 'Square area (ha)'), get: function (b) { return RS.int(b.surface_ha); } },
      { label: t('Forêt 2020 (ha)', '2020 forest (ha)'), get: function (b) { return RS.int(b.foret_2020_ha); } },
      { label: t('Perte après 2020 sur forêt (ha)', 'Loss after 2020 on forest (ha)'), get: function (b) { return RS.num(b.perte_apres_2020_foret_ha, 0); } },
      { label: t('en % de la forêt 2020', '% of 2020 forest'), get: function (b) { return RS.pct(b.part_perte_foret_pct, 1); } },
      { label: t('Perte après 2020 hors forêt (ha)', 'Loss after 2020 outside forest (ha)'), get: function (b) { return RS.num(b.perte_apres_2020_hors_foret_ha, 0); } },
      { label: t('Perte 2001-2020 (ha)', 'Loss 2001-2020 (ha)'), get: function (b) { return RS.int(b.perte_2001_2020_ha); } },
      { label: t('Hévéa 2020 (ha)', 'Rubber 2020 (ha)'), get: function (b) { return b.hevea_2020_ha == null ? '–' : RS.int(b.hevea_2020_ha); } },
      { label: t('Hévéa 2024 (ha)', 'Rubber 2024 (ha)'), get: function (b) { return b.hevea_2024_ha == null ? '–' : RS.int(b.hevea_2024_ha); } },
      { label: t('Nouvel hévéa sur forêt 2020 (ha)', 'New rubber on 2020 forest (ha)'), get: function (b) { return b.hevea_nouveau_sur_foret_2020_ha == null ? '–' : RS.num(b.hevea_nouveau_sur_foret_2020_ha, 1); } }
    ], E.bassins, t('Les quatre zones (nature E : calcul RubberSignal Technologies sur données JRC, UMD et Forest Data Partnership)', 'The four areas (nature E: RubberSignal Technologies calculation on JRC, UMD and Forest Data Partnership data)'), true);
  }
  document.addEventListener('click', function (ev) {
    var b = ev.target.closest ? ev.target.closest('[data-sat-mode]') : null;
    if (!b || b.disabled) return;
    prod.mode = b.getAttribute('data-sat-mode');
    drawSatellite();
  });

  // ── 2. Part du planteur ───────────────────────────────────
  function monthsBetween(a, b) {
    var out = [], y = +a.slice(0, 4), m = +a.slice(5, 7);
    for (var guard = 0; guard < 240; guard++) {
      var p = y + '-' + (m < 10 ? '0' : '') + m;
      out.push(p);
      if (p === b) break;
      m += 1; if (m > 12) { m = 1; y += 1; }
    }
    return out;
  }

  function drawPlanteur() {
    var h = D.rsci.historique;
    if (!h.length) return;
    var axis = cut(monthsBetween(h[0].mois, h[h.length - 1].mois));
    var by = {};
    h.forEach(function (r) { by[r.mois] = r; });
    var extra = function (i) {
      var r = by[axis[i]];
      if (!r) return t('Prix officiel non disponible pour ce mois', 'Official price not available for this month');
      return RS.int(r.apromac_fcfa) + ' FCFA/kg · ' + r.type + (r.source_faible ? t(' · source à confirmer', ' · source to be confirmed') : '');
    };
    C.line($('c-rsci'), {
      x: axis, xFmt: RS.month, height: 240, endLabels: true,
      series: [{ name: 'RSCI', color: S1, values: axis.map(function (p) { return by[p] ? by[p].rsci_pct : null; }) }],
      yFmt: function (v) { return RS.pct(v, 0); }, tipFmt: function (v) { return RS.pct(v, 1); }, tipExtra: extra,
      label: t('Part du prix TSR20 reçue par le planteur', 'Share of the TSR20 price received by the farmer')
    });
    C.bars($('c-apromac'), {
      x: axis, xFmt: RS.month, height: 240, color: S1, name: t('Prix APROMAC', 'APROMAC price'),
      values: axis.map(function (p) { return by[p] ? by[p].apromac_fcfa : null; }),
      yFmt: function (v) { return RS.int(v); }, tipFmt: function (v) { return RS.int(v) + ' FCFA/kg'; },
      tipExtra: function (i) { var r = by[axis[i]]; return r ? r.type + (r.source_faible ? t(' · source à confirmer', ' · source to be confirmed') : '') : t('Non disponible', 'Not available'); },
      label: t('Prix officiel APROMAC du mois', 'Official monthly APROMAC price')
    });

    C.table(clear($('t-rsci')), [
      { label: t('Mois du prix', 'Price month'), left: true, get: function (r) { return RS.month(r.mois, true); } },
      { label: 'APROMAC (FCFA/kg)', get: function (r) { return RS.int(r.apromac_fcfa); } },
      { label: t('TSR20 de référence', 'Reference TSR20'), get: function (r) { return RS.num(r.tsr20_ref, 2) + ' (' + RS.month(r.ref_mois) + ')'; } },
      { label: t('Taux USD/EUR', 'USD/EUR rate'), get: function (r) { return RS.num(r.taux_usd_eur, 4); } },
      { label: t('Prix sec (USD/kg)', 'Dry price (USD/kg)'), get: function (r) { return RS.num(r.prix_sec_usd, 3); } },
      { label: 'RSCI', get: function (r) { return RS.pct(r.rsci_pct, 1); } },
      { label: t('Part officielle (du prix net)', 'Official share (of net price)'), get: function (r) { return RS.pct(r.part_officielle_pct, 0); } },
      { label: t('Type de prix et source', 'Price type and source'), left: true, wrap: true, get: function (r) { return r.type + ' · ' + r.source + (r.source_faible ? t(' (à confirmer)', ' (to be confirmed)') : ''); } }
    ], h.slice().reverse(), t('Historique mensuel (nature E, méthode ', 'Monthly history (nature E, method ') + D.rsci.version + ')', true);

    var i = lastWith(h, 'rsci_pct');
    var f = clear($('fiche-rsci'));
    if (i < 0) return;
    var r = h[i];
    $('fiche-titre').textContent = t('Fiche de calcul · prix de ', 'Calculation sheet · price of ') + RS.month(r.mois, true);
    var line = function (label, value, total) {
      var tr = RS.el('tr', total ? 'total' : null, null, f);
      RS.el('td', null, label, tr); RS.el('td', null, value, tr);
    };
    line(t('Prix APROMAC, caoutchouc humide (nature O)', 'APROMAC price, wet rubber (nature O)'), RS.int(r.apromac_fcfa) + ' FCFA/kg');
    line(t('Teneur en caoutchouc sec de référence (DRC)', 'Reference dry rubber content (DRC)'), RS.pct(D.rsci.drc * 100, 0));
    line(t('Parité fixe', 'Fixed parity'), RS.num(655.957, 3) + ' FCFA = 1 EUR');
    line(t('Taux USD/EUR, moyenne BCE de ', 'USD/EUR rate, ECB average for ') + RS.month(r.ref_mois, true), RS.num(r.taux_usd_eur, 4));
    line(t('Prix planteur, équivalent sec', 'Farmer price, dry equivalent'), RS.num(r.prix_sec_usd, 3) + ' USD/kg');
    line(t('TSR20 Banque mondiale de ', 'World Bank TSR20 for ') + RS.month(r.ref_mois, true) + ' (T)', RS.num(r.tsr20_ref, 2) + ' USD/kg');
    line('RSCI (E)', RS.pct(r.rsci_pct, 1), true);
  }

  // ── 3. Références mondiales ───────────────────────────────
  function drawReferences() {
    var bm = cut(D.references.bm_tsr20);
    C.line($('c-tsr20'), {
      x: bm.map(function (p) { return p.p; }), xFmt: RS.month, height: 260,
      series: [{ name: 'TSR20', color: S3, values: bm.map(function (p) { return p.v; }) }],
      yFmt: function (v) { return RS.num(v, 2); }, tipFmt: function (v) { return RS.num(v, 2) + ' USD/kg'; },
      label: t('TSR20, moyenne mensuelle Banque mondiale', 'TSR20, World Bank monthly average')
    });
    var fmi = {};
    D.references.fmi.forEach(function (p) { fmi[p.p] = p.v; });
    C.table(clear($('t-tsr20')), [
      { label: t('Mois', 'Month'), left: true, get: function (p) { return RS.month(p.p, true); } },
      { label: 'TSR20 (USD/kg)', get: function (p) { return RS.num(p.v, 2); } },
      { label: t('Prix mondial FMI (proche du RSS3, USD/kg)', 'IMF world price (tracks RSS3, USD/kg)'), get: function (p) { return RS.num(fmi[p.p], 3); } }
    ], bm.slice().reverse(), t('Tableau des valeurs (nature T ; Banque mondiale, licence CC BY 4.0 ; FMI via FRED)', 'Table of values (nature T; World Bank, CC BY 4.0; IMF via FRED)'));

    var v = D.volatilite, host = clear($('vol'));
    if (!v || !v.horizons) { RS.el('p', 'note', t('Non disponible cette semaine.', 'Not available this week.'), host); return; }
    $('vol-sub').textContent = t('Départ : ', 'Start: ') + RS.num(v.depart, 2) + ' USD/kg (' + RS.month(v.depart_periode, true) + ') · ' +
      t('volatilité annuelle ', 'annual volatility ') + RS.pct(v.volatilite_pct, 1);
    var lo = Math.min.apply(null, v.horizons.map(function (x) { return x.p10; }));
    var hi = Math.max.apply(null, v.horizons.map(function (x) { return x.p90; }));
    var pos = function (val) { return 100 * (val - lo) / (hi - lo); };
    v.horizons.forEach(function (x) {
      var row = RS.el('div', 'range-row', null, host);
      RS.el('span', null, x.sem + t(' semaines', ' weeks'), row);
      var tr = RS.el('div', 'range-track', null, row);
      tr.title = RS.num(x.p10, 2) + ' – ' + RS.num(x.p90, 2) + ' USD/kg';
      var band = RS.el('span', 'band', null, tr);
      band.style.left = pos(x.p10) + '%'; band.style.width = Math.max(1, pos(x.p90) - pos(x.p10)) + '%';
      var mid = RS.el('span', 'mid', null, tr); mid.style.left = pos(x.p50) + '%';
      var vals = RS.el('div', 'range-vals', null, row);
      RS.el('span', null, RS.num(x.p10, 2), vals);
      RS.el('span', null, t('médiane ', 'median ') + RS.num(x.p50, 2), vals);
      RS.el('span', null, RS.num(x.p90, 2), vals);
    });
    $('vol-note').textContent = t('Bornes 10 % et 90 % de ', '10% and 90% bounds of ') + RS.int(v.n_simulations) +
      t(' simulations sans tendance : amplitude compatible avec la volatilité passée, pas une prévision. Nature E, ', ' drift-free simulations: range consistent with past volatility, not a forecast. Nature E, ') + v.version + '.';
  }

  // ── 3 bis. Simulateur de budget d'achat ───────────────────
  var sim = { mois: 6 };
  function moisPlus(p, k) {
    var y = +p.slice(0, 4), m = +p.slice(5, 7) + k;
    while (m > 12) { m -= 12; y += 1; }
    return y + '-' + (m < 10 ? '0' : '') + m;
  }
  function lireNombre(id, min, max, facultatif) {
    var el = $(id), raw = String(el.value).trim().replace(',', '.');
    if (facultatif && raw === '') { el.removeAttribute('aria-invalid'); return null; }
    var v = parseFloat(raw), bon = isFinite(v) && v >= min && v <= max;
    if (bon) el.removeAttribute('aria-invalid'); else el.setAttribute('aria-invalid', 'true');
    return bon ? v : NaN;
  }
  // Taux fixe pour l'affichage en euros : dernier mois douanier (prix USD ÷ prix EUR)
  function tauxFixe() {
    var ms = D.rapi.mois;
    for (var i = ms.length - 1; i >= 0; i--) if (ok(ms[i].ci_usd) && ok(ms[i].ci_eur) && ms[i].ci_eur > 0) return { t: ms[i].ci_usd / ms[i].ci_eur, p: ms[i].p };
    return null;
  }
  function montant(usd, fx) {
    var eur = state.cur === 'eur' && fx, v = eur ? usd / fx.t : usd, u = eur ? ' €' : ' USD';
    return Math.abs(v) >= 1e6 ? RS.num(v / 1e6, 2) + ' M' + u : RS.int(v) + u;
  }

  function drawBudget() {
    var V = D.volatilite, sec = $('budget');
    if (!V || !ok(V.depart) || !ok(V.volatilite_pct) || !window.RSBudget) { sec.hidden = true; return; }
    sec.hidden = false;
    var b = document.querySelectorAll('[data-sim-mois]');
    for (var i = 0; i < b.length; i++) b[i].setAttribute('aria-pressed', +b[i].getAttribute('data-sim-mois') === sim.mois ? 'true' : 'false');

    // Aide : écart moyen observé entre le prix ivoirien rendu UE et le TSR20 (6 derniers mois communs)
    var ms = D.rapi.mois.filter(function (m) { return ok(m.ci_usd) && ok(m.tsr20_bm); }).slice(-6);
    var aide = $('sim-prime-aide'); aide.textContent = '';
    if (ms.length) {
      var ec = ms.reduce(function (s, m) { return s + (m.ci_usd - m.tsr20_bm); }, 0) / ms.length;
      RS.el('span', null, t('Repère : sur les ', 'Benchmark: over the last ') + ms.length +
        t(' derniers mois publiés, le caoutchouc ivoirien rendu UE s’écarte en moyenne de ', ' published months, Ivorian rubber delivered to the EU differs on average by ') +
        (ec >= 0 ? '+' : '') + RS.num(ec, 3) + t(' USD/kg du TSR20. ', ' USD/kg from TSR20. '), aide);
      var bt = RS.el('button', 'linkbtn', t('Utiliser cet écart', 'Use this gap'), aide);
      bt.type = 'button';
      bt.addEventListener('click', function () { $('sim-prime').value = ec.toFixed(3); calculer(); });
    }
    calculer();
  }

  var attente = null;
  function calculer() {
    var V = D.volatilite, fx = tauxFixe();
    var vol = lireNombre('sim-volume', 1, 100000), prime = lireNombre('sim-prime', -1, 2), budget = lireNombre('sim-budget', 0, 1e12, true);
    var k = clear($('sim-kpis')), bt = $('sim-budget-txt'), hm = clear($('sim-mois'));
    var debut = moisPlus(V.depart_periode, 1), fin = moisPlus(V.depart_periode, sim.mois);
    $('sim-titre').textContent = t('Coût de vos achats, de ', 'Cost of your purchases, ') + RS.month(debut, true) + t(' à ', ' to ') + RS.month(fin, true);
    if (!isFinite(vol) || !isFinite(prime) || (budget !== null && !isFinite(budget))) {
      bt.textContent = t('Vérifiez les valeurs saisies (volume de 1 à 100 000 t, prime de −1 à +2 USD/kg).', 'Check the values entered (volume 1 to 100,000 t, premium −1 to +2 USD/kg).');
      return;
    }
    var budgetUsd = budget === null ? null : (state.cur === 'eur' && fx ? budget * fx.t : budget);
    var r = window.RSBudget.simuler({ prix0: V.depart, sigma: V.volatilite_pct / 100, mois: sim.mois, volume_t: vol, prime: prime, budget: budgetUsd || 0 });
    var unitKg = state.cur === 'eur' && fx ? '€/kg' : 'USD/kg', conv = function (x) { return state.cur === 'eur' && fx ? x / fx.t : x; };
    kpi(k, t('Coût total médian', 'Median total cost'), montant(r.cout.p50, fx), null, RS.int(vol * sim.mois) + t(' t achetées', ' t bought'), 'E');
    kpi(k, t('8 cas sur 10 entre', '8 cases out of 10 between'), montant(r.cout.p10, fx), null, t('et ', 'and ') + montant(r.cout.p90, fx), 'E');
    kpi(k, t('Prix moyen payé (médian)', 'Average price paid (median)'), RS.num(conv(r.prix_moyen.p50), 3), unitKg,
      RS.num(conv(r.prix_moyen.p10), 3) + ' – ' + RS.num(conv(r.prix_moyen.p90), 3), 'E');
    if (r.prob_depassement !== null)
      kpi(k, t('Risque de dépasser votre budget', 'Risk of exceeding your budget'), RS.pct(r.prob_depassement * 100, 0), null,
        t('budget de ', 'budget of ') + montant(budgetUsd, fx), 'E');
    var lignes = [t('Budget qui couvre 9 cas sur 10 : ', 'Budget covering 9 cases out of 10: ') + montant(r.budget_p90, fx) + '.'];
    if (state.cur === 'eur' && fx) lignes.push(t('Euros au taux fixe de ', 'Euros at the fixed rate of ') + RS.num(fx.t, 4) + t(' USD/€ (moyenne BCE, ', ' USD/€ (ECB average, ') + RS.month(fx.p, true) + ').');
    var devs = document.querySelectorAll('.sim-dev');
    for (var d = 0; d < devs.length; d++) devs[d].textContent = state.cur === 'eur' && fx ? '€' : 'USD';
    lignes.push(t('Départ : TSR20 Banque mondiale, ', 'Start: World Bank TSR20, ') + RS.month(V.depart_periode, true) + t(' : ', ': ') + RS.num(V.depart, 2) +
      t(' USD/kg ; volatilité annuelle ', ' USD/kg; annual volatility ') + RS.pct(V.volatilite_pct, 1) + ' (' + V.version + ', ' + RS.int(r.n) + t(' trajectoires).', ' paths).'));
    bt.textContent = lignes.join(' ');

    // Fourchette mensuelle du prix TSR20 (sans prime), échelle commune
    $('sim-mois-sub').textContent = unitKg + t(' · bornes 10 % et 90 %, trait : médiane', ' · 10% and 90% bounds, mark: median');
    var lo = conv(Math.min.apply(null, r.mois.map(function (x) { return x.p10; }))), hi = conv(Math.max.apply(null, r.mois.map(function (x) { return x.p90; })));
    var pos = function (v) { return 100 * (conv(v) - lo) / (hi - lo); };
    r.mois.forEach(function (x) {
      var row = RS.el('div', 'range-row', null, hm);
      RS.el('span', null, RS.month(moisPlus(V.depart_periode, x.m), false), row);
      var tr = RS.el('div', 'range-track', null, row);
      tr.title = RS.num(conv(x.p10), 2) + ' – ' + RS.num(conv(x.p90), 2) + ' ' + unitKg;
      var band = RS.el('span', 'band', null, tr);
      band.style.left = pos(x.p10) + '%'; band.style.width = Math.max(1, pos(x.p90) - pos(x.p10)) + '%';
      RS.el('span', 'mid', null, tr).style.left = pos(x.p50) + '%';
      var vals = RS.el('div', 'range-vals', null, row);
      RS.el('span', null, RS.num(conv(x.p10), 2), vals);
      RS.el('span', null, t('médiane ', 'median ') + RS.num(conv(x.p50), 2), vals);
      RS.el('span', null, RS.num(conv(x.p90), 2), vals);
    });
  }
  document.addEventListener('input', function (ev) {
    if (!ev.target || !ev.target.closest || !ev.target.closest('#sim-form')) return;
    clearTimeout(attente); attente = setTimeout(calculer, 150);
  });
  document.addEventListener('click', function (ev) {
    var b = ev.target.closest ? ev.target.closest('[data-sim-mois]') : null;
    if (!b) return;
    sim.mois = +b.getAttribute('data-sim-mois');
    drawBudget();
  });
  var formSim = $('sim-form');
  if (formSim) formSim.addEventListener('submit', function (ev) { ev.preventDefault(); calculer(); });

  // ── 4. Signaux ────────────────────────────────────────────
  var COMP = {
    meteo: ['Météo et offre', 'Weather and supply'], devises: ['Devises (yuan)', 'Currencies (yuan)'],
    demande: ['Demande industrielle', 'Industrial demand'], terrain: ['Terrain Côte d’Ivoire', 'Côte d’Ivoire field data'],
    geo: ['Géopolitique', 'Geopolitics']
  };
  var SENS = { hausse: ['↑ hausse', '↑ up'], baisse: ['↓ baisse', '↓ down'], neutre: ['→ neutre', '→ neutral'], absente: ['absente', 'missing'] };

  function drawSignaux() {
    var host = clear($('prersi')), p = D.pre_rsi;
    if (!p) { RS.el('p', 'note', t('Non publié cette semaine (moins de 3 composantes disponibles).', 'Not published this week (fewer than 3 components available).'), host); }
    else {
      var sc = RS.el('div', 'score', null, host);
      RS.el('b', null, RS.int(p.score), sc);
      RS.el('span', null, '/100 · ' + (t(p.signal_fr, p.signal_en) || ''), sc);
      var me = RS.el('div', 'meter', null, host);
      me.setAttribute('role', 'img');
      me.setAttribute('aria-label', 'Pré-RSI ' + p.score + '/100');
      RS.el('span', 'zone', null, me);
      RS.el('span', 'mark', null, me).style.left = Math.max(0, Math.min(100, p.score)) + '%';
      var scale = RS.el('div', 'meter-scale', null, host);
      RS.el('span', null, t('0 · baissier sous 45', '0 · bearish below 45'), scale);
      RS.el('span', null, t('haussier au-dessus de 55 · 100', 'bullish above 55 · 100'), scale);
      C.table(host, [
        { label: t('Composante', 'Component'), left: true, get: function (c) { var n = COMP[c.cle]; return n ? t(n[0], n[1]) : c.cle; } },
        { label: t('Score (0 à 100)', 'Score (0 to 100)'), get: function (c) { return ok(c.score) ? RS.int(c.score) : '—'; } },
        { label: t('Sens pour le prix', 'Price direction'), get: function (c) { var s = SENS[c.sens] || SENS.absente; return t(s[0], s[1]); } },
        { label: t('Poids nominal', 'Nominal weight'), get: function (c) { return RS.pct(c.poids_nominal * 100, 0); } },
        { label: t('Poids effectif', 'Effective weight'), get: function (c) { return RS.pct(c.poids_effectif * 100, 1); } }
      ], p.composantes, t('Composantes (nature E, ', 'Components (nature E, ') + p.version + ')', true);
    }
    drawActualites();
    var mh = clear($('meteo')), m = D.meteo;
    if (!m || !m.zones) { RS.el('p', 'note', t('Non disponible.', 'Not available.'), mh); return; }
    C.table(mh, [
      { label: t('Zone', 'Area'), left: true, get: function (z) { return z.ville + ', ' + z.pays; } },
      { label: t('Pluie, 30 jours (mm)', 'Rain, 30 days (mm)'), get: function (z) { return RS.num(z.pluie_30j_mm, 0); } },
      { label: t('Température (°C)', 'Temperature (°C)'), get: function (z) { return RS.num(z.temp_c, 1); } },
      { label: t('Score de production', 'Production score'), get: function (z) { return ok(z.score_production) ? RS.int(z.score_production) : '—'; } }
    ], m.zones, t('Relevé du ', 'Reading of ') + RS.date(m.date) + ' · ' + m.source, true);
  }

  // Actualités : titre, éditeur, date et lien (le contenu reste chez l'éditeur)
  function drawActualites() {
    var A = D.actualites, card = $('actu-card');
    if (!A || !A.articles || !A.articles.length) { card.hidden = true; return; }
    card.hidden = false;
    var ul = clear($('actu'));
    A.articles.forEach(function (a) {
      var li = RS.el('li', null, null, ul);
      var meta = RS.el('span', 'news-meta', null, li);
      RS.el('span', 'tag', t(a.categorie_fr, a.categorie_en), meta);
      if (a.tension) RS.el('span', 'tag', t('tension', 'tension'), meta);
      RS.el('span', null, a.source + ' · ' + RS.date(a.date), meta);
      if (a.lien) {
        var l = RS.el('a', 'news-title', a.titre, li);
        l.href = a.lien; l.rel = 'noopener noreferrer nofollow'; l.target = '_blank';
      } else RS.el('span', 'news-title', a.titre, li);
    });
    $('actu-note').textContent = RS.int(A.n) + t(' titres retenus cette semaine (flux RSS : Google News et éditeurs). Titres et liens seulement : le contenu appartient aux éditeurs. « tension » : titre compté dans la composante géopolitique du Pré-RSI.',
      ' headlines kept this week (RSS feeds: Google News and publishers). Headlines and links only: content belongs to the publishers. "tension": headline counted in the Pre-RSI geopolitics component.');
  }

  // ── 5. Données ────────────────────────────────────────────
  function drawDonnees() {
    C.table(clear($('t-catalogue')), [
      { label: t('Série', 'Series'), left: true, wrap: true, get: function (s) { return t(s.libelle_fr, s.libelle_en); } },
      { label: t('Unité', 'Unit'), left: true, get: function (s) { return s.unite; } },
      { label: 'Nature', left: true, get: function (s) { return s.nature; } },
      { label: t('Dernière valeur', 'Latest value'), get: function (s) { var v = s.derniere_valeur; return ok(v) ? RS.num(v, Math.round(v) === v || Math.abs(v) >= 1000 ? 0 : 3) : '—'; } },
      { label: t('Période', 'Period'), left: true, get: function (s) { return s.derniere_date || '—'; } },
      { label: t('Points', 'Points'), get: function (s) { return RS.int(s.n); } },
      { label: t('Licence', 'Licence'), left: true, wrap: true, get: function (s) { return s.licence; } },
      { label: t('Méthode', 'Method'), left: true, get: function (s) { return s.version; } }
    ], D.catalogue, D.catalogue.length + t(' séries publiées', ' published series'), true);

    var a = clear($('alertes')), list = (D.meta.alertes || []).filter(Boolean);
    if (!list.length) { RS.el('p', 'note', t('Aucune alerte : toutes les sources ont répondu.', 'No alert: every source responded.'), a); return; }
    RS.el('p', 'note', t('Ce qui manque ou date est signalé, jamais remplacé par une estimation silencieuse.', 'What is missing or dated is flagged, never replaced by a silent estimate.'), a);
    var ul = RS.el('ul', 'plain', null, a);
    list.forEach(function (x) { RS.el('li', null, x, ul); });
  }

  // ── Filtres et rendu ──────────────────────────────────────
  function syncFilters() {
    var b = document.querySelectorAll('[data-range],[data-cur]');
    for (var i = 0; i < b.length; i++) {
      var on = b[i].hasAttribute('data-range') ? +b[i].getAttribute('data-range') === state.range : b[i].getAttribute('data-cur') === state.cur;
      b[i].setAttribute('aria-pressed', on ? 'true' : 'false');
    }
  }
  function drawAll() {
    t = RS.t;
    syncFilters(); drawStatus(); drawKpis(); drawMatrice(); drawOrigines(); drawAfrique(); drawProduction(); drawPlanteur(); drawReferences(); drawBudget(); drawSignaux(); drawDonnees();
  }
  document.addEventListener('click', function (ev) {
    var b = ev.target.closest ? ev.target.closest('[data-range],[data-cur]') : null;
    if (!b) return;
    if (b.hasAttribute('data-range')) state.range = +b.getAttribute('data-range'); else state.cur = b.getAttribute('data-cur');
    drawAll();
  });
  RS.onLang(drawAll);
  drawAll();

  // Redessin des graphiques quand la largeur change
  var lastW = window.innerWidth, pending = false;
  window.addEventListener('resize', function () {
    if (pending || Math.abs(window.innerWidth - lastW) < 8) return;
    pending = true;
    window.requestAnimationFrame(function () { pending = false; lastW = window.innerWidth; drawAll(); });
  });
})();
