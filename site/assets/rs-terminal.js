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
    var mh = clear($('meteo')), m = D.meteo;
    if (!m || !m.zones) { RS.el('p', 'note', t('Non disponible.', 'Not available.'), mh); return; }
    C.table(mh, [
      { label: t('Zone', 'Area'), left: true, get: function (z) { return z.ville + ', ' + z.pays; } },
      { label: t('Pluie, 30 jours (mm)', 'Rain, 30 days (mm)'), get: function (z) { return RS.num(z.pluie_30j_mm, 0); } },
      { label: t('Température (°C)', 'Temperature (°C)'), get: function (z) { return RS.num(z.temp_c, 1); } },
      { label: t('Score de production', 'Production score'), get: function (z) { return ok(z.score_production) ? RS.int(z.score_production) : '—'; } }
    ], m.zones, t('Relevé du ', 'Reading of ') + RS.date(m.date) + ' · ' + m.source, true);
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
    syncFilters(); drawStatus(); drawKpis(); drawMatrice(); drawOrigines(); drawAfrique(); drawPlanteur(); drawReferences(); drawSignaux(); drawDonnees();
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
