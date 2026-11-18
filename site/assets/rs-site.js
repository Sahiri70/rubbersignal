/* RubberSignal : page d'accueil. Les chiffres viennent des mêmes données publiques que le terminal. */
(function () {
  'use strict';
  var RS = window.RS, C = window.RSCharts, D = window.RS_DATA;
  var $ = function (id) { return document.getElementById(id); };
  if (!D || !D.rapi || !D.rapi.mois || !D.rapi.mois.length) return;

  function lastWith(arr, key) { for (var i = arr.length - 1; i >= 0; i--) if (RS.ok(arr[i][key])) return i; return -1; }
  function clear(n) { n.textContent = ''; return n; }
  function row(dl, label, value) { RS.el('dt', null, label, dl); RS.el('dd', null, value, dl); }
  function fact(host, label, value, meta) {
    var k = RS.el('div', 'kpi', null, host);
    RS.el('div', 'label', label, k);
    RS.el('div', 'value', value, k);
    RS.el('div', 'meta', meta, k);
  }

  function draw() {
    var t = RS.t, mois = D.rapi.mois, i = lastWith(mois, 'decote');
    if (i < 0) return;
    var m = mois[i], cheaper = m.decote < 0;

    // En-tête : le chiffre du mois
    $('h-big').textContent = RS.pct(m.decote, 1);
    $('h-cap').textContent = t('Écart de prix du caoutchouc ivoirien par rapport au caoutchouc asiatique, à l’entrée dans l’Union européenne, en ',
      'Price gap of Ivorian rubber against Asian rubber on entry into the European Union, in ') + RS.month(m.p, true) + '.';
    var dl = clear($('h-dl'));
    row(dl, t('Côte d’Ivoire, rendu UE', 'Côte d’Ivoire, delivered EU'), RS.num(m.ci_usd, 3) + ' USD/kg');
    row(dl, t('Asie, rendu UE', 'Asia, delivered EU'), RS.num(m.asie_usd, 3) + ' USD/kg');
    row(dl, t('Volume importé depuis la Côte d’Ivoire', 'Volume imported from Côte d’Ivoire'), RS.int(m.vol_t) + ' t');
    $('h-src').textContent = t('Douanes de l’UE (Eurostat Comext, NC 4001 22 00), mise à jour du ', 'EU customs (Eurostat Comext, CN 4001 22 00), updated ') +
      RS.date(D.rapi.maj_eurostat) + t('. Prix : transactions déclarées. Écart : calcul RubberSignal Technologies.', '. Prices: declared transactions. Gap: RubberSignal Technologies calculation.');

    // Cartes des questions
    $('q1-val').textContent = RS.num(m.ci_usd, 3) + ' USD/kg';
    $('q1-sub').textContent = t('en ', 'in ') + RS.month(m.p, true);
    var h = D.rsci.historique, r = lastWith(h, 'rsci_pct');
    if (r >= 0) {
      $('q3-val').textContent = RS.pct(h[r].rsci_pct, 1);
      $('q3-sub').textContent = t('du prix TSR20 pour le planteur (prix de ', 'of the TSR20 price for the farmer (price of ') + RS.month(h[r].mois, true) + ')';
    }

    // Faits tirés des douanes (6 derniers mois)
    var six = mois.slice(Math.max(0, i - 5), i + 1).filter(function (x) { return RS.ok(x.decote); });
    var moy = six.reduce(function (s, x) { return s + x.decote; }, 0) / six.length;
    $('c-titre').textContent = cheaper
      ? t('Le caoutchouc ivoirien entre dans l’UE moins cher que l’asiatique', 'Ivorian rubber enters the EU cheaper than Asian rubber')
      : t('Le caoutchouc ivoirien entre dans l’UE plus cher que l’asiatique', 'Ivorian rubber enters the EU dearer than Asian rubber');
    $('c-lead').textContent = t('Sur les six derniers mois publiés, l’écart moyen est de ', 'Over the last six published months, the average gap is ') + RS.pct(moy, 1) +
      t('. Il se lit sur une tendance : la valeur en douane reflète des contrats signés plusieurs semaines plus tôt.', '. Read it as a trend: customs values reflect contracts signed several weeks earlier.');
    var faits = clear($('faits')), o = D.rapi.origines;
    if (o && o.pays) {
      var tri = o.pays.filter(function (p) { return RS.ok(p.vol_t); }).sort(function (a, b) { return b.vol_t - a.vol_t; });
      var ci = tri.filter(function (p) { return p.code === 'CI'; })[0];
      var rang = tri.indexOf(ci) + 1, second = tri[rang === 1 ? 1 : 0];
      if (ci) {
        fact(faits, t('Importations de l’UE depuis la Côte d’Ivoire, 6 mois', 'EU imports from Côte d’Ivoire, 6 months'), RS.int(ci.vol_t) + ' t',
          rang === 1 && second ? RS.num(ci.vol_t / second.vol_t, 1) + t(' fois ', ' times ') + t(second.nom_fr, second.nom_en) + t(', deuxième origine', ', the second origin')
            : t('rang ', 'rank ') + rang + t(' parmi les origines suivies', ' among the origins tracked'));
        fact(faits, t('Prix moyen rendu UE, Côte d’Ivoire, 6 mois', 'Average EU-delivered price, Côte d’Ivoire, 6 months'), RS.num(ci.prix_eur_kg, 3) + ' €/kg',
          second ? t(second.nom_fr, second.nom_en) + ' : ' + RS.num(second.prix_eur_kg, 3) + ' €/kg' : '');
      }
    }
    $('c-note').textContent = t('Période : ', 'Period: ') + (o ? RS.month(o.debut, true) + t(' à ', ' to ') + RS.month(o.fin, true) : '') +
      t('. Limites : prix rendu frontière UE, pas un prix FOB ; plusieurs grades regroupés (TSR10, TSR20).', '. Limits: delivered EU-border price, not FOB; several grades pooled (TSR10, TSR20).');

    // Base de données : compteurs tirés du catalogue publié (rien d'écrit à la main)
    var bk = $('base-kpis');
    if (bk && D.catalogue && D.catalogue.length) {
      clear(bk);
      var nObs = D.catalogue.reduce(function (s, x) { return s + (x.n || 0); }, 0);
      fact(bk, t('Séries publiées, toutes sous licence libre', 'Published series, all openly licensed'), RS.int(D.catalogue.length),
        t('chacune avec sa source, sa nature et sa méthode', 'each with its source, nature and method'));
      fact(bk, t('Valeurs datées et sourcées', 'Dated, sourced values'), RS.int(nObs),
        t('les révisions s’ajoutent, rien n’est effacé', 'revisions are added, nothing is deleted'));
      var nAs = o && o.pays ? o.pays.filter(function (p) { return p.code !== 'CI'; }).length : 0;
      var nAf = D.afrique && D.afrique.origines ? D.afrique.origines.length : 1;
      fact(bk, t('Pays d’origine suivis', 'Origin countries tracked'), RS.int(nAf + nAs),
        nAf > 1 ? RS.int(nAf) + t(' origines africaines et ', ' African origins and ') + RS.int(nAs) + t(' asiatiques pour la comparaison', ' Asian ones for comparison')
          : t('Côte d’Ivoire et ', 'Côte d’Ivoire and ') + RS.int(nAs) + t(' origines asiatiques', ' Asian origins'));
    }

    // Graphique : 12 derniers mois
    var d12 = mois.slice(-12);
    C.line($('c-prix'), {
      x: d12.map(function (x) { return x.p; }), xFmt: RS.month, height: 280,
      series: [
        { name: t('Côte d’Ivoire', 'Côte d’Ivoire'), color: 'var(--s1)', values: d12.map(function (x) { return x.ci_usd; }) },
        { name: t('Asie (4 origines)', 'Asia (4 origins)'), color: 'var(--s2)', values: d12.map(function (x) { return x.asie_usd; }) }
      ],
      yFmt: function (v) { return RS.num(v, 2); }, tipFmt: function (v) { return RS.num(v, 3); },
      label: t('Prix rendu UE, Côte d’Ivoire et Asie', 'EU-delivered price, Côte d’Ivoire and Asia')
    });
    C.table(clear($('c-table')), [
      { label: t('Mois', 'Month'), left: true, get: function (x) { return RS.month(x.p, true); } },
      { label: t('Côte d’Ivoire', 'Côte d’Ivoire'), get: function (x) { return RS.num(x.ci_usd, 3); } },
      { label: t('Asie', 'Asia'), get: function (x) { return RS.num(x.asie_usd, 3); } },
      { label: t('Écart', 'Gap'), get: function (x) { return RS.pct(x.decote, 1); } }
    ], d12.slice().reverse());
  }

  RS.onLang(draw);
  draw();
  var lastW = window.innerWidth, pending = false;
  window.addEventListener('resize', function () {
    if (pending || Math.abs(window.innerWidth - lastW) < 8) return;
    pending = true;
    window.requestAnimationFrame(function () { pending = false; lastW = window.innerWidth; draw(); });
  });
})();
