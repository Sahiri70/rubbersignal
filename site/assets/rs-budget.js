/* RubberSignal : simulateur de budget d'achat (Simulateur de budget v1, 18/11/2026).
   Calcul pur, sans dépendance, exécuté dans le navigateur : aucune donnée saisie
   n'est envoyée ni conservée.
   Modèle : prix mensuel moyen du TSR20 en mouvement brownien géométrique SANS
   tendance (même hypothèse que la « fourchette de volatilité », méthode Volatilité v1) :
     P(m) = P(m-1) × exp(−σ²/2 × dt + σ × √dt × ε),  dt = 1/12,  ε ~ N(0,1)
   L'acheteur achète V tonnes chaque mois au prix du mois + prime (USD/kg).
   Coût total = Σ V × 1000 × (P(m) + prime). Graine fixe : mêmes entrées, mêmes résultats. */
(function () {
  'use strict';

  // Générateur pseudo-aléatoire reproductible (mulberry32) et loi normale (Box-Muller)
  function rng(seed) {
    var a = seed >>> 0;
    return function () {
      a = (a + 0x6D2B79F5) >>> 0;
      var t = a;
      t = Math.imul(t ^ (t >>> 15), t | 1);
      t ^= t + Math.imul(t ^ (t >>> 7), t | 61);
      return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
    };
  }
  function normal(u) {
    var spare = null;
    return function () {
      if (spare !== null) { var s = spare; spare = null; return s; }
      var u1 = 0; while (u1 === 0) u1 = u();
      var u2 = u(), r = Math.sqrt(-2 * Math.log(u1)), th = 2 * Math.PI * u2;
      spare = r * Math.sin(th);
      return r * Math.cos(th);
    };
  }
  function quantile(sorted, q) {
    if (!sorted.length) return NaN;
    var pos = (sorted.length - 1) * q, lo = Math.floor(pos), hi = Math.ceil(pos);
    return sorted[lo] + (sorted[hi] - sorted[lo]) * (pos - lo);
  }

  /* o : { prix0 (USD/kg), sigma (annuelle, fraction), mois (1..24), volume_t (par mois),
          prime (USD/kg), budget (USD, facultatif), n (trajectoires), seed }
     Retour : { mois: [{ m, p10, p50, p90 }], cout: { p10, p50, p90, moyenne },
                prix_moyen: { p10, p50, p90 }, prob_depassement, budget_p90, n } */
  function simuler(o) {
    var n = o.n || 10000, H = Math.max(1, Math.min(24, Math.round(o.mois))), dt = 1 / 12;
    var sig = o.sigma, drift = -0.5 * sig * sig * dt, vol = sig * Math.sqrt(dt);
    var kg = Math.max(0, o.volume_t) * 1000, prime = o.prime || 0;
    var z = normal(rng(o.seed === undefined ? 20261118 : o.seed));
    var parMois = []; for (var h = 0; h < H; h++) parMois.push(new Float64Array(n));
    var couts = new Float64Array(n);
    for (var i = 0; i < n; i++) {
      var p = o.prix0, c = 0;
      for (var m = 0; m < H; m++) {
        p = p * Math.exp(drift + vol * z());
        parMois[m][i] = p;
        c += kg * Math.max(0, p + prime);
      }
      couts[i] = c;
    }
    var mois = parMois.map(function (arr, m) {
      var s = Array.prototype.slice.call(arr).sort(function (a, b) { return a - b; });
      return { m: m + 1, p10: quantile(s, 0.10), p50: quantile(s, 0.50), p90: quantile(s, 0.90) };
    });
    var sc = Array.prototype.slice.call(couts).sort(function (a, b) { return a - b; });
    var moy = sc.reduce(function (a, b) { return a + b; }, 0) / n;
    var totKg = kg * H;
    var out = {
      n: n, mois: mois,
      cout: { p10: quantile(sc, 0.10), p50: quantile(sc, 0.50), p90: quantile(sc, 0.90), moyenne: moy },
      prix_moyen: totKg > 0 ? { p10: quantile(sc, 0.10) / totKg, p50: quantile(sc, 0.50) / totKg, p90: quantile(sc, 0.90) / totKg } : null,
      budget_p90: quantile(sc, 0.90), prob_depassement: null
    };
    if (o.budget > 0) {
      var k = 0; for (var j = 0; j < n; j++) if (couts[j] > o.budget) k++;
      out.prob_depassement = k / n;
    }
    return out;
  }

  window.RSBudget = { simuler: simuler, quantile: quantile, rng: rng };
})();
