/* RubberSignal : langue, thème et formats communs. Chargé dans <head> (court, sans dépendance). */
(function () {
  'use strict';
  var root = document.documentElement;

  function store(k, v) {
    try {
      if (v === undefined) return window.localStorage.getItem(k);
      window.localStorage.setItem(k, v);
    } catch (e) { /* stockage indisponible : on continue sans mémoire */ }
    return null;
  }

  var params = new URLSearchParams(window.location.search);
  var lang = params.get('lang') || store('rs-lang') ||
    (String(navigator.language || 'fr').slice(0, 2).toLowerCase() === 'fr' ? 'fr' : 'en');
  if (lang !== 'fr' && lang !== 'en') lang = 'fr';
  root.setAttribute('data-lang', lang);
  root.setAttribute('lang', lang);

  var theme = store('rs-theme');
  if (theme === 'light' || theme === 'dark') root.setAttribute('data-theme', theme);

  var listeners = [];
  var nfCache = {};
  function nf(d) {
    var k = lang + d;
    if (!nfCache[k]) {
      nfCache[k] = new Intl.NumberFormat(lang === 'fr' ? 'fr-FR' : 'en-GB',
        { minimumFractionDigits: d, maximumFractionDigits: d });
    }
    return nfCache[k];
  }
  function ok(v) { return v !== null && v !== undefined && isFinite(v); }

  var RS = {
    get lang() { return lang; },
    t: function (fr, en) { return lang === 'fr' ? fr : en; },
    onLang: function (cb) { listeners.push(cb); },
    setLang: function (l) {
      if (l !== 'fr' && l !== 'en') return;
      lang = l;
      root.setAttribute('data-lang', l);
      root.setAttribute('lang', l);
      store('rs-lang', l);
      syncButtons();
      listeners.forEach(function (cb) { cb(l); });
    },
    toggleTheme: function () {
      var dark = root.getAttribute('data-theme') === 'dark' ||
        (!root.getAttribute('data-theme') && window.matchMedia('(prefers-color-scheme: dark)').matches);
      var next = dark ? 'light' : 'dark';
      root.setAttribute('data-theme', next);
      store('rs-theme', next);
    },
    num: function (v, d) { return ok(v) ? nf(d === undefined ? 2 : d).format(v) : '—'; },
    int: function (v) { return ok(v) ? nf(0).format(Math.round(v)) : '—'; },
    pct: function (v, d, signed) {
      if (!ok(v)) return '—';
      var s = nf(d === undefined ? 1 : d).format(Math.abs(v));
      var sign = v < 0 ? '−' : (signed && v > 0 ? '+' : '');
      return sign + s + (lang === 'fr' ? ' %' : '%');
    },
    // '2026-09' -> « sept. 2026 » ; long = « septembre 2026 »
    month: function (p, long) {
      if (!/^\d{4}-\d{2}/.test(String(p))) return String(p || '—');
      var d = new Date(Date.UTC(+p.slice(0, 4), +p.slice(5, 7) - 1, 1));
      return new Intl.DateTimeFormat(lang === 'fr' ? 'fr-FR' : 'en-GB',
        { month: long ? 'long' : 'short', year: long ? 'numeric' : '2-digit', timeZone: 'UTC' }).format(d);
    },
    date: function (iso) {
      if (!/^\d{4}-\d{2}-\d{2}/.test(String(iso))) return '—';
      var d = new Date(Date.UTC(+iso.slice(0, 4), +iso.slice(5, 7) - 1, +iso.slice(8, 10)));
      return new Intl.DateTimeFormat(lang === 'fr' ? 'fr-FR' : 'en-GB',
        { day: 'numeric', month: 'long', year: 'numeric', timeZone: 'UTC' }).format(d);
    },
    ok: ok,
    el: function (tag, cls, text, parent) {
      var e = document.createElement(tag);
      if (cls) e.className = cls;
      if (text !== null && text !== undefined) e.textContent = text;
      if (parent) parent.appendChild(e);
      return e;
    }
  };

  function syncButtons() {
    var b = document.querySelectorAll('[data-set-lang]');
    for (var i = 0; i < b.length; i++) {
      b[i].setAttribute('aria-pressed', b[i].getAttribute('data-set-lang') === lang ? 'true' : 'false');
    }
  }

  document.addEventListener('DOMContentLoaded', function () {
    syncButtons();
    document.addEventListener('click', function (ev) {
      var t = ev.target.closest ? ev.target.closest('[data-set-lang],[data-toggle-theme]') : null;
      if (!t) return;
      if (t.hasAttribute('data-set-lang')) RS.setLang(t.getAttribute('data-set-lang'));
      else RS.toggleTheme();
    });
  });

  window.RS = RS;
})();
