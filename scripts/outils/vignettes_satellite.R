# =============================================================
# RUBBERSIGNAL TECHNOLOGIES — Outil : vues satellite des bassins d'hévéa
# Version : 1.0 (18/11/2026). Outil ponctuel (hors pipeline hebdomadaire).
# Source  : EOxCloudless 2016 (mosaïque Sentinel-2 sans nuages, 10 m),
#           EOX IT Services GmbH, licence CC BY 4.0 — couche
#           « s2cloudless_3857 » (2016). La couche 2017 est vide sur l'Afrique ;
#           les années 2018 et suivantes sont sous
#           licence non commerciale : NE PAS les utiliser.
# Attribution obligatoire (affichée sous chaque image) :
#   « EOxCloudless 2016 https://cloudless.eox.at by EOX IT Services GmbH
#     (Contains modified Copernicus Sentinel data 2016) »
# Pourquoi des images fixes : la politique de sécurité du site n'autorise
# aucune ressource externe ; le visiteur ne contacte aucun serveur tiers.
# Usage : Rscript scripts/outils/vignettes_satellite.R [dossier_sortie]
# =============================================================

suppressPackageStartupMessages({ library(httr); library(jpeg) })

EOX_COUCHE  <- "s2cloudless_3857"
EOX_URL     <- "https://tiles.maps.eox.at/wmts/1.0.0/%s/default/g/%d/%d/%d.jpg"
EOX_CREDIT  <- "EOxCloudless 2016 https://cloudless.eox.at by EOX IT Services GmbH (Contains modified Copernicus Sentinel data 2016)"

# Tuile Web Mercator (x = colonne, y = ligne) contenant un point
tuile_xy <- function(lat, lon, z) {
  n <- 2^z; lr <- lat * pi / 180
  c(x = (lon + 180) / 360 * n, y = (1 - log(tan(lr) + 1 / cos(lr)) / pi) / 2 * n)
}
# Mètres par pixel à une latitude donnée
m_par_px <- function(lat, z) 156543.03392 * cos(lat * pi / 180) / 2^z

# Image de nx × ny tuiles centrée sur (lat, lon) ; écrit un JPEG ; renvoie l'emprise
vignette <- function(lat, lon, z, nx, ny, fichier, pause = 0.3) {
  t <- tuile_xy(lat, lon, z)
  x0 <- floor(t[["x"]] - nx / 2); y0 <- floor(t[["y"]] - ny / 2)
  img <- array(0, dim = c(256 * ny, 256 * nx, 3))
  for (j in 0:(ny - 1)) for (i in 0:(nx - 1)) {
    u <- sprintf(EOX_URL, EOX_COUCHE, z, y0 + j, x0 + i)
    r <- RETRY("GET", u, user_agent("RubberSignalBot/1.0 (+github.com/Sahiri70/rubbersignal)"), timeout(30), times = 3, quiet = TRUE)
    stop_for_status(r)
    raw <- content(r, "raw")
    tl <- if (length(raw) > 4 && raw[1] == as.raw(0x89)) png::readPNG(raw) else readJPEG(raw)   # certaines tuiles sont en PNG
    if (length(dim(tl)) == 2) tl <- array(rep(tl, 3), dim = c(dim(tl), 3))
    img[j * 256 + 1:256, i * 256 + 1:256, ] <- tl[, , 1:3]
    Sys.sleep(pause)
  }
  writeJPEG(img, fichier, quality = 0.82)
  n <- 2^z
  lon_de <- function(x) x / n * 360 - 180
  lat_de <- function(y) atan(sinh(pi * (1 - 2 * y / n))) * 180 / pi
  list(fichier = fichier, z = z, largeur_km = round(256 * nx * m_par_px(lat, z) / 1000, 1),
       ouest = lon_de(x0), est = lon_de(x0 + nx), nord = lat_de(y0), sud = lat_de(y0 + ny))
}

if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  out <- if (length(args) >= 1) args[1] else "site/assets/sat"
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  zones <- utils::read.csv("data/reference/zones_satellite.csv", stringsAsFactors = FALSE, encoding = "UTF-8")
  for (k in seq_len(nrow(zones))) {
    z <- zones[k, ]
    v <- vignette(z$lat, z$lon, z$zoom, z$nx, z$ny, file.path(out, paste0(z$id, ".jpg")))
    cat(sprintf("%-14s %6.3f %7.3f z%d -> %s (%.1f km de large)\n", z$id, z$lat, z$lon, z$zoom, v$fichier, v$largeur_km))
  }
}
