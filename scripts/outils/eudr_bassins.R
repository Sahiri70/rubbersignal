# =============================================================
# RUBBERSIGNAL TECHNOLOGIES — Outil : indicateur EUDR des bassins de production
# Version : 1.1 (26/11/2026, ajout de la carte de l'hévéa ; 1.0 du 18/11/2026).
# Outil ponctuel (à relancer à chaque nouvelle version des cartes sources,
# une fois par an environ), exécuté EN LOCAL seulement, jamais dans le pipeline.
# Question : autour de chaque bassin (emprise de la vue satellite), quelle
# surface était de la FORÊT au 31/12/2020 (date de référence EUDR) et a perdu
# son couvert arboré APRÈS cette date ?
# Sources (lecture partielle des fichiers, rien n'est téléchargé en entier) :
#   - Forêt 2020 : JRC Global Forest Cover 2020, version 4 (10 m), carte de
#     référence de la Commission pour l'EUDR, CC BY 4.0. Citation : Bourgoin
#     et al. (2026), « Global map of forest cover 2020 - version 4 », European
#     Commission, Joint Research Centre, doi:10.2905/JRC.3KATEH8.
#   - Pertes de couvert arboré : Hansen et al., Global Forest Change 2024
#     v1.12 (University of Maryland, 30 m), couche lossyear (1 à 24 = 2001 à
#     2024), CC BY 4.0. Citation : Hansen et al. (2013) Science 342:850-853.
#   - Hévéa : Forest Data Partnership, modèle de probabilité « rubber » 2026a
#     (10 m, années 2020 et 2024), CC BY 4.0, mention obligatoire « Produced by
#     Google for the Forest Data Partnership ». Citation : Forest Data
#     Partnership (2026), Community models 2026a. Stockage Google « requester
#     pays » : le transfert est facturé au projet Google Cloud de RubberSignal
#     (variable RS_GCP_PROJET), accès par le compte connecté avec
#     « gcloud auth login ». Aucun fichier de clé.
# Calcul (grille JRC, 10 m) :
#   forêt 2020 = JRC = 1 ; perte après 2020 = Hansen lossyear >= 21 ;
#   indicateur EUDR = forêt 2020 ET perte après 2020 (surface en ha) ;
#   perte après 2020 hors forêt 2020 = plantations, cultures, jachères (rotation) ;
#   hévéa = probabilité FDP >= 0,5 (seuil choisi par RubberSignal : le catalogue
#   n'en recommande aucun) ; nouvel hévéa sur forêt 2020 = forêt 2020 ET hévéa
#   en 2024 ET pas hévéa en 2020 (plantation probable sur forêt après la date EUDR).
# Limites : une perte de couvert n'est pas une preuve de déforestation (coupe
# sanitaire, chablis, erreur de classification) ; résolutions différentes
# (10 m / 30 m) ; l'emprise est un carré autour du bassin, pas une exploitation.
# Sorties : site/assets/sat/<id>_eudr.jpg (vue 2016 + couches),
#           site/data/eudr_bassins.js (window.RS_EUDR), data/processed/eudr_bassins.csv
# Usage   : Rscript scripts/outils/eudr_bassins.R
# =============================================================

suppressPackageStartupMessages({ library(terra); library(jpeg) })
setGDALconfig("GDAL_DISABLE_READDIR_ON_OPEN", "EMPTY_DIR")
setGDALconfig("GDAL_HTTP_USERAGENT", "RubberSignalBot/1.0 (+github.com/Sahiri70/rubbersignal)")
setGDALconfig("GDAL_HTTP_MAX_RETRY", "3")

JRC_URL <- "/vsicurl/https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/FOREST/GFC2020/LATEST/single-cog/JRC_GFC2020_V4_COG.tif"
HAN_URL <- "/vsicurl/https://storage.googleapis.com/earthenginepartners-hansen/GFC-2024-v1.12/Hansen_GFC-2024-v1.12_lossyear_%s_%s.tif"
FDP_URL   <- "/vsicurl/https://storage.googleapis.com/earth-engine-public-requester-pays/forestdatapartnership/2026a/rubber/%d/lng_%d_lat_%d.tif"
FDP_SEUIL <- 0.5
EUDR_DATE <- "2020-12-31"
VERSION   <- "EUDR bassins v1.1"

# Accès à la carte hévéa (stockage « requester pays »). Le jeton OAuth du compte
# connecté avec gcloud reste en mémoire le temps de la lecture : jamais affiché,
# jamais écrit. L'en-tête est posé APRÈS la lecture des cartes JRC et Hansen et
# retiré aussitôt, pour qu'il ne parte que vers storage.googleapis.com.
fdp_lire <- function(e, annee, projet = Sys.getenv("RS_GCP_PROJET")) {
  if (!nzchar(projet)) stop("RS_GCP_PROJET non défini : identifiant du projet Google Cloud qui paie le transfert")
  gcloud <- Sys.which("gcloud")
  if (!nzchar(gcloud)) gcloud <- file.path(Sys.getenv("LOCALAPPDATA"), "Google", "Cloud SDK", "google-cloud-sdk", "bin", "gcloud.cmd")
  j <- suppressWarnings(system2(gcloud, c("auth", "print-access-token"), stdout = TRUE, stderr = FALSE))
  j <- utils::tail(j[nzchar(j)], 1)
  if (length(j) != 1 || nchar(j) < 20) stop("gcloud : pas de jeton d'accès (lancer « gcloud auth login »)")
  setGDALconfig("GDAL_HTTP_HEADERS", paste0("Authorization: Bearer ", j, "\r\nx-goog-user-project: ", projet))
  rm(j)
  on.exit(setGDALconfig("GDAL_HTTP_HEADERS", ""), add = TRUE)
  # Tuiles de 1° nommées par le coin sud-ouest
  lo <- seq(floor(xmin(e)), floor(xmax(e) - 1e-9)); la <- seq(floor(ymin(e)), floor(ymax(e) - 1e-9))
  g <- expand.grid(lo = lo, la = la)
  u <- sprintf(FDP_URL, annee, g$lo, g$la)
  for (essai in 1:3) {                       # une lecture réseau peut échouer : jusqu'à 3 essais
    r <- tryCatch(toMemory(if (length(u) == 1) crop(rast(u), e) else crop(vrt(u), e)), error = function(err) err)
    if (!inherits(r, "error")) return(r)
    message("FDP ", annee, " : lecture interrompue (essai ", essai, "/3), nouvelle tentative")
    Sys.sleep(5 * essai)
  }
  stop(r)
}

# Emprise d'une vue (même calcul que vignettes_satellite.R : tuiles Web Mercator)
emprise_vue <- function(lat, lon, z, nx, ny) {
  n <- 2^z; lr <- lat * pi / 180
  tx <- (lon + 180) / 360 * n; ty <- (1 - log(tan(lr) + 1 / cos(lr)) / pi) / 2 * n
  x0 <- floor(tx - nx / 2); y0 <- floor(ty - ny / 2)
  lon_de <- function(x) x / n * 360 - 180
  lat_de <- function(y) atan(sinh(pi * (1 - 2 * y / n))) * 180 / pi
  R <- 6378137; mx <- function(x) (x / n * 2 - 1) * pi * R; my <- function(y) (1 - y / n * 2) * pi * R
  list(geo = ext(lon_de(x0), lon_de(x0 + nx), lat_de(y0 + ny), lat_de(y0)),
       merc = ext(mx(x0), mx(x0 + nx), my(y0 + ny), my(y0)), largeur = 256 * nx, hauteur = 256 * ny)
}

# Tuiles Hansen (10° x 10°, nommées par le coin nord-ouest) couvrant une emprise
tuiles_hansen <- function(e) {
  la <- seq(ceiling(ymax(e) / 10) * 10, floor(ymin(e) / 10) * 10 + 10, by = -10)
  lo <- seq(floor(xmin(e) / 10) * 10, floor((xmax(e) - 1e-9) / 10) * 10, by = 10)
  g <- expand.grid(la = la, lo = lo)
  fmt_la <- function(v) sprintf("%02d%s", abs(v), ifelse(v >= 0, "N", "S"))
  fmt_lo <- function(v) sprintf("%03d%s", abs(v), ifelse(v >= 0, "E", "W"))
  sprintf(HAN_URL, fmt_la(g$la), fmt_lo(g$lo))
}

analyser <- function(z, sortie_img = "site/assets/sat") {
  em <- emprise_vue(z$lat, z$lon, z$zoom, z$nx, z$ny)
  jrc <- toMemory(crop(rast(JRC_URL), em$geo))
  han <- tuiles_hansen(em$geo)
  hl  <- if (length(han) == 1) crop(rast(han), em$geo) else crop(vrt(han), em$geo)
  hl  <- toMemory(resample(hl, jrc, method = "near"))            # grille JRC (10 m)
  # Hévéa 2020 et 2024 (FDP), seuillé puis ramené sur la grille JRC
  hev <- lapply(c(2020, 2024), function(a) resample(fdp_lire(em$geo, a) >= FDP_SEUIL, jrc, method = "near"))
  ha  <- cellSize(jrc, unit = "ha")
  foret <- jrc == 1
  apres <- hl >= 21 & hl <= 24
  hev20 <- !is.na(hev[[1]]) & hev[[1]] == 1
  hev24 <- !is.na(hev[[2]]) & hev[[2]] == 1
  s <- function(m) global(ha * m, "sum", na.rm = TRUE)[[1]]
  par_an <- vapply(1:24, function(k) s(hl == k), numeric(1))
  par_an_foret <- vapply(21:24, function(k) s(foret & hl == k), numeric(1))
  res <- list(id = z$id, surface_ha = round(s(!is.na(jrc))), foret_2020_ha = round(s(foret)),
              perte_apres_2020_foret_ha = round(s(foret & apres), 1),
              perte_apres_2020_hors_foret_ha = round(s(!foret & apres), 1),
              perte_2001_2020_ha = round(sum(par_an[1:20])),
              perte_par_an = setNames(as.list(round(par_an, 1)), 2001:2024),
              perte_foret_par_an = setNames(as.list(round(par_an_foret, 1)), 2021:2024))
  res$part_foret_pct <- round(100 * res$foret_2020_ha / res$surface_ha, 1)
  res$part_perte_foret_pct <- if (res$foret_2020_ha > 0) round(100 * res$perte_apres_2020_foret_ha / res$foret_2020_ha, 2) else NA
  res$hevea_seuil <- FDP_SEUIL
  res$hevea_2020_ha <- round(s(hev20))
  res$hevea_2024_ha <- round(s(hev24))
  res$part_hevea_2024_pct <- round(100 * res$hevea_2024_ha / res$surface_ha, 1)
  res$hevea_nouveau_ha <- round(s(hev24 & !hev20), 1)
  res$hevea_nouveau_sur_foret_2020_ha <- round(s(foret & hev24 & !hev20), 1)

  # Image : vue 2016 + forêt 2020 (vert) + hévéa 2024 (bleu) + perte après 2020 en forêt (rouge) / hors forêt (jaune)
  modele <- rast(em$merc, nrows = em$hauteur, ncols = em$largeur, crs = "EPSG:3857")
  code <- ifel(foret & apres, 3, ifel(apres, 2, ifel(hev24, 4, ifel(foret, 1, 0))))
  cm <- as.matrix(project(code, modele, method = "near"), wide = TRUE)
  base <- readJPEG(file.path(sortie_img, paste0(z$id, ".jpg")))
  peindre <- function(img, masque, rgb, a) { for (k in 1:3) img[, , k][masque] <- (1 - a) * img[, , k][masque] + a * rgb[k]; img }
  m <- function(v) !is.na(cm) & cm == v
  img <- peindre(base, m(1), c(27, 175, 122) / 255, 0.38)
  img <- peindre(img, m(4), c(47, 111, 208) / 255, 0.60)
  img <- peindre(img, m(2), c(237, 161, 0) / 255, 0.85)
  img <- peindre(img, m(3), c(227, 73, 72) / 255, 0.95)
  writeJPEG(img, file.path(sortie_img, paste0(z$id, "_eudr.jpg")), quality = 0.85)
  res
}

if (sys.nframe() == 0) {
  zones <- utils::read.csv("data/reference/zones_satellite.csv", stringsAsFactors = FALSE, encoding = "UTF-8")
  out <- lapply(seq_len(nrow(zones)), function(k) {
    z <- zones[k, ]; t0 <- Sys.time()
    for (essai in 1:3) {                     # lectures réseau parfois interrompues : jusqu'à 3 essais par bassin
      r <- tryCatch(analyser(z), error = function(err) err)
      if (!inherits(r, "error")) break
      message(z$id, " : ", conditionMessage(r), " (essai ", essai, "/3)")
      if (essai == 3) stop(r)
      Sys.sleep(10 * essai)
    }
    cat(sprintf("%-8s surface %6d ha | forêt 2020 %6d ha (%4.1f %%) | perte après 2020 en forêt %7.1f ha (%.2f %%) | hors forêt %7.1f ha | hévéa 2020 %6d / 2024 %6d ha | nouvel hévéa sur forêt 2020 %6.1f ha | %ds\n",
                r$id, r$surface_ha, r$foret_2020_ha, r$part_foret_pct, r$perte_apres_2020_foret_ha, r$part_perte_foret_pct,
                r$perte_apres_2020_hors_foret_ha, r$hevea_2020_ha, r$hevea_2024_ha, r$hevea_nouveau_sur_foret_2020_ha,
                round(as.numeric(difftime(Sys.time(), t0, units = "secs")))))
    r
  })
  meta <- list(version = VERSION, date_calcul = as.character(Sys.Date()), date_reference_eudr = EUDR_DATE,
               sources = list(
                 foret = "JRC Global Forest Cover 2020, version 4 (10 m), European Commission, Joint Research Centre, doi:10.2905/JRC.3KATEH8, CC BY 4.0",
                 pertes = "Hansen et al. (2013), Global Forest Change 2024 v1.12, University of Maryland, CC BY 4.0",
                 hevea = "Forest Data Partnership (2026), Community models 2026a, rubber probability (10 m), Produced by Google for the Forest Data Partnership, CC BY 4.0",
                 image = "EOxCloudless 2016, EOX IT Services GmbH (Contains modified Copernicus Sentinel data 2016), CC BY 4.0"),
               bassins = out)
  txt <- jsonlite::toJSON(meta, auto_unbox = TRUE, digits = NA, pretty = TRUE)
  con <- file("site/data/eudr_bassins.js", open = "wb"); writeBin(charToRaw(enc2utf8(paste0("window.RS_EUDR = ", txt, ";\n"))), con); close(con)
  tab <- do.call(rbind, lapply(out, function(r) data.frame(id = r$id, surface_ha = r$surface_ha, foret_2020_ha = r$foret_2020_ha,
    perte_apres_2020_foret_ha = r$perte_apres_2020_foret_ha, perte_apres_2020_hors_foret_ha = r$perte_apres_2020_hors_foret_ha,
    perte_2001_2020_ha = r$perte_2001_2020_ha, hevea_2020_ha = r$hevea_2020_ha, hevea_2024_ha = r$hevea_2024_ha,
    hevea_nouveau_sur_foret_2020_ha = r$hevea_nouveau_sur_foret_2020_ha, version = VERSION, date_calcul = as.character(Sys.Date()))))
  dir.create("data/processed", showWarnings = FALSE)
  con <- file("data/processed/eudr_bassins.csv", open = "wb"); utils::write.csv(tab, con, row.names = FALSE, fileEncoding = "UTF-8"); close(con)
  cat("Écrit : site/data/eudr_bassins.js, data/processed/eudr_bassins.csv, site/assets/sat/*_eudr.jpg\n")
}
