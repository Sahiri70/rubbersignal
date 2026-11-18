# =============================================================
# RUBBERSIGNAL TECHNOLOGIES — Outil : fond de carte Afrique de l'Ouest et centrale
# Version : 1.0 (18/11/2026). Outil ponctuel (géométrie fixe, hors pipeline).
# Source  : Natural Earth, Admin 0 – Countries, 1:50m (domaine public),
#           https://www.naturalearthdata.com
# Sortie  : site/data/carte_afrique.js (window.RS_CARTE) : chemins SVG des pays,
#           projection équirectangulaire simple (x = longitude, y = −latitude),
#           acceptable sous les tropiques ; aucune ressource externe sur le site.
# Usage   : Rscript scripts/outils/carte_afrique.R chemin/ne_50m_admin_0_countries.geojson
# =============================================================

suppressPackageStartupMessages(library(sf))
sf_use_s2(FALSE)

BBOX <- c(xmin = -18, ymin = -6, xmax = 19, ymax = 16)   # Sénégal au Gabon / RD Congo (ouest)
ECHELLE <- 20                                            # unités SVG par degré
TOLERANCE <- 0.03                                        # simplification (degrés)

args <- commandArgs(trailingOnly = TRUE)
src <- if (length(args) >= 1) args[1] else stop("chemin du fichier Natural Earth attendu")
p <- st_read(src, quiet = TRUE)
p <- p[, c("ISO_A2_EH", "NAME_FR", "NAME_EN")]
names(p)[1:3] <- c("iso2", "nom_fr", "nom_en")
p <- suppressWarnings(st_crop(st_make_valid(p), st_bbox(BBOX, crs = st_crs(p))))
p <- p[!st_is_empty(p), ]
p <- st_simplify(p, dTolerance = TOLERANCE, preserveTopology = TRUE)

vers_svg <- function(g) {
  anneaux <- list()
  for (poly in st_cast(st_geometry(g), "POLYGON")) for (r in poly) anneaux[[length(anneaux) + 1]] <- r
  paste(vapply(anneaux, function(m) {
    if (nrow(m) < 4) return("")
    x <- round((m[, 1] - BBOX[["xmin"]]) * ECHELLE, 1); y <- round((BBOX[["ymax"]] - m[, 2]) * ECHELLE, 1)
    paste0("M", x[1], " ", y[1], paste0("L", x[-1], " ", y[-1], collapse = ""), "Z")
  }, character(1)), collapse = "")
}
pays <- lapply(seq_len(nrow(p)), function(i) list(iso2 = p$iso2[i], nom_fr = p$nom_fr[i], nom_en = p$nom_en[i], d = vers_svg(p[i, ])))
pays <- Filter(function(x) nzchar(x$d), pays)
carte <- list(source = "Natural Earth 1:50m (domaine public)", bbox = as.list(BBOX), echelle = ECHELLE,
              largeur = (BBOX[["xmax"]] - BBOX[["xmin"]]) * ECHELLE, hauteur = (BBOX[["ymax"]] - BBOX[["ymin"]]) * ECHELLE,
              pays = pays)
txt <- jsonlite::toJSON(carte, auto_unbox = TRUE, digits = NA)
con <- file("site/data/carte_afrique.js", open = "wb")
writeBin(charToRaw(enc2utf8(paste0("window.RS_CARTE = ", txt, ";\n"))), con); close(con)
cat("Pays :", length(pays), "| taille :", round(file.size("site/data/carte_afrique.js") / 1024), "Ko\n")
