# Aperçu local du site vitrine et du terminal : http://127.0.0.1:4173
# Usage (depuis la racine du projet) : Rscript scripts/servir_site.R
# Serveur de fichiers statiques, en lecture seule, limité à cet ordinateur.
args <- commandArgs(trailingOnly = TRUE)
dossier <- normalizePath(if (length(args) >= 1) args[1] else "site", mustWork = TRUE)
port <- if (length(args) >= 2) as.integer(args[2]) else 4173L
cat("Site servi depuis", dossier, "sur http://127.0.0.1:", port, "\n", sep = "")
httpuv::runServer("127.0.0.1", port, list(
  call = function(req) list(status = 404L, headers = list("Content-Type" = "text/plain"), body = "Introuvable"),
  staticPaths = list("/" = httpuv::staticPath(dossier, indexhtml = TRUE))))
