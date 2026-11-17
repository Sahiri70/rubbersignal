# Lance tous les tests hors ligne (aucune clé, aucun réseau) et sort en code 1
# au premier échec. Usage, depuis la racine du projet : Rscript tests/run_all.R
# test_sources.R est un script simple (pas testthat) : il est lancé à part.

suppressPackageStartupMessages(library(testthat))
fichiers <- setdiff(list.files("tests", "^test_.*\\.R$", full.names = TRUE), "tests/test_sources.R")
total <- c(ok = 0, echecs = 0, erreurs = 0)
for (f in fichiers) {
  r <- as.data.frame(test_file(f, reporter = "silent", stop_on_failure = FALSE))
  n <- c(ok = sum(r$passed), echecs = sum(r$failed), erreurs = sum(r$error))
  cat(sprintf("%-32s OK %4d | echecs %d | erreurs %d\n", basename(f), n[["ok"]], n[["echecs"]], n[["erreurs"]]))
  if (n[["echecs"]] + n[["erreurs"]] > 0) print(r[r$failed > 0 | r$error, c("test", "failed", "error")])
  total <- total + n
}
src <- system2(file.path(R.home("bin"), "Rscript"), "tests/test_sources.R", stdout = TRUE, stderr = TRUE)
code_src <- attr(src, "status") %||% 0L
cat(tail(src, 1), "\n")
cat(sprintf("TOTAL testthat : OK %d | echecs %d | erreurs %d ; test_sources : %s\n",
            total[["ok"]], total[["echecs"]], total[["erreurs"]], if (code_src == 0) "OK" else "ECHEC"))
if (total[["echecs"]] + total[["erreurs"]] > 0 || code_src != 0) quit(status = 1)
