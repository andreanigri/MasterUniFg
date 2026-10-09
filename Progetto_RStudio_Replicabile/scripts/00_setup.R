# ============================================================
# 00_setup.R - Setup del progetto RStudio
# Manuale: Dispensa_R_Autocontenuta.pdf, Capitolo 1
# ============================================================
# Ogni passaggio eseguibile è preceduto da una breve spiegazione didattica.

# Aprire sempre il file StatisticaSociale_HMD.Rproj.
# I percorsi sono relativi alla radice del progetto: non usare setwd().

# Spiegazione: Creiamo o aggiorniamo l’oggetto `base_pkgs`; il valore sarà usato nei passaggi successivi.
base_pkgs <- c(
  "data.table", "dplyr", "tidyr", "ggplot2",
  "caret", "rpart", "randomForest", "nnet"
)

# Spiegazione: Ricaviamo `missing` selezionando o trasformando parti dell’oggetto indicato a destra.
missing <- base_pkgs[!vapply(base_pkgs, requireNamespace, logical(1), quietly = TRUE)]
# Spiegazione: Controlliamo questa condizione prima di eseguire il blocco successivo.
if (length(missing)) {
  # Spiegazione: Mostriamo un messaggio informativo senza interrompere l’esecuzione.
  message("Pacchetti mancanti: ", paste(missing, collapse = ", "))
  # Spiegazione: Mostriamo un messaggio informativo senza interrompere l’esecuzione.
  message("Installazione suggerita:")
  # Spiegazione: Mostriamo un messaggio informativo senza interrompere l’esecuzione.
  message("install.packages(c(", paste(sprintf("'%s'", missing), collapse = ", "), "))")
}

# Spiegazione: Carichiamo un altro script per riutilizzare funzioni già definite.
source("scripts/00_funzioni_comuni.R")

# Spiegazione: Stampiamo un messaggio sintetico nella console per seguire l’avanzamento dello script.
cat("Setup completato.
")
# Spiegazione: Stampiamo un messaggio sintetico nella console per seguire l’avanzamento dello script.
cat("Esempio di import HMD usato nel manuale:
")
# Spiegazione: Stampiamo un messaggio sintetico nella console per seguire l’avanzamento dello script.
cat('library(data.table)
')
# Spiegazione: Stampiamo un messaggio sintetico nella console per seguire l’avanzamento dello script.
cat('deaths <- fread("dati/HMD/Deaths_1x1.txt", skip = "PopName", na.strings = ".")\n')
# Spiegazione: Stampiamo un messaggio sintetico nella console per seguire l’avanzamento dello script.
cat('exposure <- fread("dati/HMD/Exposures_1x1.txt", skip = "PopName", na.strings = ".")\n')
