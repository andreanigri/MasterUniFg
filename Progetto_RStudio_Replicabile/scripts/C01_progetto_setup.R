# ============================================================
# C01_progetto_setup.R
# Progetto RStudio, dati originali e cartelle
# Manuale: Dispensa_R_Autocontenuta.pdf - Capitolo 1
# Script guida: segue l'ordine del manuale, senza modificarlo.
# ============================================================
# Ogni passaggio eseguibile è preceduto da una breve spiegazione didattica.

# 1. Verifica cartelle principali ------------------------------------------
# Spiegazione: Elenciamo i file presenti nella cartella per verificare che i dati siano disponibili.
list.files()
# Spiegazione: Elenciamo i file presenti nella cartella per verificare che i dati siano disponibili.
list.files("dati")
# Spiegazione: Elenciamo i file presenti nella cartella per verificare che i dati siano disponibili.
list.files("dati/HMD")
# Spiegazione: Elenciamo i file presenti nella cartella per verificare che i dati siano disponibili.
list.files("dati/casi_didattici")

# 2. File HMD originali usati nel manuale ----------------------------------
# Spiegazione: Carichiamo il pacchetto che contiene le funzioni usate nelle righe successive.
library(data.table)
# I file HMD hanno una riga descrittiva iniziale e usano "." per i mancanti.
# skip = "PopName" parte dalla vera intestazione; na.strings = "." crea NA.
# Spiegazione: Importiamo il file di testo con `fread()` e salviamo la tabella in `deaths`.
deaths <- fread("dati/HMD/Deaths_1x1.txt",
                skip = "PopName", na.strings = ".")
# Spiegazione: Importiamo il file di testo con `fread()` e salviamo la tabella in `exposure`.
exposure <- fread("dati/HMD/Exposures_1x1.txt",
                  skip = "PopName", na.strings = ".")

# Spiegazione: Visualizziamo le prime righe per controllare rapidamente il contenuto dei dati.
head(deaths)
# Spiegazione: Visualizziamo le prime righe per controllare rapidamente il contenuto dei dati.
head(exposure)
# Spiegazione: Controlliamo quante righe e colonne contiene l’oggetto.
dim(deaths)
# Spiegazione: Controlliamo quante righe e colonne contiene l’oggetto.
dim(exposure)
# Spiegazione: Controlliamo i nomi delle variabili presenti nell’oggetto.
names(deaths)

# 3. Perche' non usiamo file derivati --------------------------------------
# Nel manuale i tassi, le life table e gli indicatori sono ricostruiti
# dai file originali Deaths_1x1.txt e Exposures_1x1.txt.
