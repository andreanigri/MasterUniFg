# ============================================================
# 00_funzioni_comuni.R
# Funzioni condivise dal progetto.
#
# NOTA DIDATTICA
# Questo file contiene funzioni di servizio che evitano di ripetere negli
# script gli stessi passaggi. Lo studente non deve memorizzarne ogni riga:
# negli script C01-C03 i passaggi principali sono mostrati in modo esteso.
# ============================================================
# Ogni passaggio eseguibile è preceduto da una breve spiegazione didattica.

# Spiegazione: Definiamo la funzione `pacchetto`; il blocco successivo descrive cosa farà ogni volta che verrà chiamata.
pacchetto <- function(x) {
  # Spiegazione: Controlliamo questa condizione prima di eseguire il blocco successivo.
  if (!requireNamespace(x, quietly = TRUE)) {
    # Spiegazione: Interrompiamo lo script con un messaggio esplicativo perché un controllo non è stato superato.
    stop(sprintf("Pacchetto '%s' non installato. Eseguire install.packages('%s').", x, x))
  }
}

# ------------------------------------------------------------
# Import HMD robusto e leggibile
# ------------------------------------------------------------
# Spiegazione: Definiamo la funzione `leggi_hmd`; il blocco successivo descrive cosa farà ogni volta che verrà chiamata.
leggi_hmd <- function(path = "dati/HMD",
                      paesi = NULL, anni = NULL,
                      eta_min = NULL, eta_max = NULL,
                      sessi = c("F", "M")) {

  # Spiegazione: Eseguiamo questo passaggio per preparare, controllare o mostrare un risultato utile alla fase successiva.
  pacchetto("data.table")
  # Spiegazione: Eseguiamo questo passaggio per preparare, controllare o mostrare un risultato utile alla fase successiva.
  pacchetto("dplyr")
  # Spiegazione: Eseguiamo questo passaggio per preparare, controllare o mostrare un risultato utile alla fase successiva.
  pacchetto("tidyr")

  # 1) Import.
  # skip = "PopName" salta la riga descrittiva iniziale del file HMD.
  # na.strings = "." trasforma i punti HMD in veri valori mancanti NA.
  # Spiegazione: Importiamo il file di testo con `fread()` e salviamo la tabella in `deaths`.
  deaths <- data.table::fread(
    file.path(path, "Deaths_1x1.txt"),
    skip = "PopName",
    na.strings = "."
  )

  # Spiegazione: Importiamo il file di testo con `fread()` e salviamo la tabella in `exposure`.
  exposure <- data.table::fread(
    file.path(path, "Exposures_1x1.txt"),
    skip = "PopName",
    na.strings = "."
  )

  # Controllo: le colonne usate nei rapporti devono essere numeriche.
  # Spiegazione: Controlliamo questa condizione prima di eseguire il blocco successivo.
  if (!is.numeric(deaths$Female) || !is.numeric(deaths$Male) ||
      !is.numeric(exposure$Female) || !is.numeric(exposure$Male)) {
    # Spiegazione: Interrompiamo lo script con un messaggio esplicativo perché un controllo non è stato superato.
    stop("Import HMD non corretto: Female/Male non sono numeriche. Controllare na.strings='.'.")
  }

  # 2) Pulizia dell'eta'. La classe aperta 110+ viene ricordata in open_age.
  # Spiegazione: Creiamo o aggiorniamo l’oggetto `deaths`; il valore sarà usato nei passaggi successivi.
  deaths <- dplyr::mutate(
    deaths,
    open_age = Age == "110+",
    Age = as.integer(ifelse(Age == "110+", "110", Age)),
    Year = as.integer(Year)
  )

  # Spiegazione: Creiamo o aggiorniamo l’oggetto `exposure`; il valore sarà usato nei passaggi successivi.
  exposure <- dplyr::mutate(
    exposure,
    open_age = Age == "110+",
    Age = as.integer(ifelse(Age == "110+", "110", Age)),
    Year = as.integer(Year)
  )

  # 3) Female/Male da colonne separate a una variabile Sex.
  # Spiegazione: Portiamo la tabella in formato long e salviamo il risultato in `deaths_long`.
  deaths_long <- tidyr::pivot_longer(
    deaths,
    cols = c("Female", "Male"),
    names_to = "Sex",
    values_to = "Deaths"
  )
  # Spiegazione: Creiamo o aggiorniamo l’oggetto `deaths_long`; il valore sarà usato nei passaggi successivi.
  deaths_long <- dplyr::select(
    deaths_long, PopName, Year, Age, open_age, Sex, Deaths
  )

  # Spiegazione: Portiamo la tabella in formato long e salviamo il risultato in `exposure_long`.
  exposure_long <- tidyr::pivot_longer(
    exposure,
    cols = c("Female", "Male"),
    names_to = "Sex",
    values_to = "Exposure"
  )
  # Spiegazione: Creiamo o aggiorniamo l’oggetto `exposure_long`; il valore sarà usato nei passaggi successivi.
  exposure_long <- dplyr::select(
    exposure_long, PopName, Year, Age, open_age, Sex, Exposure
  )

  # 4) Unione di decessi ed esposizioni.
  # Spiegazione: Uniamo due tabelle sulle chiavi comuni e salviamo il risultato in `H`.
  H <- dplyr::inner_join(
    deaths_long,
    exposure_long,
    by = c("PopName", "Year", "Age", "open_age", "Sex")
  )

  # 5) Variabili derivate.
  # Spiegazione: Creiamo o aggiorniamo l’oggetto `H`; il valore sarà usato nei passaggi successivi.
  H <- dplyr::mutate(
    H,
    Sex = ifelse(Sex == "Female", "F", "M"),
    country = PopName,
    cohort = Year - Age
  )

  # Calcolo del tasso e gestione esplicita dei casi non validi.
  # Spiegazione: Creiamo o aggiorniamo una variabile all’interno del data frame.
  H$mx <- H$Deaths / H$Exposure
  # Spiegazione: Eseguiamo questo passaggio per preparare, controllare o mostrare un risultato utile alla fase successiva.
  H$mx[is.na(H$Exposure) | H$Exposure <= 0] <- NA_real_

  # Spiegazione: Creiamo o aggiorniamo una variabile all’interno del data frame.
  H$log_mx <- log(H$mx)
  # Spiegazione: Eseguiamo questo passaggio per preparare, controllare o mostrare un risultato utile alla fase successiva.
  H$log_mx[!is.finite(H$log_mx)] <- NA_real_

  # 6) Filtri richiesti dall'utente della funzione.
  # Spiegazione: Controlliamo questa condizione prima di eseguire il blocco successivo.
  if (!is.null(paesi)) H <- H[H$country %in% paesi, ]
  # Spiegazione: Controlliamo questa condizione prima di eseguire il blocco successivo.
  if (!is.null(anni)) H <- H[H$Year %in% anni, ]
  # Spiegazione: Controlliamo questa condizione prima di eseguire il blocco successivo.
  if (!is.null(eta_min)) H <- H[H$Age >= eta_min, ]
  # Spiegazione: Controlliamo questa condizione prima di eseguire il blocco successivo.
  if (!is.null(eta_max)) H <- H[H$Age <= eta_max, ]
  # Spiegazione: Controlliamo questa condizione prima di eseguire il blocco successivo.
  if (!is.null(sessi)) H <- H[H$Sex %in% sessi, ]

  # Spiegazione: Ricaviamo `H` selezionando o trasformando parti dell’oggetto indicato a destra.
  H <- H[order(H$country, H$Year, H$Sex, H$Age), ]

  # Restituiamo una data.table per mantenere compatibilita' con gli script
  # successivi del corso.
  # Spiegazione: Eseguiamo questo passaggio per preparare, controllare o mostrare un risultato utile alla fase successiva.
  data.table::as.data.table(H)
}

# Serie nazionali utilizzate per confronti internazionali.
# Il vettore è codice R, non una fonte dati esterna.
# Spiegazione: Definiamo la funzione `serie_nazionali_hmd`; il blocco successivo descrive cosa farà ogni volta che verrà chiamata.
serie_nazionali_hmd <- function() {
  # Spiegazione: Eseguiamo questo passaggio per preparare, controllare o mostrare un risultato utile alla fase successiva.
  c("AUS","AUT","BEL","BGR","BLR","CAN","CHE","CHL","CZE","DEUTNP",
    "DNK","ESP","EST","FIN","FRATNP","GBR_NP","GRC","HKG","HRV","HUN",
    "IRL","ISL","ISR","ITA","JPN","KOR","LTU","LUX","LVA","NLD","NOR",
    "NZL_NP","POL","PRT","RUS","SVK","SVN","SWE","TWN","UKR","USA")
}

# Conversione minima HMD -> ISO3 per le sole geometrie cartografiche.
# I valori statistici restano sempre quelli ricavati dai file HMD originali.
# Spiegazione: Definiamo la funzione `hmd_to_iso3`; il blocco successivo descrive cosa farà ogni volta che verrà chiamata.
hmd_to_iso3 <- function(x) {
  # Spiegazione: Creiamo o aggiorniamo l’oggetto `out`; il valore sarà usato nei passaggi successivi.
  out <- x
  # Spiegazione: Modifichiamo solo le celle che soddisfano la condizione tra parentesi quadre.
  out[out == "DEUTNP"] <- "DEU"
  # Spiegazione: Modifichiamo solo le celle che soddisfano la condizione tra parentesi quadre.
  out[out == "FRATNP"] <- "FRA"
  # Spiegazione: Modifichiamo solo le celle che soddisfano la condizione tra parentesi quadre.
  out[out == "GBR_NP"] <- "GBR"
  # Spiegazione: Modifichiamo solo le celle che soddisfano la condizione tra parentesi quadre.
  out[out == "NZL_NP"] <- "NZL"
  # Spiegazione: Eseguiamo questo passaggio per preparare, controllare o mostrare un risultato utile alla fase successiva.
  out
}

# ------------------------------------------------------------
# Life table periodale didattica da mx in età singole
# ------------------------------------------------------------
# Spiegazione: Definiamo la funzione `life_table_hmd`; il blocco successivo descrive cosa farà ogni volta che verrà chiamata.
life_table_hmd <- function(d, radix = 100000) {
  # Spiegazione: Verifichiamo un requisito essenziale sui dati; se non è rispettato R interrompe lo script.
  stopifnot(all(c("Age", "mx") %in% names(d)))
  # Spiegazione: Ricaviamo `d` selezionando o trasformando parti dell’oggetto indicato a destra.
  d <- d[order(d$Age), , drop = FALSE]
  # Spiegazione: Ricaviamo `d` selezionando o trasformando parti dell’oggetto indicato a destra.
  d <- d[is.finite(d$mx) & d$mx >= 0, , drop = FALSE]
  # Spiegazione: Controlliamo questa condizione prima di eseguire il blocco successivo.
  if (nrow(d) < 2) stop("Numero di età insufficiente per costruire la life table.")

  # Spiegazione: Creiamo o aggiorniamo l’oggetto `n`; il valore sarà usato nei passaggi successivi.
  n <- nrow(d)
  # Spiegazione: Creiamo o aggiorniamo l’oggetto `ax`; il valore sarà usato nei passaggi successivi.
  ax <- rep(0.5, n)

  # Approssimazione standard per l'età 0: differenziata per sesso quando disponibile.
  # Serve a rendere la tavola più realistica rispetto a porre a0=0.5.
  # Spiegazione: Controlliamo questa condizione prima di eseguire il blocco successivo.
  if (d$Age[1] == 0) {
    # Spiegazione: Ricaviamo `m0` selezionando o trasformando parti dell’oggetto indicato a destra.
    m0 <- d$mx[1]
    # Spiegazione: Ricaviamo `sex` selezionando o trasformando parti dell’oggetto indicato a destra.
    sex <- if ("Sex" %in% names(d)) as.character(d$Sex[1]) else NA_character_
    # Spiegazione: Controlliamo questa condizione prima di eseguire il blocco successivo.
    if (identical(sex, "M")) {
      # Spiegazione: Modifichiamo solo le celle che soddisfano la condizione tra parentesi quadre.
      ax[1] <- if (m0 < 0.107) 0.045 + 2.684 * m0 else 0.330
    # Spiegazione: Eseguiamo questo passaggio per preparare, controllare o mostrare un risultato utile alla fase successiva.
    } else if (identical(sex, "F")) {
      # Spiegazione: Modifichiamo solo le celle che soddisfano la condizione tra parentesi quadre.
      ax[1] <- if (m0 < 0.107) 0.053 + 2.800 * m0 else 0.350
    # Spiegazione: Eseguiamo questo passaggio per preparare, controllare o mostrare un risultato utile alla fase successiva.
    } else {
      # Spiegazione: Modifichiamo solo le celle che soddisfano la condizione tra parentesi quadre.
      ax[1] <- if (m0 < 0.107) 0.049 + 2.742 * m0 else 0.340
    }
  }

  # Spiegazione: Ricaviamo `qx` selezionando o trasformando parti dell’oggetto indicato a destra.
  qx <- d$mx / (1 + (1 - ax) * d$mx)
  # Spiegazione: Creiamo o aggiorniamo l’oggetto `qx`; il valore sarà usato nei passaggi successivi.
  qx <- pmin(pmax(qx, 0), 1)

  # L'ultima età è trattata come intervallo aperto.
  # Spiegazione: Modifichiamo solo le celle che soddisfano la condizione tra parentesi quadre.
  qx[n] <- 1

  # Spiegazione: Creiamo o aggiorniamo l’oggetto `lx`; il valore sarà usato nei passaggi successivi.
  lx <- numeric(n); dx <- numeric(n); Lx <- numeric(n)
  # Spiegazione: Modifichiamo solo le celle che soddisfano la condizione tra parentesi quadre.
  lx[1] <- radix
  # Spiegazione: Ripetiamo le istruzioni del blocco per ogni elemento della sequenza indicata.
  for (i in seq_len(n)) {
    # Spiegazione: Modifichiamo solo le celle che soddisfano la condizione tra parentesi quadre.
    dx[i] <- lx[i] * qx[i]
    # Spiegazione: Controlliamo questa condizione prima di eseguire il blocco successivo.
    if (i < n) lx[i + 1] <- lx[i] - dx[i]
    # Spiegazione: Controlliamo questa condizione prima di eseguire il blocco successivo.
    if (i < n) Lx[i] <- lx[i] - (1 - ax[i]) * dx[i]
  }
  # Spiegazione: Modifichiamo solo le celle che soddisfano la condizione tra parentesi quadre.
  Lx[n] <- if (d$mx[n] > 0) lx[n] / d$mx[n] else 0

  # Spiegazione: Creiamo o aggiorniamo l’oggetto `Tx`; il valore sarà usato nei passaggi successivi.
  Tx <- rev(cumsum(rev(Lx)))
  # Spiegazione: Creiamo `ex` scegliendo un valore quando la condizione è vera e un altro quando è falsa.
  ex <- ifelse(lx > 0, Tx / lx, NA_real_)

  # Spiegazione: Eseguiamo questo passaggio per preparare, controllare o mostrare un risultato utile alla fase successiva.
  cbind(d, ax = ax, qx = qx, lx = lx, dx = dx, Lx = Lx, Tx = Tx, ex = ex)
}

# Estrae e0/e65 costruendo PRIMA la life table per ciascun gruppo.
# Spiegazione: Definiamo la funzione `calcola_ex_hmd`; il blocco successivo descrive cosa farà ogni volta che verrà chiamata.
calcola_ex_hmd <- function(H, eta = c(0, 65)) {
  # Spiegazione: Creiamo il data frame `d` mettendo insieme le variabili indicate.
  d <- as.data.frame(H)
  # Spiegazione: Creiamo o aggiorniamo l’oggetto `req`; il valore sarà usato nei passaggi successivi.
  req <- c("country", "Year", "Sex", "Age", "mx")
  # Spiegazione: Controlliamo questa condizione prima di eseguire il blocco successivo.
  if (!all(req %in% names(d))) stop("H non contiene tutte le variabili richieste.")

  # Dividiamo i dati per paese-anno-sesso.
  # Spiegazione: Ricaviamo `gruppi` selezionando o trasformando parti dell’oggetto indicato a destra.
  gruppi <- split(d, list(d$country, d$Year, d$Sex), drop = TRUE)

  # Spiegazione: Creiamo o aggiorniamo l’oggetto `risultati`; il valore sarà usato nei passaggi successivi.
  risultati <- list()
  # Spiegazione: Creiamo o aggiorniamo l’oggetto `gruppi_saltati`; il valore sarà usato nei passaggi successivi.
  gruppi_saltati <- 0L

  # Spiegazione: Ripetiamo le istruzioni del blocco per ogni elemento della sequenza indicata.
  for (g in gruppi) {
    # Alcune serie storiche HMD contengono interi anni senza informazioni
    # utilizzabili (per esempio alcuni anni bellici). In quel caso non e'
    # possibile costruire una life table e il gruppo viene semplicemente saltato.
    # Spiegazione: Ricaviamo `g` selezionando o trasformando parti dell’oggetto indicato a destra.
    g <- g[is.finite(g$Age) & is.finite(g$mx) & g$mx >= 0, , drop = FALSE]

    # Spiegazione: Controlliamo questa condizione prima di eseguire il blocco successivo.
    if (nrow(g) < 2 || !any(g$Age == 0)) {
      # Spiegazione: Creiamo o aggiorniamo l’oggetto `gruppi_saltati`; il valore sarà usato nei passaggi successivi.
      gruppi_saltati <- gruppi_saltati + 1L
      # Spiegazione: Eseguiamo questo passaggio per preparare, controllare o mostrare un risultato utile alla fase successiva.
      next
    }

    # Spiegazione: Costruiamo la tavola di mortalità e salviamo il risultato in `lt`.
    lt <- life_table_hmd(g)
    # Spiegazione: Ricaviamo `out` selezionando o trasformando parti dell’oggetto indicato a destra.
    out <- lt[lt$Age %in% eta,
              c("country", "Year", "Sex", "Age", "ex"),
              drop = FALSE]

    # Spiegazione: Controlliamo questa condizione prima di eseguire il blocco successivo.
    if (nrow(out) > 0) risultati[[length(risultati) + 1L]] <- out
  }

  # Spiegazione: Controlliamo questa condizione prima di eseguire il blocco successivo.
  if (length(risultati) == 0) {
    # Spiegazione: Interrompiamo lo script con un messaggio esplicativo perché un controllo non è stato superato.
    stop("Nessun gruppo contiene dati sufficienti per costruire la life table.")
  }

  # Spiegazione: Controlliamo questa condizione prima di eseguire il blocco successivo.
  if (gruppi_saltati > 0) {
    # Spiegazione: Mostriamo un messaggio informativo senza interrompere l’esecuzione.
    message("Life table: saltati ", gruppi_saltati,
            " gruppi paese-anno-sesso senza dati sufficienti.")
  }

  # Spiegazione: Eseguiamo questo passaggio per preparare, controllare o mostrare un risultato utile alla fase successiva.
  data.table::as.data.table(do.call(rbind, risultati))
}

# Spiegazione: Definiamo la funzione `best_practice_e0`; il blocco successivo descrive cosa farà ogni volta che verrà chiamata.
best_practice_e0 <- function(H, sesso = "F", anni = NULL,
                             paesi = serie_nazionali_hmd()) {
  # Spiegazione: Creiamo il data frame `d` mettendo insieme le variabili indicate.
  d <- as.data.frame(H)
  # Spiegazione: Ricaviamo `d` selezionando o trasformando parti dell’oggetto indicato a destra.
  d <- d[d$Sex == sesso & d$country %in% paesi, ]
  # Spiegazione: Controlliamo questa condizione prima di eseguire il blocco successivo.
  if (!is.null(anni)) d <- d[d$Year %in% anni, ]

  # Spiegazione: Creiamo il data frame `e0` mettendo insieme le variabili indicate.
  e0 <- as.data.frame(calcola_ex_hmd(d, eta = 0))
  # Spiegazione: Ricaviamo `e0` selezionando o trasformando parti dell’oggetto indicato a destra.
  e0 <- e0[order(e0$Year, -e0$ex), ]

  # Dopo l'ordinamento, la prima riga disponibile di ogni anno ha l'e0 piu' alto.
  # Spiegazione: Ricaviamo `bp` selezionando o trasformando parti dell’oggetto indicato a destra.
  bp <- e0[!duplicated(e0$Year), ]
  # Spiegazione: Controlliamo i nomi delle variabili presenti nell’oggetto.
  names(bp)[names(bp) == "ex"] <- "e0"

  # Spiegazione: Eseguiamo questo passaggio per preparare, controllare o mostrare un risultato utile alla fase successiva.
  data.table::as.data.table(bp)
}

# Metriche di regressione su dati nuovi
# Spiegazione: Definiamo la funzione `rmse`; il blocco successivo descrive cosa farà ogni volta che verrà chiamata.
rmse <- function(obs, pred) sqrt(mean((obs - pred)^2, na.rm = TRUE))
# Spiegazione: Definiamo la funzione `mae`; il blocco successivo descrive cosa farà ogni volta che verrà chiamata.
mae  <- function(obs, pred) mean(abs(obs - pred), na.rm = TRUE)

# Brier score per probabilità previste
# Spiegazione: Definiamo la funzione `brier`; il blocco successivo descrive cosa farà ogni volta che verrà chiamata.
brier <- function(obs01, p) mean((obs01 - p)^2, na.rm = TRUE)
