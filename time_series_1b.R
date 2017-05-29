# Milano MeetUp Rlab #3 
# Data: 29/05/2017
# Scordino Pasquale - Caputo Enrica


# Carico libreie necessarie
library(ggplot2)
library(dplyr)
library(tidyr)
library(plyr)

# Carico dataset
df <- read.csv(paste0(dir.data,"PEG_2017_2019_opendata_16_05_anni2013_2019.csv"), 
             header=T, stringsAsFactors = F, dec = ".")

# Manipolazione dataset
df$RENDICONTO.2013 <- gsub(",", "", df$RENDICONTO.2013)
df$RENDICONTO.2014 <- gsub(",", "", df$RENDICONTO.2014)
df$RENDICONTO.2015 <- gsub(",", "", df$RENDICONTO.2015)
df$RENDICONTO.2016 <- gsub(",", "", df$RENDICONTO.2016)
df$STANZIAMENTO.2017 <- gsub(",", "", df$STANZIAMENTO.2017)
df$STANZIAMENTO.2018 <- gsub(",", "", df$STANZIAMENTO.2018)
df$STANZIAMENTO.2019 <- gsub(",", "", df$STANZIAMENTO.2019)

df$RENDICONTO.2013 <- as.numeric(df$RENDICONTO.2013)
df$RENDICONTO.2014 <- as.numeric(df$RENDICONTO.2014)
df$RENDICONTO.2015 <- as.numeric(df$RENDICONTO.2015)
df$RENDICONTO.2016 <- as.numeric(df$RENDICONTO.2016)
df$STANZIAMENTO.2017 <- as.numeric(df$STANZIAMENTO.2017)
df$STANZIAMENTO.2018 <- as.numeric(df$STANZIAMENTO.2018)
df$STANZIAMENTO.2019 <- as.numeric(df$STANZIAMENTO.2019)

# Subsetting del dataset con le 5 voci di classificazioni delle spese
spese.Milano <- subset(df, TIPO == "USCITE" & PDC.Descrizione.Livello1
                       %in% c("SPESE CORRENTI", "SPESE IN CONTO CAPITALE", 
                              "SPESE PER INCREMENTO ATTIVITA' FINANZIARIE", "RIMBORSO PRESTITI",
                              "USCITE PER CONTO TERZI E PARTITE DI GIRO"))


# Dati da formato largo a formato lungo
spese.Milano_seq <- spese.Milano %>%
  select(-STANZIAMENTO.DI.CASSA.2017) %>% 
  gather(ANNO, IMPORTO, RENDICONTO.2013:STANZIAMENTO.2019) %>%
  mutate(ANNO = substr(ANNO, nchar(ANNO)-3, nchar(ANNO)))

# Calcolo somma importi rispetto ad anno, tipo di spesa e tipo di programma
MilanoUscite_Aggregate <- tapply(spese.Milano_seq$IMPORTO, 
                               list(spese.Milano_seq$ANNO,spese.Milano_seq$PDC.Descrizione.Livello1, 
                                    spese.Milano_seq$PDC.Descrizione.Programma), 
                               sum)

# Manipolazione dataset calcolato
MilanoUscite_Aggregate <- adply(MilanoUscite_Aggregate, c(1,2,3))
names(MilanoUscite_Aggregate) <- c("ANNO", "SPESA", "PROGRAMMA", "IMPORTO")
MilanoUscite_Aggregate$ANNO <- as.character(MilanoUscite_Aggregate$ANNO)

# Funzione grafica - manca implementazione droplevels
TimeSeries_Programma <- function(nome_programma){
  ggplot(data = MilanoUscite_Aggregate %>% 
           filter(PROGRAMMA == nome_programma), 
         aes(x=as.numeric(ANNO), y=IMPORTO, col = SPESA)) +
    geom_line() +
    ggtitle(nome_programma) +
    labs(x="ANNO")
}

# Test funzione grafica
TimeSeries_Programma("ISTRUZIONE PRESCOLASTICA")
