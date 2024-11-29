# Charger tous les packages requis pour les fonctions
library(dplyr)
library(flextable)
library(forcats)
library(labelled)
library(officer)
library(rlang)
library(stats)
library(tibble)
library(tidyr)
library(purrr)
library(stringr)
library(utils)
library(tidyselect)


# Charger le package here
library(here)

# Définir le dossier contenant les fichiers R
dossier_R <- here("R")

# Lister tous les fichiers avec extension .R dans le dossier spécifié
fichiers_R <- list.files(dossier_R, pattern = "\\.R$", full.names = TRUE)

# Sourcer chaque fichier trouvé
lapply(fichiers_R, source)

cat("Tous les fichiers dans le dossier R ont été sourcés.")

