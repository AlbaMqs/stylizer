library(dplyr)
library(testthat)

# Définir les niveaux pour chaque variable catégorielle
levels_A <- c("A1", "A2", "A3")
levels_B <- c("B1", "B2", "B3", "B4")
levels_C <- c("C1", "C2")

# Générer le dataframe avec des tailles d'échantillons équilibrées
set.seed(123)  # Assure la reproductibilité des résultats

df <- tibble(
  VarA = sample(levels_A, size = 1000, replace = TRUE, prob = c(0.3, 0.4, 0.3)),
  VarB = sample(levels_B, size = 1000, replace = TRUE, prob = c(0.25, 0.25, 0.25, 0.25)),
  VarC = sample(levels_C, size = 1000, replace = TRUE, prob = c(0.5, 0.5))
)

# Introduire des valeurs manquantes (NA) dans chaque variable avec une probabilité de 10%
set.seed(456)  # Assure la reproductibilité des NA
df <- df %>%
  mutate(
    VarA = ifelse(runif(n()) < 0.1, NA, VarA),
    VarB = ifelse(runif(n()) < 0.1, NA, VarB),
    VarC = ifelse(runif(n()) < 0.1, NA, VarC)
  )

# Introduire une catégorie rare dans VarA (par exemple, ajouter une catégorie "A4" avec moins de 5 éléments)
df <- df %>%
  mutate(VarA = ifelse(row_number() <= 4, "A4", VarA))

# Liste de combinaisons de paramètres pour vérification manuelle
test_cases <- list(
  list(n.col = FALSE, na.use = "as.note", test = "chi2", out = "obf", p_group = c(2,2)),
  list(n.col = TRUE, na.use = "as.note", test = "chi2", out = "obf", p_group = c(2,2)),
  list(n.col = TRUE, na.use = "as.cat", test = "chi2", out = "obf", p_group = c(2,2)),
  list(n.col = TRUE, na.use = "no", test = "chi2", out = "obf", p_group = c(2,2)),
  list(n.col = TRUE, na.use = "as.note", test = "chi2", out = "pct", p_group = c(2,2)),
  list(n.col = TRUE, na.use = "as.note", test = "chi2", out = "n", p_group = c(2,2)),
  list(n.col = TRUE, na.use = "as.note", test = "chi2", out = "obf", p_group = NULL)
)

# Fonction pour exécuter et afficher les résultats pour chaque test
run_manual_tests <- function() {
  for (i in seq_along(test_cases)) {
    params <- test_cases[[i]]

    # Exécuter la fonction avec les paramètres actuels
    cat("\n-----------------------------------\n")
    cat("Test Case", i, "\n")
    print(params)  # Affiche les paramètres utilisés dans le test

    result <- tab_contingency_binded(
      df = df,
      VarA, VarC,
      y = VarB,
      p_group = params$p_group,
      n.col = params$n.col,
      na.use = params$na.use,
      test = params$test,
      out = params$out,
      label.title = TRUE,
      lang = "en"
    )

    # Afficher les résultats pour revue manuelle
    print(result)  # Affiche les premières lignes du résultat
  }
}

# Exécuter les tests manuels
run_manual_tests()

