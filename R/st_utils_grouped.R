st_utils_grouped <- function(data, group) {
  # Ajout des colonnes de groupe
  pvalue_columns <- which(str_detect(names(data), regex("p_value", ignore_case = TRUE)))
  if (length(pvalue_columns > 1)) {
    pvalue_columns <- pvalue_columns[-length(pvalue_columns)]

    # Ajouter une colonne vide après la colonne spécifiée
    data <- add_column(data, `<suppr1>` = "", .after = pvalue_columns[[1]])
  }

  # Création de la flextable de base
  ft <- flextable(data)

  # Suppression des titres de colonnes vide
  col.suppr <- grep("(<suppr\\d+>|Variable)", names(data))

  for (col in col.suppr) {
    ft <- flextable::compose(
      ft,
      part = "header",
      i = 1,
      j = col,
      value = as_paragraph("")
    )
  }

  # Ajout d'un sur-header
  name_col <- names(data)
  ind_group <- which(name_col == "Variable" | grepl("<suppr\\d+>", name_col))
  ind_group <- ind_group + 1
  ind_group <- ind_group[ind_group <= length(name_col)]

  replace_even <- function(source, by) {
    indices_pairs <- which(seq_along(source) %% 2 == 0)
    nb_replacements <- min(length(indices_pairs), length(by))
    source[indices_pairs[seq_len(nb_replacements)]] <- by[seq_len(nb_replacements)]

    return(source)
  }

  header_values <- rep("", length(ind_group) * 2)
  header_values <- replace_even(header_values, by = group)

  colwidths <- rep(1, length(header_values))
  diff_ind_group <- diff(ind_group) - 1
  diff_ind_group <- c(diff_ind_group, (length(name_col) - tail(ind_group, 1)) + 1)
  colwidths <- replace_even(colwidths, by = diff_ind_group)

  ft <- add_header_row(ft,
                       values = header_values,
                       colwidths = colwidths)

  ft <- border_remove(ft)  # Supprimer les bordures par défaut
  ft <- hline(ft, i = 1, j = ind_group, part = "header", border = fp_border(color = "black", width = 1.5))

  liste <- list(data,ft)
  return(liste)
}
