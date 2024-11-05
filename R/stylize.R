#' Stylize
#'
#' Handles the table layout by organizing its structure, formatting cells, and aligning content for optimal readability.
#'
#' @param data A table or dataframe to be formatted.
#' @param theme Flextable theme you want to use
#'
#' @return A Flextable object ready to be printed in your Word document.
#' @import flextable
#' @export
#'
#' @examples
#' # Example of using the stylize function
#'
#' # Create a small dataset
#' example_data <- data.frame(
#'   Name = c("Alice", "Bob", "Charlie"),
#'   Age = c(25, 30, 35),
#'   Profession = c("Engineer", "Doctor", "Teacher")
#' )
#'
#' # Apply the stylize function to the dataset
#' stylized_table <- stylize(example_data)
#'
#' # Display the result (here, stylized_table is the formatted Flextable object)
#' print(stylized_table)

stylize <- function(data,
                    theme = "classy"){
  ft <- flextable(data)

  # Application du thème
  if(theme == "classy"){
    ft <- theme_classy(ft)
  }

  return(ft)
}
# Create a small dataset
# example_data <- data.frame(
#   Name = c("Alice", "Bob", "Charlie"),
#   Age = c(25, 30, 35),
#   Profession = c("Engineer", "Doctor", "Teacher")
# )
#
# example_ft <- example_data |>
#   stylize()
#
# example_ft


#
# ftab_add_missing <- function(ft, data) {
#   # Détection des <missing>
#   row.var <- grep("<missing\\:\\d+>", data$Variable)
#   missing.list <- ""
#
#   # Suppression des missings dans le corps
#   for (row in row.var) {
#     missing <- data$Variable[row] |>
#       str_extract("(?<=<missing\\:)\\d+(?=>)")
#
#     var_name <-  data$Variable[row] |>
#       str_remove_all("<.+>")
#
#     missing <- paste0("; n=", missing, " on ", str_to_lower(var_name))
#     missing.list <- paste0(missing.list, missing)
#
#     data$Variable[row] <- data$Variable[row] |>
#       str_remove("<missing\\:\\d+>")
#
#     ft <- flextable::compose(ft,
#                              i = row,
#                              j = "Variable",
#                              value = as_paragraph(as_b(data$Variable[row])))
#   }
#
#   missing.list <- str_remove(missing.list, "^;")
#
#   # Nombre de poulation différentes
#   ncol_data <- data |>
#     select(-contains("p-value")) |>
#     select(-1) |>
#     ncol()
#
#   # Population totale
#   n_ens <- data |>
#     select(-contains("p-value")) |>
#     names() |>
#     str_extract_all("(?<=N=)\\d+")  |>
#     as.numeric() |>
#     sum(na.rm = TRUE)
#
#   # Concatenation de la note de bas de page
#   if (ncol_data == 1) {
#     missing.list <- paste0("Number of total missings for each variable:", missing.list)
#   }
#   else{
#     missing.list <- paste0(
#       "Number of total missings for the ",
#       ncol_data,
#       " populations (n=",
#       n_ens,
#       ") for each variable:",
#       missing.list
#     )
#   }
#
#   # Aout de la note de bas de page
#   ft <- add_footer_lines(ft, values = missing.list)
#
#   return(ft)
# }
#
# ftab_add_note <- function(ft, txt, n) {
#   ft <- add_footer_lines(ft, values = "")
#   last_footer_index <- ft$footer$content[["nrow"]]
#
#   ft <- flextable::compose(
#     x = ft,
#     part = "footer",
#     i = last_footer_index,
#     value = as_paragraph(as_sup(as_integer(n)), txt)
#   )
#
#   return(ft)
# }
#
# ftab_grouped_flextable <- function(data, group) {
#   # Ajout des colonnes de groupe
#   pvalue_columns <- which(str_detect(names(data), regex("p-value", ignore_case = TRUE)))
#   if (length(pvalue_columns > 1)) {
#     pvalue_columns <- pvalue_columns[-length(pvalue_columns)]
#
#     # Ajouter une colonne vide après la colonne spécifiée
#     data <- add_column(data, `<suppr1>` = "", .after = pvalue_columns[[1]])
#   }
#
#   # Création de la flextable de base
#   ft <- flextable(data)
#
#   # Suppression des titres de colonnes vide
#   col.suppr <- grep("(<suppr\\d+>|Variable)", names(data))
#
#   for (col in col.suppr) {
#     ft <- flextable::compose(
#       ft,
#       part = "header",
#       i = 1,
#       j = col,
#       value = as_paragraph("")
#     )
#   }
#
#   # Ajout d'un sur-header
#   name_col <- names(data)
#   ind_group <- which(name_col == "Variable" | grepl("<suppr\\d+>", name_col))
#   ind_group <- ind_group + 1
#   ind_group <- ind_group[ind_group <= length(name_col)]
#
#   replace_even <- function(source, by) {
#     indices_pairs <- which(seq_along(source) %% 2 == 0)
#     nb_replacements <- min(length(indices_pairs), length(by))
#     source[indices_pairs[seq_len(nb_replacements)]] <- by[seq_len(nb_replacements)]
#
#     return(source)
#   }
#
#   header_values <- rep("", length(ind_group) * 2)
#   header_values <- replace_even(header_values, by = group)
#
#   colwidths <- rep(1, length(header_values))
#   diff_ind_group <- diff(ind_group) - 1
#   diff_ind_group <- c(diff_ind_group, (length(name_col) - tail(ind_group, 1)) + 1)
#   colwidths <- replace_even(colwidths, by = diff_ind_group)
#
#   ft <- add_header_row(ft,
#                        values = header_values,
#                        colwidths = colwidths)
#
#   ft <- border_remove(ft)  # Supprimer les bordures par défaut
#   ft <- hline(ft, i = 1, j = ind_group, part = "header", border = fp_border(color = "black", width = 1.5))
#
#   liste <- list(data,ft)
#   return(liste)
# }
#
# ftab_stylize <- function(data, group = NULL) {
#
#   n <- as.integer(1)
#
#   if(!is.null(group)){
#     liste_ft <- ftab_grouped_flextable(data, group)
#     data <- liste_ft[[1]]
#     ft <- liste_ft[[2]]
#   }
#   else{
#     ft <- flextable(data)
#   }
#
#   # Affichage des missing
#   ft <- ftab_add_missing(ft, data)
#
#   # Mise en page de la colonne de modalité
#   mod <- is.na(data[[2]])
#
#   for (i in which(mod)) { #
#     ft <- flextable::merge_at(ft, i = i, part = "body")
#   }
#   ft <- flextable::padding(ft,
#                            i = !mod,
#                            padding.left = 20,
#                            part = "body")
#   ft <- flextable::compose(ft,
#                            part = "header",
#                            i = 1, j = 1,
#                            value = as_paragraph(""))
#
#   # Mise en forme des colonnes de p-value
#   col.var <- grep("p-value", names(data))
#   if(is.null(group)){
#     i = 1
#   }
#   else{
#     i = 2
#   }
#
#   if (length(col.var) > 0) {
#     ft <- flextable::compose(ft,
#                              part = "header",
#                              j = col.var,
#                              i = i,
#                              value = as_paragraph(as_i("p value"), as_sup(n)))
#
#     ft <- ftab_add_note(ft, "Categories with counts lower than 5 are excluded from the χ² test used to calculate the p-value.", n)
#     n <- n + 1
#   }
#
#   # Ajout des non addible
#   row.var <- grep("<nonadd>", data$Variable)
#
#   for (row in row.var) {
#
#     data$Variable[row] <- data$Variable[row] |>
#       str_remove("<nonadd>") |>
#       str_remove("<missing\\:\\d+>")
#
#     ft <- flextable::compose(ft,
#                              i = row,
#                              j = "Variable",
#                              value = as_paragraph(as_b(data$Variable[row]), as_sup(as_integer(n))))
#   }
#
#   if(length(row.var) > 0){
#     ft <- ftab_add_note(ft, "Categories are non-exclusive; therefore, the totals in the columns may not add up to 100%", n)
#     n <- n + 1
#   }
#
#   # Ajout des notes de bas de page
#   row.var <- grep("<note\\:.+>", data$Variable)
#
#   for (row in row.var) {
#     note_content <- data$Variable[row] |>
#       str_extract("(?<=<note\\:).+(?=>)")
#
#     data$Variable[row] <- data$Variable[row] |>
#       str_remove("<note\\:.+>") |>
#       str_remove("<nonadd>") |>
#       str_remove("<missing\\:\\d+>")
#
#     ft <- flextable::compose(ft,
#                              i = row,
#                              j = "Variable",
#                              value = as_paragraph(data$Variable[row], as_sup(as_integer(n))))
#     ft <- ftab_add_note(ft, note_content, n)
#     n <- n + 1
#   }
#
#   # Application du thème
#   ft <- ftab_theme_perso(ft)
#
#   # Retourner le tableau formaté
#   return(ft)
# }
#
# # Fonction de découpe d'un vecteur
# split_col <- function(cols, p_group) {
#   start <- 1
#   result <- list()
#
#   for (i in seq_along(p_group)) {
#     end <- start + p_group[i] - 1
#     result[[i]] <- cols[start:end]
#     start <- end + 1
#   }
#
#   return(result)
# }
#
# format_p_val <- function(tab){
#
#   tab <- tab |>
#     column_to_rownames(var = names(tab[1])) |>
#     filter(!if_any(where(is.numeric), ~ . < 5))
#
#   chisq <- chisq.test(tab, correct = TRUE)
#
#   p.value <- case_when(
#     chisq$p.value < 0.001 ~ "< 0.001",
#     chisq$p.value < 0.01 ~ "< 0.01",
#     TRUE ~ as.character(format(round(chisq$p.value, 2), nsmall = 2))
#   )
#
#   return(p.value)
# }
#
# pourcentage <- function(n, eff) {
#   pourcent <- (n / sum(eff)) * 100
#   pourcent <- formatC(pourcent, format = "f", digits = 2)
#   pourcent <- paste(pourcent, "%")
#
#   return(pourcent)
# }
#
# format_pourcent_col <- function(tab) {
#   tab <- tab |>
#     mutate(across(where(is.numeric),
#                   ~ case_when(.x >= 5 ~ pourcentage(.x, sum(.x)),
#                               .x > 0 ~ "(<5)",
#                               .x == 0 ~ "(0)")))
#
#   return(tab)
# }
#
# get_missing <- function(df){
#   df <- df |>
#     pivot_longer(everything())
#
#   missing <- str_extract_all(df$value, "(?<=<missing\\:)\\d+") |>
#     unlist() |>
#     as.numeric()
#
#   missing <- sum(missing, na.rm = TRUE)
#
#   return(missing)
# }
#
# remove_missing <- function(df){
#   df <- df |>
#     mutate(across(where(is.character),
#                   ~ str_remove_all(.x, "<missing\\:(\\d+)?>")))
#   return(df)
# }
#
# tab_prop <- function(df, var_row, var_col) {
#   # Ajout des effectifs
#   df <- df |>
#     group_by({{var_col}}) |>
#     mutate(!!enquo(var_col) := paste0(!!enquo(var_col),
#                                       "\nN=", n())) |>
#     ungroup()
#
#   # Extraction des levels
#   level_col <-  df |>
#     pull({{var_col}}) |>
#     unique()
#
#   # Expliciter les NA
#   df <- df |>
#     mutate(!!enquo(var_row) := fct_na_value_to_level({{var_row}},
#                                                      level = "Missing"))
#
#   # Tableau de contingence
#   tab <- df |>
#     count({{var_row}}, {{var_col}}, .drop = FALSE) |>
#     filter({{var_col}} %in% level_col) |>
#     pivot_wider(
#       names_from = as_name(enquo(var_col)),
#       values_from = n,
#       values_fill = 0
#     )
#
#   # Gestion des missing
#   missing <- tab |>
#     filter({{var_row}} == "Missing") |>
#     select(-1) |>
#     rowSums()
#
#   if (length(missing) > 0) {
#     missing <- paste0("<missing:", missing, ">")
#   }
#   else {
#     missing <- "<missing:0>"
#   }
#
#   tab <- tab |>
#     filter({{var_row}} != "Missing")
#
#   # P value
#   if (ncol(tab) > 2) {
#     p.value <- format_p_val(tab)
#     col_pval <- paste(c("p-value", level_col), collapse = "")
#
#     tab <- tab |>
#       mutate(!!col_pval := c(p.value, rep(NA, nrow(tab) - 1))) |>
#       mutate(!!col_pval := ifelse(if_any(where(is.numeric),
#                                          ~ . < 5),
#                                   "—",
#                                   !!sym(col_pval)))
#   }
#
#   # Transformation en pourcentage
#   tab <- format_pourcent_col(tab)
#
#   # Ajout d'un titre
#   label <- labelled::var_label(df[[as_name(enquo(var_row))]])
#   if (is.null(label)) {
#     label <- as_name(enquo(var_row))# Si pas de label, utilise le nom de la variable
#   }
#   label <- paste0(label, missing)
#
#   tab <- bind_rows(tibble(!!!setNames(c(label, rep(NA, ncol(tab) - 1)),
#                                       names(tab))), tab)
#   tab <- rename(tab, Variable = 1)
#
#   return(tab)
# }
#
# tab_prop_group <- function(df, var_row, var_col, p_group) {
#   list_col <- df |>
#     pull({{var_col}}) |>
#     levels() |>
#     split_col(p_group)
#
#   df_list <- map(1:length(p_group), ~ df)
#
#   df_list <- map2(df_list, list_col, ~ .x |>
#                     filter({{var_col}} %in% .y))
#
#   df_list <- lapply(df_list, tab_prop, {{var_row}}, {{var_col}})
#
#   # Gestion des <missing:\\d>
#   missing <- lapply(df_list, get_missing) |>
#     unlist() |>
#     sum(na.rm = TRUE)
#
#   df_list <- lapply(df_list, remove_missing)
#   df_combined <- reduce(df_list, full_join, by = "Variable")
#   df_combined[1,1] <- paste0(df_combined[1,1], "<missing:", missing, ">")
#
#   return(df_combined)
# }
#
# bind_tab_prop <- function(df, ..., by, p_group = NULL) {
#   vars <- quos(...)
#
#   if (is.null(p_group)) {
#     list_tables <- map(vars, ~ tab_prop(df, !!.x, {{by}}))
#   }
#   else{
#     list_tables <- map(vars, ~ tab_prop_group(df, !!.x, {{by}}, {{p_group}}))
#   }
#
#   result <- bind_rows(list_tables)
#
#   return(result)
# }
#
# tab_prop_yn <- function(df, var_row, var_col){
#   # Tableau de contingence
#   tab <- df |>
#     mutate(!!enquo(var_row) := as.factor(!!enquo(var_row))) |>
#     tab_prop({{var_row}}, {{var_col}})
#
#   # Récupération des missing et p.value
#   missing <- get_missing(tab)
#
#   if(ncol(tab) > 2){
#     p.value <- tab[2, ncol(tab)]
#   }
#   else{
#     p.value <- NULL
#   }
#
#   # Selection de la valeur positive
#   tab <- tab |>
#     filter(
#       Variable == "TRUE" |
#         Variable == "Oui" |
#         Variable == "Vrai" |
#         Variable == "Yes" |
#         Variable == "True" |
#         Variable == "1"
#     )
#
#   # Ajout d'un titre
#   title <- labelled::var_label(df[[as_name(enquo(var_row))]])
#   if (is.null(title)) {
#     title <- as_name(enquo(var_row))# Si pas de label, utilise le nom de la variable
#   }
#
#   title <- paste0(title, "<missing:", missing, ">")
#   tab[1, 1] <- title
#   if(!is.null(p.value)) {
#     tab[1, ncol(tab)] <- p.value
#   }
#
#   return(tab)
# }
#
# bind_tab_prop_yn <- function(df, ..., by, title, p_group = NULL){
#   vars <- quos(...)
#
#   if (is.null(p_group)) {
#     list_tables <- map(vars, ~ tab_prop_yn(df, !!.x, {{by}}))
#   }
#   else{
#     list_tables <- map(vars, ~ tab_prop_yn_group(df, !!.x, {{by}}, {{p_group}}))
#   }
#
#   result <- bind_rows(list_tables)
#
#   missing <- get_missing(result)
#   result <- remove_missing(result)
#   title <- paste0(title, "<nonadd><missing:", missing, ">")
#
#   result <- bind_rows(tibble(!!!setNames(c(title, rep(NA, ncol(result) - 1)),
#                                          names(result))), result)
#
#   return(result)
# }
