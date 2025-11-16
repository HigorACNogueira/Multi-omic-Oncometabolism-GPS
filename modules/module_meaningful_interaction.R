# # =========================================================
# # Module: Meaningful Interactions (by Nomenclature)
# # File: modules/module_meaningful_interaction.R
# # =========================================================
# 
# # =========================== UI ==============================================
# mod_meaningful_interaction_ui <- function(id) {
#   ns <- NS(id)
#   tagList(
#     h3("Signature–interaction pair in the same tumor type"),
#     fluidRow(
#       column(
#         width = 3,
#         wellPanel(
#           selectizeInput(
#             ns("nomen_input"),
#             "Signature Nomenclature:",
#             choices = NULL, multiple = FALSE,
#             options = list(placeholder = "Type to search…")
#           ),
#           actionButton(ns("run"), "Load", class = "btn-primary"),
#           tags$hr(),
#           downloadButton(ns("dl_row"),       "Download row (.csv)"),
#           downloadButton(ns("dl_plot_png"),  "Plot (.png)"),
#           downloadButton(ns("dl_plot_pdf"),  "Plot (.pdf)"),
#           downloadButton(ns("dl_summary"),   "Summary (.txt)")
#         )
#       ),
#       column(
#         width = 9,
#         wellPanel(
#           tags$details(
#             open = NA,
#             tags$summary(tags$strong("Integrative summary")),
#             uiOutput(ns("summaryText"))
#           )
#         ),
#         wellPanel(
#           h4("Signature regulatory circuitry"),
#           plotOutput(ns("netplot"), height = "620px")
#         ),
#         wellPanel(
#           h4("Selected row"),
#           DTOutput(ns("row_table"))
#         )
#       )
#     )
#   )
# }
# 
# # ====================== HELPERS: generic utilities ===========================
# 
# # Escolhe primeira coluna válida entre candidatos
# coalesce_col <- function(row, candidates){
#   for (cn in candidates) {
#     if (cn %in% names(row)) {
#       v <- row[[cn]]
#       if (!all(is.na(v))) {
#         if (is.character(v)) {
#           if (any(stringr::str_squish(v) != "")) return(v)
#         } else {
#           return(v)
#         }
#       }
#     }
#   }
#   NA
# }
# 
# norm_cd <- function(v){
#   v <- trimws(tolower(as.character(v)))
#   v <- ifelse(v %in% c("convergent","divergent","ns"), v, NA)
#   factor(v, levels = c("convergent","divergent","ns"))
# }
# 
# mk_edge <- function(from, to, type, concordance = NA_character_){
#   from <- stringr::str_squish(as.character(from))
#   to   <- stringr::str_squish(as.character(to))
#   if (is.na(from) || is.na(to) || from == "" || to == "") return(NULL)
#   if (!is.na(concordance) && identical(concordance, "ns")) return(NULL)
#   tibble::tibble(from = from, to = to, type = type, concordance = concordance)
# }
# 
# # NÃO dividir por vírgula: preserva nomes como
# # "Valine, leucine and isoleucine degradation"
# split_multi <- function(x, delim = "\\s*[+;/|]\\s*") {
#   if (is.null(x) || all(is.na(x))) return(character(0))
#   x %>%
#     as.character() %>%
#     stringr::str_replace_all("[(){}\\[\\]]", "") %>%
#     stringr::str_replace_all("[\u200B-\u200D\uFEFF]", "") %>%
#     stringr::str_split(delim) %>%
#     purrr::pluck(1) %>%
#     purrr::map_chr(~ stringr::str_squish(.x)) %>%
#     purrr::discard(~ .x == "" | is.na(.x)) %>%
#     unique()
# }
# 
# pick_immune_node <- function(label){
#   lab <- tolower(trimws(as.character(label)))
#   if (lab %in% "hot")      return("Hot immune profile")
#   if (lab %in% "cold")     return("Cold immune profile")
#   if (lab %in% "variable") return("Variable immune profile")
#   NA_character_
# }
# 
# phenotype_from_rho <- function(phenotype_label, rho){
#   if (is.null(rho) || all(is.na(rho))) return(list(label = NA_character_, dir = NA_character_))
#   rho <- suppressWarnings(as.numeric(rho))
#   if (is.na(rho)) return(list(label = NA_character_, dir = NA_character_))
#   plab <- tolower(trimws(as.character(phenotype_label)))
#   dir  <- ifelse(rho > 0, "pos", ifelse(rho < 0, "neg", NA_character_))
#   
#   if (stringr::str_detect(plab, "stemness"))
#     return(list(label = if (rho > 0) "Lower stemness" else "Higher stemness", dir = dir))
#   if (stringr::str_detect(plab, "tumor mutational burden"))
#     return(list(label = if (rho > 0) "Higher TMB" else "Lower TMB", dir = dir))
#   if (stringr::str_detect(plab, "microsatellite instability"))
#     return(list(label = if (rho > 0) "Higher MSI" else "Lower MSI", dir = dir))
#   
#   list(label = NA_character_, dir = dir)
# }
# 
# prog_node_from_keywords <- function(text){
#   if (is.null(text) || all(is.na(text))) return(NA_character_)
#   tx <- tolower(paste(na.omit(as.character(text)), collapse = " | "))
#   if (tx == "") return(NA_character_)
#   if (stringr::str_detect(tx, "\\bprotective\\b")) return("Favorable prognosis")
#   if (stringr::str_detect(tx, "\\brisk\\b"))       return("Worse prognosis")
#   NA_character_
# }
# 
# collect_signature_text <- function(row){
#   cols <- grep("signature", names(row), ignore.case = TRUE, value = TRUE)
#   if (!length(cols)) return(NA_character_)
#   unlist(row[, cols, drop = TRUE])
# }
# 
# collect_regulator_text <- function(row){
#   cols <- grep("interaction", names(row), ignore.case = TRUE, value = TRUE)
#   if (!length(cols)) return(NA_character_)
#   unlist(row[, cols, drop = TRUE])
# }
# 
# map_side_type_to_node <- function(x){
#   x <- tolower(trimws(as.character(x)))
#   if (x %in% c("protective","low")) return("Favorable prognosis")
#   if (x %in% c("risky","high"))     return("Worse prognosis")
#   NA_character_
# }
# 
# pair_to_conc <- function(sig_type, int_type){
#   if (any(is.na(c(sig_type, int_type)))) return(NA_character_)
#   a <- tolower(sig_type); b <- tolower(int_type)
#   if ((a %in% c("protective","low")) && (b %in% c("protective","low"))) return("convergent")
#   if ((a %in% c("risky","high"))      && (b %in% c("risky","high")))     return("convergent")
#   if ((a %in% c("protective","low")) && (b %in% c("risky","high")))      return("divergent")
#   if ((a %in% c("risky","high"))      && (b %in% c("protective","low"))) return("divergent")
#   NA_character_
# }
# 
# clean_side <- function(x){
#   x <- tolower(trimws(as.character(x)))
#   if (x %in% c("ns", "", NA)) return(NA_character_)
#   x
# }
# 
# agg_to_cd <- function(x){
#   if (is.null(x) || all(is.na(x))) return(NA_character_)
#   tx <- tolower(paste(na.omit(as.character(x)), collapse = " | "))
#   if (tx == "") return(NA_character_)
#   if (grepl("\\bconvergent\\b", tx)) return("convergent")
#   if (grepl("\\bdivergent\\b", tx)) return("divergent")
#   NA_character_
# }
# 
# get_agg_concordance <- function(row){
#   cands <- c("Cox_concordance_aggregated",
#              "Final_concordance_summary",
#              "Combined_outcome_SMC")
#   vals <- lapply(cands, function(cn) if (cn %in% names(row)) row[[cn]] else NA)
#   agg_to_cd(vals)
# }
# 
# majority_concordance <- function(row){
#   ep <- list(
#     pair_to_conc(clean_side(row[["Cox_OS_type_signature"]]),
#                  clean_side(row[["Cox_OS_type_interaction"]])),
#     pair_to_conc(clean_side(row[["Cox_DSS_type_signature"]]),
#                  clean_side(row[["Cox_DSS_type_interaction"]])),
#     pair_to_conc(clean_side(row[["Cox_DFI_type_signature"]]),
#                  clean_side(row[["Cox_DFI_type_interaction"]])),
#     pair_to_conc(clean_side(row[["Cox_PFI_type_signature"]]),
#                  clean_side(row[["Cox_PFI_type_interaction"]]))
#   )
#   v <- na.omit(unlist(ep))
#   u <- unique(v)
#   if (length(v) == 0) return(NA_character_)
#   if (length(u) == 1) return(u)
#   NA_character_
# }
# 
# fallback_conc_for_interaction <- function(row){
#   conc <- get_agg_concordance(row)
#   if (!is.na(conc)) return(conc)
#   majority_concordance(row)
# }
# 
# add_prog_edges_for_endpoint <- function(edges_list, sig_type, int_type, ASSOC, NODE_SIG, regulators, row){
#   sig_type <- clean_side(sig_type)
#   int_type <- clean_side(int_type)
#   
#   sig_node <- map_side_type_to_node(sig_type)
#   int_node <- map_side_type_to_node(int_type)
#   
#   conc_ep  <- pair_to_conc(sig_type, int_type)  # tentativa por endpoint
#   
#   # 1) Aresta da ASSINATURA: mantém comportamento atual (só se conc_ep existe)
#   if (!is.na(sig_node) && !is.na(conc_ep)) {
#     edges_list <- append(edges_list, list(mk_edge(NODE_SIG, sig_node, ASSOC, conc_ep)))
#   }
#   
#   # 2) Arestas da INTERAÇÃO: se conc_ep == NA, usar fallback global
#   conc_for_int <- conc_ep
#   if (is.na(conc_for_int))
#     conc_for_int <- fallback_conc_for_interaction(row)
#   
#   # Se ainda assim for NA, não cria aresta de interação
#   if (!is.na(int_node) && !is.na(conc_for_int) && length(regulators)) {
#     edges_list <- append(
#       edges_list,
#       lapply(regulators, function(rg) mk_edge(rg, int_node, ASSOC, conc_for_int))
#     )
#   }
#   
#   edges_list
# }
# 
# # ===================== CONSTRUÇÃO DO GRAFO (1 linha) ==========================
# 
# .build_network_from_row <- function(x_row) {
#   stopifnot(nrow(x_row) == 1)
#   
#   # --- Nó central passa a ser Signatures ---
#   stopifnot("Signatures" %in% names(x_row))
#   NODE_SIG   <- unique(x_row$Signatures); stopifnot(length(NODE_SIG) == 1L)
#   NODE_SIG   <- NODE_SIG[[1]]
#   
#   # Immune (assinatura / interação)
#   IMM_sig_node <- pick_immune_node(
#     coalesce_col(x_row, c("Immune_classification_signature","Immune_classification_sig"))
#   )
#   IMM_ent_node <- pick_immune_node(
#     coalesce_col(x_row, c("Immune_classification_interaction","Immune_classification_int"))
#   )
#   
#   # Pathways da assinatura (+ MCD != Unrelated), preservando vírgulas
#   paths_sig <- split_multi(
#     coalesce_col(x_row, c("Pathways_signature","Pathways")),
#     delim = "\\s*[+;/|]\\s*"
#   )
#   mcd_sig   <- split_multi(
#     coalesce_col(x_row, c("Metabolic_cell_death_signature","Metabolic_cell_death")),
#     delim = "\\s*[+;/|]\\s*"
#   )
#   mcd_sig   <- mcd_sig[!tolower(mcd_sig) %in% "unrelated"]
#   all_sig_paths <- unique(c(paths_sig, mcd_sig))
#   
#   # Fenótipo (RHO)
#   PH_sig <- phenotype_from_rho(x_row$Phenotypic_layer_signature, x_row$rho_signature)
#   PH_ent <- phenotype_from_rho(x_row$Phenotypic_layer_signature, x_row$rho_interaction)
#   phen_conc <- if (!is.na(PH_sig$dir) && !is.na(PH_ent$dir)) {
#     if (PH_sig$dir == PH_ent$dir) "convergent" else "divergent"
#   } else NA_character_
#   
#   # Reguladores (Meaningful_interaction)
#   regulators <- split_multi(
#     coalesce_col(x_row, c("Meaningful_interaction","Nomenclature_int"))
#   )
#   if (!length(regulators)) regulators <- "(no regulator)"
#   
#   
#   ASSOC <- "Signature and Interaction association"
#   edges_row <- list()
#   
#   # (A) Reguladores -> Assinatura (Regulation)
#   edges_row <- append(
#     edges_row,
#     lapply(regulators, function(rg) mk_edge(rg, NODE_SIG, "Regulation", NA_character_))
#   )
#   
#   # (B) Assinatura/Reguladores -> Pathways/MCD (todas as vias) com "convergent"
#   if (length(all_sig_paths)) {
#     # Signature -> pathway (convergent)
#     edges_row <- append(
#       edges_row,
#       lapply(all_sig_paths, function(pth) mk_edge(NODE_SIG, pth, ASSOC, "convergent"))
#     )
#     # Regulators -> pathway (convergent)
#     if (length(regulators)) {
#       edges_row <- append(
#         edges_row,
#         purrr::flatten(
#           lapply(all_sig_paths, function(pth)
#             lapply(regulators, function(rg) mk_edge(rg, pth, ASSOC, "convergent"))
#           )
#         )
#       )
#     }
#   }
#   
#   # (D) Fenótipo (RHO)
#   if (!is.na(PH_sig$label))
#     edges_row <- append(edges_row, list(mk_edge(NODE_SIG, PH_sig$label, ASSOC, phen_conc)))
#   if (!is.na(PH_ent$label))
#     edges_row <- append(
#       edges_row,
#       lapply(regulators, function(rg) mk_edge(rg, PH_ent$label, ASSOC, phen_conc))
#     )
#   
#   # (E) Imune (ignora NS; usa Immune_concordance)
#   imm_cd <- as.character(norm_cd(x_row$Immune_concordance))
#   if (!is.na(IMM_sig_node))
#     edges_row <- append(edges_row, list(mk_edge(NODE_SIG, IMM_sig_node, ASSOC, imm_cd)))
#   if (!is.na(IMM_ent_node))
#     edges_row <- append(
#       edges_row,
#       lapply(regulators, function(rg) mk_edge(rg, IMM_ent_node, ASSOC, imm_cd))
#     )
#   
#   # (F) Keywords de prognóstico apenas se NÃO houver Cox
#   cox_sig_types <- c(
#     coalesce_col(x_row, c("Cox_OS_type_sig", "Cox_OS_type_signature")),
#     coalesce_col(x_row, c("Cox_DSS_type_sig", "Cox_DSS_type_signature")),
#     coalesce_col(x_row, c("Cox_PFI_type_sig", "Cox_PFI_type_signature")),
#     coalesce_col(x_row, c("Cox_DFI_type_sig", "Cox_DFI_type_signature"))
#   )
#   cox_int_types <- c(
#     coalesce_col(x_row, c("Cox_OS_type_int", "Cox_OS_type_interaction")),
#     coalesce_col(x_row, c("Cox_DSS_type_int", "Cox_DSS_type_interaction")),
#     coalesce_col(x_row, c("Cox_PFI_type_int", "Cox_PFI_type_interaction")),
#     coalesce_col(x_row, c("Cox_DFI_type_int", "Cox_DFI_type_interaction"))
#   )
#   has_cox <- any(!is.na(c(cox_sig_types, cox_int_types)))
#   
#   if (!has_cox) {
#     prog_sig_node <- prog_node_from_keywords(collect_signature_text(x_row))
#     prog_ent_node <- prog_node_from_keywords(collect_regulator_text(x_row))
#     prog_conc <- if (!is.na(prog_sig_node) && !is.na(prog_ent_node)) {
#       if (prog_sig_node == prog_ent_node) "convergent" else "divergent"
#     } else NA_character_
#     
#     if (!is.na(prog_conc) && !is.na(prog_sig_node))
#       edges_row <- append(edges_row, list(mk_edge(NODE_SIG, prog_sig_node, ASSOC, prog_conc)))
#     if (!is.na(prog_conc) && !is.na(prog_ent_node))
#       edges_row <- append(
#         edges_row,
#         lapply(regulators, function(rg) mk_edge(rg, prog_ent_node, ASSOC, prog_conc))
#       )
#   }
#   
#   # (G) Cox lado-específico (OS/DSS/PFI/DFI)
#   edges_row <- add_prog_edges_for_endpoint(
#     edges_row,
#     sig_type   = coalesce_col(x_row, c("Cox_OS_type_sig", "Cox_OS_type_signature")),
#     int_type   = coalesce_col(x_row, c("Cox_OS_type_int", "Cox_OS_type_interaction")),
#     ASSOC      = ASSOC,
#     NODE_SIG   = NODE_SIG,
#     regulators = regulators,
#     row        = x_row
#   )
#   edges_row <- add_prog_edges_for_endpoint(
#     edges_row,
#     sig_type   = coalesce_col(x_row, c("Cox_DSS_type_sig", "Cox_DSS_type_signature")),
#     int_type   = coalesce_col(x_row, c("Cox_DSS_type_int", "Cox_DSS_type_interaction")),
#     ASSOC      = ASSOC,
#     NODE_SIG   = NODE_SIG,
#     regulators = regulators,
#     row        = x_row
#   )
#   edges_row <- add_prog_edges_for_endpoint(
#     edges_row,
#     sig_type   = coalesce_col(x_row, c("Cox_PFI_type_sig", "Cox_PFI_type_signature")),
#     int_type   = coalesce_col(x_row, c("Cox_PFI_type_int", "Cox_PFI_type_interaction")),
#     ASSOC      = ASSOC,
#     NODE_SIG   = NODE_SIG,
#     regulators = regulators,
#     row        = x_row
#   )
#   edges_row <- add_prog_edges_for_endpoint(
#     edges_row,
#     sig_type   = coalesce_col(x_row, c("Cox_DFI_type_sig", "Cox_DFI_type_signature")),
#     int_type   = coalesce_col(x_row, c("Cox_DFI_type_int", "Cox_DFI_type_interaction")),
#     ASSOC      = ASSOC,
#     NODE_SIG   = NODE_SIG,
#     regulators = regulators,
#     row        = x_row
#   )
#   
#   # Unifica arestas de todas as "linhas" (aqui só 1)
#   edges <- bind_rows(compact(edges_row)) %>%
#     filter(!is.na(to), !is.na(from)) %>%
#     mutate(
#       type = stringr::str_squish(as.character(type)),
#       type = ifelse(type == "Regulation", "Regulation", "Signature and Interaction association"),
#       type = factor(type, levels = c("Regulation","Signature and Interaction association")),
#       concordance = factor(concordance, levels = c("convergent","divergent"))
#     ) %>%
#     distinct(from, to, type, concordance, .keep_all = TRUE)
#   
#   # Canal estético conc_plot
#   edges_for_plot <- edges %>%
#     mutate(
#       conc_plot = dplyr::case_when(
#         as.character(type) == "Regulation" ~ "regulation",
#         from == NODE_SIG                   ~ "sig_assoc",
#         is.na(concordance)                 ~ "association",
#         TRUE                               ~ as.character(concordance)
#       )
#     )
#   
#   # Paleta Okabe-Ito adaptada
#   layer_colors <- c(
#     "Interaction"          = "#D55E00",
#     "Signature"            = "#009E73",
#     "Biological processes" = "#0072B2",
#     "Immune phenotype"     = "#D0AD00",
#     "Tumor phenotype"      = "#CC79A7",
#     "Prognosis"            = "#000000"
#   )
#   
#   edge_colours <- c(
#     "sig_assoc"   = "#000000",
#     "convergent"  = "#009E73",
#     "divergent"   = "#C0392B",
#     "association" = "#A9A9A9",
#     "regulation"  = "#D55E00"
#   )
#   
#   etype_lty <- c(
#     "Regulation"                            = "dashed",
#     "Signature and Interaction association" = "solid"
#   )
#   etype_w   <- c(
#     "Regulation"                            = 1.5,
#     "Signature and Interaction association" = 1.5
#   )
#   
#   # Layer dos nós
#   layer_of <- function(name){
#     if (isTRUE(name %in% regulators)) return("Interaction")
#     if (name %in% c(NODE_SIG))       return("Signature")
#     if (grepl("stemness|tmb|msi", tolower(name))) return("Tumor phenotype")
#     if (grepl("immune profile|microenvironment", tolower(name))) return("Immune phenotype")
#     if (name %in% c("Favorable prognosis","Worse prognosis"))   return("Prognosis")
#     "Biological processes"
#   }
#   
#   nodes <- tibble(name = unique(c(edges$from, edges$to))) %>%
#     mutate(layer = vapply(name, layer_of, character(1)))
#   
#   # Tamanho de ponto diferenciado
#   nodes <- nodes %>%
#     mutate(
#       pt_size = dplyr::case_when(
#         layer %in% c("Interaction","Signature") ~ 18,
#         TRUE                                    ~ 8
#       )
#     )
#   
#   # Grafo e plot com layout "circle" (mantido)
#   g <- tidygraph::tbl_graph(nodes = nodes, edges = edges_for_plot, directed = TRUE)
#   
#   set.seed(123)
#   p <- ggraph(g, layout = "circle") +
#     ggraph::geom_edge_arc(
#       aes(edge_colour = conc_plot, edge_linetype = type, edge_width = type),
#       arrow      = grid::arrow(length = grid::unit(2.5, "mm"), type = "closed"),
#       strength   = 0.5,
#       edge_alpha = 1.0,
#       start_cap  = ggraph::circle(6, "mm"),
#       end_cap    = ggraph::circle(6, "mm")
#     ) +
#     ggraph::geom_node_point(
#       aes(fill = layer, size = pt_size),
#       shape = 21,
#       color = "white"
#     ) +
#     scale_size_identity() +
#     # Labels: Signature/Interaction acima; periféricos abaixo
#     ggraph::geom_node_text(
#       data = function(d) d %>% dplyr::filter(layer %in% c("Signature","Interaction")),
#       aes(label = name, color = layer),
#       nudge_y  = 0.35,
#       size = 9,
#       fontface = "bold"
#     ) +
#     ggraph::geom_node_text(
#       data = function(d) d %>% dplyr::filter(layer %in% c("Biological processes","Tumor phenotype","Immune phenotype","Prognosis")),
#       aes(label = name, color = layer),
#       nudge_y  = -0.30,
#       size = 8,
#       fontface = "bold"
#     ) +
#     scale_color_manual(values = layer_colors, guide = "none") +
#     scale_fill_manual(
#       values = layer_colors,
#       name   = "Dimension",
#       guide  = guide_legend(override.aes = list(size = 12))
#     ) +
#     ggraph::scale_edge_colour_manual(
#       values = edge_colours,
#       name   = "Interpretation",
#       breaks = c("sig_assoc","convergent","divergent"),
#       labels = c("Signature association",
#                  "Convergent Sig-Int association",
#                  "Divergent Sig-Int association")
#     ) +
#     ggraph::scale_edge_width_manual(values = etype_w, guide = "none") +
#     ggraph::scale_edge_linetype_manual(values = etype_lty, name = "Relation type") +
#     theme_void() +
#     coord_equal(expand = FALSE, clip = "off") +
#     theme(
#       legend.title = element_text(size = 16, face = "bold"),
#       legend.text  = element_text(size = 16),
#       legend.position = "left",
#       legend.box.margin = margin(r=10, l=10, t=10, b=10),
#       plot.margin = margin(t=20, r=20, b=20, l=20)
#     )
#   
#   list(plot = p, nodes = nodes, edges = edges_for_plot)
# }
# 
# # =========================== SUMMARY BUILDER ==================================
# .build_summary_html <- function(row, tcga_types = NULL) {
#   pick <- function(nm) if (nm %in% names(row)) row[[nm]][1] else NA
#   
#   cancer_abbr <- pick("CTAB")
#   cancer_full <- cancer_abbr
#   if (!is.null(tcga_types) && all(c("Cancer_abbreviation","Cancer_names") %in% names(tcga_types))) {
#     hit <- tcga_types$Cancer_names[match(cancer_abbr, tcga_types$Cancer_abbreviation)]
#     if (!is.na(hit) && nzchar(hit)) cancer_full <- hit
#   }
#   
#   nom   <- pick("Nomenclature")
#   sig   <- pick("Signatures")
#   met   <- pick("Metabolism")
#   path  <- pick("Pathways")
#   mcd   <- pick("Metabolic_cell_death")
#   mcd_txt <- if (!is.na(mcd) && nzchar(mcd) && tolower(mcd) != "unrelated")
#     glue("<p>This signature is associated with <b>{mcd}</b>.</p>") else ""
#   
#   inter <- pick("Meaningful_interaction")
#   inter_sentence <- if (!is.na(inter) && nzchar(inter) && tolower(inter) != "no meaningful interaction")
#     glue("<p>Clinically meaningful interaction(s): <b>{inter}</b>.</p>") else ""
#   
#   rho_sig  <- suppressWarnings(as.numeric(pick("rho_signature")))
#   rho_int  <- suppressWarnings(as.numeric(pick("rho_interaction")))
#   phen_sig <- pick("Phenotypic_layer_signature")
#   phen_int <- pick("Phenotypic_layer_interaction")
#   omic_sig <- pick("Omic_layer_signature")
#   omic_int <- pick("Omic_layer_interaction")
#   
#   fmt_rho <- function(r) if (!is.na(r)) sprintf("%.3f", r) else "NA"
#   
#   corr_sig <- if (!is.na(phen_sig) && nzchar(phen_sig))
#     glue("<p>Signature side: correlation between <b>{omic_sig}</b> and <b>{phen_sig}</b> (ρ = {fmt_rho(rho_sig)}).</p>")
#   else ""
#   
#   corr_int <- if (!is.na(phen_int) && nzchar(phen_int))
#     glue("<p>Interaction side: correlation between <b>{omic_int}</b> and <b>{phen_int}</b> (ρ = {fmt_rho(rho_int)}).</p>")
#   else ""
#   
#   phen_cd <- pick("Phenotypic_concordance") %>%
#     as.character() %>% {ifelse(. %in% c("convergent","divergent"), ., NA)}
#   phen_line <- if (!is.na(phen_cd)) glue("<p>Phenotypic concordance: <b>{phen_cd}</b>.</p>") else ""
#   
#   imm_sig <- pick("Immune_classification_signature")
#   imm_int <- pick("Immune_classification_interaction")
#   imm_cd  <- pick("Immune_concordance") %>% as.character()
#   imm_lines <- if (!is.na(imm_cd) && imm_cd %in% c("convergent","divergent")) {
#     glue("<p>Immune: signature=<b>{imm_sig}</b>, interaction=<b>{imm_int}</b>, concordance=<b>{imm_cd}</b>.</p>")
#   } else ""
#   
#   cox_block <- {
#     items <- list()
#     add <- function(lbl, sig_t, int_t, cd) {
#       if (!is.na(sig_t) && tolower(sig_t) == "ns" &&
#           !is.na(int_t) && tolower(int_t) == "ns") return(NULL)
#       cd_txt <- if (!is.na(cd) && cd %in% c("convergent","divergent")) glue(" (concordance: {cd})") else ""
#       items <<- append(items, glue("{lbl}: signature={sig_t}, interaction={int_t}{cd_txt}"))
#       NULL
#     }
#     add("OS",  pick("Cox_OS_type_signature"),  pick("Cox_OS_type_interaction"),  pick("Cox_concordance_OS"))
#     add("DSS", pick("Cox_DSS_type_signature"), pick("Cox_DSS_type_interaction"), pick("Cox_concordance_DSS"))
#     add("DFI", pick("Cox_DFI_type_signature"), pick("Cox_DFI_type_interaction"), pick("Cox_concordance_DFI"))
#     add("PFI", pick("Cox_PFI_type_signature"), pick("Cox_PFI_type_interaction"), pick("Cox_concordance_PFI"))
#     if (length(items)) glue("<p>Cox regression: <b>{paste(items, collapse = '; ')}</b>.</p>") else ""
#   }
#   
#   surv_block <- {
#     items <- list()
#     add <- function(lbl, sig_t, int_t, cd) {
#       if (is.na(sig_t) && is.na(int_t)) return(NULL)
#       cd_txt <- if (!is.na(cd) && cd %in% c("convergent","divergent")) glue(" (concordance: {cd})") else ""
#       items <<- append(items, glue("{lbl}: signature={sig_t}, interaction={int_t}{cd_txt}"))
#       NULL
#     }
#     add("OS",  pick("OS_worst_prognosis_group_signature"),  pick("OS_worst_prognosis_group_interaction"),  pick("Survival_concordance_OS"))
#     add("DSS", pick("DSS_worst_prognosis_group_signature"), pick("DSS_worst_prognosis_group_interaction"), pick("Survival_concordance_DSS"))
#     add("DFI", pick("DFI_worst_prognosis_group_signature"), pick("DFI_worst_prognosis_group_interaction"), pick("Survival_concordance_DFI"))
#     add("PFI", pick("PFI_worst_prognosis_group_signature"), pick("PFI_worst_prognosis_group_interaction"), pick("Survival_concordance_PFI"))
#     if (length(items)) glue("<p>Survival analysis: <b>{paste(items, collapse = '; ')}</b>.</p>") else ""
#   }
#   
#   intro <- glue(
#     "<p>The signature <b>{nom}</b> (components: <b>{sig}</b>) was identified in <b>{cancer_full}</b>, ",
#     "involved in <b>{met}</b> and <b>{path}</b>.</p>"
#   )
#   
#   HTML(paste0(
#     intro,
#     inter_sentence,
#     mcd_txt,
#     corr_sig,
#     corr_int,
#     phen_line,
#     imm_lines,
#     cox_block,
#     surv_block,
#     if ("Final_concordance_summary" %in% names(row) && nzchar(as.character(row$Final_concordance_summary)))
#       glue("<p>Overall concordance: <b>{row$Final_concordance_summary}</b>.</p>") else "",
#     "<p><i>Use the plot for topology; export the row for full statistics.</i></p>"
#   ))
# }
# 
# # ============================= SERVER =========================================
# mod_meaningful_interaction_server <- function(id, dataset, tcga_types = NULL) {
#   moduleServer(id, function(input, output, session) {
#     ns <- session$ns
#     
#     # Popular Nomenclature
#     observe({
#       req(dataset())
#       df <- dataset()
#       validate(need("Nomenclature" %in% names(df), "Dataset must contain 'Nomenclature'."))
#       ch <- sort(unique(na.omit(trimws(as.character(df$Nomenclature)))))
#       updateSelectizeInput(session, "nomen_input", choices = ch, server = TRUE)
#     })
#     
#     # Linha escolhida pela Nomenclature
#     # Todas as linhas para a Nomenclature escolhida
#     matches_all <- eventReactive(input$run, {
#       df <- dataset(); req(df, input$nomen_input)
#       hit <- df %>% dplyr::filter(.data$Nomenclature == input$nomen_input)
#       validate(need(nrow(hit) > 0, "Nomenclature not found."))
#       
#       # Garantia: mesma assinatura em todas as linhas (caso contrário, avisa)
#       if ("Signatures" %in% names(hit)) {
#         stopifnot(length(unique(hit$Signatures)) == 1L)
#       }
#       hit
#     }, ignoreInit = TRUE)
#     
#     # Summary
#     output$summaryText <- renderUI({
#       row <- selected_row(); req(row)
#       .build_summary_html(row, tcga_types = tcga_types)
#     })
#     
#     # Plot (nova lógica)
#     net_obj <- reactive({
#       row <- chosen_row(); req(row)
#       .build_network_from_row(row)
#     })
#     
#     output$netplot <- renderPlot({
#       obj <- net_obj(); req(obj$plot)
#       obj$plot
#     }, res = 120)
#     
#     # Tabela da linha
#     output$row_table <- renderDT({
#       row <- chosen_row(); req(row)
#       datatable(row, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 5))
#     })
#     
#     # Downloads
#     output$dl_row <- downloadHandler(
#       filename = function() paste0(
#         "meaningful_interaction_row_",
#         gsub("[^A-Za-z0-9]+","_", input$nomen_input %||% "signature"), ".csv"
#       ),
#       content = function(file) {
#         readr::write_csv(selected_row(), file)
#       }
#     )
#     
#     
#     output$dl_plot_png <- downloadHandler(
#       filename = function() paste0("meaningful_interaction_plot_",
#                                    gsub("[^A-Za-z0-9]+","_",
#                                         input$nomen_input %||% "signature"), ".png"),
#       content = function(file) {
#         obj <- net_obj(); req(obj$plot)
#         ggsave(file, obj$plot, width = 14, height = 8, dpi = 300, bg = "white")
#       }
#     )
#     
#     output$dl_plot_pdf <- downloadHandler(
#       filename = function() paste0("meaningful_interaction_plot_",
#                                    gsub("[^A-Za-z0-9]+","_",
#                                         input$nomen_input %||% "signature"), ".pdf"),
#       content = function(file) {
#         obj <- net_obj(); req(obj$plot)
#         ggsave(file, obj$plot, width = 14, height = 8, device = cairo_pdf, bg = "white")
#       }
#     )
#     
#     output$dl_summary <- downloadHandler(
#       filename = function() paste0("meaningful_interaction_summary_",
#                                    gsub("[^A-Za-z0-9]+","_",
#                                         input$nomen_input %||% "signature"), ".txt"),
#       content = function(file) {
#         html <- as.character(isolate(.build_summary_html(chosen_row(), tcga_types = tcga_types)))
#         txt  <- gsub("<[^>]+>", "", html)
#         writeLines(txt, file, useBytes = TRUE)
#       }
#     )
#   })
# }
# 


# =========================================================
# Module: Meaningful Interactions (Nomenclature-centric)
# File: modules/module_meaningful_interaction.R
# =========================================================


`%||%` <- function(a, b) if (!is.null(a)) a else b

# ------------------------- HELPERS GERAIS -----------------------------

coalesce_col <- function(row, candidates){
  for (cn in candidates) {
    if (cn %in% names(row)) {
      v <- row[[cn]]
      if (!all(is.na(v))) {
        if (is.character(v)) {
          if (any(stringr::str_squish(v) != "")) return(v)
        } else return(v)
      }
    }
  }
  NA
}

norm_cd <- function(v){
  v <- trimws(tolower(as.character(v)))
  v <- ifelse(v %in% c("convergent","divergent","ns"), v, NA)
  factor(v, levels = c("convergent","divergent","ns"))
}

mk_edge <- function(from, to, type, concordance = NA_character_){
  from <- stringr::str_squish(as.character(from))
  to   <- stringr::str_squish(as.character(to))
  if (is.na(from) || is.na(to) || from == "" || to == "") return(NULL)
  if (!is.na(concordance) && identical(concordance, "ns")) return(NULL)
  tibble::tibble(from = from, to = to, type = type, concordance = concordance)
}

# NÃO dividir por vírgula para preservar nomes como
# "Valine, leucine and isoleucine degradation"
split_multi <- function(x, delim = "\\s*[+;/|]\\s*") {
  if (is.null(x) || all(is.na(x))) return(character(0))
  x %>%
    as.character() %>%
    stringr::str_replace_all("[(){}\\[\\]]", "") %>%
    stringr::str_replace_all("[\u200B-\u200D\uFEFF]", "") %>%
    stringr::str_split(delim) %>%
    purrr::pluck(1) %>%
    purrr::map_chr(~ stringr::str_squish(.x)) %>%
    purrr::discard(~ .x == "" | is.na(.x)) %>%
    unique()
}

pick_immune_node <- function(label){
  lab <- tolower(trimws(as.character(label)))
  if (lab %in% "hot")      return("Hot immune profile")
  if (lab %in% "cold")     return("Cold immune profile")
  if (lab %in% "variable") return("Variable immune profile")
  NA_character_
}

phenotype_from_rho <- function(phenotype_label, rho){
  if (is.na(rho)) return(list(label = NA_character_, dir = NA_character_))
  rho <- suppressWarnings(as.numeric(rho))
  if (is.na(rho)) return(list(label = NA_character_, dir = NA_character_))
  plab <- tolower(trimws(as.character(phenotype_label)))
  dir  <- ifelse(rho > 0, "pos", ifelse(rho < 0, "neg", NA_character_))
  if (stringr::str_detect(plab, "stemness"))
    return(list(label = if (rho > 0) "Lower stemness" else "Higher stemness", dir = dir))
  if (stringr::str_detect(plab, "tumor mutational burden"))
    return(list(label = if (rho > 0) "Higher TMB" else "Lower TMB", dir = dir))
  if (stringr::str_detect(plab, "microsatellite instability"))
    return(list(label = if (rho > 0) "Higher MSI" else "Lower MSI", dir = dir))
  list(label = NA_character_, dir = dir)
}

prog_node_from_keywords <- function(text){
  if (is.null(text) || all(is.na(text))) return(NA_character_)
  tx <- tolower(paste(na.omit(as.character(text)), collapse = " | "))
  if (tx == "") return(NA_character_)
  if (stringr::str_detect(tx, "\\bprotective\\b")) return("Favorable prognosis")
  if (stringr::str_detect(tx, "\\brisk\\b"))       return("Worse prognosis")
  NA_character_
}

collect_signature_text <- function(row){
  cols <- grep("signature", names(row), ignore.case = TRUE, value = TRUE)
  if (!length(cols)) return(NA_character_)
  unlist(row[, cols, drop = TRUE])
}

collect_regulator_text <- function(row){
  cols <- grep("interaction", names(row), ignore.case = TRUE, value = TRUE)
  if (!length(cols)) return(NA_character_)
  unlist(row[, cols, drop = TRUE])
}

map_side_type_to_node <- function(x){
  x <- tolower(trimws(as.character(x)))
  if (x %in% c("protective","low")) return("Favorable prognosis")
  if (x %in% c("risky","high"))     return("Worse prognosis")
  NA_character_
}

pair_to_conc <- function(sig_type, int_type){
  if (any(is.na(c(sig_type, int_type)))) return(NA_character_)
  a <- tolower(sig_type); b <- tolower(int_type)
  if ((a %in% c("protective","low")) && (b %in% c("protective","low"))) return("convergent")
  if ((a %in% c("risky","high"))      && (b %in% c("risky","high")))     return("convergent")
  if ((a %in% c("protective","low")) && (b %in% c("risky","high")))      return("divergent")
  if ((a %in% c("risky","high"))      && (b %in% c("protective","low"))) return("divergent")
  NA_character_
}

clean_side <- function(x){
  x <- tolower(trimws(as.character(x)))
  if (x %in% c("ns", "", NA)) return(NA_character_)
  x
}

agg_to_cd <- function(x){
  if (is.null(x) || all(is.na(x))) return(NA_character_)
  tx <- tolower(paste(na.omit(as.character(x)), collapse = " | "))
  if (tx == "") return(NA_character_)
  if (grepl("\\bconvergent\\b", tx)) return("convergent")
  if (grepl("\\bdivergent\\b", tx)) return("divergent")
  NA_character_
}

get_agg_concordance <- function(row){
  cands <- c("Cox_concordance_aggregated",
             "Final_concordance_summary",
             "Combined_outcome_SMC")
  vals <- lapply(cands, function(cn) if (cn %in% names(row)) row[[cn]] else NA)
  agg_to_cd(vals)
}

majority_concordance <- function(row){
  ep <- list(
    pair_to_conc(clean_side(row[["Cox_OS_type_signature"]]),
                 clean_side(row[["Cox_OS_type_interaction"]])),
    pair_to_conc(clean_side(row[["Cox_DSS_type_signature"]]),
                 clean_side(row[["Cox_DSS_type_interaction"]])),
    pair_to_conc(clean_side(row[["Cox_DFI_type_signature"]]),
                 clean_side(row[["Cox_DFI_type_interaction"]])),
    pair_to_conc(clean_side(row[["Cox_PFI_type_signature"]]),
                 clean_side(row[["Cox_PFI_type_interaction"]]))
  )
  v <- na.omit(unlist(ep))
  u <- unique(v)
  if (length(v) == 0) return(NA_character_)
  if (length(u) == 1) return(u)
  NA_character_
}

fallback_conc_for_interaction <- function(row){
  conc <- get_agg_concordance(row)
  if (!is.na(conc)) return(conc)
  majority_concordance(row)
}

add_prog_edges_for_endpoint <- function(edges_list, sig_type, int_type,
                                        ASSOC, NODE_SIG, regulators, row){
  sig_type <- clean_side(sig_type)
  int_type <- clean_side(int_type)
  
  sig_node <- map_side_type_to_node(sig_type)
  int_node <- map_side_type_to_node(int_type)
  
  conc_ep  <- pair_to_conc(sig_type, int_type)
  
  # 1) Aresta da assinatura (só se conc_ep existe)
  if (!is.na(sig_node) && !is.na(conc_ep))
    edges_list <- append(edges_list, list(mk_edge(NODE_SIG, sig_node, ASSOC, conc_ep)))
  
  # 2) Arestas da interação: se conc_ep == NA, usar fallback global
  conc_for_int <- conc_ep
  if (is.na(conc_for_int))
    conc_for_int <- fallback_conc_for_interaction(row)
  
  if (!is.na(int_node) && !is.na(conc_for_int) && length(regulators))
    edges_list <- append(
      edges_list,
      lapply(regulators, function(rg) mk_edge(rg, int_node, ASSOC, conc_for_int))
    )
  
  edges_list
}

# -------------------- ARESTAS PARA UMA ÚNICA LINHA ---------------------

build_edges_from_row <- function(x_row, NODE_SIG){
  # Immune (assinatura / interação)
  IMM_sig_node <- pick_immune_node(
    coalesce_col(x_row, c("Immune_classification_signature","Immune_classification_sig"))
  )
  IMM_ent_node <- pick_immune_node(
    coalesce_col(x_row, c("Immune_classification_interaction","Immune_classification_int"))
  )
  
  # Pathways da assinatura (+ MCD != Unrelated) — preservando vírgulas
  paths_sig <- split_multi(
    coalesce_col(x_row, c("Pathways_signature","Pathways")),
    delim = "\\s*[+;/|]\\s*"
  )
  mcd_sig   <- split_multi(
    coalesce_col(x_row, c("Metabolic_cell_death_signature","Metabolic_cell_death")),
    delim = "\\s*[+;/|]\\s*"
  )
  mcd_sig   <- mcd_sig[!tolower(mcd_sig) %in% "unrelated"]
  all_sig_paths <- unique(c(paths_sig, mcd_sig))
  
  # Fenótipo (RHO)
  PH_sig <- phenotype_from_rho(x_row$Phenotypic_layer_signature, x_row$rho_signature)
  PH_ent <- phenotype_from_rho(x_row$Phenotypic_layer_signature, x_row$rho_interaction)
  phen_conc <- if (!is.na(PH_sig$dir) && !is.na(PH_ent$dir)) {
    if (PH_sig$dir == PH_ent$dir) "convergent" else "divergent"
  } else NA_character_
  
  # Reguladores
  regulators <- split_multi(coalesce_col(x_row, c("Meaningful_interaction", "Nomenclature_int")))
  if (!length(regulators)) regulators <- "(no regulator)"
  
  ASSOC <- "Signature and Interaction association"
  edges_row <- list()
  
  # (A) Reguladores -> assinatura
  edges_row <- append(
    edges_row,
    lapply(regulators, function(rg) mk_edge(rg, NODE_SIG, "Regulation", NA_character_))
  )
  
  # (B) Assinatura/Reguladores -> TODAS as pathways (+ MCD)
  if (length(all_sig_paths)) {
    # Signature -> pathway (tratamos como "convergent" na lógica, mas no plot
    # sairá com cor "sig_assoc" pelo conc_plot)
    edges_row <- append(
      edges_row,
      lapply(all_sig_paths, function(pth) mk_edge(NODE_SIG, pth, ASSOC, "convergent"))
    )
    # Regulators -> pathway (convergent)
    if (length(regulators)) {
      edges_row <- append(
        edges_row,
        purrr::flatten(
          lapply(all_sig_paths, function(pth)
            lapply(regulators, function(rg) mk_edge(rg, pth, ASSOC, "convergent"))
          )
        )
      )
    }
  }
  
  # (D) Fenótipo (RHO)
  if (!is.na(PH_sig$label))
    edges_row <- append(edges_row, list(mk_edge(NODE_SIG, PH_sig$label, ASSOC, phen_conc)))
  if (!is.na(PH_ent$label))
    edges_row <- append(edges_row, lapply(regulators, function(rg) mk_edge(rg, PH_ent$label, ASSOC, phen_conc)))
  
  # (E) Imune (ignora NS)
  if (!is.na(IMM_sig_node))
    edges_row <- append(
      edges_row,
      list(mk_edge(
        NODE_SIG, IMM_sig_node, ASSOC,
        as.character(norm_cd(x_row$Immune_concordance))
      ))
    )
  if (!is.na(IMM_ent_node))
    edges_row <- append(
      edges_row,
      lapply(
        regulators,
        function(rg) mk_edge(
          rg, IMM_ent_node, ASSOC,
          as.character(norm_cd(x_row$Immune_concordance))
        )
      )
    )
  
  # (F) Keywords só se NÃO houver Cox
  cox_sig_types <- c(
    coalesce_col(x_row, c("Cox_OS_type_sig", "Cox_OS_type_signature")),
    coalesce_col(x_row, c("Cox_DSS_type_sig", "Cox_DSS_type_signature")),
    coalesce_col(x_row, c("Cox_PFI_type_sig", "Cox_PFI_type_signature")),
    coalesce_col(x_row, c("Cox_DFI_type_sig", "Cox_DFI_type_signature"))
  )
  cox_int_types <- c(
    coalesce_col(x_row, c("Cox_OS_type_int", "Cox_OS_type_interaction")),
    coalesce_col(x_row, c("Cox_DSS_type_int", "Cox_DSS_type_interaction")),
    coalesce_col(x_row, c("Cox_PFI_type_int", "Cox_PFI_type_interaction")),
    coalesce_col(x_row, c("Cox_DFI_type_int", "Cox_DFI_type_interaction"))
  )
  has_cox <- any(!is.na(c(cox_sig_types, cox_int_types)))
  if (!has_cox){
    prog_sig_node <- prog_node_from_keywords(collect_signature_text(x_row))
    prog_ent_node <- prog_node_from_keywords(collect_regulator_text(x_row))
    prog_conc <- if (!is.na(prog_sig_node) && !is.na(prog_ent_node)) {
      if (prog_sig_node == prog_ent_node) "convergent" else "divergent"
    } else NA_character_
    
    if (!is.na(prog_conc) && !is.na(prog_sig_node))
      edges_row <- append(edges_row, list(mk_edge(NODE_SIG, prog_sig_node, ASSOC, prog_conc)))
    if (!is.na(prog_conc) && !is.na(prog_ent_node))
      edges_row <- append(edges_row, lapply(regulators, function(rg) mk_edge(rg, prog_ent_node, ASSOC, prog_conc)))
  }
  
  # (G) Cox lado-específico (OS/DSS/PFI/DFI)
  edges_row <- add_prog_edges_for_endpoint(
    edges_row,
    sig_type = coalesce_col(x_row, c("Cox_OS_type_sig", "Cox_OS_type_signature")),
    int_type = coalesce_col(x_row, c("Cox_OS_type_int", "Cox_OS_type_interaction")),
    ASSOC    = ASSOC,
    NODE_SIG = NODE_SIG,
    regulators = regulators,
    row = x_row
  )
  edges_row <- add_prog_edges_for_endpoint(
    edges_row,
    sig_type = coalesce_col(x_row, c("Cox_DSS_type_sig", "Cox_DSS_type_signature")),
    int_type = coalesce_col(x_row, c("Cox_DSS_type_int", "Cox_DSS_type_interaction")),
    ASSOC    = ASSOC,
    NODE_SIG = NODE_SIG,
    regulators = regulators,
    row = x_row
  )
  edges_row <- add_prog_edges_for_endpoint(
    edges_row,
    sig_type = coalesce_col(x_row, c("Cox_PFI_type_sig", "Cox_PFI_type_signature")),
    int_type = coalesce_col(x_row, c("Cox_PFI_type_int", "Cox_PFI_type_interaction")),
    ASSOC    = ASSOC,
    NODE_SIG = NODE_SIG,
    regulators = regulators,
    row = x_row
  )
  edges_row <- add_prog_edges_for_endpoint(
    edges_row,
    sig_type = coalesce_col(x_row, c("Cox_DFI_type_sig", "Cox_DFI_type_signature")),
    int_type = coalesce_col(x_row, c("Cox_DFI_type_int", "Cox_DFI_type_interaction")),
    ASSOC    = ASSOC,
    NODE_SIG = NODE_SIG,
    regulators = regulators,
    row = x_row
  )
  
  dplyr::bind_rows(purrr::compact(edges_row))
}

# -------------------- CONSTRUÇÃO DO GRAFO COMPLETO ---------------------

build_network_from_matches <- function(matches){
  stopifnot(nrow(matches) >= 1)
  stopifnot("Signatures"   %in% names(matches))
  stopifnot("Nomenclature" %in% names(matches))
  
  NODE_SIG <- unique(matches$Signatures); stopifnot(length(NODE_SIG) == 1L)
  NODE_SIG <- NODE_SIG[[1]]
  
  # Todas as arestas para todas as linhas dessa Nomenclature
  edges <- purrr::map_dfr(
    seq_len(nrow(matches)),
    function(i) build_edges_from_row(matches[i, , drop = FALSE], NODE_SIG = NODE_SIG)
  ) %>%
    dplyr::filter(!is.na(to), !is.na(from)) %>%
    dplyr::mutate(
      type = stringr::str_squish(as.character(type)),
      type = ifelse(type == "Regulation", "Regulation", "Signature and Interaction association"),
      type = factor(type, levels = c("Regulation","Signature and Interaction association")),
      concordance = factor(concordance, levels = c("convergent","divergent"))
    ) %>%
    dplyr::distinct(from, to, type, concordance, .keep_all = TRUE)
  
  # Paleta de cores por camada (Okabe-Ito adaptada)
  layer_colors <- c(
    "Interaction"          = "#D55E00",  # orange
    "Signature"            = "#009E73",  # bluish green
    "Biological processes" = "#0072B2",  # blue
    "Immune phenotype"     = "#D0AD00",  # yellow-like
    "Tumor phenotype"      = "#CC79A7",  # reddish purple
    "Prognosis"            = "#000000"   # black
  )
  
  # Força rotulagem visual: conc_plot
  edge_colours <- c(
    "sig_assoc"   = "#000000",  # assinatura -> outros
    "convergent"  = "#009E73",  # verde (convergente)
    "divergent"   = "#C0392B",  # vermelho (divergente)
    "association" = "#A9A9A9",  # cinza claro
    "regulation"  = "#D55E00"   # laranja (regulação)
  )
  
  edges_for_plot <- edges %>%
    dplyr::mutate(
      conc_plot = dplyr::case_when(
        as.character(type) == "Regulation" ~ "regulation",
        from == NODE_SIG                   ~ "sig_assoc",
        is.na(concordance)                 ~ "association",
        TRUE                               ~ as.character(concordance)
      )
    )
  
  # Todos os reguladores (para classificação de camada)
  regs_all <- unique(unlist(purrr::map(
    seq_len(nrow(matches)),
    function(i) split_multi(matches$Meaningful_interaction[i])
  )))
  
  layer_of <- function(name){
    if (isTRUE(name %in% regs_all)) return("Interaction")
    if (name %in% c(NODE_SIG)) return("Signature")
    if (grepl("stemness|tmb|msi", tolower(name))) return("Tumor phenotype")
    if (grepl("immune profile|microenvironment", tolower(name))) return("Immune phenotype")
    if (name %in% c("Favorable prognosis","Worse prognosis")) return("Prognosis")
    "Biological processes"
  }
  
  nodes <- tibble::tibble(name = unique(c(edges$from, edges$to))) %>%
    dplyr::mutate(layer = vapply(name, layer_of, character(1)))
  
  # Tamanho diferenciado para assinatura/interação
  nodes <- nodes %>%
    dplyr::mutate(
      pt_size = dplyr::case_when(
        layer %in% c("Interaction", "Signature") ~ 12,
        TRUE                                     ~ 8
      )
    )
  
  g <- tidygraph::tbl_graph(nodes = nodes, edges = edges_for_plot, directed = TRUE)
  
  etype_lty <- c(
    "Regulation"                            = "dashed",
    "Signature and Interaction association" = "solid"
  )
  etype_w   <- c(
    "Regulation"                            = 1.0,
    "Signature and Interaction association" = 1.0
  )
  
  # Plot com layout circular (mantido)
  set.seed(123)
  p <- ggraph::ggraph(g, layout = "circle") +
    ggraph::geom_edge_arc(
      ggplot2::aes(edge_colour = conc_plot, edge_linetype = type, edge_width = type),
      arrow      = grid::arrow(length = grid::unit(2.0, "mm"), type = "closed"),
      strength   = 0.3,
      edge_alpha = 0.5,
      start_cap  = ggraph::circle(6, "mm"),
      end_cap    = ggraph::circle(6, "mm")
    ) +
    ggraph::geom_node_point(
      ggplot2::aes(fill = layer, size = pt_size),
      shape = 21,
      color = "white"
    ) +
    ggplot2::scale_size_identity() +
    
    # Labels – assinatura e interações um pouco maiores
    ggraph::geom_node_text(
      data = function(d) d %>% dplyr::filter(layer %in% c("Signature","Interaction")),
      ggplot2::aes(label = name, color = layer),
      nudge_y  = 0.15,
      size = 3.0,
      fontface = "bold"
    ) +
    ggraph::geom_node_text(
      data = function(d) d %>%
        dplyr::filter(layer %in% c("Biological processes","Tumor phenotype","Immune phenotype","Prognosis")),
      ggplot2::aes(label = name, color = layer),
      nudge_y  = -0.15,
      size = 3.0,
      fontface = "bold"
    ) +
    ggplot2::scale_color_manual(values = layer_colors, guide = "none") +
    
    ggplot2::scale_fill_manual(
      values = layer_colors,
      name = "Dimension",
      guide = ggplot2::guide_legend(override.aes = list(size = 7))
    ) +
    ggraph::scale_edge_colour_manual(
      values = edge_colours,
      name   = "Interpretation",
      breaks = c("sig_assoc","convergent","divergent"),
      labels = c("Signature association","Convergent Sig-Int association","Divergent Sig-Int association")
    ) +
    ggraph::scale_edge_width_manual(values = etype_w, guide = "none") +
    ggraph::scale_edge_linetype_manual(values = etype_lty, name = "Relation type") +
    ggplot2::theme_void() +
    ggplot2::coord_equal(expand = FALSE, clip = "off") +
    ggplot2::theme(
      legend.title = ggplot2::element_text(size = 10, face = "bold"),
      legend.text  = ggplot2::element_text(size = 8),
      legend.position = "left",
      legend.justification = c(-0.8, 0.0),
      legend.box.margin = ggplot2::margin(r=10, l=10, t=10, b=10),
      plot.margin = ggplot2::margin(t=20, r=20, b=20, l=20)
    )
  
  list(plot = p, nodes = nodes, edges = edges_for_plot, NODE_SIG = NODE_SIG)
}

# ------------------- RESUMO (HTML) POR LINHA ---------------------------

build_summary_html <- function(row, tcga_types = NULL){
  get1 <- function(r, nm) if (nm %in% names(r)) r[[nm]][1] else NA
  
  nomen    <- get1(row, "Nomenclature")
  sig_lab  <- get1(row, "Signatures")
  ctab_val <- get1(row, "CTAB")
  metabolism <- get1(row, "Metabolism")
  pathways   <- get1(row, "Pathways")
  mcd        <- get1(row, "Metabolic_cell_death")
  inter      <- get1(row, "Meaningful_interaction")
  
  # Nome completo do câncer se tcga_types for fornecido
  cancer_full <- ctab_val
  if (!is.null(tcga_types) &&
      all(c("Cancer_abbreviation","Cancer_names") %in% names(tcga_types))) {
    hit <- tcga_types$Cancer_names[match(ctab_val, tcga_types$Cancer_abbreviation)]
    if (!is.na(hit) && nzchar(hit)) cancer_full <- hit
  }
  
  # Omic/phenotype + rhos
  omic_sig   <- get1(row, "Omic_layer_signature")
  phen_sig   <- get1(row, "Phenotypic_layer_signature")
  rho_sig    <- suppressWarnings(as.numeric(get1(row, "rho_signature")))
  omic_int   <- get1(row, "Omic_layer_interaction")
  phen_int   <- get1(row, "Phenotypic_layer_interaction")
  rho_int    <- suppressWarnings(as.numeric(get1(row, "rho_interaction")))
  phen_conc  <- get1(row, "Phenotypic_concordance")
  
  fmt_rho <- function(x) if (!is.na(x)) sprintf("%.3f", x) else "NA"
  
  # Cox / Survival concordances
  cox_conc_OS  <- get1(row, "Cox_concordance_OS")
  cox_conc_DSS <- get1(row, "Cox_concordance_DSS")
  cox_conc_DFI <- get1(row, "Cox_concordance_DFI")
  cox_conc_PFI <- get1(row, "Cox_concordance_PFI")
  cox_agg      <- get1(row, "Cox_concordance_aggregated")
  
  surv_conc_OS  <- get1(row, "Survival_concordance_OS")
  surv_conc_DSS <- get1(row, "Survival_concordance_DSS")
  surv_conc_DFI <- get1(row, "Survival_concordance_DFI")
  surv_conc_PFI <- get1(row, "Survival_concordance_PFI")
  surv_agg      <- get1(row, "Survival_concordance_aggregated")
  
  immune_sig <- get1(row, "Immune_classification_signature")
  immune_int <- get1(row, "Immune_classification_interaction")
  immune_conc <- get1(row, "Immune_concordance")
  
  final_conc <- get1(row, "Final_concordance_summary")
  
  # Intro
  intro <- paste0(
    "<p>The nomenclature <b>", nomen, "</b> corresponds to the signature <b>",
    sig_lab, "</b> identified in <b>", cancer_full, "</b>. ",
    "This signature is mapped to <b>", metabolism, "</b> and the pathway <b>",
    pathways, "</b>."
  )
  if (!is.na(mcd) && tolower(mcd) != "unrelated" && nzchar(mcd)) {
    intro <- paste0(
      intro, " It is also associated with the metabolic cell death category <b>",
      mcd, "</b>."
    )
  }
  intro <- paste0(intro, "</p>")
  
  # Interações
  inter_txt <- ""
  if (!is.na(inter) && nzchar(inter) && inter != "No meaningful interaction") {
    inter_txt <- paste0(
      "<p>This signature establishes clinically meaningful interactions with <b>",
      inter, "</b>.</p>"
    )
  }
  
  # Fenótipo
  phen_txt <- ""
  if (!is.na(omic_sig) && !is.na(phen_sig) && !is.na(rho_sig)) {
    phen_txt <- paste0(
      "<p>At the signature level, <b>", omic_sig, "</b> is correlated with <b>",
      phen_sig, "</b> (ρ = ", fmt_rho(rho_sig), ")."
    )
    if (!is.na(omic_int) && !is.na(phen_int) && !is.na(rho_int)) {
      phen_txt <- paste0(
        phen_txt, " The interaction layer (", omic_int, ") is associated with <b>",
        phen_int, "</b> (ρ = ", fmt_rho(rho_int), ")."
      )
    }
    if (!is.na(phen_conc) && phen_conc %in% c("convergent","divergent")) {
      phen_txt <- paste0(
        phen_txt, " Overall, phenotype-level behaviour is <b>",
        phen_conc, "</b> between signature and interaction."
      )
    }
    phen_txt <- paste0(phen_txt, "</p>")
  }
  
  # Cox
  collapse_non_ns <- function(...) {
    x <- c(...)
    x <- x[!is.na(x) & x != "" & x != "NS"]
    if (!length(x)) return(NULL)
    paste(x, collapse = ", ")
  }
  
  cox_vec <- collapse_non_ns(
    if (!is.na(cox_conc_OS))  paste0("OS: ",  cox_conc_OS)  else NULL,
    if (!is.na(cox_conc_DSS)) paste0("DSS: ", cox_conc_DSS) else NULL,
    if (!is.na(cox_conc_DFI)) paste0("DFI: ", cox_conc_DFI) else NULL,
    if (!is.na(cox_conc_PFI)) paste0("PFI: ", cox_conc_PFI) else NULL
  )
  if (!is.null(cox_vec)) {
    cox_txt <- paste0(
      "<p>Cox-based prognosis concordance across clinical endpoints: <b>",
      cox_vec, "</b>."
    )
    if (!is.na(cox_agg) && cox_agg != "" && cox_agg != "NS") {
      cox_txt <- paste0(
        cox_txt, " Aggregated concordance across endpoints is <b>",
        cox_agg, "</b>."
      )
    }
    cox_txt <- paste0(cox_txt, "</p>")
  } else {
    cox_txt <- "<p>Cox-based concordance was not classified across endpoints.</p>"
  }
  
  # Survival concordance
  surv_vec <- collapse_non_ns(
    if (!is.na(surv_conc_OS))  paste0("OS: ",  surv_conc_OS)  else NULL,
    if (!is.na(surv_conc_DSS)) paste0("DSS: ", surv_conc_DSS) else NULL,
    if (!is.na(surv_conc_DFI)) paste0("DFI: ", surv_conc_DFI) else NULL,
    if (!is.na(surv_conc_PFI)) paste0("PFI: ", surv_conc_PFI) else NULL
  )
  if (!is.null(surv_vec)) {
    surv_txt <- paste0(
      "<p>Survival-group concordance across endpoints is described as <b>",
      surv_vec, "</b>."
    )
    if (!is.na(surv_agg) && surv_agg != "" && surv_agg != "NS") {
      surv_txt <- paste0(
        surv_txt, " The aggregated survival concordance is <b>",
        surv_agg, "</b>."
      )
    }
    surv_txt <- paste0(surv_txt, "</p>")
  } else {
    surv_txt <- "<p>Survival-group concordance metrics were not classified.</p>"
  }
  
  # Imune
  immune_txt <- ""
  if ((!is.na(immune_sig) && immune_sig != "" && immune_sig != "NS") ||
      (!is.na(immune_int) && immune_int != "" && immune_int != "NS")) {
    immune_txt <- "<p>At the immune level,"
    if (!is.na(immune_sig) && immune_sig != "" && immune_sig != "NS") {
      immune_txt <- paste0(
        immune_txt, " the signature is classified as <b>", immune_sig, "</b>"
      )
    }
    if (!is.na(immune_int) && immune_int != "" && immune_int != "NS") {
      if (!is.na(immune_sig) && immune_sig != "" && immune_sig != "NS") {
        immune_txt <- paste0(immune_txt, ", while the interaction is classified as <b>",
                             immune_int, "</b>")
      } else {
        immune_txt <- paste0(
          immune_txt, " the interaction is classified as <b>", immune_int, "</b>"
        )
      }
    }
    if (!is.na(immune_conc) && immune_conc != "" && immune_conc != "NS") {
      immune_txt <- paste0(
        immune_txt, ". The immune concordance between both layers is <b>",
        immune_conc, "</b>"
      )
    }
    immune_txt <- paste0(immune_txt, ".</p>")
  }
  
  # Final concordance
  final_txt <- ""
  if (!is.na(final_conc) && final_conc != "" && final_conc != "NS") {
    final_txt <- paste0(
      "<p>Integrating metabolic, phenotypic, immune and prognostic dimensions, ",
      "the overall regulatory circuitry is summarized as <b>", final_conc,
      "</b>.</p>"
    )
  }
  
  HTML(paste0(
    intro,
    inter_txt,
    phen_txt,
    cox_txt,
    surv_txt,
    immune_txt,
    final_txt,
    "<p><i>Use the regulatory network below to explore how the signature and its interaction partners",
    " connect metabolic pathways, phenotypes, immune context and prognosis.</i></p>"
  ))
}

# ======================================================================
# UI do módulo
# ======================================================================

mod_meaningful_interaction_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        h4("Select signature (Nomenclature)"),
        selectizeInput(
          ns("nomen_input"),
          "Nomenclature:",
          choices = NULL,
          multiple = FALSE,
          options = list(placeholder = "Type to search…")
        ),
        actionButton(ns("run"), "Search", class = "btn-primary"),
        hr(),
        downloadButton(ns("downloadRow"),     "Download selected row (.csv)"),
        br(), br(),
        downloadButton(ns("downloadAll"),     "Download all rows (.csv)"),
        br(), br(),
        downloadButton(ns("downloadSummary"), "Download summary (.txt)"),
        br(), br(),
        ## 🔽🔽🔽 NOVO BOTÃO 🔽🔽🔽
        downloadButton(ns("downloadPlot"),    "Download network plot (.pdf)")
      ),
      mainPanel(
        h3("Integrative summary for the selected interaction"),
        uiOutput(ns("summaryText")),
        hr(),
        h3("Regulatory circuitry network"),
        plotOutput(ns("netplot"), height = "550px"),
        hr(),
        h3("All interactions for this Nomenclature"),
        p("Click a row to update the textual summary above."),
        DT::DTOutput(ns("row_table"))
      )
    )
  )
}


# ======================================================================
# Server do módulo
# ======================================================================

mod_meaningful_interaction_server <- function(id, dataset, tcga_types = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Atualiza opções do select com todas as Nomenclature
    observe({
      req(dataset())
      data <- dataset()
      validate(need("Nomenclature" %in% names(data),
                    "Dataset must contain a 'Nomenclature' column."))
      ch <- sort(unique(na.omit(as.character(data$Nomenclature))))
      updateSelectizeInput(
        session, "nomen_input",
        choices  = ch,
        selected = NULL,
        server   = TRUE
      )
    })
    
    # Todas as linhas da Nomenclature escolhida
    matches_all <- eventReactive(input$run, {
      df <- dataset(); req(df, input$nomen_input)
      hit <- df %>% dplyr::filter(.data$Nomenclature == input$nomen_input)
      validate(need(nrow(hit) > 0, "Nomenclature not found in dataset."))
      
      if ("Signatures" %in% names(hit)) {
        stopifnot(length(unique(hit$Signatures)) == 1L)
      }
      hit
    }, ignoreInit = TRUE)
    
    # Tabela com todas as interações
    output$row_table <- DT::renderDT({
      rows <- matches_all(); req(rows)
      DT::datatable(
        rows,
        rownames  = FALSE,
        selection = "single",
        options   = list(scrollX = TRUE, pageLength = 5)
      )
    })
    
    # Linha selecionada (para resumo e download)
    selected_row <- reactive({
      rows <- matches_all(); req(rows)
      sel  <- input$row_table_rows_selected
      if (!is.null(sel) && length(sel) == 1) {
        rows[sel, , drop = FALSE]
      } else {
        rows[1, , drop = FALSE]
      }
    })
    
    # Resumo textual com base na linha selecionada
    output$summaryText <- renderUI({
      row <- selected_row(); req(row)
      build_summary_html(row, tcga_types = tcga_types)
    })
    
    # Grafo com TODAS as interações dessa Nomenclature
    net_obj <- reactive({
      rows <- matches_all(); req(rows)
      build_network_from_matches(rows)
    })
    
    output$netplot <- renderPlot({
      obj <- net_obj(); req(obj$plot)
      obj$plot
    }, res = 120)
    
    # 🔽🔽🔽 NOVO HANDLER PARA SALVAR O PLOT EM PDF 🔽🔽🔽
    output$downloadPlot <- downloadHandler(
      filename = function() {
        nm <- input$nomen_input %||% "signature"
        paste0("meaningful_interaction_network_",
               gsub("[^A-Za-z0-9]+","_", nm), ".pdf")
      },
      content = function(file) {
        obj <- net_obj(); req(obj$plot)
        # Salva o mesmo objeto de plot usado no renderPlot
        ggplot2::ggsave(
          filename = file,
          plot     = obj$plot,
          device   = "pdf",
          width    = 10,
          height   = 8
        )
      }
    )
    
    # Download da linha selecionada
    output$downloadRow <- downloadHandler(
      filename = function() {
        nm <- input$nomen_input %||% "signature"
        paste0("meaningful_interaction_row_",
               gsub("[^A-Za-z0-9]+","_", nm), ".csv")
      },
      content = function(file) {
        df <- selected_row(); req(df)
        readr::write_csv(df, file)
      }
    )
    
    # Download de todas as linhas daquela Nomenclature
    output$downloadAll <- downloadHandler(
      filename = function() {
        nm <- input$nomen_input %||% "signature"
        paste0("meaningful_interaction_all_",
               gsub("[^A-Za-z0-9]+","_", nm), ".csv")
      },
      content = function(file) {
        df <- matches_all(); req(df)
        readr::write_csv(df, file)
      }
    )
    
    # Download do resumo em texto
    output$downloadSummary <- downloadHandler(
      filename = function() {
        nm <- input$nomen_input %||% "signature"
        paste0("meaningful_interaction_summary_",
               gsub("[^A-Za-z0-9]+","_", nm), ".txt")
      },
      content = function(file) {
        row  <- selected_row(); req(row)
        html <- as.character(build_summary_html(row, tcga_types = tcga_types))
        txt  <- gsub("<[^>]+>", "", html)
        writeLines(txt, file, useBytes = TRUE)
      }
    )
  })
}
