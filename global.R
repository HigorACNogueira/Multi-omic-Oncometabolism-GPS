# =========================================================
# Load Required Packages (deduplicated and organized)
# =========================================================
suppressPackageStartupMessages({
  
  # Core Shiny
  library(shiny)
  library(shinycssloaders)
  
  # Data handling and utilities
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(tibble)
  library(purrr)
  library(readr)
  library(readxl)
  library(glue)
  library(memoise)
  
  library(qs)
  
  # Plotting and visualization
  library(ggplot2)
  library(ggsci)
  library(cowplot)
  library(grid)
  library(gridtext)
  
  # Network & Graph visualization
  library(igraph)
  library(tidygraph)
  library(ggraph)
  
  # Biological / Survival Analysis
  library(survival)
  library(survminer)
  library(fmsb)
  library(ggradar)
  
  # Shiny Data Tables
  library(DT)
  library(htmltools) 
  library(grid)
  
  # UCSC Xena data utilities
  library(UCSCXenaShiny)
  library(UCSCXenaTools)
  
  # Deployment (optional)
  library(rsconnect)
  
  # Development tools (optional but used in some parts of your code)
  library(devtools)
})


# ---------------------- Configurações do Shiny ----------------------
options(shiny.maxRequestSize = 500*1024^2) # Permite uploads de até 500MB
options(shiny.sanitize.errors = FALSE) # Exibe mensagens de erro completas
options(shiny.autoreload = TRUE) # Recarrega o app automaticamente ao editar arquivos

# # ---------------------- Carregamento dos Dados ----------------------
load_qs <- memoise(function(path) qs::qread(path))
all_data <- load_qs("data/all_data.qs")  # list: named elements per dataset

# # ---------------------- Função para carregar dados de forma segura ----------------------
# load_rds <- memoise(function(filename) {
#   file_path <- file.path("data", filename)
#   if (file.exists(file_path)) {
#     message("Carregando: ", filename)
#     return(readRDS(file_path))
#   } else {
#     warning(paste("Arquivo não encontrado:", filename))
#     return(NULL)
#   }
# })
# 
# all_data <- readRDS("data/all_data.rds")
tcga_types <- read_excel("data/TCGA_Cancer_types.xlsx")

# ---------------------- Importação de Módulos ----------------------
source("modules/module_user_manual.R")
source("modules/module_atlas.R")
source("modules/module_search_your_target.R")
source("modules/module_nomenclature.R")
source("modules/module_all_signatures.R")
source("modules/module_signature.R")
source("modules/module_meaningful_interaction.R")
source("modules/module_regulatory_circuitry.R")
source("modules/module_custom_signatures.R")
source("modules/module_correlation_analysis.R")
source("modules/module_tumor_normal_analysis.R")
source("modules/module_cox_analysis.R")
source("modules/module_survival_analysis.R")
source("modules/module_infiltrates_analysis.R")
source("modules/module_web_resources.R")
# source("modules/module_article.R")
source("modules/module_developers.R")




# # one-off script to create a master bundle
# files <- c(
#   search_your_target            = "Search_your_target.rds",
#   all_signatures                = "Dataset_S2.tsv",
#   tcga_types_xlsx               = "TCGA_Cancer_types.xlsx",
# 
#   atlas                         = "Dataset_S1.tsv",
#   meaningful_interaction        = "Dataset_S3.tsv",
#   regulatory_circuitry          = "Dataset_S4.tsv",
# 
#   cnv_signature                 = "CNV_results.rds",
#   gene_signature                = "gene_expression_results.rds",
#   methylation_signature         = "Methylation_results.rds",
#   mutation_signature            = "Mutation_results.rds",
#   mirna_signature               = "miRNA_expression_results.rds",
#   protein_signature             = "protein_expression_results.rds",
#   transcript_signature          = "Transcript_expression_results.rds"
# )
# 
# load_rds <- function(filename) readRDS(file.path("data", filename))
# bundle <- lapply(files[grepl("\\.rds$", files)], load_rds)
# bundle$tcga_types <- readxl::read_excel(file.path("data", files["tcga_types_xlsx"]))
# bundle$Dataset_S1 <- import(file.path("data", files["atlas"]))
# bundle$Dataset_S2 <- import(file.path("data", files["all_signatures"]))
# bundle$Dataset_S3 <- import(file.path("data", files["meaningful_interaction"]))
# bundle$Dataset_S4 <- import(file.path("data", files["regulatory_circuitry"]))
# 
# saveRDS(bundle, file = "data/all_data.rds", compress = "xz")
# 
# # load
# all_data <- import("data/all_data.rds")
# library(qs)
# qsave(all_data, "data/all_data.qs", preset = "balanced")  # or "high"
# # load
# all_data_0 <- qread("data/all_data.qs")
