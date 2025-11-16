# 🔹 Define Visitor Counter Function (Place at the top of server.R)
counter_file <- "counter.txt"

# Function to read and update visitor count
update_counter <- function() {
  if (!file.exists(counter_file)) {
    writeLines("0", counter_file)  # Initialize file if missing
  }
  
  count <- as.integer(readLines(counter_file))  # Read current count
  count <- count + 1  # Increment
  writeLines(as.character(count), counter_file)  # Save new count
  return(count)
}


server <- function(input, output, session) {
  session$allowReconnect(TRUE) # Permite reconexão automática
  
  # 🔹 Update Visitor Counter
  visitor_count <- update_counter()  # Increment and store visitor count
  
  output$visitor_count <- renderText({
    paste("Total:", visitor_count)
  })
  
  # Criando reativos apenas onde for necessário
  search_your_target <- reactiveVal(all_data$search_your_target)
  Target <- reactiveVal(all_data$Dataset_S2)
  
  cnv_signature <- reactiveVal(all_data$cnv_signature)
  gene_signature <- reactiveVal(all_data$gene_signature)
  methylation_signature <- reactiveVal(all_data$methylation_signature)
  mutation_signature <- reactiveVal(all_data$mutation_signature)
  mirna_signature <- reactiveVal(all_data$mirna_signature)
  protein_signature <- reactiveVal(all_data$protein_signature)
  transcript_signature <- reactiveVal(all_data$transcript_signature)
  
  meaningful_interaction <- reactiveVal(all_data$Dataset_S3)
  
  # create reactive container
  regulatory_circuitry <- reactiveVal(all_data$Dataset_S4)
  
  # After other reactiveVals:
  atlas <- reactiveVal(all_data$Dataset_S1)
  
  mod_atlas_server("atlas", atlas_reactive = atlas, tcga_types = tcga_types)
  
  # Chamando os módulos corretamente
  mod_search_your_target_server("search_your_target", search_your_target, Target, tcga_types)
  
  # Chamando outros módulos de assinatura
  mod_all_signatures_server("all_signatures", Target)
  mod_signature_server("cnv_signature", cnv_signature)
  mod_signature_server("gene_signature", gene_signature)
  mod_signature_server("methylation_signature", methylation_signature)
  mod_signature_server("mutation_signature", mutation_signature)
  mod_signature_server("mirna_signature", mirna_signature)
  mod_signature_server("protein_signature", protein_signature)
  mod_signature_server("transcript_signature", transcript_signature)
  
  mod_meaningful_interaction_server("Mean_int", meaningful_interaction)
  
  # call the module
  mod_regulatory_circuitry_server("reg_circ", regulatory_circuitry)
  
  # Call the module
  mod_custom_signatures_server("custom_signatures", dataset_reactive = atlas)
  
  # Chamando outros módulos de análise
  mod_correlation_analysis_server("correlation_analysis", Target)
  mod_tumor_normal_analysis_server("tumor_normal_analysis", Target)
  mod_cox_analysis_server("cox_analysis", Target)
  mod_survival_analysis_server("survival_analysis", Target)
  mod_infiltrates_analysis_server("infiltrates_analysis", Target)
  
  # Chamando outros módulos gerais
  # mod_web_resources_server("Web_resources")
  # mod_article_server("article")
  mod_developers_server("Developers")
  
  # 🔹 **CLEAN MEMORY WHEN A USER DISCONNECTS**
  session$onSessionEnded(function() {
    message("Session ended. Cleaning up memory.")
    rm(list = ls())  # Remove all objects from memory
    gc()  # Trigger garbage collection
  })
  
}
