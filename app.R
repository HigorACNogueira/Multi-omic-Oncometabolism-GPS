# Carregar os arquivos UI e Server
source("ui.R")
source("server.R")

# setwd("D:/Cancer_metabolism_analysis_06/30- Cancer_Metabolism_GPS_Shiny/CancerMetabolismShiny/CancerMetabolismGPSShiny")

# Iniciar a aplicação
shinyApp(ui, server)

# Run the app
shiny::runApp()
