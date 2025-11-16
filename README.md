# Welcome to Multi-omic Oncometabolism GPS Shiny

**[Higor Almeida Cordeiro Nogueira](https://www.researchgate.net/profile/Higor-Cordeiro-Nogueira), [Emanuell de Souza Rodrigues](https://www.researchgate.net/profile/Emanuell-Rodrigues-De-Souza), [Victor dos Santos Lopes](https://www.linkedin.com/in/victor-lopes-880604377?utm_source=share&utm_campaign=share_via&utm_content=profile&utm_medium=ios_app), [Enrique Medina-Acosta](https://www.researchgate.net/profile/Enrique-Medina-Acosta)**

**Multi-omic OncoMetabolismGPS** is an interactive Shiny application developed as part of the research associated with the pre-print **A Multi-Omic Atlas of Convergent and Divergent Metabolic Regulatory Circuitries in Cancer**, available at:
.

The app provides an integrative and user-friendly interface for exploring multi-omic metabolic signatures across cancer types, enabling dynamic visualization of meaningful interactions, phenotypic associations, survival metrics, and regulatory circuitries derived directly from the analyses presented in the paper. Below, we include the full abstract of the manuscript followed by the graphical abstract.

**Abstract**

Metabolic reprogramming underlies tumor progression, immune evasion, and resistance to regulated cell death, yet the higher-order regulatory logic that coordinates these processes across molecular layers remains poorly defined. We developed OncoMetabolismGPS, a multi-omic analytical framework that reconstructs a Pan-Cancer atlas of convergent and divergent metabolic regulatory circuitries. From 463,433 significant multi-omic and phenotypic associations across 33 tumor types, we derived 241,415 omic-specific metabolic signatures, each integrating metabolic pathway context with phenotypic, prognostic, and immune features. By mapping shared upstream regulators of these signatures, we identified 24,796 metabolic regulatory circuitries—classified as convergent when regulators and signatures act in the same biological direction, or divergent when they exhibit opposing associations. Divergent circuitry predominated, especially in immunosuppressive (cold) tumor contexts, revealing context-dependent regulatory compensation across metabolic, phenotypic, and clinical axes. The accompanying OncoMetabolismGPS Shiny application implements this atlas as an interactive platform that positions each signature and circuitry within a multidimensional coordinate space defined by molecular, phenotypic, immune, and clinical attributes, enabling systematic navigation of metabolic regulatory behavior in cancer. Together, this study establishes the first multi-omic atlas of metabolic regulatory circuitries, providing a conceptual and computational framework for dissecting metabolic plasticity, pathway dependencies, and therapeutic vulnerabilities across human cancers.

The schematic below represents the conceptual framework used to guide the development of this application.

<p align="center">
  <img src="https://github.com/HigorACNogueira/Multi-omic-Oncometabolism-GPS/blob/main/www/Figure_1.png" width="1000">
</p>
 
---

## 🔗 Link to Multi-omic-Oncometabolism-GPS
- 🔥 [Online App](https://oncometabolismgps.shinyapps.io/Multi-omic-oncometabolism-GPS/)  

---

### ▶️ Run Locally
To launch this tool locally in R, download **Multi-omic-oncometabolismGPS paste**, modify the path to the parent directory of the source directory, and run the code.

```r
library(shiny)
setwd("/path/to/parent/dir/of/source/")
runApp()
```

---

## 📌 Citation

If you use this repository, its datasets, analytical pipelines, figures, methods, or conceptual framework in your research, please cite:

**Nogueira HAC**, **Rodrigues ES**, **Lopes VS**, **Medina-Acosta E**.  
**A Multi-Omic Atlas of Convergent and Divergent Metabolic Regulatory Circuitries in Cancer.**  
2025.

Citing this work supports the continued development of this multi-omic atlas.

---

## 🐞 Bug Reports

Please open an **issue** on GitHub or contact:  
📧 **[Higor Almeida Cordeiro Nogueira](higoralmeida1995@gmail.com)**  

---

## ⚙️ Tested Environment

```
R version 4.3.1 (2023-06-16)
Platform: x86_64-w64-mingw32 (64-bit)

Core Shiny Framework

shiny – 1.11.1
shinycssloaders – 1.1.0

Data Handling & Utilities

dplyr – 1.1.4
tidyr – 1.3.1
stringr – 1.5.1
tibble – 3.2.1
purrr – 1.1.0
readr – 2.1.5
readxl – 1.4.5
glue – 1.8.0
memoise – 2.0.1
qs – 0.27.3

Plotting and Visualization

ggplot2 – 3.5.2
ggsci – 3.2.0
cowplot – 1.2.0
grid – 4.3.1
gridtext – 0.1.5
igraph – 2.1.4
tidygraph – 1.3.1
ggraph – 2.2.1

survival – 3.8.3
survminer – 0.5.0
fmsb – 0.7.6
ggradar – 0.2

Interactive Tables

DT – 0.33

UCSC Xena Tools

UCSCXenaShiny – 2.2.0
UCSCXenaTools – 1.6.1

Deployment

rsconnect – 1.5.0

Development Tools

devtools – 2.4.5
```

