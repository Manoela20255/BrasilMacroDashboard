suppressPackageStartupMessages({
  library(GetBCBData) # SGS/BCB
  library(rbcb)       # BCB Focus
  library(ipeadatar)  # IPEA
  library(sidrar)     # SIDRA/IBGE
  library(rdbnomics)  # DB.NOMICS
  library(WDI)        # World Bank
  library(dplyr); library(stringr); library(tidyr); library(magrittr)
  library(tsibble); library(lubridate)
  library(ggplot2); library(scales); library(plotly); library(DT); library(bslib)
})
options(timeout = 600)
