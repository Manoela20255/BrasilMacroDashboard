# Brazil Macro Dashboard — v3
- ✅ Atualiza **diariamente** via GitHub Actions (12:00 UTC) e sob demanda
- ✅ Gráficos **plotly** + tabelas **DT** (copy/CSV/Excel)
- ✅ Fallback `.rds` quando APIs falham + aviso discreto

## Como usar (qualquer uma das pastas)
1. Abra no RStudio e rode:
```r
source("libs.R")
quarto::quarto_render(as_job = FALSE)
browseURL("_site/index.html")
```
2. Para auto-update diário, suba ao GitHub e habilite **Settings → Pages → GitHub Actions**.
