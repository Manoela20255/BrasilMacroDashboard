# =============================
# Fetchers com fallback (.rds) — v3 robusto
# =============================

`%||%` <- function(a, b) if (!is.null(a)) a else b

.cache_dir <- "data"
ensure_dir <- function(p) if (!dir.exists(p)) dir.create(p, recursive = TRUE)
save_rds  <- function(x, path) { ensure_dir(dirname(path)); try(saveRDS(x, path), silent = TRUE) }
read_rds  <- function(path) if (file.exists(path)) try(readRDS(path), silent = TRUE) else NULL

# ---- SGS/BCB
fetch_sgs_basico <- function() {
  GetBCBData::gbcbd_get_series(
    id         = 432,
    first.date = as.Date("2020-01-01"),
    last.date  = Sys.Date()
  ) |>
    dplyr::arrange(ref.date)
}
fetch_sgs_multi <- function() {
  GetBCBData::gbcbd_get_series(
    id          = c("Dolar" = 3698, "IBC_Br" = 24363, "Resultado_Primario" = 5793),
    first.date  = as.Date("2020-01-01"),
    last.date   = Sys.Date(),
    use.memoise = FALSE
  ) |>
    tidyr::pivot_wider(id_cols = "ref.date", names_from = "series.name", values_from = "value") |>
    dplyr::arrange(ref.date)
}

# ---- Focus IPCA mensal (sem cache, simples)
fetch_focus_ipca_mensal <- function() {
  df <- tryCatch(
    rbcb::get_market_expectations(type = "annual", indic = "IPCA", start_date = as.Date("2020-01-01")),
    error = function(e) NULL
  )
  if (is.null(df) || nrow(df) == 0) return(tibble::tibble())
  if ("baseCalculo" %in% names(df)) df <- dplyr::filter(df, baseCalculo == 0)
  med_cols <- intersect(c("Mediana","median","mediana","Value","valor"), names(df))
  if (!length(med_cols)) return(tibble::tibble())
  med_col <- med_cols[1]
  df |>
    dplyr::transmute(
      data     = Data,
      data_ref = DataReferencia,
      mediana  = .data[[med_col]]
    ) |>
    dplyr::mutate(ano_mes = tsibble::yearmonth(data)) |>
    dplyr::group_by(data_ref, ano_mes) |>
    dplyr::summarise(expectativa_mensal = mean(mediana, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(ano_mes, data_ref)
}

# ---- IPEA
fetch_ipea_caged_embi <- function() {
  ipeadatar::ipeadata(c("caged" = "CAGED12_SALDON12", "embi_br" = "JPM366_EMBI366")) |>
    tidyr::pivot_wider(id_cols = "date", names_from = "code", values_from = "value") |>
    dplyr::rename(data = date, caged = CAGED12_SALDON12, embi_br = JPM366_EMBI366) |>
    dplyr::arrange(dplyr::desc(data))
}

# ---- SIDRA
fetch_sidra_ipca <- function() {
  cod_sidra <- "/t/7060/n1/all/v/63/p/all/c315/7169/d/v63%202"
  sidrar::get_sidra(api = cod_sidra) |>
    dplyr::mutate(data = lubridate::ym(`Mês (Código)`), ipca = Valor) |>
    dplyr::select(data, ipca) |>
    dplyr::as_tibble()
}

# ---- OECD via DB.NOMICS
fetch_ocde_desemprego <- function() {
  link <- paste0(
    "https://api.db.nomics.world/v22/series/OECD/MEI?dimensions=%7B%22SUBJECT",
    "%22%3A%5B%22LRHUTTTT%22%5D%2C%22MEASURE%22%3A%5B%22STSA%22%5D%2C%22FREQU",
    "ENCY%22%3A%5B%22M%22%5D%7D&observations=1"
  )
  rdbnomics::rdb(api_link = link) |>
    dplyr::select(data = period, pais = Country, valor = value) |>
    dplyr::as_tibble()
}

# ---- FMI via DB.NOMICS (REER demo)
fetch_fmi_reer_exemplo <- function() {
  link <- paste0(
    "https://api.db.nomics.world/v22/series/IMF/IFS?dimensions=%7B%22INDICATOR",
    "%22%3A%5B%22EREER_IX%22%5D%2C%22REF_AREA%22%3A%5B%22AU%22%2C%22CA%22%2C%22CL%22%5D%2C",
    "%22FREQ%22%3A%5B%22M%22%5D%7D&observations=1"
  )
  rdbnomics::rdb(api_link = link) |>
    tidyr::pivot_wider(id_cols = "period", names_from = "Reference Area", values_from = "value") |>
    dplyr::arrange(period)
}

# ---- WDI
fetch_wdi_core_inflation_all <- function() {
  WDI::WDI(country = "all", indicator = "CORENS", start = 2010, end = as.numeric(format(Sys.Date(), "%Y"))) |>
    tidyr::drop_na() |>
    dplyr::as_tibble()
}
fetch_wdi_two_countries <- function() {
  WDI::WDI(
    country   = c("US","GB"),
    indicator = c("juros_real" = "FR.INR.RINR", "pib_pc" = "NY.GDP.PCAP.KN"),
    start     = 2010,
    end       = as.numeric(format(Sys.Date(), "%Y"))
  ) |>
    tidyr::drop_na() |>
    dplyr::rename_with(.fn = ~c("pais","codigo2d","codigo3d","ano","juros_real","pib_pc")) |>
    dplyr::as_tibble()
}

# ---- Política Monetária (robusto + fallback)

.fetch_focus_top5_raw <- function() {
  rbcb::get_market_expectations(
    type = "annual",
    indic = c("IPCA","PIB Total","Selic","Câmbio"),
    start_date = Sys.Date() - 3650
  )
}

fetch_focus_top5 <- function(cache_path = file.path(.cache_dir, "focus_top5.rds")) {
  ok <- TRUE
  df <- tryCatch(.fetch_focus_top5_raw(), error = function(e) { ok <<- FALSE; NULL })

  if (ok && !is.null(df) && nrow(df) > 0) {
    # baseCalculo pode vir como 0/"0"/FALSE/"False"
    if ("baseCalculo" %in% names(df)) {
      # tenta coerção numérica; se NA, trata strings
      base_num <- suppressWarnings(as.numeric(df$baseCalculo))
      keep <- (base_num == 0)
      if (all(is.na(keep))) {
        keep <- df$baseCalculo %in% c("0","FALSE","False","false","0.0","0L","0.00")
      }
      df <- df[keep %||% TRUE, , drop = FALSE]
    }

    # identifica coluna de mediana
    med_cols <- intersect(c("Mediana","median","mediana","Value","valor"), names(df))
    if (!length(med_cols)) {
      top5 <- read_rds(cache_path)
      return(list(df = top5 %||% tibble::tibble(), cache_used = !is.null(top5),
                  cache_date = if (!is.null(top5)) max(top5$data, na.rm=TRUE) else NA))
    }
    med_col <- med_cols[1]

    top5 <- df |>
      dplyr::transmute(
        indicador  = Indicador,
        data       = as.Date(Data),
        data_ref   = suppressWarnings(as.numeric(DataReferencia)),
        mediana    = .data[[med_col]]
      ) |>
      dplyr::mutate(ano_mes = tsibble::yearmonth(data)) |>
      dplyr::group_by(indicador, ano_mes, data_ref) |>
      dplyr::summarise(expectativa_media = mean(mediana, na.rm = TRUE), .groups = "drop") |>
      dplyr::arrange(indicador, ano_mes, data_ref)

    save_rds(top5, cache_path)
    return(list(df = top5, cache_used = FALSE, cache_date = NA))
  } else {
    top5 <- read_rds(cache_path)
    return(list(df = top5 %||% tibble::tibble(), cache_used = !is.null(top5),
                cache_date = if (!is.null(top5)) max(top5$data, na.rm=TRUE) else NA))
  }
}

.fetch_focus_ipca_12m_raw <- function() rbcb::get_market_expectations(type = "inflation-12-months", indic = "IPCA")
fetch_focus_ipca_12m <- function(cache_path = file.path(.cache_dir, "ipca_12m.rds")) {
  ok <- TRUE
  df <- tryCatch(.fetch_focus_ipca_12m_raw(), error = function(e) { ok <<- FALSE; NULL })

  if (ok && !is.null(df) && nrow(df) > 0) {
    med_cols <- intersect(c("Mediana","median","mediana","Value","valor"), names(df))
    if (!length(med_cols)) {
      out <- read_rds(cache_path)
      return(list(df = out %||% tibble::tibble(), cache_used = !is.null(out),
                  cache_date = if (!is.null(out)) max(out$data, na.rm=TRUE) else NA))
    }
    med_col <- med_cols[1]

    if ("baseCalculo" %in% names(df)) {
      base_num <- suppressWarnings(as.numeric(df$baseCalculo))
      keep <- (base_num == 0)
      if (all(is.na(keep))) {
        keep <- df$baseCalculo %in% c("0","FALSE","False","false","0.0","0L","0.00")
      }
      df <- df[keep %||% TRUE, , drop = FALSE]
    }

    # suavizada: aceitar variações
    if ("Suavizada" %in% names(df)) {
      okS <- df$Suavizada %in% c("S","Sim","Y", TRUE)
      df <- df[okS %||% TRUE, , drop = FALSE]
    }

    out <- df |>
      dplyr::group_by(data = tsibble::yearmonth(Data)) |>
      dplyr::summarise(ipca = mean(.data[[med_col]], na.rm = TRUE), .groups = "drop") |>
      dplyr::mutate(data = as.Date(data))

    save_rds(out, cache_path)
    return(list(df = out, cache_used = FALSE, cache_date = NA))
  } else {
    out <- read_rds(cache_path)
    return(list(df = out %||% tibble::tibble(), cache_used = !is.null(out),
                cache_date = if (!is.null(out)) max(out$data, na.rm=TRUE) else NA))
  }
}

.fetch_swap_di_360_raw <- function() ipeadatar::ipeadata("BMF12_SWAPDI36012")
fetch_swap_di_360 <- function(cache_path = file.path(.cache_dir, "swap_di_360.rds")) {
  ok <- TRUE
  df <- tryCatch(.fetch_swap_di_360_raw(), error = function(e) { ok <<- FALSE; NULL })
  if (ok && !is.null(df) && nrow(df) > 0 && all(c("date","value") %in% names(df))) {
    out <- dplyr::transmute(df, data = date, swap = value)
    save_rds(out, cache_path)
    return(list(df = out, cache_used = FALSE, cache_date = NA))
  } else {
    out <- read_rds(cache_path)
    return(list(df = out %||% tibble::tibble(), cache_used = !is.null(out),
                cache_date = if (!is.null(out)) max(out$data, na.rm=TRUE) else NA))
  }
}

# ---- Cálculos
calc_juro_neutro <- function(top5_df) {
  if (is.null(top5_df) || nrow(top5_df) == 0 || !("expectativa_media" %in% names(top5_df))) {
    return(dplyr::tibble(data = as.Date(character()), neutro = NA_real_))
  }
  top5_df |>
    dplyr::filter(indicador %in% c("Selic","IPCA")) |>
    dplyr::mutate(ano = lubridate::year(ano_mes)) |>
    dplyr::filter(data_ref == ano + 3) |>
    tidyr::pivot_wider(
      id_cols    = c("ano_mes","data_ref"),
      names_from = "indicador",
      values_from= "expectativa_media"
    ) |>
    dplyr::transmute(
      data = as.Date(ano_mes),
      neutro = ((1 + Selic/100) / (1 + IPCA/100) - 1) * 100
    )
}
calc_juro_real_exante <- function(ipca12_df, swaps_df) {
  if (is.null(ipca12_df) || is.null(swaps_df) || nrow(ipca12_df) == 0 || nrow(swaps_df) == 0) {
    return(dplyr::tibble(data = as.Date(character()), ex_ante = NA_real_))
  }
  dplyr::left_join(ipca12_df, swaps_df, by = "data") |>
    dplyr::mutate(ex_ante = ((1 + swap/100) / (1 + ipca/100) - 1) * 100) |>
    dplyr::select(data, ex_ante)
}
