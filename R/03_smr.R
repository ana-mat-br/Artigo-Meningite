# =============================================================================
# 03_smr — Razão de Incidência Padronizada (SMR) por idade × sexo
#
# Origem: Extraído de 02_smr.Rmd via knitr::purl()
# Depende: dados_base.rds (01), populacao_br_mg.rds (02)
# Gera:    casos_por_faixa.rds; tabela SMR cumulativo TM vs BR/MG
#
# Projeto: Análise da meningite no Triângulo Mineiro (2015–2025)
# =============================================================================
## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE,
                      dpi = 300)

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(stringr)
  library(knitr); library(kableExtra); library(tibble)
})
# Aliases — evita mascaramento por MASS/raster/stats
select <- dplyr::select; filter <- dplyr::filter; mutate <- dplyr::mutate
summarise <- dplyr::summarise; group_by <- dplyr::group_by; rename <- dplyr::rename

# Carregar dados base
if (!file.exists("dados_base.rds"))
  stop("Rode antes: source('01_dados_setup.R')")
db <- readRDS("dados_base.rds")
list2env(db$config, envir = environment())

if (!file.exists("populacao_br_mg.rds"))
  stop("Rode antes: source('baixar_referencias_datasus.R')")
pop_ref_faixa <- readRDS("populacao_br_mg.rds")

# Janela analítica principal: 2015-2025 completo.
# Justificativa: extração realizada em 2026-05, ~5 meses após fim de 2025;
# última DT_DIGITA no banco = 2026-03-17 (lag SINAN-Mening típico ~6 meses absorvido);
# distribuição mensal de 2025 não evidencia cauda decrescente típica de incompletude
# (12-24 casos/mês no TM; 877-1284/mês no BR — estável).
ano_max_principal <- 2025L
anos_principal <- anos

# Helpers
fmt_p   <- function(x) ifelse(x < 0.001, "< 0,001",
                              formatC(x, format = "f", digits = 3, decimal.mark = ","))
fmt1    <- function(x) formatC(x, format = "f", digits = 1, decimal.mark = ",")
fmt_smr <- function(x) formatC(x, format = "f", digits = 2, decimal.mark = ",")
fmt_ic  <- function(li, ls) paste0("(", fmt_smr(li), "-", fmt_smr(ls), ")")

# IC 95% exato de Poisson para SMR (gold standard; mais preciso que Byar).
# Referencia: Breslow & Day (1987), IARC vol II, eq. 2.11.
ic_smr  <- function(obs, esp) {
  list(li = qchisq(0.025, 2 * obs)       / (2 * esp),
       ls = qchisq(0.975, 2 * (obs + 1)) / (2 * esp))
}

classificar_faixa <- function(nu_idade_n) {
  ui  <- floor(as.integer(nu_idade_n) / 1000)
  val <- as.integer(nu_idade_n) %% 1000
  ia  <- dplyr::case_when(ui == 4 ~ val, ui <= 3 ~ 0L, TRUE ~ NA_integer_)
  cut(ia, breaks = c(-1, 4, 19, 49, Inf),
      labels = c("<5","5-19","20-49","50+"), right = TRUE)
}


## ----smr-prep, include=FALSE--------------------------------------------------
# Casos por (faixa × sexo × ano) — BR, MG, SE — agregados de sinan_br
sinan_br_classif <- db$sinan_br |>
  mutate(
    faixa   = classificar_faixa(NU_IDADE_N),
    sexo    = case_when(CS_SEXO == "M" ~ "M", CS_SEXO == "F" ~ "F", TRUE ~ NA_character_),
    uf_resi = str_sub(as.character(.data[[col_muni]]), 1, 2)
  ) |>
  filter(!is.na(faixa), !is.na(sexo), ANO_NOTIF %in% anos_principal)

casos_br <- sinan_br_classif |> group_by(ano = ANO_NOTIF, sexo, faixa) |>
  summarise(casos = n(), .groups = "drop") |> mutate(uf = "BR")
casos_mg <- sinan_br_classif |> filter(uf_resi == "31") |>
  group_by(ano = ANO_NOTIF, sexo, faixa) |>
  summarise(casos = n(), .groups = "drop") |> mutate(uf = "MG")
casos_se <- sinan_br_classif |> filter(uf_resi %in% c("31","32","33","35")) |>
  group_by(ano = ANO_NOTIF, sexo, faixa) |>
  summarise(casos = n(), .groups = "drop") |> mutate(uf = "SE")

casos_ref_faixa <- bind_rows(casos_br, casos_mg, casos_se)

# Casos TM por (faixa × sexo × ano) — censurado em ano_max_principal
casos_tm_faixa_sexo <- db$sinan_tri |>
  mutate(faixa = classificar_faixa(NU_IDADE_N),
         sexo  = case_when(CS_SEXO == "M" ~ "M", CS_SEXO == "F" ~ "F",
                           TRUE ~ NA_character_)) |>
  filter(!is.na(faixa), !is.na(sexo), ANO_NOTIF <= ano_max_principal) |>
  group_by(ano = ANO_NOTIF, sexo, faixa) |>
  summarise(casos = n(), .groups = "drop")

# Versão só por faixa (para compat com 02b)
casos_tm_faixa <- casos_tm_faixa_sexo |>
  group_by(ano, faixa) |>
  summarise(casos = sum(casos), .groups = "drop")

# Salvar para reúso pelo 02b/03
saveRDS(list(casos_ref_faixa     = casos_ref_faixa,
             casos_tm_faixa      = casos_tm_faixa,
             casos_tm_faixa_sexo = casos_tm_faixa_sexo),
        "casos_por_faixa.rds")

# Taxas específicas por (faixa × sexo × ano)
taxas_ref <- casos_ref_faixa |>
  left_join(pop_ref_faixa, by = c("ano","uf","sexo","faixa")) |>
  mutate(taxa = casos / pop) |>
  dplyr::select(ano, uf, sexo, faixa, taxa)

# Esperado TM — padronização indireta por idade × sexo
esp_long <- db$pop_tm_faixa_sexo |>
  inner_join(taxas_ref, by = c("ano","sexo","faixa")) |>
  mutate(esp = pop * taxa) |>
  group_by(ano, uf) |>
  summarise(esp = sum(esp, na.rm = TRUE), .groups = "drop")

esp_br <- esp_long |> filter(uf == "BR") |> dplyr::select(ano, esp_brasil = esp)
esp_mg <- esp_long |> filter(uf == "MG") |> dplyr::select(ano, esp_mg     = esp)

# Casos TM por ano (usados como numerador do SMR)
obs_tm_ano <- casos_tm_faixa_sexo |> group_by(ano) |>
  summarise(N_TM = sum(casos), .groups = "drop")

smr_df <- obs_tm_ano |>
  left_join(esp_br, by = "ano") |>
  left_join(esp_mg, by = "ano") |>
  rename(ANO = ano) |>
  mutate(
    smr_brasil = N_TM / esp_brasil,
    smr_mg     = N_TM / esp_mg,
    li_brasil  = mapply(function(o,e) ic_smr(o,e)$li, N_TM, esp_brasil),
    ls_brasil  = mapply(function(o,e) ic_smr(o,e)$ls, N_TM, esp_brasil),
    li_mg      = mapply(function(o,e) ic_smr(o,e)$li, N_TM, esp_mg),
    ls_mg      = mapply(function(o,e) ic_smr(o,e)$ls, N_TM, esp_mg)
  )

obs_total  <- sum(smr_df$N_TM,       na.rm = TRUE)
esp_br_tot <- sum(smr_df$esp_brasil, na.rm = TRUE)
esp_mg_tot <- sum(smr_df$esp_mg,     na.rm = TRUE)
smr_br_cum <- obs_total / esp_br_tot
smr_mg_cum <- obs_total / esp_mg_tot
ic_br_cum  <- ic_smr(obs_total, esp_br_tot)
ic_mg_cum  <- ic_smr(obs_total, esp_mg_tot)


## ----tabela-smr---------------------------------------------------------------
smr_df |>
  transmute(
    Ano                = ANO,
    `Obs.`             = N_TM,
    `Esp. (BR)`        = round(esp_brasil, 1),
    `SMR (BR)`         = fmt_smr(smr_brasil),
    `IC 95% (BR)`      = fmt_ic(li_brasil, ls_brasil),
    `Esp. (MG)`        = round(esp_mg, 1),
    `SMR (MG)`         = fmt_smr(smr_mg),
    `IC 95% (MG)`      = fmt_ic(li_mg, ls_mg)
  ) |>
  kable(caption = paste0("Tabela 1. SMR padronizado por idade × sexo (indireta) da meningite ",
                         "no Triângulo Mineiro vs Brasil e MG, ", periodo_lbl, "."),
        align = "c", row.names = FALSE)


## ----sanity-check-resumo, results='asis'--------------------------------------
# Sanity check — copie estes valores para o RESUMO/ABSTRACT.
# Se divergirem do que está no manuscrito, corrija o manuscrito.
cat("**SANITY CHECK PARA O RESUMO**\n\n")
cat(sprintf("- Observados (com sexo informado): **%d**\n", obs_total))
cat(sprintf("- Esperados (BR): **%s**\n", fmt1(esp_br_tot)))
cat(sprintf("- Esperados (MG): **%s**\n", fmt1(esp_mg_tot)))
cat(sprintf("- SMR (BR) cumulativo: **%s** (IC95%% %s a %s)\n",
            fmt_smr(smr_br_cum), fmt_smr(ic_br_cum$li), fmt_smr(ic_br_cum$ls)))
cat(sprintf("- SMR (MG) cumulativo: **%s** (IC95%% %s a %s)\n",
            fmt_smr(smr_mg_cum), fmt_smr(ic_mg_cum$li), fmt_smr(ic_mg_cum$ls)))
cat(sprintf("- Janela analítica principal: **%d–%d** (n_anos = %d)\n",
            min(anos_principal), max(anos_principal), length(anos_principal)))


## ----completude-2025, results='asis'------------------------------------------
# Evidência de que 2025 está completo no banco (justifica a inclusão na análise principal).
dt_extracao <- as.Date("2026-05-29")  # ajuste para a data real da extração
ult_notif <- max(as.Date(db$sinan_tri$DT_NOTIFIC), na.rm = TRUE)
ult_digit <- max(as.Date(db$sinan_tri$DT_DIGITA), na.rm = TRUE)

mensal_25_tm <- as.data.frame(table(format(
  as.Date(db$sinan_tri$DT_NOTIFIC[db$sinan_tri$ANO_NOTIF == 2025]),
  "%Y-%m")))
names(mensal_25_tm) <- c("Mês", "Casos TM")

cat(sprintf("- Data da extração: **%s**\n", format(dt_extracao, "%d/%m/%Y")))
cat(sprintf("- Última DT_NOTIFIC no banco: **%s**\n", format(ult_notif, "%d/%m/%Y")))
cat(sprintf("- Última DT_DIGITA no banco: **%s**\n", format(ult_digit, "%d/%m/%Y")))
cat(sprintf("- Defasagem entre fim do período (31/12/2025) e extração: **%d dias**\n\n",
            as.integer(dt_extracao - as.Date("2025-12-31"))))
kable(mensal_25_tm, caption = "Distribuição mensal das notificações de 2025 no Triângulo Mineiro.",
      align = "c", row.names = FALSE)


## ----smr-sens-estrutura, include=FALSE----------------------------------------
# Sensibilidade: estrutura etária por sexo interpolada linearmente entre os
# Censos de 2010 e 2022 (extrapolada para 2023-2025), em vez do Censo 2022 fixo.
# Responde à crítica de que a estrutura etária fixa em 2022 ignora envelhecimento.

if (!file.exists("pop_tm_faixa_sexo_censo2010.rds")) {
  warning("pop_tm_faixa_sexo_censo2010.rds não encontrado; pulando sensibilidade.")
  smr_sens_ok <- FALSE
} else {
  smr_sens_ok <- TRUE
  pop_2010 <- readRDS("pop_tm_faixa_sexo_censo2010.rds")

  # Proporções de ancoragem 2010 e 2022 (TM agregado, por sexo × faixa)
  prop_2010 <- pop_2010 |>
    mutate(prop_2010 = pop_2010 / sum(pop_2010)) |>
    dplyr::select(sexo, faixa, prop_2010)

  prop_2022 <- db$pop_tm_faixa_sexo |> filter(ano == 2022) |>
    group_by(sexo, faixa) |>
    summarise(pop = sum(pop), .groups = "drop") |>
    mutate(prop_2022 = pop / sum(pop)) |>
    dplyr::select(sexo, faixa, prop_2022)

  prop_anchor <- inner_join(prop_2010, prop_2022, by = c("sexo","faixa"))

  # População total TM por ano (mantém o total do TCU; só muda a distribuição etária)
  pop_anos_tm <- db$pop_tm_faixa_sexo |>
    group_by(ano) |>
    summarise(pop_total = sum(pop), .groups = "drop")

  # Interpolação/extrapolação linear ano a ano
  prop_estimada <- expand.grid(
      ano = anos_principal, sexo = c("F","M"),
      faixa = c("<5","5-19","20-49","50+"),
      stringsAsFactors = FALSE) |>
    left_join(prop_anchor, by = c("sexo","faixa")) |>
    mutate(t = (ano - 2010) / (2022 - 2010),
           prop_alt = prop_2010 + t * (prop_2022 - prop_2010)) |>
    group_by(ano) |>
    mutate(prop_alt = prop_alt / sum(prop_alt)) |>  # renormaliza
    ungroup()

  pop_alt <- prop_estimada |>
    left_join(pop_anos_tm, by = "ano") |>
    mutate(pop = prop_alt * pop_total) |>
    dplyr::select(ano, sexo, faixa, pop)

  # Recalcula esperados com a estrutura alternativa
  esp_alt <- pop_alt |>
    inner_join(taxas_ref, by = c("ano","sexo","faixa")) |>
    mutate(esp = pop * taxa) |>
    group_by(ano, uf) |> summarise(esp = sum(esp, na.rm = TRUE), .groups = "drop")

  esp_br_alt <- sum(esp_alt$esp[esp_alt$uf == "BR"], na.rm = TRUE)
  esp_mg_alt <- sum(esp_alt$esp[esp_alt$uf == "MG"], na.rm = TRUE)
  smr_br_alt <- obs_total / esp_br_alt
  smr_mg_alt <- obs_total / esp_mg_alt
  ic_br_alt  <- ic_smr(obs_total, esp_br_alt)
  ic_mg_alt  <- ic_smr(obs_total, esp_mg_alt)
}

