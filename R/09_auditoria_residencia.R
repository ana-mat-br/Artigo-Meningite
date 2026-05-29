# =============================================================================
# 09_auditoria_residencia — Auditoria empírica do campo de residência (SINAN)
#
# Origem: check_residencia_TM.R (refatorado para usar dados_base.rds)
# Depende: dados_base.rds (01)
# Gera:    auditoria_residencia/{A,B,C,D}_*.csv
#
# Arbitra entre 3 hipóteses sobre a concentração de notificações em
# Uberaba/Uberlândia: (a) burden residencial real; (b) sensibilidade
# diagnóstica diferencial; (c) erro de preenchimento de ID_MN_RESI.
# =============================================================================


suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(stringr); library(readr)
})

if (!file.exists("dados_base.rds"))
  stop("Rode antes: source('01_dados_setup.R')")
db <- readRDS("dados_base.rds")

codigos_sinan_tm <- db$codigos_sinan_6d
polos <- c("Uberaba" = "317010", "Uberlândia" = "317020")

# Banco BR completo (não só TM), para identificar quem foi notificado nos polos
# mas reside em outro lugar.
sinan_br <- db$sinan_br

# Padronizar campos de UF/município de notificação e residência
# Variáveis do SINAN-Mening:
#   ID_MUNICIP   — município de notificação (6 dígitos)
#   SG_UF_NOT    — UF de notificação
#   ID_MN_RESI   — município de residência (6 dígitos)
#   SG_UF        — UF de residência
col_notif_mun <- intersect(c("ID_MUNICIP"), names(sinan_br))[1]
col_notif_uf  <- intersect(c("SG_UF_NOT", "SG_UF_INF"), names(sinan_br))[1]
col_resi_uf   <- intersect(c("SG_UF", "SG_UF_RES"), names(sinan_br))[1]

cat("Colunas detectadas:\n")
cat("  notificação município:", col_notif_mun, "\n")
cat("  notificação UF       :", col_notif_uf,  "\n")
cat("  residência UF        :", col_resi_uf,   "\n\n")

aux <- sinan_br |>
  mutate(
    mun_not  = str_pad(as.character(.data[[col_notif_mun]]), 6, pad = "0"),
    mun_res  = cod_sinan,  # já padronizado em 01_dados_setup
    uf_not   = as.character(.data[[col_notif_uf]]),
    uf_res   = as.character(.data[[col_resi_uf]]),
    notif_em_polo = mun_not %in% polos,
    notif_em_TM   = mun_not %in% codigos_sinan_tm,
    resid_em_polo = mun_res %in% polos,
    resid_em_TM   = mun_res %in% codigos_sinan_tm,
    resid_em_MG   = uf_res  == "31"
  )

# ---- TABELA A: residência (UF) dos casos notificados nos polos --------------
cat("============================================================\n")
cat("TABELA A — UF de residência dos casos notificados em Uberaba/Uberlândia\n")
cat("============================================================\n")
tabA <- aux |>
  filter(notif_em_polo) |>
  group_by(`Município de notificação` = case_when(
              mun_not == polos["Uberaba"]    ~ "Uberaba",
              mun_not == polos["Uberlândia"] ~ "Uberlândia"),
           `UF de residência` = ifelse(is.na(uf_res), "Ignorada", uf_res)) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(`Município de notificação`) |>
  mutate(`%` = round(100 * n / sum(n), 1)) |>
  ungroup() |>
  arrange(`Município de notificação`, desc(n))
print(as.data.frame(tabA))

# ---- TABELA B: casos atribuídos ao TM com residência em UF ≠ MG -------------
cat("\n============================================================\n")
cat("TABELA B — Notificados no TM por UF de residência\n")
cat("============================================================\n")
tabB <- aux |>
  filter(notif_em_TM) |>
  mutate(grupo_res = case_when(
    is.na(uf_res)                 ~ "Ignorada",
    resid_em_polo                 ~ "Reside no polo (Uberaba/Uberlândia)",
    resid_em_TM                   ~ "Reside no TM (não-polo)",
    resid_em_MG                   ~ "Reside em MG fora do TM",
    TRUE                          ~ paste0("Reside em ", uf_res, " (fora de MG)")
  )) |>
  count(grupo_res, name = "n") |>
  mutate(pct = round(100 * n / sum(n), 1)) |>
  arrange(desc(n))
print(as.data.frame(tabB))

# ---- TABELA C: top municípios de notificação por % externos -----------------
cat("\n============================================================\n")
cat("TABELA C — Top 10 municípios de notificação no TM com % residentes externos\n")
cat("============================================================\n")
tabC <- aux |>
  filter(notif_em_TM) |>
  group_by(mun_not) |>
  summarise(
    n_total     = n(),
    n_res_local = sum(mun_res == mun_not, na.rm = TRUE),
    n_res_outro_TM = sum(resid_em_TM & mun_res != mun_not, na.rm = TRUE),
    n_res_extTM = sum(!resid_em_TM, na.rm = TRUE),
    pct_externo_TM = round(100 * n_res_extTM / n_total, 1),
    .groups = "drop"
  ) |>
  arrange(desc(n_total)) |>
  slice_head(n = 10)
print(as.data.frame(tabC))

# ---- TABELA D: reverso — residentes do TM, onde foram notificados? ----------
cat("\n============================================================\n")
cat("TABELA D — Reverso: residentes do TM por local de notificação\n")
cat("============================================================\n")
tabD <- aux |>
  filter(resid_em_TM) |>
  mutate(grupo_not = case_when(
    is.na(mun_not)        ~ "Ignorado",
    notif_em_polo         ~ "Notificado no polo (Uberaba/Uberlândia)",
    notif_em_TM           ~ "Notificado no TM (não-polo)",
    uf_not == "31"        ~ "Notificado em MG fora do TM",
    TRUE                  ~ paste0("Notificado em ", uf_not, " (fora de MG)")
  )) |>
  count(grupo_not, name = "n") |>
  mutate(pct = round(100 * n / sum(n), 1)) |>
  arrange(desc(n))
print(as.data.frame(tabD))

# ---- INTERPRETAÇÃO PARA ARBITRAGEM ------------------------------------------
n_TM_total <- sum(aux$notif_em_TM)
n_TM_polo  <- sum(aux$notif_em_TM & aux$resid_em_polo)
n_TM_extMG <- sum(aux$notif_em_TM & !aux$resid_em_MG, na.rm = TRUE)
n_TM_outroMG <- sum(aux$notif_em_TM & aux$resid_em_MG & !aux$resid_em_TM, na.rm = TRUE)

cat("\n============================================================\n")
cat("ARBITRAGEM ENTRE HIPÓTESES (residência × captação)\n")
cat("============================================================\n")
cat(sprintf("Total notificado no TM: %d\n", n_TM_total))
cat(sprintf("  • residentes nos polos (Uberaba/Uberlândia): %d (%.1f%%)\n",
            n_TM_polo, 100*n_TM_polo/n_TM_total))
cat(sprintf("  • residentes em outros municípios de MG (fora TM): %d (%.1f%%)\n",
            n_TM_outroMG, 100*n_TM_outroMG/n_TM_total))
cat(sprintf("  • residentes em outras UFs: %d (%.1f%%)\n",
            n_TM_extMG, 100*n_TM_extMG/n_TM_total))

cat("\nLEITURA:\n")
cat("- Se 'extMG' for > 5%: hipótese (c) (vazamento) é plausível.\n")
cat("- Se 'outroMG' for > 5%: vazamento INTRA-MG também contribui.\n")
cat("- Se polos concentram 76% E são residentes do polo: hipóteses (a)/(b) dominam.\n")
cat("- Diferença entre Tabela A (UF residência nos polos) e Tabela B (composição TM)\n")
cat("  indica fluxo migratório/atendimento que pode reposicionar a tese central.\n")

# ---- Exportar -----------------------------------------------------------
dir.create("auditoria_residencia", showWarnings = FALSE)
write_csv(tabA, "auditoria_residencia/A_residencia_polos.csv")
write_csv(tabB, "auditoria_residencia/B_composicao_TM.csv")
write_csv(tabC, "auditoria_residencia/C_top_municipios_externos.csv")
write_csv(tabD, "auditoria_residencia/D_reverso_residentes_TM.csv")
cat("\nCSVs salvos em ./auditoria_residencia/\n")
