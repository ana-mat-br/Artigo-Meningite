# =============================================================================
# 06_regressao_poisson — Regressão de Poisson modificada (HC3): óbito e encerramento
#
# Origem: Extraído de 05_regressao_poisson.Rmd via knitr::purl()
# Depende: dados_base.rds (01)
# Gera:    Modelos 1 (óbito) e 2 (encerramento ≥30d), sem cross-inclusion + sensibilidade
#
# Projeto: Análise da meningite no Triângulo Mineiro (2015–2025)
# =============================================================================
## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)


## ----pacotes------------------------------------------------------------------
pkgs <- c("read.dbc","dplyr","tidyr","stringr","sandwich","lmtest","knitr","car")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE))
    install.packages(p, repos = "https://cloud.r-project.org")
}
invisible(lapply(pkgs, library, character.only = TRUE))


## ----dados, include=FALSE-----------------------------------------------------
# Aliases anti-mascaramento (MASS::select etc.)
select <- dplyr::select; filter <- dplyr::filter; mutate <- dplyr::mutate
summarise <- dplyr::summarise; group_by <- dplyr::group_by

# Carregar sinan_tri pré-processado pelo 01_dados_setup.R (CLASSI_FIN=="1" já aplicado).
if (!file.exists("dados_base.rds")) stop("Rode antes: source('01_dados_setup.R')")
db <- readRDS("dados_base.rds")
list2env(db$config, envir = environment())

perfil <- db$sinan_tri |>
  mutate(
    sexo        = case_when(CS_SEXO == "M" ~ "Masculino",
                            CS_SEXO == "F" ~ "Feminino",
                            TRUE           ~ NA_character_),
    unidade_idade = floor(as.integer(NU_IDADE_N) / 1000),
    valor_idade   = as.integer(NU_IDADE_N) %% 1000,
    idade_anos    = if_else(unidade_idade == 4, valor_idade, 0L),
    faixa4 = case_when(
      idade_anos <  5              ~ "<5 anos",
      idade_anos >= 5  & idade_anos < 20 ~ "5-19 anos",
      idade_anos >= 20 & idade_anos < 50 ~ "20-49 anos",
      idade_anos >= 50             ~ "50+ anos",
      TRUE                         ~ NA_character_
    ),
    desfecho    = case_when(
      EVOLUCAO == "1" ~ "Cura",
      EVOLUCAO == "2" ~ "Óbito por meningite",
      EVOLUCAO == "3" ~ "Óbito por outra causa",
      TRUE            ~ "Ignorado"
    ),
    obito = as.integer(EVOLUCAO == "2"),
    etiologia = case_when(
      CLA_ME_ETI == "42" ~ "Meningocócica",
      CLA_ME_ETI == "43" ~ "H. influenzae",
      CLA_ME_ETI == "44" ~ "Pneumocócica",
      CLA_ME_ETI == "47" ~ "Outras bacterianas",
      CLA_ME_ETI == "48" ~ "Viral",
      CLA_ME_ETI == "50" ~ "Criptocócica",
      CLA_ME_ETI == "52" ~ "Outras fúngicas",
      CLA_ME_ETI == "64" ~ "Não determinada",
      TRUE                ~ NA_character_
    ),
    raca  = case_when(
      CS_RACA == "1"          ~ "Branca",
      CS_RACA == "2"          ~ "Preta",
      CS_RACA == "4"          ~ "Parda",
      CS_RACA %in% c("3","5") ~ "Outras",
      TRUE                    ~ NA_character_
    ),
    dt_sin_pri  = as.Date(as.integer(DT_SIN_PRI), origin = "1970-01-01"),
    dt_encerra  = as.Date(as.integer(DT_ENCERRA),  origin = "1970-01-01"),
    dias_encerr = as.integer(dt_encerra - dt_sin_pri)
  ) |>
  filter(desfecho %in% c("Cura", "Óbito por meningite"))

# ── Funções auxiliares ─────────────────────────────────────────────────────────
fmt_p <- function(p) {
  dplyr::case_when(
    is.na(p)  ~ "NA",
    p < 0.001 ~ "<0,001",
    TRUE      ~ formatC(p, format = "f", digits = 3, decimal.mark = ",")
  )
}

rodar_poisson <- function(formula, dados) {
  m <- glm(formula, family = poisson(link = "log"), data = dados)
  v <- tryCatch(vcovHC(m, type = "HC3"), error = function(e) vcov(m))
  c <- coeftest(m, vcov = v)
  data.frame(var = rownames(c), beta = c[,1], ep = c[,2], p = c[,4],
             stringsAsFactors = FALSE) |>
    filter(var != "(Intercept)") |>
    mutate(
      RR    = exp(beta),
      IC_li = exp(beta - 1.96 * ep),
      IC_ls = exp(beta + 1.96 * ep),
      `RR (IC 95%)` = paste0(
        formatC(RR,    format="f", digits=2, decimal.mark=","), " (",
        formatC(IC_li, format="f", digits=2, decimal.mark=","), "–",
        formatC(IC_ls, format="f", digits=2, decimal.mark=","), ")"),
      `Valor de p` = fmt_p(p),
      Variável = var |>
        stringr::str_replace("sexo_rMasculino",    "Sexo: Masculino") |>
        stringr::str_replace("faixa_r<5 anos",     "Faixa etária: <5 anos") |>
        stringr::str_replace("faixa_r5-19 anos",   "Faixa etária: 5–19 anos") |>
        stringr::str_replace("faixa_r50\\+ anos",  "Faixa etária: 50+ anos") |>
        stringr::str_replace("raca2Não branca",    "Raça/cor: Não branca") |>
        stringr::str_replace("longoTRUE",          "Enc. ≥ 30 dias: Sim") |>
        stringr::str_replace("^longo$",            "Enc. ≥ 30 dias: Sim") |>
        stringr::str_replace("obitoTRUE",          "Óbito por meningite: Sim") |>
        stringr::str_replace("^obito$",            "Óbito por meningite: Sim") |>
        # Rótulos para fase pandêmica
        stringr::str_replace("fase_pandIntra \\(2020-2021\\)",
                             "Fase pandêmica: Intra (2020-2021)") |>
        stringr::str_replace("fase_pandPós \\(2022-2025\\)",
                             "Fase pandêmica: Pós (2022-2025)")
    ) |>
    select(Variável, `RR (IC 95%)`, `Valor de p`) |>
    tibble::as_tibble()  # tibble não carrega rownames
}

# ── Base analítica ─────────────────────────────────────────────────────────────
base_ef <- perfil |>
  mutate(
    sexo_r  = factor(sexo,   levels = c("Feminino","Masculino")),
    faixa_r = factor(faixa4, levels = c("20-49 anos","<5 anos","5-19 anos","50+ anos")),
    raca2   = factor(if_else(raca == "Branca", "Branca", "Não branca"),
                     levels = c("Branca","Não branca")),
    longo   = if_else(!is.na(dias_encerr) & dias_encerr >= 0 & dias_encerr <= 365,
                      as.integer(dias_encerr >= 30), NA_integer_),
    # Variáveis temporais — disponíveis após ampliação da série para 11 anos
    fase_pand = factor(case_when(
      ANO_NOTIF %in% 2015:2019 ~ "Pré (2015-2019)",
      ANO_NOTIF %in% 2020:2021 ~ "Intra (2020-2021)",
      ANO_NOTIF %in% 2022:2025 ~ "Pós (2022-2025)"),
      levels = c("Pré (2015-2019)", "Intra (2020-2021)", "Pós (2022-2025)"))
  ) |>
  filter(!is.na(sexo_r), !is.na(faixa_r),
         raca %in% c("Branca","Parda","Preta"),
         !is.na(longo))

n_base   <- nrow(base_ef)
obitos   <- sum(base_ef$obito)
longos   <- sum(base_ef$longo)

# ── Fluxo de exclusões ────────────────────────────────────────────────────────
# n_confirmados vem do dados_base: já é o número de casos confirmados residentes no TM
n_confirmados <- nrow(db$sinan_tri)
n_perfil       <- nrow(perfil)
n_raca_valida  <- perfil |> filter(raca %in% c("Branca","Parda","Preta")) |> nrow()
n_excl_desf    <- n_confirmados - n_perfil
n_excl_raca    <- n_perfil - n_raca_valida
n_excl_datas   <- n_raca_valida - n_base
n_raca_ignorada    <- sum(is.na(perfil$raca))
n_raca_outras      <- sum(perfil$raca == "Outras", na.rm = TRUE)

# ── Etiologia — completude geral e por ano ────────────────────────────────────
n_eti_valida  <- sum(!is.na(perfil$etiologia))
pct_eti_miss  <- round((1 - n_eti_valida / n_perfil) * 100, 1)

eti_por_ano <- perfil |>
  group_by(ANO_NOTIF) |>
  summarise(
    n_total = n(),
    n_eti   = sum(!is.na(etiologia)),
    pct_eti = round(n_eti / n_total * 100, 1),
    .groups = "drop"
  )
anos_sem_eti <- eti_por_ano |>
  filter(pct_eti == 0) |>
  pull(ANO_NOTIF)
anos_com_eti <- eti_por_ano |>
  filter(pct_eti > 0) |>
  pull(ANO_NOTIF)

# ── Modelo 1 PRINCIPAL — desfecho: óbito (sem `longo` para evitar circularidade)
# Preditores: demográficos + fase pandêmica
mod1     <- glm(obito ~ sexo_r + faixa_r + raca2 + fase_pand,
                family = poisson(link = "log"), data = base_ef)
vcov1    <- vcovHC(mod1, type = "HC3")
coef1    <- coeftest(mod1, vcov = vcov1)
epv1     <- round(obitos / (length(coef(mod1)) - 1), 1)
dev_gl1  <- round(mod1$deviance / mod1$df.residual, 2)
aic1     <- round(AIC(mod1), 1)
vif1     <- round(max(car::vif(mod1)), 2)

# ── Modelo 1 SENSIBILIDADE — incluindo `longo` (cross-inclusion) ───────────────
mod1_sens <- glm(obito ~ sexo_r + faixa_r + raca2 + fase_pand + longo,
                 family = poisson(link = "log"), data = base_ef)
vcov1_sens <- vcovHC(mod1_sens, type = "HC3")
coef1_sens <- coeftest(mod1_sens, vcov = vcov1_sens)

# ── Modelo 2 PRINCIPAL — desfecho: encerramento ≥ 30 dias (sem `obito`) ────────
mod2     <- glm(longo ~ sexo_r + faixa_r + raca2 + fase_pand,
                family = poisson(link = "log"), data = base_ef)
vcov2    <- vcovHC(mod2, type = "HC3")
coef2    <- coeftest(mod2, vcov = vcov2)
epv2     <- round(longos / (length(coef(mod2)) - 1), 1)
dev_gl2  <- round(mod2$deviance / mod2$df.residual, 2)
aic2     <- round(AIC(mod2), 1)

# ── Modelo 2 SENSIBILIDADE — incluindo `obito` (cross-inclusion) ──────────────
mod2_sens <- glm(longo ~ sexo_r + faixa_r + raca2 + fase_pand + obito,
                 family = poisson(link = "log"), data = base_ef)
vcov2_sens <- vcovHC(mod2_sens, type = "HC3")
coef2_sens <- coeftest(mod2_sens, vcov = vcov2_sens)
vif2     <- round(max(car::vif(mod2)), 2)

# ── Valores para interpretação — Modelo 1 ──────────────────────────────────────
rr_50      <- formatC(exp(coef1["faixa_r50+ anos",1]), format="f", digits=2, decimal.mark=",")
ic_li_50   <- formatC(exp(coef1["faixa_r50+ anos",1] - 1.96*coef1["faixa_r50+ anos",2]), format="f", digits=2, decimal.mark=",")
ic_ls_50   <- formatC(exp(coef1["faixa_r50+ anos",1] + 1.96*coef1["faixa_r50+ anos",2]), format="f", digits=2, decimal.mark=",")
p_50       <- fmt_p(coef1["faixa_r50+ anos",4])

rr_m5      <- formatC(exp(coef1["faixa_r<5 anos",1]), format="f", digits=2, decimal.mark=",")
ic_li_m5   <- formatC(exp(coef1["faixa_r<5 anos",1] - 1.96*coef1["faixa_r<5 anos",2]), format="f", digits=2, decimal.mark=",")
ic_ls_m5   <- formatC(exp(coef1["faixa_r<5 anos",1] + 1.96*coef1["faixa_r<5 anos",2]), format="f", digits=2, decimal.mark=",")
p_m5       <- fmt_p(coef1["faixa_r<5 anos",4])

rr_519     <- formatC(exp(coef1["faixa_r5-19 anos",1]), format="f", digits=2, decimal.mark=",")
ic_li_519  <- formatC(exp(coef1["faixa_r5-19 anos",1] - 1.96*coef1["faixa_r5-19 anos",2]), format="f", digits=2, decimal.mark=",")
ic_ls_519  <- formatC(exp(coef1["faixa_r5-19 anos",1] + 1.96*coef1["faixa_r5-19 anos",2]), format="f", digits=2, decimal.mark=",")
p_519      <- fmt_p(coef1["faixa_r5-19 anos",4])

rr_masc    <- formatC(exp(coef1["sexo_rMasculino",1]), format="f", digits=2, decimal.mark=",")
ic_li_masc <- formatC(exp(coef1["sexo_rMasculino",1] - 1.96*coef1["sexo_rMasculino",2]), format="f", digits=2, decimal.mark=",")
ic_ls_masc <- formatC(exp(coef1["sexo_rMasculino",1] + 1.96*coef1["sexo_rMasculino",2]), format="f", digits=2, decimal.mark=",")
p_masc     <- fmt_p(coef1["sexo_rMasculino",4])

rr_nb      <- formatC(exp(coef1["raca2Não branca",1]), format="f", digits=2, decimal.mark=",")
ic_li_nb   <- formatC(exp(coef1["raca2Não branca",1] - 1.96*coef1["raca2Não branca",2]), format="f", digits=2, decimal.mark=",")
ic_ls_nb   <- formatC(exp(coef1["raca2Não branca",1] + 1.96*coef1["raca2Não branca",2]), format="f", digits=2, decimal.mark=",")
p_nb       <- fmt_p(coef1["raca2Não branca",4])

# Estimativa de `longo` SÓ do modelo de sensibilidade (cross-included).
# No modelo principal de óbito (mod1), `longo` não é incluído para evitar
# circularidade — manter aqui apenas como referência de sensibilidade.
rr_longo1  <- formatC(exp(coef1_sens["longo",1]), format="f", digits=2, decimal.mark=",")
ic_li_l1   <- formatC(exp(coef1_sens["longo",1] - 1.96*coef1_sens["longo",2]), format="f", digits=2, decimal.mark=",")
ic_ls_l1   <- formatC(exp(coef1_sens["longo",1] + 1.96*coef1_sens["longo",2]), format="f", digits=2, decimal.mark=",")
p_longo1   <- fmt_p(coef1_sens["longo",4])

# ── Valores para interpretação — Modelo 2 ──────────────────────────────────────
rr2_m5     <- formatC(exp(coef2["faixa_r<5 anos",1]), format="f", digits=2, decimal.mark=",")
ic_li2_m5  <- formatC(exp(coef2["faixa_r<5 anos",1] - 1.96*coef2["faixa_r<5 anos",2]), format="f", digits=2, decimal.mark=",")
ic_ls2_m5  <- formatC(exp(coef2["faixa_r<5 anos",1] + 1.96*coef2["faixa_r<5 anos",2]), format="f", digits=2, decimal.mark=",")
p2_m5      <- fmt_p(coef2["faixa_r<5 anos",4])

rr2_519    <- formatC(exp(coef2["faixa_r5-19 anos",1]), format="f", digits=2, decimal.mark=",")
ic_li2_519 <- formatC(exp(coef2["faixa_r5-19 anos",1] - 1.96*coef2["faixa_r5-19 anos",2]), format="f", digits=2, decimal.mark=",")
ic_ls2_519 <- formatC(exp(coef2["faixa_r5-19 anos",1] + 1.96*coef2["faixa_r5-19 anos",2]), format="f", digits=2, decimal.mark=",")
p2_519     <- fmt_p(coef2["faixa_r5-19 anos",4])

rr2_50     <- formatC(exp(coef2["faixa_r50+ anos",1]), format="f", digits=2, decimal.mark=",")
ic_li2_50  <- formatC(exp(coef2["faixa_r50+ anos",1] - 1.96*coef2["faixa_r50+ anos",2]), format="f", digits=2, decimal.mark=",")
ic_ls2_50  <- formatC(exp(coef2["faixa_r50+ anos",1] + 1.96*coef2["faixa_r50+ anos",2]), format="f", digits=2, decimal.mark=",")
p2_50      <- fmt_p(coef2["faixa_r50+ anos",4])

rr2_masc   <- formatC(exp(coef2["sexo_rMasculino",1]), format="f", digits=2, decimal.mark=",")
ic_li2_masc<- formatC(exp(coef2["sexo_rMasculino",1] - 1.96*coef2["sexo_rMasculino",2]), format="f", digits=2, decimal.mark=",")
ic_ls2_masc<- formatC(exp(coef2["sexo_rMasculino",1] + 1.96*coef2["sexo_rMasculino",2]), format="f", digits=2, decimal.mark=",")
p2_masc    <- fmt_p(coef2["sexo_rMasculino",4])

rr2_nb     <- formatC(exp(coef2["raca2Não branca",1]), format="f", digits=2, decimal.mark=",")
ic_li2_nb  <- formatC(exp(coef2["raca2Não branca",1] - 1.96*coef2["raca2Não branca",2]), format="f", digits=2, decimal.mark=",")
ic_ls2_nb  <- formatC(exp(coef2["raca2Não branca",1] + 1.96*coef2["raca2Não branca",2]), format="f", digits=2, decimal.mark=",")
p2_nb      <- fmt_p(coef2["raca2Não branca",4])

# Mesma lógica para `obito` no modelo de encerramento: só do modelo de sensibilidade.
rr2_obito  <- formatC(exp(coef2_sens["obito",1]), format="f", digits=2, decimal.mark=",")
ic_li2_ob  <- formatC(exp(coef2_sens["obito",1] - 1.96*coef2_sens["obito",2]), format="f", digits=2, decimal.mark=",")
ic_ls2_ob  <- formatC(exp(coef2_sens["obito",1] + 1.96*coef2_sens["obito",2]), format="f", digits=2, decimal.mark=",")
p2_obito   <- fmt_p(coef2_sens["obito",4])


## ----tabela-etiologia---------------------------------------------------------
eti_ordem <- c("Meningocócica","Pneumocócica","H. influenzae",
               "Outras bacterianas","Viral","Criptocócica",
               "Outras fúngicas","Não determinada")

tab_eti <- perfil |>
  filter(!is.na(etiologia), !is.na(faixa4)) |>
  mutate(
    faixa4    = factor(faixa4, levels = c("<5 anos","5-19 anos","20-49 anos","50+ anos")),
    etiologia = factor(etiologia, levels = eti_ordem)
  ) |>
  group_by(etiologia, faixa4) |>
  summarise(n = n(), .groups = "drop") |>
  tidyr::pivot_wider(names_from = faixa4, values_from = n, values_fill = 0) |>
  arrange(etiologia) |>
  mutate(Total = rowSums(across(where(is.numeric))),
         etiologia = as.character(etiologia))

# adiciona linha de total
totais <- c("Total", colSums(tab_eti[, -1]))
tab_eti <- rbind(tab_eti, totais)

# converte para numérico exceto coluna de nome
tab_eti[, -1] <- lapply(tab_eti[, -1], as.integer)

names(tab_eti)[1] <- "Etiologia"

kable(tab_eti,
      caption = paste0(
        "Tabela E. Distribuição etiológica por faixa etária — casos confirmados de meningite ",
        "com etiologia informada no SINAN, Triângulo Mineiro, 2015–2025 (n = ",
        n_eti_valida, " de ", n_perfil, " casos com desfecho definido; ",
        pct_eti_miss, "% sem etiologia registrada)."
      ),
      align = c("l","c","c","c","c","c"))


## ----tabela-descritiva--------------------------------------------------------
desc_cat <- function(var, rotulo, dados = base_ef) {
  dados |>
    mutate(cat = as.character(.data[[var]])) |>
    filter(!is.na(cat)) |>
    group_by(Variável = rotulo, Categoria = cat) |>
    summarise(
      N                = n(),
      `Óbitos`         = sum(obito),
      `% óbito`        = round(mean(obito) * 100, 1),
      `Enc. ≥ 30 dias` = sum(longo),
      `% enc. ≥ 30`    = round(mean(longo) * 100, 1),
      .groups = "drop"
    )
}

bind_rows(
  desc_cat("sexo_r",  "Sexo"),
  desc_cat("faixa_r", "Faixa etária") |>
    mutate(Categoria = factor(Categoria,
      levels = c("<5 anos","5-19 anos","20-49 anos","50+ anos"))) |>
    arrange(Categoria) |>
    mutate(Categoria = as.character(Categoria)),
  desc_cat("raca2",   "Raça/cor")
) |>
  kable(caption = paste0(
    "Tabela 1. Distribuição de casos, óbitos e encerramento prolongado por categoria ",
    "dos preditores — Triângulo Mineiro, 2015–2025 (n = ", n_base, "). ",
    "Percentuais calculados dentro de cada categoria."
  ), align = c("l","l","c","c","c","c","c"))


## ----modelo1------------------------------------------------------------------
# Modelo principal: sem `longo` (evita circularidade); inclui fase pandêmica
rodar_poisson(obito ~ sexo_r + faixa_r + raca2 + fase_pand, base_ef) |>
  kable(caption = paste0(
    "Tabela 2. Razões de risco (RR) para óbito por meningite. ",
    "Regressão de Poisson modificada com variância robusta (sandwich HC3). ",
    "Triângulo Mineiro, ", periodo_lbl, ". n = ", n_base, " casos; ",
    obitos, " óbitos; EPV = ", epv1, ". ",
    "Categorias de referência: Sexo = Feminino; Faixa etária = 20–49 anos; ",
    "Raça/cor = Branca; Fase pandêmica = Pré (2015-2019)."
  ), align = c("l","c","c"))


## ----modelo1-sens-etio, include=FALSE-----------------------------------------
# Sensibilidade para o RR de óbito em ≥50 anos: restringir a casos com
# etiologia identificada e ajustar por grupo etiológico (Viral/Bacteriana/Outras).
# Responde à crítica de que o efeito da idade pode ser confundido por
# composição etiológica (idosos têm mais meningite bacteriana, mais letal).
base_etio_sens <- base_ef |>
  filter(!is.na(etiologia), etiologia != "Não determinada") |>
  mutate(eti_grp = factor(case_when(
    etiologia %in% c("Meningocócica","H. influenzae","Pneumocócica","Outras bacterianas") ~ "Bacteriana",
    etiologia == "Viral" ~ "Viral",
    TRUE ~ "Outras"),
    levels = c("Viral","Bacteriana","Outras")))

n_etio_sens   <- nrow(base_etio_sens)
obitos_etio_sens <- sum(base_etio_sens$obito)

mod1_etio <- glm(obito ~ sexo_r + faixa_r + raca2 + fase_pand + eti_grp,
                 family = poisson(link = "log"), data = base_etio_sens)
vcov1_etio <- vcovHC(mod1_etio, type = "HC3")
coef1_etio <- coeftest(mod1_etio, vcov = vcov1_etio)

rr_50_etio    <- formatC(exp(coef1_etio["faixa_r50+ anos",1]),
                         format="f", digits=2, decimal.mark=",")
ic_li_50_etio <- formatC(exp(coef1_etio["faixa_r50+ anos",1]
                             - 1.96*coef1_etio["faixa_r50+ anos",2]),
                         format="f", digits=2, decimal.mark=",")
ic_ls_50_etio <- formatC(exp(coef1_etio["faixa_r50+ anos",1]
                             + 1.96*coef1_etio["faixa_r50+ anos",2]),
                         format="f", digits=2, decimal.mark=",")
p_50_etio     <- fmt_p(coef1_etio["faixa_r50+ anos",4])

rr_bact       <- formatC(exp(coef1_etio["eti_grpBacteriana",1]),
                         format="f", digits=2, decimal.mark=",")
ic_li_bact    <- formatC(exp(coef1_etio["eti_grpBacteriana",1]
                             - 1.96*coef1_etio["eti_grpBacteriana",2]),
                         format="f", digits=2, decimal.mark=",")
ic_ls_bact    <- formatC(exp(coef1_etio["eti_grpBacteriana",1]
                             + 1.96*coef1_etio["eti_grpBacteriana",2]),
                         format="f", digits=2, decimal.mark=",")
p_bact        <- fmt_p(coef1_etio["eti_grpBacteriana",4])

delta_rr_pct  <- round(100 * (exp(coef1_etio["faixa_r50+ anos",1]) /
                              exp(coef1["faixa_r50+ anos",1]) - 1), 1)


## ----tabela-modelo1-sens-etio-------------------------------------------------
data.frame(
  Modelo         = c("Principal (etiologias agrupadas)",
                     "Sensibilidade (com etiologia + ajuste por grupo)"),
  `n`            = c(n_base, n_etio_sens),
  `Óbitos`       = c(obitos, obitos_etio_sens),
  `RR ≥50 anos`  = c(paste0(rr_50, " (", ic_li_50, "–", ic_ls_50, ")"),
                     paste0(rr_50_etio, " (", ic_li_50_etio, "–", ic_ls_50_etio, ")")),
  `Valor de p`   = c(p_50, p_50_etio),
  check.names    = FALSE
) |> kable(caption = paste0(
  "Tabela 2b. Razão de risco para óbito em indivíduos ≥50 anos: modelo principal vs ",
  "análise de sensibilidade restrita aos casos com etiologia identificada e ",
  "ajustada por grupo etiológico (viral/bacteriana/outras)."
), align = c("l","c","c","c","c"), row.names = FALSE)


## ----tabela-enc---------------------------------------------------------------
desc_cat("faixa_r", "Faixa etária") |>
  mutate(Categoria = factor(Categoria,
    levels = c("<5 anos","5-19 anos","20-49 anos","50+ anos"))) |>
  arrange(Categoria) |>
  mutate(Categoria = as.character(Categoria)) |>
  select(-Óbitos, -`% óbito`) |>
  bind_rows(
    desc_cat("sexo_r", "Sexo") |> select(-Óbitos, -`% óbito`),
    desc_cat("raca2",  "Raça/cor") |> select(-Óbitos, -`% óbito`)
  ) |>
  kable(caption = paste0(
    "Tabela 3. Distribuição do encerramento prolongado (≥ 30 dias) por categoria ",
    "dos preditores do Modelo 2 — Triângulo Mineiro, 2015–2025 (n = ", n_base, ")."
  ), align = c("l","l","c","c","c"))


## ----modelo2------------------------------------------------------------------
# Modelo principal: sem `obito` (evita circularidade); inclui fase pandêmica
rodar_poisson(longo ~ sexo_r + faixa_r + raca2 + fase_pand, base_ef) |>
  kable(caption = paste0(
    "Tabela 4. Razões de risco (RR) para encerramento prolongado do caso (≥ 30 dias). ",
    "Regressão de Poisson modificada com variância robusta (sandwich HC3). ",
    "Triângulo Mineiro, ", periodo_lbl, ". n = ", n_base, " casos; ",
    longos, " com encerramento ≥ 30 dias; EPV = ", epv2, ". ",
    "Categorias de referência: Sexo = Feminino; Faixa etária = 20–49 anos; ",
    "Raça/cor = Branca; Fase pandêmica = Pré (2015-2019)."
  ), align = c("l","c","c"))

