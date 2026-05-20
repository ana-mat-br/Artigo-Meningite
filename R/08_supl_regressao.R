# =============================================================================
# 08_supl_regressao — Material suplementar: GEE, GAM/spline, imputação múltipla (mice)
#
# Origem: Extraído de 05b_regressao_sensibilidades.Rmd via knitr::purl()
# Depende: dados_base.rds (01)
# Gera:    Modelos GEE, GAM com spline para idade, imputação múltipla raça/cor
#
# Projeto: Análise da meningite no Triângulo Mineiro (2015–2025)
# =============================================================================

## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE,
                     dpi = 300, fig.width = 6.5, fig.height = 4.5)

# Pacotes
pkgs <- c("dplyr", "tidyr", "stringr", "sandwich", "lmtest",
          "geepack", "mgcv", "mice", "knitr")
for (p in pkgs)
  if (!requireNamespace(p, quietly = TRUE))
    install.packages(p, repos = "https://cloud.r-project.org")
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(stringr)
  library(sandwich); library(lmtest)
  library(geepack); library(mgcv); library(mice)
  library(knitr)
})
select <- dplyr::select; filter <- dplyr::filter; mutate <- dplyr::mutate
summarise <- dplyr::summarise; group_by <- dplyr::group_by

if (!file.exists("dados_base.rds")) stop("Rode antes: source('01_dados_setup.R')")
db <- readRDS("dados_base.rds")
list2env(db$config, envir = environment())

fmt_p <- function(p) dplyr::case_when(
  is.na(p) ~ "NA", p < 0.001 ~ "<0,001",
  TRUE ~ formatC(p, format = "f", digits = 3, decimal.mark = ","))
fmt2 <- function(x) formatC(x, format = "f", digits = 2, decimal.mark = ",")

# Preparar dataset igual ao do 05 principal
perfil <- db$sinan_tri |>
  mutate(
    sexo = case_when(CS_SEXO == "M" ~ "Masculino",
                     CS_SEXO == "F" ~ "Feminino", TRUE ~ NA_character_),
    unidade_idade = floor(as.integer(NU_IDADE_N) / 1000),
    valor_idade   = as.integer(NU_IDADE_N) %% 1000,
    idade_anos    = if_else(unidade_idade == 4, valor_idade, 0L),
    faixa4 = case_when(
      idade_anos <  5              ~ "<5 anos",
      idade_anos >= 5  & idade_anos < 20 ~ "5-19 anos",
      idade_anos >= 20 & idade_anos < 50 ~ "20-49 anos",
      idade_anos >= 50             ~ "50+ anos",
      TRUE ~ NA_character_),
    desfecho = case_when(
      EVOLUCAO == "1" ~ "Cura",
      EVOLUCAO == "2" ~ "Óbito por meningite",
      EVOLUCAO == "3" ~ "Óbito por outra causa",
      TRUE ~ "Ignorado"),
    obito = as.integer(EVOLUCAO == "2"),
    raca = case_when(
      CS_RACA == "1"          ~ "Branca",
      CS_RACA == "2"          ~ "Preta",
      CS_RACA == "4"          ~ "Parda",
      CS_RACA %in% c("3","5") ~ "Outras",
      TRUE ~ NA_character_),
    dt_sin_pri = as.Date(as.integer(DT_SIN_PRI), origin = "1970-01-01"),
    dt_encerra = as.Date(as.integer(DT_ENCERRA), origin = "1970-01-01"),
    dias_encerr = as.integer(dt_encerra - dt_sin_pri),
    fase_pand = factor(case_when(
      ANO_NOTIF %in% 2015:2019 ~ "Pré",
      ANO_NOTIF %in% 2020:2021 ~ "Intra",
      ANO_NOTIF %in% 2022:2025 ~ "Pós"),
      levels = c("Pré","Intra","Pós"))
  ) |>
  filter(desfecho %in% c("Cura", "Óbito por meningite"))

base_ef <- perfil |>
  mutate(
    sexo_r  = factor(sexo,   levels = c("Feminino","Masculino")),
    faixa_r = factor(faixa4, levels = c("20-49 anos","<5 anos","5-19 anos","50+ anos")),
    raca2   = factor(if_else(raca == "Branca", "Branca", "Não branca"),
                     levels = c("Branca","Não branca")),
    longo   = if_else(!is.na(dias_encerr) & dias_encerr >= 0 & dias_encerr <= 365,
                      as.integer(dias_encerr >= 30), NA_integer_)
  ) |>
  filter(!is.na(sexo_r), !is.na(faixa_r),
         raca %in% c("Branca","Parda","Preta"),
         !is.na(longo))


## ----gee, include=FALSE-------------------------------------------------------
# GEE: Poisson com link log; cluster=cod_sinan; estrutura de correlação exchangeable
base_ef_gee <- base_ef |>
  arrange(cod_sinan) |>
  mutate(cod_sinan = factor(cod_sinan))

gee_obito <- geeglm(
  obito ~ sexo_r + faixa_r + raca2 + fase_pand,
  id      = cod_sinan,
  family  = poisson(link = "log"),
  data    = base_ef_gee,
  corstr  = "exchangeable"
)
gee_long <- geeglm(
  longo ~ sexo_r + faixa_r + raca2 + fase_pand,
  id      = cod_sinan,
  family  = poisson(link = "log"),
  data    = base_ef_gee,
  corstr  = "exchangeable"
)

gee_tabela <- function(fit, label) {
  s <- summary(fit)
  coefs <- as.data.frame(s$coefficients)
  coefs$Variavel <- rownames(coefs)
  rownames(coefs) <- NULL
  data.frame(
    Variável    = coefs$Variavel,
    RR          = fmt2(exp(coefs$Estimate)),
    `IC 95%`    = paste0("(",
                          fmt2(exp(coefs$Estimate - 1.96 * coefs$Std.err)),
                          "-",
                          fmt2(exp(coefs$Estimate + 1.96 * coefs$Std.err)),
                          ")"),
    `Valor de p` = fmt_p(coefs$`Pr(>|W|)`),
    Modelo      = label,
    check.names = FALSE
  ) |> dplyr::filter(Variável != "(Intercept)")
}

tab_gee <- bind_rows(
  gee_tabela(gee_obito, "Óbito"),
  gee_tabela(gee_long,  "Encerramento ≥ 30 dias")
)


## ----tab-gee------------------------------------------------------------------
kable(tab_gee,
      caption = paste0("Tabela S1. Modelos GEE Poisson (link log) com cluster por município ",
                       "de residência, estrutura de correlação exchangeable — ", periodo_lbl, "."),
      align = c("l","c","c","c","l"), row.names = FALSE)


## ----gam, include=FALSE-------------------------------------------------------
gam_obito <- gam(
  obito ~ s(idade_anos, k = 5) + sexo_r + raca2 + fase_pand,
  family = poisson(link = "log"),
  data   = base_ef,
  method = "REML"
)
gam_sum <- summary(gam_obito)
edf_idade <- gam_sum$s.table["s(idade_anos)", "edf"]
p_idade   <- gam_sum$s.table["s(idade_anos)", "p-value"]
dev_expl  <- gam_sum$dev.expl * 100


## ----fig-gam, fig.cap=paste0("Figura S1. Curva de risco relativo de óbito por idade (spline cúbico, GAM Poisson). Sombra: IC 95%. Linha tracejada: RR = 1.")----
# Predição: variação de RR em função da idade, mantendo demais covariáveis na referência
preds <- expand.grid(
  idade_anos = 0:90,
  sexo_r     = factor("Feminino", levels = levels(base_ef$sexo_r)),
  raca2      = factor("Branca",   levels = levels(base_ef$raca2)),
  fase_pand  = factor("Pré",      levels = levels(base_ef$fase_pand))
)
pr <- predict(gam_obito, newdata = preds, se.fit = TRUE)
# Centralizar em idade 30 anos (referência interpretativa)
ref_idx <- which(preds$idade_anos == 30)
log_rr_centered <- pr$fit - pr$fit[ref_idx]
se <- pr$se.fit

preds$rr   <- exp(log_rr_centered)
preds$rr_l <- exp(log_rr_centered - 1.96 * se)
preds$rr_u <- exp(log_rr_centered + 1.96 * se)

library(ggplot2)
ggplot(preds, aes(x = idade_anos)) +
  geom_ribbon(aes(ymin = rr_l, ymax = rr_u), alpha = 0.2, fill = "#0072B2") +
  geom_line(aes(y = rr), color = "#0072B2", linewidth = 1) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
  scale_x_continuous(breaks = seq(0, 90, 10)) +
  labs(x = "Idade (anos)", y = "RR de óbito (referência: 30 anos)") +
  theme_classic(base_size = 10)


## ----imputacao, include=FALSE-------------------------------------------------
# Conjunto incluindo casos com raça "Outras" ou ignorada (para imputar)
base_imp <- perfil |>
  mutate(
    sexo_r  = factor(sexo,   levels = c("Feminino","Masculino")),
    faixa_r = factor(faixa4, levels = c("20-49 anos","<5 anos","5-19 anos","50+ anos")),
    raca2   = factor(case_when(raca == "Branca" ~ "Branca",
                               raca %in% c("Parda","Preta") ~ "Não branca",
                               TRUE ~ NA_character_),
                     levels = c("Branca","Não branca")),
    longo   = if_else(!is.na(dias_encerr) & dias_encerr >= 0 & dias_encerr <= 365,
                      as.integer(dias_encerr >= 30), NA_integer_)
  ) |>
  filter(!is.na(sexo_r), !is.na(faixa_r), !is.na(longo)) |>
  dplyr::select(obito, longo, sexo_r, faixa_r, raca2, fase_pand)

n_imp_total   <- nrow(base_imp)
n_imp_missing <- sum(is.na(base_imp$raca2))
pct_imp_miss  <- round(n_imp_missing / n_imp_total * 100, 1)

# m = 5 imputações
imp <- mice(base_imp, m = 5, method = "logreg", printFlag = FALSE, seed = 2025)

# Ajustar Poisson HC3 em cada imputação e pool com Rubin's rules
modelos_imp_obito <- with(imp, glm(obito ~ sexo_r + faixa_r + raca2 + fase_pand,
                                    family = poisson(link = "log")))
pool_obito <- summary(pool(modelos_imp_obito), conf.int = TRUE)
pool_obito$Variavel <- as.character(pool_obito$term)
pool_obito <- pool_obito[pool_obito$Variavel != "(Intercept)", ]

tab_imp <- data.frame(
  Variável    = pool_obito$Variavel,
  RR          = fmt2(exp(pool_obito$estimate)),
  `IC 95%`    = paste0("(", fmt2(exp(pool_obito$`2.5 %`)),
                       "-", fmt2(exp(pool_obito$`97.5 %`)), ")"),
  `Valor de p` = fmt_p(pool_obito$p.value),
  check.names = FALSE
)


## ----tab-imp------------------------------------------------------------------
kable(tab_imp,
      caption = paste0("Tabela S2. Modelo de óbito por meningite com imputação múltipla ",
                       "(m = 5; logreg) para raça/cor faltante (",
                       n_imp_missing, " de ", n_imp_total, " casos; ",
                       pct_imp_miss, "%); estimativas agregadas pelas regras de Rubin."),
      align = c("l","c","c","c"), row.names = FALSE)

