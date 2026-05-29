# =============================================================================
# 01_dados_setup — Pé do pipeline: lê DBCs, monta tabelas mestre, cacheia em dados_base.rds
#
# Origem: Script original (não derivado de .Rmd)
# Depende: MENIBR15.dbc … MENIBR25.dbc, conexão à API SIDRA (IBGE)
# Gera:    dados_base.rds (lista com sinan_br, sinan_tri, mapa, pesos, pop_ibge, pop_muni_idade, pop_tm_faixa, pop_tm_faixa_sexo, ref_muni, config)
#
# Projeto: Análise da meningite no Triângulo Mineiro (2015–2025)
# =============================================================================


suppressPackageStartupMessages({
  library(read.dbc)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(sidrar)
  library(geobr)
  library(sf)
  library(spdep)
  library(tibble)
})

suppressPackageStartupMessages({
  library(read.dbc)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(sidrar)
  library(geobr)
  library(sf)
  library(spdep)
  library(tibble)
})

# --- 1. Configuração temporal -------------------------------------------------
ano_inicio  <- 2015L
ano_fim     <- 2025L
anos        <- ano_inicio:ano_fim
n_anos      <- length(anos)
periodo_lbl <- paste0(ano_inicio, "–", ano_fim)

# --- 2. Municípios do Triângulo Mineiro --------------------------------------
codigos_ibge_7d <- c(
  3100104, 3100708, 3103504, 3103751, 3104007, 3109808, 3111101, 3111408,
  3111507, 3111804, 3112604, 3114550, 3115003, 3115805, 3116902, 3117306,
  3118205, 3119302, 3121258, 3123502, 3124807, 3127008, 3127107, 3127909,
  3129103, 3129509, 3130705, 3131406, 3131604, 3133402, 3134202, 3134400,
  3138625, 3142809, 3143104, 3145000, 3148103, 3149200, 3149804, 3150703,
  3151602, 3152808, 3153004, 3156403, 3156908, 3157708, 3159803, 3161304,
  3168101, 3169604, 3170107, 3170206, 3170438, 3171105
)
codigos_sinan_6d <- as.character(floor(codigos_ibge_7d / 10))
ref_muni <- data.frame(cod_ibge = codigos_ibge_7d,
                       cod_sinan = codigos_sinan_6d,
                       stringsAsFactors = FALSE)

# --- 3. Leitura dos DBCs ------------------------------------------------------
message("Lendo arquivos DBC do SINAN-MENING para ", periodo_lbl, "…")
arquivos_dbc <- sprintf("MENIBR%s.dbc", substr(anos, 3, 4))
names(arquivos_dbc) <- as.character(anos)

faltando <- arquivos_dbc[!file.exists(arquivos_dbc)]
if (length(faltando) > 0)
  stop("Arquivo(s) não encontrado(s): ", paste(faltando, collapse = ", "))

sinan_lista <- lapply(as.character(anos), function(ano) {
  df <- tryCatch(read.dbc(arquivos_dbc[ano]), error = function(e) NULL)
  if (!is.null(df)) df$ANO_NOTIF <- as.integer(ano)
  df
})
sinan_br_raw <- bind_rows(Filter(function(x) !is.null(x) && nrow(x) > 0,
                                  sinan_lista))

# Detectar coluna de município de residência
padroes_muni <- c("ID_MN_RESI", "ID_MUNICIP", "MUN_RESI", "CO_MUN_RESI")
col_muni <- NA_character_
for (p in padroes_muni) {
  if (p %in% names(sinan_br_raw)) { col_muni <- p; break }
}
if (is.na(col_muni)) stop("Coluna de município não encontrada.")

# Filtrar confirmados e padronizar código de residência (6 dígitos)
sinan_br_raw <- sinan_br_raw |>
  filter(grepl("^1$|confirm", as.character(CLASSI_FIN), ignore.case = TRUE)) |>
  mutate(
    cod_num   = suppressWarnings(as.numeric(trimws(as.character(.data[[col_muni]])))),
    cod_sinan = str_pad(as.character(as.integer(
      case_when(
        nchar(trimws(as.character(.data[[col_muni]]))) == 7 ~ floor(cod_num / 10),
        TRUE ~ cod_num
      )
    )), width = 6, pad = "0")
  )

sinan_br  <- sinan_br_raw
sinan_tri <- sinan_br_raw |> filter(cod_sinan %in% codigos_sinan_6d)

if (nrow(sinan_tri) == 0)
  stop("Nenhum caso encontrado para os municípios do Triângulo Mineiro.")

message("  Casos confirmados — BR: ", nrow(sinan_br),
        " | TM: ", nrow(sinan_tri))

# --- 4. Agregação anual por município ----------------------------------------
casos_ano <- sinan_tri |>
  group_by(cod_sinan, ANO_NOTIF) |>
  summarise(casos = n(), .groups = "drop") |>
  pivot_wider(names_from = ANO_NOTIF, values_from = casos,
              names_prefix = "casos_", values_fill = 0) |>
  right_join(ref_muni, by = "cod_sinan") |>
  mutate(across(starts_with("casos_"), ~ replace_na(as.integer(.x), 0L)))

for (a in anos) {
  col_c <- paste0("casos_", a)
  if (!col_c %in% names(casos_ano)) casos_ano[[col_c]] <- 0L
}

# --- 5. Populações anuais por município via SIDRA 6579 -----------------------
message("Consultando SIDRA 6579 (estimativas populacionais municipais)…")
pop_ibge <- tryCatch({
  raw <- sidrar::get_sidra(
    api = paste0("/t/6579/n6/", paste(codigos_ibge_7d, collapse = ","),
                 "/v/all/p/", paste(anos, collapse = ","))
  )
  raw |>
    dplyr::select(cod_ibge = `Município (Código)`, ano = `Ano`, pop = Valor) |>
    mutate(cod_ibge = as.integer(cod_ibge),
           ano      = as.integer(ano),
           pop      = as.integer(pop)) |>
    pivot_wider(names_from = ano, values_from = pop, names_prefix = "pop_")
}, error = function(e) { warning(e); NULL })

if (is.null(pop_ibge))
  stop("SIDRA indisponível — não consegui obter populações.")

# Propagar anos faltantes
anos_pop <- paste0("pop_", anos)
for (i in seq_along(anos_pop)) {
  col <- anos_pop[i]
  if (!col %in% names(pop_ibge)) {
    anteriores <- anos_pop[seq_len(i - 1)]
    anteriores <- anteriores[anteriores %in% names(pop_ibge)]
    pop_ibge[[col]] <- if (length(anteriores) > 0)
      pop_ibge[[tail(anteriores, 1)]] else NA_integer_
  }
}

# --- 6. População por idade × sexo por município (Censo 2022, SIDRA 9514) ----
# Atualizado para incluir sexo (para padronização indireta dupla idade×sexo).
cache_pop_muni <- "pop_idade_muni_censo2022.rds"
deve_rebaixar  <- !file.exists(cache_pop_muni)
if (!deve_rebaixar) {
  tmp <- readRDS(cache_pop_muni)
  # se cache ainda for versão antiga (sem coluna sexo), re-baixar
  deve_rebaixar <- !("sexo" %in% names(tmp)) || nrow(tmp) == 0
}

if (!deve_rebaixar) {
  message("Censo 2022: usando cache (", cache_pop_muni, ")")
  pop_muni_idade <- readRDS(cache_pop_muni)
} else {
  message("Consultando SIDRA 9514 (Censo 2022 por idade × sexo)…")
  sexos_lst <- list(M = "4", F = "5")  # c2: 4=Homens, 5=Mulheres
  sub_lst <- list()
  for (sx in names(sexos_lst)) {
    raw <- sidrar::get_sidra(
      api = paste0("/t/9514/n6/", paste(codigos_ibge_7d, collapse = ","),
                   "/v/93/p/all/c2/", sexos_lst[[sx]], "/c287/all")
    )
    idade_txt <- raw[["Idade"]]
    eh_range  <- grepl(" a ",    idade_txt, fixed = TRUE)
    eh_total  <- grepl("^Total", idade_txt, ignore.case = TRUE)
    idade_int <- suppressWarnings(as.integer(
      stringr::str_extract(as.character(idade_txt), "^\\d+")
    ))
    idade_int[eh_range | eh_total] <- NA_integer_
    pop_int <- suppressWarnings(as.integer(round(as.numeric(
      gsub("\\.", "", gsub(",", ".", as.character(raw[["Valor"]])))
    ))))

    sub_lst[[sx]] <- tibble(
      cod_ibge = suppressWarnings(as.integer(raw[["Município (Código)"]])),
      sexo     = sx,
      idade    = idade_int,
      pop      = pop_int
    ) |>
      filter(!is.na(idade), !is.na(pop), pop > 0) |>
      mutate(faixa = cut(idade, breaks = c(-1, 4, 19, 49, Inf),
                         labels = c("<5","5-19","20-49","50+"), right = TRUE)) |>
      filter(!is.na(faixa)) |>
      group_by(cod_ibge, sexo, faixa) |>
      summarise(pop_2022 = sum(pop, na.rm = TRUE), .groups = "drop")
  }
  pop_muni_idade <- bind_rows(sub_lst)
  saveRDS(pop_muni_idade, cache_pop_muni)
}

# --- 6b. Censo 2010 — estrutura etária por sexo (TM agregado) ---------------
# Usado APENAS na análise de sensibilidade do SMR (§ 1.2 de 02_smr.Rmd),
# interpolando ano-a-ano a estrutura etária entre 2010 e 2022.
cache_pop_2010 <- "pop_tm_faixa_sexo_censo2010.rds"
if (!file.exists(cache_pop_2010)) {
  message("Consultando SIDRA 200 (Censo 2010 por sexo × idade — TM)…")
  grupos5 <- c("0 a 4 anos","5 a 9 anos","10 a 14 anos","15 a 19 anos",
               "20 a 24 anos","25 a 29 anos","30 a 34 anos","35 a 39 anos",
               "40 a 44 anos","45 a 49 anos","50 a 54 anos","55 a 59 anos",
               "60 a 64 anos","65 a 69 anos","70 a 74 anos","75 a 79 anos",
               "80 a 84 anos","85 a 89 anos","90 a 94 anos","95 a 99 anos",
               "100 anos ou mais")
  batches <- split(codigos_ibge_7d, ceiling(seq_along(codigos_ibge_7d) / 10))
  raw_2010 <- bind_rows(lapply(batches, function(b) {
    Sys.sleep(0.5)
    sidrar::get_sidra(x = 200, period = "2010", geo = "City",
                      geo.filter = list(City = as.character(b)),
                      variable = 93, header = FALSE)
  }))
  pop_tm_2010 <- raw_2010 |>
    filter(D4N %in% c("Homens","Mulheres"), D6N %in% grupos5) |>
    mutate(sexo = ifelse(D4N == "Homens", "M", "F"),
           faixa = case_when(
             D6N == "0 a 4 anos" ~ "<5",
             D6N %in% c("5 a 9 anos","10 a 14 anos","15 a 19 anos") ~ "5-19",
             D6N %in% c("20 a 24 anos","25 a 29 anos","30 a 34 anos",
                        "35 a 39 anos","40 a 44 anos","45 a 49 anos") ~ "20-49",
             TRUE ~ "50+"),
           pop = as.numeric(V)) |>
    group_by(sexo, faixa) |>
    summarise(pop_2010 = sum(pop, na.rm = TRUE), .groups = "drop")
  saveRDS(pop_tm_2010, cache_pop_2010)
} else {
  message("Censo 2010: usando cache (", cache_pop_2010, ")")
  pop_tm_2010 <- readRDS(cache_pop_2010)
}

# --- 7. População do TM por (ano × sexo × faixa) -----------------------------
prop_tm <- pop_muni_idade |>
  group_by(sexo, faixa) |>
  summarise(pop_2022 = sum(pop_2022), .groups = "drop") |>
  mutate(prop = pop_2022 / sum(pop_2022))

pop_tm_ano <- tibble(
  ano = anos,
  pop_total = sapply(anos, function(a)
    sum(pop_ibge[[paste0("pop_", a)]], na.rm = TRUE))
)

# Versão dupla-estratificada (ano × sexo × faixa) — para SMR principal
pop_tm_faixa_sexo <- tidyr::crossing(pop_tm_ano, prop_tm) |>
  mutate(pop = pop_total * prop) |>
  dplyr::select(ano, sexo, faixa, pop)

# Versão por idade apenas (ano × faixa) — preservada para compat com 02b/03
pop_tm_faixa <- pop_tm_faixa_sexo |>
  group_by(ano, faixa) |>
  summarise(pop = sum(pop), .groups = "drop")

# --- 8. Shapefile e dados consolidados ---------------------------------------
cache_mapa <- "muni_mg_2022.rds"
if (file.exists(cache_mapa)) {
  muni_mg <- readRDS(cache_mapa)
} else {
  muni_mg <- read_municipality(code_muni = "MG", year = 2022, showProgress = FALSE)
  if (!is.null(muni_mg)) saveRDS(muni_mg, cache_mapa)
}

muni_nomes <- muni_mg |>
  st_drop_geometry() |>
  filter(code_muni %in% codigos_ibge_7d) |>
  dplyr::select(cod_ibge = code_muni, municipio = name_muni) |>
  mutate(municipio = str_to_upper(municipio))

dados <- casos_ano |>
  left_join(pop_ibge,   by = "cod_ibge") |>
  left_join(muni_nomes, by = "cod_ibge") |>
  mutate(
    across(paste0("casos_", anos), ~ replace_na(as.integer(.x), 0L)),
    municipio_label = municipio
  )

for (a in anos) {
  col_taxa <- paste0("taxa_", a)
  col_caso <- paste0("casos_", a)
  col_pop  <- paste0("pop_",   a)
  dados[[col_taxa]] <- round(dados[[col_caso]] / dados[[col_pop]] * 100000, 2)
}

dados <- dados |>
  mutate(
    total_casos     = rowSums(across(paste0("casos_", anos)), na.rm = TRUE),
    total_pop       = rowSums(across(paste0("pop_",   anos)), na.rm = TRUE),
    taxa_cumulativa = (total_casos / total_pop) * 100000
  ) |>
  arrange(municipio)

mapa <- muni_mg |>
  filter(code_muni %in% dados$cod_ibge) |>
  left_join(dados, by = c("code_muni" = "cod_ibge"))

# --- 9. Matriz de vizinhança e estimadores EB --------------------------------
coords_viz <- poly2nb(mapa, queen = TRUE)
pesos      <- nb2listw(coords_viz, style = "W", zero.policy = TRUE)
eb         <- EBest(mapa$total_casos, mapa$total_pop)
mapa$taxa_eb <- eb$estmm * 100000

# --- 10. Salvar tudo em dados_base.rds ---------------------------------------
dados_base <- list(
  config = list(
    ano_inicio  = ano_inicio,
    ano_fim     = ano_fim,
    anos        = anos,
    n_anos      = n_anos,
    periodo_lbl = periodo_lbl,
    col_muni    = col_muni
  ),
  ref_muni       = ref_muni,
  codigos_ibge_7d= codigos_ibge_7d,
  codigos_sinan_6d= codigos_sinan_6d,
  sinan_br       = sinan_br,
  sinan_tri      = sinan_tri,
  pop_ibge       = pop_ibge,
  pop_muni_idade    = pop_muni_idade,
  pop_tm_faixa      = pop_tm_faixa,
  pop_tm_faixa_sexo = pop_tm_faixa_sexo,
  pop_tm_2010       = pop_tm_2010,
  dados          = dados,
  mapa           = mapa,
  pesos          = pesos
)
saveRDS(dados_base, "dados_base.rds")
message("OK. dados_base.rds salvo. Total: ", nrow(sinan_tri),
        " casos TM | ", nrow(sinan_br), " casos BR.")
