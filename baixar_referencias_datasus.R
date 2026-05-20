# =============================================================================
# baixar_referencias_datasus.R
#
# Objetivo: gerar referências de POPULAÇÃO por (ano × UF × faixa) para BR, MG e SE
# via SIDRA tabela 7358. Os CASOS de referência (BR/MG/SE) são derivados dos
# arquivos MENIBR*.dbc já presentes em disco (nacionais) diretamente no Rmd.
#
# Saída:
#   populacao_br_mg.rds — pop por (ano × uf × faixa) para BR/MG/SE, 2015–2025.
#
# IMPORTANTE: rode esse script apenas uma vez por sessão. Cacheia automaticamente.
# =============================================================================

# --- 0. Pacotes ---------------------------------------------------------------
pacotes <- c("dplyr", "tidyr", "sidrar", "stringr")
for (p in pacotes) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p, repos = "https://cloud.r-project.org")
  }
}
invisible(lapply(pacotes, library, character.only = TRUE))

# --- 1. Configuração ----------------------------------------------------------
ano_inicio <- 2015L
ano_fim    <- 2025L
anos       <- ano_inicio:ano_fim

breaks_faixa <- c(-1, 4, 19, 49, Inf)
labels_faixa <- c("<5", "5-19", "20-49", "50+")

cache_pop <- "populacao_br_mg.rds"

# --- 2. População por (ano × UF × faixa) ------------------------------------
# Estratégia: combinar duas tabelas SIDRA confiáveis:
#   * Tabela 6579 — Estimativas anuais da população residente (n1/n2/n3/n6).
#     Dá o TOTAL por (ano × uf), 2001–2024.
#   * Tabela 9514 — Censo Demográfico 2022 por sexo e idade simples.
#     Dá a DISTRIBUIÇÃO etária por uf em 2022.
#
# Aplica-se a distribuição etária do Censo 2022 (fixa) aos totais anuais.
# Aproximação razoável para faixas etárias amplas em janelas curtas; coerente
# com a mesma simplificação usada para os municípios do TM.

if (file.exists(cache_pop)) {
  message("População: usando cache local (", cache_pop, ")")
  pop_ref <- readRDS(cache_pop)
} else {

  niveis <- list(
    BR = list(n = "n1/all", label = "Brasil"),
    MG = list(n = "n3/31",  label = "Minas Gerais"),
    SE = list(n = "n2/3",   label = "Sudeste")
  )

  # Helper defensivo: pega a coluna cujo NOME É EXATAMENTE igual ao alvo
  pegar_coluna <- function(df, nome_exato, codigo = FALSE) {
    nomes <- names(df)
    alvo  <- if (codigo) paste0(nome_exato, " (Código)") else nome_exato
    idx   <- which(nomes == alvo)
    if (length(idx) == 0) {
      stop("Coluna não encontrada (nome exato): '", alvo,
           "'. Disponíveis: ", paste(nomes, collapse = " | "))
    }
    df[[idx[1]]]
  }

  parse_valor <- function(x) {
    chr <- as.character(x)
    num <- suppressWarnings(as.numeric(gsub("\\.", "", gsub(",", ".", chr))))
    as.integer(round(num))
  }

  # ---------- 2a. Totais anuais por (ano × uf) — Tabela 6579 ------------------
  message("População total: consultando SIDRA tabela 6579…")
  anos_6579 <- intersect(anos, 2001:2024)

  totais_lst <- list()
  for (rotulo in names(niveis)) {
    raw <- sidrar::get_sidra(
      api = paste0("/t/6579/", niveis[[rotulo]]$n,
                   "/v/9324/p/", paste(anos_6579, collapse = ","))
    )
    if (rotulo == "BR") {
      message("  Colunas 6579: ", paste(names(raw), collapse = " | "))
    }
    ano_txt <- pegar_coluna(raw, "Ano",   codigo = FALSE)
    val     <- pegar_coluna(raw, "Valor", codigo = FALSE)

    totais_lst[[rotulo]] <- tibble::tibble(
      ano       = suppressWarnings(as.integer(as.character(ano_txt))),
      uf        = rotulo,
      pop_total = parse_valor(val)
    ) |>
      filter(!is.na(ano), !is.na(pop_total), pop_total > 0)
  }
  totais_ano_uf <- bind_rows(totais_lst)

  if (nrow(totais_ano_uf) == 0) {
    stop("Tabela 6579 não retornou dados — variável 9324 talvez não exista. ",
         "Tente outras: v/all para ver opções.")
  }

  # Propagar último ano disponível para 2025 (se SIDRA ainda não atualizou)
  ultimo_t <- max(totais_ano_uf$ano)
  if (ultimo_t < ano_fim) {
    extra <- totais_ano_uf |>
      filter(ano == ultimo_t) |>
      tidyr::crossing(ano_novo = (ultimo_t + 1):ano_fim) |>
      mutate(ano = ano_novo) |>
      select(ano, uf, pop_total)
    totais_ano_uf <- bind_rows(totais_ano_uf, extra)
  }

  # ---------- 2b. Distribuição etária × sexo — Censo 2022 (Tabela 9514) ------
  message("Distribuição etária × sexo Censo 2022: consultando SIDRA tabela 9514…")

  # c2 = 4 (Homens), 5 (Mulheres) — pedimos as duas separadamente
  sexos_lst <- list(M = "4", F = "5")
  prop_lst  <- list()
  for (rotulo in names(niveis)) {
    sub_lst <- list()
    for (sx in names(sexos_lst)) {
      raw <- sidrar::get_sidra(
        api = paste0("/t/9514/", niveis[[rotulo]]$n,
                     "/v/93/p/all/c2/", sexos_lst[[sx]], "/c287/all")
      )
      if (rotulo == "BR" && sx == "M") {
        message("  Colunas 9514: ", paste(names(raw), collapse = " | "))
      }
      idade_txt <- pegar_coluna(raw, "Idade", codigo = FALSE)
      val       <- pegar_coluna(raw, "Valor", codigo = FALSE)

      idade_str <- as.character(idade_txt)
      eh_range  <- grepl(" a ", idade_str, fixed = TRUE)
      eh_total  <- grepl("^Total", idade_str, ignore.case = TRUE)
      idade_int <- suppressWarnings(as.integer(
        stringr::str_extract(idade_str, "^\\d+")
      ))
      idade_int[eh_range | eh_total] <- NA_integer_

      sub_lst[[sx]] <- tibble::tibble(
        uf    = rotulo,
        sexo  = sx,
        idade = idade_int,
        pop   = parse_valor(val)
      ) |>
        filter(!is.na(idade), !is.na(pop), pop > 0) |>
        mutate(faixa = cut(idade, breaks = breaks_faixa, labels = labels_faixa,
                           right = TRUE)) |>
        filter(!is.na(faixa)) |>
        group_by(uf, sexo, faixa) |>
        summarise(pop_2022 = sum(pop), .groups = "drop")
    }
    pop_uf <- bind_rows(sub_lst)
    total_uf <- sum(pop_uf$pop_2022)
    pop_uf$prop <- pop_uf$pop_2022 / total_uf
    prop_lst[[rotulo]] <- pop_uf |> dplyr::select(uf, sexo, faixa, prop)
  }
  prop_etaria <- bind_rows(prop_lst)

  # ---------- 2c. Combinar: pop por (ano × uf × sexo × faixa) ---------------
  pop_ref <- totais_ano_uf |>
    inner_join(prop_etaria, by = "uf", relationship = "many-to-many") |>
    mutate(pop = as.integer(round(pop_total * prop))) |>
    dplyr::select(ano, uf, sexo, faixa, pop)

  # Diagnóstico de cobertura temporal
  anos_disponiveis <- sort(unique(pop_ref$ano))
  anos_faltando    <- setdiff(anos, anos_disponiveis)
  message("Anos cobertos pela tabela 6579: ",
          paste(anos_disponiveis, collapse = ", "))
  if (length(anos_faltando) > 0) {
    message("Anos faltantes (serão preenchidos por propagação): ",
            paste(anos_faltando, collapse = ", "))

    # Para cada ano faltante, copiar do ano disponível mais próximo
    extras <- lapply(anos_faltando, function(af) {
      ano_proximo <- anos_disponiveis[which.min(abs(anos_disponiveis - af))]
      pop_ref |>
        filter(ano == ano_proximo) |>
        mutate(ano = af)
    })
    pop_ref <- bind_rows(pop_ref, do.call(bind_rows, extras))
  }

  saveRDS(pop_ref, cache_pop)
  message("População: cache salvo em ", cache_pop,
          " (", nrow(pop_ref), " linhas).")
}

# --- 3. Verificações ----------------------------------------------------------
stopifnot(
  all(anos %in% unique(pop_ref$ano)),
  all(c("BR", "MG", "SE") %in% unique(pop_ref$uf)),
  all(c("M", "F") %in% unique(pop_ref$sexo))
)

message("OK. População pronta. Os casos (BR/MG/SE) são agregados dentro do Rmd ",
        "a partir dos arquivos MENIBR*.dbc nacionais já carregados.")
