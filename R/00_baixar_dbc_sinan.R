# =============================================================================
# 00_baixar_dbc_sinan.R
#
# Baixa os arquivos DBC do SINAN-MENING (meningite) do FTP do DATASUS
# para o diretório raiz do projeto.
#
# Fonte oficial:
#   FINAIS:  ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/
#   PRELIM:  ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/PRELIM/
#
# Os arquivos são públicos, disponibilizados pelo Ministério da Saúde sob
# domínio público para fins de transparência e pesquisa científica.
#
# Estratégia: tenta primeiro a pasta FINAIS (anos consolidados); se falhar,
# tenta PRELIM (anos preliminares — caso típico do ano corrente).
#
# Uso:
#   source("R/00_baixar_dbc_sinan.R")
# =============================================================================

# --- Configuração temporal ---
ano_inicio <- 2015L
ano_fim    <- 2025L
anos       <- ano_inicio:ano_fim

ftp_finais <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/"
ftp_prelim <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/PRELIM/"

# Diretório de destino (raiz do projeto)
destino <- "."

# --- Helper: tenta baixar um arquivo de uma das duas pastas -----------------
baixar_dbc <- function(arquivo, sobrescrever = FALSE) {
  caminho_local <- file.path(destino, arquivo)
  if (file.exists(caminho_local) && !sobrescrever) {
    message("  ja existe: ", arquivo, " (use sobrescrever = TRUE para refazer)")
    return(invisible(TRUE))
  }

  for (origem in list(c("FINAIS", ftp_finais), c("PRELIM", ftp_prelim))) {
    rotulo <- origem[1]; url <- paste0(origem[2], arquivo)
    ok <- tryCatch({
      utils::download.file(url, caminho_local, mode = "wb", quiet = TRUE)
      file.exists(caminho_local) && file.info(caminho_local)$size > 0
    }, error = function(e) FALSE, warning = function(w) FALSE)

    if (ok) {
      tamanho <- format(structure(file.info(caminho_local)$size,
                                   class = "object_size"), units = "auto")
      message("  baixado: ", arquivo, " (", rotulo, ", ", tamanho, ")")
      return(invisible(TRUE))
    }
  }

  warning("nao foi possivel baixar ", arquivo,
          " nem de FINAIS nem de PRELIM (verifique conexao ou disponibilidade)")
  if (file.exists(caminho_local)) file.remove(caminho_local)
  invisible(FALSE)
}

# --- Execução --------------------------------------------------------------
message("Baixando arquivos DBC do SINAN-MENING para ", ano_inicio, "-", ano_fim, ":")
arquivos <- sprintf("MENIBR%s.dbc", substr(anos, 3, 4))
resultados <- sapply(arquivos, baixar_dbc)

# --- Sanity check ----------------------------------------------------------
n_baixados <- sum(resultados)
message("\nResumo: ", n_baixados, " de ", length(arquivos), " arquivos disponiveis.")
if (n_baixados < length(arquivos)) {
  message("Arquivos ausentes: ",
          paste(arquivos[!resultados], collapse = ", "))
  message("Esses anos podem nao ter sido publicados ainda no DATASUS.")
}
message("\nProximo passo: source('R/01_dados_setup.R')")
