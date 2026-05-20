# Artigo-Meningite

Códigos R do artigo *"Concentração da incidência de meningite em centros de referência hospitalar no Triângulo Mineiro, Brasil, 2015–2025: análise espacial bayesiana, LISA e regressão de Poisson modificada"*, submetido à *Revista Brasileira de Epidemiologia*.

## Autores

- Pedro Henrique Rodrigues Braga
- Ana Paula Fernandes (autora correspondente: anapaula.fernandes@uftm.edu.br)
- Aline Dias Paiva

Universidade Federal do Triângulo Mineiro (UFTM), Uberaba (MG), Brasil.

## Estrutura

Todos os scripts ficam em `R/`, numerados conforme a ordem de execução.

```
R/
  README.md                          Documentação interna da pasta R/
  01_dados_setup.R                   Pé do pipeline: lê DBCs + SIDRA → dados_base.rds
  02_baixar_referencias.R            Pop. BR/MG/SE por (ano × sexo × faixa) → populacao_br_mg.rds
  03_smr.R                           SMR padronizado por idade × sexo (indireta)
  04_espacial_lisa.R                 Moran Global + LISA com FDR + mapas
  05_perfil_sazonalidade.R           Perfil epidemiológico + sazonalidade + letalidade
  06_regressao_poisson.R             Poisson modificada (HC3): óbito + encerramento prolongado
  07_supl_tendencia.R                Suplementar: Joinpoint, Prais-Winsten, GAM + SMR sem UDI/UBE
  08_supl_regressao.R                Suplementar: GEE, GAM com spline, mice (imputação múltipla)
  exportar_figuras.R                 Gera Figuras 1 e 2 em 300 dpi (PNG + TIFF + PDF)
```

## Como reproduzir

A partir do diretório raiz do projeto:

```r
# 1. Preparação dos dados (uma única vez)
source("R/01_dados_setup.R")
source("R/02_baixar_referencias.R")

# 2. Análises do manuscrito principal
source("R/03_smr.R")
source("R/04_espacial_lisa.R")
source("R/05_perfil_sazonalidade.R")
source("R/06_regressao_poisson.R")

# 3. Análises de sensibilidade (material suplementar)
source("R/07_supl_tendencia.R")
source("R/08_supl_regressao.R")

# 4. Figuras finais em 300 dpi
source("R/exportar_figuras.R")
```

## Pré-requisitos

### Dados

Os scripts esperam encontrar os arquivos DBC do SINAN-MENING no diretório raiz:

```
MENIBR15.dbc, MENIBR16.dbc, …, MENIBR25.dbc
```

Esses arquivos são públicos e podem ser baixados do FTP do DATASUS:

```
ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/
ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/PRELIM/    (ano corrente)
```

Em R, exemplo de download:

```r
ano <- 2024
url <- paste0("ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/",
              "MENIBR", substr(ano, 3, 4), ".dbc")
download.file(url, paste0("MENIBR", substr(ano, 3, 4), ".dbc"), mode = "wb")
```

### Pacotes R

```r
install.packages(c(
  "read.dbc", "dplyr", "tidyr", "stringr", "readr", "sidrar",
  "geobr", "sf", "spdep", "ggplot2", "ggrepel", "ggspatial",
  "cowplot", "knitr", "kableExtra", "tibble", "sandwich", "lmtest",
  "car", "segmented", "prais", "mgcv", "mice", "geepack", "ragg"
))
```

## Fontes de dados

- **SINAN-MENING** (Sistema de Informação de Agravos de Notificação — Meningite): DATASUS.
- **Estimativas populacionais municipais anuais**: SIDRA/IBGE, tabela 6579.
- **Distribuição etária por sexo**: SIDRA/IBGE, tabela 9514 (Censo Demográfico 2022).
- **Malha territorial**: `geobr::read_municipality(code_muni = "MG", year = 2022)`.

Os dados são públicos e re-obteníveis pelos scripts. Não versionados aqui (são grandes e regeneráveis).

## Período e área

- **Período**: 2015–2025 (11 anos)
- **Área**: macrorregiões de saúde Triângulo Norte e Triângulo Sul, Minas Gerais (54 municípios)

## Critério de seleção dos casos

Casos confirmados (`CLASSI_FIN == 1`) cuja residência (`ID_MN_RESI`) pertence ao Triângulo Mineiro. A unidade analítica é o residente, não o local de notificação.

## Métodos principais

| Método | Onde |
|---|---|
| Estimador Bayesiano Empírico Global (suavização de taxas) | `R/04_espacial_lisa.R` |
| Padronização indireta do SMR por idade × sexo (Censo 2022) | `R/03_smr.R` |
| Índice de Moran Global (taxa EB) | `R/04_espacial_lisa.R` |
| LISA com correção FDR (Benjamini-Hochberg) | `R/04_espacial_lisa.R` |
| Regressão de Poisson modificada com variância robusta HC3 | `R/06_regressao_poisson.R` |
| Joinpoint / Prais-Winsten / GAM (tendência) | `R/07_supl_tendencia.R` |
| GEE com cluster por município | `R/08_supl_regressao.R` |
| Imputação múltipla (mice) para raça/cor | `R/08_supl_regressao.R` |

## Licença

MIT (apenas para o código). Os dados do SINAN/DATASUS são de domínio público e seguem as políticas do Ministério da Saúde.

## Declaração de uso de inteligência artificial

A ferramenta Claude (Anthropic, modelo Opus 4.7) foi utilizada como apoio à
organização e refatoração do código R, à implementação técnica dos procedimentos
estatísticos e à revisão da redação em português. A concepção do estudo, o
delineamento analítico, a interpretação dos resultados e as decisões editoriais
foram de responsabilidade exclusiva dos autores, que revisaram integralmente o
conteúdo produzido. Em conformidade com COPE (2023) e ICMJE (2023).
