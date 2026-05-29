# Pipeline R — Meningite no Triângulo Mineiro (2015–2025)

Scripts R que reproduzem integralmente as análises e figuras do manuscrito
submetido à *Revista Brasileira de Epidemiologia*.

## Ordem de execução

Rode na ordem numérica. Cada script declara suas dependências e gera os
arquivos esperados pelos seguintes.

| # | Script | Função |
|---|---|---|
| 00 | `00_baixar_dbc_sinan.R` | Baixa MENIBR15.dbc…MENIBR25.dbc do FTP do DATASUS |
| 01 | `01_dados_setup.R` | Lê DBCs, consulta SIDRA, monta `dados_base.rds` |
| 02 | `02_baixar_referencias.R` | População BR/MG/SE por (ano × sexo × faixa) → `populacao_br_mg.rds` |
| 03 | `03_smr.R` | SMR padronizado por idade × sexo (padronização indireta) |
| 04 | `04_espacial_lisa.R` | Moran Global + LISA com correção FDR + mapas |
| 05 | `05_perfil_sazonalidade.R` | Perfil epidemiológico, sazonalidade, letalidade |
| 06 | `06_regressao_poisson.R` | Poisson modificada (HC3) — óbito e encerramento prolongado |
| 07 | `07_supl_tendencia.R` | **Suplementar**: Joinpoint, Prais-Winsten, GAM, SMR sem Uberaba/Uberlândia |
| 08 | `08_supl_regressao.R` | **Suplementar**: GEE com cluster por município, GAM com spline, mice (imputação múltipla) |
| 09 | `09_auditoria_residencia.R` | **Suplementar**: Auditoria empírica do campo `ID_MN_RESI` (residência × notificação) |
| — | `exportar_figuras.R` | Figuras 1 e 2 em 300 dpi (PNG + TIFF + PDF) |

## Uso

A partir do diretório raiz do projeto:

```r
# Aquisição e preparação (uma única vez)
source("R/00_baixar_dbc_sinan.R")
source("R/01_dados_setup.R")
source("R/02_baixar_referencias.R")

# Análises principais
source("R/03_smr.R")
source("R/04_espacial_lisa.R")
source("R/05_perfil_sazonalidade.R")
source("R/06_regressao_poisson.R")

# Análises de sensibilidade
source("R/07_supl_tendencia.R")
source("R/08_supl_regressao.R")
source("R/09_auditoria_residencia.R")

# Figuras finais
source("R/exportar_figuras.R")
```

## Dependências (pacotes)

```r
install.packages(c(
  # I/O e manipulação
  "read.dbc", "dplyr", "tidyr", "stringr", "readr", "tibble",
  # Estatística e inferência
  "spdep", "sandwich", "lmtest", "car", "segmented", "prais",
  "mgcv", "mice", "geepack",
  # Geoespacial
  "sf", "geobr", "sidrar",
  # Visualização
  "ggplot2", "ggrepel", "ggspatial", "cowplot", "ragg",
  # Tabelas
  "knitr", "kableExtra"
))
```

R ≥ 4.2 recomendado.

## Fonte dos dados

- **SINAN/DATASUS**: arquivos `MENIBR15.dbc` a `MENIBR25.dbc` (FTP do DATASUS).
- **IBGE/SIDRA**: tabela 6579 (estimativas populacionais municipais anuais);
  tabela 9514 (Censo 2022, idade simples × sexo); tabela 200 (Censo 2010, faixas
  etárias quinquenais × sexo — utilizada na análise de sensibilidade do SMR).
- **Shapefile**: pacote `geobr` (referência 2022).

Os dados são públicos e obtidos automaticamente pelos scripts `00` e `02`.

## Estrutura de saída

Após executar o pipeline completo, os seguintes arquivos são gerados (não
versionados, ficam apenas localmente):

```
dados_base.rds                       Tabelas mestre do estudo
populacao_br_mg.rds                  Populações de referência por (ano × uf × sexo × faixa)
pop_idade_muni_censo2022.rds         Cache do Censo 2022 (idade × sexo × município)
pop_tm_faixa_sexo_censo2010.rds      Cache do Censo 2010 (sensibilidade SMR)
mapa_inset_brasil_mg.rds             Cache dos shapefiles BR e MG
casos_por_faixa.rds                  Casos agregados por (ano × sexo × faixa)
auditoria_residencia/                CSVs da auditoria de residência (script 09)
figs/Figura1_incidencia.{png,tiff,pdf}    Mapa de incidência EB (300 dpi)
figs/Figura2_LISA.{png,tiff,pdf}          Mapa LISA (300 dpi)
```
