# Pipeline de análise — Meningite no Triângulo Mineiro (2015–2025)

Códigos R que produzem todos os resultados, tabelas e figuras do manuscrito submetido à *Revista Brasileira de Epidemiologia*.

Os scripts foram extraídos automaticamente (via `knitr::purl()`) dos arquivos `.Rmd` que produzem os relatórios `.docx`. Esta pasta contém **apenas o código R**, para citação/reuso. Os `.Rmd` originais ficam no diretório raiz.

## Ordem de execução

Rode na ordem numérica. Cada script declara suas dependências (arquivos `.rds`) e gera arquivos esperados pelos seguintes.

| # | Script | Função |
|---|---|---|
| 01 | `01_dados_setup.R` | Lê arquivos DBC do SINAN e estimativas SIDRA; gera `dados_base.rds` |
| 02 | `02_baixar_referencias.R` | População BR/MG/SE por (ano × sexo × faixa); gera `populacao_br_mg.rds` |
| 03 | `03_smr.R` | SMR padronizado por idade × sexo (padronização indireta) |
| 04 | `04_espacial_lisa.R` | Moran Global + LISA com FDR + mapas |
| 05 | `05_perfil_sazonalidade.R` | Perfil epidemiológico, sazonalidade, letalidade |
| 06 | `06_regressao_poisson.R` | Modelos de Poisson modificada (HC3) — óbito e encerramento prolongado |
| 07 | `07_supl_tendencia.R` | **Suplementar**: Joinpoint, Prais-Winsten, GAM; SMR sem Uberlândia/Uberaba |
| 08 | `08_supl_regressao.R` | **Suplementar**: GEE com cluster por município, GAM com spline para idade, imputação múltipla (`mice`) |

## Como rodar

A partir do diretório raiz do projeto:

```r
# Setup (uma única vez ou quando os dados forem atualizados)
source("R/01_dados_setup.R")
source("R/02_baixar_referencias.R")

# Análises principais (cada uma gera um .docx via render do .Rmd correspondente)
rmarkdown::render("02_smr.Rmd")
rmarkdown::render("03_espacial_lisa.Rmd")
rmarkdown::render("04_perfil_sazonalidade.Rmd")
rmarkdown::render("05_regressao_poisson.Rmd")

# Análises suplementares
rmarkdown::render("02b_tendencia_suplementar.Rmd")
rmarkdown::render("05b_regressao_sensibilidades.Rmd")

# Manuscrito final
rmarkdown::render("00_manuscrito_rbe.Rmd")
```

Ou, para executar apenas os scripts R (sem gerar `.docx`):

```r
source("R/01_dados_setup.R")
source("R/02_baixar_referencias.R")
source("R/03_smr.R")
source("R/04_espacial_lisa.R")
# ...
```

## Dependências (pacotes)

`read.dbc`, `dplyr`, `tidyr`, `stringr`, `readr`, `sidrar`, `geobr`, `sf`, `spdep`,
`ggplot2`, `ggrepel`, `knitr`, `kableExtra`, `tibble`, `sandwich`, `lmtest`, `car`,
`segmented`, `prais`, `mgcv`, `mice`, `geepack`, `ragg`.

## Fonte dos dados

- **SINAN/DATASUS**: arquivos `MENIBR15.dbc`–`MENIBR25.dbc` (FTP do DATASUS)
- **IBGE/SIDRA**: tabela 6579 (estimativas populacionais municipais anuais);
  tabela 9514 (Censo 2022, idade simples × sexo)
- **Shapefile**: pacote `geobr` (referência 2022)

Os dados são públicos e podem ser re-obtidos pelos scripts `01_dados_setup.R` e
`02_baixar_referencias.R`.
