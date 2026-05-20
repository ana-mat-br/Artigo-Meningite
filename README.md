# Artigo-Meningite

Códigos R do estudo **"Concentração da incidência de meningite em centros de referência hospitalar no Triângulo Mineiro, Brasil, 2015–2025: análise espacial bayesiana, LISA e regressão de Poisson modificada"**, submetido à *Revista Brasileira de Epidemiologia*.

> **Achado central**: o aparente excesso regional de incidência de meningite (SMR vs Brasil = 1,80; SMR vs MG = 2,81) é predominantemente explicado pela concentração de notificações em dois centros de referência hospitalar (Uberaba e Uberlândia, que reúnem ~76% dos casos). Excluindo esses dois municípios, o SMR vs Brasil cai para 0,84 e o gradiente etário (RR = 2,46 para ≥50 anos) permanece como o principal preditor de óbito, robusto à fase pandêmica.

---

## Autores

- Pedro Henrique Rodrigues Braga
- Ana Paula Fernandes
- Aline Dias Paiva

Universidade Federal do Triângulo Mineiro (UFTM), Uberaba (MG), Brasil.

---

## Estrutura do repositório

```
Artigo-Meningite/
├── .gitignore                  Filtra dados, cache, .docx e .Rmd
├── README.md                   Este arquivo
└── R/
    ├── README.md                       Documentação interna
    ├── 00_baixar_dbc_sinan.R           Baixa MENIBR*.dbc do DATASUS
    ├── 01_dados_setup.R                Lê DBCs + SIDRA, monta dados_base.rds
    ├── 02_baixar_referencias.R         Pop. BR/MG/SE por idade × sexo (SIDRA)
    ├── 03_smr.R                        SMR padronizado por idade × sexo
    ├── 04_espacial_lisa.R              Moran Global + LISA com correção FDR
    ├── 05_perfil_sazonalidade.R        Perfil + sazonalidade + letalidade
    ├── 06_regressao_poisson.R          Poisson HC3 (óbito + encerramento)
    ├── 07_supl_tendencia.R             Joinpoint, Prais-Winsten, GAM, SMR sem UDI/UBE
    ├── 08_supl_regressao.R             GEE, GAM com spline, imputação múltipla (mice)
    └── exportar_figuras.R              Figuras 1 e 2 em 300 dpi (PNG + TIFF + PDF)
```

Apenas código R é versionado. Dados brutos (`*.dbc`), caches (`*.rds`) e outputs (`*.docx`, `*.png`) são gerados localmente pelos scripts.

---

## Pipeline de execução

A partir da raiz do projeto, no R:

```r
# Etapa 1 — Aquisição e preparação dos dados (executar uma vez)
source("R/00_baixar_dbc_sinan.R")     # ~30 MB do FTP do DATASUS
source("R/01_dados_setup.R")          # gera dados_base.rds
source("R/02_baixar_referencias.R")   # gera populacao_br_mg.rds

# Etapa 2 — Análises do manuscrito principal
source("R/03_smr.R")                  # Tabela 1 (SMR cumulativo)
source("R/04_espacial_lisa.R")        # Moran + LISA + mapas
source("R/05_perfil_sazonalidade.R")  # Tabelas de perfil
source("R/06_regressao_poisson.R")    # Tabela 2 (RR óbito + encerramento)

# Etapa 3 — Análises de sensibilidade (material suplementar)
source("R/07_supl_tendencia.R")
source("R/08_supl_regressao.R")

# Etapa 4 — Figuras finais para submissão
source("R/exportar_figuras.R")        # figs/Figura1_*.{png,tiff,pdf}
```

---

## Pré-requisitos

### Dados

Os arquivos `MENIBR15.dbc` a `MENIBR25.dbc` são baixados automaticamente do FTP do DATASUS:

```
ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/    # anos consolidados
ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/PRELIM/    # ano corrente
```

### Pacotes R

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

---

## Fontes de dados

| Fonte | Conteúdo | Período |
|---|---|---|
| **SINAN/DATASUS** (MENIBR*.dbc) | Notificações de meningite (Brasil completo, residentes do TM são filtrados) | 2015–2025 |
| **SIDRA/IBGE** tabela 6579 | Estimativas populacionais municipais | 2015–2024 (2025 propagada) |
| **SIDRA/IBGE** tabela 9514 | População por idade simples × sexo (Censo Demográfico 2022) | 2022 |
| **geobr** | Malha territorial dos municípios de MG | 2022 |

---

## Desenho do estudo

- **Período**: 2015–2025 (11 anos)
- **Área**: macrorregiões de saúde Triângulo Norte e Triângulo Sul, MG (54 municípios)
- **Unidade de análise**: município de residência (`ID_MN_RESI`)
- **Critério de caso**: `CLASSI_FIN == 1` (confirmado)
- **Desfechos**: óbito por meningite (`EVOLUCAO == 2`); encerramento prolongado (≥ 30 dias entre `DT_SIN_PRI` e `DT_ENCERRA`)

---

## Métodos

| Método | Implementação |
|---|---|
| Estimador Bayesiano Empírico Global (suavização) | `R/04_espacial_lisa.R` |
| Padronização indireta de SMR (idade × sexo, Censo 2022) | `R/03_smr.R` |
| Índice de Moran Global (taxa EB) | `R/04_espacial_lisa.R` |
| LISA com correção FDR (Benjamini-Hochberg) | `R/04_espacial_lisa.R` |
| Regressão de Poisson modificada com HC3 (sandwich) | `R/06_regressao_poisson.R` |
| Joinpoint segmentado + Prais-Winsten + GAM | `R/07_supl_tendencia.R` |
| SMR de sensibilidade excluindo Uberaba e Uberlândia | `R/07_supl_tendencia.R` |
| GEE com cluster por município | `R/08_supl_regressao.R` |
| GAM Poisson com spline para idade | `R/08_supl_regressao.R` |
| Imputação múltipla (mice) para raça/cor | `R/08_supl_regressao.R` |

---

## Outputs gerados

Após executar o pipeline completo, os seguintes arquivos são criados no diretório raiz (não versionados):

```
dados_base.rds                       Tabelas mestre do estudo
populacao_br_mg.rds                  Populações de referência
pop_idade_muni_censo2022.rds         Cache do Censo 2022 (idade × sexo × município)
mapa_inset_brasil_mg.rds             Cache dos shapefiles BR e MG
casos_por_faixa.rds                  Casos agregados por (ano × sexo × faixa)
figs/Figura1_incidencia.{png,tiff,pdf}    Mapa de incidência (300 dpi)
figs/Figura2_LISA.{png,tiff,pdf}          Mapa LISA (300 dpi)
```

---

## Como citar

> Braga PHR, Fernandes AP, Paiva AD. Concentração da incidência de meningite em centros de referência hospitalar no Triângulo Mineiro, Brasil, 2015–2025: análise espacial bayesiana, LISA e regressão de Poisson modificada. *Rev Bras Epidemiol*. [no prelo].

Código-fonte: <https://github.com/ana-mat-br/Artigo-Meningite>

---

## Licença

Código liberado sob a **licença MIT**. Os dados do SINAN/DATASUS e do IBGE são públicos e seguem as políticas dos respectivos órgãos governamentais.

---

## Declaração de uso de inteligência artificial

A ferramenta **Claude** (Anthropic, modelo Opus 4.7) foi utilizada como apoio à organização e refatoração do código R, à implementação técnica dos procedimentos estatísticos e à revisão da redação. A concepção do estudo, o delineamento analítico, a interpretação dos resultados e as decisões editoriais foram de responsabilidade exclusiva dos autores, em conformidade com as orientações do **COPE** (2023) e do **ICMJE** (2023).
