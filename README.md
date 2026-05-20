# Artigo-Meningite

Códigos R do artigo *"Dinâmica espaçotemporal da meningite em uma região de alta incidência no Triângulo Mineiro, Brasil, 2015–2025: análise bayesiana, LISA e regressão de Poisson modificada"*, submetido à *Revista Brasileira de Epidemiologia*.

## Autores

- Pedro Henrique Rodrigues Braga
- Ana Paula Fernandes (autora correspondente: anapaula.fernandes@uftm.edu.br)
- Aline Dias Paiva

Universidade Federal do Triângulo Mineiro (UFTM), Uberaba (MG), Brasil.

## Estrutura

Pipeline modular: cada `.Rmd` produz um `.docx` com tabelas e figuras; os mesmos blocos de código também estão disponíveis como scripts `.R` em `R/` (extraídos via `knitr::purl()`).

```
01_dados_setup.R                     Pé do pipeline (lê DBCs + SIDRA)
baixar_referencias_datasus.R         Pop. BR/MG/SE por (ano × sexo × faixa)

00_manuscrito_rbe.Rmd                Manuscrito principal (para a RBE)

02_smr.Rmd                           SMR padronizado por idade × sexo
03_espacial_lisa.Rmd                 Moran Global + LISA com correção FDR
04_perfil_sazonalidade.Rmd           Perfil epidemiológico + sazonalidade
05_regressao_poisson.Rmd             Poisson modificada (HC3): óbito + encerramento

02b_tendencia_suplementar.Rmd        Suplementar: Joinpoint, Prais-Winsten, GAM
                                     + SMR sem Uberlândia/Uberaba
05b_regressao_sensibilidades.Rmd     Suplementar: GEE, GAM com spline, mice

R/                                   Versão .R dos módulos (knitr::purl)
  README.md                          Documentação interna da pasta R/
```

## Como reproduzir

```r
# 1. Preparação dos dados (uma única vez)
source("01_dados_setup.R")
source("baixar_referencias_datasus.R")

# 2. Análises do manuscrito principal
rmarkdown::render("02_smr.Rmd")
rmarkdown::render("03_espacial_lisa.Rmd")
rmarkdown::render("04_perfil_sazonalidade.Rmd")
rmarkdown::render("05_regressao_poisson.Rmd")

# 3. Análises de sensibilidade (material suplementar)
rmarkdown::render("02b_tendencia_suplementar.Rmd")
rmarkdown::render("05b_regressao_sensibilidades.Rmd")

# 4. Manuscrito final
rmarkdown::render("00_manuscrito_rbe.Rmd")
```

## Fontes de dados

- **SINAN-MENING** (Sistema de Informação de Agravos de Notificação — Meningite): DATASUS, arquivos `MENIBR15.dbc` a `MENIBR25.dbc` (FTP do DATASUS).
- **Estimativas populacionais municipais anuais**: SIDRA/IBGE, tabela 6579.
- **Distribuição etária por sexo**: SIDRA/IBGE, tabela 9514 (Censo Demográfico 2022).
- **Malha territorial**: `geobr::read_municipality(code_muni = "MG", year = 2022)`.

Os dados são públicos e podem ser re-obtidos pelos scripts. Por serem grandes (~30 MB de `.dbc`) e regeneráveis, **não são versionados** neste repositório.

## Período e área

- **Período**: 2015–2025 (11 anos)
- **Área**: macrorregiões de saúde Triângulo Norte e Triângulo Sul, Minas Gerais (54 municípios)

## Critério de seleção dos casos

Casos confirmados (`CLASSI_FIN == 1`) cuja residência (`ID_MN_RESI`) pertence ao Triângulo Mineiro. A unidade analítica é o residente, não o local de notificação.

## Métodos principais

| Método | Onde |
|---|---|
| Estimador Bayesiano Empírico Global (suavização de taxas) | `03_espacial_lisa.Rmd` |
| Padronização indireta do SMR por idade × sexo (Censo 2022) | `02_smr.Rmd` |
| Índice de Moran Global (taxa EB) | `03_espacial_lisa.Rmd` |
| LISA com correção FDR (Benjamini-Hochberg) | `03_espacial_lisa.Rmd` |
| Regressão de Poisson modificada com variância robusta HC3 | `05_regressao_poisson.Rmd` |
| Joinpoint / Prais-Winsten / GAM (tendência) | `02b_tendencia_suplementar.Rmd` |
| GEE com cluster por município | `05b_regressao_sensibilidades.Rmd` |
| Imputação múltipla (mice) para raça/cor | `05b_regressao_sensibilidades.Rmd` |

## Pacotes

`read.dbc`, `dplyr`, `tidyr`, `stringr`, `readr`, `sidrar`, `geobr`, `sf`, `spdep`,
`ggplot2`, `ggrepel`, `knitr`, `kableExtra`, `tibble`, `sandwich`, `lmtest`, `car`,
`segmented`, `prais`, `mgcv`, `mice`, `geepack`, `ragg`.

## Licença

MIT (apenas para o código). Os dados do SINAN/DATASUS são de domínio público e seguem as políticas do Ministério da Saúde.

## Declaração de uso de inteligência artificial

A ferramenta Claude (Anthropic, modelo Opus 4.7) foi utilizada como apoio à
organização e refatoração do código R, à implementação técnica dos procedimentos
estatísticos e à revisão da redação em português. A concepção do estudo, o
delineamento analítico, a interpretação dos resultados e as decisões editoriais
foram de responsabilidade exclusiva dos autores, que revisaram integralmente o
conteúdo produzido. Detalhes adicionais no manuscrito (seção "Declaração de uso
de inteligência artificial").
