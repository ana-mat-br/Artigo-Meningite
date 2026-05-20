# =============================================================================
# 04_espacial_lisa — Moran Global e LISA com correção FDR
#
# Origem: Extraído de 03_espacial_lisa.Rmd via knitr::purl()
# Depende: dados_base.rds (01)
# Gera:    Mapas, Índice de Moran Global, quadrantes LISA (Alto-Alto, etc.)
#
# Projeto: Análise da meningite no Triângulo Mineiro (2015–2025)
# =============================================================================

## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE,
                      dpi = 300, fig.width = 6.5, fig.height = 5.5)
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(stringr); library(ggplot2)
  library(ggrepel); library(sf); library(spdep); library(knitr); library(kableExtra)
})
select <- dplyr::select; filter <- dplyr::filter; mutate <- dplyr::mutate
summarise <- dplyr::summarise; group_by <- dplyr::group_by; rename <- dplyr::rename

if (!file.exists("dados_base.rds"))
  stop("Rode antes: source('01_dados_setup.R')")
db <- readRDS("dados_base.rds")
list2env(db$config, envir = environment())
mapa  <- db$mapa
pesos <- db$pesos

fmt_p <- function(x) ifelse(x < 0.001, "< 0,001",
                            formatC(x, format = "f", digits = 3, decimal.mark = ","))
fmt4  <- function(x) formatC(x, format = "f", digits = 4, decimal.mark = ",")
buf   <- 0.08
bb    <- st_bbox(mapa)


## ----moran-global, include=FALSE----------------------------------------------
moran_bruto <- moran.test(mapa$taxa_cumulativa, pesos,
                          zero.policy = TRUE, alternative = "greater")
moran_eb    <- moran.test(mapa$taxa_eb,         pesos,
                          zero.policy = TRUE, alternative = "greater")
i_bruto <- moran_bruto$estimate[1]; p_bruto <- moran_bruto$p.value
i_eb    <- moran_eb$estimate[1];    p_eb    <- moran_eb$p.value


## ----tab-moran-global---------------------------------------------------------
data.frame(
  Análise      = c("Taxa bruta", "Taxa EB"),
  `I de Moran` = c(fmt4(i_bruto), fmt4(i_eb)),
  `p`          = c(fmt_p(p_bruto), fmt_p(p_eb)),
  check.names  = FALSE
) |> kable(caption = paste0("Tabela 1. Índice de Moran Global — Triângulo Mineiro, ", periodo_lbl, "."),
           align = "c")


## ----lisa, include=FALSE------------------------------------------------------
lisa <- localmoran(mapa$taxa_eb, pesos, zero.policy = TRUE)
mapa$moran_local <- lisa[, 1]
mapa$moran_p     <- lisa[, 5]
mapa$moran_p_fdr <- p.adjust(mapa$moran_p, method = "BH")

media_taxa <- mean(mapa$taxa_eb, na.rm = TRUE)
lag_taxa   <- lag.listw(pesos, mapa$taxa_eb, zero.policy = TRUE)

mapa$quadrante <- case_when(
  mapa$taxa_eb >= media_taxa & lag_taxa >= media_taxa & mapa$moran_p_fdr < 0.05 ~ "Alto-Alto",
  mapa$taxa_eb <  media_taxa & lag_taxa <  media_taxa & mapa$moran_p_fdr < 0.05 ~ "Baixo-Baixo",
  mapa$taxa_eb >= media_taxa & lag_taxa <  media_taxa & mapa$moran_p_fdr < 0.05 ~ "Alto-Baixo",
  mapa$taxa_eb <  media_taxa & lag_taxa >= media_taxa & mapa$moran_p_fdr < 0.05 ~ "Baixo-Alto",
  TRUE ~ "Não significativo"
)


## ----tab-lisa-----------------------------------------------------------------
as.data.frame(table(Quadrante = mapa$quadrante)) |>
  rename(`N municípios` = Freq) |>
  arrange(desc(`N municípios`)) |>
  kable(caption = paste0("Tabela 2. Quadrantes LISA (p_FDR < 0,05) — ", periodo_lbl, "."),
        align = "c")


## ----fig-incidencia, fig.cap=paste0("Figura 1. Taxas de incidência suavizadas (EB) por município — Triângulo Mineiro, ", periodo_lbl, ".")----
cents <- st_centroid(mapa); cd <- st_drop_geometry(mapa)
cd$lon <- st_coordinates(cents)[,1]; cd$lat <- st_coordinates(cents)[,2]
top5 <- cd |> slice_max(taxa_eb, n = 5)

ggplot(mapa) +
  geom_sf(aes(fill = taxa_eb), color = "gray40", linewidth = 0.3) +
  scale_fill_gradientn(colours = c("#ffffb2","#fecc5c","#fd8d3c","#f03b20","#bd0026"),
                       name = "Taxa/100k\n(EB)") +
  geom_label_repel(data = top5, aes(x = lon, y = lat, label = municipio_label),
                   size = 2.5, box.padding = 0.6, label.size = 0.2,
                   arrow = arrow(length = unit(0.015,"npc"), type = "closed"),
                   min.segment.length = 0, max.overlaps = 20) +
  coord_sf(xlim = c(bb["xmin"]-buf, bb["xmax"]+buf),
           ylim = c(bb["ymin"]-buf, bb["ymax"]+buf), expand = FALSE) +
  theme_void(base_size = 10) +
  theme(legend.position = "right")


## ----fig-lisa, fig.cap=paste0("Figura 2. Aglomerados LISA com correção FDR (p < 0,05) — ", periodo_lbl, ".")----
cores_lisa <- c("Alto-Alto"="#d7191c","Baixo-Baixo"="#2c7bb6",
                "Alto-Baixo"="#fdae61","Baixo-Alto"="#abd9e9",
                "Não significativo"="#f0f0f0")
sig <- cd |> filter(quadrante != "Não significativo")

ggplot(mapa) +
  geom_sf(aes(fill = quadrante), color = "gray40", linewidth = 0.3) +
  scale_fill_manual(values = cores_lisa, name = "Cluster LISA") +
  geom_label_repel(data = sig, aes(x = lon, y = lat, label = municipio_label),
                   size = 2.5, box.padding = 0.6, label.size = 0.2,
                   arrow = arrow(length = unit(0.015,"npc"), type = "closed"),
                   min.segment.length = 0, max.overlaps = 20) +
  coord_sf(xlim = c(bb["xmin"]-buf, bb["xmax"]+buf),
           ylim = c(bb["ymin"]-buf, bb["ymax"]+buf), expand = FALSE) +
  theme_void(base_size = 10) +
  theme(legend.position = "right")

