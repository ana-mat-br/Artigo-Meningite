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
                      dpi = 300, fig.width = 10, fig.height = 6.5)

for (pkg in c("cowplot", "geobr", "ggspatial")) {
  if (!requireNamespace(pkg, quietly = TRUE))
    install.packages(pkg, repos = "https://cloud.r-project.org")
}
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(stringr); library(ggplot2)
  library(ggrepel); library(sf); library(spdep); library(knitr); library(kableExtra)
  library(cowplot); library(geobr); library(ggspatial)
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

# Insets Brasil + MG (com cache local) ----------------------------------------
cache_inset <- "mapa_inset_brasil_mg.rds"
if (file.exists(cache_inset)) {
  insets <- readRDS(cache_inset)
} else {
  uf_br <- read_state(year = 2020, showProgress = FALSE) |> st_transform(4674)
  mg    <- uf_br |> filter(abbrev_state == "MG")
  insets <- list(br = uf_br, mg = mg)
  saveRDS(insets, cache_inset)
}
# União dos municípios do TM — contorno real (não bbox)
tm_union <- st_union(mapa)

# ----- Insets cartográficos: compactos, mesma largura, escala pequena --------
# Texto-rótulo do nome do território usando grid::textGrob — alinhado com a escala
texto_rotulo_inset <- function(rotulo) {
  annotation_custom(
    grob = grid::textGrob(
      rotulo,
      x = unit(0.04, "npc"), y = unit(0.05, "npc"),
      hjust = 0, vjust = 0,
      gp = grid::gpar(fontsize = 10, fontface = "bold", col = "gray20")
    ),
    xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
  )
}

inset_brasil <- function() {
  ggplot(insets$br) +
    geom_sf(fill = "white", color = "gray55", linewidth = 0.18) +
    geom_sf(data = insets$mg, fill = "black", color = "black", linewidth = 0.25) +
    texto_rotulo_inset("BRASIL") +
    annotation_scale(location = "br", width_hint = 0.35, style = "bar",
                     text_cex = 0.7, line_width = 0.5, height = unit(0.14, "cm"),
                     pad_x = unit(0.18, "cm"), pad_y = unit(0.05, "cm"),
                     bar_cols = c("gray20","white")) +
    coord_sf(expand = TRUE, clip = "off") +
    scale_y_continuous(expand = expansion(mult = c(0.15, 0.02))) +
    theme_void() +
    theme(plot.background = element_rect(fill = "white", color = "gray85",
                                          linewidth = 0.3),
          panel.background = element_rect(fill = "white", color = NA),
          plot.margin = margin(2, 2, 4, 2))
}
inset_mg <- function() {
  ggplot(insets$mg) +
    geom_sf(fill = "white", color = "gray45", linewidth = 0.3) +
    geom_sf(data = tm_union, fill = "black", color = "black", linewidth = 0.4) +
    texto_rotulo_inset("MG") +
    annotation_scale(location = "br", width_hint = 0.32, style = "bar",
                     text_cex = 0.7, line_width = 0.5, height = unit(0.14, "cm"),
                     pad_x = unit(0.18, "cm"), pad_y = unit(0.05, "cm"),
                     bar_cols = c("gray20","white")) +
    coord_sf(expand = TRUE, clip = "off") +
    scale_y_continuous(expand = expansion(mult = c(0.15, 0.02))) +
    theme_void() +
    theme(plot.background = element_rect(fill = "white", color = "gray85",
                                          linewidth = 0.3),
          panel.background = element_rect(fill = "white", color = NA),
          plot.margin = margin(2, 2, 4, 2))
}

# ----- Painel auxiliar: rosa dos ventos (estilo fancy, mais visível) ---------
plot_rosa_ventos <- function() {
  ggplot() +
    geom_blank() +
    coord_fixed(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
    annotation_north_arrow(
      location = "tl", which_north = "true",
      pad_x = unit(0, "cm"), pad_y = unit(0, "cm"),
      height = unit(2.0, "cm"), width = unit(2.0, "cm"),
      style = north_arrow_fancy_orienteering(
        text_size = 11, fill = c("white", "gray15"),
        line_col = "gray15", text_col = "gray15")
    ) +
    theme_void() +
    theme(plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA))
}

# ----- Figura final com qualidade cartográfica -------------------------------
# Layout:
#   * Mapa principal grande à esquerda
#   * Coluna direita (de cima para baixo, agrupados): rosa | Brasil | MG
#   * Legenda da taxa no canto sup. esq. do mapa principal (onde estava o N)
montar_figura_com_insets <- function(mapa_principal,
                                       legenda_titulo = NULL) {
  mapa_principal <- mapa_principal +
    theme(plot.background  = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA))

  p_com_leg <- mapa_principal + theme(
    legend.position  = "top",
    legend.direction = "horizontal",
    legend.title.position = "top",
    legend.title = element_text(size = 13, face = "bold"),
    legend.text  = element_text(size = 12),
    legend.key.width  = unit(1.4, "cm"),
    legend.key.height = unit(0.5, "cm"),
    legend.background = element_rect(fill = alpha("white", 0.9), color = NA),
    legend.margin = margin(6, 10, 6, 10)
  )
  legenda <- cowplot::get_legend(p_com_leg)
  mapa_clean <- mapa_principal + theme(legend.position = "none")

  cowplot::ggdraw() +
    cowplot::draw_grob(grid::rectGrob(gp = grid::gpar(fill = "white", col = NA))) +
    # Mapa principal grande
    cowplot::draw_plot(mapa_clean,     x = 0.00, y = 0.00, width = 0.76, height = 1.00) +
    # Legenda da taxa no canto sup. esq. do mapa principal
    cowplot::draw_plot(legenda,        x = 0.01, y = 0.80, width = 0.40, height = 0.18) +
    # Rosa dos ventos no topo da coluna direita (maior e mais visível)
    cowplot::draw_plot(plot_rosa_ventos(),
                                       x = 0.81, y = 0.78, width = 0.16, height = 0.20) +
    # Brasil logo abaixo da rosa
    cowplot::draw_plot(inset_brasil(), x = 0.76, y = 0.42, width = 0.23, height = 0.38) +
    # MG colado ao Brasil (gap mínimo)
    cowplot::draw_plot(inset_mg(),     x = 0.76, y = 0.06, width = 0.23, height = 0.38)
}


## ----moran-global, include=FALSE----------------------------------------------
set.seed(2025)
moran_bruto <- moran.mc(mapa$taxa_cumulativa, pesos, nsim = 999,
                        zero.policy = TRUE, alternative = "greater")
moran_eb    <- moran.mc(mapa$taxa_eb,         pesos, nsim = 999,
                        zero.policy = TRUE, alternative = "greater")
i_bruto <- moran_bruto$statistic; p_bruto <- moran_bruto$p.value
i_eb    <- moran_eb$statistic;    p_eb    <- moran_eb$p.value


## ----tab-moran-global---------------------------------------------------------
data.frame(
  Análise      = c("Taxa bruta (principal)", "Taxa EB (sensibilidade)"),
  `I de Moran` = c(fmt4(i_bruto), fmt4(i_eb)),
  `p`          = c(fmt_p(p_bruto), fmt_p(p_eb)),
  check.names  = FALSE
) |> kable(caption = paste0("Tabela 1. Índice de Moran Global da incidência de meningite — Triângulo Mineiro, ", periodo_lbl, "."),
           align = "c")


## ----lisa, include=FALSE------------------------------------------------------
# LISA PRINCIPAL — sobre taxa bruta (recomendado: evita artefatos do shrinkage EB)
lisa <- localmoran(mapa$taxa_cumulativa, pesos, zero.policy = TRUE)
mapa$moran_local <- lisa[, 1]
mapa$moran_p     <- lisa[, 5]
mapa$moran_p_fdr <- p.adjust(mapa$moran_p, method = "BH")

media_taxa <- mean(mapa$taxa_cumulativa, na.rm = TRUE)
lag_taxa   <- lag.listw(pesos, mapa$taxa_cumulativa, zero.policy = TRUE)

mapa$quadrante <- case_when(
  mapa$taxa_cumulativa >= media_taxa & lag_taxa >= media_taxa & mapa$moran_p_fdr < 0.05 ~ "Alto-Alto",
  mapa$taxa_cumulativa <  media_taxa & lag_taxa <  media_taxa & mapa$moran_p_fdr < 0.05 ~ "Baixo-Baixo",
  mapa$taxa_cumulativa >= media_taxa & lag_taxa <  media_taxa & mapa$moran_p_fdr < 0.05 ~ "Alto-Baixo",
  mapa$taxa_cumulativa <  media_taxa & lag_taxa >= media_taxa & mapa$moran_p_fdr < 0.05 ~ "Baixo-Alto",
  TRUE ~ "Não significativo"
)

# LISA SENSIBILIDADE — sobre taxa EB (para comparação)
lisa_eb <- localmoran(mapa$taxa_eb, pesos, zero.policy = TRUE)
mapa$moran_p_eb     <- lisa_eb[, 5]
mapa$moran_p_fdr_eb <- p.adjust(mapa$moran_p_eb, method = "BH")
media_taxa_eb <- mean(mapa$taxa_eb, na.rm = TRUE)
lag_taxa_eb   <- lag.listw(pesos, mapa$taxa_eb, zero.policy = TRUE)
mapa$quadrante_eb <- case_when(
  mapa$taxa_eb >= media_taxa_eb & lag_taxa_eb >= media_taxa_eb & mapa$moran_p_fdr_eb < 0.05 ~ "Alto-Alto",
  mapa$taxa_eb <  media_taxa_eb & lag_taxa_eb <  media_taxa_eb & mapa$moran_p_fdr_eb < 0.05 ~ "Baixo-Baixo",
  mapa$taxa_eb >= media_taxa_eb & lag_taxa_eb <  media_taxa_eb & mapa$moran_p_fdr_eb < 0.05 ~ "Alto-Baixo",
  mapa$taxa_eb <  media_taxa_eb & lag_taxa_eb >= media_taxa_eb & mapa$moran_p_fdr_eb < 0.05 ~ "Baixo-Alto",
  TRUE ~ "Não significativo"
)


## ----tab-lisa-----------------------------------------------------------------
bind_rows(
  as.data.frame(table(Quadrante = mapa$quadrante)) |>
    mutate(Análise = "Taxa bruta (principal)"),
  as.data.frame(table(Quadrante = mapa$quadrante_eb)) |>
    mutate(Análise = "Taxa EB (sensibilidade)")
) |>
  rename(`N municípios` = Freq) |>
  dplyr::select(Análise, Quadrante, `N municípios`) |>
  arrange(Análise, desc(`N municípios`)) |>
  kable(caption = paste0("Tabela 2. Quadrantes LISA (p_FDR < 0,05) — análise principal (taxa bruta) e sensibilidade (taxa EB), ", periodo_lbl, "."),
        align = "c", row.names = FALSE)


## ----sanity-lisa, results='asis'----------------------------------------------
cat("**SANITY CHECK — LISA**\n\n")
cat(sprintf("- Moran Global (taxa bruta): I = %s; p = %s\n", fmt4(i_bruto), fmt_p(p_bruto)))
cat(sprintf("- Moran Global (taxa EB):    I = %s; p = %s\n", fmt4(i_eb),    fmt_p(p_eb)))
cat(sprintf("- N municípios significativos (p_FDR<0,05) — bruto: %d; EB: %d\n",
            sum(mapa$quadrante    != "Não significativo"),
            sum(mapa$quadrante_eb != "Não significativo")))
if (any(mapa$quadrante != "Não significativo")) {
  sig_principal <- st_drop_geometry(mapa) |>
    dplyr::filter(quadrante != "Não significativo") |>
    dplyr::select(municipio_label, quadrante, taxa_cumulativa, moran_p_fdr)
  cat("\nMunicípios significativos (análise principal):\n\n")
  print(knitr::kable(sig_principal, digits = 4))
}


## ----fig-incidencia, fig.cap=paste0("Figura 1. Taxas de incidência de meningite suavizadas (Estimador Bayesiano Empírico) por município, Triângulo Mineiro, ", periodo_lbl, ".")----
cents <- st_centroid(mapa); cd <- st_drop_geometry(mapa)
cd$lon <- st_coordinates(cents)[,1]; cd$lat <- st_coordinates(cents)[,2]
top5 <- cd |> slice_max(taxa_eb, n = 5)

p_inc <- ggplot(mapa) +
  geom_sf(aes(fill = taxa_eb), color = "gray40", linewidth = 0.3) +
  scale_fill_gradientn(colours = c("#ffffb2","#fecc5c","#fd8d3c","#f03b20","#bd0026"),
                       name = "Taxa de incidência\n(EB) por\n100.000 hab./ano") +
  geom_label_repel(data = top5, aes(x = lon, y = lat, label = municipio_label),
                   size = 3.4, fontface = "bold", box.padding = 0.7,
                   label.size = 0.25, label.padding = unit(0.18, "lines"),
                   arrow = arrow(length = unit(0.018,"npc"), type = "closed"),
                   min.segment.length = 0, max.overlaps = 20) +
  annotation_scale(location = "bl", width_hint = 0.22, style = "bar",
                   text_cex = 0.8, line_width = 0.6,
                   height = unit(0.2, "cm"),
                   pad_x = unit(0.4, "cm"), pad_y = unit(0.4, "cm"),
                   bar_cols = c("gray15","white")) +
  coord_sf(xlim = c(bb["xmin"]-buf, bb["xmax"]+buf),
           ylim = c(bb["ymin"]-buf, bb["ymax"]+buf), expand = FALSE) +
  theme_void(base_size = 10)

montar_figura_com_insets(p_inc)


## ----fig-lisa, fig.cap=paste0("Figura 2. Aglomerados espaciais locais (LISA) da incidência de meningite com correção FDR (p < 0,05) por município, Triângulo Mineiro, ", periodo_lbl, ".")----
cores_lisa <- c("Alto-Alto"="#d7191c","Baixo-Baixo"="#2c7bb6",
                "Alto-Baixo"="#fdae61","Baixo-Alto"="#abd9e9",
                "Não significativo"="#f0f0f0")
sig <- cd |> filter(quadrante != "Não significativo")

p_lisa <- ggplot(mapa) +
  geom_sf(aes(fill = quadrante), color = "gray40", linewidth = 0.3) +
  scale_fill_manual(values = cores_lisa, name = "Cluster LISA") +
  geom_label_repel(data = sig, aes(x = lon, y = lat, label = municipio_label),
                   size = 3.4, fontface = "bold", box.padding = 0.7,
                   label.size = 0.25, label.padding = unit(0.18, "lines"),
                   arrow = arrow(length = unit(0.018,"npc"), type = "closed"),
                   min.segment.length = 0, max.overlaps = 20) +
  annotation_scale(location = "bl", width_hint = 0.22, style = "bar",
                   text_cex = 0.8, line_width = 0.6,
                   height = unit(0.2, "cm"),
                   pad_x = unit(0.4, "cm"), pad_y = unit(0.4, "cm"),
                   bar_cols = c("gray15","white")) +
  coord_sf(xlim = c(bb["xmin"]-buf, bb["xmax"]+buf),
           ylim = c(bb["ymin"]-buf, bb["ymax"]+buf), expand = FALSE) +
  theme_void(base_size = 10)

montar_figura_com_insets(p_lisa)

