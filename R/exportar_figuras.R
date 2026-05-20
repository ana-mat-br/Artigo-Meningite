# =============================================================================
# exportar_figuras.R
#
# Exporta as Figuras 1 (incidência EB) e 2 (LISA) como arquivos standalone em
# 300 dpi (PNG e TIFF), prontos para submissão à Rev Bras Epidemiol.
#
# Saídas:
#   figs/Figura1_incidencia.png   (300 dpi, ~10 × 6.5 in)
#   figs/Figura1_incidencia.tiff  (300 dpi, sem compressão, LZW)
#   figs/Figura2_LISA.png
#   figs/Figura2_LISA.tiff
#
# Origem: replica o código de 03_espacial_lisa.Rmd para gerar as figuras
# autonomamente, sem o overhead do knit.
# =============================================================================

setwd("/Users/anafernandes/IndiceMoran")
dir.create("figs", showWarnings = FALSE)

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(stringr); library(ggplot2)
  library(ggrepel); library(sf); library(spdep)
  library(cowplot); library(geobr); library(ggspatial)
})

# --- Carregar dados ---
db <- readRDS("dados_base.rds")
list2env(db$config, envir = environment())
mapa  <- db$mapa
pesos <- db$pesos

if (file.exists("mapa_inset_brasil_mg.rds")) {
  insets <- readRDS("mapa_inset_brasil_mg.rds")
} else {
  uf_br <- read_state(year = 2020, showProgress = FALSE) |> st_transform(4674)
  insets <- list(br = uf_br, mg = uf_br |> filter(abbrev_state == "MG"))
  saveRDS(insets, "mapa_inset_brasil_mg.rds")
}
tm_union <- st_union(mapa)
buf <- 0.08; bb <- st_bbox(mapa)

fmt_p <- function(x) ifelse(x < 0.001, "< 0,001",
                            formatC(x, format = "f", digits = 3, decimal.mark = ","))
fmt4  <- function(x) formatC(x, format = "f", digits = 4, decimal.mark = ",")

# --- Insets, rosa e função de composição (cópia do 03_espacial_lisa.Rmd) ---
texto_rotulo_inset <- function(rotulo) {
  annotation_custom(
    grob = grid::textGrob(rotulo,
                            x = unit(0.04, "npc"), y = unit(0.05, "npc"),
                            hjust = 0, vjust = 0,
                            gp = grid::gpar(fontsize = 10, fontface = "bold",
                                            col = "gray20")),
    xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)
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
plot_rosa_ventos <- function() {
  ggplot() + geom_blank() +
    coord_fixed(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
    annotation_north_arrow(
      location = "tl", which_north = "true",
      pad_x = unit(0, "cm"), pad_y = unit(0, "cm"),
      height = unit(2.0, "cm"), width = unit(2.0, "cm"),
      style = north_arrow_fancy_orienteering(
        text_size = 11, fill = c("white", "gray15"),
        line_col = "gray15", text_col = "gray15")) +
    theme_void() +
    theme(plot.background  = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA))
}
montar_figura_com_insets <- function(mapa_principal) {
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
    legend.margin = margin(6, 10, 6, 10))
  legenda <- cowplot::get_legend(p_com_leg)
  mapa_clean <- mapa_principal + theme(legend.position = "none")
  cowplot::ggdraw() +
    cowplot::draw_grob(grid::rectGrob(gp = grid::gpar(fill = "white", col = NA))) +
    cowplot::draw_plot(mapa_clean,     x = 0.00, y = 0.00, width = 0.76, height = 1.00) +
    cowplot::draw_plot(legenda,        x = 0.01, y = 0.80, width = 0.40, height = 0.18) +
    cowplot::draw_plot(plot_rosa_ventos(),
                                       x = 0.81, y = 0.78, width = 0.16, height = 0.20) +
    cowplot::draw_plot(inset_brasil(), x = 0.76, y = 0.42, width = 0.23, height = 0.38) +
    cowplot::draw_plot(inset_mg(),     x = 0.76, y = 0.06, width = 0.23, height = 0.38)
}

# === FIGURA 1 — Incidência (EB) ============================================
cents <- st_centroid(mapa); cd <- st_drop_geometry(mapa)
cd$lon <- st_coordinates(cents)[, 1]; cd$lat <- st_coordinates(cents)[, 2]
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
                   text_cex = 0.8, line_width = 0.6, height = unit(0.2, "cm"),
                   pad_x = unit(0.4, "cm"), pad_y = unit(0.4, "cm"),
                   bar_cols = c("gray15","white")) +
  coord_sf(xlim = c(bb["xmin"]-buf, bb["xmax"]+buf),
           ylim = c(bb["ymin"]-buf, bb["ymax"]+buf), expand = FALSE) +
  theme_void(base_size = 10)

fig1 <- montar_figura_com_insets(p_inc)

# === FIGURA 2 — LISA ========================================================
# Recomputar quadrantes (mesmo critério do 03_espacial_lisa.Rmd)
lisa <- localmoran(mapa$taxa_eb, pesos, zero.policy = TRUE)
mapa$lp_fdr <- p.adjust(lisa[,5], method = "BH")
mt   <- mean(mapa$taxa_eb, na.rm = TRUE)
lag_t <- lag.listw(pesos, mapa$taxa_eb, zero.policy = TRUE)
mapa$quadrante <- case_when(
  mapa$taxa_eb >= mt & lag_t >= mt & mapa$lp_fdr < 0.05 ~ "Alto-Alto",
  mapa$taxa_eb <  mt & lag_t <  mt & mapa$lp_fdr < 0.05 ~ "Baixo-Baixo",
  mapa$taxa_eb >= mt & lag_t <  mt & mapa$lp_fdr < 0.05 ~ "Alto-Baixo",
  mapa$taxa_eb <  mt & lag_t >= mt & mapa$lp_fdr < 0.05 ~ "Baixo-Alto",
  TRUE ~ "Não significativo")
cd$quadrante <- mapa$quadrante
sig <- cd |> filter(quadrante != "Não significativo")

cores_lisa <- c("Alto-Alto"="#d7191c","Baixo-Baixo"="#2c7bb6",
                "Alto-Baixo"="#fdae61","Baixo-Alto"="#abd9e9",
                "Não significativo"="#f0f0f0")

p_lisa <- ggplot(mapa) +
  geom_sf(aes(fill = quadrante), color = "gray40", linewidth = 0.3) +
  scale_fill_manual(values = cores_lisa, name = "Cluster LISA") +
  geom_label_repel(data = sig, aes(x = lon, y = lat, label = municipio_label),
                   size = 3.4, fontface = "bold", box.padding = 0.7,
                   label.size = 0.25, label.padding = unit(0.18, "lines"),
                   arrow = arrow(length = unit(0.018,"npc"), type = "closed"),
                   min.segment.length = 0, max.overlaps = 20) +
  annotation_scale(location = "bl", width_hint = 0.22, style = "bar",
                   text_cex = 0.8, line_width = 0.6, height = unit(0.2, "cm"),
                   pad_x = unit(0.4, "cm"), pad_y = unit(0.4, "cm"),
                   bar_cols = c("gray15","white")) +
  coord_sf(xlim = c(bb["xmin"]-buf, bb["xmax"]+buf),
           ylim = c(bb["ymin"]-buf, bb["ymax"]+buf), expand = FALSE) +
  theme_void(base_size = 10)

fig2 <- montar_figura_com_insets(p_lisa)

# === Exportar em 300 dpi ====================================================
# Dimensões: 10 × 6.5 polegadas → 3000 × 1950 px em 300 dpi
salvar <- function(fig, prefixo) {
  ggsave(filename = file.path("figs", paste0(prefixo, ".png")),
         plot = fig, width = 10, height = 6.5, dpi = 300, units = "in",
         device = "png", bg = "white")
  ggsave(filename = file.path("figs", paste0(prefixo, ".tiff")),
         plot = fig, width = 10, height = 6.5, dpi = 300, units = "in",
         device = "tiff", bg = "white",
         compression = "lzw")
  tryCatch(
    ggsave(filename = file.path("figs", paste0(prefixo, ".pdf")),
           plot = fig, width = 10, height = 6.5, units = "in",
           device = "pdf", bg = "white"),
    error = function(e) message("PDF não gerado para ", prefixo, ": ", e$message))
  message("Salvo: figs/", prefixo, ".(png|tiff|pdf)")
}

salvar(fig1, "Figura1_incidencia")
salvar(fig2, "Figura2_LISA")

message("\nOK — figuras em figs/ prontas para submissão (300 dpi).")
