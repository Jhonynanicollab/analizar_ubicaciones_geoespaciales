

library(data.table)
library(sf)
library(ggplot2)
library(ggspatial)   


final <- fread(
  "C:/Users/crist/Downloads/final.csv",
  colClasses = "character"
)


setDT(final)

final[, DEPARTAMENTO := toupper(trimws(NOMBREDD))]
final <- final[!is.na(DEPARTAMENTO) & DEPARTAMENTO != ""]

final[, P1203_SUP_1 := as.numeric(ifelse(P1203_SUP_1 == "" | is.na(P1203_SUP_1), NA, P1203_SUP_1))]
final[, P1203_SUP_2 := as.numeric(ifelse(P1203_SUP_2 == "" | is.na(P1203_SUP_2), NA, P1203_SUP_2))]

final[, superficie := P1203_SUP_1 + (P1203_SUP_2 / 100)]

final[superficie < 0 | superficie > 100000, superficie := NA]

final[P1207_TIPO == "" | is.na(P1207_TIPO), P1207_TIPO := NA]
final[P1208      == "" | is.na(P1208),      P1208      := NA]

final[, P1208_LABEL := fcase(
  P1208 == "1", "Propio",
  P1208 == "2", "De la organización",
  P1208 == "3", "Alquilado",
  P1208 == "4", "Alquilado Dir. Reg. Agric.",
  P1208 == "5", "Cedido",
  P1208 == "6", "Otro",
  default = NA_character_
)]

final[, P1207_LABEL := fcase(
  P1207_TIPO == "1", "Maquinaria",
  P1207_TIPO == "2", "Equipo",
  default = NA_character_
)]


clean_depto <- function(x) {
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
  x <- toupper(trimws(x))
  x <- gsub("[^A-Z ]", "", x)
  x <- trimws(x)
  return(x)
}

final[, DEPARTAMENTO := clean_depto(DEPARTAMENTO)]


resumen_sup <- final[!is.na(superficie), .(
  promedio_superficie = mean(superficie, na.rm = TRUE),
  total_registros     = .N
), by = DEPARTAMENTO]

maquina_top <- final[!is.na(P1207_LABEL), .N, by = .(DEPARTAMENTO, P1207_LABEL)]
maquina_top <- maquina_top[order(-N), .SD[1], by = DEPARTAMENTO]

prop_top <- final[!is.na(P1208_LABEL), .N, by = .(DEPARTAMENTO, P1208_LABEL)]
prop_top  <- prop_top[order(-N), .SD[1], by = DEPARTAMENTO]

cat("Departamentos en resumen:", sort(unique(resumen_sup$DEPARTAMENTO)), "\n")


suppressMessages(
  mapa <- st_read("C:/Users/crist/Downloads/Departamental INEI 2023 geogpsperu SuyoPomalia/Departamental INEI 2023 geogpsperu SuyoPomalia.shp")
)

mapa$DEPARTAMENTO <- clean_depto(mapa$DEPARTAMEN)

cat("En resumen pero NO en mapa:\n"); print(setdiff(resumen_sup$DEPARTAMENTO, mapa$DEPARTAMENTO))
cat("En mapa pero NO en resumen:\n"); print(setdiff(mapa$DEPARTAMENTO, resumen_sup$DEPARTAMENTO))

centroides    <- st_centroid(st_geometry(mapa))
centroides_df <- data.frame(
  DEPARTAMENTO = mapa$DEPARTAMENTO,
  X = st_coordinates(centroides)[, 1],
  Y = st_coordinates(centroides)[, 2]
)


mapa_sup     <- merge(mapa, resumen_sup, by = "DEPARTAMENTO", all.x = TRUE)
mapa_maquina <- merge(mapa, maquina_top, by = "DEPARTAMENTO", all.x = TRUE)
mapa_prop    <- merge(mapa, prop_top,    by = "DEPARTAMENTO", all.x = TRUE)

cent_sup     <- merge(centroides_df, as.data.frame(mapa_sup)[,     c("DEPARTAMENTO","promedio_superficie")], by = "DEPARTAMENTO")
cent_maquina <- merge(centroides_df, as.data.frame(mapa_maquina)[, c("DEPARTAMENTO","P1207_LABEL")],         by = "DEPARTAMENTO")
cent_prop    <- merge(centroides_df, as.data.frame(mapa_prop)[,    c("DEPARTAMENTO","P1208_LABEL")],          by = "DEPARTAMENTO")



tema_mapa <- theme(
  plot.title      = element_text(size = 13, face = "bold",  hjust = 0.5,
                                 margin = margin(b = 5)),
  plot.subtitle   = element_text(size = 9,  hjust = 0.5, color = "grey40",
                                 margin = margin(b = 8)),
  plot.caption    = element_text(size = 7,  color = "grey50", hjust = 1),
  
  legend.position  = "right",
  legend.title     = element_text(size = 8, face = "bold"),
  legend.text      = element_text(size = 7),
  legend.key.size  = unit(0.45, "cm"),
  legend.margin    = margin(0, 0, 0, 4),
  
  panel.background = element_rect(fill = "#D6EAF8", color = NA),
  plot.background  = element_rect(fill = "white",   color = "grey80", linewidth = 0.4),
  panel.grid       = element_blank(),
  axis.text        = element_blank(),
  axis.ticks       = element_blank(),
  
  plot.margin      = margin(t = 12, r = 12, b = 12, l = 12)
)


ggplot(mapa_sup) +
  geom_sf(aes(fill = promedio_superficie), color = "white", linewidth = 0.35) +
  
  geom_text(data = cent_sup,
            aes(x = X, y = Y,
                label = paste0(DEPARTAMENTO, "\n", round(promedio_superficie, 1))),
            size = 1.7, color = "black", fontface = "bold", lineheight = 0.85) +
  
  scale_fill_viridis_c(option = "C", na.value = "grey80",
                       name = "Superficie\npromedio (ha)") +
  
  annotation_north_arrow(location = "bl", which_north = "true",
                         style = north_arrow_fancy_orienteering(),
                         height = unit(1, "cm"), width = unit(1, "cm")) +
  annotation_scale(location = "br", width_hint = 0.25) +
  
coord_sf(expand = FALSE) +
  
  labs(
    title    = "Promedio de Superficie Sembrada por Departamento",
    subtitle = "Censo Agropecuario — Perú",
    caption  = "Fuente: Censo Nacional Agropecuario  |  Shapefile: INEI 2023"
  ) +
  tema_mapa

ggsave("mapa_superficie.png", width = 8, height = 9, dpi = 200, bg = "white")
cat("Guardado: mapa_superficie.png\n")

ggplot(mapa_maquina) +
  geom_sf(aes(fill = P1207_LABEL), color = "white", linewidth = 0.35) +
  
  geom_text(data = cent_maquina,
            aes(x = X, y = Y, label = DEPARTAMENTO),
            size = 1.7, color = "black", fontface = "bold") +
  
  scale_fill_manual(
    values   = c("Maquinaria" = "#2ecc71", "Equipo" = "#3498db"),
    na.value = "grey80",
    name     = "Tipo de\nMaquinaria"
  ) +
  
  annotation_north_arrow(location = "bl", which_north = "true",
                         style = north_arrow_fancy_orienteering(),
                         height = unit(1, "cm"), width = unit(1, "cm")) +
  annotation_scale(location = "br", width_hint = 0.25) +
  
  coord_sf(expand = FALSE) +
  
  labs(
    title    = "Tipo de Maquinaria Predominante por Departamento",
    subtitle = "Censo Agropecuario — Perú",
    caption  = "Fuente: Censo Nacional Agropecuario  |  Shapefile: INEI 2023"
  ) +
  tema_mapa

ggsave("mapa_maquinaria.png", width = 8, height = 9, dpi = 200, bg = "white")
cat("Guardado: mapa_maquinaria.png\n")


ggplot(mapa_prop) +
  geom_sf(aes(fill = P1208_LABEL), color = "white", linewidth = 0.35) +
  
  geom_text(data = cent_prop,
            aes(x = X, y = Y, label = DEPARTAMENTO),
            size = 1.7, color = "black", fontface = "bold") +
  
  scale_fill_manual(
    values = c(
      "Propio"                    = "#e74c3c",
      "De la organización"        = "#9b59b6",
      "Alquilado"                 = "#f39c12",
      "Alquilado Dir. Reg. Agric."= "#1abc9c",
      "Cedido"                    = "#34495e",
      "Otro"                      = "#95a5a6"
    ),
    na.value = "grey80",
    name     = "Tipo de\nPropiedad"
  ) +
  
  annotation_north_arrow(location = "bl", which_north = "true",
                         style = north_arrow_fancy_orienteering(),
                         height = unit(1, "cm"), width = unit(1, "cm")) +
  annotation_scale(location = "br", width_hint = 0.25) +
  
  coord_sf(expand = FALSE) +
  
  labs(
    title    = "Tipo de Propiedad Predominante por Departamento",
    subtitle = "Censo Agropecuario — Perú",
    caption  = "Fuente: Censo Nacional Agropecuario  |  Shapefile: INEI 2023"
  ) +
  tema_mapa

ggsave("mapa_propiedad.png", width = 8, height = 9, dpi = 200, bg = "white")
cat("Guardado: mapa_propiedad.png\n")


ggplot(resumen_sup, aes(x = reorder(DEPARTAMENTO, promedio_superficie),
                        y = promedio_superficie,
                        fill = promedio_superficie)) +
  geom_col(show.legend = FALSE) +
  
  geom_text(aes(label = round(promedio_superficie, 1)),
            hjust = -0.1, size = 2.5, color = "grey20") +
  
  scale_fill_viridis_c(option = "C") +
  
  coord_flip(clip = "off") +
  expand_limits(y = max(resumen_sup$promedio_superficie, na.rm = TRUE) * 1.15) +
  
  theme_minimal() +
  theme(
    plot.title   = element_text(size = 12, face = "bold", hjust = 0.5),
    axis.text.y  = element_text(size = 7),
    plot.margin  = margin(10, 25, 10, 10),
    plot.background = element_rect(fill = "white", color = "grey80", linewidth = 0.4)
  ) +
  labs(
    title   = "Ranking: Superficie Sembrada Promedio por Departamento",
    x       = NULL,
    y       = "Superficie promedio (ha)",
    caption = "Fuente: Censo Nacional Agropecuario"
  )

ggsave("grafico_ranking.png", width = 8, height = 7, dpi = 200, bg = "white")
cat("Guardado: grafico_ranking.png\n")
