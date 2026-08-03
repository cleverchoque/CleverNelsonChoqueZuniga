#===============================================================================
# ANÁLISIS DE VARIABILIDAD ESPACIAL DE ÍNDICES DE VEGETACIÓN (NDVI, SAVI, EVI, NDMI)
# MEDIANTE KRIGING ORDINARIO Y KRIGING INDICATIVO
# Provincias del departamento de Puno, Perú
#
# Metodología basada en: Lourenço, R.W. & Landim, P.M.B. - "Estudo da
# variabilidade do NDVI utilizando krigagem indicativa" (UNESP)
#
# Curso: Estadística Espacial - UNAP FINESI
#===============================================================================

# =============================================================================
# 0. CONFIGURACIÓN GLOBAL (ajustar solo esta sección al reutilizar el script)
# =============================================================================

# Carpeta donde están los CSV exportados de Google Earth Engine y el shapefile
ruta_datos <- "D:/UNA/A/10/estadistica espacial/final/Puno_shapefile/"

# Carpeta donde se guardarán todas las figuras generadas
ruta_salida <- "C:/Users/robin/OneDrive/Documentos/"

archivo_csv_seca   <- paste0(ruta_datos, "muestras_indices_puno_seca.csv")
archivo_csv_humeda <- paste0(ruta_datos, "muestras_indices_puno_humeda.csv")
archivo_shapefile  <- paste0(ruta_datos, "provincias_puno.gpkg")

# Sistema de coordenadas proyectado para Puno (UTM zona 19S, en metros)
crs_utm <- 32719

# Resolución de la grilla de predicción (metros)
resolucion_grilla <- 2000

# Semilla para reproducibilidad de gráficos con ggrepel
semilla <- 42


# =============================================================================
# 1. LIBRERÍAS
# =============================================================================

paquetes <- c("sf", "sp", "gstat", "dplyr", "tidyr", "jsonlite",
              "ggplot2", "patchwork", "viridis", "ggspatial", "ggrepel")

instalar_faltantes <- paquetes[!(paquetes %in% installed.packages()[, "Package"])]
if (length(instalar_faltantes) > 0) install.packages(instalar_faltantes)

invisible(lapply(paquetes, library, character.only = TRUE))


# =============================================================================
# 2. CARGA DE DATOS
# =============================================================================

seca   <- read.csv(archivo_csv_seca)
humeda <- read.csv(archivo_csv_humeda)
puno   <- st_read(archivo_shapefile, quiet = TRUE)

cat("Puntos exportados -> Seca:", nrow(seca), "| Húmeda:", nrow(humeda), "\n")


# =============================================================================
# 3. LIMPIEZA DE DATOS
# =============================================================================

# 3.1 Extraer coordenadas del campo .geo (formato GeoJSON embebido por GEE)
extraer_coords <- function(df) {
  coords <- df$.geo %>%
    lapply(function(x) fromJSON(x)$coordinates) %>%
    do.call(rbind, .)
  df$lon  <- coords[, 1]
  df$lat  <- coords[, 2]
  df$.geo <- NULL
  df
}

seca   <- extraer_coords(seca)
humeda <- extraer_coords(humeda)

# 3.2 Eliminar NA y outliers fuera de rango físico posible
# (EVI en particular puede dispararse a valores absurdos cuando el
#  denominador de la fórmula se acerca a cero: agua, nieve, nubes residuales)
limpiar_indices <- function(df) {
  df %>%
    filter(
      !is.na(NDVI), !is.na(SAVI), !is.na(EVI), !is.na(NDMI),
      NDVI >= -1,   NDVI <= 1,
      SAVI >= -1.5, SAVI <= 1.5,
      EVI  >= -1,   EVI  <= 1
    )
}

seca   <- limpiar_indices(seca)
humeda <- limpiar_indices(humeda)

cat("Puntos válidos tras limpieza -> Seca:", nrow(seca),
    "| Húmeda:", nrow(humeda), "\n")


# =============================================================================
# 4. OBJETOS ESPACIALES (sf + sp, proyectados a UTM)
# =============================================================================

crear_objeto_espacial <- function(df) {
  df_sf  <- st_as_sf(df, coords = c("lon", "lat"), crs = 4326)
  df_utm <- st_transform(df_sf, crs = crs_utm)
  as(df_utm, "Spatial")
}

seca_sp   <- crear_objeto_espacial(seca)
humeda_sp <- crear_objeto_espacial(humeda)

puno_utm <- st_transform(puno, crs = crs_utm)

# Centroides de provincias, usados para las etiquetas en los mapas
puno_centroides <- st_centroid(puno_utm)
puno_centroides_coords <- cbind(
  st_drop_geometry(puno_centroides),
  st_coordinates(puno_centroides)
)


# =============================================================================
# 5. ANÁLISIS EXPLORATORIO
# =============================================================================

summary(seca[, c("NDVI", "SAVI", "EVI", "NDMI")])
summary(humeda[, c("NDVI", "SAVI", "EVI", "NDMI")])

# 5.1 Histogramas por índice (época seca)
graf_hist <- function(data, var, color, titulo) {
  ggplot(data, aes(x = .data[[var]])) +
    geom_histogram(aes(y = after_stat(density)), bins = 30,
                    fill = color, color = "white", alpha = 0.85, linewidth = 0.2) +
    geom_density(color = "grey20", linewidth = 0.8) +
    labs(title = titulo, x = var, y = "Densidad") +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold", size = 13))
}

colores_indices <- c(NDVI = "#2E8B57", SAVI = "#D2691E",
                     EVI = "#228B22", NDMI = "#4682B4")

panel_histogramas <- wrap_plots(
  lapply(names(colores_indices), function(v)
    graf_hist(seca, v, colores_indices[v], paste0(v, " — Época Seca"))),
  ncol = 2
) + plot_annotation(
  title = "Distribución de índices de vegetación — Puno (Época Seca)"
)

ggsave(paste0(ruta_salida, "histogramas_seca.png"), panel_histogramas,
       width = 11, height = 8, dpi = 300, bg = "white")

# 5.2 Boxplot comparativo Seca vs. Húmeda
seca$epoca   <- "Seca"
humeda$epoca <- "Húmeda"

comparativo_long <- bind_rows(
  seca   %>% select(NDVI, SAVI, EVI, NDMI, epoca),
  humeda %>% select(NDVI, SAVI, EVI, NDMI, epoca)
) %>%
  pivot_longer(cols = c(NDVI, SAVI, EVI, NDMI), names_to = "indice", values_to = "valor")

boxplot_comparativo <- ggplot(comparativo_long, aes(x = indice, y = valor, fill = epoca)) +
  geom_boxplot(alpha = 0.85, outlier.size = 0.8, outlier.alpha = 0.4, width = 0.6) +
  scale_fill_manual(values = c(Seca = "#D2691E", Húmeda = "#2E8B57"), name = "Época") +
  labs(title = "Comparación estacional de índices de vegetación",
       subtitle = "Provincias del departamento de Puno", x = NULL, y = "Valor del índice") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", size = 15), legend.position = "top")

ggsave(paste0(ruta_salida, "boxplot_comparativo.png"), boxplot_comparativo,
       width = 10, height = 7, dpi = 300, bg = "white")


# =============================================================================
# 6. SEMIVARIOGRAMAS EXPERIMENTALES (ajuste automático del mejor modelo)
# =============================================================================

# Prueba Esférico, Exponencial y Gaussiano; selecciona el de menor SSErr
ajustar_variograma <- function(formula, datos_sp) {
  vgm_exp <- variogram(formula, datos_sp)

  modelos <- list(
    Sph = tryCatch(fit.variogram(vgm_exp, vgm("Sph")), error = function(e) NULL),
    Exp = tryCatch(fit.variogram(vgm_exp, vgm("Exp")), error = function(e) NULL),
    Gau = tryCatch(fit.variogram(vgm_exp, vgm("Gau")), error = function(e) NULL)
  )

  errores <- sapply(modelos, function(m) if (!is.null(m)) attr(m, "SSErr") else Inf)
  mejor   <- modelos[[which.min(errores)]]

  list(experimental = vgm_exp, modelo = mejor, nombre_modelo = names(errores)[which.min(errores)])
}

graf_variograma <- function(resultado, titulo, color) {
  linea <- variogramLine(resultado$modelo, maxdist = max(resultado$experimental$dist))

  ggplot() +
    geom_point(data = resultado$experimental, aes(x = dist, y = gamma),
               color = color, size = 2.8, alpha = 0.85) +
    geom_line(data = linea, aes(x = dist, y = gamma), color = "grey20", linewidth = 0.9) +
    labs(
      title = titulo,
      subtitle = paste0("Modelo: ", resultado$nombre_modelo,
                         " | Nugget: ", round(resultado$modelo$psill[1], 4),
                         " | Sill: ", round(sum(resultado$modelo$psill), 4),
                         " | Rango: ", round(resultado$modelo$range[2] / 1000, 1), " km"),
      x = "Distancia (m)", y = "Semivarianza"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(color = "grey40", size = 9.5),
      panel.grid.minor = element_blank()
    )
}

indices <- c("NDVI", "SAVI", "EVI", "NDMI")

# Ajustar variogramas para los 4 índices, en ambas épocas
resultados_vgm_seca   <- setNames(
  lapply(indices, function(v) ajustar_variograma(reformulate("1", v), seca_sp)), indices)
resultados_vgm_humeda <- setNames(
  lapply(indices, function(v) ajustar_variograma(reformulate("1", v), humeda_sp)), indices)

# Graficar paneles 2x2 por época
graficos_seca   <- Map(graf_variograma, resultados_vgm_seca,   indices, colores_indices)
graficos_humeda <- Map(graf_variograma, resultados_vgm_humeda, indices, colores_indices)

panel_vgm_seca <- wrap_plots(graficos_seca, ncol = 2) +
  plot_annotation(title = "Semivariogramas experimentales — Época Seca — Provincias de Puno")

panel_vgm_humeda <- wrap_plots(graficos_humeda, ncol = 2) +
  plot_annotation(title = "Semivariogramas experimentales — Época Húmeda — Provincias de Puno")

ggsave(paste0(ruta_salida, "semivariogramas_seca.png"), panel_vgm_seca,
       width = 11, height = 8, dpi = 300, bg = "white")
ggsave(paste0(ruta_salida, "semivariogramas_humeda.png"), panel_vgm_humeda,
       width = 11, height = 8, dpi = 300, bg = "white")

# Tabla resumen de modelos seleccionados (para el reporte)
tabla_modelos <- data.frame(
  Indice = rep(indices, 2),
  Epoca  = c(rep("Seca", 4), rep("Húmeda", 4)),
  Modelo = c(sapply(resultados_vgm_seca, `[[`, "nombre_modelo"),
             sapply(resultados_vgm_humeda, `[[`, "nombre_modelo")),
  Rango_km = round(c(sapply(resultados_vgm_seca, function(r) r$modelo$range[2]),
                      sapply(resultados_vgm_humeda, function(r) r$modelo$range[2])) / 1000, 1)
)
print(tabla_modelos)


# =============================================================================
# 7. GRILLA DE PREDICCIÓN (recortada al polígono de Puno)
# =============================================================================

bbox_puno <- st_bbox(puno_utm)

grilla_pred <- expand.grid(
  x = seq(bbox_puno["xmin"], bbox_puno["xmax"], by = resolucion_grilla),
  y = seq(bbox_puno["ymin"], bbox_puno["ymax"], by = resolucion_grilla)
)
coordinates(grilla_pred) <- ~x + y
gridded(grilla_pred)     <- TRUE
proj4string(grilla_pred) <- CRS(paste0("+init=epsg:", crs_utm))

puno_union  <- st_union(puno_utm)
dentro      <- st_intersects(st_as_sf(grilla_pred), puno_union, sparse = FALSE)[, 1]
grilla_pred <- grilla_pred[dentro, ]

cat("Puntos de la grilla dentro de Puno:", length(grilla_pred), "\n")


# =============================================================================
# 8. KRIGING ORDINARIO (mapas continuos por índice y época)
# =============================================================================

hacer_kriging <- function(formula, datos_sp, modelo, grilla) {
  resultado <- krige(formula, datos_sp, grilla, model = modelo)
  df <- as.data.frame(resultado)
  colnames(df)[1:2] <- c("x", "y")
  df
}

kriging_seca <- setNames(
  lapply(indices, function(v)
    hacer_kriging(reformulate("1", v), seca_sp, resultados_vgm_seca[[v]]$modelo, grilla_pred)),
  indices)

kriging_humeda <- setNames(
  lapply(indices, function(v)
    hacer_kriging(reformulate("1", v), humeda_sp, resultados_vgm_humeda[[v]]$modelo, grilla_pred)),
  indices)

mapa_kriging <- function(datos_krige, titulo, subtitulo, paleta = "viridis") {
  ggplot() +
    geom_raster(data = datos_krige, aes(x = x, y = y, fill = var1.pred)) +
    geom_sf(data = puno_utm, fill = NA, color = "white", linewidth = 0.4, inherit.aes = FALSE) +
    geom_text_repel(
      data = puno_centroides_coords, aes(x = X, y = Y, label = PROVINCIA),
      size = 2.8, color = "white", fontface = "bold",
      bg.color = "black", bg.r = 0.12, max.overlaps = 20, seed = semilla
    ) +
    scale_fill_viridis_c(option = paleta, name = titulo) +
    annotation_scale(location = "bl", width_hint = 0.25, text_col = "white", line_col = "white") +
    annotation_north_arrow(location = "tr", which_north = "true",
                            style = north_arrow_fancy_orienteering(fill = c("white", "white"), line_col = "white")) +
    labs(title = subtitulo, x = NULL, y = NULL) +
    coord_sf(expand = FALSE) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
      panel.background = element_rect(fill = "grey10", color = NA),
      panel.grid = element_blank(),
      axis.text = element_text(size = 7, color = "grey40"),
      legend.title = element_text(size = 9, face = "bold")
    )
}

paletas <- c(NDVI = "viridis", SAVI = "mako", EVI = "viridis", NDMI = "mako")

mapas_seca <- Map(function(v) mapa_kriging(kriging_seca[[v]], v, paste0(v, " — Época Seca"), paletas[v]),
                   indices)
mapas_humeda <- Map(function(v) mapa_kriging(kriging_humeda[[v]], v, paste0(v, " — Época Húmeda"), paletas[v]),
                     indices)
names(mapas_seca) <- indices
names(mapas_humeda) <- indices

# Guardar mapas individuales
for (v in indices) {
  ggsave(paste0(ruta_salida, "mapa_", v, "_seca.png"), mapas_seca[[v]],
         width = 8, height = 7, dpi = 300, bg = "white")
  ggsave(paste0(ruta_salida, "mapa_", v, "_humeda.png"), mapas_humeda[[v]],
         width = 8, height = 7, dpi = 300, bg = "white")
}

# Guardar paneles combinados (2x2) por época
panel_kriging_seca <- wrap_plots(mapas_seca, ncol = 2) +
  plot_annotation(title = "Interpolación por Kriging — Época Seca — Provincias de Puno")
panel_kriging_humeda <- wrap_plots(mapas_humeda, ncol = 2) +
  plot_annotation(title = "Interpolación por Kriging — Época Húmeda — Provincias de Puno")

ggsave(paste0(ruta_salida, "kriging_epoca_seca.png"), panel_kriging_seca,
       width = 14, height = 12, dpi = 300, bg = "white")
ggsave(paste0(ruta_salida, "kriging_epoca_humeda.png"), panel_kriging_humeda,
       width = 14, height = 12, dpi = 300, bg = "white")


# =============================================================================
# 9. KRIGING INDICATIVO (clasificación Baja / Media / Alta por NDVI)
# =============================================================================

# Ajusta el semivariograma de un indicador binario. Si el rango sale
# absurdamente grande (> rango_max_km), se refuerza con un rango fijo
# razonable, ya que el optimizador puede converger a mínimos "planos".
ajustar_variograma_indicador <- function(datos_sp, indicador, rango_max_km = 200,
                                          rango_inicial = 100000) {
  vgm_exp <- variogram(reformulate("1", indicador), datos_sp)
  modelo  <- fit.variogram(vgm_exp, vgm("Sph"))

  if (modelo$range[2] / 1000 > rango_max_km || modelo$range[2] <= 0) {
    modelo <- fit.variogram(
      vgm_exp,
      vgm(psill = 0.15, model = "Sph", range = rango_inicial, nugget = 0.15),
      fit.ranges = FALSE
    )
  }
  list(experimental = vgm_exp, modelo = modelo)
}

# Ejecuta el flujo completo de kriging indicativo para una época:
# crea los 2 indicadores (terciles 33%/66%), krigea cada uno,
# combina probabilidades y asigna categoría final.
kriging_indicativo_ndvi <- function(datos_sp, grilla, rango_inicial = 100000) {
  cortes <- quantile(datos_sp$NDVI, probs = c(0.33, 0.66))

  datos_sp$ind1 <- ifelse(datos_sp$NDVI <= cortes[1], 1, 0)
  datos_sp$ind2 <- ifelse(datos_sp$NDVI <= cortes[2], 1, 0)

  vgm1 <- ajustar_variograma_indicador(datos_sp, "ind1", rango_inicial = rango_inicial)
  vgm2 <- ajustar_variograma_indicador(datos_sp, "ind2", rango_inicial = rango_inicial)

  k1 <- hacer_kriging(ind1 ~ 1, datos_sp, vgm1$modelo, grilla)
  k2 <- hacer_kriging(ind2 ~ 1, datos_sp, vgm2$modelo, grilla)

  k1$var1.pred <- pmin(pmax(k1$var1.pred, 0), 1)
  k2$var1.pred <- pmin(pmax(k2$var1.pred, 0), 1)

  clasificacion <- k1 %>%
    select(x, y, prob_bajo1 = var1.pred) %>%
    left_join(k2 %>% select(x, y, prob_bajo2 = var1.pred), by = c("x", "y")) %>%
    mutate(
      prob_baja  = prob_bajo1,
      prob_media = pmax(prob_bajo2 - prob_bajo1, 0),
      prob_alta  = 1 - prob_bajo2
    ) %>%
    rowwise() %>%
    mutate(categoria = c("Baja", "Media", "Alta")[which.max(c(prob_baja, prob_media, prob_alta))]) %>%
    ungroup() %>%
    mutate(categoria = factor(categoria, levels = c("Baja", "Media", "Alta")))

  list(clasificacion = clasificacion, cortes = cortes)
}

mapa_clasificacion <- function(clasificacion, cortes, subtitulo) {
  ggplot() +
    geom_raster(data = clasificacion, aes(x = x, y = y, fill = categoria)) +
    geom_sf(data = puno_utm, fill = NA, color = "white", linewidth = 0.4, inherit.aes = FALSE) +
    geom_text_repel(
      data = puno_centroides_coords, aes(x = X, y = Y, label = PROVINCIA),
      size = 3, color = "white", fontface = "bold",
      bg.color = "black", bg.r = 0.12, max.overlaps = 20, seed = semilla
    ) +
    scale_fill_manual(
      values = c(Baja = "#B22222", Media = "#DAA520", Alta = "#2E8B57"),
      name = "Categoría\nde vegetación\n(NDVI)"
    ) +
    annotation_scale(location = "bl", width_hint = 0.25, text_col = "white", line_col = "white") +
    annotation_north_arrow(location = "tr", which_north = "true",
                            style = north_arrow_fancy_orienteering(fill = c("white", "white"), line_col = "white")) +
    labs(
      title = "Clasificación espacial de vegetación por Kriging Indicativo",
      subtitle = subtitulo,
      caption = paste0("Cortes: Baja < ", round(cortes[1], 3),
                        " | Media: ", round(cortes[1], 3), "–", round(cortes[2], 3),
                        " | Alta ≥ ", round(cortes[2], 3)),
      x = NULL, y = NULL
    ) +
    coord_sf(expand = FALSE) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      plot.subtitle = element_text(size = 10, color = "grey30", hjust = 0.5),
      plot.caption = element_text(size = 8, color = "grey50"),
      panel.background = element_rect(fill = "grey10", color = NA),
      panel.grid = element_blank(),
      axis.text = element_text(size = 7, color = "grey40"),
      legend.title = element_text(size = 9, face = "bold")
    )
}

# Época seca: el corte 66% requiere rango inicial mayor (~120 km) para converger bien
resultado_ik_seca   <- kriging_indicativo_ndvi(seca_sp, grilla_pred, rango_inicial = 120000)
# Época húmeda: rango de corto alcance observado en los semivariogramas continuos (~50 km)
resultado_ik_humeda <- kriging_indicativo_ndvi(humeda_sp, grilla_pred, rango_inicial = 50000)

mapa_ik_seca <- mapa_clasificacion(
  resultado_ik_seca$clasificacion, resultado_ik_seca$cortes,
  "NDVI — Época Seca — Provincias de Puno, Perú"
)
mapa_ik_humeda <- mapa_clasificacion(
  resultado_ik_humeda$clasificacion, resultado_ik_humeda$cortes,
  "NDVI — Época Húmeda — Provincias de Puno, Perú"
)

ggsave(paste0(ruta_salida, "mapa_clasificacion_kriging_indicativo_seca.png"),
       mapa_ik_seca, width = 9, height = 8, dpi = 300, bg = "white")
ggsave(paste0(ruta_salida, "mapa_clasificacion_kriging_indicativo_humeda.png"),
       mapa_ik_humeda, width = 9, height = 8, dpi = 300, bg = "white")

# Tabla comparativa de proporciones por categoría (para discusión del artículo)
tabla_comparativa <- data.frame(
  Categoria = c("Baja", "Media", "Alta"),
  Seca_n    = as.integer(table(resultado_ik_seca$clasificacion$categoria)),
  Humeda_n  = as.integer(table(resultado_ik_humeda$clasificacion$categoria))
) %>%
  mutate(
    Seca_pct   = round(100 * Seca_n / sum(Seca_n), 1),
    Humeda_pct = round(100 * Humeda_n / sum(Humeda_n), 1)
  )

print(tabla_comparativa)


# =============================================================================
# FIN DEL SCRIPT
# Figuras generadas en: ruta_salida
#   - histogramas_seca.png
#   - boxplot_comparativo.png
#   - semivariogramas_seca.png / semivariogramas_humeda.png
#   - mapa_[INDICE]_seca.png / mapa_[INDICE]_humeda.png  (x4 cada uno)
#   - kriging_epoca_seca.png / kriging_epoca_humeda.png (paneles 2x2)
#   - mapa_clasificacion_kriging_indicativo_seca.png / _humeda.png
# =============================================================================
