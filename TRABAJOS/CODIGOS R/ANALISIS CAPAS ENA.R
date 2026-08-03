# =============================================================================
#  ANÁLISIS DE PISOS ALTITUDINALES CON METODOLOGÍA JENKS NATURAL BREAKS
#  Encuesta Nacional Agropecuaria (ENA) 2014 - 2024 - Perú
#  Metodología: Jenks-Fisher Natural Breaks (classInt)
# =============================================================================


# ─────────────────────────────────────────────────────────────────────────────
# 1. INSTALACIÓN Y CARGA DE PAQUETES
# ─────────────────────────────────────────────────────────────────────────────

paquetes <- c("readxl", "dplyr", "ggplot2", "classInt", "scales",
              "tidyr", "forcats", "ggridges", "patchwork", "viridis",
              "openxlsx")

instalar <- paquetes[!paquetes %in% installed.packages()[, "Package"]]
if (length(instalar) > 0) install.packages(instalar, dependencies = TRUE)

suppressMessages({
  library(readxl)
  library(dplyr)
  library(ggplot2)
  library(classInt)
  library(scales)
  library(tidyr)
  library(forcats)
  library(ggridges)
  library(patchwork)
  library(viridis)
  library(openxlsx)
})

cat("✔ Paquetes cargados correctamente\n")


# ─────────────────────────────────────────────────────────────────────────────
# 2. CARGA DE DATOS
# ─────────────────────────────────────────────────────────────────────────────



ruta_excel <- "C:/Users/TUF GAMING/Downloads/ENA_2014_2024_SIMULADA.xlsx"
View(ENA_2014_2024_SIMULADA)

df <- read_excel(ruta_excel, sheet = "ENA_DATA")

cat(sprintf("✔ Datos cargados: %d registros | %d variables\n", nrow(df), ncol(df)))
cat("  Variables:", paste(names(df), collapse = ", "), "\n\n")

stopifnot("ALTITUD_msnm" %in% names(df))
df <- df %>% filter(!is.na(ALTITUD_msnm), ALTITUD_msnm >= 0)


# ─────────────────────────────────────────────────────────────────────────────
# 3. METODOLOGÍA JENKS NATURAL BREAKS
# ─────────────────────────────────────────────────────────────────────────────
# Jenks minimiza la varianza DENTRO de cada clase y maximiza la varianza
# ENTRE clases → detecta quiebres naturales en la distribución de altitudes.

n_pisos <- 5   # ◄ Ajusta el número de pisos según tu análisis

set.seed(123)
jenks <- classIntervals(
  var        = df$ALTITUD_msnm,
  n          = n_pisos,
  style      = "jenks",
  warnSmallN = FALSE
)

quiebres <- jenks$brks
gvf      <- jenks.tests(jenks)   # Goodness of Variance Fit

cat("─────────────────────────────────────────────────────────\n")
cat("  QUIEBRES JENKS NATURAL BREAKS (msnm):\n")
cat(sprintf("  %s\n", paste(round(quiebres), collapse = " → ")))
cat(sprintf("  GVF = %.4f  (>0.80 = excelente clasificación)\n", gvf[[2]]))
cat("─────────────────────────────────────────────────────────\n\n")

# Etiquetas según pisos ecológicos peruanos (Pulgar Vidal)
etiquetas_pisos <- c(
  "P1: Costa / Chala",
  "P2: Yunga",
  "P3: Quechua",
  "P4: Suni / Puna",
  "P5: Janca / Cordillera"
)

# Asignar piso a cada fila
df <- df %>%
  mutate(
    PISO_NUM = cut(
      ALTITUD_msnm,
      breaks         = quiebres,
      labels         = etiquetas_pisos,
      include.lowest = TRUE,
      right          = TRUE
    ),
    RANGO_ALTITUD = cut(
      ALTITUD_msnm,
      breaks         = quiebres,
      labels         = paste0(
        round(quiebres[-length(quiebres)]), " - ",
        round(quiebres[-1]), " msnm"
      ),
      include.lowest = TRUE,
      right          = TRUE
    )
  )

cat(sprintf("✔ Clasificación completada: %d registros asignados a pisos\n\n",
            sum(!is.na(df$PISO_NUM))))


# ─────────────────────────────────────────────────────────────────────────────
# 4. RESUMEN ESTADÍSTICO POR PISO
# ─────────────────────────────────────────────────────────────────────────────

resumen_pisos <- df %>%
  filter(!is.na(PISO_NUM)) %>%
  group_by(PISO_NUM, RANGO_ALTITUD) %>%
  summarise(
    N_Registros       = n(),
    Altitud_Min       = min(ALTITUD_msnm, na.rm = TRUE),
    Altitud_Max       = max(ALTITUD_msnm, na.rm = TRUE),
    Altitud_Media     = round(mean(ALTITUD_msnm, na.rm = TRUE), 1),
    Rendimiento_Media = round(mean(RENDIMIENTO_kg_ha, na.rm = TRUE), 1),
    Rendimiento_SD    = round(sd(RENDIMIENTO_kg_ha, na.rm = TRUE), 1),
    Produccion_Total  = round(sum(PRODUCCION_t, na.rm = TRUE), 2),
    Superficie_Total  = round(sum(SUPERFICIE_ha, na.rm = TRUE), 2),
    Temp_Media        = round(mean(TEMPERATURA_C, na.rm = TRUE), 1),
    Precip_Media      = round(mean(PRECIPITACION_mm, na.rm = TRUE), 1),
    .groups = "drop"
  )

cat("═══════════════════════════════════════════════════════════\n")
cat("  RESUMEN POR PISO ALTITUDINAL\n")
cat("═══════════════════════════════════════════════════════════\n")
print(as.data.frame(resumen_pisos), row.names = FALSE)
cat("\n")


# ─────────────────────────────────────────────────────────────────────────────
# 5. DIFERENCIAS ENTRE PISOS ALTITUDINALES ← OBJETIVO PRINCIPAL
# ─────────────────────────────────────────────────────────────────────────────

diferencias <- resumen_pisos %>%
  arrange(PISO_NUM) %>%
  mutate(
    Dif_Rend_abs   = round(Rendimiento_Media - lag(Rendimiento_Media), 1),
    Dif_Rend_pct   = round((Rendimiento_Media - lag(Rendimiento_Media)) /
                             lag(Rendimiento_Media) * 100, 2),
    Dif_Altitud    = round(Altitud_Media    - lag(Altitud_Media), 1),
    Dif_Temp       = round(Temp_Media       - lag(Temp_Media), 2),
    Dif_Precip     = round(Precip_Media     - lag(Precip_Media), 1),
    Dif_Produccion = round(Produccion_Total - lag(Produccion_Total), 2)
  )

cat("╔══════════════════════════════════════════════════════════════════╗\n")
cat("║      DIFERENCIAS ENTRE PISOS ALTITUDINALES (Jenks NB)           ║\n")
cat("╚══════════════════════════════════════════════════════════════════╝\n\n")

tabla_dif <- diferencias %>%
  select(
    Piso               = PISO_NUM,
    `Rango msnm`       = RANGO_ALTITUD,
    `Rend. kg/ha`      = Rendimiento_Media,
    `Δ Rend. abs.`     = Dif_Rend_abs,
    `Δ Rend. %`        = Dif_Rend_pct,
    `Δ Temp. °C`       = Dif_Temp,
    `Δ Precip. mm`     = Dif_Precip,
    `Δ Prod. (t)`      = Dif_Produccion
  )

print(as.data.frame(tabla_dif), row.names = FALSE)

cat("\n  Nota: Δ = diferencia respecto al piso anterior (NA en el primero)\n")
cat("  Δ negativo en Rendimiento → menor productividad a mayor altitud\n")
cat("  Δ negativo en Temp. → enfriamiento progresivo por altitud\n\n")


# ─────────────────────────────────────────────────────────────────────────────
# 6. ANÁLISIS POR CULTIVO Y PISO
# ─────────────────────────────────────────────────────────────────────────────

cultivo_piso <- df %>%
  filter(!is.na(PISO_NUM)) %>%
  group_by(PISO_NUM, CULTIVO) %>%
  summarise(
    N           = n(),
    Rendimiento = round(mean(RENDIMIENTO_kg_ha, na.rm = TRUE), 1),
    Produccion  = round(sum(PRODUCCION_t, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  arrange(PISO_NUM, desc(N))

top_cultivo <- cultivo_piso %>%
  group_by(PISO_NUM) %>%
  slice_max(N, n = 1) %>%
  ungroup()

cat("─────────────────────────────────────────────────────────\n")
cat("  CULTIVO PREDOMINANTE POR PISO ALTITUDINAL\n")
cat("─────────────────────────────────────────────────────────\n")
print(as.data.frame(top_cultivo), row.names = FALSE)
cat("\n")


# ─────────────────────────────────────────────────────────────────────────────
# 7. VISUALIZACIONES
# ─────────────────────────────────────────────────────────────────────────────

colores_pisos <- c(
  "P1: Costa / Chala"       = "#E8C07D",
  "P2: Yunga"               = "#6DBF67",
  "P3: Quechua"             = "#2E8B57",
  "P4: Suni / Puna"         = "#4682B4",
  "P5: Janca / Cordillera"  = "#7B68EE"
)

tema_ena <- theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 13, color = "#1F4E79"),
    plot.subtitle    = element_text(size = 10, color = "gray40"),
    plot.caption     = element_text(size = 8, color = "gray55", hjust = 0),
    legend.position  = "bottom",
    legend.title     = element_text(face = "bold", size = 9),
    panel.grid.minor = element_blank(),
    strip.text       = element_text(face = "bold"),
    axis.title       = element_text(size = 10, color = "gray30")
  )


# ── GRÁFICO 1: Histograma con quiebres Jenks ─────────────────────────────────
g1 <- ggplot(df, aes(x = ALTITUD_msnm, fill = PISO_NUM)) +
  geom_histogram(bins = 55, color = "white", linewidth = 0.2, alpha = 0.9) +
  geom_vline(
    xintercept = quiebres[-c(1, length(quiebres))],
    linetype = "dashed", color = "red", linewidth = 0.9
  ) +
  annotate("text",
           x     = quiebres[-c(1, length(quiebres))] + 80,
           y     = Inf,
           label = paste0(round(quiebres[-c(1, length(quiebres))]), " m"),
           vjust = 1.5, hjust = 0, size = 3, color = "red", fontface = "bold") +
  scale_fill_manual(values = colores_pisos, na.value = "gray80") +
  scale_x_continuous(labels = comma) +
  labs(
    title    = "Distribución de Altitud — Quiebres Jenks Natural Breaks",
    subtitle = paste0("n = ", nrow(df), " registros | GVF = ",
                      round(gvf[[2]], 4), " | Líneas rojas = puntos de quiebre"),
    x        = "Altitud (msnm)",
    y        = "Frecuencia",
    fill     = "Piso Altitudinal",
    caption  = "Fuente: ENA 2014-2024 (simulado) | Método: classInt::classIntervals(style='jenks')"
  ) +
  tema_ena

print(g1)


# ── GRÁFICO 2: Boxplot de Rendimiento por Piso ───────────────────────────────
g2 <- df %>%
  filter(!is.na(PISO_NUM)) %>%
  ggplot(aes(x = fct_rev(PISO_NUM), y = RENDIMIENTO_kg_ha, fill = PISO_NUM)) +
  geom_boxplot(
    outlier.shape = 21, outlier.size = 1.8,
    outlier.alpha = 0.5, width = 0.55, alpha = 0.85
  ) +
  geom_jitter(aes(color = PISO_NUM), width = 0.18, alpha = 0.25, size = 0.9) +
  scale_fill_manual(values  = colores_pisos, guide = "none") +
  scale_color_manual(values = colores_pisos, guide = "none") +
  scale_y_continuous(labels = comma) +
  coord_flip() +
  labs(
    title    = "Rendimiento Agrícola por Piso Altitudinal",
    subtitle = "Distribución de kg/ha según clasificación Jenks Natural Breaks",
    x        = NULL,
    y        = "Rendimiento (kg/ha)",
    caption  = "Cada punto = un registro de productor"
  ) +
  tema_ena

print(g2)


# ── GRÁFICO 3: Diferencias entre pisos (barras divergentes) ──────────────────
dif_plot <- diferencias %>%
  filter(!is.na(Dif_Rend_abs)) %>%
  mutate(direccion = Dif_Rend_abs > 0)

g3 <- ggplot(dif_plot,
             aes(x = PISO_NUM, y = Dif_Rend_abs, fill = direccion)) +
  geom_col(width = 0.6, show.legend = FALSE, alpha = 0.9) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.6) +
  geom_text(
    aes(label = paste0(ifelse(Dif_Rend_abs > 0, "+", ""),
                       format(Dif_Rend_abs, big.mark = ","), " kg/ha\n(",
                       ifelse(Dif_Rend_pct > 0, "+", ""), Dif_Rend_pct, "%)")),
    vjust = ifelse(dif_plot$Dif_Rend_abs > 0, -0.3, 1.3),
    size = 3.2, fontface = "bold", color = "gray20"
  ) +
  scale_fill_manual(values = c("TRUE" = "#2E8B57", "FALSE" = "#C0392B")) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0.2, 0.2))) +
  labs(
    title    = "Diferencia de Rendimiento entre Pisos Altitudinales Consecutivos",
    subtitle = "Δ kg/ha y Δ% respecto al piso inmediatamente inferior",
    x        = "Piso Altitudinal",
    y        = "Δ Rendimiento (kg/ha)",
    caption  = "Verde = mayor rendimiento | Rojo = menor rendimiento que el piso anterior"
  ) +
  tema_ena +
  theme(axis.text.x = element_text(angle = 12, hjust = 1, size = 9))

print(g3)


# ── GRÁFICO 4: Mapa de calor Rendimiento × Piso × Cultivo ────────────────────
g4 <- cultivo_piso %>%
  ggplot(aes(x = PISO_NUM,
             y = fct_reorder(CULTIVO, Rendimiento),
             fill = Rendimiento)) +
  geom_tile(color = "white", linewidth = 0.6) +
  geom_text(aes(label = format(Rendimiento, big.mark = ",")),
            size = 2.7, color = "white", fontface = "bold") +
  scale_fill_viridis_c(option = "D", labels = comma,
                       name = "Rendimiento\n(kg/ha)") +
  labs(
    title    = "Mapa de Calor: Rendimiento por Cultivo y Piso Altitudinal",
    subtitle = "Clasificación Jenks Natural Breaks | ENA 2014-2024",
    x        = "Piso Altitudinal",
    y        = "Cultivo",
    caption  = "Valor = rendimiento promedio (kg/ha)"
  ) +
  tema_ena +
  theme(axis.text.x = element_text(angle = 12, hjust = 1, size = 9))

print(g4)


# ── GRÁFICO 5: Ridge plot altitud por departamento ───────────────────────────
g5 <- df %>%
  filter(!is.na(PISO_NUM)) %>%
  mutate(DEPARTAMENTO = fct_reorder(DEPARTAMENTO, ALTITUD_msnm, .fun = median)) %>%
  ggplot(aes(x = ALTITUD_msnm, y = DEPARTAMENTO, fill = after_stat(x))) +
  geom_density_ridges_gradient(
    scale = 2.2, rel_min_height = 0.01,
    color = "white", linewidth = 0.3
  ) +
  geom_vline(
    xintercept = quiebres[-c(1, length(quiebres))],
    linetype = "dashed", color = "red", linewidth = 0.65, alpha = 0.75
  ) +
  scale_fill_viridis_c(option = "C", name = "Altitud\n(msnm)") +
  scale_x_continuous(labels = comma) +
  labs(
    title    = "Perfil Altitudinal por Departamento",
    subtitle = "Líneas rojas = quiebres Jenks | Ordenado por altitud mediana",
    x        = "Altitud (msnm)",
    y        = NULL,
    caption  = "Fuente: ENA 2014-2024 (simulado)"
  ) +
  tema_ena +
  theme(legend.position = "right")

print(g5)


# ── GRÁFICO 6: Producción total por piso y cultivo (stacked) ─────────────────
top6 <- cultivo_piso %>%
  group_by(CULTIVO) %>%
  summarise(tot = sum(Produccion)) %>%
  slice_max(tot, n = 6) %>%
  pull(CULTIVO)

g6 <- df %>%
  filter(!is.na(PISO_NUM), CULTIVO %in% top6) %>%
  group_by(PISO_NUM, CULTIVO) %>%
  summarise(Produccion = sum(PRODUCCION_t, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = PISO_NUM, y = Produccion, fill = CULTIVO)) +
  geom_col(position = "stack", width = 0.65,
           color = "white", linewidth = 0.35, alpha = 0.9) +
  scale_fill_brewer(palette = "Set2", name = "Cultivo") +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "Producción Total por Piso Altitudinal y Cultivo",
    subtitle = "Top 6 cultivos | Datos simulados ENA 2014-2024",
    x        = "Piso Altitudinal",
    y        = "Producción (toneladas)",
    caption  = "Fuente: ENA 2014-2024 (simulado)"
  ) +
  tema_ena +
  theme(axis.text.x = element_text(angle = 12, hjust = 1, size = 9))

print(g6)




