#===============================================================
# AUTOCORRELACIÓN ESPACIAL — ÍNDICE DE MORAN
# Aplicado a múltiples datasets reales
# UNAP — Estadística Espacial — Clever Nelson Choque Zuñiga
#===============================================================

# ---------------------------------------------------------------
# 0. LIBRERÍAS
# ---------------------------------------------------------------
paquetes <- c("sf", "spdep", "spData", "ggplot2", "dplyr",
              "sp", "MASS", "Guerry", "patchwork", "scales")

instalar_faltantes <- paquetes[!(paquetes %in% installed.packages()[, "Package"])]
if (length(instalar_faltantes) > 0) install.packages(instalar_faltantes)

lapply(paquetes, library, character.only = TRUE)

set.seed(123)
NSIM <- 499

# ---------------------------------------------------------------
# 1. FUNCIÓN GENERAL DE ANÁLISIS DE AUTOCORRELACIÓN ESPACIAL
# ---------------------------------------------------------------
# sf_obj    : objeto sf con geometría (polígonos o puntos)
# var       : nombre (string) de la variable a analizar
# vecindad  : "queen" (polígonos) o "knn" (puntos)
# k         : número de vecinos si vecindad = "knn"
# nombre    : etiqueta del dataset para los títulos
analizar_moran <- function(sf_obj, var, vecindad = "queen", k = 5,
                            nombre = "Dataset", unidad_leyenda = "") {

  x <- sf_obj[[var]]

  # --- Matriz de vecindad ---
  if (vecindad == "queen") {
    nb <- poly2nb(sf_obj, queen = TRUE)
  } else {
    coords <- st_coordinates(st_centroid(st_geometry(sf_obj)))
    nb <- knn2nb(knearneigh(coords, k = k))
  }
  lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

  # --- Test de Moran (normalidad) ---
  mt <- moran.test(x, lw, zero.policy = TRUE)
  I_obs   <- unname(mt$estimate["Moran I statistic"])
  E_I     <- unname(mt$estimate["Expectation"])
  Var_I   <- unname(mt$estimate["Variance"])
  p_norm  <- mt$p.value

  # --- Test de Monte Carlo ---
  mc <- moran.mc(x, lw, nsim = NSIM, zero.policy = TRUE)
  p_mc <- mc$p.value

  # --- Clasificación del resultado ---
  resultado <- if (p_mc >= 0.05) {
    "Aleatorio (no significativo)"
  } else if (I_obs > E_I) {
    "Cluster positivo (+)"
  } else {
    "Cluster negativo / disperso (-)"
  }

  # --- Diagrama de dispersión de Moran + cuadrantes LISA ---
  z  <- scale(x)[, 1]
  wz <- lag.listw(lw, z, zero.policy = TRUE)

  cuadrante <- case_when(
    z >= 0 & wz >= 0 ~ "HH (Alto-Alto)",
    z <  0 & wz <  0 ~ "LL (Bajo-Bajo)",
    z >= 0 & wz <  0 ~ "HL (Alto-Bajo)",
    z <  0 & wz >= 0 ~ "LH (Bajo-Alto)"
  )

  df_moran <- data.frame(z = z, wz = wz, cuadrante = cuadrante)
  sf_obj$cuadrante_lisa <- cuadrante

  colores_lisa <- c("HH (Alto-Alto)" = "#1f4e8c", "HL (Alto-Bajo)" = "#7a1f1f",
                     "LH (Bajo-Alto)" = "#f2a488", "LL (Bajo-Bajo)" = "#a8ddd0")

  # Mapa de la variable
  p_mapa <- ggplot(sf_obj) +
    geom_sf(aes(fill = .data[[var]]), color = "white", linewidth = 0.1) +
    scale_fill_viridis_c(option = "inferno", name = unidad_leyenda) +
    labs(title = paste0("Mapa: ", var)) +
    theme_minimal(base_size = 9) +
    theme(axis.text = element_blank(), axis.ticks = element_blank())

  # Mapa de cuadrantes LISA
  p_lisa <- ggplot(sf_obj) +
    geom_sf(aes(fill = cuadrante_lisa), color = "white", linewidth = 0.1) +
    scale_fill_manual(values = colores_lisa, name = "Cuadrante") +
    labs(title = "Cuadrantes LISA") +
    theme_minimal(base_size = 9) +
    theme(axis.text = element_blank(), axis.ticks = element_blank())

  # Diagrama de dispersión de Moran
  p_scatter <- ggplot(df_moran, aes(x = z, y = wz, color = cuadrante)) +
    geom_point(alpha = 0.75) +
    geom_smooth(method = "lm", se = FALSE, color = "orangered",
                aes(group = 1), linewidth = 0.8) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
    scale_color_manual(values = colores_lisa, name = "Cuadrante") +
    labs(title = sprintf("Diagrama de Moran  (I=%.4f  p=%.3f)", I_obs, p_mc),
         x = "z(i) estandarizado", y = "W.z(i) rezago") +
    theme_minimal(base_size = 9)

  # Histograma Monte Carlo
  df_mc <- data.frame(I_perm = mc$res[-length(mc$res)])
  p_mc_plot <- ggplot(df_mc, aes(x = I_perm)) +
    geom_histogram(aes(fill = I_perm >= I_obs), bins = 30, color = "white") +
    scale_fill_manual(values = c("FALSE" = "#3b7dd8", "TRUE" = "#d64545"),
                       labels = c("< I obs", ">= I obs"), name = "") +
    geom_vline(xintercept = I_obs, color = "orangered", linewidth = 0.9) +
    labs(title = sprintf("Monte Carlo (n=%d perm.)", NSIM),
         x = "I permutado", y = "Frecuencia") +
    theme_minimal(base_size = 9)

  panel <- (p_mapa + p_lisa) / (p_scatter + p_mc_plot) +
    plot_annotation(
      title = sprintf("%s | I=%.4f | p(MC)=%.3f -> %s", nombre, I_obs, p_mc, resultado),
      subtitle = sprintf("E[I]=%.4f | Var[I]=%.6f | p(normal)=%.4g", E_I, Var_I, p_norm)
    )

  list(
    nombre = nombre, n = nrow(sf_obj), I = I_obs, E_I = E_I, Var_I = Var_I,
    p_norm = p_norm, p_mc = p_mc, resultado = resultado,
    moran.test = mt, moran.mc = mc, panel = panel
  )
}

# ---------------------------------------------------------------
# 2. DATASET 1 — NORTH CAROLINA: TASA DE MORTALIDAD SIDS
# ---------------------------------------------------------------
nc <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
nc$SIDS_RATE <- (nc$SID74 / nc$BIR74) * 1000

res_nc <- analizar_moran(nc, "SIDS_RATE", vecindad = "queen",
                          nombre = "N. Carolina - Tasa SIDS",
                          unidad_leyenda = "por 1000 nac.")

# ---------------------------------------------------------------
# 3. DATASET 2 — NUEVA ZELANDA: INGRESO MEDIANO REGIONAL
# ---------------------------------------------------------------
data(nz, package = "spData")
nz_sf <- st_as_sf(nz)
# La variable de ingreso mediano puede llamarse Median_income o similar según versión de spData
nombre_var_nz <- intersect(c("Median_income", "median_income"), names(nz_sf))[1]

res_nz <- analizar_moran(nz_sf, nombre_var_nz, vecindad = "queen",
                          nombre = "Nueva Zelanda - Ingreso mediano",
                          unidad_leyenda = "NZD")

# ---------------------------------------------------------------
# 4. DATASET 3 — MEUSE: CONCENTRACIÓN DE ZINC EN SUELO
# ---------------------------------------------------------------
data(meuse, package = "sp")
meuse_sf <- st_as_sf(meuse, coords = c("x", "y"), crs = 28992)
meuse_sf$logzn <- log(meuse_sf$zinc)

res_meuse <- analizar_moran(meuse_sf, "logzn", vecindad = "knn", k = 5,
                             nombre = "Meuse - log(Zinc)",
                             unidad_leyenda = "log(ppm)")

# ---------------------------------------------------------------
# 5. DATASET 4 — BOSTON: VALOR MEDIANO DE VIVIENDAS
# ---------------------------------------------------------------
data(Boston, package = "MASS")
# Boston no trae coordenadas reales -> se genera una grilla sintética
# reproducible únicamente para fines de vecindad KNN (ver limitaciones del informe)
set.seed(123)
n_boston <- nrow(Boston)
lado <- ceiling(sqrt(n_boston))
grid_xy <- expand.grid(x = 1:lado, y = 1:lado)[1:n_boston, ]
boston_sf <- st_as_sf(cbind(Boston, grid_xy), coords = c("x", "y"), crs = NA)

res_boston <- analizar_moran(boston_sf, "medv", vecindad = "knn", k = 5,
                              nombre = "Boston - Valor vivienda",
                              unidad_leyenda = "miles USD")

# ---------------------------------------------------------------
# 6. DATASET 5 — FRANCIA 1830: CRIMINALIDAD CONTRA LA PROPIEDAD
# ---------------------------------------------------------------
data(gfrance85, package = "Guerry")
gfrance_sf <- st_as_sf(gfrance85)
gfrance_sf$Crime_prop <- gfrance_sf$Crm_prs  # ajustar nombre real de columna si difiere

res_francia <- analizar_moran(gfrance_sf, "Crime_prop", vecindad = "queen",
                               nombre = "Francia 1830 - Crimen propiedad",
                               unidad_leyenda = "índice")

# ---------------------------------------------------------------
# 7. TABLA RESUMEN GLOBAL
# ---------------------------------------------------------------
resultados <- list(res_nc, res_nz, res_meuse, res_boston, res_francia)

tabla_resumen <- do.call(rbind, lapply(resultados, function(r) {
  data.frame(
    Dataset   = r$nombre,
    n         = r$n,
    I_obs     = round(r$I, 4),
    E_I       = round(r$E_I, 4),
    p_normal  = signif(r$p_norm, 3),
    p_MC      = round(r$p_mc, 3),
    Resultado = r$resultado
  )
}))

print(tabla_resumen)

# ---------------------------------------------------------------
# 8. GRÁFICO COMPARATIVO
# ---------------------------------------------------------------
tabla_resumen$Significativo <- ifelse(tabla_resumen$p_MC < 0.05,
                                       "Significativo p<0.05", "No significativo")

grafico_comparativo <- ggplot(tabla_resumen,
       aes(x = reorder(Dataset, I_obs), y = I_obs, fill = Significativo)) +
  geom_col() +
  geom_text(aes(label = I_obs), hjust = ifelse(tabla_resumen$I_obs >= 0, -0.15, 1.1),
            size = 3.2, fontface = "bold") +
  coord_flip() +
  scale_fill_manual(values = c("Significativo p<0.05" = "#2f6db5",
                                "No significativo" = "grey70")) +
  labs(title = "Comparación del Índice de Moran - 5 datasets",
       subtitle = "Vecindad queen/KNN | Monte Carlo 499 permutaciones",
       x = NULL, y = "I de Moran observado", fill = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

grafico_comparativo

# ---------------------------------------------------------------
# 9. EXPORTAR PANELES Y RESULTADOS
# ---------------------------------------------------------------
dir.create("salidas_moran", showWarnings = FALSE)

ggsave("salidas_moran/01_north_carolina.png", res_nc$panel,     width = 10, height = 8, dpi = 150)
ggsave("salidas_moran/02_nueva_zelanda.png",  res_nz$panel,     width = 10, height = 8, dpi = 150)
ggsave("salidas_moran/03_meuse.png",          res_meuse$panel,  width = 10, height = 8, dpi = 150)
ggsave("salidas_moran/04_boston.png",         res_boston$panel, width = 10, height = 8, dpi = 150)
ggsave("salidas_moran/05_francia_1830.png",   res_francia$panel,width = 10, height = 8, dpi = 150)
ggsave("salidas_moran/06_comparacion.png",    grafico_comparativo, width = 8, height = 5, dpi = 150)

write.csv(tabla_resumen, "salidas_moran/tabla_resumen_moran.csv", row.names = FALSE)

cat("\n✔ Análisis completo. Paneles y tabla exportados en la carpeta 'salidas_moran/'.\n")
