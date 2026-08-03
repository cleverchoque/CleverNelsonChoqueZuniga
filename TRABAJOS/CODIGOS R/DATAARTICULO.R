# ============================================================
# IDW vs KRIGING — CÓDIGO FINAL COMPLETO + RESULTADOS
# SIN SUPERPOSICIÓN DE TÍTULOS — TODAS LAS FIGURAS CON print()
# Temperatura Mínima: Estaciones SENAMHI Puno (15 estaciones)
# Parcelas Papa y Quinua: ENA 2024 INEI
# Universidad Nacional del Altiplano — UNAP — FINESI
# ============================================================
# INSTRUCCIONES:
#   Ctrl+Shift+Enter en RStudio para ejecutar todo
#   Figuras aparecen en panel Plots — usa ◀ ▶ para navegar
#   Resultados numéricos aparecen en la Consola
#   Export → Save as Image para guardar cada figura
# ============================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(readr, dplyr, sf, gstat, sp, ggplot2,
               gridExtra, grid, moments)

UMBRAL_PAPA   <-  0
UMBRAL_QUINUA <- -3

# ============================================================
# PASO 1: CARGAR DATOS
# ============================================================
CARATULA <- read_csv(
  "C:/Users/TUF GAMING/Downloads/CARATULA.csv",
  show_col_types = FALSE)

USOSTIERRA <- read_csv(
  "C:/Users/TUF GAMING/Downloads/USOSTIERRA.csv",
  show_col_types = FALSE)

# ============================================================
# PASO 2: FILTRAR PUNO — PAPA Y QUINUA
# ============================================================
cultivos_puno <- USOSTIERRA %>%
  filter(toupper(NOMBREDD) == "PUNO") %>%
  filter(grepl("PAPA|QUINUA|QUINOA", toupper(P115_NOM))) %>%
  mutate(CULTIVO = case_when(
    grepl("PAPA",          toupper(P115_NOM)) ~ "PAPA",
    grepl("QUINUA|QUINOA", toupper(P115_NOM)) ~ "QUINUA",
    TRUE ~ "OTRO")) %>%
  select(NSEGM, ID_PROD, NOMBREDD, NOMBREPV, NOMBREDI,
         CULTIVO, P115_NOM, P117_SUP_ha)

puno_gps <- CARATULA %>%
  filter(toupper(NOMBREDD) == "PUNO") %>%
  filter(!is.na(LATITUD) & !is.na(LONGITUD)) %>%
  select(NSEGM, ID_PROD, LATITUD, LONGITUD)

parcelas_gps <- cultivos_puno %>%
  inner_join(puno_gps, by = c("NSEGM","ID_PROD")) %>%
  distinct(NSEGM, ID_PROD, .keep_all = TRUE) %>%
  filter(!is.na(LATITUD) & !is.na(LONGITUD))

# ============================================================
# PASO 3: DATOS SENAMHI
# ============================================================
senamhi_puno <- data.frame(
  ESTACION  = c("PUNO","JULIACA","TARACO","AZANGARO","LAMPA",
                "HUANCANE","ILAVE","MACUSANI","CRUCERO","YUNGUYO",
                "AYAVIRI","DESAGUADERO","SANTA LUCIA","PROGRESO","CAPAZO"),
  LATITUD   = c(-15.826,-15.484,-15.317,-14.916,-15.362,
                -15.201,-16.083,-14.073,-14.362,-16.243,
                -14.879,-16.563,-15.717,-14.827,-17.001),
  LONGITUD  = c(-70.012,-70.157,-69.810,-70.194,-70.371,
                -69.758,-69.648,-70.431,-70.021,-69.092,
                -70.590,-69.036,-70.617,-69.853,-69.533),
  ALTITUD   = c(3812,3826,3819,3860,3892,3846,3878,
                4316,4148,3823,3916,3810,4005,4380,4548),
  TMIN_PROM = c(2.1,1.8,1.5,0.9,0.3,1.2,-0.1,
                -3.8,-2.1,1.7,-0.8,0.4,-1.2,-2.9,-5.3),
  stringsAsFactors = FALSE)

# ============================================================
# PASO 4: ANÁLISIS EXPLORATORIO COMPLETO
# ============================================================

cat("\n╔══════════════════════════════════════════════════════╗\n")
cat("║   ANÁLISIS EXPLORATORIO — DATOS SENAMHI PUNO        ║\n")
cat("╚══════════════════════════════════════════════════════╝\n\n")

cat("── ESTACIONES SENAMHI ──────────────────────────────────\n")
print(senamhi_puno)

cat("\n── ESTADÍSTICOS TEMPERATURA MÍNIMA (°C) ───────────────\n")
cat("N:        ", nrow(senamhi_puno), "\n")
cat("Mínimo:   ", min(senamhi_puno$TMIN_PROM), "°C\n")
cat("Q1:       ", quantile(senamhi_puno$TMIN_PROM, 0.25), "°C\n")
cat("Mediana:  ", median(senamhi_puno$TMIN_PROM), "°C\n")
cat("Media:    ", round(mean(senamhi_puno$TMIN_PROM), 4), "°C\n")
cat("Q3:       ", quantile(senamhi_puno$TMIN_PROM, 0.75), "°C\n")
cat("Máximo:   ", max(senamhi_puno$TMIN_PROM), "°C\n")
cat("DE:       ", round(sd(senamhi_puno$TMIN_PROM), 4), "°C\n")
cat("CV (%):   ", round(sd(senamhi_puno$TMIN_PROM)/
                          abs(mean(senamhi_puno$TMIN_PROM))*100, 2), "\n")
cat("Asimetría:", round(skewness(senamhi_puno$TMIN_PROM), 4), "\n")
cat("Curtosis: ", round(kurtosis(senamhi_puno$TMIN_PROM), 4), "\n")

cat("\n── ESTADÍSTICOS ALTITUD (m.s.n.m.) ────────────────────\n")
cat("N:        ", nrow(senamhi_puno), "\n")
cat("Mínimo:   ", min(senamhi_puno$ALTITUD), "m\n")
cat("Media:    ", round(mean(senamhi_puno$ALTITUD), 1), "m\n")
cat("Máximo:   ", max(senamhi_puno$ALTITUD), "m\n")
cat("DE:       ", round(sd(senamhi_puno$ALTITUD), 1), "m\n")

cat("\n── CORRELACIÓN TMIN ~ ALTITUD ──────────────────────────\n")
cor_tmin_alt <- cor(senamhi_puno$TMIN_PROM, senamhi_puno$ALTITUD)
cor_test     <- cor.test(senamhi_puno$TMIN_PROM, senamhi_puno$ALTITUD)
modelo_grad  <- lm(TMIN_PROM ~ ALTITUD, data = senamhi_puno)
gradiente    <- coef(modelo_grad)[2] * 100

cat("Pearson r:  ", round(cor_tmin_alt, 4), "\n")
cat("r²:         ", round(cor_tmin_alt^2, 4), "\n")
cat("p-valor:    ", round(cor_test$p.value, 6), "\n")
cat("IC 95%:    [", round(cor_test$conf.int[1], 4),
    ",", round(cor_test$conf.int[2], 4), "]\n")
cat("Gradiente:  ", round(gradiente, 4), "°C / 100m\n")
cat("Intercepto: ", round(coef(modelo_grad)[1], 4), "°C\n")
cat("R² modelo:  ", round(summary(modelo_grad)$r.squared, 4), "\n")
cat("Ecuación:   Tmin =", round(coef(modelo_grad)[1], 3),
    "+", round(coef(modelo_grad)[2], 6), "* Altitud\n")

cat("\n── SHAPIRO-WILK (Normalidad Tmin) ──────────────────────\n")
sw_test <- shapiro.test(senamhi_puno$TMIN_PROM)
cat("Estadístico W:", round(sw_test$statistic, 4), "\n")
cat("p-valor:      ", round(sw_test$p.value, 4), "\n")
cat("Conclusión:   ",
    ifelse(sw_test$p.value > 0.05,
           "Normal (p > 0.05)", "No Normal (p < 0.05)"), "\n")

cat("\n── PARCELAS ENA 2024 — PUNO ────────────────────────────\n")
cat("Total parcelas con GPS:       ", nrow(parcelas_gps), "\n")
cat("Parcelas PAPA:                ", sum(parcelas_gps$CULTIVO=="PAPA"), "\n")
cat("Parcelas QUINUA:              ", sum(parcelas_gps$CULTIVO=="QUINUA"), "\n")

cat("\nDistribución por cultivo y provincia:\n")
print(parcelas_gps %>%
        group_by(CULTIVO, NOMBREPV) %>%
        summarise(N = n(), .groups="drop") %>%
        arrange(CULTIVO, desc(N)))

cat("\nEstadísticos superficie (ha) PAPA:\n")
papa_sup <- parcelas_gps %>% filter(CULTIVO=="PAPA") %>%
  pull(P117_SUP_ha) %>% as.numeric()
cat("Media:", round(mean(papa_sup, na.rm=TRUE), 3), "ha\n")
cat("Mediana:", round(median(papa_sup, na.rm=TRUE), 3), "ha\n")
cat("Min:", round(min(papa_sup, na.rm=TRUE), 3), "ha\n")
cat("Max:", round(max(papa_sup, na.rm=TRUE), 3), "ha\n")

cat("\nEstadísticos superficie (ha) QUINUA:\n")
quin_sup <- parcelas_gps %>% filter(CULTIVO=="QUINUA") %>%
  pull(P117_SUP_ha) %>% as.numeric()
cat("Media:", round(mean(quin_sup, na.rm=TRUE), 3), "ha\n")
cat("Mediana:", round(median(quin_sup, na.rm=TRUE), 3), "ha\n")
cat("Min:", round(min(quin_sup, na.rm=TRUE), 3), "ha\n")
cat("Max:", round(max(quin_sup, na.rm=TRUE), 3), "ha\n")

# ratio para mapas correctos
lat_media <- mean(c(parcelas_gps$LATITUD, senamhi_puno$LATITUD))
ratio_geo <- 1 / cos(lat_media * pi / 180)

# ── FIGURA 1 ─────────────────────────────────────────────────
print(
  ggplot() +
    geom_point(data=senamhi_puno,
               aes(x=LONGITUD, y=LATITUD, color=TMIN_PROM), size=5) +
    geom_text(data=senamhi_puno,
              aes(x=LONGITUD, y=LATITUD, label=ESTACION),
              size=2.8, vjust=-1.0, fontface="bold") +
    scale_color_gradientn(
      colours=c("#053061","#4393C3","#FFFFFF","#F4A582","#B2182B"),
      name="T_min (°C)") +
    coord_fixed(ratio=ratio_geo) +
    scale_x_continuous(labels=function(x) paste0(abs(x),"°O")) +
    scale_y_continuous(labels=function(y) paste0(abs(y),"°S")) +
    labs(title="Figura 1. Estaciones SENAMHI — Temperatura Mínima",
         subtitle=paste0("Región Puno | ",nrow(senamhi_puno)," estaciones"),
         x="Longitud", y="Latitud", caption="Fuente: SENAMHI Puno") +
    theme_minimal(base_size=12) +
    theme(plot.title=element_text(face="bold"))
)

# ── FIGURA 2 ─────────────────────────────────────────────────
print(
  ggplot(senamhi_puno, aes(x=ALTITUD, y=TMIN_PROM, label=ESTACION)) +
    geom_point(color="#1565C0", size=4) +
    geom_smooth(method="lm", color="red", se=TRUE, linewidth=1.2) +
    geom_text(vjust=-0.9, size=3, color="gray30") +
    geom_hline(yintercept=UMBRAL_PAPA,
               color="red", linetype="dashed", linewidth=1) +
    geom_hline(yintercept=UMBRAL_QUINUA,
               color="darkred", linetype="dotted", linewidth=1) +
    annotate("text", x=3660, y=UMBRAL_PAPA+0.35,
             label="Umbral Papa (0°C)", color="red", size=3.5, hjust=0) +
    annotate("text", x=3660, y=UMBRAL_QUINUA-0.45,
             label="Umbral Quinua (-3°C)", color="darkred", size=3.5, hjust=0) +
    scale_y_continuous(expand=expansion(mult=c(0.12,0.12))) +
    labs(title="Figura 2. Temperatura Mínima vs Altitud — SENAMHI Puno",
         subtitle=paste0("r = ",round(cor_tmin_alt,3),
                         "  |  Gradiente: ",round(gradiente,3)," °C/100m"),
         x="Altitud (m.s.n.m.)", y="Temperatura Mínima (°C)",
         caption="Fuente: SENAMHI Puno") +
    theme_minimal(base_size=12) +
    theme(plot.title=element_text(face="bold"))
)

# ── FIGURA 3 ─────────────────────────────────────────────────
print(
  ggplot() +
    geom_point(data=parcelas_gps,
               aes(x=LONGITUD, y=LATITUD, color=CULTIVO),
               size=2, alpha=0.8) +
    geom_point(data=senamhi_puno, aes(x=LONGITUD, y=LATITUD),
               shape=17, size=4, color="black") +
    geom_text(data=senamhi_puno,
              aes(x=LONGITUD, y=LATITUD, label=ESTACION),
              size=2.5, vjust=-0.9, color="black", fontface="bold") +
    scale_color_manual(values=c("PAPA"="#E65100","QUINUA"="#1B5E20"),
                       name="Cultivo") +
    coord_fixed(ratio=ratio_geo) +
    scale_x_continuous(labels=function(x) paste0(abs(x),"°O")) +
    scale_y_continuous(labels=function(y) paste0(abs(y),"°S")) +
    labs(title="Figura 3. Parcelas ENA 2024 — Papa y Quinua en Puno",
         subtitle=paste0("Puntos = parcelas ENA  |  ▲ = Estaciones SENAMHI\n",
                         "Papa: ",sum(parcelas_gps$CULTIVO=="PAPA"),
                         "  |  Quinua: ",sum(parcelas_gps$CULTIVO=="QUINUA")),
         x="Longitud", y="Latitud",
         caption="Fuente: ENA 2024 INEI + SENAMHI") +
    theme_minimal(base_size=12) +
    theme(plot.title=element_text(face="bold"))
)

# ============================================================
# PASO 5: OBJETOS ESPACIALES
# ============================================================
senamhi_sf  <- st_as_sf(senamhi_puno,
                        coords=c("LONGITUD","LATITUD"), crs=4326)
senamhi_utm <- st_transform(senamhi_sf, crs=32719)
senamhi_sp  <- as(senamhi_utm, "Spatial")

puno_bbox <- st_bbox(c(xmin=-71.5,xmax=-68.8,ymin=-17.3,ymax=-13.5),
                     crs=st_crs(4326)) %>%
  st_as_sfc() %>% st_as_sf() %>% st_transform(crs=32719)

grilla <- st_make_grid(puno_bbox, cellsize=c(5000,5000),
                       what="centers") %>%
  st_as_sf() %>% st_filter(puno_bbox)
grilla_sp <- as(grilla, "Spatial")

cat("\n╔══════════════════════════════════════════════════════╗\n")
cat("║   MÉTODO 1: IDW — VALIDACIÓN CRUZADA LOOCV          ║\n")
cat("╚══════════════════════════════════════════════════════╝\n\n")

# ============================================================
# PASO 6: IDW
# ============================================================
res_idw <- data.frame(p=numeric(),RMSE=numeric(),MAE=numeric(),R2=numeric())

for (p_val in c(1,2,3)) {
  cv   <- krige.cv(TMIN_PROM~1, senamhi_sp,
                   nfold=nrow(senamhi_sp), set=list(idp=p_val))
  r    <- cv$residual
  obs  <- senamhi_sp$TMIN_PROM
  pred <- obs - r
  rmse <- sqrt(mean(r^2))
  mae  <- mean(abs(r))
  r2   <- 1 - sum(r^2)/sum((obs-mean(obs))^2)
  bias <- mean(r)
  
  cat("── IDW p =", p_val, "──────────────────────────────────\n")
  cat("RMSE:  ", round(rmse, 4), "°C\n")
  cat("MAE:   ", round(mae,  4), "°C\n")
  cat("R²:    ", round(r2,   4), "\n")
  cat("Sesgo: ", round(bias, 4), "°C\n")
  cat("Residuos por estación:\n")
  resid_df <- data.frame(
    Estacion = senamhi_puno$ESTACION,
    Obs      = round(obs, 2),
    Pred     = round(pred, 2),
    Residuo  = round(r, 4)
  )
  print(resid_df)
  cat("\n")
  
  res_idw <- rbind(res_idw,
                   data.frame(p=p_val, RMSE=round(rmse,4),
                              MAE=round(mae,4), R2=round(r2,4)))
}

p_opt    <- res_idw$p[which.min(res_idw$RMSE)]
idw_best <- res_idw[which.min(res_idw$RMSE),]

cat("── TABLA COMPARATIVA IDW (p=1,2,3) ────────────────────\n")
print(res_idw)
cat("\nPotencia óptima: p =", p_opt, "\n")
cat("Mejor IDW — RMSE =", idw_best$RMSE,
    "| MAE =", idw_best$MAE, "| R² =", idw_best$R2, "\n\n")

idw_result <- idw(TMIN_PROM~1, senamhi_sp, grilla_sp, idp=p_opt)

# ============================================================
# PASO 7: KRIGING ORDINARIO
# ============================================================
cat("╔══════════════════════════════════════════════════════╗\n")
cat("║   MÉTODO 2: KRIGING ORDINARIO                       ║\n")
cat("╚══════════════════════════════════════════════════════╝\n\n")

vgm_exp   <- variogram(TMIN_PROM~1, senamhi_sp,
                       cutoff=250000, width=30000)

cat("── VARIOGRAMA EXPERIMENTAL ─────────────────────────────\n")
print(vgm_exp)

mod_abrev <- c("Sph","Exp","Gau")
mod_names <- c("Esférico","Exponencial","Gaussiano")
sse_vals  <- rep(Inf,3)
mods_fit  <- vector("list",3)

cat("\n── AJUSTE DE MODELOS TEÓRICOS ──────────────────────────\n")
for (i in 1:3) {
  tryCatch({
    fit           <- fit.variogram(vgm_exp, model=vgm(mod_abrev[i]))
    mods_fit[[i]] <- fit
    sse_vals[i]   <- attr(fit,"SSErr")
    cat("\nModelo:", mod_names[i], "\n")
    cat("  SSErr:  ", round(sse_vals[i], 6), "\n")
    cat("  Pepita (nugget): ", round(fit$psill[1], 4), "\n")
    cat("  Meseta (sill):   ", round(sum(fit$psill), 4), "\n")
    cat("  Alcance (range): ", round(fit$range[2]/1000, 2), "km\n")
    print(fit)
  }, error=function(e) cat("Modelo", mod_names[i], "no ajustó.\n"))
}

mejor_idx  <- which.min(sse_vals)
mejor_mod  <- mods_fit[[mejor_idx]]
mejor_name <- mod_names[mejor_idx]

cat("\n── MODELO SELECCIONADO: ", mejor_name, "──────────────────\n")
cat("SSErr:   ", round(sse_vals[mejor_idx], 6), "\n")
cat("Pepita:  ", round(mejor_mod$psill[1], 4), "\n")
cat("Meseta:  ", round(sum(mejor_mod$psill), 4), "\n")
cat("Alcance: ", round(mejor_mod$range[2]/1000, 2), "km\n\n")

# ── FIGURA 4: VARIOGRAMA ──────────────────────────────────────
plot(vgm_exp, mejor_mod,
     main=paste0("Figura 4. Variograma Experimental — Modelo ",
                 mejor_name, "\nTemperatura Mínima | Región Puno"),
     xlab="Distancia (m)",
     ylab=expression(Semivarianza ~ hat(gamma)(h)),
     pch=16, col="#1565C0", cex=1.3)

cat("── KRIGING LOOCV — RESIDUOS POR ESTACIÓN ──────────────\n")
cv_krig  <- krige.cv(TMIN_PROM~1, senamhi_sp,
                     model=mejor_mod, nfold=nrow(senamhi_sp))
r_krig   <- cv_krig$residual
obs_krig <- senamhi_sp$TMIN_PROM
pred_krig <- obs_krig - r_krig

resid_krig <- data.frame(
  Estacion = senamhi_puno$ESTACION,
  Altitud  = senamhi_puno$ALTITUD,
  Obs      = round(obs_krig, 2),
  Pred     = round(pred_krig, 2),
  Residuo  = round(r_krig, 4),
  Abs_Res  = round(abs(r_krig), 4)
)
print(resid_krig)

rmse_krig <- sqrt(mean(r_krig^2))
mae_krig  <- mean(abs(r_krig))
r2_krig   <- 1 - sum(r_krig^2)/sum((obs_krig-mean(obs_krig))^2)
bias_krig <- mean(r_krig)

cat("\n── KRIGING LOOCV — MÉTRICAS ────────────────────────────\n")
cat("RMSE:  ", round(rmse_krig, 4), "°C\n")
cat("MAE:   ", round(mae_krig,  4), "°C\n")
cat("R²:    ", round(r2_krig,   4), "\n")
cat("Sesgo: ", round(bias_krig, 4), "°C\n")

krig_result <- krige(TMIN_PROM~1, senamhi_sp,
                     grilla_sp, model=mejor_mod)
mejor_metodo <- ifelse(rmse_krig < idw_best$RMSE,
                       "Kriging Ordinario","IDW")

# ============================================================
# TABLA COMPARATIVA FINAL
# ============================================================
cat("\n╔══════════════════════════════════════════════════════╗\n")
cat("║   TABLA COMPARATIVA FINAL — IDW vs KRIGING          ║\n")
cat("╚══════════════════════════════════════════════════════╝\n\n")

tabla_final <- data.frame(
  Fuente   = rep("SENAMHI Puno (15 estaciones)", 2),
  Variable = rep("Temperatura Mínima (°C)", 2),
  Metodo   = c(paste0("IDW (p=",p_opt,")"),
               paste0("Kriging (",mejor_name,")")),
  RMSE     = c(idw_best$RMSE, round(rmse_krig,4)),
  MAE      = c(idw_best$MAE,  round(mae_krig,4)),
  R2       = c(idw_best$R2,   round(r2_krig,4)),
  Bias     = c(round(mean(krige.cv(TMIN_PROM~1, senamhi_sp,
                                   nfold=nrow(senamhi_sp),
                                   set=list(idp=p_opt))$residual), 4),
               round(bias_krig, 4))
)
print(tabla_final)
cat("\n▶ MEJOR MÉTODO:", mejor_metodo, "\n")
cat("▶ Mejora RMSE Kriging vs IDW:",
    round((idw_best$RMSE - rmse_krig)/idw_best$RMSE*100, 2), "%\n")

# ============================================================
# MAPAS — WGS84
# ============================================================
idw_sf_wgs <- idw_result %>% st_as_sf() %>%
  rename(tmin_pred=var1.pred) %>% st_transform(crs=4326)

krig_sf_wgs <- krig_result %>% st_as_sf() %>%
  rename(tmin_pred=var1.pred, tmin_var=var1.var) %>%
  st_transform(crs=4326)

lim_min <- floor(min(c(idw_sf_wgs$tmin_pred, krig_sf_wgs$tmin_pred)))
lim_max <- ceiling(max(c(idw_sf_wgs$tmin_pred, krig_sf_wgs$tmin_pred)))
paleta  <- c("#053061","#2166AC","#4393C3","#92C5DE",
             "#D1E5F0","#FDDBC7","#F4A582","#D6604D","#B2182B")

crear_mapa <- function(datos, titulo, rmse_val) {
  ggplot() +
    geom_sf(data=datos, aes(color=tmin_pred), size=0.9, shape=15) +
    geom_point(data=senamhi_puno, aes(x=LONGITUD,y=LATITUD),
               color="yellow", shape=17, size=3) +
    scale_color_gradientn(colours=paleta, name="T_min\n(°C)",
                          limits=c(lim_min,lim_max)) +
    coord_sf(crs=st_crs(4326)) +
    scale_x_continuous(labels=function(x) paste0(abs(x),"°O")) +
    scale_y_continuous(labels=function(y) paste0(abs(y),"°S")) +
    labs(title=titulo,
         subtitle=paste0("RMSE=",round(rmse_val,3),
                         "°C  |  ▲=Estaciones SENAMHI"),
         x="Longitud", y="Latitud",
         caption="SENAMHI Puno + ENA 2024 INEI") +
    theme_minimal(base_size=11) +
    theme(plot.title=element_text(face="bold"))
}

# ── FIGURAS 5a y 5b ──────────────────────────────────────────
print(crear_mapa(idw_sf_wgs,
                 paste0("Figura 5a. IDW — p=",p_opt), idw_best$RMSE))
print(crear_mapa(krig_sf_wgs,
                 paste0("Figura 5b. Kriging — ",mejor_name), rmse_krig))

# ── CATEGORÍAS DE RIESGO ──────────────────────────────────────
krig_sf_wgs <- krig_sf_wgs %>%
  mutate(
    riesgo_papa = case_when(
      tmin_pred < -5          ~ "Muy Alto (< -5°C)",
      tmin_pred < UMBRAL_PAPA ~ "Alto (-5 a 0°C)",
      tmin_pred < 3           ~ "Moderado (0 a 3°C)",
      TRUE                    ~ "Sin riesgo (>= 3°C)"),
    riesgo_papa = factor(riesgo_papa,
                         levels=c("Muy Alto (< -5°C)","Alto (-5 a 0°C)",
                                  "Moderado (0 a 3°C)","Sin riesgo (>= 3°C)")),
    riesgo_quinua = case_when(
      tmin_pred < -7            ~ "Muy Alto (< -7°C)",
      tmin_pred < UMBRAL_QUINUA ~ "Alto (-7 a -3°C)",
      tmin_pred < UMBRAL_PAPA   ~ "Moderado (-3 a 0°C)",
      tmin_pred < 3             ~ "Bajo (0 a 3°C)",
      TRUE                      ~ "Sin riesgo (>= 3°C)"),
    riesgo_quinua = factor(riesgo_quinua,
                           levels=c("Muy Alto (< -7°C)","Alto (-7 a -3°C)",
                                    "Moderado (-3 a 0°C)","Bajo (0 a 3°C)",
                                    "Sin riesgo (>= 3°C)"))
  )

# ── FIGURAS 6 y 7 ────────────────────────────────────────────
print(
  ggplot() +
    geom_sf(data=krig_sf_wgs, aes(color=riesgo_papa), size=1.0, shape=15) +
    geom_point(data=parcelas_gps %>% filter(CULTIVO=="PAPA"),
               aes(x=LONGITUD,y=LATITUD),
               color="white", shape=4, size=1.2, alpha=0.9) +
    geom_point(data=senamhi_puno, aes(x=LONGITUD,y=LATITUD),
               shape=17, size=3, color="black") +
    scale_color_manual(
      values=c("Muy Alto (< -5°C)"="#67000D","Alto (-5 a 0°C)"="#D32F2F",
               "Moderado (0 a 3°C)"="#FF9800","Sin riesgo (>= 3°C)"="#4CAF50"),
      name="Riesgo\nHelada") +
    coord_sf(crs=st_crs(4326)) +
    scale_x_continuous(labels=function(x) paste0(abs(x),"°O")) +
    scale_y_continuous(labels=function(y) paste0(abs(y),"°S")) +
    labs(title="Figura 6. Riesgo de Heladas — PAPA",
         subtitle=paste0("Umbral: T_min < ",UMBRAL_PAPA,
                         "°C  |  × = Parcelas ENA (N=",
                         sum(parcelas_gps$CULTIVO=="PAPA"),")"),
         x="Longitud", y="Latitud",
         caption="Kriging Ordinario  |  SENAMHI Puno + ENA 2024 INEI") +
    theme_minimal(base_size=12) +
    theme(plot.title=element_text(face="bold",color="#B71C1C",size=14))
)

print(
  ggplot() +
    geom_sf(data=krig_sf_wgs, aes(color=riesgo_quinua), size=1.0, shape=15) +
    geom_point(data=parcelas_gps %>% filter(CULTIVO=="QUINUA"),
               aes(x=LONGITUD,y=LATITUD),
               color="white", shape=4, size=1.2, alpha=0.9) +
    geom_point(data=senamhi_puno, aes(x=LONGITUD,y=LATITUD),
               shape=17, size=3, color="black") +
    scale_color_manual(
      values=c("Muy Alto (< -7°C)"="#4A148C","Alto (-7 a -3°C)"="#D32F2F",
               "Moderado (-3 a 0°C)"="#FF9800","Bajo (0 a 3°C)"="#FFF176",
               "Sin riesgo (>= 3°C)"="#4CAF50"),
      name="Riesgo\nHelada") +
    coord_sf(crs=st_crs(4326)) +
    scale_x_continuous(labels=function(x) paste0(abs(x),"°O")) +
    scale_y_continuous(labels=function(y) paste0(abs(y),"°S")) +
    labs(title="Figura 7. Riesgo de Heladas — QUINUA",
         subtitle=paste0("Umbral: T_min < ",UMBRAL_QUINUA,
                         "°C  |  × = Parcelas ENA (N=",
                         sum(parcelas_gps$CULTIVO=="QUINUA"),")"),
         x="Longitud", y="Latitud",
         caption="Kriging Ordinario  |  SENAMHI Puno + ENA 2024 INEI") +
    theme_minimal(base_size=12) +
    theme(plot.title=element_text(face="bold",color="#1B5E20",size=14))
)

# ============================================================
# DISTRIBUCIÓN DEL RIESGO — DATOS COMPLETOS
# ============================================================
cat("\n╔══════════════════════════════════════════════════════╗\n")
cat("║   DISTRIBUCIÓN DEL RIESGO DE HELADAS                ║\n")
cat("╚══════════════════════════════════════════════════════╝\n\n")

cat("── PAPA — Frecuencia absoluta (puntos de grilla) ───────\n")
print(table(krig_sf_wgs$riesgo_papa))

cat("\n── PAPA — Frecuencia relativa (%) ──────────────────────\n")
print(round(prop.table(table(krig_sf_wgs$riesgo_papa))*100, 2))

cat("\n── QUINUA — Frecuencia absoluta ────────────────────────\n")
print(table(krig_sf_wgs$riesgo_quinua))

cat("\n── QUINUA — Frecuencia relativa (%) ────────────────────\n")
print(round(prop.table(table(krig_sf_wgs$riesgo_quinua))*100, 2))

cat("\n── ESTADÍSTICOS Tmin INTERPOLADA (Kriging) ─────────────\n")
cat("Min:   ", round(min(krig_sf_wgs$tmin_pred),3), "°C\n")
cat("Media: ", round(mean(krig_sf_wgs$tmin_pred),3), "°C\n")
cat("Max:   ", round(max(krig_sf_wgs$tmin_pred),3), "°C\n")
cat("DE:    ", round(sd(krig_sf_wgs$tmin_pred),3), "°C\n")

cat("\n── ESTADÍSTICOS Varianza Kriging (incertidumbre) ───────\n")
cat("Min var:  ", round(min(krig_sf_wgs$tmin_var),4), "\n")
cat("Media var:", round(mean(krig_sf_wgs$tmin_var),4), "\n")
cat("Max var:  ", round(max(krig_sf_wgs$tmin_var),4), "\n")

# ── FIGURAS 8a y 8b ──────────────────────────────────────────
print(
  ggplot() +
    geom_sf(data=krig_sf_wgs, aes(color=riesgo_papa), size=1.0, shape=15) +
    geom_point(data=parcelas_gps %>% filter(CULTIVO=="PAPA"),
               aes(x=LONGITUD,y=LATITUD),
               color="white", shape=4, size=1.2) +
    geom_point(data=senamhi_puno, aes(x=LONGITUD,y=LATITUD),
               shape=17, size=3, color="black") +
    scale_color_manual(
      values=c("Muy Alto (< -5°C)"="#67000D","Alto (-5 a 0°C)"="#D32F2F",
               "Moderado (0 a 3°C)"="#FF9800","Sin riesgo (>= 3°C)"="#4CAF50"),
      name="Riesgo") +
    coord_sf(crs=st_crs(4326)) +
    scale_x_continuous(labels=function(x) paste0(abs(x),"°O")) +
    scale_y_continuous(labels=function(y) paste0(abs(y),"°S")) +
    labs(title="Figura 8a. Riesgo PAPA — Comparación",
         x="Longitud", y="Latitud") +
    theme_minimal(base_size=11) +
    theme(plot.title=element_text(face="bold",color="#B71C1C"))
)

print(
  ggplot() +
    geom_sf(data=krig_sf_wgs, aes(color=riesgo_quinua), size=1.0, shape=15) +
    geom_point(data=parcelas_gps %>% filter(CULTIVO=="QUINUA"),
               aes(x=LONGITUD,y=LATITUD),
               color="white", shape=4, size=1.2) +
    geom_point(data=senamhi_puno, aes(x=LONGITUD,y=LATITUD),
               shape=17, size=3, color="black") +
    scale_color_manual(
      values=c("Muy Alto (< -7°C)"="#4A148C","Alto (-7 a -3°C)"="#D32F2F",
               "Moderado (-3 a 0°C)"="#FF9800","Bajo (0 a 3°C)"="#FFF176",
               "Sin riesgo (>= 3°C)"="#4CAF50"),
      name="Riesgo") +
    coord_sf(crs=st_crs(4326)) +
    scale_x_continuous(labels=function(x) paste0(abs(x),"°O")) +
    scale_y_continuous(labels=function(y) paste0(abs(y),"°S")) +
    labs(title="Figura 8b. Riesgo QUINUA — Comparación",
         x="Longitud", y="Latitud") +
    theme_minimal(base_size=11) +
    theme(plot.title=element_text(face="bold",color="#1B5E20"))
)

# ── FIGURAS 9a, 9b, 9c ───────────────────────────────────────
df_met <- data.frame(
  Metodo = c(paste0("IDW\n(p=",p_opt,")"),
             paste0("Kriging\n(",mejor_name,")")),
  RMSE   = c(idw_best$RMSE, round(rmse_krig,4)),
  MAE    = c(idw_best$MAE,  round(mae_krig,4)),
  R2     = c(idw_best$R2,   round(r2_krig,4))
)

print(
  ggplot(df_met, aes(Metodo,RMSE,fill=Metodo)) +
    geom_col(width=0.5, show.legend=FALSE) +
    geom_text(aes(label=RMSE), vjust=-0.5, size=5, fontface="bold") +
    scale_fill_manual(values=c("#FF7043","#1565C0")) +
    scale_y_continuous(expand=expansion(mult=c(0,0.25))) +
    labs(title="Figura 9a. RMSE — Validación LOOCV",
         subtitle="Menor = Mejor", y="RMSE (°C)", x="") +
    theme_minimal(base_size=12) +
    theme(plot.title=element_text(face="bold"))
)

print(
  ggplot(df_met, aes(Metodo,MAE,fill=Metodo)) +
    geom_col(width=0.5, show.legend=FALSE) +
    geom_text(aes(label=MAE), vjust=-0.5, size=5, fontface="bold") +
    scale_fill_manual(values=c("#FF7043","#1565C0")) +
    scale_y_continuous(expand=expansion(mult=c(0,0.25))) +
    labs(title="Figura 9b. MAE — Validación LOOCV",
         subtitle="Menor = Mejor", y="MAE (°C)", x="") +
    theme_minimal(base_size=12) +
    theme(plot.title=element_text(face="bold"))
)

print(
  ggplot(df_met, aes(Metodo,R2,fill=Metodo)) +
    geom_col(width=0.5, show.legend=FALSE) +
    geom_text(aes(label=R2), vjust=-0.5, size=5, fontface="bold") +
    scale_fill_manual(values=c("#FF7043","#1565C0")) +
    scale_y_continuous(limits=c(0,1.25),
                       expand=expansion(mult=c(0,0))) +
    labs(title="Figura 9c. R² — Validación LOOCV",
         subtitle="Mayor = Mejor", y="R²", x="") +
    theme_minimal(base_size=12) +
    theme(plot.title=element_text(face="bold"))
)

# ============================================================
# RESULTADOS FINALES PARA EL ARTÍCULO LaTeX
# ============================================================
cat("\n╔══════════════════════════════════════════════════════╗\n")
cat("║   VALORES PARA COMPLETAR EL ARTÍCULO LaTeX          ║\n")
cat("╚══════════════════════════════════════════════════════╝\n\n")

cat("── TABLA 1 (Estadísticos descriptivos) ─────────────────\n")
cat("N parcelas PAPA:    ", sum(parcelas_gps$CULTIVO=="PAPA"), "\n")
cat("N parcelas QUINUA:  ", sum(parcelas_gps$CULTIVO=="QUINUA"), "\n")
cat("N parcelas Total:   ", nrow(parcelas_gps), "\n")
cat("Tmin SENAMHI min:   ", min(senamhi_puno$TMIN_PROM), "°C\n")
cat("Tmin SENAMHI media: ", round(mean(senamhi_puno$TMIN_PROM),3), "°C\n")
cat("Tmin SENAMHI max:   ", max(senamhi_puno$TMIN_PROM), "°C\n")
cat("Tmin SENAMHI DE:    ", round(sd(senamhi_puno$TMIN_PROM),3), "°C\n")

cat("\n── TABLA 2 (IDW vs Kriging LOOCV) ──────────────────────\n")
cat("IDW p óptimo:       ", p_opt, "\n")
cat("IDW RMSE:           ", idw_best$RMSE, "°C\n")
cat("IDW MAE:            ", idw_best$MAE,  "°C\n")
cat("IDW R²:             ", idw_best$R2, "\n")
cat("Kriging modelo:     ", mejor_name, "\n")
cat("Kriging alcance:    ", round(mejor_mod$range[2]/1000,1), "km\n")
cat("Kriging pepita:     ", round(mejor_mod$psill[1],4), "\n")
cat("Kriging meseta:     ", round(sum(mejor_mod$psill),4), "\n")
cat("Kriging RMSE:       ", round(rmse_krig,4), "°C\n")
cat("Kriging MAE:        ", round(mae_krig,4),  "°C\n")
cat("Kriging R²:         ", round(r2_krig,4), "\n")
cat("Mejor método:       ", mejor_metodo, "\n")
cat("Mejora RMSE (%):    ",
    round((idw_best$RMSE-rmse_krig)/idw_best$RMSE*100,2), "%\n")

cat("\n── TABLA 3 (Riesgo heladas %) ──────────────────────────\n")
rp <- round(prop.table(table(krig_sf_wgs$riesgo_papa))*100,1)
rq <- round(prop.table(table(krig_sf_wgs$riesgo_quinua))*100,1)
cat("PAPA — Muy Alto:    ", rp["Muy Alto (< -5°C)"], "%\n")
cat("PAPA — Alto:        ", rp["Alto (-5 a 0°C)"], "%\n")
cat("PAPA — Moderado:    ", rp["Moderado (0 a 3°C)"], "%\n")
cat("PAPA — Sin riesgo:  ", rp["Sin riesgo (>= 3°C)"], "%\n")
cat("QUINUA — Muy Alto:  ", rq["Muy Alto (< -7°C)"], "%\n")
cat("QUINUA — Alto:      ", rq["Alto (-7 a -3°C)"], "%\n")
cat("QUINUA — Moderado:  ", rq["Moderado (-3 a 0°C)"], "%\n")
cat("QUINUA — Bajo:      ", rq["Bajo (0 a 3°C)"], "%\n")
cat("QUINUA — Sin riesgo:", rq["Sin riesgo (>= 3°C)"], "%\n")

cat("\n── PARA INTRODUCCIÓN ───────────────────────────────────\n")
cat("r Tmin~Altitud:     ", round(cor_tmin_alt,4), "\n")
cat("p-valor:            ", round(cor_test$p.value,6), "\n")
cat("Gradiente:          ", round(gradiente,3), "°C/100m\n")
cat("R² regresión:       ", round(summary(modelo_grad)$r.squared,4), "\n")
cat("Shapiro-Wilk W:     ", round(sw_test$statistic,4), "\n")
cat("Shapiro p-valor:    ", round(sw_test$p.value,4), "\n")

cat("\n=======================================================\n")
cat("12 figuras en panel Plots — usa ◀ ▶ para navegar\n")
cat("=======================================================\n")

