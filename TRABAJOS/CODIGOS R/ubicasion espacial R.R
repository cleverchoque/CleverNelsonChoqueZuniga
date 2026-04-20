#librerias

library(haven)
library(ggplot2)
library(dplyr)

# Cargar datos

datos3 <- read_sav("D:/UNIVERCIDAD UNAP/CURSOS X SEMESTRE/ESTADISTICA ESPACIAL/UNIDAD I/ENA_2014_2024.sav", 
                   n_max = 500000, encoding = "latin1")

colSums(!is.na(datos3[, c("P901","P905","P907","LATITUD","LONGITUD")]))



# FILTRADO - solo filas con coordenadas y valores válidos
datos_sp <- datos3 %>%
  select(LATITUD, LONGITUD, P901, P905, P907) %>%
  filter(!is.na(LATITUD), !is.na(LONGITUD),
         !is.na(P901), !is.na(P905), !is.na(P907)) %>%
  mutate(
    P901 = as.numeric(P901),
    P905 = as.numeric(P905),
    P907 = as.numeric(P907),
    P901_label = ifelse(P901 == 1, "Si solicito credito", "No solicito credito"),
    P905_label = ifelse(P905 == 1, "Si tuvo seguro", "No tuvo seguro"),
    P907_label = ifelse(P907 == 1, "Si tuvo ahorro", "No tuvo ahorro")
  )

cat("Total de registros con coordenadas:", nrow(datos_sp), "\n")

# APLICASIONES BASICAS 

cat("\n--- P901: Solicitud de Credito ---\n")
print(table(datos_sp$P901_label))
print(round(prop.table(table(datos_sp$P901_label)) * 100, 1))

cat("\n--- P905: Seguro Agropecuario ---\n")
print(table(datos_sp$P905_label))
print(round(prop.table(table(datos_sp$P905_label)) * 100, 1))

cat("\n--- P907: Cuenta de Ahorro ---\n")
print(table(datos_sp$P907_label))
print(round(prop.table(table(datos_sp$P907_label)) * 100, 1))

# MAPA 1 - P901 Credito
ggplot(datos_sp, aes(x = LONGITUD, y = LATITUD, color = P901_label)) +
  geom_point(size = 3, alpha = 0.8) +
  scale_color_manual(values = c("Si solicito credito" = "#00c48c",
                                "No solicito credito" = "#e74c3c")) +
  labs(title = "P901 - Solicitud de Credito Agropecuario",
       subtitle = paste0("ENA | n = ", nrow(datos_sp), " unidades agropecuarias con coordenadas"),
       x = "Longitud", y = "Latitud", color = "Credito",
       caption = "Fuente: Encuesta Nacional Agraria - INEI") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")
ggsave("mapa_P901_credito.png", width = 10, height = 8, dpi = 300)


# MAPA 2 - P905 Seguro
ggplot(datos_sp, aes(x = LONGITUD, y = LATITUD, color = P905_label)) +
  geom_point(size = 3, alpha = 0.8) +
  scale_color_manual(values = c("Si tuvo seguro" = "#3498db",
                                "No tuvo seguro" = "#e67e22")) +
  labs(title = "P905 - Seguro Agropecuario",
       subtitle = paste0("ENA | n = ", nrow(datos_sp), " unidades agropecuarias con coordenadas"),
       x = "Longitud", y = "Latitud", color = "Seguro",
       caption = "Fuente: Encuesta Nacional Agraria - INEI") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")
ggsave("mapa_P905_seguro.png", width = 10, height = 8, dpi = 300)


# MAPA 3 - P907 Ahorro
ggplot(datos_sp, aes(x = LONGITUD, y = LATITUD, color = P907_label)) +
  geom_point(size = 3, alpha = 0.8) +
  scale_color_manual(values = c("Si tuvo ahorro" = "#9b59b6",
                                "No tuvo ahorro" = "#95a5a6")) +
  labs(title = "P907 - Cuenta de Ahorro",
       subtitle = paste0("ENA | n = ", nrow(datos_sp), " unidades agropecuarias con coordenadas"),
       x = "Longitud", y = "Latitud", color = "Ahorro",
       caption = "Fuente: Encuesta Nacional Agraria - INEI") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")
ggsave("mapa_P907_ahorro.png", width = 10, height = 8, dpi = 300)


# GRAFICO DE BARRAS - Comparacion
datos_barras <- data.frame(
  Variable = c("P901 Credito", "P901 Credito",
               "P905 Seguro", "P905 Seguro",
               "P907 Ahorro", "P907 Ahorro"),
  Respuesta = c("Si", "No", "Si", "No", "Si", "No"),
  Cantidad = c(
    sum(datos_sp$P901 == 1), sum(datos_sp$P901 == 2),
    sum(datos_sp$P905 == 1), sum(datos_sp$P905 == 2),
    sum(datos_sp$P907 == 1), sum(datos_sp$P907 == 2)
  )
) %>%
  group_by(Variable) %>%
  mutate(Porcentaje = round(Cantidad / sum(Cantidad) * 100, 1))

ggplot(datos_barras, aes(x = Variable, y = Porcentaje, fill = Respuesta)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(Porcentaje, "%\n(n=", Cantidad, ")")),
            position = position_dodge(0.9), vjust = -0.3, size = 3.5) +
  scale_fill_manual(values = c("Si" = "#00c48c", "No" = "#e74c3c")) +
  labs(title = "Comparacion de Servicios Financieros Agropecuarios",
       subtitle = "P901: Credito | P905: Seguro | P907: Ahorro — ENA",
       x = "Variable", y = "Porcentaje (%)",
       caption = "Fuente: Encuesta Nacional Agraria - INEI") +
  theme_minimal(base_size = 13) +
  ylim(0, 110)
ggsave("grafico_comparacion.png", width = 12, height = 7, dpi = 300)


