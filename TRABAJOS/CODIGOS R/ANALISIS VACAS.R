# ============================================================
# ANALISIS GANADERO COMPLETO - TODAS LAS HOJAS
# ============================================================

library(readxl); library(dplyr); library(tidyr)
library(ggplot2); library(scales); library(stringr)
library(gridExtra); library(knitr)

EXCEL <- "datos_ganaderos_2026-04-20.xlsx"

COL_AZUL  <- "#1565C0"
COL_VERDE <- "#2d6a4f"
COL_ROJO  <- "#C62828"
COL_AMBAR <- "#F57F17"
COL_GRIS  <- "#546E7A"

tema <- theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face="bold", size=13, color="#1a1a2e"),
    plot.subtitle    = element_text(size=9, color="#666"),
    axis.text.x      = element_text(angle=45, hjust=1, size=8),
    axis.text.y      = element_text(size=8),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color="#eeeeee"),
    legend.position  = "bottom",
    legend.text      = element_text(size=8),
    plot.background  = element_rect(fill="white", color=NA)
  )

titulo <- function(txt) {
  grid::textGrob(txt, gp=grid::gpar(fontsize=14, fontface="bold", col="#1a1a2e"))
}

# ============================================================
# CARGAR TODAS LAS HOJAS
# ============================================================

mastitis <- read_excel(EXCEL, sheet="MASTITIS") %>%
  rename(n_vaca=N_Vaca, edad=Edad, lactacion_n=Lactacion_N,
         lactacion_dias=Lactacion_Dias, leche_24h=Leche_24h,
         dias_enferma=Dias_Enferma, fecha=Fecha, com1=Comentario1, com2=Comentario2) %>%
  mutate(
    n_vaca        = as.character(n_vaca),
    edad          = as.numeric(edad),
    lactacion_n   = as.integer(lactacion_n),
    lactacion_dias= as.integer(lactacion_dias),
    leche_24h     = as.numeric(leche_24h),
    dias_enferma  = suppressWarnings(as.numeric(dias_enferma)),
    cuadrante = case_when(
      com1=="PI" ~ "Ant. Izquierdo", com1=="PD" ~ "Ant. Derecho",
      com1=="AI" ~ "Post. Izquierdo", com1=="AD" ~ "Post. Derecho",
      TRUE ~ com1),
    rango_del = case_when(
      is.na(dias_enferma) ~ NA_character_,
      dias_enferma < 0    ~ "Pre-parto",
      dias_enferma <= 60  ~ "0-60 DEL",
      dias_enferma <= 120 ~ "61-120 DEL",
      dias_enferma <= 200 ~ "121-200 DEL",
      TRUE                ~ ">200 DEL")
  )

prod_raw <- read_excel(EXCEL, sheet="PRODUCCI\u00d3N DE LECHE", col_names=FALSE, skip=6)
prod_leche <- prod_raw %>%
  rename(col1=1,col2=2,col3=3,col4=4,col5=5) %>%
  filter(!is.na(col1), str_detect(as.character(col1),"^\\d{4}")) %>%
  mutate(
    n_vaca    = str_extract(as.character(col1),"^\\d{4}"),
    status    = str_extract(as.character(col1),"(Pre.\\d?|Insem|Matar|Seca|Vacia)"),
    dias_lact = as.integer(as.character(col2)),
    prod_act  = as.numeric(str_extract(as.character(col4),"^[\\d.]+")),
    prod_prom = as.numeric(str_extract(as.character(col4),"[\\d.]+$")),
    prod_total= as.numeric(str_extract(as.character(col5),"\\d+$"))
  ) %>%
  select(n_vaca,status,dias_lact,prod_act,prod_prom,prod_total) %>%
  filter(!is.na(n_vaca),!is.na(prod_act))

gest_raw <- read_excel(EXCEL, sheet="VACAS GESTANTES", col_names=FALSE, skip=4)
gestantes <- gest_raw %>%
  rename(col1=1,col2=2,col3=3,col4=4,col5=5,col6=6,col7=7) %>%
  filter(!is.na(col1), str_detect(as.character(col1),"^\\d{4}")) %>%
  mutate(
    n_vaca       = str_extract(as.character(col1),"^\\d{4}"),
    lactacion    = str_extract(as.character(col1),"\\d+$"),
    dias_lech    = as.integer(as.character(col3)),
    status       = str_extract(as.character(col4),"^\\w+"),
    dias_gest    = as.integer(str_extract(as.character(col4),"\\d+$")),
    n_ins        = as.integer(as.character(col5)),
    dias_abiertos= as.integer(as.character(col6)),
    prod_prom    = as.numeric(as.character(col7))
  ) %>%
  select(n_vaca,lactacion,dias_lech,status,dias_gest,n_ins,dias_abiertos,prod_prom)

insem_raw <- read_excel(EXCEL, sheet="VACAS INSEMINADAS", col_names=FALSE, skip=5)
inseminadas <- insem_raw %>%
  rename(col1=1,col2=2,col3=3,col4=4) %>%
  filter(!is.na(col1), str_detect(as.character(col1),"^\\d{4}")) %>%
  mutate(
    n_vaca       = str_extract(as.character(col1),"^\\d{4}"),
    status       = str_extract(as.character(col2),"Insem|Pre.|Seca|Vacia"),
    dias_abiertos= as.integer(str_extract(as.character(col2),"(?<=Insem\\.\\s{0,3})\\d+")),
    prod_prom    = as.numeric(as.character(col3))
  ) %>%
  select(n_vaca,status,dias_abiertos,prod_prom) %>%
  filter(!is.na(n_vaca))

secar_raw <- read_excel(EXCEL, sheet="VACAS PARA SECAR", col_names=FALSE, skip=6)
para_secar <- secar_raw %>%
  rename(col1=1,col2=2,col3=3,col4=4) %>%
  filter(!is.na(col1), str_detect(as.character(col1),"^\\d{4}")) %>%
  mutate(
    n_vaca       = str_extract(as.character(col1),"^\\d{4}"),
    status       = str_extract(as.character(col1),"Pre.|Seca|Insem"),
    dias_en_lact = as.integer(str_extract(as.character(col1),"(?<=\\s)\\d{3}(?=\\s)")),
    prod_prom    = as.numeric(str_extract(as.character(col1),"\\d+\\.\\d(?=\\s+\\d)")),
    fecha_secado = as.character(col2)
  ) %>%
  select(n_vaca,status,dias_en_lact,prod_prom,fecha_secado) %>%
  filter(!is.na(n_vaca))

prob_raw <- read_excel(EXCEL, sheet="VACAS PROBLEMA", col_names=FALSE, skip=3)
problemas <- prob_raw %>%
  rename(col1=1,col2=2,col3=3,col4=4,col5=5,col6=6) %>%
  filter(!is.na(col1), str_detect(as.character(col1),"^\\d{4}")) %>%
  mutate(
    n_vaca    = str_extract(as.character(col1),"^\\d{4}"),
    problema  = as.character(col2),
    dias_lact = as.integer(str_extract(as.character(col1),"(?<=\\s)\\d{3}(?=\\s)"))
  ) %>%
  select(n_vaca,problema,dias_lact) %>%
  filter(!is.na(n_vaca))

parto_raw <- read_excel(EXCEL, sheet="VACAS PROXIMAS AL PARTO", col_names=FALSE, skip=3)
proximas_parto <- parto_raw %>%
  rename(col1=1,col2=2,col3=3,col4=4) %>%
  filter(!is.na(col1), str_detect(as.character(col1),"^\\d{4}")) %>%
  mutate(
    n_vaca      = str_extract(as.character(col1),"^\\d{4}"),
    fecha_parto = as.character(col2),
    dias_falta  = as.integer(str_extract(as.character(col1),"(?<=-)-?\\d+"))
  ) %>%
  select(n_vaca,fecha_parto,dias_falta) %>%
  filter(!is.na(n_vaca))

secas_raw <- read_excel(EXCEL, sheet="VACAS SECAS", col_names=FALSE, skip=3)
vacas_secas <- secas_raw %>%
  rename(col1=1,col2=2,col3=3) %>%
  filter(!is.na(col1), str_detect(as.character(col1),"^\\d{4}")) %>%
  mutate(
    n_vaca      = str_extract(as.character(col1),"^\\d{4}"),
    fecha_parto = as.character(col2),
    dias_seca   = as.integer(str_extract(as.character(col1),"\\d+$"))
  ) %>%
  select(n_vaca,fecha_parto,dias_seca) %>%
  filter(!is.na(n_vaca))

vaqi_raw <- read_excel(EXCEL, sheet="VAQUILLAS PARA TEST DE PRE\u00d1EZ", col_names=FALSE, skip=3)
vaqi_test <- vaqi_raw %>%
  rename(col1=1,col2=2,col3=3,col4=4,col5=5) %>%
  filter(!is.na(col1), str_detect(as.character(col1),"^\\d{4}")) %>%
  mutate(
    n_vaca = str_extract(as.character(col1),"^\\d{4}"),
    edad   = as.numeric(str_extract(as.character(col1),"\\d+\\.\\d")),
    status = str_extract(as.character(col1),"Insem|Pre.|Vacia")
  ) %>%
  select(n_vaca,edad,status) %>%
  filter(!is.na(n_vaca))

vaqp_raw <- read_excel(EXCEL, sheet="VAQUILLAS PROXIMAS AL PARTO", col_names=FALSE, skip=3)
vaqi_parto <- vaqp_raw %>%
  rename(col1=1,col2=2,col3=3,col4=4,col5=5,col6=6,col7=7) %>%
  filter(!is.na(col1), str_detect(as.character(col1),"^\\d{4}")) %>%
  mutate(
    n_vaca      = str_extract(as.character(col1),"^\\d{4}"),
    fecha_parto = as.character(col2),
    dias_falta  = as.integer(as.character(col3))
  ) %>%
  select(n_vaca,fecha_parto,dias_falta) %>%
  filter(!is.na(n_vaca))

cat("Todas las hojas cargadas correctamente\n")

# ============================================================
# SECCION 1: RESUMEN GENERAL DEL HATO
# ============================================================

cat("\n========== RESUMEN GENERAL DEL HATO ==========\n")

resumen <- data.frame(
  Categoria = c(
    "--- PRODUCCION ---","Vacas en ordeno","Prod. promedio 24h (kg)",
    "DEL promedio","Prod. maxima 24h (kg)",
    "--- REPRODUCCION ---","Vacas gestantes","Dias abiertos prom (gestantes)",
    "Servicios prom para prenez","Vacas inseminadas",
    "Vacas proximas al parto","Vacas para secar","Vacas secas",
    "--- SANIDAD ---","Vacas con mastitis","Total episodios mastitis",
    "Episodios por vaca (prom)","Vacas problema",
    "--- RECRIA ---","Vaquillas proximas al parto","Vaquillas para test prenez"
  ),
  Valor = c(
    "","",
    round(mean(prod_leche$prod_act,na.rm=T),1),
    round(mean(prod_leche$dias_lact,na.rm=T),0),
    round(max(prod_leche$prod_act,na.rm=T),1),
    "","",
    round(mean(gestantes$dias_abiertos,na.rm=T),0),
    round(mean(gestantes$n_ins,na.rm=T),1),
    "","","","",
    "","",
    "","",
    "","",""
  )
)

resumen$Valor[resumen$Categoria=="Vacas en ordeno"]          <- nrow(prod_leche)
resumen$Valor[resumen$Categoria=="Vacas gestantes"]          <- nrow(gestantes)
resumen$Valor[resumen$Categoria=="Vacas inseminadas"]        <- nrow(inseminadas)
resumen$Valor[resumen$Categoria=="Vacas proximas al parto"]  <- nrow(proximas_parto)
resumen$Valor[resumen$Categoria=="Vacas para secar"]         <- nrow(para_secar)
resumen$Valor[resumen$Categoria=="Vacas secas"]              <- nrow(vacas_secas)
resumen$Valor[resumen$Categoria=="Vacas con mastitis"]       <- n_distinct(mastitis$n_vaca)
resumen$Valor[resumen$Categoria=="Total episodios mastitis"] <- nrow(mastitis)
resumen$Valor[resumen$Categoria=="Episodios por vaca (prom)"]<- round(nrow(mastitis)/n_distinct(mastitis$n_vaca),1)
resumen$Valor[resumen$Categoria=="Vacas problema"]           <- nrow(problemas)
resumen$Valor[resumen$Categoria=="Vaquillas proximas al parto"] <- nrow(vaqi_parto)
resumen$Valor[resumen$Categoria=="Vaquillas para test prenez"]  <- nrow(vaqi_test)

print(resumen, row.names=FALSE)

# Grafico resumen general
dat_res <- data.frame(
  Grupo  = c("En Ordeno","Gestantes","Inseminadas","Para Secar",
             "Secas","Con Mastitis","Problema","Vaqi. Parto"),
  N      = c(nrow(prod_leche),nrow(gestantes),nrow(inseminadas),
             nrow(para_secar),nrow(vacas_secas),n_distinct(mastitis$n_vaca),
             nrow(problemas),nrow(vaqi_parto)),
  Color  = c(COL_AZUL,COL_VERDE,COL_AMBAR,COL_GRIS,
             "#78909C",COL_ROJO,"#7B1FA2","#00838F")
)

ggplot(dat_res, aes(x=reorder(Grupo,N), y=N, fill=Grupo)) +
  geom_col(show.legend=FALSE) +
  geom_text(aes(label=N), hjust=-0.2, size=4, fontface="bold") +
  coord_flip() +
  scale_fill_manual(values=setNames(dat_res$Color, dat_res$Grupo)) +
  expand_limits(y=max(dat_res$N)*1.15) +
  labs(title="RESUMEN GENERAL DEL HATO",
       subtitle="Distribucion de vacas por categoria",
       x="", y="N de animales") + tema

# ============================================================
# SECCION 2: MASTITIS
# ============================================================

cat("\n========== MASTITIS ==========\n")
cat("Vacas afectadas:", n_distinct(mastitis$n_vaca), "\n")
cat("Total episodios:", nrow(mastitis), "\n")
cat("Episodios/vaca :", round(nrow(mastitis)/n_distinct(mastitis$n_vaca),1), "\n")

cat("\nTop 15 vacas:\n")
mastitis %>%
  group_by(n_vaca, edad, lactacion_n) %>%
  summarise(episodios=n(), leche=round(first(leche_24h),1),
            dias_lact=first(lactacion_dias), .groups="drop") %>%
  arrange(desc(episodios)) %>% slice(1:15) %>% print()

cat("\nPor cuadrante:\n")
mastitis %>%
  filter(!is.na(cuadrante), !cuadrante %in% c("AI+AD","SUBCLINICA")) %>%
  count(cuadrante,sort=T) %>%
  mutate(pct=percent(n/sum(n),1)) %>% print()

cat("\nPor etapa DEL:\n")
mastitis %>% filter(!is.na(rango_del)) %>%
  mutate(rango_del=factor(rango_del,
                          levels=c("Pre-parto","0-60 DEL","61-120 DEL","121-200 DEL",">200 DEL"))) %>%
  count(rango_del) %>% mutate(pct=percent(n/sum(n),1)) %>% print()

m1 <- mastitis %>% count(n_vaca) %>%
  ggplot(aes(x=n)) +
  geom_histogram(binwidth=1, fill=COL_AZUL, color="white") +
  geom_vline(xintercept=mean(count(mastitis,n_vaca)$n),
             linetype="dashed", color=COL_ROJO, linewidth=1) +
  labs(title="Episodios por vaca",
       subtitle=paste0("Media=",round(mean(count(mastitis,n_vaca)$n),1)),
       x="N episodios", y="N vacas") + tema

m2 <- mastitis %>%
  filter(!is.na(cuadrante), !cuadrante %in% c("AI+AD","SUBCLINICA")) %>%
  count(cuadrante) %>% mutate(pct=n/sum(n)) %>%
  ggplot(aes(x=reorder(cuadrante,n), y=n, fill=cuadrante)) +
  geom_col(show.legend=FALSE) +
  geom_text(aes(label=paste0(n," (",percent(pct,1),")")), hjust=-0.1, size=3.5) +
  coord_flip() +
  expand_limits(y=max(count(filter(mastitis,!cuadrante%in%c("AI+AD","SUBCLINICA")),cuadrante)$n)*1.4) +
  scale_fill_brewer(palette="Set2") +
  labs(title="Por cuadrante", x="", y="N episodios") + tema

m3 <- mastitis %>% filter(!is.na(lactacion_n)) %>%
  count(lactacion_n) %>% mutate(pct=n/sum(n)) %>%
  ggplot(aes(x=factor(lactacion_n), y=n, fill=factor(lactacion_n))) +
  geom_col(show.legend=FALSE) +
  geom_text(aes(label=paste0(n,"\n",percent(pct,1))), vjust=-0.3, size=3) +
  scale_fill_brewer(palette="Blues") +
  labs(title="Por lactacion", x="Lactacion", y="N episodios") + tema

m4 <- mastitis %>% filter(!is.na(rango_del)) %>%
  mutate(rango_del=factor(rango_del,
                          levels=c("Pre-parto","0-60 DEL","61-120 DEL","121-200 DEL",">200 DEL"))) %>%
  count(rango_del) %>% mutate(pct=n/sum(n)) %>%
  ggplot(aes(x=rango_del, y=pct, fill=rango_del)) +
  geom_col(show.legend=FALSE) +
  geom_text(aes(label=percent(pct,1)), vjust=-0.4, size=3.5) +
  scale_fill_manual(values=c("#90A4AE",COL_VERDE,COL_AMBAR,COL_ROJO,"#7B1FA2")) +
  scale_y_continuous(labels=percent, limits=c(0,0.9)) +
  labs(title="Por etapa de lactacion", x="", y="% episodios") + tema

grid.arrange(m1,m2,m3,m4, ncol=2, top=titulo("ANALISIS DE MASTITIS"))

# ============================================================
# SECCION 3: PRODUCCION DE LECHE
# ============================================================

cat("\n========== PRODUCCION DE LECHE ==========\n")
prod_leche %>% summarise(
  n=n(), min=round(min(prod_act,na.rm=T),1),
  media=round(mean(prod_act,na.rm=T),1),
  max=round(max(prod_act,na.rm=T),1),
  sd=round(sd(prod_act,na.rm=T),1),
  del_prom=round(mean(dias_lact,na.rm=T),0)
) %>% print()

cat("\nPor status:\n")
prod_leche %>% filter(!is.na(status)) %>%
  group_by(status) %>%
  summarise(n=n(), media=round(mean(prod_act,na.rm=T),1),
            max=round(max(prod_act,na.rm=T),1), .groups="drop") %>%
  arrange(desc(n)) %>% print()

p1 <- prod_leche %>%
  filter(!is.na(dias_lact), dias_lact<700) %>%
  mutate(rango=cut(dias_lact, breaks=c(0,60,120,180,240,300,400,700),
                   labels=c("1-60","61-120","121-180","181-240","241-300","301-400",">400"))) %>%
  group_by(rango) %>%
  summarise(n=n(), media=mean(prod_act,na.rm=T), de=sd(prod_act,na.rm=T),.groups="drop") %>%
  filter(!is.na(rango)) %>%
  ggplot(aes(x=rango, y=media, group=1)) +
  geom_ribbon(aes(ymin=pmax(media-de,0),ymax=media+de), fill=COL_AZUL, alpha=0.15) +
  geom_line(color=COL_AZUL, linewidth=1.3) +
  geom_point(aes(size=n), color=COL_AZUL, fill="white", shape=21, stroke=2) +
  geom_text(aes(label=paste0(round(media,1),"kg\n(n=",n,")")), vjust=-1.8, size=2.8) +
  scale_size_continuous(range=c(3,9), name="N vacas") +
  expand_limits(y=c(0,max(prod_leche$prod_act,na.rm=T)*1.3)) +
  labs(title="Curva de lactacion",
       x="Dias en leche (DEL)", y="kg/24h") + tema

p2 <- prod_leche %>%
  ggplot(aes(x=prod_act)) +
  geom_histogram(bins=20, fill=COL_VERDE, color="white", alpha=0.85) +
  geom_vline(xintercept=mean(prod_leche$prod_act,na.rm=T),
             linetype="dashed", color=COL_ROJO, linewidth=1) +
  annotate("text", x=mean(prod_leche$prod_act,na.rm=T)+1, y=Inf, vjust=2,
           label=paste0("Media=",round(mean(prod_leche$prod_act,na.rm=T),1),"kg"),
           color=COL_ROJO, size=3.5) +
  labs(title="Distribucion produccion 24h", x="kg/24h", y="N vacas") + tema

p3 <- prod_leche %>% filter(!is.na(status)) %>%
  group_by(status) %>%
  summarise(media=mean(prod_act,na.rm=T), n=n(), .groups="drop") %>%
  ggplot(aes(x=reorder(status,media), y=media, fill=status)) +
  geom_col(show.legend=FALSE) +
  geom_text(aes(label=paste0(round(media,1),"kg\n(n=",n,")")), hjust=-0.1, size=3) +
  coord_flip() +
  expand_limits(y=max(prod_leche$prod_act,na.rm=T)*1.3) +
  scale_fill_brewer(palette="Set2") +
  labs(title="Produccion por status", x="", y="kg/24h") + tema

p4 <- prod_leche %>% filter(!is.na(prod_total)) %>%
  ggplot(aes(x=prod_total/1000)) +
  geom_histogram(bins=18, fill=COL_AMBAR, color="white", alpha=0.85) +
  labs(title="Produccion total acumulada", x="Toneladas", y="N vacas") + tema

grid.arrange(p1,p2,p3,p4, ncol=2, top=titulo("ANALISIS DE PRODUCCION DE LECHE"))

# ============================================================
# SECCION 4: VACAS GESTANTES
# ============================================================

cat("\n========== VACAS GESTANTES ==========\n")
gestantes %>% summarise(
  n=n(),
  dias_abiertos_prom=round(mean(dias_abiertos,na.rm=T),0),
  dias_abiertos_max=max(dias_abiertos,na.rm=T),
  servicios_prom=round(mean(n_ins,na.rm=T),1),
  prod_prom=round(mean(prod_prom,na.rm=T),1),
  del_prom=round(mean(dias_lech,na.rm=T),0)
) %>% print()

g1 <- gestantes %>% filter(!is.na(dias_abiertos)) %>%
  ggplot(aes(x=dias_abiertos)) +
  geom_histogram(bins=15, fill=COL_AZUL, color="white", alpha=0.85) +
  geom_vline(xintercept=c(85,120), linetype="dashed",
             color=c(COL_VERDE,COL_ROJO), linewidth=1) +
  annotate("text",x=85,y=Inf,vjust=2,hjust=1.1,size=3,color=COL_VERDE,label="Meta:85d") +
  annotate("text",x=120,y=Inf,vjust=2,hjust=-0.1,size=3,color=COL_ROJO,label="Alerta:120d") +
  labs(title="Dias abiertos", x="Dias", y="N vacas") + tema

g2 <- gestantes %>% filter(!is.na(n_ins)) %>%
  count(n_ins) %>% mutate(pct=n/sum(n)) %>%
  ggplot(aes(x=factor(n_ins), y=n, fill=factor(n_ins))) +
  geom_col(show.legend=FALSE) +
  geom_text(aes(label=paste0(n,"\n(",percent(pct,1),")")), vjust=-0.3, size=3.5) +
  scale_fill_brewer(palette="RdYlGn", direction=-1) +
  labs(title="Servicios para prenez", x="N servicios", y="N vacas") + tema

g3 <- gestantes %>% filter(!is.na(dias_gest)) %>%
  mutate(etapa=case_when(
    dias_gest<250 ~ "Temprana (<250d)",
    dias_gest<270 ~ "Media (250-270d)",
    TRUE          ~ "Avanzada (>270d)"),
    etapa=factor(etapa,levels=c("Temprana (<250d)","Media (250-270d)","Avanzada (>270d)"))) %>%
  count(etapa) %>% mutate(pct=n/sum(n)) %>%
  ggplot(aes(x=etapa, y=n, fill=etapa)) +
  geom_col(show.legend=FALSE) +
  geom_text(aes(label=paste0(n,"\n",percent(pct,1))), vjust=-0.3, size=3.5) +
  scale_fill_manual(values=c(COL_VERDE,COL_AMBAR,COL_ROJO)) +
  labs(title="Estado de gestacion", x="", y="N vacas") + tema

g4 <- gestantes %>% filter(!is.na(prod_prom)) %>%
  ggplot(aes(x=prod_prom)) +
  geom_histogram(bins=15, fill=COL_VERDE, color="white", alpha=0.85) +
  geom_vline(xintercept=mean(gestantes$prod_prom,na.rm=T),
             linetype="dashed", color=COL_ROJO, linewidth=1) +
  annotate("text", x=mean(gestantes$prod_prom,na.rm=T)+1, y=Inf, vjust=2,
           label=paste0("Media=",round(mean(gestantes$prod_prom,na.rm=T),1),"kg"),
           color=COL_ROJO, size=3.5) +
  labs(title="Produccion de vacas gestantes", x="kg/24h", y="N vacas") + tema

grid.arrange(g1,g2,g3,g4, ncol=2, top=titulo("ANALISIS DE VACAS GESTANTES"))

# ============================================================
# SECCION 5: VACAS INSEMINADAS
# ============================================================

cat("\n========== VACAS INSEMINADAS ==========\n")
cat("Total inseminadas:", nrow(inseminadas), "\n")

i1 <- inseminadas %>% filter(!is.na(prod_prom)) %>%
  ggplot(aes(x=prod_prom)) +
  geom_histogram(bins=15, fill=COL_AZUL, color="white", alpha=0.85) +
  geom_vline(xintercept=mean(inseminadas$prod_prom,na.rm=T),
             linetype="dashed", color=COL_ROJO, linewidth=1) +
  annotate("text", x=mean(inseminadas$prod_prom,na.rm=T)+1, y=Inf, vjust=2,
           label=paste0("Media=",round(mean(inseminadas$prod_prom,na.rm=T),1),"kg"),
           color=COL_ROJO, size=3.5) +
  labs(title="Produccion de vacas inseminadas", x="kg/24h", y="N vacas") + tema

i2 <- inseminadas %>% filter(!is.na(dias_abiertos)) %>%
  ggplot(aes(x=dias_abiertos)) +
  geom_histogram(bins=15, fill=COL_AMBAR, color="white", alpha=0.85) +
  geom_vline(xintercept=mean(inseminadas$dias_abiertos,na.rm=T),
             linetype="dashed", color=COL_ROJO, linewidth=1) +
  labs(title="Dias abiertos al momento de inseminacion",
       x="Dias abiertos", y="N vacas") + tema

grid.arrange(i1,i2, ncol=2, top=titulo("ANALISIS DE VACAS INSEMINADAS"))

# ============================================================
# SECCION 6: VACAS PARA SECAR
# ============================================================

cat("\n========== VACAS PARA SECAR ==========\n")
cat("Total para secar:", nrow(para_secar), "\n")

if(nrow(para_secar) > 0 & sum(!is.na(para_secar$prod_prom)) > 0){
  s1 <- para_secar %>% filter(!is.na(prod_prom)) %>%
    ggplot(aes(x=prod_prom)) +
    geom_histogram(bins=12, fill=COL_GRIS, color="white", alpha=0.85) +
    labs(title="Produccion promedio al secado", x="kg/24h", y="N vacas") + tema
  
  s2 <- para_secar %>% filter(!is.na(status)) %>%
    count(status) %>% mutate(pct=n/sum(n)) %>%
    ggplot(aes(x=reorder(status,n), y=n, fill=status)) +
    geom_col(show.legend=FALSE) +
    geom_text(aes(label=paste0(n," (",percent(pct,1),")")), hjust=-0.1, size=3.5) +
    coord_flip() +
    scale_fill_brewer(palette="Pastel1") +
    labs(title="Status de vacas para secar", x="", y="N vacas") + tema
  
  grid.arrange(s1,s2, ncol=2, top=titulo("ANALISIS DE VACAS PARA SECAR"))
} else {
  cat("No hay datos suficientes para graficar vacas para secar\n")
}

# ============================================================
# SECCION 7: PROXIMAS AL PARTO
# ============================================================

cat("\n========== PROXIMAS AL PARTO ==========\n")
cat("Vacas proximas al parto:", nrow(proximas_parto), "\n")
cat("Vaquillas proximas al parto:", nrow(vaqi_parto), "\n")

pp1 <- proximas_parto %>% filter(!is.na(dias_falta)) %>%
  mutate(semana=case_when(
    dias_falta <= 7  ~ "Esta semana",
    dias_falta <= 14 ~ "2da semana",
    dias_falta <= 21 ~ "3ra semana",
    TRUE             ~ "4ta semana+")) %>%
  count(semana) %>%
  ggplot(aes(x=reorder(semana,n), y=n, fill=semana)) +
  geom_col(show.legend=FALSE) +
  geom_text(aes(label=n), hjust=-0.2, size=4) +
  coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(title="Vacas proximas al parto por semana",
       x="", y="N vacas") + tema

pp2 <- vaqi_parto %>% filter(!is.na(dias_falta)) %>%
  mutate(semana=case_when(
    dias_falta <= 7  ~ "Esta semana",
    dias_falta <= 14 ~ "2da semana",
    dias_falta <= 21 ~ "3ra semana",
    TRUE             ~ "4ta semana+")) %>%
  count(semana) %>%
  ggplot(aes(x=reorder(semana,n), y=n, fill=semana)) +
  geom_col(show.legend=FALSE) +
  geom_text(aes(label=n), hjust=-0.2, size=4) +
  coord_flip() +
  scale_fill_brewer(palette="Greens") +
  labs(title="Vaquillas proximas al parto por semana",
       x="", y="N vaquillas") + tema

grid.arrange(pp1,pp2, ncol=2, top=titulo("PROXIMAS AL PARTO"))

# ============================================================
# SECCION 8: VACAS SECAS
# ============================================================

cat("\n========== VACAS SECAS ==========\n")
cat("Total vacas secas:", nrow(vacas_secas), "\n")

if(nrow(vacas_secas) > 1 & sum(!is.na(vacas_secas$dias_seca)) > 1){
  ggplot(vacas_secas %>% filter(!is.na(dias_seca)), aes(x=dias_seca)) +
    geom_histogram(bins=10, fill="#78909C", color="white", alpha=0.85) +
    geom_vline(xintercept=mean(vacas_secas$dias_seca,na.rm=T),
               linetype="dashed", color=COL_ROJO, linewidth=1) +
    labs(title="VACAS SECAS - Dias en periodo seco",
         x="Dias secos", y="N vacas") + tema
}

# ============================================================
# SECCION 9: VAQUILLAS PARA TEST DE PRENEZ
# ============================================================

cat("\n========== VAQUILLAS PARA TEST DE PRENEZ ==========\n")
cat("Total:", nrow(vaqi_test), "\n")

if(nrow(vaqi_test) > 0 & sum(!is.na(vaqi_test$status)) > 0){
  vt1 <- vaqi_test %>% filter(!is.na(status)) %>%
    count(status) %>% mutate(pct=n/sum(n)) %>%
    ggplot(aes(x=reorder(status,n), y=n, fill=status)) +
    geom_col(show.legend=FALSE) +
    geom_text(aes(label=paste0(n," (",percent(pct,1),")")), hjust=-0.1, size=3.5) +
    coord_flip() +
    scale_fill_brewer(palette="Set3") +
    labs(title="Status reproductivo", x="", y="N vaquillas") + tema
  
  vt2 <- vaqi_test %>% filter(!is.na(edad)) %>%
    ggplot(aes(x=edad)) +
    geom_histogram(bins=10, fill="#00838F", color="white", alpha=0.85) +
    geom_vline(xintercept=mean(vaqi_test$edad,na.rm=T),
               linetype="dashed", color=COL_ROJO, linewidth=1) +
    labs(title="Edad de vaquillas", x="Edad (meses)", y="N vaquillas") + tema
  
  grid.arrange(vt1,vt2, ncol=2, top=titulo("VAQUILLAS PARA TEST DE PRENEZ"))
} else {
  cat("Sin datos suficientes\n")
}

cat("\n============================================================\n")
cat("ANALISIS COMPLETO FINALIZADO\n")
cat("Hojas analizadas: MASTITIS, PRODUCCION DE LECHE,\n")
cat("  VACAS GESTANTES, VACAS INSEMINADAS, VACAS PARA SECAR,\n")
cat("  VACAS PROXIMAS AL PARTO, VACAS SECAS,\n")
cat("  VAQUILLAS PROXIMAS AL PARTO, VAQUILLAS TEST PRENEZ\n")
cat("============================================================\n")

