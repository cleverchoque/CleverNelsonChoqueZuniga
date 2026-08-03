# ============================================================
#  DASHBOARD GANADERO — SHINY
#  Valores exactos del Excel
#
#  Instalar:
#    install.packages(c("shiny","readxl","dplyr","stringr","plotly"))
# ============================================================

library(shiny)
library(readxl)
library(dplyr)
library(stringr)
library(plotly)

EXCEL <- "D:/UNIVERCIDAD UNAP/CURSOS X SEMESTRE/ESTADISTICA ESPACIAL/UNIDAD I/datos_ganaderos_2026-04-20.xlsx"

# ============================================================
# CARGA Y PARSEO DE DATOS
# ============================================================

# MASTITIS — columna se llama N_Vaca
mastitis_raw <- read_excel(EXCEL, sheet="MASTITIS")
# Renombrar segun nombre real de columnas
colnames(mastitis_raw)[1] <- "n_vaca"
mastitis <- mastitis_raw %>%
  mutate(n_vaca = as.character(n_vaca),
         leche_24h = as.numeric(Leche_24h),
         lactacion_n = as.integer(Lactacion_N),
         lactacion_dias = as.integer(Lactacion_Dias),
         dias_enferma = suppressWarnings(as.numeric(Dias_Enferma)),
         cuadrante = case_when(
           Comentario1=="PI" ~ "Ant. Izquierdo",
           Comentario1=="PD" ~ "Ant. Derecho",
           Comentario1=="AI" ~ "Post. Izquierdo",
           Comentario1=="AD" ~ "Post. Derecho",
           TRUE ~ Comentario1))

# PRODUCCION DE LECHE
prod_raw <- read_excel(EXCEL, sheet="PRODUCCI\u00d3N DE LECHE", col_names=FALSE, skip=6)
prod_leche <- prod_raw %>%
  rename(col1=1,col2=2,col3=3,col4=4,col5=5) %>%
  filter(!is.na(col1), str_detect(as.character(col1),"^\\d{4}")) %>%
  mutate(
    n_vaca    = str_extract(as.character(col1),"^\\d{4}"),
    status_raw= as.character(col1),
    dias_lact = suppressWarnings(as.integer(as.character(col2))),
    prod_act  = as.numeric(str_extract(as.character(col4),"^[\\d.]+")),
    prod_prom = as.numeric(str_extract(as.character(col4),"[\\d.]+$")),
    prod_total= as.numeric(str_extract(as.character(col5),"\\d+$")),
    status = case_when(
      str_detect(status_raw,"Pre\u00f1") | str_detect(status_raw,"Pren") ~ "Prenada",
      str_detect(status_raw,"Insem") ~ "Insem",
      str_detect(status_raw,"Vacia") ~ "Vacia",
      str_detect(status_raw,"Matar") ~ "Matar",
      str_detect(status_raw,"Seca")  ~ "Seca",
      TRUE ~ "Otro")
  ) %>%
  select(n_vaca,status,dias_lact,prod_act,prod_prom,prod_total) %>%
  filter(!is.na(n_vaca),!is.na(prod_act))

# VACAS GESTANTES — datos en col1 formato "NNNN LL LC"
gest_raw <- read_excel(EXCEL, sheet="VACAS GESTANTES", col_names=FALSE, skip=4)
gestantes <- gest_raw %>%
  rename(col1=1,col2=2,col3=3,col4=4,col5=5,col6=6,col7=7) %>%
  filter(!is.na(col1), str_detect(as.character(col1),"^\\d{4}")) %>%
  mutate(
    n_vaca        = str_extract(as.character(col1),"^\\d{4}"),
    lactacion     = suppressWarnings(as.integer(str_extract(as.character(col1),"\\d+$"))),
    dias_lech     = suppressWarnings(as.integer(as.character(col3))),
    status        = str_extract(as.character(col4),"^\\w+"),
    n_ins         = suppressWarnings(as.integer(as.character(col5))),
    dias_abiertos = suppressWarnings(as.integer(as.character(col6))),
    prod_prom     = suppressWarnings(as.numeric(as.character(col7)))
  ) %>%
  select(n_vaca,lactacion,dias_lech,status,n_ins,dias_abiertos,prod_prom)

# VACAS INSEMINADAS
insem_raw <- read_excel(EXCEL, sheet="VACAS INSEMINADAS", col_names=FALSE, skip=5)
inseminadas <- insem_raw %>%
  rename(col1=1,col2=2,col3=3,col4=4) %>%
  filter(!is.na(col1), str_detect(as.character(col1),"^\\d{4}")) %>%
  mutate(
    n_vaca    = str_extract(as.character(col1),"^\\d{4}"),
    prod_prom = suppressWarnings(as.numeric(as.character(col3)))
  ) %>%
  select(n_vaca,prod_prom) %>% filter(!is.na(n_vaca))

# VACAS SECAS — datos en col2
secas_raw <- read_excel(EXCEL, sheet="VACAS SECAS", col_names=FALSE, skip=3)
vacas_secas <- secas_raw %>%
  rename(col1=1,col2=2) %>%
  filter(!is.na(col2), str_detect(as.character(col2),"^\\d{4}")) %>%
  mutate(
    n_vaca = str_extract(as.character(col2),"^\\d{4}"),
    status = case_when(
      str_detect(as.character(col2),"Prepa") ~ "Seca Prenada",
      str_detect(as.character(col2),"Seca")  ~ "Seca Vacia",
      TRUE ~ "Seca Prenada")
  ) %>%
  select(n_vaca,status) %>% filter(!is.na(n_vaca))

# ============================================================
# KPIs EXACTOS
# ============================================================
vacas_ordeno   <- nrow(prod_leche)                    # 230
vacas_secas_n  <- nrow(vacas_secas)                   # 16
vacas_total    <- vacas_ordeno + vacas_secas_n         # 246
vacas_prenadas <- nrow(gestantes)                      # 98
vacas_vacias   <- vacas_total - vacas_prenadas         # 148
pct_prenadas   <- round(vacas_prenadas/vacas_total*100,1)  # 39.8
pct_vacias     <- round(pct_vacias <- 100 - pct_prenadas,1)
pct_ordeno     <- round(vacas_ordeno/vacas_total*100,1)    # 93.5
pct_secas      <- round(100 - pct_ordeno,1)                # 6.5

del_prom   <- round(mean(gestantes$dias_lech, na.rm=TRUE),2)   # 407.66
lact_prom  <- round(mean(gestantes$lactacion, na.rm=TRUE),2)   # 1.72
prod_media <- round(mean(prod_leche$prod_act, na.rm=TRUE),1)   # 36.5
ins_n      <- nrow(inseminadas)                                 # 48

dias_ab_prom <- round(mean(gestantes$dias_abiertos, na.rm=TRUE),0)
serv_prom    <- round(mean(gestantes$n_ins, na.rm=TRUE),1)

mast_vacas    <- n_distinct(mastitis$n_vaca)   # 92
mast_episod   <- nrow(mastitis)                # 234
mast_por_vaca <- round(mast_episod/mast_vacas,1)  # 2.5

# Lactaciones (LC1/LC2/LC3) por numero de lactacion en gestantes
lc1 <- sum(gestantes$lactacion == 1, na.rm=TRUE)
lc2 <- sum(gestantes$lactacion == 2, na.rm=TRUE)
lc3 <- sum(gestantes$lactacion >= 3, na.rm=TRUE)
lc_total <- lc1 + lc2 + lc3

# Status de ordeno
ord_prenada <- sum(prod_leche$status == "Prenada", na.rm=TRUE)   # 82
ord_insem   <- sum(prod_leche$status == "Insem",   na.rm=TRUE)   # 48
ord_vacia   <- sum(prod_leche$status == "Vacia",   na.rm=TRUE)   # 30
ord_mas60   <- sum(prod_leche$dias_lact > 60, na.rm=TRUE)        # 230
ord_menos60 <- sum(prod_leche$dias_lact <= 60, na.rm=TRUE)       # 0

# Status de secas
seca_prenada <- sum(vacas_secas$status == "Seca Prenada")   # 8
seca_vacia   <- sum(vacas_secas$status == "Seca Vacia")     # 8
seca_insem   <- ins_n                                        # 48

# ============================================================
# FUNCION TORTA PLOTLY
# ============================================================
hacer_torta <- function(vals, labs, cols, hole=0) {
  df <- data.frame(label=labs, valor=as.numeric(vals)) %>%
    filter(valor > 0) %>%
    mutate(pct=round(valor/sum(valor)*100,1))
  
  plot_ly(df,
          labels = ~label,
          values = ~valor,
          type   = "pie",
          hole   = hole,
          textinfo         = "label+value+percent",
          textposition     = "inside",
          insidetextorientation = "radial",
          marker = list(
            colors = cols[seq_len(nrow(df))],
            line   = list(color="white", width=2)
          ),
          hovertemplate = "%{label}<br>%{value} (%{percent})<extra></extra>",
          showlegend    = TRUE) %>%
    layout(
      legend  = list(orientation="h", x=0.5, xanchor="center",
                     y=-0.18, font=list(size=9)),
      margin  = list(t=5, b=5, l=5, r=5),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)"
    )
}

# ============================================================
# UI
# ============================================================
ui <- fluidPage(
  tags$head(
    tags$link(href="https://fonts.googleapis.com/css2?family=Inter:wght@300;400;600;700&display=swap",
              rel="stylesheet"),
    tags$style(HTML("
      * { box-sizing:border-box; margin:0; padding:0; }
      body { background:#f0f2f5; font-family:'Inter',sans-serif; font-size:13px; color:#1a1a2e; }

      .topbar {
        background:#1c2b4a; color:white; padding:10px 24px;
        display:flex; align-items:center; justify-content:space-between;
        border-bottom:3px solid #FFC107;
      }
      .topbar-title { font-size:1rem; font-weight:700; }
      .topbar-sub   { font-size:0.65rem; color:#FFC107; letter-spacing:1px; text-transform:uppercase; }
      .topbar-right { font-size:0.7rem; color:#aaa; text-align:right; line-height:1.6; }

      .filtros {
        background:white; padding:8px 24px; display:flex; gap:16px;
        align-items:center; border-bottom:1px solid #e8e8e8; font-size:0.78rem;
      }
      .f-label { color:#888; font-size:0.62rem; text-transform:uppercase; letter-spacing:1px; }
      .f-val   { background:#f4f4f4; border:1px solid #ddd; border-radius:4px;
                 padding:3px 10px; font-weight:600; color:#1c2b4a; }

      .grid3 {
        display:grid; grid-template-columns:1fr 1fr 1fr;
        gap:12px; padding:14px 20px 0;
      }
      .card {
        background:white; border-radius:8px; padding:12px 14px 10px;
        box-shadow:0 1px 3px rgba(0,0,0,0.07); border:1px solid #eee;
      }
      .card-lbl {
        font-size:0.58rem; font-weight:700; text-transform:uppercase;
        letter-spacing:1.2px; color:#888; margin-bottom:4px;
      }
      .card-info {
        font-size:0.72rem; color:#555; line-height:1.5;
        margin-top:6px; padding-top:6px; border-top:1px solid #f0f0f0;
      }

      .grid3-kpi {
        display:grid; grid-template-columns:1fr 1fr 1fr;
        gap:12px; padding:12px 20px 20px;
      }

      /* DEL */
      .kpi-del {
        background:white; border-radius:8px; padding:24px 16px;
        text-align:center; box-shadow:0 1px 3px rgba(0,0,0,0.07);
        border:1px solid #eee; display:flex; flex-direction:column;
        align-items:center; justify-content:center;
      }
      .kpi-del .num { font-size:3rem; font-weight:700; color:#1565C0; line-height:1; }
      .kpi-del .lbl { font-size:0.62rem; font-weight:700; text-transform:uppercase;
                      letter-spacing:1.5px; color:#1565C0; margin-top:6px; }

      /* LACTACION */
      .kpi-lact {
        background:white; border-radius:8px; padding:16px;
        box-shadow:0 1px 3px rgba(0,0,0,0.07); border:1px solid #eee;
      }
      .kpi-lact .titulo { font-size:0.62rem; font-weight:700; text-transform:uppercase;
                          letter-spacing:1px; color:#888; text-align:center; margin-bottom:6px; }
      .kpi-lact .num-grande { font-size:2.8rem; font-weight:700; color:#1a1a2e;
                               text-align:center; line-height:1; }
      .separador { height:1px; background:#eee; margin:10px 0; }
      .mini-grid { display:grid; grid-template-columns:1fr 1fr; gap:8px; }
      .mini-chip { background:#f7f8fa; border-radius:6px; padding:8px 10px; text-align:center; }
      .mini-chip .mv { font-size:1.2rem; font-weight:700; color:#1c2b4a; line-height:1; }
      .mini-chip .ml { font-size:0.55rem; text-transform:uppercase; letter-spacing:0.8px;
                       color:#888; margin-top:3px; }

      /* SANIDAD */
      .kpi-sanidad {
        background:white; border-radius:8px; padding:16px;
        box-shadow:0 1px 3px rgba(0,0,0,0.07); border:1px solid #eee;
      }
      .sanidad-titulo { font-size:0.62rem; font-weight:700; text-transform:uppercase;
                        letter-spacing:1px; color:#888; margin-bottom:10px; }
      .san-grid { display:grid; grid-template-columns:1fr 1fr; gap:8px; }
      .san-chip { background:#f7f8fa; border-radius:6px; padding:8px 10px; text-align:center; }
      .san-chip .sv { font-size:1.3rem; font-weight:700; color:#1c2b4a; line-height:1; }
      .san-chip .sl { font-size:0.54rem; text-transform:uppercase; letter-spacing:0.7px;
                      color:#888; margin-top:3px; }
    "))
  ),
  
  div(class="topbar",
      div(
        div(class="topbar-title","Dashboard Ganadero — Establo Sausalito"),
        div(class="topbar-sub","Monitoreo del hato")
      ),
      div(class="topbar-right",
          div("CODIGO: 7PULILI"),
          div(paste0("Fecha: ",format(Sys.Date(),"%d/%m/%Y")))
      )
  ),
  
  div(class="filtros",
      div(div(class="f-label","Periodo"),
          div(class="f-val",paste0(format(Sys.Date(),"%Y")," — ",format(Sys.Date(),"%B")))),
      div(div(class="f-label","Codigo"), div(class="f-val","7PULILI")),
      div(div(class="f-label","Establo"), div(class="f-val","Sausalito"))
  ),
  
  # FILA 1
  div(class="grid3",
      div(class="card",
          div(class="card-lbl","Estado Reproductivo"),
          plotlyOutput("t1", height="210px"),
          div(class="card-info",
              paste0("De un total de ",vacas_total," vacas, ",vacas_prenadas,
                     " estan prenadas (",pct_prenadas,"% del rebano)."))
      ),
      div(class="card",
          div(class="card-lbl","Vacas en Ordeno vs Secas"),
          plotlyOutput("t2", height="210px"),
          div(class="card-info",
              paste0(vacas_ordeno," en produccion (",pct_ordeno,
                     "%), ",vacas_secas_n," secas (",pct_secas,"%)."))
      ),
      div(class="card",
          div(class="card-lbl","Distribucion por Lactacion"),
          plotlyOutput("t3", height="210px"),
          div(class="card-info",
              paste0("LC1: ",lc1," (",round(lc1/lc_total*100,1),
                     "%) | LC2: ",lc2," (",round(lc2/lc_total*100,1),
                     "%) | LC3+: ",lc3," (",round(lc3/lc_total*100,1),"%)"))
      )
  ),
  
  # FILA 2
  div(class="grid3",style="padding-top:12px;",
      div(class="card",
          div(class="card-lbl","Vacas Total vs Recria"),
          plotlyOutput("t4", height="210px"),
          div(class="card-info",
              paste0("Produccion promedio: ",prod_media," kg/24h."))
      ),
      div(class="card",
          div(class="card-lbl","Status de Vacas en Ordeno"),
          plotlyOutput("t5", height="210px")
      ),
      div(class="card",
          div(class="card-lbl","Status de Vacas Secas"),
          plotlyOutput("t6", height="210px")
      )
  ),
  
  # FILA 3 — KPIs
  div(class="grid3-kpi",
      
      div(class="kpi-del",
          div(class="num", del_prom),
          div(class="lbl","DEL Promedio")
      ),
      
      div(class="kpi-lact",
          div(class="titulo","Lactacion Promedio"),
          div(class="num-grande", lact_prom),
          div(class="separador"),
          div(class="mini-grid",
              div(class="mini-chip",
                  div(class="mv",vacas_ordeno), div(class="ml","Vacas Ordeno")),
              div(class="mini-chip",
                  div(class="mv",vacas_secas_n), div(class="ml","Vacas Secas")),
              div(class="mini-chip",
                  div(class="mv",vacas_prenadas), div(class="ml","Prenadas")),
              div(class="mini-chip",
                  div(class="mv",ins_n), div(class="ml","Inseminadas"))
          )
      ),
      
      div(class="kpi-sanidad",
          div(class="sanidad-titulo","Resumen Sanidad"),
          div(class="san-grid",
              div(class="san-chip",
                  div(class="sv",mast_vacas), div(class="sl","Vacas c/ Mastitis")),
              div(class="san-chip",
                  div(class="sv",mast_episod), div(class="sl","Episodios")),
              div(class="san-chip",
                  div(class="sv",mast_por_vaca), div(class="sl","Episod./Vaca")),
              div(class="san-chip",
                  div(class="sv",prod_media), div(class="sl","kg/24h Prom")),
              div(class="san-chip",
                  div(class="sv",dias_ab_prom), div(class="sl","Dias Abiertos")),
              div(class="san-chip",
                  div(class="sv",serv_prom), div(class="sl","Servicios Prom"))
          )
      )
  )
)

# ============================================================
# SERVER
# ============================================================
server <- function(input, output, session) {
  
  output$t1 <- renderPlotly({
    hacer_torta(
      vals = c(vacas_prenadas, vacas_vacias),
      labs = c(paste0("VACAPRENA\n",vacas_prenadas,"\n",pct_prenadas,"%"),
               paste0("VACA VACIA\n",vacas_vacias,"\n",pct_vacias,"%")),
      cols = c("#EF6C00","#FFC107")
    )
  })
  
  output$t2 <- renderPlotly({
    hacer_torta(
      vals = c(vacas_ordeno, vacas_secas_n),
      labs = c(paste0("VACAS ORDENO\n",vacas_ordeno,"\n",pct_ordeno,"%"),
               paste0("VACAS SECA\n",vacas_secas_n,"\n",pct_secas,"%")),
      cols = c("#2E7D32","#1565C0")
    )
  })
  
  output$t3 <- renderPlotly({
    hacer_torta(
      vals = c(lc1, lc2, lc3),
      labs = c(paste0("LC1\n",lc1,"\n",round(lc1/lc_total*100,1),"%"),
               paste0("LC2\n",lc2,"\n",round(lc2/lc_total*100,1),"%"),
               paste0("LC 3M\n",lc3,"\n",round(lc3/lc_total*100,1),"%")),
      cols = c("#42A5F5","#1565C0","#EF6C00"),
      hole = 0.35
    )
  })
  
  output$t4 <- renderPlotly({
    hacer_torta(
      vals = c(vacas_ordeno, vacas_secas_n),
      labs = c(paste0("VACAS TOTAL\n",vacas_ordeno,"\n",pct_ordeno,"%"),
               paste0("RECRIA TOTAL\n",vacas_secas_n,"\n",pct_secas,"%")),
      cols = c("#1565C0","#42A5F5")
    )
  })
  
  output$t5 <- renderPlotly({
    total_ord <- ord_mas60 + ord_prenada + ord_insem
    hacer_torta(
      vals = c(ord_mas60, ord_prenada, ord_insem),
      labs = c(paste0("Ordeno>60DEL\n",ord_mas60,"\n",round(ord_mas60/total_ord*100,1),"%"),
               paste0("Prenada\n",ord_prenada,"\n",round(ord_prenada/total_ord*100,1),"%"),
               paste0("Insem\n",ord_insem,"\n",round(ord_insem/total_ord*100,1),"%")),
      cols = c("#EF6C00","#EC407A","#1565C0")
    )
  })
  
  output$t6 <- renderPlotly({
    total_sec <- seca_prenada + seca_insem + pmax(seca_vacia,1)
    hacer_torta(
      vals = c(seca_prenada, seca_insem, pmax(seca_vacia,1)),
      labs = c(paste0("Seca Prenada\n",seca_prenada,"\n",round(seca_prenada/total_sec*100,1),"%"),
               paste0("Seca Insem\n",seca_insem,"\n",round(seca_insem/total_sec*100,1),"%"),
               paste0("Seca Vacia\n",pmax(seca_vacia,1),"\n",round(pmax(seca_vacia,1)/total_sec*100,1),"%")),
      cols = c("#42A5F5","#1565C0","#EF6C00")
    )
  })
}

shinyApp(ui, server)}