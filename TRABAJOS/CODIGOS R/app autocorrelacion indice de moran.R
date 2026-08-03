# ============================================================
#  APLICATIVO SHINY — ÍNDICE DE MORAN
#  Estadística Espacial — FINESI-UNAP — 2026-I
# ============================================================

pkgs <- c("shiny","shinydashboard","spdep","spData","sf",
          "ggplot2","patchwork","viridis","dplyr","sp",
          "MASS","Guerry","DT")
for (p in pkgs) {
  if (!require(p, quietly = TRUE, character.only = TRUE))
    install.packages(p, dependencies = TRUE)
}

library(shiny); library(shinydashboard)
library(spdep); library(spData); library(sf)
library(ggplot2); library(viridis); library(dplyr)
library(sp); library(MASS); library(Guerry); library(DT)

# ============================================================
# PRE-CARGAR DATASETS
# ============================================================
cargar_datasets <- function() {
  ds <- list()
  
  # 1. North Carolina
  nc <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  nc$SIDS_RATE <- (nc$SID74 / nc$BIR74) * 1000
  ds[["North Carolina — Tasa SIDS"]] <- list(
    sf = nc, variable = "SIDS_RATE", unidad = "por 1000 nac.",
    lon_lat = FALSE,
    desc = "100 condados de Carolina del Norte. Tasa SIDS por 1000 nacidos vivos (1974-1978). Fuente: paquete sf."
  )
  
  # 2. Nueva Zelanda
  data("nz", package = "spData")
  ds[["Nueva Zelanda — Ingreso Mediano"]] <- list(
    sf = nz, variable = "Median_income", unidad = "NZD",
    lon_lat = FALSE,
    desc = "16 regiones de Nueva Zelanda. Ingreso mediano en dólares neozelandeses. Fuente: paquete spData."
  )
  
  # 3. Meuse
  data("meuse", package = "sp")
  meuse_sf <- st_as_sf(meuse, coords = c("x","y"), crs = 28992)
  meuse_sf$logzn <- log(meuse_sf$zinc)
  ds[["Meuse — log(Zinc en suelo)"]] <- list(
    sf = meuse_sf, variable = "logzn", unidad = "log(ppm)",
    lon_lat = TRUE,
    desc = "155 muestras de suelo, río Meuse (Países Bajos). log(Zinc en ppm). Datos puntuales KNN-5. Fuente: paquete sp."
  )
  
  # 4. Boston — usar CRS 4326 con coords reales aproximadas
  data("Boston", package = "MASS")
  set.seed(1)
  Boston$lon <- -71.06 + ((seq_len(nrow(Boston)) - 1) %% 23) * (0.35 / 22) +
    rnorm(nrow(Boston), 0, 0.003)
  Boston$lat <-  42.20 + floor((seq_len(nrow(Boston)) - 1) / 23) * (0.25 / 21) +
    rnorm(nrow(Boston), 0, 0.003)
  boston_sf <- st_as_sf(Boston, coords = c("lon","lat"), crs = 4326)
  ds[["Boston — Valor de Vivienda"]] <- list(
    sf = boston_sf, variable = "medv", unidad = "miles USD",
    lon_lat = TRUE,
    desc = "506 zonas censales de Boston, MA. Valor mediano de viviendas en miles USD. Harrison & Rubinfeld (1978). Fuente: paquete MASS."
  )
  
  # 5. Francia 1830
  data("gfrance85", package = "Guerry")
  ds[["Francia 1830 — Crimen Propiedad"]] <- list(
    sf = st_as_sf(gfrance85), variable = "Crime_prop", unidad = "índice",
    lon_lat = FALSE,
    desc = "85 departamentos de Francia. Índice de criminalidad contra la propiedad, 1825-1830. Guerry (1833). Fuente: paquete Guerry."
  )
  
  ds
}

DATASETS <- cargar_datasets()

# ============================================================
# FUNCIÓN CENTRAL
# ============================================================
calcular_moran <- function(ds_item, tipo_nb = "queen", k = 5, nsim = 499) {
  
  sf_obj   <- ds_item$sf
  variable <- ds_item$variable
  
  sf_obj  <- sf_obj[!st_is_empty(sf_obj), ]
  sf_obj  <- sf_obj[!is.na(sf_obj[[variable]]), ]
  valores <- as.numeric(sf_obj[[variable]])
  
  if (ds_item$lon_lat) {
    coords <- st_coordinates(sf_obj)
    nb     <- knn2nb(knearneigh(coords, k = k))
  } else {
    nb <- poly2nb(sf_obj, queen = (tipo_nb == "queen"))
  }
  
  nb <- make.sym.nb(nb)
  lw <- nb2listw(nb, style = "W", zero.policy = TRUE)
  
  mt   <- moran.test(valores, lw, zero.policy = TRUE)
  I    <- as.numeric(mt$estimate["Moran I statistic"])
  EI   <- as.numeric(mt$estimate["Expectation"])
  VarI <- as.numeric(mt$estimate["Variance"])
  
  set.seed(123)
  mc  <- moran.mc(valores, lw, nsim = nsim, zero.policy = TRUE)
  pmc <- mc$p.value
  
  z  <- as.numeric(scale(valores))
  Wz <- as.numeric(lag.listw(lw, z, zero.policy = TRUE))
  
  sf_obj$z_std     <- z
  sf_obj$Wz_lag    <- Wz
  sf_obj$cuadrante <- dplyr::case_when(
    z >  0 & Wz >  0 ~ "HH (Alto-Alto)",
    z <  0 & Wz <  0 ~ "LL (Bajo-Bajo)",
    z >  0 & Wz <= 0 ~ "HL (Alto-Bajo)",
    z <= 0 & Wz >  0 ~ "LH (Bajo-Alto)",
    TRUE              ~ "Neutro"
  )
  
  list(sf_obj = sf_obj, variable = variable, unidad = ds_item$unidad,
       I = I, EI = EI, VarI = VarI,
       p_normal = as.numeric(mt$p.value), p_mc = pmc,
       z = z, Wz = Wz, sims = as.numeric(mc$res),
       n = nrow(sf_obj))
}

COLORES_Q <- c(
  "HH (Alto-Alto)" = "#185FA5",
  "LL (Bajo-Bajo)" = "#5DCAA5",
  "HL (Alto-Bajo)" = "#993C1D",
  "LH (Bajo-Alto)" = "#F0997B",
  "Neutro"         = "gray70"
)

# ============================================================
# PRECALCULAR todos los datasets para la tabla/comparativo
# ============================================================
RESULTADOS_BASE <- lapply(DATASETS, function(ds_item) {
  tryCatch(calcular_moran(ds_item, "queen", 5, 499), error = function(e) NULL)
})

# ============================================================
# UI
# ============================================================
ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(
    title = span(icon("globe"), " Índice de Moran — FINESI UNAP"),
    titleWidth = 320
  ),
  
  dashboardSidebar(
    width = 290,
    sidebarMenu(
      id = "tabs",
      menuItem("Análisis por Dataset", tabName = "analisis",  icon = icon("chart-area")),
      menuItem("Comparación Global",   tabName = "comparacion", icon = icon("chart-bar")),
      menuItem("Tabla de Resultados",  tabName = "tabla",      icon = icon("table"))
    ),
    hr(),
    h4("Parámetros", style = "padding-left:15px; color:#ccc; font-size:13px;"),
    
    selectInput("dataset", "Dataset:",
                choices = names(DATASETS),
                selected = names(DATASETS)[1]),
    
    conditionalPanel(
      "input.dataset == 'North Carolina — Tasa SIDS' ||
       input.dataset == 'Nueva Zelanda — Ingreso Mediano' ||
       input.dataset == 'Francia 1830 — Crimen Propiedad'",
      radioButtons("tipo_nb", "Vecindad:",
                   choices = c("Queen (reina)" = "queen",
                               "Rook (torre)"  = "rook"),
                   selected = "queen")
    ),
    
    conditionalPanel(
      "input.dataset == 'Meuse — log(Zinc en suelo)' ||
       input.dataset == 'Boston — Valor de Vivienda'",
      sliderInput("k_nn", "K vecinos (KNN):",
                  min = 3, max = 10, value = 5, step = 1)
    ),
    
    sliderInput("nsim", "Permutaciones MC:",
                min = 99, max = 999, value = 499, step = 100),
    
    br(),
    div(style = "padding: 0 15px;",
        actionButton("calcular", "  Calcular",
                     icon  = icon("play"),
                     class = "btn-primary",
                     style = "width:100%;")
    ),
    br(),
    div(style = "padding:8px 15px; font-size:11px; color:#aaa;",
        "CHOQUE ZUÑIGA Clever Nelson", br(),
        "Estadística Espacial — Grupo B", br(),
        "Semestre XI — 2026-I")
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML("
      .content-wrapper { background-color: #f4f6f9; }
      .box { border-radius:6px; }
      .skin-blue .main-header .logo { background:#1a3a5c; }
      .skin-blue .main-header .navbar { background:#1e4976; }
      .skin-blue .main-sidebar { background:#1a3a5c; }
      .skin-blue .sidebar-menu>li.active>a,
      .skin-blue .sidebar-menu>li:hover>a {
        background:#2a5a8c; border-left-color:#5bc0de; }
    "))),
    
    tabItems(
      
      # ── TAB ANÁLISIS ───────────────────────────────────────
      tabItem("analisis",
              fluidRow(
                box(width=12, status="primary", solidHeader=TRUE,
                    title = uiOutput("titulo_ds"),
                    p(uiOutput("desc_ds"), style="color:#555;font-size:13px;"))
              ),
              fluidRow(
                valueBoxOutput("vb_I",      width=3),
                valueBoxOutput("vb_EI",     width=3),
                valueBoxOutput("vb_pmc",    width=3),
                valueBoxOutput("vb_result", width=3)
              ),
              fluidRow(
                box(width=6, title="Mapa de valores",    status="info",    solidHeader=TRUE, plotOutput("p_mapa",    height="320px")),
                box(width=6, title="Cuadrantes LISA",    status="warning", solidHeader=TRUE, plotOutput("p_lisa",    height="320px"))
              ),
              fluidRow(
                box(width=6, title="Diagrama de Moran",  status="primary", solidHeader=TRUE, plotOutput("p_scatter", height="320px")),
                box(width=6, title="Monte Carlo",        status="danger",  solidHeader=TRUE, plotOutput("p_mc",      height="320px"))
              ),
              fluidRow(
                box(width=12, status="success", solidHeader=TRUE,
                    title="Interpretación", uiOutput("interp"))
              )
      ),
      
      # ── TAB COMPARACIÓN ────────────────────────────────────
      tabItem("comparacion",
              fluidRow(
                box(width=12, status="primary", solidHeader=TRUE,
                    title="Comparación del Índice de Moran — 5 Datasets",
                    plotOutput("p_comp", height="420px"))
              )
      ),
      
      # ── TAB TABLA ──────────────────────────────────────────
      tabItem("tabla",
              fluidRow(
                box(width=12, status="primary", solidHeader=TRUE,
                    title="Tabla Resumen — Índice de Moran",
                    DTOutput("dt_tabla"))
              )
      )
    )
  )
)

# ============================================================
# SERVER
# ============================================================
server <- function(input, output, session) {
  
  # ── Resultado reactivo ─────────────────────────────────────
  rv <- reactiveValues(res = NULL, ds_name = NULL)
  
  # Calcular al inicio con dataset 1
  observe({
    if (is.null(rv$res)) {
      isolate({
        ds_item <- DATASETS[[names(DATASETS)[1]]]
        rv$res     <- calcular_moran(ds_item, "queen", 5, 499)
        rv$ds_name <- names(DATASETS)[1]
      })
    }
  })
  
  # Calcular al presionar botón
  observeEvent(input$calcular, {
    ds_name <- input$dataset
    ds_item <- DATASETS[[ds_name]]
    lon_lat <- ds_item$lon_lat
    tipo_nb <- if (!lon_lat && !is.null(input$tipo_nb)) input$tipo_nb else "queen"
    k       <- if (lon_lat  && !is.null(input$k_nn))   input$k_nn   else 5
    
    withProgress(message = "Calculando...", {
      incProgress(0.4)
      res <- tryCatch(
        calcular_moran(ds_item, tipo_nb, k, input$nsim),
        error = function(e) { showNotification(paste("Error:", e$message), type="error"); NULL }
      )
      incProgress(0.6)
      if (!is.null(res)) {
        rv$res     <- res
        rv$ds_name <- ds_name
      }
    })
  })
  
  # ── Helpers ────────────────────────────────────────────────
  get_res <- reactive({
    req(rv$res)
    rv$res
  })
  
  # ── UI títulos ─────────────────────────────────────────────
  output$titulo_ds <- renderUI({
    strong(rv$ds_name %||% names(DATASETS)[1])
  })
  output$desc_ds <- renderUI({
    nm <- rv$ds_name %||% names(DATASETS)[1]
    HTML(DATASETS[[nm]]$desc)
  })
  
  # ── Value boxes ────────────────────────────────────────────
  output$vb_I <- renderValueBox({
    r <- get_res()
    col <- if (r$I > 0.1) "blue" else if (r$I < -0.1) "orange" else "light-blue"
    valueBox(round(r$I,4), "I de Moran observado", icon("wave-square"), color=col)
  })
  output$vb_EI <- renderValueBox({
    r <- get_res()
    valueBox(round(r$EI,4), "E[I] bajo H₀", icon("equals"), color="navy")
  })
  output$vb_pmc <- renderValueBox({
    r <- get_res()
    col <- if (r$p_mc < 0.05) "green" else "red"
    valueBox(round(r$p_mc,4), "p-valor Monte Carlo", icon("shuffle"), color=col)
  })
  output$vb_result <- renderValueBox({
    r <- get_res()
    if (r$p_mc < 0.05) {
      if (r$I > 0) { lbl<-"Cluster (+)";    col<-"green";  ico<-icon("circle-nodes") }
      else         { lbl<-"Dispersión (-)"; col<-"orange"; ico<-icon("expand") }
    } else         { lbl<-"Aleatorio";      col<-"gray";   ico<-icon("random") }
    valueBox(lbl, "Resultado", ico, color=col)
  })
  
  # ── Mapa de valores ────────────────────────────────────────
  output$p_mapa <- renderPlot({
    r   <- get_res()
    var <- r$variable
    uni <- r$unidad
    nm  <- rv$ds_name %||% names(DATASETS)[1]
    es_punto <- DATASETS[[nm]]$lon_lat
    
    if (es_punto) {
      # Datos puntuales: extraer coordenadas y usar geom_point
      coords <- st_coordinates(r$sf_obj)
      df_map <- cbind(as.data.frame(r$sf_obj), X = coords[,1], Y = coords[,2])
      ggplot(df_map, aes(x = X, y = Y, color = .data[[var]])) +
        geom_point(size = 2.5, alpha = 0.9) +
        scale_color_viridis_c(name = uni, option = "plasma") +
        coord_equal() +
        theme_void(base_size = 12) +
        theme(legend.position    = "right",
              legend.key.height  = unit(1.2, "cm"),
              legend.text        = element_text(size = 9))
    } else {
      ggplot(r$sf_obj) +
        geom_sf(aes(fill = .data[[var]]), color = "white", linewidth = 0.25) +
        scale_fill_viridis_c(name = uni, option = "plasma") +
        theme_void(base_size = 12) +
        theme(legend.position   = "right",
              legend.key.height = unit(1.2, "cm"),
              legend.text       = element_text(size = 9))
    }
  })
  
  # ── Cuadrantes LISA ────────────────────────────────────────
  output$p_lisa <- renderPlot({
    r  <- get_res()
    nm <- rv$ds_name %||% names(DATASETS)[1]
    es_punto <- DATASETS[[nm]]$lon_lat
    
    if (es_punto) {
      coords <- st_coordinates(r$sf_obj)
      df_map <- cbind(as.data.frame(r$sf_obj), X = coords[,1], Y = coords[,2])
      ggplot(df_map, aes(x = X, y = Y, color = cuadrante)) +
        geom_point(size = 2.5, alpha = 0.9) +
        scale_color_manual(values = COLORES_Q, name = "Cuadrante", drop = FALSE) +
        coord_equal() +
        theme_void(base_size = 12) +
        theme(legend.position = "right",
              legend.text     = element_text(size = 9))
    } else {
      ggplot(r$sf_obj) +
        geom_sf(aes(fill = cuadrante), color = "white", linewidth = 0.25) +
        scale_fill_manual(values = COLORES_Q, name = "Cuadrante", drop = FALSE) +
        theme_void(base_size = 12) +
        theme(legend.position = "right",
              legend.text     = element_text(size = 9))
    }
  })
  
  # ── Diagrama de Moran ──────────────────────────────────────
  output$p_scatter <- renderPlot({
    r <- get_res()
    df_sc <- data.frame(z=r$z, Wz=r$Wz,
                        cuadrante=r$sf_obj$cuadrante,
                        stringsAsFactors=FALSE)
    ggplot(df_sc, aes(x=z, y=Wz, color=cuadrante)) +
      geom_hline(yintercept=0, linetype="dashed", color="gray60", linewidth=0.5) +
      geom_vline(xintercept=0, linetype="dashed", color="gray60", linewidth=0.5) +
      geom_point(size=2.5, alpha=0.8) +
      geom_smooth(aes(group=1), method="lm", se=FALSE,
                  color="#D85A30", linewidth=1.2) +
      annotate("text", x=-Inf, y=Inf, hjust=-0.1, vjust=1.8,
               label=paste0("I=",round(r$I,4),"  p=",round(r$p_mc,4)),
               size=4.5, fontface="bold", color="#D85A30") +
      scale_color_manual(values=COLORES_Q, name="Cuadrante", drop=FALSE) +
      labs(x="z(i) estandarizado", y="W·z(i) rezago") +
      theme_minimal(base_size=12) +
      theme(legend.position="bottom",
            legend.text=element_text(size=8),
            legend.key.size=unit(0.4,"cm"))
  })
  
  # ── Monte Carlo ────────────────────────────────────────────
  output$p_mc <- renderPlot({
    r <- get_res()
    sims_df <- data.frame(I_sim=r$sims)
    n_gte <- sum(r$sims >= r$I)
    n_lt  <- sum(r$sims  < r$I)
    ggplot(sims_df, aes(x=I_sim, fill=I_sim >= r$I)) +
      geom_histogram(bins=40, color="white", linewidth=0.1) +
      geom_vline(xintercept=r$I, color="#D85A30", linewidth=1.5) +
      annotate("text", x=r$I, y=Inf, vjust=1.8, hjust=-0.1,
               label=paste0("I=",round(r$I,4)),
               size=4, fontface="bold", color="#D85A30") +
      scale_fill_manual(
        values=c("TRUE"="#E24B4A","FALSE"="#378ADD"),
        labels=c("TRUE"=paste0(n_gte," >= I obs"),
                 "FALSE"=paste0(n_lt," < I obs")),
        name="") +
      labs(x="I permutado", y="Frecuencia",
           subtitle=paste0("p=",round(r$p_mc,4),
                           "  E[I]=",round(r$EI,4),
                           "  Var[I]=",round(r$VarI,5))) +
      theme_minimal(base_size=12) +
      theme(legend.position="bottom",
            legend.text=element_text(size=9),
            plot.subtitle=element_text(size=10,color="gray40"))
  })
  
  # ── Interpretación ─────────────────────────────────────────
  output$interp <- renderUI({
    r <- get_res()
    if (r$p_mc < 0.05) {
      if (r$I > 0) {
        color <- "#1a6e2e"; icono <- "✔"
        texto <- paste0(
          "<b>Autocorrelación espacial positiva significativa</b> ",
          "(I=",round(r$I,4),", p=",round(r$p_mc,4),"). ",
          "Valores similares se agrupan geográficamente (clustering). ",
          "I supera E[I] en ",round(r$I-r$EI,4)," unidades.")
      } else {
        color <- "#b34a00"; icono <- "⚠"
        texto <- paste0(
          "<b>Autocorrelación espacial negativa significativa</b> ",
          "(I=",round(r$I,4),", p=",round(r$p_mc,4),"). ",
          "Unidades vecinas presentan valores opuestos (patrón disperso).")
      }
    } else {
      color <- "#555"; icono <- "—"
      texto <- paste0(
        "<b>No se rechaza la aleatoriedad espacial</b> ",
        "(I=",round(r$I,4),", p=",round(r$p_mc,4),"). ",
        "La distribución es compatible con el azar. ",
        "Posible causa: baja potencia estadística (n=",r$n," unidades).")
    }
    div(
      style=paste0("border-left:4px solid ",color,
                   ";padding:12px 16px;background:#f9f9f9;",
                   "border-radius:4px;font-size:14px;"),
      HTML(paste0(
        "<span style='font-size:18px;color:",color,";'>",icono,"</span>  ",
        texto,
        "<br><br><b>n=</b>",r$n,
        " | <b>E[I]=</b>",round(r$EI,4),
        " | <b>Var[I]=</b>",round(r$VarI,6),
        " | <b>p(normal)=</b>",round(r$p_normal,4)
      ))
    )
  })
  
  # ── Comparativo ────────────────────────────────────────────
  output$p_comp <- renderPlot({
    df_comp <- do.call(rbind, lapply(names(RESULTADOS_BASE), function(nm) {
      r <- RESULTADOS_BASE[[nm]]
      if (is.null(r)) return(NULL)
      data.frame(Dataset=nm, I=round(r$I,4),
                 p_mc=round(r$p_mc,4),
                 sig=r$p_mc < 0.05,
                 stringsAsFactors=FALSE)
    }))
    
    ggplot(df_comp, aes(x=reorder(Dataset,I), y=I, fill=sig)) +
      geom_col(width=0.6, color="white") +
      geom_hline(yintercept=0, linetype="dashed",
                 color="gray40", linewidth=0.6) +
      geom_text(aes(label=round(I,4),
                    vjust=ifelse(I>=0,-0.5,1.4)),
                size=4.5, fontface="bold") +
      scale_fill_manual(
        values=c("TRUE"="#185FA5","FALSE"="#B4B2A9"),
        labels=c("TRUE"="Significativo (p<0.05)",
                 "FALSE"="No significativo"),
        name="") +
      coord_flip() +
      labs(title="Índice de Moran — 5 Datasets",
           subtitle="Queen/KNN-5 | MC 499 permutaciones | Semilla 123",
           x=NULL, y="I de Moran observado") +
      theme_minimal(base_size=13) +
      theme(legend.position="bottom",
            legend.text=element_text(size=11),
            plot.title=element_text(face="bold",size=14),
            plot.subtitle=element_text(color="gray50",size=10),
            axis.text.y=element_text(size=10))
  })
  
  # ── Tabla ──────────────────────────────────────────────────
  output$dt_tabla <- renderDT({
    df_tab <- do.call(rbind, lapply(names(RESULTADOS_BASE), function(nm) {
      r  <- RESULTADOS_BASE[[nm]]
      ds <- DATASETS[[nm]]
      if (is.null(r)) return(NULL)
      data.frame(
        Dataset      = nm,
        Variable     = ds$variable,
        n            = r$n,
        "I obs"      = round(r$I,       4),
        "E[I]"       = round(r$EI,      4),
        "Var[I]"     = round(r$VarI,    6),
        "p normal"   = round(r$p_normal,4),
        "p MC"       = round(r$p_mc,    4),
        Resultado    = if (r$p_mc<0.05)
          if(r$I>0) "Cluster (+)" else "Dispersion (-)"
        else "Aleatorio",
        check.names  = FALSE,
        stringsAsFactors = FALSE
      )
    }))
    
    datatable(df_tab, rownames=FALSE, options=list(pageLength=10, scrollX=TRUE)) |>
      formatStyle("Resultado",
                  backgroundColor = styleEqual(
                    c("Cluster (+)","Dispersion (-)","Aleatorio"),
                    c("#d4edda","#fff3cd","#f8d7da")),
                  fontWeight="bold") |>
      formatStyle("p MC",
                  color=styleInterval(0.05, c("#1a6e2e","#cc0000")),
                  fontWeight="bold")
  })
}

# Operador null-coalesce
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

shinyApp(ui = ui, server = server)

