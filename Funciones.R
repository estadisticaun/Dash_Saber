PocentRelativo <- function(x) {
  TotPercent <- function(m){ m*100/sum(m, na.rm = TRUE) }
  RowPorcent <- round( t(apply(x, MARGIN = 1, FUN = TotPercent)), 0 )
  return(as.data.frame(RowPorcent))
}
cv <- function(x, na.rm = TRUE) {
  return( sd(x, na.rm = na.rm)/abs(mean(x, na.rm = na.rm)) )
}
vars2vec <- function(quosure) {
  Abc <- NULL
  for (i in 1:length(quosure)) { Abc <- c(Abc, rlang::quo_name(quosure[[i]])) }
  return(Abc)
}

Spanish.Highcharter <- function() {
  
  lang <- getOption("highcharter.lang")
  
  lang$contextButtonTitle <- "Men\u00fa Contextual del Gr\u00e1fico"
  lang$viewFullscreen     <- "Ver en pantalla completa"
  lang$printChart   <- "Imprimir gr\u00e1fico"
  lang$downloadPNG  <- "Descargar imagen PNG"
  lang$downloadJPEG <- "Descargar imagen JPEG"
  lang$downloadPDF  <- "Descargar documento PDF"
  lang$downloadSVG  <- "Descargar imagen vectorial SVG"
  lang$downloadCSV  <- "Descargar CSV"
  lang$downloadXLS  <- "Descargar XLS"
  lang$viewData     <- "Ver tabla de datos"
  lang$loading      <- "Cargando..."
  lang$noData       <- "No hay informaci\u00f3n para mostrar"
  lang$drillUpText  <- "<< Volver a {series.name}"
  
  options(highcharter.lang = lang)
}
Plot_Boxplot <- function(datos, variable, grupo1, grupo2,
                         outliers = TRUE, jitter = FALSE, violin = FALSE, numericalVars,
                         ylim, colores, sizeOutlier = 0, colOutlier = "#08306B",
                         textBox = "", titulo = "", labelX = "Periodo", labelY = "",
                         libreria = c("highcharter", "plotly"), estilo = NULL) {
  
  # COMANDOS DE VERIFICACIÓN Y VALIDACIÓN
  if(missingArg(datos) || missingArg(variable) || missingArg(grupo1)) {
    stop("\u00a1Por favor introduzca un conjunto de datos, una variable numérica y un grupo con los cuales se graficará!", call. = FALSE)
  }
  if (!(is.character(titulo) && is.character(labelX) && is.character(labelY) && is.character(textBox))) {
    stop("\u00a1El argumento 'titulo', 'labelX', 'labelY' y 'textBox' deben ser una cadena de texto!", call. = FALSE)
  }
  if (missingArg(libreria)) {
    warning("\u00a1Se usar\u00e1 la librer\u00eda 'highcharter' por defecto para realizar el plot!", call. = FALSE)
    libreria <- "highcharter"
  } else {
    libreria  <- tolower(libreria)
    '%NotIN%' <- Negate('%in%')
    if (libreria %NotIN% c("highcharter", "plotly")) {
      stop("\u00a1Por favor introduzca el nombre de una librer\u00eda valida (paquete usado para realizar la gr\u00e1fica)!", call. = FALSE)
    }
  }
  if (!missingArg(ylim)) {
    if (!(is.numeric(ylim) && length(ylim) == 2)) {
      stop("\u00a1Por favor introduzca un vector de longitud dos que definen los límites del eje Y!", call. = FALSE)
    }
    yLim <- ylim
  } else { yLim <- NULL }
  
  # AJUSTES Y CONDICIONALES PRELIMINARES POR CONSIDERAR
  LegendTitle <- ifelse(is.null(estilo$LegendTitle), "", estilo$LegendTitle)
  Puntos      <- ifelse(jitter, "all", ifelse(outliers, "outliers", FALSE))
  
  if (!missingArg(grupo2)) {
    Levels <- datos %>% select({{grupo2}}) %>% distinct() %>% pull()
    if (!(missingArg(colores) || length(colores)==length(Levels))) {
      stop(paste0("\u00a1El n\u00famero de colores ingresados en el vector 'colores' no corresponde con el n\u00famero de categor\u00edas a colorear!",
                  "\n\tNo. colores ingresados = ", length(colores), " != ", "No. de categor\u00edas = ", length(Levels)), call. = FALSE)
    }
    if (missingArg(colores)) { colores <- rainbow(length(Levels), alpha = 0.7) }
    
    ColxPunto <- FALSE; ShowLegend <- TRUE
    Intento <- try(data_to_boxplot(data = datos, variable = {{variable}},
                                   group_var = {{grupo1}}, group_var2 = {{grupo2}},
                                   add_outliers = outliers, color = colores
    ),
    silent = TRUE
    )
    if (any(class(Intento) == "try-error")) {
      dfBoxPlot <- data_to_boxplot(data = datos, variable = {{variable}},
                                   group_var = {{grupo1}}, group_var2 = {{grupo2}},
                                   add_outliers = FALSE, color = colores
      )
    } else {
      dfBoxPlot <- Intento
    }
    # .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
    TypeGroup <- "group"
    if (!violin) {
      Dots <- list(data = datos, x = rlang::enquo(grupo1), y = rlang::enquo(variable), type = "box",
                   color = rlang::enquo(grupo2), colors = colores, boxpoints = Puntos, pointpos = 0,
                   jitter = 0.4, marker = list(color = colOutlier, size = 2+sizeOutlier)
      )
    } else {
      Dots <- list(data = datos, x = rlang::enquo(grupo1), y = rlang::enquo(variable),
                   type = "violin", color = rlang::enquo(grupo2), colors = colores,
                   box = list(visible = FALSE), meanline = list(visible = TRUE)
      )
    }
    
  } else {
    Levels <- datos %>% select({{grupo1}}) %>% distinct() %>% pull()
    if (!(missingArg(colores) || length(colores)==length(Levels))) {
      stop(paste0("\u00a1El n\u00famero de colores ingresados en el vector 'colores' no corresponde con el n\u00famero de categor\u00edas a colorear!",
                  "\n\tNo. colores ingresados = ", length(colores), " != ", "No. de categor\u00edas = ", length(Levels)), call. = FALSE)
    }
    if (missingArg(colores)) { colores <- rainbow(length(Levels), alpha = 0.7) }
    
    ColxPunto <- TRUE; ShowLegend <- FALSE
    dfBoxPlot <- data_to_boxplot(data = datos, variable = {{variable}}, group_var = {{grupo1}},
                                 add_outliers = outliers, name = textBox
    )
    # .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
    TypeGroup <- NULL
    if (!violin) {
      Dots <- list(data = datos, x = rlang::enquo(grupo1), y = rlang::enquo(variable),
                   type = "box", color = rlang::enquo(grupo1), colors = colores,
                   name = textBox, showlegend = FALSE, boxpoints = Puntos, pointpos = 0, jitter = 0.4,
                   marker = list(color = colOutlier, size = 2+sizeOutlier)
      )
    } else {
      Dots <- list(data = datos, x = rlang::enquo(grupo1), y = rlang::enquo(variable),
                   type = "violin", color = rlang::enquo(grupo1), colors = colores, name = textBox,
                   showlegend = FALSE, box = list(visible = FALSE), meanline = list(visible = TRUE)
      )
    }
  }
  
  # CREACIÓN DEL PLOT RETORNAR
  if(libreria == "highcharter") {
    
    Spanish.Highcharter()
    if(!(missingArg(estilo) || is.null(estilo$hc.Tema))) {
      ThemeHC <- switch(estilo$hc.Tema,
                        "1"  = hc_theme_ffx(),
                        "2"  = hc_theme_google(),
                        "3"  = hc_theme_538(),
                        "4"  = hc_theme_ggplot2(),
                        "5"  = hc_theme_economist(),
                        "6"  = hc_theme_sandsignika(),
                        "7"  = hc_theme_ft(),
                        "8"  = hc_theme_superheroes(),
                        "9"  = hc_theme_flatdark(),
                        "10" = hc_theme_flat()
      )
    } else { ThemeHC <- hc_theme_flat() }
    
    PlotBoxPlot <- highchart()   %>%
      hc_chart(type = "boxplot") %>%
      hc_xAxis(type = "category", title = list(text = labelX,
                                               offset = 70,
                                               style  = list(fontWeight = "bold",
                                                             fontSize   = "18px",
                                                             color      = "black")
      ),
      align = "center", lineColor = "#787878", opposite  = FALSE,
      labels = list(style = list(fontWeight = "bold", color = "black", fontSize = "18px"))) %>%
      hc_add_series_list(dfBoxPlot) %>%
      hc_plotOptions(boxplot = list(colorByPoint = ColxPunto, colors = colores),
                     series  = list(marker = list(fillColor = colOutlier, radius = 1.5+sizeOutlier))
      ) %>%
      hc_title(text = titulo, style = list(fontWeight = "bold",
                                           fontSize   = "22px",
                                           color      = "#333333",
                                           useHTML    = TRUE)
      ) %>%
      hc_yAxis(title = list(text   = labelY,
                            offset = 70,
                            style  = list(fontWeight = "bold",
                                          fontSize   = "18px",
                                          color      = "black")
      ),
      lineColor = "#787878", opposite  = FALSE, lineWidth = 1, min = yLim[1], max = yLim[2],
      labels    = list(style = list(fontWeight = "bold", color = "black", fontSize = "18px"))
      ) %>%
      hc_credits(enabled = TRUE, text = "DNPE", href = "http://estadisticas.unal.edu.co/home/") %>%
      # https://github.com/jbkunst/highcharter/issues/331
      hc_exporting(enabled = TRUE, filename = paste0("PlotBoxPlot_", as_label(enquo(grupo1)))) %>%
      hc_legend(enabled = ShowLegend, align = "center", verticalAlign = "bottom", layout = "horizontal",
                title = list(text = LegendTitle, style = list(textDecoration = "underline")),
                x = 42, y = 0, itemStyle = list(fontWeight = "bold",
                                                color      = "black",
                                                fontSize   = "18px")) %>%
      hc_add_theme(ThemeHC)
    
    # if (jitter) {
    #   PlotBoxPlot <- PlotBoxPlot %>%
    #     hc_add_series(data = datos, type = "scatter",
    #                   hcaes(x = {{grupo1}}, y = {{variable}}, group = {{grupo1}}),
    #                   showInLegend = FALSE
    #                   ) %>%
    #     hc_plotOptions(scatter = list(color = "#000000", jitter = list(x = 0.1, y = 0),
    #                                   # hc_plotOptions(line = list(marker = list(enabled = FALSE, symbol = "square", radius = 1))) %>%
    #                                   marker = list(symbol = "circle", radius = 1, lineWidth = 0.5)
    #                                   ),
    #                    series = list(animation = FALSE)
    #                    )
    # }
    if (!(missingArg(estilo) || is.null(estilo$hc.Credits))) {
      PlotBoxPlot <- PlotBoxPlot %>%
        hc_subtitle(text = estilo$hc.Credits, align = "left", style = list(color = "#2B908F", fontWeight = "bold"))
    }
    
  } else if (libreria == "plotly") {
    
    if (!missingArg(numericalVars)) {
      df  <- as.data.frame(datos)
      CrearBotones <- function(df, listVars) {
        lapply(listVars,
               FUN = function(varName, df) {
                 boton <- list(method = "restyle", args = list("y", list(df[,varName])),
                               label = sprintf("Mostrar: %s", varName)
                 )
               },
               df
        )
      }
      Dots <- list(data = df, x = rlang::enquo(grupo1), y = rlang::enquo(variable),
                   type = "violin", color = rlang::enquo(grupo1), colors = colores,
                   box = list(visible = TRUE), meanline = list(visible = TRUE)
      )
    }
    # .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
    if (!(missingArg(estilo) || is.null(estilo$ply.LegendPosition))) {
      ParmsLegend <- estilo$ply.LegendPosition
    } else {
      ParmsLegend <- list(x = 1, y = 0.5, orientation = "v")
    }
    if (!(missingArg(estilo) || is.null(estilo$ply.Credits))) {
      ParmsCredits <- estilo$ply.Credits
    } else {
      ParmsCredits <- list(x = 0.2, y = 1, text = "")
    }
    
    Hovermode <- ifelse(!(missingArg(estilo) || is.null(estilo$ply.Interaction)),
                        estilo$ply.Interaction, ifelse(missingArg(grupo2), "x", "closest")
    )
    
    # Arial | Open Sans | Courier New, monospace
    FamilyAxis  <- list(family = "Old Standard TT, serif", size = 16, color = "#525252")
    FamilyTitle <- list(family = "Open Sans", size = 24, color = "#333333")
    
    Title <- list(text = paste0("<b>", titulo, "</b>"), font = FamilyTitle, y = 0.96)
    Xaxis <- list(title = paste0("<i>", labelX, "</i>"),
                  zeroline = FALSE,
                  showline = TRUE,
                  showgrid = FALSE,
                  showticklabels = TRUE,
                  linecolor = "#787878",
                  linewidth = 2.5,
                  autotick  = FALSE,
                  ticks     = "outside",
                  tickwidth = 2.5,
                  ticklen   = 10,
                  tickcolor = "#CCCCCC",
                  tickangle = -45,
                  tickfont  = FamilyAxis)
    Yaxis <- list(title = labelY,
                  zeroline = TRUE,
                  showline = TRUE,
                  showgrid = TRUE,
                  showticklabels = TRUE,
                  linecolor = "#787878",
                  linewidth = 3,
                  range     = yLim,
                  separatethousands = TRUE,
                  tickfont  = FamilyAxis)
    
    PlotBoxPlot <- do.call(plot_ly, Dots) %>%
      layout(boxmode = TypeGroup, violinmode = TypeGroup, title = Title, xaxis = Xaxis, yaxis = Yaxis,
             autosize = TRUE, showlegend = TRUE,
             legend = append(ParmsLegend, list(traceorder = "normal", title = list(text = paste0("<b>", LegendTitle, "</b>")))),
             hovermode = Hovermode,
             annotations = append(ParmsCredits, list(showarrow = FALSE, xref = "paper", yref = "paper",
                                                     xanchor = "right", yanchor = "auto", xshift = 0, yshift = 0,
                                                     font = list(size = 12, color = "#CCCCCC")))
      ) %>% config(locale = "es")
    
    if (!missingArg(numericalVars)) {
      PlotBoxPlot <- PlotBoxPlot %>%
        layout(updatemenus = list(list(buttons = CrearBotones(df, vars2vec(numericalVars)))))
    }
    
  }
  
  return(PlotBoxPlot)
}

Plot_Radar <- function(datos, categoria, variables, estadistico = c("Promedio", "Mediana", "Varianza", "SD", "CV", "Min", "Max"),
                       colores, rango, ordinal = FALSE, titulo = "", libreria = c("plotly", "echarts"), estilo = NULL) {
  
  # COMANDOS DE VERIFICACIÓN Y VALIDACIÓN
  if(missingArg(datos) || missingArg(categoria) || missingArg(variables)) {
    stop("\u00a1Por favor introduzca un conjunto de datos, una categor\u00eda y una lista de variables numéricas con las cuales se graficará!", call. = FALSE)
  }
  if (!(is.list(variables) && length(variables)>=3)) {
    stop("\u00a1El parámetro 'variables' debe ser una lista y además contener tres o más variables cuantitativas!", call. = FALSE)
  }
  if (missingArg(estadistico)) {
    warning("\u00a1Se usar\u00e1 como estadístico la media muestral ('mean') por defecto!", call. = FALSE)
  }
  CoefVar   <- function(x, na.rm = T) { return( sd(x, na.rm = na.rm)/abs(mean(x, na.rm = na.rm)) ) }
  Statistic <- match.arg(estadistico)
  Function  <- switch(Statistic,
                      Promedio = mean,
                      Mediana  = median,
                      Varianza = var,
                      SD       = sd,
                      CV       = CoefVar,
                      Min      = min,
                      Max      = max
  )
  if (!(is.numeric(rango) && length(rango)==2)) {
    stop("\u00a1El parámetro 'rango' debe ser un vector numérico de longitud 2!", call. = FALSE)
  }
  if (!is.logical(ordinal)) {
    stop("\u00a1El argumento 'ordinal' debe ser un booleano (TRUE o FALSE)!", call. = FALSE)
  }
  if (!(is.character(titulo))) {
    stop("\u00a1El argumento 'titulo' debe ser una cadena de texto!", call. = FALSE)
  }
  if (missingArg(libreria)) {
    warning("\u00a1Se usar\u00e1 la librer\u00eda 'plotly' por defecto para realizar el plot!", call. = FALSE)
    libreria <- "plotly"
  } else {
    libreria  <- tolower(libreria)
    '%NotIN%' <- Negate('%in%')
    if (libreria %NotIN% c("plotly", "echarts")) {
      stop("\u00a1Por favor introduzca el nombre de una librer\u00eda valida (paquete usado para realizar la gr\u00e1fica)!", call. = FALSE)
    }
  }
  LegendTitle <- ifelse(is.null(estilo$ply.LegendTitle), "", estilo$ply.LegendTitle)
  
  # CREACIÓN DEL DATAFRAME CON EL CUAL SE CREARÁ LA GRÁFICA
  Groups  <- datos %>% select({{categoria}}, !!!variables) %>% group_by({{categoria}}, .drop = FALSE)
  N       <- Groups %>% summarise("n" = n())
  df_Full <- Groups %>% summarise_all(Function, na.rm = TRUE) %>% left_join(N)
  df      <- df_Full %>% select(!c({{categoria}}, n))
  categorias <- colnames(df)
  
  if (ordinal) {
    Orden <- nrow(df):1; OrdenLegend <- "reversed"
  } else {
    Orden <- 1:nrow(df); OrdenLegend <- "normal"
  }
  
  if (!(missingArg(colores) || length(colores)==nrow(df_Full[,1]))) {
    stop(paste0("\u00a1El n\u00famero de colores ingresados en el vector 'colores' no corresponde con el n\u00famero de categor\u00edas a colorear!",
                "\n\tNo. colores ingresados = ", length(colores), " != ", "No. de categor\u00edas = ", nrow(df_Full[,1])), call. = FALSE)
  }
  
  # CREACIÓN DEL PLOT RETORNAR
  if(libreria == "plotly") {
    
    if (missingArg(colores)) { colores <- rainbow(nrow(df_Full[,1]), alpha = 0.2) }
    if (!(missingArg(estilo) || is.null(estilo$ply.LegendPosition))) {
      ParmsLegend <- estilo$ply.LegendPosition
    } else {
      ParmsLegend <- list(x = 1, y = 0.5, orientation = "v")
    }
    if (!(missingArg(estilo) || is.null(estilo$ply.Credits))) {
      ParmsCredits <- estilo$ply.Credits
    } else {
      ParmsCredits <- list(x = 0, y = 0, text = "")
    }
    
    Relleno  <- ifelse(!(missingArg(estilo) || is.null(estilo$ply.Relleno)) , estilo$ply.Relleno , "toself")
    Opacidad <- ifelse(!(missingArg(estilo) || is.null(estilo$ply.Opacidad)), estilo$ply.Opacidad, 1)
    
    PlotRadar <- plot_ly(type = "scatterpolar", fill = Relleno, mode = "markers+lines")
    for (i in Orden) {
      PlotRadar <- add_trace(PlotRadar, r = matrix(df[i,]), theta = categorias, name = df_Full[i,1],
                             line   = list(color = colores[i], width = 2),
                             marker = list(color = colores[i], size = 6, line = list(width = 1, color = "#787878")),
                             fillcolor = list(color = colores[i]), opacity = Opacidad,
                             hoverinfo = "text",
                             text = paste0(df_Full[i,1], "<br> ", Statistic, ": ", round(df[i,], 2), "<br> N: ", df_Full[i,ncol(df_Full)])
      )
    }
    
    # Arial | Open Sans | Courier New, monospace, Old Standard TT
    FamilyTitle  <- list(family = "Old Standard TT", size = 24, color = "#333333")
    FamilyLegend <- list(family = "Open Sans", size = 14, color = "#525252")
    Title  <- list(text = paste0("<b>", titulo, "</b>"), font = FamilyTitle, y = 0.995)
    Legend <- list(text = paste0("<b>", LegendTitle, "</b>"), font = FamilyLegend)
    
    PlotRadar <- PlotRadar %>%
      layout(title = Title, autosize = TRUE, showlegend = TRUE,
             polar = list(radialaxis = list(visible = TRUE, range = rango)),
             legend = append(ParmsLegend, list(title = Legend, traceorder = OrdenLegend)),
             annotations = append(ParmsCredits, list(showarrow = FALSE, xref = "paper", yref = "paper",
                                                     xanchor = "right", yanchor = "auto", xshift = 0, yshift = 0,
                                                     font = list(size = 12, color = "#CCCCCC"))
             )
      ) %>% config(locale = "es")
    
  } else if (libreria == "echarts") {
    
    if(!(missingArg(estilo) || is.null(estilo$e.Tema))) {
      Theme <- switch(estilo$e.Tema,
                      "1"  = "helianthus",
                      "2"  = "azul",
                      "3"  = "inspired",
                      "4"  = "macarons",
                      "5"  = "westeros",
                      "6"  = "walden",
                      "7"  = "roma",
                      "8"  = "royal",
                      "9"  = "fruit",
                      "10" = "dark",
                      "11" = "chalk",
                      "12" = "purple-passion",
                      "13" = "vintage",
                      "14" = "essos"
      )
    } else { Theme <- "default" }
    Subtitle <- ifelse(!(missingArg(estilo) || is.null(estilo$e.Credits)), estilo$e.Credits, "")
    Forma    <- ifelse(!(missingArg(estilo) || is.null(estilo$e.Forma))  , estilo$e.Forma  , "polygon")
    LegType  <- ifelse(!(missingArg(estilo) || is.null(estilo$e.LegType)), estilo$e.LegType, "plain")
    if (!(missingArg(estilo) || is.null(estilo$e.LegLoc))) { LegLoc <- estilo$e.LegLoc } else { LegLoc <- NULL }
    
    df_echarts <- df_Full %>% select(-n) %>%
      pivot_longer(cols = !{{categoria}}, values_to = "Estadistico") %>%
      pivot_wider(names_from = {{categoria}}, values_from = Estadistico) %>%
      mutate_if(is.numeric, ~round(., 2))
    
    Columns <- colnames(df_echarts)[-1]
    Maximo  <- ifelse(is.nan(rango[2]), max(df_echarts[, -1]), rango[2])
    # https://echarts4r.john-coene.com/articles/get_started.html
    PlotRadar <- df_echarts %>% e_charts_("name")
    for (i in 1:length(Columns)) {
      PlotRadar <- PlotRadar %>%
        e_radar_(serie = Columns[i], max = Maximo, name = Columns[i], legend = TRUE)
    }
    
    PlotRadar <- PlotRadar %>%
      e_radar_opts(shape = Forma) %>%
      e_tooltip(trigger = "item") %>%
      e_title(text = titulo, subtext = Subtitle) %>%
      e_legend(right = LegLoc, type = LegType) %>%
      e_theme(Theme)
    
    if (!missingArg(colores)) { PlotRadar <- PlotRadar %>% e_color(colores) }
  }
  
  return(PlotRadar)
}
Plot.Drilldown <- function(datos, varPrincipal, varSecundaria, ano, periodo,
                           torta = TRUE, vertical = TRUE, colores, colores2,
                           titulo = "", label = "", textInfo = "",
                           addPeriodo = TRUE, estilo = NULL) {
  
  # COMANDOS DE VERIFICACIÓN Y VALIDACIÓN
  if(missingArg(datos) || missingArg(varPrincipal) || missingArg(varSecundaria)) {
    stop("¡Por favor introduzca un conjunto de datos, una variable principal y una secundaria (a desglosar)!", call. = FALSE)
  }
  class <- toupper(varPrincipal); class2 <- toupper(varSecundaria)
  if (!(class %in% datos$Variable && class2 %in% datos$Variable)) {
    stop("¡Por favor asegúrese que tanto la variable principal como la secundaria se encuentren dentro de la columna 'Variable'!", call. = FALSE)
  }
  if (!(is.character(titulo) && is.character(label) && is.character(textInfo))) {
    stop("\u00a1El argumento 'titulo', 'label' y 'textInfo' deben ser una cadena de texto!", call. = FALSE)
  }
  LegendTitle <- ifelse(is.null(estilo$LegendTitle), "", estilo$LegendTitle)
  Outside     <- paste("Total de", label); Inside  <- paste("Número de", label)
  
  # CREACIÓN DEL DATAFRAME CON EL CUAL SE CREARÁ LA GRÁFICA
  if (missingArg(ano) && missingArg(periodo)) {
    df  <- ungroup(datos) %>% filter(Variable==varPrincipal, !is.na(Clase)) %>%
      group_by(Clase) %>% summarise(Total = sum(Total)) %>% arrange(desc(Total)) %>%
      mutate(Clase = fct_drop(Clase), drilldown = c(NA, "Sí"))
    
    df2 <- ungroup(datos) %>% filter(Variable==varSecundaria, !is.na(Clase)) %>%
      group_by(Clase) %>% summarise(Total = sum(Total)) %>% arrange(desc(Total))
  } else {
    titulo <- ifelse(!missingArg(titulo) && addPeriodo, paste0(titulo, " (PERIODO ", ano, "-", periodo, ")"), titulo)
    df  <- ungroup(datos) %>%
      filter(Variable==varPrincipal, !is.na(Clase), YEAR==ano, SEMESTRE==periodo) %>%
      arrange(desc(Total)) %>% select(-Variable, -YEAR, -SEMESTRE) %>%
      mutate(Clase = fct_drop(Clase), drilldown = c(NA, "Sí"))
    
    df2 <- ungroup(datos) %>%
      filter(Variable==varSecundaria, !is.na(Clase), YEAR==ano, SEMESTRE==periodo) %>%
      arrange(desc(Total)) %>% select(-Variable, -YEAR, -SEMESTRE)
  }
  categorias <- df %>% select(Clase) %>% distinct() %>% pull(); categorias2 <- df2 %>% select(Clase) %>% distinct() %>% pull()
  
  # CREACIÓN DEL PLOT RETORNAR
  if (!(missingArg(colores) || length(colores)==length(categorias))) {
    stop(paste0("¡El número de colores ingresados en el vector 'colores' no corresponde con el número de categorías a colorear!",
                "\n\tNo. colores ingresados = ", length(colores), " != ", "No. de categorías = ", length(categorias)), call. = FALSE)
  }
  if (missingArg(colores)) { colores <- rainbow(length(categorias), alpha = 0.7) }
  if (!(missingArg(colores2) || length(colores2)==length(categorias2))) {
    stop(paste0("¡El número de colores ingresados en el vector 'colores2' no corresponde con el número de categorías a colorear!",
                "\n\tNo. colores ingresados = ", length(colores2), " != ", "No. de categorías = ", length(categorias2)), call. = FALSE)
  }
  if (missingArg(colores2)) { colores2 <- rainbow(length(categorias2), alpha = 0.7, rev = TRUE) }
  
  Activado <- ifelse(torta, TRUE, FALSE)
  optionsOutside <- list(colorByPoint = TRUE, colors = colores,
                         dataLabels = list(enabled = TRUE, style = list(fontWeight = "bold", color = "black", fontSize = "18px"))
  )
  optionsInside  <- list(colorByPoint = TRUE, colors = colores2,
                         dataLabels = list(enabled = TRUE, style = list(fontWeight = "bold", color = "black", fontSize = "18px"))
  )
  
  if(!(missingArg(estilo) || is.null(estilo$hc.Tema))) {
    ThemeHC <- switch(estilo$hc.Tema,
                      "1" = hc_theme_538(),
                      "2" = hc_theme_alone(),
                      "3" = hc_theme_economist(),
                      "4" = hc_theme_ffx(),
                      "5" = hc_theme_flat(),
                      "6" = hc_theme_ggplot2(),
                      "7" = hc_theme_google(),
                      "8" = hc_theme_monokai(),
                      "9" = hc_theme_darkunica(),
                      "10" = hc_theme_gridlight()
    )
  } else { ThemeHC <- hc_theme_flat() }
  
  if(torta) {
    InvOrientacion <- "column"
    
    PlotDrilldown <- df %>%
      hchart(type = "pie", hcaes(x = Clase, y = Total), name = Outside, showInLegend = TRUE) %>%
      hc_plotOptions(pie = list(allowPointSelect = TRUE, colorByPoint = TRUE, colors = colores,
                                dataLabels = list(enabled = TRUE, format = "<b>{point.name}</b>: {point.percentage:.1f} %",
                                                  style = list(fontWeight = "bold", color = "black", fontSize = "18px")
                                )
      ),
      column = optionsInside
      )
  } else {
    Orientacion <- ifelse(vertical, "column", "bar"); InvOrientacion <- ifelse(vertical, "bar", "column")
    
    PlotDrilldown <- highchart() %>%
      hc_add_series(df, type = Orientacion, colorByPoint = TRUE, hcaes(x = paste(Clase, "-", round(Total*100/sum(Total),1), "%"), y = Total),
                    name = Outside, showInLegend = FALSE
      )
    
    if (vertical) {
      PlotDrilldown <- PlotDrilldown %>% hc_plotOptions(column = optionsOutside, bar = optionsInside)
    } else {
      PlotDrilldown <- PlotDrilldown %>% hc_plotOptions(bar = optionsOutside, column = optionsInside)
    }
  }
  
  PlotDrilldown <- PlotDrilldown %>%
    hc_title(text = titulo, style = list(fontWeight = "bold", fontSize = "22px", color = "#333333", useHTML = TRUE))  %>%
    hc_xAxis(type = "category", labels = list(style = list(fontWeight = "bold", color = "black", fontSize = "18px"))) %>%
    hc_yAxis(title  = list(text = Inside, style = list(fontWeight = "bold", color = "black", fontSize = "18px")),
             labels = list(style = list(fontWeight = "bold", color = "black", fontSize = "18px"))
    ) %>%
    hc_drilldown(drillUpButton = list(theme = list(fill= "#00C0FF", states = list(hover = list(fill = "#FFC000")))),
                 activeDataLabelStyle = list(color = "#0080FF", textDecoration = "underline", fontStyle = "italic"),
                 activeAxisLabelStyle = list(textDecoration = "none"),
                 allowPointDrilldown = TRUE, series = list(list(id = "Sí", name = textInfo,
                                                                data = list_parse2(df2), type = InvOrientacion
                 )
                 )
    ) %>%
    hc_exporting(enabled = TRUE, filename = paste0("PlotDrilldown_", str_to_title(varPrincipal))) %>%
    hc_credits(enabled = TRUE, text = "DNPE", href = "http://estadisticas.unal.edu.co/home/") %>%
    hc_legend(enabled = Activado, align = "center", verticalAlign = "bottom",
              title = list(text = LegendTitle, style = list(textDecoration = "underline")),
              itemStyle = list(fontWeight = "bold", color = "black", fontSize = "18px")) %>%
    hc_add_theme(ThemeHC)
  
  if (!(missingArg(estilo) || is.null(estilo$hc.Credits))) {
    PlotDrilldown <- PlotDrilldown %>%
      hc_subtitle(text = estilo$hc.Credits, align = "left", style = list(color = "#2B908F", fontWeight = "bold"))
  }
  
  return(PlotDrilldown)
}
Tabla.General <- function(datos, colNames, filtros = FALSE, colFilters,
                          encabezado = "", leyenda = "", tituloPdf = NULL, mensajePdf = "",
                          ajustarNiveles = TRUE, colorHead = "#FFFFFF", estilo) {
  
  # COMANDOS DE VERIFICACIÓN Y VALIDACIÓN
  if(missingArg(datos)) {
    stop("\u00a1Por favor introduzca un conjunto de datos!", call. = FALSE)
  }
  if (missingArg(colNames)) {
    colNames <- colnames(datos)
  } else {
    if (length(colNames) != ncol(datos)) {
      stop(paste0("\u00a1El número de nombres para las columnas de la tabla no coincide con el número de columnas presentes en 'datos'!",
                  "\n\t", length(colNames), " != ", ncol(datos)), call. = FALSE)
    }
  }
  if (!(is.logical(filtros) && is.logical(ajustarNiveles))) {
    stop("\u00a1Los argumentos 'filtros' y 'ajustarNiveles' deben ser un booleano (TRUE o FALSE)!", call. = FALSE)
  }
  if (!is.character(colorHead)) {
    stop("\u00a1El argumento 'colorHead' debe ser un car\u00e1cter que indique un color con el nombre ('red'), c\u00f3digo hexadecimal ('#FF0000') o RGB (rgb(1, 0, 0))!", call. = FALSE)
  }
  if (missingArg(tituloPdf)) { tituloPdf <- encabezado }
  if (!missingArg(leyenda)) {
    leyenda <- htmltools::tags$caption(style = "caption-side: bottom; text-align: center;", "Tabla: ", htmltools::em(leyenda))
  }
  AjusteNiveles <- ifelse(ajustarNiveles == TRUE, "compact nowrap hover row-border", "display")
  
  if (filtros) {
    Filtros <- list(position = "top", clear = TRUE, plain = FALSE)
    
    if (missingArg(colFilters)) {
      dots <- list()
    } else {
      if (max(colFilters)>=length(colNames) || min(colFilters)<0) {
        stop("\u00a1El vector ingresado para seleccionar las columnas con filtro debe estar entre [0, n-1] donde n representa el total de columnas!", call. = FALSE)
      } else {
        U <- 0:(length(colNames)-1)
        dots <- list(targets = setdiff(U, colFilters), searchable = FALSE)
      }
    }
  } else {
    if (!missingArg(colFilters)) {
      warning("\u00a1El valor para el argumento 'colFilters' que ha ingresado queda deshabilitado debido a que 'filtros = FALSE'!", call. = FALSE)
    }
    Filtros <- "none"; dots <- list()
  }
  
  thead <- function(...) { htmltools::tag("thead", ...) }
  th <- function(...) { htmltools::tag("th", ...) }
  tr <- function(...) { htmltools::tag("tr", ...) }
  
  # CREACIÓN DEL DATAFRAME CON EL CUAL SE CREARÁ LA TABLA
  DataFrame <- datos %>% mutate_all(., as.factor)
  # Custom Table Container (Nombre de los Encabezados Agrupados)
  sketch = htmltools::withTags(table(
    class = "display",
    thead(
      tr(
        th(colspan = length(colNames), encabezado)
      ),
      tr( lapply(colNames, th) )
    )
  ))
  # CREACIÓN DE LA TABLA A RETORNAR
  TablaFinal <- datatable(
    DataFrame,
    class      = AjusteNiveles,
    rownames   = FALSE,
    colnames   = colNames,
    container  = sketch,
    caption    = leyenda,
    filter     = Filtros,
    extensions = c("Buttons", "KeyTable"),
    options    = list(autoWidth  = TRUE,
                      columnDefs = list(list(className = "dt-center", targets = 0:(length(colNames)-1)),
                                        dots, list(width = "65px", targets = 0)),
                      pageLength = 8,
                      order = list(list(0, "desc"), list(1, "asc")),
                      dom   = "Bfrtip",
                      keys  = TRUE,
                      searchHighlight = TRUE,
                      scrollX = TRUE,
                      initComplete = JS(
                        "function(settings, json) {",
                        "$(this.api().table().header()).css({'background-color':", paste0("'", colorHead, "'"), ", 'color': '#000000'});","}"),
                      language = list(
                        processing     = "Procesando...",
                        lengthMenu     = "Mostrar _MENU_ registros",
                        zeroRecords    = "No se encontraron resultados",
                        emptyTable     = "Ning\u00fan dato disponible en esta tabla",
                        info           = "Mostrando registros del _START_ al _END_ de un total de _TOTAL_ registros",
                        infoEmpty      = "Mostrando registros del 0 al 0 de un total de 0 registros",
                        infoFiltered   = "(filtrado de un total de _MAX_ registros)",
                        infoPostFix    = "",
                        search         = "Buscar:",
                        url            = "",
                        infoThousands  = ",",
                        loadingRecords = "Cargando...",
                        paginate = list(
                          first    = "Primero",
                          last     = "\u00daltimo",
                          `next`   = "Siguiente",
                          previous = "Anterior"
                        ),
                        aria = list(
                          sortAscending  = "Activar para ordenar la columna de manera ascendente",
                          sortDescending = "Activar para ordenar la columna de manera descendente"
                        )
                      ),
                      buttons = list(list(extend = "copy", text = "Copiar"), "csv", "excel",
                                     list(extend = "pdf", pageSize = "A4", filename = "pdf",
                                          message = mensajePdf, title = tituloPdf),
                                     list(extend = "print", text = "Imprimir", pageSize = "A4",
                                          message = mensajePdf, title = tituloPdf))
    )
  )
  
  if (!missingArg(estilo)) {
    for (i in 1:length(estilo)) {
      Temp <- do.call(formatStyle, append(list(table = TablaFinal), estilo[[i]]))
      TablaFinal <- Temp
    }
  }
  
  return(TablaFinal)
}