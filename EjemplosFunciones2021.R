# Cargando las funciones
source("ColeccionFunciones2021.R")
# Llamando las librerías necesarias
library(UnalData)     # 0.0.0.9000
library(tidyverse)    # 1.3.1
library(dplyr)        # 1.0.7
library(tidyr)        # 1.1.4
library(highcharter)  # 0.8.2
library(plotly)       # 4.10.0
library(echarts4r)    # 0.4.2
library(DT)           # 0.19

# Ejemplos del uso de la función Plot.Boxplot()
Puntaje <- UnalData::Aspirantes %>% filter(TIPO_NIVEL == "Pregrado") %>%
  mutate(Serie = factor(paste(YEAR, SEMESTRE, sep = "-"))) %>%
  select(Serie, PTOTAL, TIPO_INS, INS_SEDE_NOMBRE, ADM_SEDE_NOMBRE, FACULTAD) %>%
  replace_na(list(TIPO_INS = "Sin Información"))
Split       <- split(Puntaje, Puntaje$TIPO_INS)
GroupSizes  <- vapply(Split, nrow, integer(1))
SampledObs  <- mapply(sample, GroupSizes, c(10000, 10000, 10000, 50))
Get_Rows    <- function(df, rows) df[rows, , drop = FALSE]
MiniPuntaje <- do.call(rbind, mapply(Get_Rows, Split, SampledObs, SIMPLIFY = FALSE))

Msj <- "Aspirantes a pregrado (<i>no se incluye los datos atípicos</i>)"
Plot_Boxplot(datos       = MiniPuntaje,
             variable    = PTOTAL,
             grupo1      = Serie,
             outliers    = FALSE,
             jitter      = "QUE",
             violin      = 12345,
             ylim        = c(0, 1000),
             colores     = pals::jet(27),
             sizeOutlier = 1,
             colOutlier  = "#FF3366",
             textBox     = "Score",
             titulo      = "EVOLUCIÓN DEL PUNTAJE EN EL EXAMEN DE ADMISIÓN",
             labelY      = "Puntaje",
             libreria    = "highcharter",
             estilo      = list(hc.Tema = 1, hc.Credits = Msj)
             )

Msj <- "Aspirantes a pregrado (<i>cada periodo se encuentra segregado por el tipo de admisión</i>)"
Plot_Boxplot(datos    = MiniPuntaje,
             variable = PTOTAL,
             grupo1   = Serie,
             grupo2   = TIPO_INS,
             outliers = TRUE,
             ylim     = c(0, 1000),
             colores  = c("#00ABFF", "#F3224B", "#FCD116", "#29DF2C"),
             titulo   = "EVOLUCIÓN DEL PUNTAJE EN EL EXAMEN DE ADMISIÓN",
             labelY   = "Puntaje",
             libreria = "highcharter",
             estilo   = list(LegendTitle = "Programa:", hc.Tema = 6, hc.Credits = Msj)
             )

Saber2020 <- UnalData::SaberPro %>% filter(YEAR == 2020) %>%
  select(SEDE_NOMBRE_ADM, PUNTAJE_GLOBAL, PUNT_RAZO_CUANT, PUNT_INGLES,
         PUNT_LECT_CRIT, PUNT_COMP_CIUD, PUNT_COMU_ESCR)
col <- c("#29ABE2", # AZUL CLARO  | Amazonia
         "#8CC63F", # VERDE       | Bogota
         "#CC241D", # ROJO        | Caribe
         "#0071BC", # AZUL VIVO   | Manizales
         "#F15A24", # NARANJA     | Medellin
         "#FBB03B", # AMARILLO    | Orinoquia
         "#93278F", # MORADO      | Palmira
         "#8A381A") # GRIS        | Tumaco
Numericas <- vars(PUNT_RAZO_CUANT, PUNT_INGLES, PUNT_LECT_CRIT, PUNT_COMP_CIUD, PUNT_COMU_ESCR)
Plot_Boxplot(datos         = Saber2020,
             variable      = PUNTAJE_GLOBAL,
             grupo1        = SEDE_NOMBRE_ADM,
             numericalVars = Numericas,
             colores       = col,
             libreria      = "plotly"
             )


# Ejemplos del uso de la función Plot.Radar()
Saber2020 <- UnalData::SaberPro %>% filter(YEAR == 2020) %>% mutate(PRUEBA = "Saber2020")

col <- c("#29ABE2", # AZUL CLARO  | Amazonia
         "#8CC63F", # VERDE       | Bogota
         "#CC241D", # ROJO        | Caribe
         "#0071BC", # AZUL VIVO   | Manizales
         "#F15A24", # NARANJA     | Medellin
         "#FBB03B", # AMARILLO    | Orinoquia
         "#93278F", # MORADO      | Palmira
         "#8A381A") # GRIS        | Tumaco
Msj <- "Gráfico de radar para representar los puntajes multivariados de la prueba Saber Pro."
Plot_Radar(datos       = Saber2020,
           categoria   = SEDE_NOMBRE_ADM,
           variables   = vars(PUNTAJE_GLOBAL, PUNT_RAZO_CUANT, PUNT_INGLES,
                              PUNT_LECT_CRIT, PUNT_COMP_CIUD, PUNT_COMU_ESCR),
           estadistico = "SD",
           colores     = col,
           rango       = c(0, NaN),
           titulo      = "SPIDER PLOT",
           libreria    = "plotly",
           estilo      = list(ply.LegendTitle = "SEDE:", ply.LegendPosition = list(x = 0.15, y = -0.15, orientation = "h"),
                              ply.Relleno = "tonext", ply.Opacidad = 0.5, ply.Credits = list(x = 0.7, y = -0.1, text = Msj)
                              )
           )
Plot_Radar(datos       = Saber2020,
           categoria   = SEDE_NOMBRE_ADM,
           variables   = vars(PUNTAJE_GLOBAL, PUNT_RAZO_CUANT, PUNT_INGLES,
                              PUNT_LECT_CRIT, PUNT_COMP_CIUD, PUNT_COMU_ESCR),
           estadistico = "CV",
           rango       = c(0, 0.25),
           titulo      = "RADAR CHART",
           libreria    = "echarts",
           estilo      = list(e.Credits = Msj, e.Forma = "circle", e.Tema = 1,
                              e.LegType = "scroll", e.LegLoc = 0
                              )
           )


# Ejemplos del uso de la función Plot.Drilldown()
aspirantesPre <- UnalData::Aspirantes %>% filter(TIPO_NIVEL == "Pregrado")
bind_rows(
  UnalR::Agregar(formula = DISCAPACIDAD ~ YEAR + SEMESTRE,
                 frecuencia = list("Year" = 2011:2021, "Period" = 1:2),
                 ask = FALSE, datos = aspirantesPre),
  UnalR::Agregar(formula = TIPO_DISC ~ YEAR + SEMESTRE,
                 frecuencia = list("TIPO_DISC" = 2008:2021, "Period" = 1:2),
                 ask = FALSE, datos = aspirantesPre)
  ) -> df
df <- df %>% filter(Clase != "Sin Información", tolower(Clase) != "no aplica")

Msj <- "Discapacidad: Deficiencia, limitación de la actividad y la restricción de la participación."
Plot.Drilldown(datos         = df,
               varPrincipal  = "DISCAPACIDAD",
               varSecundaria = "TIPO_DISC",
               ano           = max(df$YEAR),
               periodo       = slice(df, n())$SEMESTRE,
               torta         = T,
               vertical      = TRUE,
               colores       = c("#FF0040", "#00FF40"),
               colores2      = RColorBrewer::brewer.pal(n = 6, "Set2"),
               titulo        = "DISTRIBUCIÓN DE ASPIRANTES A PREGRADO EN SITUACIÓN DE DISCAPACIDAD",
               label         = "aspirantes",
               textInfo      = "Aspirantes con discapacidades por tipo",
               addPeriodo    = TRUE,
               estilo        = list(LegendTitle = "SEDE:", hc.Tema = 4, hc.Credits = Msj)
               )


# Ejemplos del uso de la función Tabla.General()
DataF3 <- UnalData::Matriculados %>%
  filter(TIPO_NIVEL == "Pregrado", SEDE_NOMBRE_ADM == "De La Paz") %>% mutate(TOTAL = "Total") %>%
  group_by(YEAR, SEMESTRE, DEP_PROC, CIU_PROC, SEXO, CAT_EDAD, ESTRATO, PROGRAMA) %>%
  summarise(Total = n(), .groups = "drop") %>%
  mutate(across(where(is.character), replace_na, replace = "SIN INFORMACIÓN"))

Nombres <- c("Año", "Semestre", "Departamento", "Municipio", "Sexo", "Edad", "Estrato", "Carrera", "Total")
Titulo  <- "HISTÓRICO DEL TOTAL DE MATRICULADOS EN PREGRADO DEPENDIENDO DE LAS VARIABLES SELECCIONADAS"
Tabla.General(datos          = DataF3,
              colNames       = Nombres,
              filtros        = TRUE,
              colFilters     = 0:3,
              encabezado     = Titulo,
              leyenda        = "Número de matriculados en pregrado por lugar de procedencia.",
              tituloPdf      = "Este es un título provisional para el PDF",
              mensajePdf     = "Este es un mensaje provisional para el PDF",
              ajustarNiveles = TRUE,
              colorHead      = "#4CFF49",
              estilo         = list(list(columns = "YEAR", target = "cell", fontWeight = "bold",
                                         backgroundColor = styleEqual(unique(DataF3$YEAR), c("#FF6400", "#01CDFE", "#FF0532"))
                                         ),
                                    list(columns = "SEMESTRE", target = "cell", fontWeight = "bold",
                                         color = styleEqual(unique(DataF3$SEMESTRE), c("#3D3397", "#AE0421"))
                                         ),
                                    list(columns = "DEP_PROC", color = "#FFFFFF", backgroundColor = "#4D1B7B"),
                                    list(columns = "CIU_PROC", color = "#FFFFFF", backgroundColor = "#F59E11")
                                    )
              )