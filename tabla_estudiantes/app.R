# --- VERIFICACIÓN E INSTALACIÓN AUTOMÁTICA DE PAQUETES ---
paquetes <- c("shiny", "DT", "ggplot2", "plotly", "dplyr", "readxl")
paquetes_faltantes <- paquetes[!(paquetes %in% installed.packages()[,"Package"])]

if(length(paquetes_faltantes) > 0) {
  message("Instalando paquetes faltantes: ", paste(paquetes_faltantes, collapse = ", "))
  install.packages(paquetes_faltantes, dependencies = TRUE)
}

# Cargar las librerías
library(shiny)
library(DT)
library(ggplot2)
library(plotly)
library(dplyr)
library(readxl)

# --- INTERFAZ DE USUARIO (UI) ---
ui <- fluidPage(
  titlePanel("Dashboard de Estadísticas y Notas de Estudiantes"),

  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Subir archivo Excel (.xlsx)",
                accept = c(".xlsx", ".xls")),
      helpText("Formato esperado: codigo, nombre, ciudad, edad, asignatura, semestre, nota1, nota2[cite: 1]."),
      hr(),
      checkboxInput("use_sample", "Usar datos de ejemplo (idénticos a la tabla)", value = TRUE),
      hr(),

      # Filtros reactivos
      uiOutput("filtro_ciudad_ui"),
      uiOutput("filtro_semestre_ui"),
      uiOutput("filtro_asignatura_ui"),
      uiOutput("filtro_estado_ui"),
      hr(),

      selectInput("plot_type", "Tipo de Gráfica:",
                  choices = c("Aprobados vs Reprobados (Cantidad)" = "bar_estado",
                              "Ranking de Promedios por Estudiante" = "bar_estudiantes",
                              "Promedio por Asignatura" = "bar_asignatura",
                              "Promedio por Semestre" = "bar_semestre",
                              "Promedio por Ciudad" = "bar_ciudad",
                              "Distribución de Promedios (Histograma)" = "hist")),
      tags$hr(),
      tags$small("Desarrollado con R Shiny, ggplot2 y Plotly.")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Tabla de Datos y Promedios",
                 br(),
                 DTOutput("tabla_estudiantes")),
        tabPanel("Estadísticas Resumen",
                 br(),
                 verbatimTextOutput("summary_stats")),
        tabPanel("Visualización Gráfica",
                 br(),
                 plotlyOutput("grafico_estudiantes", height = "500px"))
      )
    )
  )
)

# --- SERVIDOR (SERVER) ---
server <- function(input, output, session) {

  # Carga o generación de datos reactivos
  datos_base <- reactive({
    if (input$use_sample || is.null(input$file1)) {
      # Datos simulados con la estructura exacta de la imagen[cite: 1]
      df <- data.frame(
        codigo = 1001:1029,
        nombre = c("Pedro Sanchez", "Pedro Jose", "Lucia Gomez", "Maria Diaz", "Elena Gomez",
                   "Luis Perez", "Ana Lopez", "Elena Martinez", "Diego Sanchez", "Lucia Diaz",
                   "Luis Diaz", "Pedro Diaz", "Diego Martinez", "Diego Gomez", "Diego Perez",
                   "Pedro Rodriguez", "Sofia Rodriguez", "Lucia Sanchez", "Carlos Rodriguez", "Lucia Martinez",
                   "Diego Perez", "Pedro Garcia", "Ana Gomez", "Juan Sanchez", "Carlos Lopez",
                   "Luis Garcia", "Lucia Gomez", "Carlos Lopez", "Diego Diaz"),
        ciudad = c("Riobamba", "Loja", "Riobamba", "Guayaquil", "Cuenca",
                   "Quito", "Quito", "Manta", "Manta", "Cuenca",
                   "Guayaquil", "Manta", "Loja", "Manta", "Cuenca",
                   "Cuenca", "Quito", "Ambato", "Guayaquil", "Riobamba",
                   "Quito", "Riobamba", "Guayaquil", "Ambato", "Manta",
                   "Quito", "Quito", "Quito", "Manta"),
        edad = c(20, 19, 19, 22, 29, 26, 30, 27, 30, 23, 27, 27, 23, 29, 20, 21, 28, 28, 20, 30, 24, 20, 19, 24, 20, 26, 30, 24, 26),
        asignatura = c("Calculo", "Fisica", "Estadistica", "Algebra", "Calculo",
                       "Estadistica", "Estadistica", "Estadistica", "Programacion", "Programacion",
                       "Estadistica", "Fisica", "Fisica", "Algebra", "Calculo",
                       "Calculo", "Fisica", "Calculo", "Programacion", "Fisica",
                       "Estadistica", "Programacion", "Algebra", "Estadistica", "Fisica",
                       "Calculo", "Algebra", "Algebra", "Calculo"),
        semestre = c(5, 7, 3, 2, 8, 4, 6, 5, 2, 7, 3, 3, 6, 1, 6, 8, 3, 2, 3, 8, 3, 1, 6, 2, 2, 8, 8, 2, 4),
        nota1 = c(8.3, 7.1, 8.3, 2.2, 3.3, 4.0, 4.7, 6.7, 8.3, 3.3, 8.4, 1.1, 6.9, 8.3, 5.2, 2.8, 1.2, 3.5, 9.2, 9.4, 5.9, 3.0, 5.4, 8.2, 4.7, 1.9, 3.5, 4.3, 3.3),
        nota2 = c(2.5, 7.9, 5.7, 8.7, 7.1, 1.1, 7.2, 9.0, 6.7, 2.0, 9.3, 7.1, 2.3, 7.7, 9.5, 4.8, 3.7, 3.3, 3.0, 6.1, 7.8, 7.0, 5.9, 8.3, 7.8, 1.2, 4.4, 1.5, 8.2)
      )
    } else {
      ext <- tools::file_ext(input$file1$datapath)
      req(ext %in% c("xlsx", "xls"))
      df <- read_excel(input$file1$datapath)
    }

    # Calcular promedio y estado automáticamente
    if ("nota1" %in% names(df) && "nota2" %in% names(df)) {
      df$Promedio <- round((df$nota1 + df$nota2) / 2, 2)
      # Se define Aprobado si el promedio es >= 7.0, caso contrario Reprobado
      df$Estado <- ifelse(df$Promedio >= 7.0, "Aprobado", "Reprobado")
    } else {
      df$Promedio <- 0
      df$Estado <- "Reprobado"
    }

    return(df)
  })

  # Controles de filtro dinámicos
  output$filtro_ciudad_ui <- renderUI({
    df <- datos_base()
    req("ciudad" %in% names(df))
    selectInput("filtro_ciudad", "Filtrar por Ciudad:",
                choices = c("Todas", unique(as.character(df$ciudad))),
                selected = "Todas")
  })

  output$filtro_semestre_ui <- renderUI({
    df <- datos_base()
    req("semestre" %in% names(df))
    selectInput("filtro_semestre", "Filtrar por Semestre:",
                choices = c("Todos", sort(unique(as.numeric(df$semestre)))),
                selected = "Todos")
  })

  output$filtro_asignatura_ui <- renderUI({
    df <- datos_base()
    req("asignatura" %in% names(df))
    selectInput("filtro_asignatura", "Filtrar por Asignatura:",
                choices = c("Todas", unique(as.character(df$asignatura))),
                selected = "Todas")
  })

  output$filtro_estado_ui <- renderUI({
    df <- datos_base()
    req("Estado" %in% names(df))
    selectInput("filtro_estado", "Filtrar por Estado:",
                choices = c("Todos", unique(as.character(df$Estado))),
                selected = "Todos")
  })

  # Filtrado reactivo de los datos
  datos_filtrados <- reactive({
    df <- datos_base()

    if (!is.null(input$filtro_ciudad) && input$filtro_ciudad != "Todas") {
      df <- df %>% filter(ciudad == input$filtro_ciudad)
    }
    if (!is.null(input$filtro_semestre) && input$filtro_semestre != "Todos") {
      df <- df %>% filter(semestre == as.numeric(input$filtro_semestre))
    }
    if (!is.null(input$filtro_asignatura) && input$filtro_asignatura != "Todas") {
      df <- df %>% filter(asignatura == input$filtro_asignatura)
    }
    if (!is.null(input$filtro_estado) && input$filtro_estado != "Todos") {
      df <- df %>% filter(Estado == input$filtro_estado)
    }

    return(df)
  })

  # Renderizar tabla interactiva con DT
  output$tabla_estudiantes <- renderDT({
    datatable(datos_filtrados(),
              options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE),
              rownames = FALSE)
  })

  # Mostrar estadísticas resumen
  output$summary_stats <- renderPrint({
    df <- datos_filtrados()
    cols_numericas <- sapply(df, is.numeric)
    summary(df[, cols_numericas, drop = FALSE])
  })

  # Renderizar gráficas interactivas con Plotly
  output$grafico_estudiantes <- renderPlotly({
    df <- datos_filtrados()
    req(nrow(df) > 0)
    req("Promedio" %in% names(df))

    if (input$plot_type == "bar_estado") {
      df_agg <- df %>% group_by(Estado) %>% summarise(Cantidad = n())
      p <- ggplot(df_agg, aes(x = Estado, y = Cantidad, fill = Estado)) +
        geom_col(alpha = 0.85, show.legend = FALSE) +
        geom_text(aes(label = Cantidad), vjust = 1.5, color = "white", fontface = "bold") +
        scale_fill_manual(values = c("Aprobado" = "#2ECC71", "Reprobado" = "#E74C3C")) +
        labs(title = "Cantidad de Estudiantes Aprobados y Reprobados",
             x = "Estado", y = "Cantidad de Estudiantes") +
        theme_minimal()

    } else if (input$plot_type == "bar_estudiantes") {
      p <- ggplot(df, aes(x = reorder(nombre, Promedio), y = Promedio, fill = Estado)) +
        geom_col(alpha = 0.85) +
        scale_fill_manual(values = c("Aprobado" = "#2ECC71", "Reprobado" = "#E74C3C")) +
        coord_flip() +
        labs(title = "Ranking de Estudiantes por Promedio",
             x = "Estudiante", y = "Promedio Final") +
        theme_minimal()

    } else if (input$plot_type == "bar_asignatura") {
      df_agg <- df %>% group_by(asignatura) %>% summarise(Promedio_General = round(mean(Promedio), 2))
      p <- ggplot(df_agg, aes(x = reorder(asignatura, Promedio_General), y = Promedio_General, fill = asignatura)) +
        geom_col(alpha = 0.85, show.legend = FALSE) +
        geom_text(aes(label = Promedio_General), hjust = 1.2, color = "white", fontface = "bold") +
        coord_flip() +
        labs(title = "Promedio General por Asignatura",
             x = "Asignatura", y = "Promedio General") +
        theme_minimal()

    } else if (input$plot_type == "bar_semestre") {
      df_agg <- df %>% group_by(semestre) %>% summarise(Promedio_General = round(mean(Promedio), 2))
      p <- ggplot(df_agg, aes(x = as.factor(semestre), y = Promedio_General, fill = as.factor(semestre))) +
        geom_col(alpha = 0.85, show.legend = FALSE) +
        geom_text(aes(label = Promedio_General), vjust = 1.5, color = "white", fontface = "bold") +
        labs(title = "Promedio General por Semestre",
             x = "Semestre", y = "Promedio General") +
        theme_minimal()

    } else if (input$plot_type == "bar_ciudad") {
      df_agg <- df %>% group_by(ciudad) %>% summarise(Promedio_General = round(mean(Promedio), 2))
      p <- ggplot(df_agg, aes(x = reorder(ciudad, Promedio_General), y = Promedio_General, fill = ciudad)) +
        geom_col(alpha = 0.85, show.legend = FALSE) +
        geom_text(aes(label = Promedio_General), hjust = 1.2, color = "white", fontface = "bold") +
        coord_flip() +
        labs(title = "Promedio General por Ciudad",
             x = "Ciudad", y = "Promedio General") +
        theme_minimal()

    } else if (input$plot_type == "hist") {
      p <- ggplot(df, aes(x = Promedio)) +
        geom_histogram(bins = 8, fill = "#2C3E50", color = "white", alpha = 0.85) +
        labs(title = "Distribución de Promedios Generales",
             x = "Promedio Final", y = "Cantidad de Estudiantes") +
        theme_minimal()
    }

    ggplotly(p)
  })
}

# Ejecutar la aplicación
shinyApp(ui, server)
