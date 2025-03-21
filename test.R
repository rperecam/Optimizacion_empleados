library(dplyr)
library(readr)
library(shiny)
library(ggplot2)
library(DT)
library(plotly)
library(corrplot)

# Funciones de carga y análisis (sin cambios)
cargar_y_limpiar_datos <- function(ruta_archivo) {
  if (!file.exists(ruta_archivo)) {
    stop("El archivo no existe. Por favor, verifica la ruta.")
  }
  read_csv(ruta_archivo, show_col_types = FALSE) %>%
    select(employee_id, gender, age, department, region, education, salary, previous_year_rating, `KPIs_met >80%`, `awards_won?`, `yearly_bonus%`, avg_training_score, is_promoted) %>%
    mutate(
      genero = as.integer(gender == "Male"),
      educacion = case_when(
        education == "Master's & above" ~ 3,
        education == "Bachelor's" ~ 2,
        education == "Secondary" ~ 1,
        education == "Below Secondary" ~ 0,
        TRUE ~ NA_real_
      ),
      calificacion_anterior = scale(ifelse(is.na(previous_year_rating), mean(previous_year_rating, na.rm = TRUE), previous_year_rating)),
      bono_anual = scale(`yearly_bonus%`),
      puntaje_entrenamiento = scale(avg_training_score)
    )
}

calcular_productividad <- function(datos) {
  escasez <- function(columna) { mean(1 - columna, na.rm = TRUE) }
  escasez_premios <- escasez(datos$`awards_won?`)
  escasez_promocion <- escasez(datos$is_promoted)
  escasez_kpis <- escasez(datos$`KPIs_met >80%`)
  pesos_base <- c(calificacion_anterior = 0.30, `KPIs_met >80%` = 0.25, `awards_won?` = 0.10, is_promoted = 0.15, bono_anual = 0.10, puntaje_entrenamiento = 0.10)
  pesos_escasez <- c(`awards_won?` = escasez_premios, is_promoted = escasez_promocion, `KPIs_met >80%` = escasez_kpis)
  pesos_finales <- pesos_base * (1 + c(pesos_escasez, rep(0, length(pesos_base) - length(pesos_escasez))))
  datos$productividad <- with(datos, calificacion_anterior * pesos_finales["calificacion_anterior"] + `KPIs_met >80%` * pesos_finales["KPIs_met >80%"] + `awards_won?` * pesos_finales["awards_won?"] + is_promoted * pesos_finales["is_promoted"] + bono_anual * pesos_finales["bono_anual"] + puntaje_entrenamiento * pesos_finales["puntaje_entrenamiento"])
  datos$productividad <- 1 / (1 + exp(-datos$productividad))
  select(datos, -calificacion_anterior, -`KPIs_met >80%`, -`awards_won?`, -is_promoted, -bono_anual, -puntaje_entrenamiento)
}

detectar_outliers_salario <- function(datos) {
  cuartiles <- quantile(datos$salary, c(0.25, 0.75))
  iqr <- cuartiles[2] - cuartiles[1]
  datos$salary < (cuartiles[1] - 1.5 * iqr) | datos$salary > (cuartiles[2] + 1.5 * iqr)
}

analizar_empleados_mayores <- function(datos, edad_minima) {
  filter(datos, age > edad_minima)
}

analisis_principal <- function(ruta_archivo) {
  datos <- cargar_y_limpiar_datos(ruta_archivo)
  datos <- calcular_productividad(datos)
  outliers_salario <- detectar_outliers_salario(datos)
  datos_despidos <- datos[outliers_salario, ] %>% filter(productividad <= quantile(productividad, 0.25))
  empleados_mayores <- analizar_empleados_mayores(datos, 55)
  datos_despidos <- bind_rows(datos_despidos, empleados_mayores) %>% distinct(employee_id, .keep_all = TRUE)
  list(datos, datos_despidos)
}

ruta_archivo <- "data/data.csv"
resultados_analisis <- analisis_principal(ruta_archivo)

save(resultados_analisis, file = "C:data/pdata.RData")
load("data/pdata.RData")

datos <- resultados_analisis[[1]]
datos_despidos <- resultados_analisis[[2]]

# UI
ui <- fluidPage(
  titlePanel("Análisis de Datos de Empleados"),
  sidebarLayout(
    sidebarPanel(
      selectInput("departamento", "Departamento:", choices = c("Todos", unique(datos$department)), multiple = TRUE),
      selectInput("region", "Región:", choices = c("Todas", unique(datos$region)), multiple = TRUE),
      sliderInput("edad", "Edad Mínima:", min = min(datos$age, na.rm = TRUE), max = max(datos$age, na.rm = TRUE), value = min(datos$age, na.rm = TRUE)),
      checkboxInput("bajo_rendimiento", "Mostrar solo bajo rendimiento", value = FALSE)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Mapa de Correlación", plotOutput("correlation_matrix")),
        tabPanel("Distribución de Productividad", plotOutput("distribucion_productividad")),
        tabPanel("Diagrama de caja de salarios", plotOutput("boxplot_salario")),
        tabPanel("Distribución de Edades", plotOutput("distribucion_edades")),
        tabPanel("Salario vs Productividad", plotOutput("scatter_salary_productivity")),
        tabPanel("Gráfico 3D", plotlyOutput("scatter_plot_3d")),
        tabPanel("Cantidad de Empleados por Departamento/Región", plotOutput("cantidad_empleados")),
        tabPanel("Tabla de Candidatos a Despido", DTOutput("tabla_despidos")),
        tabPanel("Datos de Empleados", DTOutput("tabla_empleados"))
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  observe({
    if ("Todas" %in% input$region) {
      selected_regions <- setdiff(unique(datos$region), "Todas")
    } else {
      selected_regions <- input$region
    }
    updateSelectInput(session, "region", choices = c("Todas", unique(datos$region)), selected = selected_regions)
  })
  
  observe({
    if ("Todos" %in% input$departamento) {
      selected_departments <- setdiff(unique(datos$department), "Todos")
    } else {
      selected_departments <- input$departamento
    }
    updateSelectInput(session, "departamento", choices = c("Todos", unique(datos$department)), selected = selected_departments)
  })
  
  filtered_data <- reactive({
    datos %>%
      filter(department %in% input$departamento, region %in% input$region, age > input$edad) %>%
      filter(productividad <= quantile(productividad, 0.25) | !input$bajo_rendimiento)
  })
  
  # Gráficos
  output$correlation_matrix <- renderPlot({
    cor_matrix_filtered <- cor(filtered_data() %>% select(age, salary, avg_training_score, productividad), use = "complete.obs")
    cor_matrix_all <- cor(datos %>% select(age, salary, avg_training_score, productividad), use = "complete.obs")
    
    par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
    corrplot(cor_matrix_filtered, method = "color", addCoef.col = "black", tl.col = "black", tl.srt = 45, title = "Correlación Filtrada")
    corrplot(cor_matrix_all, method = "color", addCoef.col = "black", tl.col = "black", tl.srt = 45, title = "Correlación Total")
  })
  
  output$distribucion_productividad <- renderPlot({
    ggplot() +
      geom_density(data = filtered_data(), aes(x = productividad), fill = "skyblue", alpha = 0.7) +
      geom_density(data = datos, aes(x = productividad), color = "red", linetype = "dashed", linewidth = 1) +
      geom_vline(aes(xintercept = mean(filtered_data()$productividad)), color = "blue", linetype = "dotted", linewidth = 1) +
      labs(title = "Distribución de Productividad", x = "Productividad", y = "Densidad") +
      theme_minimal()
  })
  
  output$boxplot_salario <- renderPlot({
    ggplot() +
      geom_boxplot(data = filtered_data(), aes(y = salary), fill = "skyblue") +
      geom_boxplot(data = datos, aes(y = salary), color = "red", alpha = 0.3, width = 0.2) +
      labs(title = "Diagrama de caja de salarios", y = "Salario") +
      theme_minimal()
  })
  
  output$distribucion_edades <- renderPlot({
    ggplot() +
      geom_density(data = filtered_data(), aes(x = age), fill = "skyblue", alpha = 0.7) +
      geom_density(data = datos, aes(x = age), color = "red", linetype = "dashed", linewidth = 1) +
      geom_vline(aes(xintercept = mean(filtered_data()$age)), color = "blue", linetype = "dotted", linewidth = 1) +
      labs(title = "Distribución de Edades", x = "Edad", y = "Densidad") +
      theme_minimal()
  })
  
  output$scatter_salary_productivity <- renderPlot({
    ggplot() +
      geom_point(data = filtered_data(), aes(x = salary, y = productividad), alpha = 0.6) +
      geom_smooth(data = filtered_data(), aes(x = salary, y = productividad), method = "lm", se = FALSE, color = "blue", linetype = "solid") +
      geom_smooth(data = datos, aes(x = salary, y = productividad), method = "lm", se = FALSE, color = "red", linetype = "dashed") +
      labs(title = "Salario vs Productividad", x = "Salario", y = "Productividad") +
      theme_minimal()
  })
  
  output$scatter_plot_3d <- renderPlotly({
    plot_ly(filtered_data(), x = ~age, y = ~salary, z = ~productividad, color = ~gender, colors = c("pink", "blue")) %>%
      add_markers() %>%
      layout(scene = list(xaxis = list(title = 'Edad'), yaxis = list(title = 'Salario'), zaxis = list(title = 'Productividad')),
             title = "Gráfico 3D: Edad, Salario y Productividad")
  })
  
  output$cantidad_empleados <- renderPlot({
    ggplot(filtered_data(), aes(x = department)) +
      geom_bar(fill = "skyblue") +
      labs(title = "Cantidad de Empleados por Departamento", x = "Departamento", y = "Cantidad de Empleados") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      facet_wrap(~region)
  })
  
  # Tablas de datos
  output$tabla_despidos <- renderDT({
    datatable(datos_despidos)
  })
  
  output$tabla_empleados <- renderDT({
    datatable(filtered_data())
  })
}

# Ejecutar la aplicación Shiny
shinyApp(ui, server)
