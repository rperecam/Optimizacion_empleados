library(dplyr)
library(readr)
library(shiny)
library(ggplot2)
library(DT)

# Funciones esenciales
cargar_y_limpiar_datos <- function(ruta_archivo) {
  read_csv(ruta_archivo, show_col_types = FALSE) %>%
    select(employee_id, gender, age, department, region, education, salary,
           previous_year_rating, `KPIs_met >80%`, `awards_won?`,
           `yearly_bonus%`, avg_training_score, is_promoted,
           length_of_service) %>%
    mutate(
      genero = as.integer(gender == "Male"),
      educacion = case_when(
        education == "Master's & above" ~ 3,
        education == "Bachelor's" ~ 2,
        education == "Secondary" ~ 1,
        education == "Below Secondary" ~ 0,
        TRUE ~ NA_real_
      ),
      calificacion_anterior = scale(ifelse(is.na(previous_year_rating),
                                           mean(previous_year_rating, na.rm = TRUE),
                                           previous_year_rating)),
      bono_anual = scale(`yearly_bonus%`),
      puntaje_entrenamiento = scale(avg_training_score)
    )
}

calcular_productividad <- function(datos) {
  pesos_base <- c(
    calificacion_anterior = 0.35,
    `KPIs_met >80%` = 0.30,
    `awards_won?` = 0.10,
    is_promoted = 0.10,
    bono_anual = 0.05,
    puntaje_entrenamiento = 0.10
  )
  
  datos$productividad <- with(datos,
                              calificacion_anterior * pesos_base["calificacion_anterior"] +
                                `KPIs_met >80%` * pesos_base["KPIs_met >80%"] +
                                `awards_won?` * pesos_base["awards_won?"] +
                                is_promoted * pesos_base["is_promoted"] +
                                bono_anual * pesos_base["bono_anual"] +
                                puntaje_entrenamiento * pesos_base["puntaje_entrenamiento"])
  
  datos$productividad <- 1 / (1 + exp(-datos$productividad))
  datos
}

calcular_ratio_coste_productividad <- function(datos) {
  datos$ratio_coste_productividad <- datos$salary / (datos$productividad * 100)
  datos
}

calcular_valor_empresa <- function(datos) {
  max_servicio <- max(datos$length_of_service, na.rm = TRUE)
  datos$valor_empresa <- datos$productividad -
    (0.5 * datos$ratio_coste_productividad /
       max(datos$ratio_coste_productividad, na.rm = TRUE)) +
    (0.2 * datos$length_of_service / max_servicio)
  datos
}

detectar_candidatos_ere <- function(datos, porcentaje_reduccion = 0.20) {
  datos <- datos %>% arrange(valor_empresa)
  coste_total <- sum(datos$salary, na.rm = TRUE)
  objetivo_ahorro <- coste_total * porcentaje_reduccion
  
  datos$candidato_ere <- FALSE
  ahorro_acumulado <- 0
  
  for (i in 1:nrow(datos)) {
    if (ahorro_acumulado < objetivo_ahorro) {
      datos$candidato_ere[i] <- TRUE
      ahorro_acumulado <- ahorro_acumulado + datos$salary[i]
    } else {
      break
    }
  }
  
  list(
    datos = datos,
    ahorro_total = sum(datos$salary[datos$candidato_ere], na.rm = TRUE),
    objetivo_ahorro = objetivo_ahorro,
    porcentaje_logrado = sum(datos$salary[datos$candidato_ere], na.rm = TRUE) / coste_total,
    num_empleados_ere = sum(datos$candidato_ere, na.rm = TRUE)
  )
}

analisis_principal <- function(ruta_archivo, porcentaje_reduccion = 0.20) {
  datos <- cargar_y_limpiar_datos(ruta_archivo)
  datos <- calcular_productividad(datos)
  datos <- calcular_ratio_coste_productividad(datos)
  datos <- calcular_valor_empresa(datos)
  resultado_ere <- detectar_candidatos_ere(datos, porcentaje_reduccion)
  datos <- resultado_ere$datos
  
  list(
    datos_completos = datos,
    candidatos_ere = filter(datos, candidato_ere == TRUE),
    resumen_ere = list(
      ahorro_total = resultado_ere$ahorro_total,
      objetivo_ahorro = resultado_ere$objetivo_ahorro,
      porcentaje_logrado = resultado_ere$porcentaje_logrado,
      num_empleados_ere = resultado_ere$num_empleados_ere,
      porcentaje_plantilla = resultado_ere$num_empleados_ere / nrow(datos)
    )
  )
}

ruta_archivo <- "data/data.csv"
resultados_analisis <- analisis_principal(ruta_archivo, 0.20)

save(resultados_analisis, file = "data/pdata.RData")
load("data/pdata.RData")

datos <- resultados_analisis$datos_completos
datos_ere <- resultados_analisis$candidatos_ere
resumen_ere <- resultados_analisis$resumen_ere

# Interfaz de usuario simplificada
ui <- fluidPage(
  titlePanel("Análisis de Reducción de Plantilla - SOLUCIONA"),
  
  wellPanel(
    h3("Resumen del Plan de Reducción"),
    fluidRow(
      column(3, h4("Objetivo de ahorro:"), textOutput("objetivo_ahorro")),
      column(3, h4("Ahorro estimado:"), textOutput("ahorro_total")),
      column(3, h4("Empleados afectados:"), textOutput("empleados_afectados")),
      column(3, h4("% de plantilla:"), textOutput("porcentaje_plantilla"))
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("departamento", "Departamento:", choices = c("Todos", unique(datos$department)), multiple = TRUE),
      selectInput("region", "Región:", choices = c("Todas", unique(datos$region)), multiple = TRUE),
      sliderInput("edad", "Edad Mínima:", min = min(datos$age, na.rm = TRUE), max = max(datos$age, na.rm = TRUE), value = min(datos$age, na.rm = TRUE)),
      checkboxInput("mostrar_candidatos", "Mostrar solo candidatos a ERE", value = FALSE)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Dashboard ERE",
                 fluidRow(
                   column(6, plotOutput("distribucion_valor_empresa")),
                   column(6, plotOutput("valor_vs_salario"))
                 ),
                 fluidRow(
                   column(12, plotOutput("departamentos_stats"))
                 )
        ),
        tabPanel("Candidatos a ERE", DTOutput("tabla_candidatos_ere"))
      )
    )
  )
)

# Servidor simplificado
server <- function(input, output, session) {
  output$objetivo_ahorro <- renderText({
    paste(format(round(resumen_ere$objetivo_ahorro / 1000000, 2), nsmall = 2), "millones €")
  })
  
  output$ahorro_total <- renderText({
    paste(format(round(resumen_ere$ahorro_total / 1000000, 2), nsmall = 2), "millones € (", round(resumen_ere$porcentaje_logrado * 100, 1), "%)")
  })
  
  output$empleados_afectados <- renderText({
    paste(resumen_ere$num_empleados_ere, "empleados")
  })
  
  output$porcentaje_plantilla <- renderText({
    paste(round(resumen_ere$porcentaje_plantilla * 100, 1), "%")
  })
  
  filtered_data <- reactive({
    temp_data <- datos
    if (!("Todos" %in% input$departamento)) {
      temp_data <- temp_data %>% filter(department %in% input$departamento)
    }
    if (!("Todas" %in% input$region)) {
      temp_data <- temp_data %>% filter(region %in% input$region)
    }
    temp_data <- temp_data %>% filter(age >= input$edad)
    if (input$mostrar_candidatos) {
      temp_data <- temp_data %>% filter(candidato_ere == TRUE)
    }
    temp_data
  })
  
  output$distribucion_valor_empresa <- renderPlot({
    ggplot() +
      geom_density(data = filtered_data(), aes(x = valor_empresa), fill = "skyblue", alpha = 0.7) +
      geom_density(data = datos_ere, aes(x = valor_empresa), fill = "red", alpha = 0.4) +
      labs(title = "Distribución del Valor para la Empresa", subtitle = "Candidatos ERE en rojo", x = "Valor para la Empresa", y = "Densidad") +
      theme_minimal()
  })
  
  output$valor_vs_salario <- renderPlot({
    ggplot() +
      geom_point(data = datos, aes(x = salary, y = valor_empresa), alpha = 0.3, color = "gray") +
      geom_point(data = filtered_data(), aes(x = salary, y = valor_empresa), alpha = 0.7, color = "blue") +
      geom_point(data = datos_ere, aes(x = salary, y = valor_empresa), alpha = 0.7, color = "red") +
      labs(title = "Relación Salario vs. Valor para la Empresa", subtitle = "Candidatos ERE en rojo", x = "Salario (€)", y = "Valor para la Empresa") +
      theme_minimal()
  })
  
  output$departamentos_stats <- renderPlot({
    stats_dept <- datos %>%
      group_by(department) %>%
      summarise(ratio_medio = mean(ratio_coste_productividad, na.rm = TRUE))
    
    ggplot(stats_dept, aes(x = reorder(department, -ratio_medio), y = ratio_medio)) +
      geom_bar(stat = "identity") +
      labs(title = "Ratio Coste/Productividad por Departamento", x = "Departamento", y = "Ratio Coste/Productividad") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$tabla_candidatos_ere <- renderDT({
    datos_ere %>%
      select(ID = employee_id, Género = gender, Edad = age, Departamento = department, Región = region, Educación = education, `Salario (€)` = salary, `Calificación` = previous_year_rating, `Productividad (%)` = productividad, `Valor empresa` = valor_empresa, `Antigüedad` = length_of_service, `Ratio C/P` = ratio_coste_productividad) %>%
      mutate(`Productividad (%)` = round(`Productividad (%)` * 100, 1), `Valor empresa` = round(`Valor empresa`, 2), `Ratio C/P` = round(`Ratio C/P`, 2)) %>%
      datatable(options = list(pageLength = 10))
  })
}

shinyApp(ui = ui, server = server)
