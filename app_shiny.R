library(shinydashboard)
library(shiny)
library(ggplot2)
library(caretEnsemble)
library(corrplot)
library(caret)
library(tidyverse)
library(ggplot2)
library(AppliedPredictiveModeling)
library(randomForest)
library(C50)
library(gbm)
library(caretEnsemble)
#Sustituir más adelante por el archivo mergeado en caso de añadir nuevos valores para reentrenar los modelos
#El reentramiento será auto y manual para lo que sustituiremos los modelos almacenados
coimbra_data=read.csv(file = 'dataR2.csv')
coimbra_data$Classification=unlist(lapply(coimbra_data$Classification, function(x) ifelse(x==1,'sano','enfermo') ))
coimbra_data$Classification=as.factor(coimbra_data$Classification)
#Creación del conjunto de test predefinido
set.seed(998)
inTraining <- createDataPartition(coimbra_data$Classification, p = .70, list = FALSE)
Coimbra_test_9  <- coimbra_data[-inTraining,]
#Comienzo de definición del Dashboard
ui <- dashboardPage(
  dashboardHeader(title = "Cáncer Mama HUB"),
  #Creo la barra de apoyo lateral que srevirá como menú
  dashboardSidebar(    sidebarMenu(
    menuItem("Explicación Herramienta", tabName = "Ayuda", icon = icon("home", lib = "glyphicon")),
    menuItem("Listado de Datos", tabName = "Lista", icon = icon("th-list", lib = "glyphicon")),
    menuItem("Añadir Datos", tabName = "Ingestion", icon = icon("plus-sign", lib = "glyphicon")),
    menuItem("Explorar Datos", tabName = "Exploration", icon = icon("stats", lib = "glyphicon")),
    menuItem("Clasificar Pacientes", tabName = "Classify", icon = icon("th"))
    
  )),
  dashboardBody(
    #Creo el contenido de cada una de las pentañas presentadas
    tabItems(
      tabItem(tabName = "Ayuda",h4("En esta herramienta se provee al personal sanitario de
                                   todo lo necesario para explorar el dataset de la Universidad de Coimbra
                                   (https://bmccancer.biomedcentral.com/articles/10.1186/s12885-017-3877-1)"),br(),
              h4("Además se permite importar nuevos datasets para intentar incrementar la muestra, así como
                                   realizar una prueba de apoyo diagnóstico con un conjunto de registros de pacientes
                                   que aún no hayan sido diagnosticados"),br(),
              h4("Esta herrramienta se espera ir mejorando conforme los usuarios finales la prueben"),br(),
              h4("Las variables utilizadas son Edad, Indice de masa corporal, Glucosa en Sangre, Insulina en Sangre,
                 Resistina en sangre, Adenopectina en sangre, Leptina en sangre, MCP.1 en sangre, y el coeficiente HOMA, resultante
                 de la relacion de Insulina y Glucosa"),br(),
              h4("Como métrica de evaluación de los modelos se utiliza el valor del area bajo la curva ROC, que tiene en cuenta la
                 sensibilidad y la especificidad del modelo. Cuanto más cercano a 1, mejor el modelo, y más confianza podemos tener en los resultados")
              
             ),
      tabItem(tabName = "Lista", downloadButton("export", "Exportar conjunto de datos completo"),
              fluidRow(
            column(10,
               dataTableOutput('table')
        )
      )),
      tabItem(tabName = "Ingestion",fileInput("archivo", h3("Selecciona archivo CSV para importar"),
                                              multiple = FALSE,
                                              accept = c("text/csv",
                                                         "text/comma-separated-values,text/plain",
                                                         ".csv")),textOutput("text_1"),textOutput("text_2"),h3("Introducción manual de registros"),
              fluidRow(
                column(5,
                       h4("Datos paciente diagnosticado/control"),
                       textInput("Age_a", "Edad"),
                       textInput("BMI_a", "IMC"),
                       textInput("Glucose_a", "Glucosa"),
                       textInput("Insulin_a", "Insulina"),
                       textInput("HOMA_a", "HOMA"),
                       textInput("Leptin_a", "Leptina"),
                       textInput("Resistin_a", "Resistina"),
                       textInput("Adenopectin_a", "Adenopectina"),
                       textInput("MCP.1_a", "MCP.1"),
                       textInput("Classification_a", "Estado clínico"),
                       actionButton("submit_a", "Añadir Registro")
                ),
                column(5,
                       h4("Datos paciente sin diagnosticar"),
                       textInput("Age_b", "Edad"),
                       textInput("BMI_b", "IMC"),
                       textInput("Glucose_b", "Glucosa"),
                       textInput("Insulin_b", "Insulina"),
                       textInput("HOMA_b", "HOMA"),
                       textInput("Leptin_b", "Leptina"),
                       textInput("Resistin_b", "Resistina"),
                       textInput("Adenopectin_b", "Adenopectina"),
                       textInput("MCP.1_b", "MCP.1"),
                       actionButton("submit_b", "Añadir Registro")
                )
              ),
              tags$head(tags$style("#text_1{color: red;
                                 font-size: 20px;
                                 font-style: italic;
                                 align: center;
                                 }"
              )),
              tags$head(tags$style("#text_2{color: green;
                                 font-size: 20px;
                                 font-style: italic;
                                 align: center;
                                 }"
              ))
              
      ),
      tabItem(tabName = "Exploration",
              selectInput("variable", "Selecciona la variable:",
                          list("Edad" = "Age", 
                               "IMC" = "BMI", 
                               "Glucosa" = "Glucose",
                               "Insulina" = "Insulin", 
                               "HOMA" = "HOMA", 
                               "Leptina" = "Leptin",
                               "Adiponectina" = "Adiponectin", 
                               "Resistina" = "Resistin", 
                               "MCP.1" = "MCP.1")),
              plotOutput("density"),
              plotOutput("boxplot")
      ),
      tabItem(tabName = "Classify",h3(textOutput("confianza")),h3(textOutput("confianzaSD")),h4(textOutput("mensaje")),
              column(6,selectInput("conjunto", "Conjunto a clasificar:",
                          list("Conjunto test" = "Coimbra_test_9", 
                               "Conjunto importado" = "import.csv"
                               ))),
              column(6,selectInput("modelo", "Modelado a aplicar:",
                          list("Árboles de Decision" = "DT", 
                               "RandomForest" = "RF", 
                              "Gradient Boosted Trees" = "GBT"))),
                       actionButton("clasificar", "Clasificar"),
                      downloadButton("export_pred", "Descargar Resultados Estimación"),
              
              fluidRow(
                column(10,
                       dataTableOutput('table_pred')
                ))

      )

     )
  )
)

#A continuación se implementa toda la lógica del servidor, solo quedaría por añadir la unión de datasets y poder eliminar datos
#Con este método poblaremos las tablas, realizaremos las visualizaciones, daremos la funcionaldiad de carga y descarga
#Aportaremos los indicaros de éxito y clasficaremos el conjunto de entrenamiento o los datos para clasificar
#Para posteriormente mostrar metricas del modelo, resutlados y permitir exportar los mismo
server <- function(input, output) {
  set.seed(122)

  output$table <- renderDataTable(coimbra_data)
  observeEvent(input$submit_a,{
    out_1<-''
    out_2<-''
      if(!isTruthy(input$Age_a) | !isTruthy(input$BMI_a) | !isTruthy(input$Glucose_a) | !isTruthy(input$Insulin_a) | !isTruthy(input$HOMA_a) | !isTruthy(input$Leptin_a) | !isTruthy(input$Resistin_a) | !isTruthy(input$Adenopectin_a) | !isTruthy(input$MCP.1_a) | !isTruthy(input$Classification_a)){
        out_1<-"Datos paciente diagnosticado/control incompletos"
        output$text_1 <- renderText({out_1})

      } else {
        out_2<-'Registro paciente diagnosticado/control añadido'
        output$text_2 <- renderText({out_2})

      }
    
    output$text_1 <- renderText({out_1})
    output$text_2 <- renderText({out_2})
    
  })
  observeEvent(input$submit_b,{
    out_1<-''
    out_2<-''
    # if(input$submit_a > 0) {
    if(!isTruthy(input$Age_b) | !isTruthy(input$BMI_b) | !isTruthy(input$Glucose_b) | !isTruthy(input$Insulin_b) | !isTruthy(input$HOMA_b) | !isTruthy(input$Leptin_b) | !isTruthy(input$Resistin_b) | !isTruthy(input$Adenopectin_b) | !isTruthy(input$MCP.1_b)){
      out_1<-"Datos paciente sin diagnostico incompletos"
      output$text_1 <- renderText({out_1})
      
    } else {
      out_2<-'Registro paciente sin diagnostico añadido'
      output$text_2 <- renderText({out_2})
      
      # values$variable <- isolate(input$variable)
    }
    
    # }
    output$text_1 <- renderText({out_1})
    output$text_2 <- renderText({out_2})
    
  })
  data<-reactive(coimbra_data)
  # observeEvent(input$export,{
  output$export <- downloadHandler(
    filename = function() {
      paste('Dataset_Coimbra', ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data(), file, row.names = FALSE)
    }
  )
  observeEvent(input$archivo,{
    new_data <- reactive({read.csv(input$archivo$datapath)})
    write.csv(new_data(),'Archivos Importados Shiny/import.csv', row.names = FALSE)
  })
  output$density <- renderPlot({
    p<-ggplot(coimbra_data, aes(x=eval(as.symbol(input$variable)), fill=Classification)) +
      geom_density(position="identity", alpha=0.5)+ labs(
        y="Densidad", x = input$variable,fill = "Estado")
    
    p
  })
  output$boxplot <- renderPlot({
    p<-ggplot(coimbra_data, aes(x=Classification, y=eval(as.symbol(input$variable)), color=Classification)) +
      geom_boxplot()+ labs(
        y="Distribución", x = input$variable,color = "Estado")
    p
  })
  observeEvent(input$clasificar,{
    out_1<-''
    out_2<-''
    if(input$conjunto=='Coimbra_test_9'){
      data_predict<-Coimbra_test_9 
      rownames(data_predict) <- NULL
      nombre_modelo<-paste(input$modelo,'.rds',sep='')
      model<-readRDS(paste('modelos9/',nombre_modelo,sep=''))
      prediccion <- predict(model, data_predict[-10])
      data_to_show<-data_predict[-10]
      data_to_show$posibleEstadoClinico<-prediccion
      data_to_show<-data_to_show[, c(10, 1, 2, 3, 4,5,6,7,8,9)]
      print(data_to_show)
      output$table_pred <- renderDataTable(data_to_show)
      out_1<- paste('Valor de rendimiento ROC: ',round(getMetric(model, metric='ROC'),digits=4), sep='')  
      out_2<- paste('Desviación del valor de rendimiento ROC: ',round(getMetricSD(model, metric='ROC'),digits=4), sep='')
    }
    output$confianza <- renderText({out_1})
    output$confianzaSD <- renderText({out_2})
    # output$mensaje<-renderText({             
    #   'Cuanto más cercano a 1, mejor el modelo, y más confianza podemos tener en los resultados'})
    output$export_pred <- downloadHandler(
      filename = function() {
        paste('Estados_Clínicos_Estimados', ".csv", sep = "")
      },
      content = function(file) {
        write.csv(data_to_show, file, row.names = FALSE)
      }
    )
    
    
  })

    
  # })
}

shinyApp(ui, server)
