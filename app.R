if(!require(shinyWidgets)) install.packages("shinyWidgets")
if(!require(shinyWidgets)) install.packages("tidyverse")

library(shiny)
library(shinyWidgets)
library(tidyverse)
    
  # Define la interfaz de usuario
  ui <- fluidPage(
    titlePanel("Introducción al estudio de las fluctuaciones"),
    sidebarLayout(
      sidebarPanel(
        wellPanel(
          h3("Parámetros del modelo"),
          sliderInput("c", "Propensión marginal a consumir:", min = 0, max = 1, value = 0.6),
          sliderInput("t", "Tipo impositivo:", min = 0, max = 1, value = 0.4),
          sliderInput("b", "Sensibilidad  de la inversión al tipo de interés", min = 0, max = 20, value = 5),
          sliderInput("k", "Sensibilidad de la demanda de dinero al nivel de renta:", min = 0, max = 20, value = 13),
          sliderInput("l", "Sensibilidad de la demanda de dinero al tipo de interés:", min = 0, max = 20, value = 2),
        ),
        wellPanel(
          h3("Condiciones iniciales"),
          sliderInput("C_0", "Consumo autónomo:", min = 0, max = 100, value = 30),
          sliderInput("I_0", "Inversión autónoma:", min = 0, max = 100, value = 10),
          sliderInput("G_0", "Compras del estado", min = 0, max = 100, value = 15),
          sliderInput("M_0", "Oferta de dinero:", min = 0, max = 100, value = 100),
          sliderInput("P_0", "Nivel de precios:", min = 0, max = 2.0, value = 1.0, step=0.1),
          sliderInput("P_p", "Producción potencial:", min = 0, max = 100, value = 59.59)
        ),
        wellPanel(
          h3("Shocks"),
          numericInput("num_variables", "Número de variables para Shocks:",
                       value = 1, min = 1, max = 10),
          
          uiOutput("shock_input_pairs")
        )
      ),
      
      mainPanel(
        plotOutput("plot_modelo_1"),
        plotOutput("plot_modelo_2"),
        plotOutput("plot_modelo_3"),
        tableOutput("datos"),
        tableOutput("datos_ajuste")
        
      )
    )
  )
  
  # Define el servidor
  server <- function(input, output) {
    
    observeEvent(input$num_variables, {
      # Generar los controles para seleccionar las variables para los shocks
      
      output$shock_input_pairs <- renderUI({
        shock_input_pairs <- lapply(1:input$num_variables, function(i) {
          tagList(
            selectInput(inputId = paste0("shock_variable_", i), 
                        label = paste("Variable", i, ":"),
                        choices = c("Propensión marginal a consumir (c)", 
                                    "Tipo impositivo (t)",
                                    "Sensibilidad de la inversión al tipo de interés (b)",
                                    "Sensibilidad de la demanda de dinero al nivel de renta (k)",
                                    "Sensibilidad de la demanda de dinero al tipo de interés (l)",
                                    "Consumo autónomo (C_0)",
                                    "Inversión autónoma (I_0)",
                                    "Compras del estado (G_0)",
                                    "Oferta de dinero (M_0)",
                                    "Nivel de precios (P_0)",
                                    "Producción potencial (P_p)")),
            sliderInput(inputId = paste0("shock_magnitude_", i), 
                        label = paste("% shock", i, ":"),
                        min = -50,
                        max=50,
                        value=5)
          )
          
        })
        return(shock_input_pairs)
      })
      
    })
    
    datos <- reactive({
      
      for(i in 1:input$num_variables){
        
        a <- input[[paste0("shock_variable_", i)]]
        variable <- regmatches(a, regexec("\\((.*?)\\)", a))[[1]][2]
        value <- input[[paste0("shock_magnitude_", i)]]
        
        if(i==1){
          variable_v <- variable
          value_v <- value
        }else{
          
          variable_v <- c(variable_v,variable)
          value_v <- c(value_v,value)
        }
      }
      
      datos_s <- data.frame(variable_v,value_v)
      names(datos_s) <- c("Variable","Shock (%)")
      
      names_v <- c("c", "t", "b", "k", "l", "C_0", "I_0", "G_0", "M_0", "P_0", "P_p")
      values_v <- c(input$c, input$t, input$b, input$k, input$l, input$C_0, input$I_0, input$G_0, input$M_0, input$P_0, input$P_p)
      
      
      datos_0 <- data.frame(Variable = names_v, "Valor inicial" = values_v)
      names(datos_0) <- c("Variable", "Valor inicial")
      
      datos_0 %>% 
        left_join(.,datos_s,by="Variable") -> datos
      
      return(datos)
      
    })
    
    
    output$plot_modelo_1 <- renderPlot({
      
      datos_0 <- datos()
      
      ## Antes del shock
      
      c <- datos_0[datos_0$Variable == "c",2]
      t <- datos_0[datos_0$Variable == "t",2]
      k <- datos_0[datos_0$Variable == "k",2]
      b <- datos_0[datos_0$Variable == "b",2]
      l <- datos_0[datos_0$Variable == "l",2]
      C_0 <- datos_0[datos_0$Variable == "C_0",2]
      I_0 <- datos_0[datos_0$Variable == "I_0",2]
      G_0 <- datos_0[datos_0$Variable == "G_0",2]
      M_0 <- datos_0[datos_0$Variable == "M_0",2]
      P_0 <- datos_0[datos_0$Variable == "P_0",2]
      P_p <- datos_0[datos_0$Variable == "P_p",2]
      
      z <- 1/(1-c*(1-t))
      A <- C_0+I_0+G_0
      
      
      ## Despues del shock
      
      c_s <- ifelse(is.na(datos_0[datos_0$Variable == "c",3]),c,c*(1+datos_0[datos_0$Variable == "c",3]/100))
      t_s <- ifelse(is.na(datos_0[datos_0$Variable == "t",3]),t,t*(1+datos_0[datos_0$Variable == "t",3]/100))
      k_s <- ifelse(is.na(datos_0[datos_0$Variable == "k",3]),k,k*(1+datos_0[datos_0$Variable == "k",3]/100))
      b_s <- ifelse(is.na(datos_0[datos_0$Variable == "b",3]),b,b*(1+datos_0[datos_0$Variable == "b",3]/100))
      l_s <- ifelse(is.na(datos_0[datos_0$Variable == "l",3]),l,l*(1+datos_0[datos_0$Variable == "l",3]/100))
      C_0_s <- ifelse(is.na(datos_0[datos_0$Variable == "C_0",3]),C_0,C_0*(1+datos_0[datos_0$Variable == "C_0",3]/100))
      I_0_s <- ifelse(is.na(datos_0[datos_0$Variable == "I_0",3]),I_0,I_0*(1+datos_0[datos_0$Variable == "I_0",3]/100))
      G_0_s <- ifelse(is.na(datos_0[datos_0$Variable == "G_0",3]),G_0,G_0*(1+datos_0[datos_0$Variable == "G_0",3]/100))
      M_0_s <- ifelse(is.na(datos_0[datos_0$Variable == "M_0",3]),M_0,M_0*(1+datos_0[datos_0$Variable == "M_0",3]/100))
      P_0_s <- ifelse(is.na(datos_0[datos_0$Variable == "P_0",3]),P_0,P_0*(1+datos_0[datos_0$Variable == "P_0",3]/100))
      P_p_s <- ifelse(is.na(datos_0[datos_0$Variable == "P_p",3]),P_p,P_p*(1+datos_0[datos_0$Variable == "P_p",3]/100))
      
      z_s <- 1/(1-c_s*(1-t_s))
      A_s <- C_0_s+I_0_s+G_0_s
      
      
      ## Curvas antes shock
      
      is_curve <- function(r) {z*(A-b*r)}
      lm_curve <- function(r) { (M_0*(1+l*r))/(k*P_0)}
      
      ## Curvas despues shock
      
      is_curve_s <- function(r) {z_s*(A_s-b_s*r)}
      lm_curve_s <- function(r) { (M_0_s*(1+l_s*r))/(k_s*P_0_s)}
      
      r_values <- seq(0, 10, 0.1)
      
      is_values <- is_curve(r_values)
      lm_values <- lm_curve(r_values)
      
      is_values_s <- is_curve_s(r_values)
      lm_values_s <- lm_curve_s(r_values)
      
      data.frame(is_values, lm_values, is_values_s, lm_values_s, r_values) %>% 
        pivot_longer(cols=1:4,names_to = "curva", values_to = "value") -> is_lm
      
      value_x <- max(is_lm$value)
      
      ggplot(is_lm)+
        geom_line(aes(y=r_values, x=value, color=curva, linetype = curva))+
        scale_colour_manual(values = c("is_values" = "red4", 
                                       "lm_values" = "blue4",
                                       "is_values_s" = "red1",
                                       "lm_values_s" = "blue1"), 
                            labels = c("is_values" = "Curva IS", 
                                       "lm_values"="Curva LM",
                                       "is_values_s" = "Curva IS tras shock",
                                       "lm_values_s" = "Curva LM tras shock"))+
        scale_linetype_manual(values = c("is_values" = "solid", 
                                         "lm_values" = "solid",
                                         "is_values_s" = "dashed",
                                         "lm_values_s" = "dashed"), 
                              labels = c("is_values" = "Curva IS", 
                                         "lm_values"="Curva LM",
                                         "is_values_s" = "Curva IS tras shock",
                                         "lm_values_s" = "Curva LM tras shock"))+
        xlab("Nivel de producción (Y)")+
        ylab("Tipo de interés real (r)")+
        ggtitle("Modelo IS-LM")+
        ylim(0,10)+
        scale_x_continuous(limits = c(0,value_x), breaks = seq(0,value_x,by=25))+
        theme_classic()+
        theme(panel.grid.major = element_blank(),
              text = element_text(size=18),
              axis.title = element_text(size = 18),
              legend.title = element_blank(),
              legend.position = "bottom",
              legend.text = element_text(),
              axis.text.x = element_text(angle = 45, hjust = 1),
              plot.title = element_text(face = "bold"),
              panel.grid.major.y = element_line())
      
    })
    
    output$plot_modelo_2 <- renderPlot({
      
      datos_0 <- datos()
      
      ## Antes del shock
      
      c <- datos_0[datos_0$Variable == "c",2]
      t <- datos_0[datos_0$Variable == "t",2]
      k <- datos_0[datos_0$Variable == "k",2]
      b <- datos_0[datos_0$Variable == "b",2]
      l <- datos_0[datos_0$Variable == "l",2]
      C_0 <- datos_0[datos_0$Variable == "C_0",2]
      I_0 <- datos_0[datos_0$Variable == "I_0",2]
      G_0 <- datos_0[datos_0$Variable == "G_0",2]
      M_0 <- datos_0[datos_0$Variable == "M_0",2]
      P_0 <- datos_0[datos_0$Variable == "P_0",2]
      P_p <- datos_0[datos_0$Variable == "P_p",2]
      
      z <- 1/(1-c*(1-t))
      A <- C_0+I_0+G_0
      
      
      ## Despues del shock
      
      c_s <- ifelse(is.na(datos_0[datos_0$Variable == "c",3]),c,c*(1+datos_0[datos_0$Variable == "c",3]/100))
      t_s <- ifelse(is.na(datos_0[datos_0$Variable == "t",3]),t,t*(1+datos_0[datos_0$Variable == "t",3]/100))
      k_s <- ifelse(is.na(datos_0[datos_0$Variable == "k",3]),k,k*(1+datos_0[datos_0$Variable == "k",3]/100))
      b_s <- ifelse(is.na(datos_0[datos_0$Variable == "b",3]),b,b*(1+datos_0[datos_0$Variable == "b",3]/100))
      l_s <- ifelse(is.na(datos_0[datos_0$Variable == "l",3]),l,l*(1+datos_0[datos_0$Variable == "l",3]/100))
      C_0_s <- ifelse(is.na(datos_0[datos_0$Variable == "C_0",3]),C_0,C_0*(1+datos_0[datos_0$Variable == "C_0",3]/100))
      I_0_s <- ifelse(is.na(datos_0[datos_0$Variable == "I_0",3]),I_0,I_0*(1+datos_0[datos_0$Variable == "I_0",3]/100))
      G_0_s <- ifelse(is.na(datos_0[datos_0$Variable == "G_0",3]),G_0,G_0*(1+datos_0[datos_0$Variable == "G_0",3]/100))
      M_0_s <- ifelse(is.na(datos_0[datos_0$Variable == "M_0",3]),M_0,M_0*(1+datos_0[datos_0$Variable == "M_0",3]/100))
      P_0_s <- ifelse(is.na(datos_0[datos_0$Variable == "P_0",3]),P_0,P_0*(1+datos_0[datos_0$Variable == "P_0",3]/100))
      P_p_s <- ifelse(is.na(datos_0[datos_0$Variable == "P_p",3]),P_p,P_p*(1+datos_0[datos_0$Variable == "P_p",3]/100))
      
      z_s <- 1/(1-c_s*(1-t_s))
      A_s <- C_0_s+I_0_s+G_0_s
      
      
      ## Curvas
      
      da_curve <- function(P) {z*(A-b*((z*A-M_0/k*P)/(b*z+M_0*l/(k*P))))}
      da_curve_s <- function(P) {z_s*(A_s-b_s*((z_s*A_s-M_0_s/k_s*P)/(b_s*z_s+M_0_s*l_s/(k_s*P))))}
      
      P_values <- seq(0.2,2.2,0.1)
      
      da_values <- da_curve(P_values)
      da_values_s <- da_curve_s(P_values)
      
      data.frame(P_values, da_values, da_values_s) %>% 
        pivot_longer(cols = 2:3,names_to = "curva",values_to = "values") -> da_oa
      
      value_x <- max(da_oa$values)
      
      ggplot(da_oa)+
        geom_line(aes(y=P_values, x=values, color=curva, linetype = curva))+
        scale_colour_manual(values = c("da_values" = "red4", 
                                       "da_values_s" = "red1"), 
                            labels = c("da_values" = "Curva DA",
                                       "da_values_s" = "Curva DA tras shock"))+
        scale_linetype_manual(values = c("da_values" = "solid", 
                                         "da_values_s" = "dashed"), 
                              labels = c("da_values" = "Curva DA",
                                         "da_values_s" = "Curva DA tras shock"))+
        geom_hline(yintercept = P_0, color="blue4")+
        geom_hline(yintercept = P_0_s, color="blue1")+
        geom_vline(xintercept = P_p, color = "darkgreen")+
        geom_vline(xintercept = P_p_s, color="green3", linetype = "dashed")+
        xlab("Nivel de producción (Y)")+
        ylab("Nivel de precios (P)")+
        ggtitle("Modelo OA-DA")+
        scale_x_continuous(limits = c(0, value_x*2), breaks = seq(0, value_x*2, by = 25))+
        theme_classic()+
        theme(panel.grid.major = element_blank(),
              text = element_text(size=18),
              axis.title = element_text(size = 18),
              legend.title = element_blank(),
              legend.position = "bottom",
              legend.text = element_text(),
              axis.text.x = element_text(angle = 45, hjust = 1),
              plot.title = element_text(face = "bold"),
              panel.grid.major.y = element_line())
      
    })
    
    
    output$plot_modelo_3 <- renderPlot({
      
      datos_0 <- datos()
      
      ## Antes del shock
      
      c <- datos_0[datos_0$Variable == "c",2]
      t <- datos_0[datos_0$Variable == "t",2]
      k <- datos_0[datos_0$Variable == "k",2]
      b <- datos_0[datos_0$Variable == "b",2]
      l <- datos_0[datos_0$Variable == "l",2]
      C_0 <- datos_0[datos_0$Variable == "C_0",2]
      I_0 <- datos_0[datos_0$Variable == "I_0",2]
      G_0 <- datos_0[datos_0$Variable == "G_0",2]
      M_0 <- datos_0[datos_0$Variable == "M_0",2]
      P_0 <- datos_0[datos_0$Variable == "P_0",2]
      P_p <- datos_0[datos_0$Variable == "P_p",2]
      
      z <- 1/(1-c*(1-t))
      A <- C_0+I_0+G_0
      
      
      ## Despues del shock
      
      c_s <- ifelse(is.na(datos_0[datos_0$Variable == "c",3]),c,c*(1+datos_0[datos_0$Variable == "c",3]/100))
      t_s <- ifelse(is.na(datos_0[datos_0$Variable == "t",3]),t,t*(1+datos_0[datos_0$Variable == "t",3]/100))
      k_s <- ifelse(is.na(datos_0[datos_0$Variable == "k",3]),k,k*(1+datos_0[datos_0$Variable == "k",3]/100))
      b_s <- ifelse(is.na(datos_0[datos_0$Variable == "b",3]),b,b*(1+datos_0[datos_0$Variable == "b",3]/100))
      l_s <- ifelse(is.na(datos_0[datos_0$Variable == "l",3]),l,l*(1+datos_0[datos_0$Variable == "l",3]/100))
      C_0_s <- ifelse(is.na(datos_0[datos_0$Variable == "C_0",3]),C_0,C_0*(1+datos_0[datos_0$Variable == "C_0",3]/100))
      I_0_s <- ifelse(is.na(datos_0[datos_0$Variable == "I_0",3]),I_0,I_0*(1+datos_0[datos_0$Variable == "I_0",3]/100))
      G_0_s <- ifelse(is.na(datos_0[datos_0$Variable == "G_0",3]),G_0,G_0*(1+datos_0[datos_0$Variable == "G_0",3]/100))
      M_0_s <- ifelse(is.na(datos_0[datos_0$Variable == "M_0",3]),M_0,M_0*(1+datos_0[datos_0$Variable == "M_0",3]/100))
      P_0_s <- ifelse(is.na(datos_0[datos_0$Variable == "P_0",3]),P_0,P_0*(1+datos_0[datos_0$Variable == "P_0",3]/100))
      P_p_s <- ifelse(is.na(datos_0[datos_0$Variable == "P_p",3]),P_p,P_p*(1+datos_0[datos_0$Variable == "P_p",3]/100))
      
      z_s <- 1/(1-c_s*(1-t_s))
      A_s <- C_0_s+I_0_s+G_0_s
      
      
      ## Curvas
      
      da_curve <- function(P) {z*(A-b*((z*A-M_0/k*P)/(b*z+M_0*l/(k*P))))}
      money_d <- function(Y,r) {k*Y/(1+l*r)}
      money_s <- function(r) {M_0/P_0}
      
      da_curve_s <- function(P) {z_s*(A_s-b_s*((z_s*A_s-M_0_s/k_s*P)/(b_s*z_s+M_0_s*l_s/(k_s*P))))}
      money_d_s <- function(Y,r) {k_s*Y/(1+l_s*r)}
      money_s_s <- function(r) {M_0_s/P_0_s}
      
      r_values <- seq(0, 10, 0.1)
      
      money_d_value <- money_d(da_curve(P_0),r_values)
      money_s_value <- money_s(r_values)
      
      money_d_value_s <- money_d_s(da_curve_s(P_0_s),r_values)
      money_s_value_s <- money_s_s(r_values)
      
      
      data.frame(r_values, money_d_value, money_s_value, money_d_value_s, money_s_value_s) %>% 
        pivot_longer(cols=2:5,names_to = "variable", values_to = "valor") -> monetario
      
      
      ggplot(monetario)+
        geom_line(aes(y=r_values, x=valor, color=variable, linetype = variable))+
        scale_colour_manual(values = c("money_d_value" = "red4", "money_s_value" = "blue4", "money_d_value_s" = "red1", "money_s_value_s" = "blue1"), 
                            labels = c("money_d_value" = "Demanda de saldos reales", "money_s_value"="Oferta monetaria real", "money_d_value_s" = "Demanda de saldos reales tras shock", "money_s_value_s" = "Oferta monetaria real tras shock"))+
        scale_linetype_manual(values = c("money_d_value" = "solid", "money_s_value" = "solid", "money_d_value_s" = "dashed", "money_s_value_s" = "dashed"), 
                              labels = c("money_d_value" = "Demanda de saldos reales", "money_s_value"="Oferta monetaria real", "money_d_value_s" = "Demanda de saldos reales tras shock", "money_s_value_s" = "Oferta monetaria real tras shock"))+
        xlab("Demanda de saldos monetarios reales")+
        ylab("Tipo de interés real (r)")+
        ggtitle("Mercado monetario")+
        scale_x_continuous(limits = c(0, 200), breaks = seq(0, 200, by = 25))+
        theme_classic()+
        theme(panel.grid.major = element_blank(),
              text = element_text(size=18),
              axis.title = element_text(size = 18),
              legend.title = element_blank(),
              legend.position = "bottom",
              legend.text = element_text(),
              axis.text.x = element_text(angle = 45, hjust = 1),
              plot.title = element_text(face = "bold"),
              panel.grid.major.y = element_line())
      
    })
    
    
    equilibrios <- reactive({
      
      datos_0 <- datos()
      
      ## Antes del shock
      
      c <- datos_0[datos_0$Variable == "c",2]
      t <- datos_0[datos_0$Variable == "t",2]
      k <- datos_0[datos_0$Variable == "k",2]
      b <- datos_0[datos_0$Variable == "b",2]
      l <- datos_0[datos_0$Variable == "l",2]
      C_0 <- datos_0[datos_0$Variable == "C_0",2]
      I_0 <- datos_0[datos_0$Variable == "I_0",2]
      G_0 <- datos_0[datos_0$Variable == "G_0",2]
      M_0 <- datos_0[datos_0$Variable == "M_0",2]
      P_0 <- datos_0[datos_0$Variable == "P_0",2]
      P_p <- datos_0[datos_0$Variable == "P_p",2]
      
      z <- 1/(1-c*(1-t))
      A <- C_0+I_0+G_0
      
      
      ## Despues del shock
      
      c_s <- ifelse(is.na(datos_0[datos_0$Variable == "c",3]),c,c*(1+datos_0[datos_0$Variable == "c",3]/100))
      t_s <- ifelse(is.na(datos_0[datos_0$Variable == "t",3]),t,t*(1+datos_0[datos_0$Variable == "t",3]/100))
      k_s <- ifelse(is.na(datos_0[datos_0$Variable == "k",3]),k,k*(1+datos_0[datos_0$Variable == "k",3]/100))
      b_s <- ifelse(is.na(datos_0[datos_0$Variable == "b",3]),b,b*(1+datos_0[datos_0$Variable == "b",3]/100))
      l_s <- ifelse(is.na(datos_0[datos_0$Variable == "l",3]),l,l*(1+datos_0[datos_0$Variable == "l",3]/100))
      C_0_s <- ifelse(is.na(datos_0[datos_0$Variable == "C_0",3]),C_0,C_0*(1+datos_0[datos_0$Variable == "C_0",3]/100))
      I_0_s <- ifelse(is.na(datos_0[datos_0$Variable == "I_0",3]),I_0,I_0*(1+datos_0[datos_0$Variable == "I_0",3]/100))
      G_0_s <- ifelse(is.na(datos_0[datos_0$Variable == "G_0",3]),G_0,G_0*(1+datos_0[datos_0$Variable == "G_0",3]/100))
      M_0_s <- ifelse(is.na(datos_0[datos_0$Variable == "M_0",3]),M_0,M_0*(1+datos_0[datos_0$Variable == "M_0",3]/100))
      P_0_s <- ifelse(is.na(datos_0[datos_0$Variable == "P_0",3]),P_0,P_0*(1+datos_0[datos_0$Variable == "P_0",3]/100))
      P_p_s <- ifelse(is.na(datos_0[datos_0$Variable == "P_p",3]),P_p,P_p*(1+datos_0[datos_0$Variable == "P_p",3]/100))
      
      z_s <- 1/(1-c_s*(1-t_s))
      A_s <- C_0_s+I_0_s+G_0_s
      
      
      ## Curvas
      
      da_curve <- function(P) {z*(A-b*((z*A-M_0/k*P)/(b*z+M_0*l/(k*P))))}
      money_d <- function(Y,r) {k*Y/(1+l*r)}
      money_s <- function(r) {M_0/P_0}
      
      da_curve_s <- function(P) {z_s*(A_s-b_s*((z_s*A_s-M_0_s/k_s*P)/(b_s*z_s+M_0_s*l_s/(k_s*P))))}
      money_d_s <- function(Y,r) {k_s*Y/(1+l_s*r)}
      money_s_s <- function(r) {M_0_s/P_0_s}
      
      Y <- da_curve(P_0)
      Y_s <- da_curve_s(P_0_s)
      
      r <- (1/(b*z))*(z*A-Y)
      r_s <- (1/(b_s*z_s))*(z_s*A_s-Y_s)
      
      data.frame(`Variable` = c("Nivel de producción", "Tipo de interés (r)", "Nivel de precios (P)"),
                 `Antes del shock` = c(Y,r, P_0),
                 `Despues del shock` = c(Y_s,r_s, P_0_s),
                 `Tasa de variación` = c((Y_s-Y)*100/Y,(r_s-r)*100/r, (P_0_s-P_0)*100/P_0)) -> datos_eq
      
      return(datos_eq)
      
    })
    
    output$datos <- renderTable({
      
      equilibrios()
    }, caption = "Equilibrio en el corto plazo")
    
    
    
    equilibrios_medio <- reactive({
      
      datos_0 <- datos()
      
      ## Antes del shock
      
      c <- datos_0[datos_0$Variable == "c",2]
      t <- datos_0[datos_0$Variable == "t",2]
      k <- datos_0[datos_0$Variable == "k",2]
      b <- datos_0[datos_0$Variable == "b",2]
      l <- datos_0[datos_0$Variable == "l",2]
      C_0 <- datos_0[datos_0$Variable == "C_0",2]
      I_0 <- datos_0[datos_0$Variable == "I_0",2]
      G_0 <- datos_0[datos_0$Variable == "G_0",2]
      M_0 <- datos_0[datos_0$Variable == "M_0",2]
      P_0 <- datos_0[datos_0$Variable == "P_0",2]
      P_p <- datos_0[datos_0$Variable == "P_p",2]
      
      z <- 1/(1-c*(1-t))
      A <- C_0+I_0+G_0
      
      
      ## Despues del shock
      
      c_s <- ifelse(is.na(datos_0[datos_0$Variable == "c",3]),c,c*(1+datos_0[datos_0$Variable == "c",3]/100))
      t_s <- ifelse(is.na(datos_0[datos_0$Variable == "t",3]),t,t*(1+datos_0[datos_0$Variable == "t",3]/100))
      k_s <- ifelse(is.na(datos_0[datos_0$Variable == "k",3]),k,k*(1+datos_0[datos_0$Variable == "k",3]/100))
      b_s <- ifelse(is.na(datos_0[datos_0$Variable == "b",3]),b,b*(1+datos_0[datos_0$Variable == "b",3]/100))
      l_s <- ifelse(is.na(datos_0[datos_0$Variable == "l",3]),l,l*(1+datos_0[datos_0$Variable == "l",3]/100))
      C_0_s <- ifelse(is.na(datos_0[datos_0$Variable == "C_0",3]),C_0,C_0*(1+datos_0[datos_0$Variable == "C_0",3]/100))
      I_0_s <- ifelse(is.na(datos_0[datos_0$Variable == "I_0",3]),I_0,I_0*(1+datos_0[datos_0$Variable == "I_0",3]/100))
      G_0_s <- ifelse(is.na(datos_0[datos_0$Variable == "G_0",3]),G_0,G_0*(1+datos_0[datos_0$Variable == "G_0",3]/100))
      M_0_s <- ifelse(is.na(datos_0[datos_0$Variable == "M_0",3]),M_0,M_0*(1+datos_0[datos_0$Variable == "M_0",3]/100))
      P_0_s <- ifelse(is.na(datos_0[datos_0$Variable == "P_0",3]),P_0,P_0*(1+datos_0[datos_0$Variable == "P_0",3]/100))
      P_p_s <- ifelse(is.na(datos_0[datos_0$Variable == "P_p",3]),P_p,P_p*(1+datos_0[datos_0$Variable == "P_p",3]/100))
      
      z_s <- 1/(1-c_s*(1-t_s))
      A_s <- C_0_s+I_0_s+G_0_s
      
      
      ## Curvas
      
      da_curve <- function(P) {z*(A-b*((z*A-M_0/k*P)/(b*z+M_0*l/(k*P))))}
      money_d <- function(Y,r) {k*Y/(1+l*r)}
      money_s <- function(r) {M_0/P_0}
      
      da_curve_s <- function(P) {z_s*(A_s-b_s*((z_s*A_s-M_0_s/k_s*P)/(b_s*z_s+M_0_s*l_s/(k_s*P))))}
      money_d_s <- function(Y,r) {k_s*Y/(1+l_s*r)}
      money_s_s <- function(r) {M_0_s/P_0_s}
      
      Y_s <- da_curve_s(P_0_s)
      Y_pot <- P_p_s
      
      r_s <- (1/(b*z))*(z*A-Y_s)
      r_medio <- (1/(b_s*z_s))*(z_s*A_s-Y_pot)
      P_medio <- M_0_s*(1+l_s*r_medio)/(k_s*Y_pot)
      
      ### falta añadir los precios, definir la tabla y ver si sale todo correcto
      
      data.frame(`Variable` = c("Nivel de producción", "Tipo de interés (r)", "Nivel de precios (P)"),
                 `Después del shock` = c(Y_s,r_s,P_0_s),
                 `Ajuste en el medio plazo` = c(Y_pot,r_medio,P_medio),
                 `Tasa de variación` = c((Y_pot-Y_s)*100/Y_s,(r_medio-r_s)*100/r_s, (P_medio - P_0_s)*100/P_0_s)) -> datos_eq_medio
      
      return(datos_eq_medio)
      
    })
    
    
    output$datos_ajuste <- renderTable({
      
      equilibrios_medio()
    }, caption = "Tendencia en el medio plazo")
    
    
  }
  
  # Ejecuta la aplicación
  shinyApp(ui = ui, server = server)
  
  
