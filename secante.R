library(shiny)
library(ggplot2)

ui <- fluidPage(
  h2('Obtiene una raíz de un polinomio con el método de la secante'),
  textInput('funcion', 'Polinomio', 'x^2-4', placeholder = 'x^2-4'),
  h3('Valores iniciales'),
  numericInput('x0',HTML('x<sub>0</sub>'),5),
  numericInput('x1',HTML('x<sub>1</sub>'),7),
  numericInput('error','Error tolerado', '0.0001'),
  numericInput('n_max','Número máximo de iteraciones', '1000'),
  actionButton('buscar','Buscar raiz'),br(),br(),
  span(textOutput('resultado'), style='font-size:20px;'),br(),
  uiOutput('controles'),br(),
  plotOutput('grafica', dblclick = "grafica_dblclick", brush = brushOpts(id = "grafica_brush", resetOnNew = TRUE))
)

server <- function(input, output, session) {
  rv <- reactiveValues()
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  raiz <- eventReactive(input$buscar,{
    rv$pagina <- 1
    eval(parse(text=paste0('f<-function(x) ',input$funcion,'')))
    x0 <- input$x0
    x1 <- input$x1
    error <- input$error
    n_max <- input$n_max
    
    merror <- error+1
    i <- 0
    encontrado <- F
    valor <- 1
    tabla <- data.frame(matrix(ncol = 4, nrow = 0))
    while (i<n_max&!encontrado) {
      fx0 <- f(x0)
      fx1 <- f(x1)
      i <- i+1
      tabla[i,1] <- x0
      tabla[i,2] <- fx0
      tabla[i,3] <- x1
      tabla[i,4] <- fx1
      if(fx0==0) valor <- x0
      if(fx1==0) valor <- x1
      xm <- x1-(fx1*(x0-x1))/(fx0-fx1)
      if(xm==0) merror <- 0 else merror <- abs((xm-x1)/xm)
      x0 <- x1
      x1 <- xm
      if(merror<error) valor <- xm
      encontrado <- valor==xm
    }
    return(list(paste('El valor de la raiz es', round(valor,4)), tabla, valor))
  })
  
  output$resultado <- renderText({
    if(is.null(raiz())) return()
    raiz()[[1]]
  })
  
  output$controles <- renderUI({
    if(is.null(raiz())) return()
    div(style='display:flex;', 
        actionButton('pri','<<'),
        actionButton('ant','<'),
        uiOutput('pag'),
        actionButton('sig','>'),
        actionButton('ult','>>')
    )
  })
  
  output$pag <- renderUI({
    if(is.null(raiz())) return()
    div(br(), p(style='text-align: center;width: 40px;', rv$pagina))
  })
  observeEvent(input$pri, {if(is.null(raiz())) return(); rv$pagina <- 1})
  observeEvent(input$ult, {if(is.null(raiz())) return(); rv$pagina <- nrow(raiz()[[2]])})
  observeEvent(input$ant, {if(is.null(raiz())) return(); if(rv$pagina!=1) rv$pagina <- rv$pagina-1})
  observeEvent(input$sig, {if(is.null(raiz())) return(); if(rv$pagina!=nrow(raiz()[[2]])) rv$pagina <- rv$pagina+1})
  
  output$grafica <- renderPlot({
    if(is.null(raiz())) return()
    isolate({eval(parse(text = paste0('f<-function(x) {',input$funcion,'}')))})
    isolate({minf <- input$x0})
    isolate({msup <- input$x1})
    minf <- min(minf, raiz()[[3]])
    msup <- max(msup, raiz()[[3]])
    xs <- seq(minf-2,msup+2,0.01)
    ys <- f(xs)
    
    tabla <- raiz()[[2]]
    
    datos <- data.frame(x=xs, y=ys)
    i <- rv$pagina
    m <- (tabla[i, 4]-tabla[i, 2])/(tabla[i, 3]-tabla[i, 1])
    b <- tabla[i,4]-m*tabla[i,3]
    p <- ggplot(datos) +
      geom_line(aes(x=x, y=y), col='blue') +
      coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE) +
      geom_hline(yintercept=0) +
      geom_vline(xintercept=0) +
      theme(
        panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent')
      ) +
      geom_abline(intercept = b, slope = m, col='red')
    p
  })
  
  observeEvent(input$grafica_dblclick, {
    brush <- input$grafica_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
}

shinyApp(ui, server)

