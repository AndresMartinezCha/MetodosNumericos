library(shiny)
library(ggplot2)

ui <- fluidPage(
  h2('Obtiene una raíz de un polinomio con el método de punto fijo'),
  textInput('funcion', 'Polinomio', 'x^2+2*x-3', placeholder = 'x^2+2*x-3'),
  textInput('guncion', 'g(x)', '(3-x^2)/2', placeholder = 'x^2-4'),
  h3('Valores iniciales'),
  numericInput('inicial','Valor inicial',0),
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
    eval(parse(text = paste0('f<-function(x) {',input$funcion,'}')))
    eval(parse(text = paste0('g<-function(x) {',input$guncion,'}')))
    inicial <- input$inicial
    error <- input$error
    n_max <- input$n_max
    
    merror <- error+1
    i <- 0
    encontrado <- F
    valor <- 1
    tabla <- data.frame(matrix(ncol = 2, nrow = 0))
    x <- inicial
    while (i<n_max&!encontrado) {
      gx <- g(x)
      fx <- f(gx)
      # print(gx)
      if(gx==Inf) return(list('La ecuación no converge', NULL, NULL))
      i <- i+1
      tabla[i,1] <- gx
      tabla[i,2] <- fx
      if(fx==0) valor <- x
      xm <- gx
      if(xm==0) merror <- 0 else merror <- abs((xm-x)/xm)
      x <- xm
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
    if(is.null(raiz()[[2]])) return()
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
    if(is.null(raiz()[[2]])) return()
    div(br(), p(style='text-align: center;width: 40px;', rv$pagina))
  })
  observeEvent(input$pri, {if(is.null(raiz())) return(); rv$pagina <- 1})
  observeEvent(input$ult, {if(is.null(raiz())) return(); rv$pagina <- nrow(raiz()[[2]])})
  observeEvent(input$ant, {if(is.null(raiz())) return(); if(rv$pagina!=1) rv$pagina <- rv$pagina-1})
  observeEvent(input$sig, {if(is.null(raiz())) return(); if(rv$pagina!=nrow(raiz()[[2]])) rv$pagina <- rv$pagina+1})
  
  output$grafica <- renderPlot({
    if(is.null(raiz())) return()
    if(is.null(raiz()[[2]])) return()
    isolate({eval(parse(text = paste0('f<-function(x) {',input$funcion,'}')))})
    isolate({minf <- input$inicial})
    isolate({msup <- input$inicial})
    minf <- min(minf, raiz()[[3]])
    msup <- max(msup, raiz()[[3]])
    xs <- seq(minf-2,msup+2,0.01)
    ys <- f(xs)
    
    tabla <- raiz()[[2]]
    
    datos <- data.frame(x=xs, y=ys)
    i <- rv$pagina
    punto <- tabla[i, ]
    m <- tabla[i, 4]
    b <- tabla[i,2]-m*tabla[i,1]
    px <- punto[1,1]
    py <- punto[1,2]
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
      # geom_point(aes(x=px, y=py), colour="red")
      annotate('point', x=px, y=py, colour="red")
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

