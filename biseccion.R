library(shiny)

ui <- fluidPage(
  h2('Obtiene una raíz de un polinomio con el método de Bisección'),
  textInput('funcion', 'Polinomio', 'x^2-4', placeholder = 'x^2-4'),
  h3('Intervalo'),
  numericInput('inf','Inferior',-1),
  numericInput('sup','Superior',3),
  numericInput('error','Error tolerado', '0.01'),
  numericInput('n_max','Número máximo de iteraciones', '1000'),
  actionButton('buscar','Buscar raiz'),
  textOutput('resultado')
)

server <- function(input, output, session) {
  raiz <- eventReactive(input$buscar,{
    eval(parse(text = paste0('f<-function(x) {',input$funcion,'}')))
    a <- input$inf
    b <- input$sup
    if(f(a)*f(b)>0) return('El intervalo es incorrecto')
    error <- input$error
    n_max <- input$n_max
    
    merror <- error+1
    i <- 0
    valor <- 1
    while (i<n_max&merror>error&valor!=0) {
      fx1 <- f(a)
      fx2 <- f(b)
      if(fx1==0) valor <- a
      if(fx2==0) valor <- b
      xm <- (a+b)/2
      if(f(xm)<0) a <- xm else b <- xm
      i <- i+1
    }
    return(paste('El valor de la raiz es', round(valor,2)))
  })
  
  output$resultado <- renderText({
    if(is.null(raiz())) return()
    raiz()
  })
}

shinyApp(ui, server)

