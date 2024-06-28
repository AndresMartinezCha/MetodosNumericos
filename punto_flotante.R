library(shiny)

convertir <- function(x, dec_a_bin=T, n_dec=10){
  numero <- x
  enteros <- as.integer(abs(numero))
  if(grepl('\\.',numero)) {
    decimales <- as.numeric(paste0('0.',gsub('^.+?\\.','',numero)))
  } else {
    decimales <- 0
  }
  
  if(dec_a_bin){
    conv_ent <- ''
    while (enteros!=0) {
      conv_ent <- paste0(enteros%%2, conv_ent)
      enteros <- as.integer(enteros/2)
    }
    
    conv_dec <- ''
    i <- 0
    while(i<=n_dec&decimales!=0){
      conv_dec <- paste0(conv_dec, as.integer(decimales*2))
      decimales <- decimales*2-as.integer(decimales*2)
      i <- i+1
    }
  }else{
    conv_ent <- 0
    i <- 0
    while (enteros!=0) {
      conv_ent <- conv_ent+(enteros%%10)*(2**i)
      enteros <- as.integer(enteros/10)
      i <- i+1
    }
    
    conv_dec <- 0
    i <- 1
    while (decimales!=0&i<=n_dec) {
      conv_dec <- conv_dec+as.integer(decimales*10)/(2**i)
      decimales <- decimales*10-as.integer(decimales*10)
      i <- i+1
    }
    conv_dec <- gsub('0.','',as.character(conv_dec))
  }
  convertido <- paste0(conv_ent,'.',conv_dec)
  if(numero<0) convertido <- paste0('-', convertido)
  while(substr(convertido,nchar(convertido),nchar(convertido))=='0') 
    convertido <- substr(convertido,0,nchar(convertido)-1)
  if(substr(convertido,1,1)=='.')
    convertido <- paste0('0',convertido)
  if(substr(convertido,nchar(convertido),nchar(convertido))=='.') 
    convertido <- substr(convertido,0,nchar(convertido)-1)
  return(convertido)
}

ui <- fluidPage(
  h2('Convierte números de decimal a binario y de binario a decimal'),
  radioButtons('tipo','Tipo de conversion', c('Decimal a binario'='dec','Binario a decimal'='bin')),
  numericInput('n_dec','Numero de decimales',10),
  numericInput('numero','Número a convertir', 1),
  actionButton('convertir','Convertir'),br(),
  h5(tags$b('Número convertido')),
  textOutput('convertido'),
  h2('Representación de un número decimal en punto flotante'),
  numericInput('decimal','Número decimal',1),
  numericInput('exponente','Exponente',0),
  actionButton('representar','Representar'),br(),br(),
  div(
    style='display: flex;', 
    div(style='background-color:green;color:white;text-align: center;', 'Signo',br(),tags$b(uiOutput('signo'))), 
    div(style='background-color:red;color:white;text-align: center;', 'Exponente',br(),tags$b(uiOutput('expon'))),
    div(style='background-color:blue;color:white;text-align: center;', 'Mantissa',br(),tags$b(uiOutput('mantis')))
  )
)

server <- function(input, output, session) {
  f_conv <- eventReactive(input$convertir,{
    if(is.null(input$numero)) return()
    if(input$tipo=='dec') 
      convertido <- convertir(input$numero, dec_a_bin=T, input$n_dec)
    else 
      convertido <- convertir(input$numero, dec_a_bin=F, input$n_dec)
    return(convertido)
  })
  
  f_repr <- eventReactive(input$representar, {
    numero <- input$decimal*(10**input$exponente)
    # numero <- input$decimal
    binario <- convertir(abs(numero), T, 24)
    if(!grepl('\\.', binario)) binario <- paste0(binario,'.')
    normal <- binario
    numeros <- gsub('\\D','',binario)
    cuantos <- gsub('(?=\\.).*?$','',binario, perl = T)
    i <- 0
    while (nchar(cuantos)>1|cuantos=='0') {
      if(nchar(cuantos)>1) {
        normal <- paste0(substr(numeros,1,nchar(cuantos)-1),'.',substr(numeros,nchar(cuantos),nchar(numeros)))
        i <- i+1
      }else{
        normal <- paste0(substr(normal,3,3),'.',substr(normal,4,nchar(normal)))
        i <- i-1
      }
      cuantos <- gsub('(?=\\.).*?$','',normal, perl = T)
    }
    m_exp <- i
    m_exp <- 127+m_exp
    m_exp <- as.numeric(convertir(m_exp, T, 24))
    m_exp <- sprintf('%08d', m_exp)
    signo <- ifelse(numero<0,'1','0')
    mantissa <- gsub('^..','',normal)
    mantissa <- substr(paste0(mantissa, paste(rep('0',23), collapse = '')),1,23)
    # final <- paste0(signo, m_exp, mantissa)
    # return(final)
    return(list(signo,m_exp,mantissa))
  })
  
  output$convertido <- renderText({
    if(is.null(f_conv())) return()
    f_conv()
  })
  
  # output$representado <- renderText({
  #   if(is.null(f_repr())) return()
  #   f_repr()
  # })
  
  output$signo <- renderUI({
    if(is.null(f_repr())) return()
    p(f_repr()[[1]])
  })
  output$expon <- renderUI({
    if(is.null(f_repr())) return()
    p(f_repr()[[2]])
  })
  output$mantis <- renderUI({
    if(is.null(f_repr())) return()
    p(f_repr()[[3]])
  })
}

shinyApp(ui, server)

