
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
  h2('Convierte números de punto flotante a binario'),
  # numericInput('numero','Número a convertir', 1),
  textInput('numero','Número a convertir', '11000000101101000000000000000000'),
  actionButton('convertir','Convertir'),br(),
  h5(tags$b('Número convertido')),
  textOutput('convertido')
  
)

server <- function(input, output, session) {
  convertido <- eventReactive(input$convertir, {
    flotante <- input$numero
    signo <- substr(flotante, 1, 1)
    exponente <- substr(flotante, 2, 9)
    mexp <- 0
    for(i in nchar(exponente):1) {mexp <- mexp + as.numeric(substr(exponente,i,i))*(2**(nchar(exponente)-i))}
    mexp <- mexp-127
    onum <- substr(flotante,10,nchar(flotante))
    enteros <- paste0('1',substr(onum,1,mexp))
    decimales <- substr(onum,mexp+1,nchar(onum))
    decimales <- paste0('0.',gsub('0+?$','',decimales))
    ment <- as.numeric(convertir(as.numeric(enteros),F))
    mdec <- as.numeric(convertir(as.numeric(decimales),F))
    mnum <- ifelse(signo=='1',-1,1)*(ment+mdec)
    return(mnum)
  })
  output$convertido <- renderText({
    if(is.null(convertido())) return()
    convertido()
  })
}

shinyApp(ui, server)
