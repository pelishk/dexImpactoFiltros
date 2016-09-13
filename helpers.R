cruces_descriptivo <- function(datos,variables_filtro = NULL,variables = NULL,preguntas = NULL,
                               regresar_graficas=F){
  require(dplyr)
  require(ggplot2)
  
  dat <- datos[,which(sapply(1:ncol(datos), function(k) {  is.factor(datos[[k]]) }))]
  
  if(length(variables_filtro) > 0){
    
    for(k in 1:ncol(datos[,variables_filtro])){
      cat(length(levels(datos[,variables_filtro[k]])),
          "niveles de la variable",variables_filtro[k],": ",levels(datos[,variables_filtro[k]]) ,"\n")
    }
    
    filtro <- list()
    for(k in 1:length(variables_filtro))  {
      filtro[[k]] <-readline(prompt =   paste("Selecciona el filtro de la variable",variables_filtro[k],"\n"))
    }
    
    ll <- list()
    for(k in 1:length(variables_filtro)){
      ll[[k]] <- which(datos[[  variables_filtro[k] ]] == filtro[[k]])
    }
    
    fil = Reduce(intersect,ll)
    fil = unique(fil)
    
    dat = datos[fil,]
    
    if(nrow(dat) == 0) { stop("No hay observaciones que cumplan con el filtro")}
    if(nrow(dat) > 0) { cat("Con el filtro se tienen",nrow(dat),"observaciones")}
  }
  
  if(length(variables_filtro) == 0){
    cat("Se tienen",nrow(dat),"observaciones")
  }
  
  if(sum(variables %in% names(datos)) != length(variables)) {  
    for(k in 1:length(which(!variables %in% names(datos)))){
      warning(paste("La variable", variables[which(!variables %in% names(datos))[k]],"no se encuentra en los datos"))  
    }
    variables <- variables[which(variables %in% names(datos))]
  }
  
  if(length(variables) == 0){ return() }
  
  if(length(preguntas) == 0){
    gr <- list()
    for(k in 1:length(variables)){
      gr[[k]] <- descriptivo(datos = dat ,
                             variable = variables[k],
                             col1 = rgb(runif(1),runif(1),runif(1)) ,
                             col2 = rgb(runif(1),runif(1),runif(1)) ,
                             titulo = variables[k] )
    }
  }
  
  if(length(preguntas) != 0){
    i <- numeric()
    for(k in 1:length(variables)){
      i[k] <-  which(variables[k] == preguntas$Variables)
    }
    
    gr <- list()
    for(k in 1:length(variables)){
      gr[[k]] <- descriptivo(datos = dat ,
                             variable = variables[k],
                             col1 = rgb(runif(1),runif(1),runif(1)) ,
                             col2 = rgb(runif(1),runif(1),runif(1)) ,
                             titulo = as.character(preguntas[ i[k],2]) )
    }
  }
  
  #  for(k in seq(1,length(gr),2)){
  #    do.call(function(...){ multiplot(...,cols = 2)  } ,gr[seq(k,k+1)])
  #  }
  
  do.call(function(...){ multiplot(...,cols = 2)  } ,gr)
  
  if(regresar_graficas){
    return(gr)
  }
}

descriptivo <- function(datos,variable,col1="black",col2="black",titulo=""){
  require(ggplot2)
  g<-
    ggplot(data =   datos[!is.na(datos[[ variable ]]),] ) + 
    geom_bar(aes(datos[[variable]][!is.na(datos[[variable]])],
                 y=(..count../sum(..count..))*100),col=col1,fill=col2) +
    theme_classic() +xlab('')+ylab('') + 
    theme(axis.text.x=element_text(angle=90,hjust=1)) +
    ggtitle(titulo) +
    theme(plot.title = element_text(lineheight=.8, face="bold")) 
  
  return(g)
}


multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
