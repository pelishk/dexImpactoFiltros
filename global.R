library(dplyr)

datosTotales<-readRDS('data/baja california completo.rds')

datosTotales$dummy<-rep(1,nrow(datosTotales))
datosTotales$montoP<-datosTotales$monto

datosTotales<-datosTotales %>%
  filter(!is.na(lon)) %>%
  filter(pagoPaisNombre=='MEXICO')

names(datosTotales)[32:33]<-c('longitude','latitude')
datosAno<-datosTotales %>%
  filter(envioAno==2015&monto<=100000)




datos<-datosTotales %>%
  filter(envioAno==2015) %>%
  group_by(envioSucursalNombre) %>%
  summarise(latitude=min(latitude),
            longitude=min(longitude),
            montoPromedio=mean(monto),
            comisionPromedio=sum(comision)/sum(monto),
            montoTotal=sum(monto),
            estado=unique(envioEstadoNombre),
            ciudad=unique(envioCiudadNombre)
            ) %>%
  as.data.frame

datos$ID<-1:nrow(datos)

cuantiles<-quantile(datos$montoTotal,seq(0,1,.25))
  datos<-datos %>%
    mutate(
      percentil=cut(
        montoTotal,
        breaks=c(-Inf,cuantiles[2],cuantiles[3],cuantiles[4],Inf),
        labels=c('1er cuantil','2do cuantil','3er cuantil','4to cuantil')
      )
    )

  
  

  
  
  

# names(datos)[39:40]<-c('latitude','longitude')

#blaj
# 
# datos$Nombre.de.la.Unidad.Económica
# 
# write.csv(datos[,c(1,2,3)],'archivo para limpieza de datos.csv')
# datos2<-read.csv("archivo para limpieza de datos (limpios).csv")

# 
# llave<-match(datos$ID,datos2$ID)
# 
# datos$casa<-datos2$casa[llave]
# 
# head(datos)
# 
# 

# datos$casaGrupo<-datos$casa
# levels(datos$casaGrupo)<-c("otro","otro",
#                            "otro","empeño fácil",
#                            "otro","first cash",
#                            "micro apoyos","otro",
#                            "otro","otro",
#                            "monte de piedad","otro",
#                            "otro","monte pío",
#                            "monte providencia","otro",
#                            "otro","prenda lana",
#                            "otro","prenda mex",
#                            "otro","otro",
#                            "otro","otro",
#                            "presta prenda","otro",
#                            "otro"
#                            )
# 
#  saveRDS(datos,"data/casas de empeno df.rds")
# 
# datos$Nombre.de.la.Unidad.Económica<-as.character(datos$Nombre.de.la.Unidad.Económica)
# datos$Razón.social<-as.character(datos$Razón.social)
# datos$Descripcion.estrato.personal.ocupado<-as.character(datos$Descripcion.estrato.personal.ocupado)

# 
# 
# 
# monte<-read.table("coordenadas_Monte-de-piedad.txt",header=T,sep=',')
# names(monte)
# for(i in 1:nrow(monte)){
#   datos[(nrow(datos)+1),]<-NA
#   datos$casa[nrow(datos)]<-'monte de piedad'
#   datos$latitude[nrow(datos)]<-monte$Lat[i]
#   datos$longitude[nrow(datos)]<-monte$Lon[i]
# }
# 
# saveRDS(datos,"data/casas de empeno df.rds")

#datos$Nombre.de.la.Unidad.Económica[432:482]<-'Nacional monte de piedad'

# datos$latitude[432:482]<-monte$Lon
# datos$longitude[432:482]<-monte$Lat

# datos$ID[432:482]<-1:51
