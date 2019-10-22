#######################################################################################
#Build burned pixels SPDF
burnedPixelDF <- function(pathIn){
  f_bd <- paste0(pathIn,'Burn_Date')
  f_bdu <- paste0(pathIn,'Burn_Date_Unc')
  f_qa <- paste0(pathIn,'QA')
  
  files_bd <- list.files(f_bd,pattern='.tif$',full.names=TRUE)
  name <- basename(files_bd)
  files_unc <- list.files(f_bdu,pattern='.tif$',full.names=TRUE)
  files_qa <- list.files(f_qa,pattern='.tif$',full.names=TRUE)
  if (length(files_bd) == length(files_unc) & length(files_bd) == length(files_qa)){
    print(".TIF Okay")
  } else {
    print("Error reading .TIF")
    break()
  }
  #Select years and tails
  year <- unique(substr(name, 19, 22))
  n <- 0#unique(substr(name, 18, 23))
  #Vector reclasificacion. Crear vector de días para la matriz de reclasificación. Se añaden los valores no data al vector
  rule_reclas <-  c(-Inf, 0, NA, 400, Inf, NA)
  rule_reclas2 <-  c(1, Inf, 1)
  m <- c(0, 100, 1,  101, 200, 2,  201, 300, 3)
  rm(p_incendio)
  # for (n in ntail){
  #   print(n)
  for (y in year){
    print(y)
    lista_acum <- files_bd[grepl(paste0(y),files_bd)]#files_bd[grepl(paste0("MCD64A1.A",y), files_bd)& grepl(paste0(".",n,"."), files_bd)]
    lista_unc <- files_unc[grepl(paste0(y),files_unc)]#iles_unc[grepl(paste0("MCD64A1.A",y), files_unc)& grepl(paste0(".",n,"."), files_unc)]
    lista_qa <- files_qa[grepl(paste0(y),files_qa)]#files_qa[grepl(paste0("MCD64A1.A",y), files_qa)& grepl(paste0(".",n,"."), files_qa)]
    print(lista_acum)
    print(lista_unc)
    print(lista_qa)
    for (i in 1:length(lista_acum)){
      name_raster_bd <- paste("raster_bd", i, sep="")
      #Acumulado fecha_quema. Crear recuento imagenes, llamar raster y reclasificar
      assign(name_raster_bd, raster(lista_acum[i])) 
      assign(name_raster_bd, reclassify(get(name_raster_bd), matrix(rule_reclas, ncol=3, byrow = TRUE)))
      #Acumulado qa
      name_raster_qa <- paste("raster_qa", i, sep="")
      assign(name_raster_qa, raster(lista_qa[i])) 
      name_raster_bd_qa <- paste("raster_bd_qa", i, sep="")
      assign(name_raster_bd_qa, reclassify(get(name_raster_bd), matrix(rule_reclas2, ncol=3, byrow = TRUE)))
      assign(name_raster_qa, get(name_raster_qa)*get(name_raster_bd_qa))
    }
    
    print('Acumulado fechas')
    if (length(lista_acum)==12){st_bd<- stack(raster_bd1,raster_bd2,raster_bd3,raster_bd4,raster_bd5,raster_bd6,raster_bd7,raster_bd8,raster_bd9,raster_bd10,raster_bd11,raster_bd12)
    } else if (length(lista_acum)==11)  {st_bd<- stack(raster_bd1,raster_bd2,raster_bd3,raster_bd4,raster_bd5,raster_bd6,raster_bd7,raster_bd8,raster_bd9,raster_bd10,raster_bd11)
    } else if (length(lista_acum)==10)  {st_bd<- stack(raster_bd1,raster_bd2,raster_bd3,raster_bd4,raster_bd5,raster_bd6,raster_bd7,raster_bd8,raster_bd9,raster_bd10)
    } else if (length(lista_acum)==9)  {st_bd<- stack(raster_bd1,raster_bd2,raster_bd3,raster_bd4,raster_bd5,raster_bd6,raster_bd7,raster_bd8,raster_bd9)
    } else if (length(lista_acum)==8)  {st_bd<- stack(raster_bd1,raster_bd2,raster_bd3,raster_bd4,raster_bd5,raster_bd6,raster_bd7,raster_bd8)
    } else if (length(lista_acum)==7)  {st_bd<- stack(raster_bd1,raster_bd2,raster_bd3,raster_bd4,raster_bd5,raster_bd6,raster_bd7)
    } else if (length(lista_acum)==6)  {st_bd<- stack(raster_bd1,raster_bd2,raster_bd3,raster_bd4,raster_bd5,raster_bd6)
    } else if (length(lista_acum)==5)  {st_bd<- stack(raster_bd1,raster_bd2,raster_bd3,raster_bd4,raster_bd5)
    } else if (length(lista_acum)==4)  {st_bd<- stack(raster_bd1,raster_bd2,raster_bd3,raster_bd4)
    } else if (length(lista_acum)==3)  {st_bd<- stack(raster_bd1,raster_bd2,raster_bd3)
    } else if (length(lista_acum)==2)  {st_bd<- stack(raster_bd1,raster_bd2)
    } else if (length(lista_acum)==1)  {st_bd<- stack(raster_bd1)}
    #st_bd[is.na(st_bd)] <- 0
    acumulado<- max(st_bd, na.rm = T)
    acumulado[acumulado==0] <- NA
    # writeRaster(acumulado, paste0(pathIn,"/",n,"_",y,"_acum.tif"), format = "GTiff", overwrite=T)
    rm(raster_bd1,raster_bd2,raster_bd3,raster_bd4,raster_bd5,raster_bd6,raster_bd7,raster_bd8,raster_bd9,raster_bd10,raster_bd11,raster_bd12, st_bd)
    rm(raster_bd_qa1,raster_bd_qa2,raster_bd_qa3,raster_bd_qa4,raster_bd_qa5,raster_bd_qa6,raster_bd_qa7,raster_bd_qa8,raster_bd_qa9,raster_bd_qa10,raster_bd_qa11,raster_bd_qa12, st_bd)
    gc()
    
    #QA
    print('Acumulado QA')
    if (length(lista_acum)==12){st_qa<- stack(raster_qa1,raster_qa2,raster_qa3,raster_qa4,raster_qa5,raster_qa6,raster_qa7,raster_qa8,raster_qa9,raster_qa10,raster_qa11,raster_qa12)
    } else if (length(lista_acum)==11)  {st_qa<- stack(raster_qa1,raster_qa2,raster_qa3,raster_qa4,raster_qa5,raster_qa6,raster_qa7,raster_qa8,raster_qa9,raster_qa10,raster_qa11)
    } else if (length(lista_acum)==10)  {st_qa<- stack(raster_qa1,raster_qa2,raster_qa3,raster_qa4,raster_qa5,raster_qa6,raster_qa7,raster_qa8,raster_qa9,raster_qa10)
    } else if (length(lista_acum)==9)  {st_qa<- stack(raster_qa1,raster_qa2,raster_qa3,raster_qa4,raster_qa5,raster_qa6,raster_qa7,raster_qa8,raster_qa9)
    } else if (length(lista_acum)==8)  {st_qa<- stack(raster_qa1,raster_qa2,raster_qa3,raster_qa4,raster_qa5,raster_qa6,raster_qa7,raster_qa8)
    } else if (length(lista_acum)==7)  {st_qa<- stack(raster_qa1,raster_qa2,raster_qa3,raster_qa4,raster_qa5,raster_qa6,raster_qa7)
    } else if (length(lista_acum)==6)  {st_qa<- stack(raster_qa1,raster_qa2,raster_qa3,raster_qa4,raster_qa5,raster_qa6)
    } else if (length(lista_acum)==5)  {st_qa<- stack(raster_qa1,raster_qa2,raster_qa3,raster_qa4,raster_qa5)
    } else if (length(lista_acum)==4)  {st_qa<- stack(raster_qa1,raster_qa2,raster_qa3,raster_qa4)
    } else if (length(lista_acum)==3)  {st_qa<- stack(raster_qa1,raster_qa2,raster_qa3)
    } else if (length(lista_acum)==2)  {st_qa<- stack(raster_qa1,raster_qa2)
    } else if (length(lista_acum)==1)  {st_qa<- stack(raster_qa1)}
    #st_qa[is.na(st_qa)] <- 0
    st_qa <- max(st_qa, na.rm = T)
    st_qa[st_qa==0] <- NA
    qa <- st_qa
    # writeRaster(qa, paste0(pathIn,"/",n,"_",y,"_qa.tif"), format = "GTiff", overwrite=T)
    rm(raster_qa1,raster_qa2,raster_qa3,raster_qa4,raster_qa5,raster_qa6,raster_qa7,raster_qa8,raster_qa9,raster_qa10,raster_qa11,raster_qa12, st_qa)
    gc()
    
    print('Acumulado incertidumbre')
    for (i in 1:length(lista_unc)){
      name_raster_unc <- paste("raster_unc", i, sep="")
      #Acumulado incertidumbre. Crear recuento imagenes, llamar raster y reclasificar
      assign(name_raster_unc, raster(lista_unc[i])) 
    }
    
    if (length(lista_acum)==12){st_unc<- stack(raster_unc1,raster_unc2,raster_unc3,raster_unc4,raster_unc5,raster_unc6,raster_unc7,raster_unc8,raster_unc9,raster_unc10,raster_unc11,raster_unc12)
    } else if (length(lista_acum)==11)  {st_unc<- stack(raster_unc1,raster_unc2,raster_unc3,raster_unc4,raster_unc5,raster_unc6,raster_unc7,raster_unc8,raster_unc9,raster_unc10,raster_unc11)
    } else if (length(lista_acum)==10)  {st_unc<- stack(raster_unc1,raster_unc2,raster_unc3,raster_unc4,raster_unc5,raster_unc6,raster_unc7,raster_unc8,raster_unc9,raster_unc10)
    } else if (length(lista_acum)==9)  {st_unc<- stack(raster_unc1,raster_unc2,raster_unc3,raster_unc4,raster_unc5,raster_unc6,raster_unc7,raster_unc8,raster_unc9)
    } else if (length(lista_acum)==8)  {st_unc<- stack(raster_unc1,raster_unc2,raster_unc3,raster_unc4,raster_unc5,raster_unc6,raster_unc7,raster_unc8)
    } else if (length(lista_acum)==7)  {st_unc<- stack(raster_unc1,raster_unc2,raster_unc3,raster_unc4,raster_unc5,raster_unc6,raster_unc7)
    } else if (length(lista_acum)==6)  {st_unc<- stack(raster_unc1,raster_unc2,raster_unc3,raster_unc4,raster_unc5,raster_unc6)
    } else if (length(lista_acum)==5)  {st_unc<- stack(raster_unc1,raster_unc2,raster_unc3,raster_unc4,raster_unc5)
    } else if (length(lista_acum)==4)  {st_unc<- stack(raster_unc1,raster_unc2,raster_unc3,raster_unc4)
    } else if (length(lista_acum)==3)  {st_unc<- stack(raster_unc1,raster_unc2,raster_unc3)
    } else if (length(lista_acum)==2)  {st_unc<- stack(raster_unc1,raster_unc2)
    } else if (length(lista_acum)==1)  {st_unc<- stack(raster_unc1)}
    #st_unc[is.na(st_unc)] <- 0
    st_unc <- max(st_unc,na.rm = T)
    st_unc[st_unc==0] <- NA
    unc <- st_unc
    # writeRaster(unc, paste0(pathIn,"/",n,"_",y,"_unc.tif"), format = "GTiff", overwrite=T)
    rm(raster_unc1,raster_unc2,raster_unc3,raster_unc4,raster_unc5,raster_unc6,raster_unc7,raster_unc8,raster_unc9,raster_unc10,raster_unc11,raster_unc12, st_unc)
    gc()
    
    #Eliminar
    removeTmpFiles(1)
    print('-------Clump1-----')
    #Generar identificaci?n clump. Crear stack layer. 
    clumpy <- clump(acumulado, directions=8)
    # writeRaster(clumpy, paste0(pathIn,"/",n,"_",y,"_clumpy.tif"), format = "GTiff", overwrite=T)
    #Si no existen valores de incendio saltar a la siguiente imagen
    st<- stack(clumpy, acumulado, unc, qa)
    #Coordenadas geogr?ficas
    pj1 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
    st<- projectRaster(st, crs = pj1, method = "ngb")
    #Seleccionar el a?o de la imagen. El dia de la imagena actual y el dia de la siguiente imagen.
    #Convertir r?ster a data.frame de valores y coordenadas. Se eliminan los valores no data. Cambiar el nombre de las columnas.
    df_stack <- raster::as.data.frame(st, xy=T, centroids=T, na.rm=T)
    # df_stack <- cbind(df_stack, y, n)
    df_stack$year <- y
    df_stack$ntile <- n
    
    if (!exists('p_incendio')){
      p_incendio<- setNames(data.frame(matrix(ncol = ncol(df_stack), nrow = 0)), colnames(df_stack))
    }
    p_incendio <- rbind(p_incendio, df_stack)
  }
  # }
  rm(df_stack, acumulado, clumpy, st, qa, unc)
  # colnames(p_incendio) <- c("x","y","clump","day","unc","qa","year","ntail")
  colnames(p_incendio) <- c("clump","day","unc","qa","x","y","year","ntail")
  for (p in 1:nrow(p_incendio)){
    p_incendio$qa_bit0[p] <- unbinary(paste0(as.integer(intToBits(p_incendio$qa[p])[1]),collapse=""))
    p_incendio$qa_bit1[p] <- unbinary(paste0(as.integer(intToBits(p_incendio$qa[p])[2]),collapse=""))
    p_incendio$qa_bit2[p] <- unbinary(paste0(as.integer(intToBits(p_incendio$qa[p])[3]),collapse=""))
    p_incendio$qa_bit3[p] <- unbinary(paste0(as.integer(intToBits(p_incendio$qa[p])[4]),collapse=""))
    p_incendio$qa_bit4[p] <- unbinary(paste0(as.integer(intToBits(p_incendio$qa[p])[5]),collapse=""))
    p_incendio$qa_bit5[p] <- unbinary(paste0(as.integer(intToBits(p_incendio$qa[p])[6]),collapse=""))
    p_incendio$qa_bit6[p] <- unbinary(paste0(as.integer(intToBits(p_incendio$qa[p])[7]),collapse=""))
    p_incendio$qa_bit7[p] <- unbinary(paste0(as.integer(intToBits(p_incendio$qa[p])[8]),collapse=""))
  }
  #Agua 0 no agua 1
  length(which(p_incendio$qa_bit0==0))
  # Datos insuficientes 0 Datos suficientes 1
  length(which(p_incendio$qa_bit1==0))
  # Periodo de confianza no abarca todo el mes 0 Abarca todo el mes 1
  length(which(p_incendio$qa_bit2==1))
  # No reetiquetado 0 Reetiquetado contextual 1
  length(which(p_incendio$qa_bit3==1))
  #Bit de repuesto ajustado a 0
  length(which(p_incendio$qa_bit4!=0))
  # 0 Quemada, no mapeada o celda de agua
  length(which(p_incendio$qa_bit5!=0))
  length(which(p_incendio$qa_bit6!=0))
  length(which(p_incendio$qa_bit7!=0))
  
  #Empezamos de nuevo la numeracion y asignamos clumps distintos segun a?o de incendio y clump
  rownames(p_incendio) = 1:nrow(p_incendio)
  p_incendio$year <- as.numeric(as.character(p_incendio$year))
  id=1
  seguridad <- p_incendio
  for (zzz in unique(p_incendio$year)){
    for (nnn in unique(p_incendio$ntail)){
      for (mmm in unique(p_incendio$clump)){
        p_incendio$clump[p_incendio$year == zzz & p_incendio$clump == mmm & p_incendio$ntail == nnn] <- id
        id = id+1
      }
    }
  }
  ###############################################################################################
  ##########################################Punto de restauracion################################
  ##############################################################################################
  # saveRDS(p_incendio, paste0(f_rds,"/","p_incendio.rds"))
  # p_incendio <- readRDS(paste0(f_rds,"/","p_incendio.rds"))
  ###############################################################################################
  ###############################################################################################
  
  
  df_stack <- p_incendio
  #Se comprueba si existen irregularidades (qa) pero no se eliminan.
  nrow(subset(df_stack,df_stack$qa_bit0!=1 | df_stack$qa_bit1!=1 | df_stack$qa_bit2!=0 | df_stack$qa_bit3!=0))
  #Estos puntos/pixeles pueden ser utilizados pero deber?a se?alarse que deben ser utilizados con posibles riesgos
  ##Simplemente se puede se?alar el tipo de error que podrian contener.
  df_stack$year <- as.numeric(as.character(df_stack$year))
  df_stack$date <- as.Date(as.vector(df_stack$day-1), origin = paste0(df_stack$year,"-01-01"))
  df_stack_shp <- SpatialPointsDataFrame(df_stack[c("x","y")], df_stack)
  crs(df_stack_shp) <-  "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +to wgs84=0,0,0"
  # writeOGR(obj=df_stack_shp, dsn=paste0(f_shp), layer="df_stack", driver="ESRI Shapefile")
  
  #######################################################Clump2#######################################################
  # plot(prueba)
  print('-------Clump2-----')
  df_stack_shp$clump2 <- 0
  #Recodificar clumps por cercania espacial
  num_clump <- 1
  for (c in unique(df_stack_shp$clump)){ #unique(df_stack_shp$clump)
    subset_clump <- subset(df_stack_shp, df_stack_shp$clump==c)
    if (nrow(subset_clump)==0){next}
    if(subset_clump$clump2==0){
      df_stack_shp$clump2[df_stack_shp$clump == c] <- num_clump 
    } else {
      temporal_clump <- mean(subset_clump$clump2)
    }
    buffer_clump <- buffer(subset_clump, width=1500, dissolve=T) 
    # plot(buffer_clump)
    # points(subset_clump, pch=21, col="blue", bg="blue")
    intersect_clump <- df_stack_shp[buffer_clump, ]
    # points(intersect_clump, pch=21, col="green", bg="green", cex=0.2)
    clump_intersect <- unique(intersect_clump$clump)
    clump_intersect <-  clump_intersect[clump_intersect!= c & clump_intersect!=0]
    if(length(clump_intersect)>0){
      for (tr_int in clump_intersect){
        if (exists("temporal_clump")){
          df_stack_shp$clump2[df_stack_shp$clump == tr_int & df_stack_shp$clump2 == 0] <- temporal_clump
          rm(temporal_clump)
        } else {
          df_stack_shp$clump2[df_stack_shp$clump == tr_int & df_stack_shp$clump2 == 0] <- num_clump
        }
      }
    }
    if (exists("temporal_clump")){
      rm(temporal_clump)
    } else {num_clump <- num_clump+1}
  }
  rm(tr_int, num_clump)
  # saveRDS(df_stack_shp, paste0(f_rds,"/","df_stack_clump2.rds"))
  ###########################################################################################################
  # df_stack_shp <- readRDS(paste0(f_rds,"/","df_stack_clump2.rds"))
  df_stack <- as.data.frame(df_stack_shp)[1:18]
  # writeOGR(obj=df_stack_shp, dsn=paste0(f_shp), layer="df_stack_clump2", driver="ESRI Shapefile")
  #SEGUIR AQUI
  
  #######################################################Clump3#######################################################
  ###################################################################
  #Calculamos el n?mero de clumps totales sobre los que generar el bucle (tratar cada clump independientemente).
  #Se aplica un corte en el clump si has m?s de dos d?as sin incendio
  #Incendios (x, y, clump, day, unc, qa, qa_bit0,qa_bit1,qa_bit2,qa_bit3,qa_bit4,qa_bit5). Insertar esta estructura. 
  #days_cut = cortar tras x dias sin incendio
  #Arriba debe seguir creandose la tabla_incendios
  ##################################################################
  print('-------Clump3-----')
  df_stack$clump3 <- 0
  num_clump <- 1
  for (c in unique(df_stack$clump2)){
    #Se seleccionn los p?xeles del clump concreto
    df_clump <- subset(df_stack, df_stack$clump2==c)
    #Aunque todos los clumps deberian tener incendios..
    if (nrow(df_clump)==0){
      print("no incendio en clump")
      next
    }
    #Sacar fechas unicas
    pos_day <- unique(df_clump$date)
    #En caso de que el incendio solo tenga una fecha se salta... a else final
    #Si es mayor a 1 calculamos los cortes del incendio
    if (length(pos_day)>1){
      # pos_day2 <- c()
      # pos_day22 <- c()
      pos_date2 <- c()
      #Para cada registro del clump se trata la fecha de quema con la incertidumbre del dia de quema
      for (n in 1:nrow(df_clump)){
        #Se genera un vector con las fechas teniendo en cuenta la incertidumbre. Como el formato date no permite crear el vector del mismo modo,
        # se realiza un bucle for para añadir los valores
        if (df_clump$unc[n]>1){
          pos_date1 <- c()
          for (n_unc in 1:df_clump$unc[n]){
            pos_date1 <- append(pos_date1, df_clump$date[n]-n_unc)
          }
        } else {
          pos_date1 <- c()
          pos_date1 <- append(pos_date1, df_clump$date[n]-1)
        }
        pos_date1 <- append(pos_date1, df_clump$date[n])
        pos_date2 <- append(pos_date2, pos_date1)
      }
      pos_date <- unique(pos_date2)
      pos_date <- sort(pos_date, decreasing=F)
      pos_date1 <- pos_date[2:length(pos_date)]
      pos_date2 <- pos_date[1:length(pos_date)-1]
      dif_date <- as.numeric(difftime(pos_date1, pos_date2 , units = c("days")))
      ####
      cut_t <- which(dif_date>= 3)
      ####
      if(length(cut_t)==0){
        df_stack$clump3[df_stack$clump2 == c] <- num_clump
        num_clump = num_clump+1
      }
      #Si el incendio tiene +1 corte
      if(length(cut_t)>0){
        #Para cada corte asignar nuevo clump
        for (d_cut in 0:length(cut_t)){
          #Si es el primer corte recortar o escoger fechas a partir de vector..
          if (d_cut == 0){
            ind_date <- pos_date[1:cut_t[1]]
            df_stack$clump3[df_stack$clump2 == c & df_stack$date %in% ind_date] <- num_clump
            num_clump = num_clump+1
            #Si son cortes intermedios recortar o escoger fechas a partir de vector..
          } else if (d_cut != 0 & d_cut != length(cut_t)){
            ind_date <- pos_date[(cut_t[d_cut]+1):(cut_t[d_cut+1])]
            df_stack$clump3[df_stack$clump2 == c & df_stack$date %in% ind_date] <- num_clump
            num_clump = num_clump+1
            #Si es ultimo corte
          } else {
            ind_date <- pos_date[(cut_t[d_cut]+1):length(pos_date)]
            df_stack$clump3[df_stack$clump2 == c & df_stack$date %in% ind_date] <- num_clump
            num_clump = num_clump+1
          }
        }
      }
      #En caso de que solo tenga una fecha (se entiende que fecha minima y maxima es la misma y solo hay que sacar la coordenada del incendio)
    } else {
      df_stack$clump3[df_stack$clump2 == c] <- num_clump
      num_clump = num_clump+1
    }
  }
  rm(cut_t, d_cut, dif_date, ind_date, num_clump,  pos_date,  pos_date1, pos_date2, df_clump)
  df_stack_shp <- SpatialPointsDataFrame(df_stack[c("x","y")], df_stack)
  crs(df_stack_shp) <-  "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +to wgs84=0,0,0"
  # writeOGR(obj=df_stack_shp, dsn=paste0(f_shp), layer="df_stack_clump3", driver="ESRI Shapefile")
  
  
  #######################################################Clump4#######################################################
  df_stack_shp$id <- seq.int(nrow(df_stack_shp))
  crs(df_stack_shp) <-  "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +to wgs84=0,0,0"
  print('-------Clump4-----')
  df_stack_shp$clump4 = 0
  num_clump <- 1
  ###Seleccionamos clump
  for (c in unique(df_stack_shp$clump3)){
    subset_clump <- subset(df_stack_shp, df_stack_shp$clump3==c)
    if (nrow(subset_clump)==0){next}
    while (nrow(subset_clump)>0) {
      n_points=0
      buffer_clump <- buffer(subset_clump[1,], width=1500, dissolve=T) 
      plot(buffer_clump)
      points(subset_clump[1,], pch=21, col="blue", bg="blue")
      intersect_clump <- subset_clump[buffer_clump, ]
      points(intersect_clump, pch=21, col="green", bg="green")
      if (nrow(intersect_clump) > 1){n_points = 2}
      while (n_points > 1) {
        n_points <- nrow(intersect_clump)
        buffer_clump <- buffer(intersect_clump, width=1500, dissolve=T) 
        # plot(buffer_clump)
        # points(intersect_clump, pch=21, col="blue", bg="blue")
        intersect_clump <- subset_clump[buffer_clump, ]
        # points(intersect_clump, pch=21, col="green", bg="green")
        if (n_points == nrow(intersect_clump)){n_points = 0}
      }
      df_stack_shp$clump4[df_stack_shp$id %in% intersect_clump$id] <- num_clump
      subset_clump <- subset_clump[!subset_clump$id %in% intersect_clump$id, ]
      num_clump <-  num_clump+1
    }
  }
  rm(subset_clump, n_points, intersect_clump, buffer_clump)
  # writeOGR(obj=df_stack_shp, dsn="E:/MODIS/df_stack_CA.shp", layer="E:/MODIS/df_stack_CA.shp", driver="ESRI Shapefile")
  df_stack <- as.data.frame(df_stack_shp)[1:21]
  return(df_stack)  
}


burnedPixelDF_ <- function(pathIn, clumpDist = 1500, ndays = 3, pSMP = TRUE, pCR = TRUE){
  for (package in c('sp','maptools','rgeos','raster','rgdal','gdalUtils', 'igraph', 'compositions', 'lubridate', 'modiscloud', 'plyr', 'dplyr', 'rts')) {
    if (!require(package, character.only=T, quietly=T)) {
      install.packages(package)
      library(package, character.only=T)
      # print(paste0(package," package has been installed"))
    }
    else {
      library(package, character.only=T)
      # print(paste0(package," package is installed"))
      }
  }
  
  if(missing(pSMP)){pSMP = TRUE} # Add pixels with Shortened mapping period pSMP = True or False , by default is True
  if(missing(pCR)){pCR = TRUE} # Grid cell was relabeled during the contextual relabeling phase of the algorithm (0 = false, 1 = true). By default is True.
  if(missing(clumpDist)){clumpDist = 1500} # By default use 1500 meters
  if(missing(ndays)){ndays = 3} # By default use 3 (days)
  print(paste0(" pSMP =", pSMP," pCR = ", pCR, " clumpDist =", clumpDist, " ndays = ", ndays))
  
  # pathIn = "D:/MIKEL/DATOS_AUSTRALIA/MODISTSP/Burned_Monthly_500m_v6"
  if (substr(pathIn, nchar(pathIn), nchar(pathIn)) != "/"){pathIn = paste0(pathIn,"/")}
  
  #Path to MODIS images
  f_bd <- paste0(pathIn,'Burn_Date')
  f_bdu <- paste0(pathIn,'Burn_Date_Unc')
  f_qa <- paste0(pathIn,'QA')
  files_bd <- list.files(f_bd,pattern='.tif$',full.names=TRUE)
  name <- basename(files_bd)
  files_unc <- list.files(f_bdu,pattern='.tif$',full.names=TRUE)
  files_qa <- list.files(f_qa,pattern='.tif$',full.names=TRUE)
  
  #Check files
  if (length(files_bd) == length(files_unc) & length(files_bd) == length(files_qa)){
    print(".TIF Okay")
  } else {
    print("Error reading .TIF")
    break()
  }
  
  #Select years and tails
  year <- unique(substr(name, 19, 22))
  # n <- 0
  
  #Rules for reclassify data
  rule_reclas <-  c(-Inf, 0, NA, 400, Inf, NA)
  rule_reclas2 <-  c(1, Inf, 1)
  m <- c(0, 100, 1,  101, 200, 2,  201, 300, 3)
  # rm(p_incendio)
  
  #Looping through years
  for (y in year){
    inicio = Sys.time()
    print(y)
    lista_acum <- files_bd[grepl(paste0(y),files_bd)]
    lista_unc <- files_unc[grepl(paste0(y),files_unc)]
    lista_qa <- files_qa[grepl(paste0(y),files_qa)]
    print(lista_acum)
    print(lista_unc)
    print(lista_qa)
    for (i in 1:length(lista_acum)){
      name_raster_bd <- paste("raster_bd", i, sep="")
      #Accumulated raster layers of burn data, uncertanigy and QA
      assign(name_raster_bd, raster(lista_acum[i]))
      assign(name_raster_bd, reclassify(get(name_raster_bd), matrix(rule_reclas, ncol=3, byrow = TRUE)))
      name_raster_qa <- paste("raster_qa", i, sep="")
      assign(name_raster_qa, raster(lista_qa[i]))
      name_raster_bd_qa <- paste("raster_bd_qa", i, sep="")
      assign(name_raster_bd_qa, reclassify(get(name_raster_bd), matrix(rule_reclas2, ncol=3, byrow = TRUE)))
      assign(name_raster_qa, get(name_raster_qa)*get(name_raster_bd_qa))
      }
    
    if (length(lista_acum)==12){st_bd<- stack(raster_bd1,raster_bd2,raster_bd3,raster_bd4,raster_bd5,raster_bd6,raster_bd7,raster_bd8,raster_bd9,raster_bd10,raster_bd11,raster_bd12)
    } else if (length(lista_acum)==11)  {st_bd<- stack(raster_bd1,raster_bd2,raster_bd3,raster_bd4,raster_bd5,raster_bd6,raster_bd7,raster_bd8,raster_bd9,raster_bd10,raster_bd11)
    } else if (length(lista_acum)==10)  {st_bd<- stack(raster_bd1,raster_bd2,raster_bd3,raster_bd4,raster_bd5,raster_bd6,raster_bd7,raster_bd8,raster_bd9,raster_bd10)
    } else if (length(lista_acum)==9)  {st_bd<- stack(raster_bd1,raster_bd2,raster_bd3,raster_bd4,raster_bd5,raster_bd6,raster_bd7,raster_bd8,raster_bd9)
    } else if (length(lista_acum)==8)  {st_bd<- stack(raster_bd1,raster_bd2,raster_bd3,raster_bd4,raster_bd5,raster_bd6,raster_bd7,raster_bd8)
    } else if (length(lista_acum)==7)  {st_bd<- stack(raster_bd1,raster_bd2,raster_bd3,raster_bd4,raster_bd5,raster_bd6,raster_bd7)
    } else if (length(lista_acum)==6)  {st_bd<- stack(raster_bd1,raster_bd2,raster_bd3,raster_bd4,raster_bd5,raster_bd6)
    } else if (length(lista_acum)==5)  {st_bd<- stack(raster_bd1,raster_bd2,raster_bd3,raster_bd4,raster_bd5)
    } else if (length(lista_acum)==4)  {st_bd<- stack(raster_bd1,raster_bd2,raster_bd3,raster_bd4)
    } else if (length(lista_acum)==3)  {st_bd<- stack(raster_bd1,raster_bd2,raster_bd3)
    } else if (length(lista_acum)==2)  {st_bd<- stack(raster_bd1,raster_bd2)
    } else if (length(lista_acum)==1)  {st_bd<- raster_bd1}
    #st_bd[is.na(st_bd)] <- 0
    acumulado<- max(st_bd,na.rm=T)
    acumulado[acumulado==0] <- NA
    
    #Deleting temporal file
    # rm(raster_bd1,raster_bd2,raster_bd3,raster_bd4,raster_bd5,raster_bd6,raster_bd7,raster_bd8,raster_bd9,raster_bd10,raster_bd11,raster_bd12, st_bd)
    # rm(raster_bd_qa1,raster_bd_qa2,raster_bd_qa3,raster_bd_qa4,raster_bd_qa5,raster_bd_qa6,raster_bd_qa7,raster_bd_qa8,raster_bd_qa9,raster_bd_qa10,raster_bd_qa11,raster_bd_qa12, st_bd)
    # gc()
    
    #QA
    if (length(lista_acum)==12){st_qa<- stack(raster_qa1,raster_qa2,raster_qa3,raster_qa4,raster_qa5,raster_qa6,raster_qa7,raster_qa8,raster_qa9,raster_qa10,raster_qa11,raster_qa12)
    } else if (length(lista_acum)==11)  {st_qa<- stack(raster_qa1,raster_qa2,raster_qa3,raster_qa4,raster_qa5,raster_qa6,raster_qa7,raster_qa8,raster_qa9,raster_qa10,raster_qa11)
    } else if (length(lista_acum)==10)  {st_qa<- stack(raster_qa1,raster_qa2,raster_qa3,raster_qa4,raster_qa5,raster_qa6,raster_qa7,raster_qa8,raster_qa9,raster_qa10)
    } else if (length(lista_acum)==9)  {st_qa<- stack(raster_qa1,raster_qa2,raster_qa3,raster_qa4,raster_qa5,raster_qa6,raster_qa7,raster_qa8,raster_qa9)
    } else if (length(lista_acum)==8)  {st_qa<- stack(raster_qa1,raster_qa2,raster_qa3,raster_qa4,raster_qa5,raster_qa6,raster_qa7,raster_qa8)
    } else if (length(lista_acum)==7)  {st_qa<- stack(raster_qa1,raster_qa2,raster_qa3,raster_qa4,raster_qa5,raster_qa6,raster_qa7)
    } else if (length(lista_acum)==6)  {st_qa<- stack(raster_qa1,raster_qa2,raster_qa3,raster_qa4,raster_qa5,raster_qa6)
    } else if (length(lista_acum)==5)  {st_qa<- stack(raster_qa1,raster_qa2,raster_qa3,raster_qa4,raster_qa5)
    } else if (length(lista_acum)==4)  {st_qa<- stack(raster_qa1,raster_qa2,raster_qa3,raster_qa4)
    } else if (length(lista_acum)==3)  {st_qa<- stack(raster_qa1,raster_qa2,raster_qa3)
    } else if (length(lista_acum)==2)  {st_qa<- stack(raster_qa1,raster_qa2)
    } else if (length(lista_acum)==1)  {st_qa<- raster_qa1}
    #st_qa[is.na(st_qa)] <- 0
    st_qa <- max(st_qa,na.rm=T)
    st_qa[st_qa==0] <- NA
    qa <- st_qa
    # rm(raster_qa1,raster_qa2,raster_qa3,raster_qa4,raster_qa5,raster_qa6,raster_qa7,raster_qa8,raster_qa9,raster_qa10,raster_qa11,raster_qa12, st_qa)
    # gc()
    
    #Uncerainty cumulative layer
    for (i in 1:length(lista_unc)){
      name_raster_unc <- paste("raster_unc", i, sep="")
      assign(name_raster_unc, raster(lista_unc[i]))
    }
    
    if (length(lista_acum)==12){st_unc<- stack(raster_unc1,raster_unc2,raster_unc3,raster_unc4,raster_unc5,raster_unc6,raster_unc7,raster_unc8,raster_unc9,raster_unc10,raster_unc11,raster_unc12)
    } else if (length(lista_acum)==11)  {st_unc<- stack(raster_unc1,raster_unc2,raster_unc3,raster_unc4,raster_unc5,raster_unc6,raster_unc7,raster_unc8,raster_unc9,raster_unc10,raster_unc11)
    } else if (length(lista_acum)==10)  {st_unc<- stack(raster_unc1,raster_unc2,raster_unc3,raster_unc4,raster_unc5,raster_unc6,raster_unc7,raster_unc8,raster_unc9,raster_unc10)
    } else if (length(lista_acum)==9)  {st_unc<- stack(raster_unc1,raster_unc2,raster_unc3,raster_unc4,raster_unc5,raster_unc6,raster_unc7,raster_unc8,raster_unc9)
    } else if (length(lista_acum)==8)  {st_unc<- stack(raster_unc1,raster_unc2,raster_unc3,raster_unc4,raster_unc5,raster_unc6,raster_unc7,raster_unc8)
    } else if (length(lista_acum)==7)  {st_unc<- stack(raster_unc1,raster_unc2,raster_unc3,raster_unc4,raster_unc5,raster_unc6,raster_unc7)
    } else if (length(lista_acum)==6)  {st_unc<- stack(raster_unc1,raster_unc2,raster_unc3,raster_unc4,raster_unc5,raster_unc6)
    } else if (length(lista_acum)==5)  {st_unc<- stack(raster_unc1,raster_unc2,raster_unc3,raster_unc4,raster_unc5)
    } else if (length(lista_acum)==4)  {st_unc<- stack(raster_unc1,raster_unc2,raster_unc3,raster_unc4)
    } else if (length(lista_acum)==3)  {st_unc<- stack(raster_unc1,raster_unc2,raster_unc3)
    } else if (length(lista_acum)==2)  {st_unc<- stack(raster_unc1,raster_unc2)
    } else if (length(lista_acum)==1)  {st_unc<- raster_unc1}
    #st_unc[is.na(st_unc)] <- 0
    st_unc <- max(st_unc,na.rm=T)
    st_unc[st_unc==0] <- NA
    unc <- st_unc
    # rm(raster_unc1,raster_unc2,raster_unc3,raster_unc4,raster_unc5,raster_unc6,raster_unc7,raster_unc8,raster_unc9,raster_unc10,raster_unc11,raster_unc12, st_unc)
    # gc()
    

    print('-------Clump1-----')
    #Firts grouping attempt
    clumpy <- clump(acumulado, directions=8)
    
    st<- stack(clumpy, acumulado, unc, qa)
    #Project to WGS84
    pj1 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
    st<- projectRaster(st, crs = pj1, method = "ngb")
    
    #Convert to data.frame
    print('L-153')
    df_stack <- raster::as.data.frame(st, xy=T, centroids=T, na.rm=T)
    df_stack$year <- y
    
    #Added to solve naming issue
    colnames(df_stack) <- c("x","y","clump","day","unc","qa","year")
    
    if (!exists('p_incendio')){
      p_incendio<- setNames(data.frame(matrix(ncol = ncol(df_stack), nrow = 0)), colnames(df_stack))
    }
    if (nrow(df_stack) > 0){
      p_incendio <- rbind(p_incendio, df_stack)
    }
    final = Sys.time()
    print(final-inicio)
    gc()
    
  }

  # }
  # rm(df_stack, acumulado, clumpy, st, qa, unc)
  colnames(p_incendio) <- c("x","y","clump","day","unc","qa","year")
  # colnames(p_incendio) <- c("clump","day","unc","qa","x","y","year","ntail")
  
  print('L-161')
  
  p_incendio$qa <- as.integer(p_incendio$qa)
  
  p_incendio$qa_bit0 <- sapply(p_incendio$qa,  function(x) extract_bit(x, bitnum=1))
  p_incendio$qa_bit1 <- sapply(p_incendio$qa,  function(x) extract_bit(x, bitnum=2))
  p_incendio$qa_bit2 <- sapply(p_incendio$qa,  function(x) extract_bit(x, bitnum=3))
  p_incendio$qa_bit3 <- sapply(p_incendio$qa,  function(x) extract_bit(x, bitnum=4))
  p_incendio$qa_bit4 <- sapply(p_incendio$qa,  function(x) extract_bit(x, bitnum=5))
  p_incendio$qa_bit5 <- sapply(p_incendio$qa,  function(x) extract_bit(x, bitnum=6))
  p_incendio$qa_bit6 <- sapply(p_incendio$qa,  function(x) extract_bit(x, bitnum=7))
  p_incendio$qa_bit7 <- sapply(p_incendio$qa,  function(x) extract_bit(x, bitnum=8))
  
  # #Agua 0 no agua 1 bit0
  # # Datos insuficientes 0 Datos suficientes 1 bit1
  # # Periodo de confianza no abarca todo el mes 0 Abarca todo el mes 1 bit2
  # # No reetiquetado 0 Reetiquetado contextual 1 bit3
  # #Bit de repuesto ajustado a 0 bit4
  # # 0 Quemada, no mapeada o celda de agua bit5,6,7

  #Initialize clump codes and start assignation
  # rownames(p_incendio) = 1:nrow(p_incendio)
  p_incendio$year <- as.numeric(as.character(p_incendio$year))
  print('L-190')
  p_incendio<- p_incendio %>% mutate(clump = group_indices(., year, clump))
  ###############################################################################################
  
  df_stack <- p_incendio
  names(df_stack)[4:6] <- c('day','unc','qa')
  
  #Check remaing QA issues
  if (pSMP == TRUE & pCR == TRUE){
    df_stack <- subset(df_stack,df_stack$qa_bit0==1 & df_stack$qa_bit1==1)
  }
  if (pSMP == FALSE & pCR == TRUE){
    df_stack <- subset(df_stack,df_stack$qa_bit0==1 & df_stack$qa_bit1==1 & df_stack$qa_bit2==0)
  }
  if (pSMP == TRUE & pCR == FALSE){
    df_stack <- subset(df_stack,df_stack$qa_bit0==1 & df_stack$qa_bit1==1 & df_stack$qa_bit3==0)
  }
  if (pSMP == FALSE & pCR == FALSE){
    df_stack <- subset(df_stack,df_stack$qa_bit0==1 & df_stack$qa_bit1==1 & df_stack$qa_bit2==0 & df_stack$qa_bit3==0)
  }
  # df_stack <- subset(df_stack,df_stack$qa_bit0==1 & df_stack$qa_bit1==1 & df_stack$qa_bit2==1 | df_stack$qa_bit3==0)
  

  df_stack$date <- as.Date(as.vector(df_stack$day-1), origin = paste0(df_stack$year,"-01-01"))
  df_stack_shp <- SpatialPointsDataFrame(df_stack[c("x","y")], df_stack)
  crs(df_stack_shp) <-  "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +to wgs84=0,0,0"
  
  #######################################################Clump2#######################################################
  # Second clump, refining the first
  print('-------Clump2-----')
  df_stack_shp$clump2 <- 0
  #Recode clump based on distance
  num_clump <- 1
  for (c in unique(df_stack_shp$clump)){ #unique(df_stack_shp$clump)
    subset_clump <- subset(df_stack_shp, df_stack_shp$clump==c)
    if (nrow(subset_clump)==0){next}
    if(subset_clump$clump2==0){
      df_stack_shp$clump2[df_stack_shp$clump == c] <- num_clump
    } else {
      temporal_clump <- mean(subset_clump$clump2)
    }
    buffer_clump <- buffer(subset_clump, width=clumpDist, dissolve=T)
    
    intersect_clump <- df_stack_shp[buffer_clump, ]
    
    clump_intersect <- unique(intersect_clump$clump)
    clump_intersect <-  clump_intersect[clump_intersect!= c & clump_intersect!=0]
    if(length(clump_intersect)>0){
      for (tr_int in clump_intersect){
        if (exists("temporal_clump")){
          df_stack_shp$clump2[df_stack_shp$clump == tr_int & df_stack_shp$clump2 == 0] <- temporal_clump
          rm(temporal_clump)
        } else {
          df_stack_shp$clump2[df_stack_shp$clump == tr_int & df_stack_shp$clump2 == 0] <- num_clump
        }
      }
    }
    if (exists("temporal_clump")){
      rm(temporal_clump)
    } else {num_clump <- num_clump+1}
  }
  
  ###########################################################################################################
  df_stack <- as.data.frame(df_stack_shp)[1:17]
  
  #Third clump attemp. Dis/aggregate accordign to temporal window
  ##################################################################
  print('-------Clump3-----')
  df_stack$clump3 <- 0
  num_clump <- 1
  for (c in unique(df_stack$clump2)){
    
    df_clump <- subset(df_stack, df_stack$clump2==c)
    
    if (nrow(df_clump)==0){
      print("no incendio en clump")
      next
    }
    #Get unique dates
    pos_day <- unique(df_clump$date)
    
    if (length(pos_day)>1){
      
      pos_date2 <- c()
      
      #Clump according to date plus uncertainty
      
      for (n in 1:nrow(df_clump)){
        
        if (df_clump$unc[n]>1){
          pos_date1 <- c()
          for (n_unc in 1:df_clump$unc[n]){
            pos_date1 <- append(pos_date1, df_clump$date[n]-n_unc)
          }
        } else {
          pos_date1 <- c()
          pos_date1 <- append(pos_date1, df_clump$date[n]-1)
        }
        pos_date1 <- append(pos_date1, df_clump$date[n])
        pos_date2 <- append(pos_date2, pos_date1)
      }
      pos_date <- unique(pos_date2)
      pos_date <- sort(pos_date, decreasing=F)
      pos_date1 <- pos_date[2:length(pos_date)]
      pos_date2 <- pos_date[1:length(pos_date)-1]
      dif_date <- as.numeric(difftime(pos_date1, pos_date2 , units = c("days")))
      ####
      cut_t <- which(dif_date >= ndays)
      ####
      if(length(cut_t)==0){
        df_stack$clump3[df_stack$clump2 == c] <- num_clump
        num_clump = num_clump+1
      }
      
      #If there are different dates then...
      if(length(cut_t)>0){
        #We assign a new clump code
        for (d_cut in 0:length(cut_t)){
          
          if (d_cut == 0){
            ind_date <- pos_date[1:cut_t[1]]
            df_stack$clump3[df_stack$clump2 == c & df_stack$date %in% ind_date] <- num_clump
            num_clump = num_clump+1
            
          } else if (d_cut != 0 & d_cut != length(cut_t)){
            ind_date <- pos_date[(cut_t[d_cut]+1):(cut_t[d_cut+1])]
            df_stack$clump3[df_stack$clump2 == c & df_stack$date %in% ind_date] <- num_clump
            num_clump = num_clump+1
            #Si es ultimo corte
          } else {
            ind_date <- pos_date[(cut_t[d_cut]+1):length(pos_date)]
            df_stack$clump3[df_stack$clump2 == c & df_stack$date %in% ind_date] <- num_clump
            num_clump = num_clump+1
          }
        }
      }
      
    } else {
      df_stack$clump3[df_stack$clump2 == c] <- num_clump
      num_clump = num_clump+1
    }
  }
  df_stack_shp <- SpatialPointsDataFrame(df_stack[c("x","y")], df_stack)
  crs(df_stack_shp) <-  "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +to wgs84=0,0,0"
  
  
  #######################################################Clump4#######################################################
  df_stack_shp$id <- seq.int(nrow(df_stack_shp))
  crs(df_stack_shp) <-  "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +to wgs84=0,0,0"
  
  print('-------Clump4-----')
  df_stack_shp$clump4 = 0
  num_clump <- 1
  # contador = 0
  ###Last clump
  for (c in unique(df_stack_shp$clump3)){
    # print(paste0(contador,"/", length(unique(df_stack_shp$clump3))))
    # contador = contador + 1
    subset_clump <- subset(df_stack_shp, df_stack_shp$clump3==c)
    if (nrow(subset_clump)==0){next}
    while (nrow(subset_clump)>0) {
      n_points=0
      buffer_clump <- buffer(subset_clump[1,], width=clumpDist, dissolve=T)
      # plot(buffer_clump)
      # points(subset_clump[1,], pch=21, col="blue", bg="blue")
      intersect_clump <- subset_clump[buffer_clump, ]
      # points(intersect_clump, pch=21, col="green", bg="green")
      if (nrow(intersect_clump) > 1){n_points = 2}
      while (n_points > 1) {
        n_points <- nrow(intersect_clump)
        buffer_clump <- buffer(intersect_clump, width=clumpDist, dissolve=T)
        intersect_clump <- subset_clump[buffer_clump, ]
        if (n_points == nrow(intersect_clump)){n_points = 0}
      }
      df_stack_shp$clump4[df_stack_shp$id %in% intersect_clump$id] <- num_clump
      subset_clump <- subset_clump[!subset_clump$id %in% intersect_clump$id, ]
      num_clump <-  num_clump+1
    }
  }
  gc()
  df_stack <- as.data.frame(df_stack_shp)[1:20]
  
  
  #Count recurrence
  df_stack$recurr <- 1
  ind_recurr <- df_stack[duplicated(df_stack[,c('x','y')]) | duplicated(df_stack[,c('x','y')], fromLast=TRUE),]
  for (n in ind_recurr$id){
    x_r <-  df_stack$x[df_stack$id == n]
    y_r <-  df_stack$y[df_stack$id == n]
    subset_xy <- subset(df_stack, df_stack$x == x_r & df_stack$y == y_r)
    recurrencia <- nrow(subset_xy)
    df_stack$recurr[df_stack$x == x_r & df_stack$y == y_r & df_stack$recurr == 1] <- recurrencia
  }

  
  return(df_stack)
}
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################

###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################



# prueba_rec <- burnRec(prueba)
#################################################################


#Get the ignition point
ignitionPts <- function(df_stack){
  ignition_points = data.frame()
  for (c in unique(df_stack$clump4)){
    df_clump <- subset(df_stack, df_stack$clump4 == c)
    size <- nrow(df_clump)*25
    last_date = max(df_clump$date)
    max_unc_fd <- max(df_clump$unc[df_clump$date == max(df_clump$date)])
    df_clump <- subset(df_clump, df_clump$date == min(df_clump$date))
    if (nrow(df_clump) == 1){
      df_min <- cbind(df_clump[,c("x","y","day","year","date","clump4")],size, last_date, unc_fd = df_clump$unc ,max_unc_fd = max_unc_fd, mean_unc_ld = df_clump$unc, pip = 1)
      ignition_points <- rbind(ignition_points, df_min)
    } else {
      df_min <- aggregate(df_clump[, c("x","y","day","year","date","clump4")], list(df_clump$clump4), mean)
      df_min <- cbind(df_min[,c("x","y","day","year","date","clump4")], size, last_date, unc_fd = min(df_clump$unc), max_unc_fd = max_unc_fd , mean_unc_ld = min(df_clump$unc), pip = nrow(df_clump))
      if (ncol(df_clump[df_clump$x %in% df_min$x & df_clump$y %in% df_min$y]) != 1){
        df_clump$dif_x <- df_clump$x - df_min$x; df_clump$dif_y <- df_clump$y - df_min$y
        df_clump$dif <- abs(df_clump$dif_x)+abs(df_clump$dif_y)
        df_clump <- subset(df_clump,df_clump$dif == min(df_clump$dif))[1,]
        df_min[,c("x","y")] <- df_clump[,c("x","y")]
        df_min$unc_fd<- df_clump$unc
      }
      ignition_points <- rbind(ignition_points, df_min)
    }
  }
  
 
  
  ################################################################
  #Comprobar que es correcto
  max(df_stack$clump4) == nrow(ignition_points)
  #Info dias
  colnames(ignition_points)[grep("^date$", colnames(ignition_points))] <- "first_date"
  ignition_points$duration <- ignition_points$last_date-ignition_points$first_date+1
  ignition_points$duration <- as.numeric(ignition_points$duration)
  ignition_points$n_month <- month(as.POSIXlt(ignition_points$first_date, format="%Y/%m/%d"))
  ignition_points$month <- mapvalues(ignition_points$n_month, c("1","2","3","4","5","6","7","8","9","10","11","12"), c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"), warn_missing = T)
  ignition_points$week_day <- weekdays(as.Date(ignition_points$first_date), abbreviate= F)
  ignition_points$week_num <- mapvalues(ignition_points$week_day, c("lunes","martes","miércoles","jueves","viernes","sábado","domingo"), c("1","2","3","4","5","6","7"), warn_missing = T)
  ignition_points$week_day <- mapvalues(ignition_points$week_day, c("lunes","martes","miércoles","jueves","viernes","sábado","domingo"), c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"), warn_missing = T)
  ignition_points$season <- as.factor(ifelse(ignition_points$month=="Dec"|ignition_points$month=="Jan"|ignition_points$month=="Feb","win",
                                             ifelse(ignition_points$month=="Mar"|ignition_points$month=="Apr"|ignition_points$month=="May","spr",
                                                    ifelse(ignition_points$month=="Jun"|ignition_points$month=="Jul"|ignition_points$month=="Aug","sum","aut"))))
  
  ignition_points_shp <- SpatialPointsDataFrame(ignition_points[c("x","y")], ignition_points)
  crs(ignition_points_shp) <-  "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +to wgs84=0,0,0"
  return(ignition_points_shp)
}


# ip <- ignitionPts(prueba)

# writeOGR(obj=ignition_points_shp, dsn="E:/MODIS/igntion_points_CA.shp", layer="E:/MODIS/igntion_points_CA.shp", driver="ESRI Shapefile")

#Alternative method ot outline perimeters
convex <- function(fire,d){
  
  p <- Polygon(fire[chull(fire@coords),]@coords)
  ps <- Polygons(list(p),1)
  sps <- buffer(SpatialPolygons(list(ps)),0.001, dissolve = TRUE)
  row.names(sps) <- row.names(d)
  fire.poly <- SpatialPolygonsDataFrame(sps,d)
  
  return(fire.poly)
  
}

#Main method to build perimeters
fromRaster <- function(fire,d){
  
  gridded(fire) <- TRUE
  fire.raster <- raster(fire)
  fire.poly <- raster::aggregate(rasterToPolygons(fire.raster))
  fire.poly <- SpatialPolygonsDataFrame(fire.poly,d)
  
  return(fire.poly)
  
}

#Get the perimeter of a single fire (clump)
getPerimeter <- function(clumpID,pts){
  
  f <- pts %>%
    filter(clump4 == clumpID)
  
  date <- min(f$date)
  duration <- max(f$date) - min(f$date) + 1
  barea <- nrow(f)*25
  coordinates(f) <- ~x+y
  
  d <- as.data.frame(cbind(clumpID,barea,date,duration))
  
  try(fire.poly <- fromRaster(f,d))
  
  if (exists("fire.poly") == FALSE) {
    
    fire.poly <- convex(f,d)
    
  }
  
  fire.poly$date <- as.Date(date)
  
  return(fire.poly)
  
}


#Iterate over fires and get perimeters
buildPerimeters <- function(pts.df){
  perimeters <- list()
  iter <- 1
  
  for(i in unique(pts.df$clump4)){
    
    print(i)
    perimeters[[iter]] <- getPerimeter(i,pts.df)
    iter <- iter + 1
    
  }
  
  perimeters.layer <- do.call(rbind,perimeters)
  return(perimeters.layer)
}
