#********************************************************************
# PREVI 1.- INSTAL.LAR i CARREGAR PAQUETS DE FUNCIONS
#********************************************************************

    # Llista de paquets de funcions que volem instal.lar
    .packages = c("mclust","FactoMineR","factoextra","ggplot2", "plotly", "xlsx","scales","knitr","dplyr","psych","stringr","VIM","pROC","lubridate")
    
    # Instal.la paquets sinó estan instal.lats
    .inst <- .packages %in% installed.packages()
    if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])
    
    # Carrega paquetes sinó están carregats
    lapply(.packages, require, character.only=TRUE)

#********************************************************************
# PREVI 2 - Integració i selecció de les dades d'interès a analitzar. 
#********************************************************************

    # Indiquem a la sessió quin és el directori de treball on tenim els fitxers.
    setwd("~/R/UOC-PAC")
    
    # Carreguem els dos dataset que tenim en dos fitxers .csv diferents.
    red_WINES <- read.table("wineQualityReds.csv", header=TRUE,sep=",")
    white_WINES <- read.table("wineQualityWhites.csv", header=TRUE,sep=",")
    

#********************************************************************
# 1.- Descripció del dataset. 
#********************************************************************    
    
    #Nom de les variables
    names(red_WINES)
    names(white_WINES)
    
    #Volem saber la notja mitjana per a vins negres i vins blancs
    mean(red_WINES$quality)
    mean(white_WINES$quality)
    
    #Tipus de variables del dataset
    res <- sapply(red_WINES,class)
    kable(data.frame(variables=names(res),clase=as.vector(res)))
    
    res <- sapply(white_WINES,class)
    kable(data.frame(variables=names(res),clase=as.vector(res)))
    
    rm(res)
    
    #Resum descriptiu del dataset
    summary(red_WINES)
    summary(white_WINES)

#********************************************************************
# 2.- Integració i selecció de les dades d'interès a analitzar.
#********************************************************************  
    
    #Seleccionem les variables estadístiques i les escalem a valors similars (Normalització)
    red_WINES.scaled <- as.data.frame(scale(red_WINES[,c(2:12)],center=T,scale=T))
    white_WINES.scaled <- as.data.frame(scale(white_WINES[,c(2:12)],center=T,scale=T))
    
    #Resum posterior al escalat
    summary(red_WINES.scaled)
    summary(white_WINES.scaled)
    
    
#********************************************************************
# 3.1- Neteja de les dades. Zeros o elements buits.
#********************************************************************  
    
    sapply(red_WINES.scaled, function(x) sum(is.na(x)))
    summarise_all(red_WINES.scaled, funs(sum(is.na(.))))
    
    sapply(white_WINES.scaled, function(x) sum(is.na(x)))
    summarise_all(white_WINES.scaled, funs(sum(is.na(.))))

#********************************************************************
# 3.2.- Neteja de les dades. Valors extrems. 
#********************************************************************  
  
  #Gràfica previa a retirar els outliers    
  boxplot(red_WINES.scaled, main="Box Plots red Wines")
  boxplot(white_WINES.scaled, main="Box Plots white Wines")
  
  #Funció per treure outliers
  #Considerem outliers aquells valors allunyats 3 desviacions stándard de la mitjana.
  OutliersReplace <- function(x, na.rm = TRUE, ...) 
  {
    qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
    H <- 1.5 * IQR(x, na.rm = na.rm)
    y <- x
    y[x < (qnt[1] - H)] <- NA
    y[x > (qnt[2] + H)] <- NA
    y
  }
  
  #Ho farem per a totes les variables
  
  #vins blancs
  ol_w_fixed.acidity <- OutliersReplace(white_WINES.scaled$fixed.acidity)
  ol_w_volatile.acidity <- OutliersReplace(white_WINES.scaled$volatile.acidity)
  ol_w_citric.acid <- OutliersReplace(white_WINES.scaled$citric.acid)
  ol_w_residual.sugar <- OutliersReplace(white_WINES.scaled$residual.sugar)
  ol_W_chlorides <- OutliersReplace(white_WINES.scaled$chlorides)
  ol_w_free.sulfur.dioxide <- OutliersReplace(white_WINES.scaled$free.sulfur.dioxide)
  ol_w_total.sulfur.dioxide <- OutliersReplace(white_WINES.scaled$total.sulfur.dioxide)
  ol_w_density <- OutliersReplace(white_WINES.scaled$density)
  ol_W_pH <- OutliersReplace(white_WINES.scaled$pH)
  ol_w_sulphates <- OutliersReplace(white_WINES.scaled$sulphates)
  ol_w_alcohol <- OutliersReplace(white_WINES.scaled$alcohol)
  
  #Gràfica posterior a la retirada dels outliers
  white_WINES.ol <- data.frame(ol_w_fixed.acidity,ol_w_volatile.acidity,ol_w_citric.acid,ol_w_residual.sugar,ol_W_chlorides,
                               ol_w_free.sulfur.dioxide,ol_w_total.sulfur.dioxide,ol_w_density,ol_W_pH,ol_w_sulphates,ol_w_alcohol)
  
  boxplot(white_WINES.ol[1:11], main="Box Plots white Wines sense outliers") 
  
  
  #vins negres
  ol_r_fixed.acidity <- OutliersReplace(red_WINES.scaled$fixed.acidity)
  ol_r_volatile.acidity <- OutliersReplace(red_WINES.scaled$volatile.acidity)
  ol_r_citric.acid <- OutliersReplace(red_WINES.scaled$citric.acid)
  ol_r_residual.sugar <- OutliersReplace(red_WINES.scaled$residual.sugar)
  ol_r_chlorides <- OutliersReplace(red_WINES.scaled$chlorides)
  ol_r_free.sulfur.dioxide <- OutliersReplace(red_WINES.scaled$free.sulfur.dioxide)
  ol_r_total.sulfur.dioxide <- OutliersReplace(red_WINES.scaled$total.sulfur.dioxide)
  ol_r_density <- OutliersReplace(red_WINES.scaled$density)
  ol_r_pH <- OutliersReplace(red_WINES.scaled$pH)
  ol_r_sulphates <- OutliersReplace(red_WINES.scaled$sulphates)
  ol_r_alcohol <- OutliersReplace(red_WINES.scaled$alcohol)
  
  #Gràfica posterior a la retirada dels outliers
  red_WINES.ol <- data.frame(ol_r_fixed.acidity,ol_r_volatile.acidity,ol_r_citric.acid,ol_r_residual.sugar,ol_r_chlorides,
                               ol_r_free.sulfur.dioxide,ol_r_total.sulfur.dioxide,ol_r_density,ol_r_pH,ol_r_sulphates,ol_r_alcohol)
  
  boxplot(red_WINES.ol[1:11], main="Box Plots red Wines sense outliers") 

  
  #Ara sí que tenim valors NA a causa de la retirada dels outliers
  sapply(white_WINES.ol, function(x) sum(is.na(x)))
  summarise_all(white_WINES.ol, funs(sum(is.na(.))))
  
  sapply(red_WINES.ol, function(x) sum(is.na(x)))
  summarise_all(red_WINES.ol, funs(sum(is.na(.))))
  
#********************************************************************
# 4.1.- Anàlisi de les dades.
# Selecció dels grups de dades que es volen analitzar/comparar
#********************************************************************  
  
  #VINS NEGRES  
  #Incorporem els id i les puntuacions dels vins en un únic dataframe
  df_red <- data.frame(red_WINES$id,red_WINES.ol, red_WINES$quality)
  
  #Preparem el dataset sense outliers
  ds_red <- filter(df_red, 
                   !is.na(ol_r_fixed.acidity),
                   !is.na(ol_r_volatile.acidity),
                   !is.na(ol_r_citric.acid),
                   !is.na(ol_r_residual.sugar),
                   !is.na(ol_r_chlorides),
                   !is.na(ol_r_free.sulfur.dioxide),
                   !is.na(ol_r_total.sulfur.dioxide),
                   !is.na(ol_r_density),
                   !is.na(ol_r_pH),
                   !is.na(ol_r_sulphates),
                   !is.na(ol_r_alcohol)
                  )
  
  #VINS BLANCS
  #Incorporem els id i les puntuacions dels vins en un únic dataframe
  df_white <- data.frame(white_WINES$id,white_WINES.ol, white_WINES$quality)
  
  #Preparem els vins blancs sense Outliers
  ds_white <- filter(df_white, 
                       !is.na(ol_w_fixed.acidity),
                       !is.na(ol_w_volatile.acidity),
                       !is.na(ol_w_citric.acid),
                       !is.na(ol_w_residual.sugar),
                       !is.na(ol_W_chlorides),
                       !is.na(ol_w_free.sulfur.dioxide),
                       !is.na(ol_w_total.sulfur.dioxide),
                       !is.na(ol_w_density),
                       !is.na(ol_W_pH),
                       !is.na(ol_w_sulphates),
                       !is.na(ol_w_alcohol)
                       )
  
  #Ja tenim apunt els dos datasets sobre els quals aplicarem els algoritmes de clustering
  summary(ds_white)
  count(ds_white)
  
  summary(ds_red)
  count(ds_red)

#********************************************************************
# 4.2.- Anàlisi de les dades.
# Comprovació de la normalitat i homogeneïtat de la variància. 
#********************************************************************  
    
#Normalitat:
  #Blancs
  shapiro.test(ds_white$ol_w_fixed.acidity)
  shapiro.test(ds_white$ol_w_volatile.acidity)
  shapiro.test(ds_white$ol_w_citric.acid)
  shapiro.test(ds_white$ol_w_residual.sugar)
  shapiro.test(ds_white$ol_W_chlorides)
  shapiro.test(ds_white$ol_w_free.sulfur.dioxide)
  shapiro.test(ds_white$ol_w_total.sulfur.dioxide)
  shapiro.test(ds_white$ol_w_density)
  shapiro.test(ds_white$ol_W_pH)
  shapiro.test(ds_white$ol_w_sulphates)
  shapiro.test(ds_white$ol_w_alcohol)
  
  #Negres
  shapiro.test(ds_red$ol_r_fixed.acidity)
  shapiro.test(ds_red$ol_r_volatile.acidity)
  shapiro.test(ds_red$ol_r_citric.acid)
  shapiro.test(ds_red$ol_r_residual.sugar)
  shapiro.test(ds_red$ol_r_chlorides)
  shapiro.test(ds_red$ol_r_free.sulfur.dioxide)
  shapiro.test(ds_red$ol_r_total.sulfur.dioxide)
  shapiro.test(ds_red$ol_r_density)
  shapiro.test(ds_red$ol_r_pH)
  shapiro.test(ds_red$ol_r_sulphates)
  shapiro.test(ds_red$ol_r_alcohol)
  
#Homogeneitat de la variància
  
  aux_red <- as.data.frame(c(red_WINES[1:13], type=1))
  aux_white <- as.data.frame(c(white_WINES[1:13], type=0))
  
  total_wines <- rbind(aux_red,aux_white)
  
  a<- total_wines[total_wines$type==1, "fixed.acidity"]
  b<- total_wines[total_wines$type==0, "fixed.acidity"]
  fligner.test(x= list(a,b))
  
  a<- total_wines[total_wines$type==1, "alcohol"]
  b<- total_wines[total_wines$type==0, "alcohol"]
  fligner.test(x= list(a,b))
  
  a<- total_wines[total_wines$type==1, "pH"]
  b<- total_wines[total_wines$type==0, "pH"]
  fligner.test(x= list(a,b))
  
#********************************************************************
# 4.3.- Anàlisi de les dades.
# Aplicació de proves estadístiques per comparar els grups de dades.
# Aplicar proves de contrast d'hipòtesis, correlacions, regressions, etc. 
# Aplicar almenys tres mètodes d'anàlisi diferents. 
#******************************************************************** 
  #Abans de plantejar la hipòtesis d'investigació, mirem si algunes variables tenen relació entre elles
  
    plot(ds_red[2:12])
    cor(ds_red[2:12])
  
  # Clustering
  # El nostre objectiu és obtenir els grups òptims de vins a partir de les seves característiques químiques
  # Un cop aconseguit, comparar les notes mitjanes que obtenen aquests grups entre ells per veure si existeixen
  # vins amb categories molt diferents
  # Ho farem amb vins blancs i amb vins negres per comparar les notes mitjanes de les categories entre elles.
  
  # Utilitzarem tres algoritmes de clustering i seleccionarem el que ens agrupi millor els vins
  # El nombre òptim de clusters
  cluster.optim <- fviz_nbclust(ds_red[2:12], hcut, method = "gap_stat")
  cluster.optim.gap <- cluster.optim$data$gap
  cluster.optim.gap == max(cluster.optim.gap)
  k <- 10 # nombre òptim de clusters
  
  # Hierarchical
  # k-means
  # GMM - Models gaussians mixtes
  
  # Hierarchical
  res.hc <- eclust(ds_red[2:12], "hclust", k = k, graph = FALSE) 
  fviz_dend(res.hc, rect = TRUE, show_labels = FALSE)
  
  pairs(ds_red[2:12],col=res.hc$cluster,pch = 21)
  res.hc.clusters <- res.hc$cluster
  eclust(ds_red[2:12], "hclust", k = k, graph = TRUE) 
  
  # k-means
  km.res <- eclust(ds_red[2:12], "kmeans", k = k,nstart = 25, graph = FALSE)
  fviz_silhouette(km.res)
  
  pairs(ds_red[2:12],col=km.res$cluster,pch = 21)
  res.km.clusters <- km.res$cluster
  eclust(ds_red[2:12], "kmeans", k = k, nstart = 25, graph = TRUE)
  
  # GMM
  gmm.res = Mclust(ds_red[2:12], G = k)
  summary(gmm.res, parameters = TRUE)
  
  plot(gmm.res, what = "classification")
  res.gmm.clusters <- gmm.res$classification

    
#********************************************************************
# 5.- Representació dels resultats a partir de taules i gràfiques. 
#********************************************************************    

  #Dedidirem quin mètode ens ha classificat millor les observacions:
  pca <- PCA(ds_red[2:12],graph = TRUE)
  
  plot(pca$ind$coord[,1:2], pch=16, col=res.hc.clusters, 
       main="HIERARCHICAL")
  
  plot(pca$ind$coord[,1:2], pch=16, col=res.km.clusters, 
       main="k-MEANS")
  
  plot(pca$ind$coord[,1:2], pch=16, col=res.gmm.clusters, 
       main="GMM")
    
#********************************************************************
# 6.- Resolució del problema. Conclusions 
#******************************************************************** 
  
  #Aqui tenim els resultats de la classificació
  res.hc.clusters
  res.km.clusters
  res.gmm.clusters
  
  #creem el fitxer de sortida
  ds_resultat <- data.frame(ds_red$red_WINES.id, res.km.clusters, ds_red$red_WINES.quality)
  
  setnames(ds_resultat, "ds_red.red_WINES.id","id")
  setnames(ds_resultat, "res.km.clusters", "Cluster")
  setnames(ds_resultat, "ds_red.red_WINES.quality", "Quality")

  #Nota mitja total
  mean(ds_resultat$Quality)
  
  #nota mitja Cluster=1
  mean(filter(ds_resultat, Cluster==1)$Quality)
  #nota mitja Cluster=2
  mean(filter(ds_resultat, Cluster==2)$Quality)
  #nota mitja Cluster=3
  mean(filter(ds_resultat, Cluster==3)$Quality)
  #nota mitja Cluster=4
  mean(filter(ds_resultat, Cluster==4)$Quality)
  #nota mitja Cluster=5
  mean(filter(ds_resultat, Cluster==5)$Quality)
  #nota mitja Cluster=6
  mean(filter(ds_resultat, Cluster==6)$Quality)
  #nota mitja Cluster=7
  mean(filter(ds_resultat, Cluster==7)$Quality)
  #nota mitja Cluster=8
  mean(filter(ds_resultat, Cluster==8)$Quality)
  #nota mitja Cluster=9
  mean(filter(ds_resultat, Cluster==9)$Quality)
  #nota mitja Cluster=10
  mean(filter(ds_resultat, Cluster==10)$Quality)
 
  #Cluster amb la nota més baixa
  filter(ds_resultat, Cluster==7)
  
  #Cluster amb la nota més alta
  filter(ds_resultat, Cluster==6)
  
  #Exportem els resultats
  write.csv(ds_resultat,"output_red_WINES.csv", row.names = FALSE)

  #********************************************************************
  # ANNEX.- Repetim a passos ràpids pels vins blancs 
  #********************************************************************
  
  
  cluster2.optim <- fviz_nbclust(ds_white[2:12], hcut, method = "gap_stat")
  cluster2.optim.gap <- cluster2.optim$data$gap
  cluster2.optim.gap == max(cluster2.optim.gap)
  k2 <- 10 # nombre òptim de clusters
  
  # Directament aplicarem el k-means
  km2.res <- eclust(ds_white[2:12], "kmeans", k = k2,nstart = 25, graph = FALSE)
  fviz_silhouette(km2.res)
  
  pairs(ds_white[2:12],col=km2.res$cluster,pch = 21)
  res2.km.clusters <- km2.res$cluster
  eclust(ds_white[2:12], "kmeans", k = k2, nstart = 25, graph = TRUE)
  
  
  #creem el fitxer de sortida
  ds_resultat2 <- data.frame(ds_white$white_WINES.id, res2.km.clusters, ds_white$white_WINES.quality)
  
  setnames(ds_resultat2, "ds_white.white_WINES.id","id")
  setnames(ds_resultat2, "res2.km.clusters", "Cluster")
  setnames(ds_resultat2, "ds_white.white_WINES.quality", "Quality")
  
  #Nota mitja total
  mean(ds_resultat2$Quality)
  
  #nota mitja Cluster=1
  mean(filter(ds_resultat2, Cluster==1)$Quality)
  #nota mitja Cluster=2
  mean(filter(ds_resultat2, Cluster==2)$Quality)
  #nota mitja Cluster=3
  mean(filter(ds_resultat2, Cluster==3)$Quality)
  #nota mitja Cluster=4
  mean(filter(ds_resultat2, Cluster==4)$Quality)
  #nota mitja Cluster=5
  mean(filter(ds_resultat2, Cluster==5)$Quality)
  #nota mitja Cluster=6
  mean(filter(ds_resultat2, Cluster==6)$Quality)
  #nota mitja Cluster=7
  mean(filter(ds_resultat2, Cluster==7)$Quality)
  #nota mitja Cluster=8
  mean(filter(ds_resultat2, Cluster==8)$Quality)
  #nota mitja Cluster=9
  mean(filter(ds_resultat2, Cluster==9)$Quality)
  #nota mitja Cluster=10
  mean(filter(ds_resultat2, Cluster==10)$Quality)
  
  #Exportem els resultats
  write.csv(ds_resultat2,"output_white_WINES.csv", row.names = FALSE)
