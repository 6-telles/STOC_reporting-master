library(sf)
##Importation des données
d <- read.csv2("library/coord_precis.csv",stringsAsFactors=FALSE)



##Importation des shapefiles des 6 régions biogéographiques (4 fichiers : shp,shx,prj,dbr)
Alpin <- st_read(dsn = 'Regions/Alpin/Alpin.shp',
                 layer = 'Alpin')
Atlantique_aquitain <- st_read(dsn = 'Regions/Atlantique_aquitain/Atlantique_aquitain.shp',
                               layer = 'Atlantique_aquitain')
Atlantique_NO <- st_read(dsn = 'Regions/Atlantique_NO/Atlantique_NO.shp',
                         layer = 'Atlantique_NO')
Atlantique_Paris <- st_read(dsn = 'Regions/Atlantique_Paris/Atlantique_Paris.shp',
                            layer = 'Atlantique_Paris')
Continental <- st_read(dsn = 'Regions/Continental/Continental.shp',
                       layer = 'Continental')
Mediterraneen <- st_read(dsn = 'Regions/Mediterraneen/Mediterraneen.shp',
                         layer = 'Mediterraneen')


polygon <- st_union(Alpin,Atlantique_aquitain, by_feature = TRUE, is_coverage = TRUE)
polygon <- st_union(polygon,Atlantique_NO, by_feature = TRUE, is_coverage = TRUE)
polygon <- st_union(polygon,Atlantique_Paris, by_feature = TRUE, is_coverage = TRUE)
polygon <- st_union(polygon,Continental, by_feature = TRUE, is_coverage = TRUE)
polygon <- st_union(polygon,Mediterraneen, by_feature = TRUE, is_coverage = TRUE)


## Récupération des coordonnées des sommets de toutes les zones géographiques
shpA <- Alpin
coordA <- st_coordinates(shpA)
coordA <- coordA[,1:2]
shpAA <- Atlantique_aquitain
coordAA <- st_coordinates(shpAA)
coordAA <- coordAA[,1:2]
shpANO <- Atlantique_NO
coordANO <- st_coordinates(shpANO)
coordANO <- coordANO[,1:2]
shpAP <- Atlantique_Paris
coordAP <- st_coordinates(shpAP)
coordAP <- coordAP[,1:2]
shpC <- Continental
coordC <- st_coordinates(shpC)
coordC <- coordC[,1:2]
shpM <- Mediterraneen
coordM <- st_coordinates(shpM)
coordM <- coordM[,1:2]


coordr <- list(coordA,coordAA,coordANO,coordAP,coordC,coordM)


## vecteur contenant le nom de toutes les régions biogéographiques
regions <- c("Alpin","Atlantique_aquitain","Atlantique_NO","Atlantique_Paris","Continental","Mediterraneen")





## Définition de la fonction point dans un polygone
## regarde si la station appartient ou non à la région biogéographique
point_in_polygon <- function(polygon, point){
  #' Raycasting Algorithm to find out whether a point is in a given polygon.
  #' Performs the even-odd-rule Algorithm to find out whether a point is in a given polygon.
  #' This runs in O(n) where n is the number of edges of the polygon.
  #' @param polygon an array representation of the polygon where polygon[i,1] is the x Value of the i-th point and polygon[i,2] is the y Value.
  #' @param point   an array representation of the point where point[1] is its x Value and point[2] is its y Value
  #' @return whether the point is in the polygon (not on the edge, just turn < into <= and > into >= for that)

  # A point is in a polygon if a line from the point to infinity crosses the polygon an odd number of times
  odd = FALSE
  # For each edge (In this case for each point of the polygon and the previous one)
  i = 0
  j = nrow(polygon) - 1
  while(i < nrow(polygon) - 1){
    i = i + 1
    # If a line from the point into infinity crosses this edge
    # One point needs to be above, one below our y coordinate
    # ...and the edge doesn't cross our Y corrdinate before our x coordinate (but between our x coordinate and infinity)
    if (((polygon[i,2] > point[2]) != (polygon[j,2] > point[2]))
        && (point[1] < ((polygon[j,1] - polygon[i,1]) * (point[2] - polygon[i,2]) / (polygon[j,2] - polygon[i,2])) + polygon[i,1])){
      # Invert odd
      odd = !odd
    }
    j = i
  }
  # If the number of crossings was odd, the point is in the polygon
  return (odd)
}




## Récupération des coordonnées des stations
coordAll <- aggregate(cbind(as.numeric(d$LONG),as.numeric(d$LAT)) ~ d$ID_PROG,data=d,mean)
coordAll
colnames(coordAll)[2:3] <- c("LONG","LAT")
coordAll
ID_exclu <- as.character(coordAll$ID_PROG[which(coordAll$LAT<40)])
coordAll <- subset(coordAll,LAT>40)
site <- nrow(coordAll)

## Création d'un data.frame pour associer les régions biogéographiques aux stations
regionSite <- array(NA,c(site,2))
colnames(regionSite)[1:2] <- c('NEW.ID_PROG','BIOGEO')
regionSite[,1] = coordAll[,1]
regionSite

## Remplissage du dataframe
for(ss in 1:site){
  point<-coordAll[ss,2:3]
  point
  o=1
  r=FALSE
  while ((r==FALSE )&(o<7)){
    polygone=coordr[o]
    polygone<-as.data.frame(polygone)
    st_as_sf(polygone)
    st_crs(polygone) # pour vérifier que la couche est bien en lambert 93 crs/epsg = 2154
    st_transform(polygone,crs=4326)
    st_crs(polygone) <- 4326

    ## regarde si la station appartient ou non à la région biogéographique
    r<-point_in_polygon(polygone,point)
    r
    o=o+1
  }
  regionSite[ss,2]=regions[o]
}
regionSite


library(ggplot2)

d <- read.csv2("library/coord_precis.csv",stringsAsFactors=FALSE,dec=".")

d_sf <- st_as_sf(d,coords=c("LONG","LAT"),crs = 4626)


eco <- st_read("library/tnc_terr_ecoregions.shp")
## ma couche d'écoregion est mondial ce n'est pas pratique alors je la crop à l'extent des site de capture
Ymin <- min(d$LAT)
Ymax <- max(d$LAT)
Xmin <- min(d$LONG)
Xmax <- max(d$LONG)
eco <- st_crop(eco,xmin=Xmin,ymin=Ymin,xmax=Xmax,ymax=Ymax)

## on change la projection pour passer dans une projection qui respect les distances, c'est mieux avant des faire des buffer
d_sf_l93 <- st_transform(d_sf,crs = 2154)
eco_l93 <- st_transform(eco,crs = 2154)

st_crs(eco)
st_crs(eco_l93)

## on affect à chaque point le polygon le plus proche
d_first <- st_join(d_sf_l93, eco_l93, join = st_nearest_feature)

gg <- ggplot() + geom_sf(data=eco_l93,aes(fill=ECO_NAME),colour=NA,alpha=.5)
gg <- gg + geom_sf(data=d_first,aes(colour = ECO_NAME))
gg



## on pourrait aussi agrandir u peu les buffer pour avoir un peu plus de site pour la réfférence.
eco_l93_buff <- st_buffer(eco_l93,20000)
## dans ce cas là certain site seront dans plusieur référence, mais ce n'est pas forcément grave
d_second <- st_join(d_sf_l93, eco_l93_buff, join = st_within)

gg <- ggplot() + geom_sf(data=eco_l93_buff,aes(fill=ECO_NAME),colour=NA,alpha=.5)
gg <- gg + geom_sf(data=d_second,aes(colour = ECO_NAME),alpha=0.5)
gg

## si on veut récupére que le data.frame de la jointure
d_first_df <- d_first
st_geometry(d_first_df) <- NULL
