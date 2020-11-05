

source("functions/fun_generic_latin1.r")


##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param site
##' @param d
##' @param fileLog
##' @param aggAllSession
##' @param aggSession
##' @param aggSessionRef
##' @return
##' @author Romain Lorrilliere
##'
selectedSession.site <- function(site=NULL,d,fileLog=NULL,print.fig=TRUE,save.fig=FALSE,aggAllSession=NULL,aggSession=NULL,aggSessionRef=NULL,add_title=TRUE) {


    ##    fileSave <- "output/summarySession.csv"  #####
    ##    aggAllSession <- read.csv2(fileSave,stringsAsFactors=FALSE) #####
    ##    fileSave <- "output/historicSession.csv" #####
    ##    aggSession <- read.csv2(fileSave,stringsAsFactors=FALSE) #####
    ##    fileSave <- "output/SessionReference.csv" #####
    ##    aggSessionRef <-  read.csv2(fileSave,stringsAsFactors=FALSE) #####
    
## Si le tableau AggAllSession est vide, alors on lui attribue le tableau summarySession (résultats sessions) du dossier data_France
    if(is.null(aggAllSession)) {
        fileSave <- "data_France/summarySession.csv"
        aggAllSession <- read.csv2(fileSave,stringsAsFactors=FALSE)
    }
## Si le tableau aggSession est vide, alors on lui attribue le tableau historicSession (horaires sessions) du dossier data_France
    if(is.null(aggSession)) {
        fileSave <- "data_France/historicSession.csv"
        aggSession <- read.csv2(fileSave,stringsAsFactors=FALSE)
    }
## Si le tableau aggSessionRef est vide, alors on lui attribue le tableau SessionReference (références stations= num+jour session) du dossier data_France
    if(is.null(aggSessionRef)) {
        fileSave <- "data_France/SessionReference.csv"
        aggSessionRef <-  read.csv2(fileSave,stringsAsFactors=FALSE)
    }

## Conversion en caractères de la colonne SESSION (1 2 3 ou 0 si non prise en compte) des trois tableaux
    aggSession$SESSION <- as.character(aggSession$SESSION)
    aggSessionRef$SESSION <- as.character(aggSessionRef$SESSION)
    aggAllSession$SESSION <- as.character(aggAllSession$SESSION)



## Si on ne précise pas de site particulier, triage des stations dans l'ordre ascendant après conversion en caractères
    if(is.null(site)) site <- sort(as.character(unique(d$NEW.ID_PROG)))
    
## Pour chaque station :
    for(ss in site)
    {
        ## Sélection dans le tableau AggSession des lignes correspondant à la station pour lesquelles une date est précisée
        aggSession.s <- subset(aggSession,NEW.ID_PROG == ss & !is.na(JULIANDAY))
        ## ????????????????????????????????????????????????????????????
        minYear <- max(min(aggSession.s$YEAR)-2,min(aggSession$YEAR))
        maxYear <- min(max(aggSession.s$YEAR)+2,max(aggSession$YEAR))
        ## Isole les lignes de aggSessionRef correspondant à la station
        aggSessionRef.s <- subset(aggSessionRef,NEW.ID_PROG == ss )

        ## Associe des couleurs aux valeurs de la colonne SESSION
        vcolour <- c("0"="#737373","1" = "#1c30f3","2" = "#951cf3","3" = "#f90d28","0" = "#1e1112")
        ## Création d'intervalles de dates pour les sessions
        vdate <- c("05-01","05-15","06-01","06-15","07-01","07-15","08-01")
        ## Création d'un intervalle de dates complètes au format numérique
        vdate <- as.numeric(format(as.Date(paste(2000,vdate,sep="-")),"%j"))-1
        ## Tri dans l'ordre croissant des dates du vecteur vdate et celles de aggSessionRef
        vdate <- sort(c(vdate,aggSessionRef.s$JULIANDAY))
## Si on veut mettre un titre, le graphique aura pour titre "Date des sessions selectionnées pour la station n°__", sinon pas de titre
        if(add_title) title_txt <- paste("Date des sessions selectionnées pour la station",ss) else title_txt <- ""
## Création du graphique (figure 2 du rapport) 
        gg <- ggplot(data=aggAllSession,aes(x=YEAR,y=med,colour=SESSION,fill=SESSION,group=paste(SESSION)))
        gg <- gg + geom_ribbon(aes(ymin=CIinf,ymax=CIsup),alpha=.2,colour=NA)
        gg <- gg + geom_line(aes(y=CIquart_inf),size=0.6,alpha=.3)+ geom_line(aes(y=CIquart_sup),size=0.6,alpha=.3)

        gg <- gg + geom_line(size=1.5,alpha=.3)
        gg <- gg + geom_hline(data=aggSessionRef.s,aes(yintercept=JULIANDAY,colour=SESSION),size=1,linetype="dotted")
        gg <- gg + geom_line(data=subset(aggSession.s,SESSION != 0),aes(y=JULIANDAY),size=1.2)
        gg <- gg + geom_point(data=aggSession.s,aes(y=JULIANDAY,shape=VALIDE),size=2.5)
        gg <- gg + scale_fill_manual( values=vcolour,limits=c("0","1","2","3"),name = "Session",breaks=c("1","2","3"))
        gg <- gg + scale_colour_manual(values=vcolour,limits=c("0","1","2","3"),name = "Session",breaks=c("1","2","3"))
        gg <- gg + scale_shape_manual(values=c("TRUE"=19,"FALSE"=8),label=c("TRUE"="oui","FALSE"="non"),breaks=c(TRUE,FALSE),name="Session valide")
        gg <- gg + coord_cartesian(xlim=c(minYear,maxYear)) + scale_x_continuous(breaks=pretty_breaks())
        gg <- gg + scale_y_continuous(labels = function(x) format(as.Date(as.character(x), "%j"), "%d-%m"),breaks = vdate)
        gg <- gg + labs(title=title_txt,x="Année",y="Date")
## Si on veut sauvegarder la figure, création d'un dossier pour la station dans le dossier output
        if(save.fig) {
            ggfile <- paste("output/",ss,"/session_",ss,".png",sep="")
            catlog(c("Check",ggfile,":"),fileLog)
            ggsave(ggfile,gg)
            catlog(c("\n"),fileLog)
        }
## Si on veut afficher la figure, affichage de la figure
        if(print.fig) print(gg)

                                        #        cmdMove <- paste("cp ",ggfile," output/lesSessions",sep="")
                                        #        system(cmdMove)
    }

}






##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param site
##' @param d
##' @param fileLog
##' @return
##' @author Romain Lorrilliere
##'
carteStation <- function(site=NULL,d,add_local=FALSE,fileLog=NULL,print.fig=TRUE,save.fig=FALSE,add_title=TRUE) {

    require(rgdal)
    require(ggmap)
    require(maptools)
    library(sf)
    require(maps)
    if(add_local) { library(OpenStreetMap)

        library(gridExtra)

    }

    france <- map_data("france")
    if(is.null(site)) site <- sort(as.character(unique(d$NEW.ID_PROG)))
    if(save.fig) checkRepertories(site)

    coordAll <- aggregate(cbind(d$LON,d$LAT) ~ d$NEW.ID_PROG,data=d,mean)
    colnames(coordAll)[2:3] <- c("LON","LAT")
    ID_exclu <- as.character(coordAll$NEW.ID_PROG[which(coordAll$LAT<40)])
    coordAll <- subset(coordAll,LAT>40)

    ## transformation en SpatialPointDataFrame
                                        #   coordinates(coordAll) <- ~ LON + LAT
    ## on déclare le système de coordonnées de référence: dégrés décimaux WGS84

                                        #   proj4string(coordAll) <- CRS("+init=epsg:2154") # lambert 93

    ## conversion en Lamber étendu
                                        #  coordAllWGS84 <- spTransform(coordAll,CRS("+proj=longlat +ellps=WGS84")) #transformation en WGS84
                                        #  coordAll2 <- data.frame(NEW.ID_PROG = coordAllWGS84@data,as.data.frame(coordAllWGS84@coords))
                                        #  colnames(coordAll2)[1] <- "NEW.ID_PROG"
                                        #  ID_exclu <- as.character(coordAll2$NEW.ID_PROG[which(coordAll2$LAT<40)])
                                        #  coordAll2 <- subset(coordAll2,LAT>40)

    anneeAll <- unique(subset(d,select=c("NEW.ID_PROG","FIRST.YEAR","LAST.YEAR")))
    typeAll <-  unique(subset(d,select=c("NEW.ID_PROG","HABITAT")))

                                        #  coordAll2 <- merge(coordAll2,typeAll,by="NEW.ID_PROG",all=TRUE)
                                        #  coordAll2 <- merge(coordAll2,anneeAll,by="NEW.ID_PROG",all=TRUE)
    colnames(coordAll)[1] <- "NEW.ID_PROG"
    coordAll2 <- merge(coordAll,typeAll,by="NEW.ID_PROG",all=TRUE)
    coordAll2 <- merge(coordAll2,anneeAll,by="NEW.ID_PROG",all=TRUE)


    for(ss in site)
    {
        h <- as.character(coordAll2$HABITAT[coordAll2$NEW.ID_PROG==ss])
        fy <- max(coordAll2$FIRST.YEAR[coordAll2$NEW.ID_PROG==ss] - 1,min(d$YEAR))
        ly <- min(coordAll2$LAST.YEAR[coordAll2$NEW.ID_PROG==ss] + 1,max(d$YEAR))


        coordAllh <- subset(coordAll2,HABITAT == h & FIRST.YEAR <= ly & LAST.YEAR >= fy)
        nbs <- length(unique(coordAllh$NEW.ID_PROG))-1
        dcoord.s <- subset(coordAll2,NEW.ID_PROG == ss)

        coordAllh$DUREE <- apply(subset(coordAllh,select=c("FIRST.YEAR","LAST.YEAR")),1,FUN = function(X) (min(X[2],ly) - max(X[1],fy) + 1))

        max_duree <- max(coordAllh$DUREE)
        min_duree <- 1
        inter_duree <- 3

        world1 <- sf::st_as_sf(map('world', plot = FALSE, fill = TRUE))
        france <- sf::st_as_sf(map('france', plot = FALSE, fill = TRUE))


        mytheme <- theme(plot.title = element_text(face = "bold",size = rel(1.2), hjust = 0.5),
                         panel.background = element_rect(colour = NA),
                         plot.background = element_rect(colour = NA),
                         axis.title = element_text(face = "bold",size = rel(1)),
                         axis.title.y = element_text(angle=90,vjust =2),
                         axis.title.x = element_text(vjust = -0.2),
                         legend.position=NULL)


        if(add_title) title_txt <- paste0("Localisation de la station ",ss,"\n et des ",nbs," stations de référence de type ",h, "\n suivies entre ",fy," et ",ly) else title_txt <- ""
        gg <- ggplot()
        gg <- gg + geom_sf(data = world1,fill="white", colour="#7f7f7f", size=0.2)+ geom_sf(data = france,fill="white", colour="#7f7f7f", size=0.5)
        gg <- gg + coord_sf(xlim=c(-5,9),ylim=c(41.5,52))
        gg <- gg + geom_point(data = coordAll2,aes(LON,LAT),shape=1,size=1,colour="black")
        gg <- gg + geom_point(data = dcoord.s,aes(LON,LAT), colour="red",size=4)
        gg <- gg + geom_point(data = coordAllh, aes(LON,LAT,colour=DUREE),size=2,shape=19)
        gg <- gg + labs(x="",y="",title=title_txt)
        gg <- gg + scale_colour_gradient2(low = "#b2182b",mid="#92c5de",high = "#053061",midpoint = 3,name="Nombre\nd'années\nde suivi",limits=c(min_duree,max_duree))

        if(add_local) {

            degloc <- 0.25
            lat_min <- min(dcoord.s$LAT,na.rm=TRUE) - degloc
            lat_max <- max(dcoord.s$LAT,na.rm=TRUE) + degloc
            lon_min <- min(dcoord.s$LON,na.rm=TRUE) - degloc
            lon_max <- max(dcoord.s$LON,na.rm=TRUE) + degloc


            coordAll2.loc <- subset(coordAll2,LAT > lat_min & LAT < lat_max & LON > lon_min & LON < lon_max)
            coordAllh.loc <- subset(coordAllh,LAT > lat_min & LAT < lat_max & LON > lon_min & LON < lon_max)

            map <- openmap(c(lat_max,lon_min), c(lat_min,lon_max), zoom = NULL,type = c("osm", "stamen-toner", "stamen-terrain","stamen-watercolor", "esri","esri-topo")[1], mergeTiles = TRUE)
            map.latlon <- openproj(map, projection = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
            ggloc <- autoplot(map.latlon)  + labs(x = "", y="")
            ggloc <- ggloc + geom_point(data = coordAll2.loc,aes(x=LON,y=LAT),shape=1,size=1,colour="black")
            ggloc <- ggloc + geom_point(data = dcoord.s,aes(x=LON,y=LAT), colour="red",size=4)
            ggloc <- ggloc + geom_point(data =coordAllh.loc, aes(x=LON,y=LAT,colour=DUREE),size=2,shape=19)
            ggloc <- ggloc + labs(x="",y="",title="",fy," et ",ly,sep="")
            ggloc <- ggloc +   scale_colour_gradient2(low = "#b2182b",mid="#92c5de",high = "#053061",midpoint = 3,name="",limits=c(min_duree,max_duree),guide =FALSE)

        }

        if(save.fig) {
            if(add_local) g <- arrangeGrob(gg,ggloc,ncol = 1, nrow = 2,heights=c(4,3)) else g <- gg
            ggfile <- paste("output/",ss,"/carte_",ss,".png",sep="")
            catlog(c("Check",ggfile,":"),fileLog)
            ggsave(ggfile,g)#,height = 10.5,width = 13)
            catlog(c("\n"),fileLog)
        }

        if(print.fig)
            if(add_local)  grid.arrange(gg,ggloc,ncol = 1, nrow = 2,heights=c(4,3)) else print(gg)
    }
}





speciesRelativeAbund.site <- function(d,site=NULL,col_nomsp=NULL,seuil_prop_station=0,fileLog="log.txt",print.fig=TRUE,save.fig=FALSE,add_title=TRUE,return_table=FALSE) {

    require(ggplot2)
    dsp <- read.csv2("library/sp.csv",stringsAsFactors=FALSE)

    if(is.null(site)) {
        site <- sort(unique(d$NEW.ID_PROG))
        habitats <- unique(d$HABITAT)
    } else {
        habitats <- unique(subset(d,NEW.ID_PROG %in% site)$HABITAT)
    }

    ggTableH <- read.csv2("data_France/quantileEspece_France_.csv",stringsAsFactors=FALSE)

    ggTableH$HABITAT <- as.character(ggTableH$HABITAT)
    ggTableH$SP <- as.character(ggTableH$SP)

    ggTableH <- subset(ggTableH,PROP_STATION_CAPTURE>seuil_prop_station)

    if(!is.null(col_nomsp)) tabsp <- unique(d[,c("SP",col_nomsp)])

    for(ss in site) {
                                        #  print(ss)

        ds <- subset(d,AGE_first == "AD"& NEW.ID_PROG==ss)
        habitat <- ds$HABITAT[1]
        ggTableHab <- subset(ggTableH,HABITAT==habitat)
        listSP.s <- ggTableHab$SP

        ds <- subset(ds,SP %in% listSP.s)
        ds$SP <- as.character(ds$SP)

        du.bague <- unique(subset(ds,select=c("SP","BAGUE","YEAR")))
        ggNbCapt <- data.frame(table(du.bague$SP,du.bague$YEAR),stringsAsFactors=FALSE)
        colnames(ggNbCapt) <- c("SP","YEAR","ABUND")
                                        #  ggNbCapt <- subset(ggNbCapt,ABUND>0)
        ggNbCapt$SP <- as.character(ggNbCapt$SP)
        ggNbCapt$YEAR <- as.numeric(as.character(ggNbCapt$YEAR))

        nbsite <- ggTableHab[,c("SP","nb_site")]
        ggNbCapt <- merge(ggNbCapt,nbsite,by="SP")

        ggTableS <- aggregate(ggNbCapt$ABUND,by=list(ggNbCapt$SP),sum)
        colnames(ggTableS) <- c("SP","ABUNDsite_sum_ad")
        ggTableS$SP <- as.character(ggTableS$SP)


        ggTableM <- aggregate(ggNbCapt$ABUND,by=list(ggNbCapt$SP),median)
        colnames(ggTableM) <- c("SP","ABUNDsite_med_ad")
        ggTableM$SP <- as.character(ggTableM$SP)
        ggTableS <- merge(ggTableS,ggTableM,by="SP")






        ds2 <- subset(d,NEW.ID_PROG==ss & SP %in% listSP.s)
        ds2$SP <- as.character(ds2$SP)

        du2.bague <- unique(subset(ds2,select=c("SP","BAGUE","YEAR")))
        ggNbCapt2 <- data.frame(table(du2.bague$SP,du2.bague$YEAR),stringsAsFactors=FALSE)
        colnames(ggNbCapt2) <- c("SP","YEAR","ABUND")
                                        #  ggNbCapt <- subset(ggNbCapt,ABUND>0)
        ggNbCapt2$SP <- as.character(ggNbCapt2$SP)
        ggNbCapt2$YEAR <- as.numeric(as.character(ggNbCapt2$YEAR))

        ggTableS2 <- aggregate(ggNbCapt2$ABUND,by=list(ggNbCapt2$SP),sum)
        colnames(ggTableS2) <- c("SP","ABUNDsite_sum_all")
        ggTableS2$SP <- as.character(ggTableS2$SP)


        ggTableM2 <- aggregate(ggNbCapt2$ABUND,by=list(ggNbCapt2$SP),median)
        colnames(ggTableM2) <- c("SP","ABUNDsite_med_all")
        ggTableM2$SP <- as.character(ggTableM2$SP)
        ggTableS2 <- merge(ggTableS2,ggTableM2,by="SP")

        ggTableS <- merge(ggTableS,ggTableS2,by="SP")

        ggTable <- merge(ggTableS,ggTableHab,by="SP")


        if(!is.null(col_nomsp)) {
            ggTable <- merge(ggTable,tabsp,by="SP")
            ggTable$sp_fig <- ggTable[,col_nomsp]
            ggNbCapt <- merge(ggNbCapt,tabsp,by="SP")
            ggNbCapt$sp_fig <- ggNbCapt[,col_nomsp]

        } else {
            ggTable$sp_fig <- ggTable$SP
            ggNbCapt$sp_fig <- ggNbCapt$SP
        }

        ggTable$sp_fig <- paste0(ggTable$sp_fig," (",ggTable$nb_site,")")
        ggNbCapt$sp_fig <- paste0(ggNbCapt$sp_fig," (",ggNbCapt$nb_site,")")


        if(add_title) title_txt <- paste("Site ",ss) else title_txt <- ""
        colour_break <- sort(unique(round(seq(min(ggNbCapt$YEAR),max(ggNbCapt$YEAR),length.out=5))))

        gg <- ggplot(ggTable,aes(x=reorder(sp_fig, (ABUNDsite_sum_ad)),y=ABUND50))
        gg <- gg + geom_linerange(aes(ymin=ABUND025,ymax=ABUND975),colour="#08306b",alpha=.5)
        gg <- gg + geom_crossbar(aes(ymin = ABUND25, ymax = ABUND75), width = 0.5,alpha=.5,colour="#08306b",fill="#08306b")
        ## gg <- gg + theme(axis.text.x  = element_text(angle=90,vjust=.5))
        gg <- gg + labs(x="",y="Nombre d'individus adultes capturés par an",title=title_txt,colour="")
        gg <- gg + geom_jitter(data=ggNbCapt,aes(x=sp_fig,y=ABUND,colour=YEAR),size=2.2,alpha=.8,width = .1)
        gg <- gg + scale_y_log10(breaks=c(0,1,2,5,10,20,50,100,200,400)) + coord_flip()
        gg <- gg + scale_colour_continuous(low="#ffa200",high="#960000",breaks=colour_break)#"#07307b","#0c5ef6","#c10909","#ea5d18"

        if(print.fig) print(gg)

        if(save.fig) {
            ggfile <- paste("output/",ss,"/nbCapture_site_",ss,".png",sep="")
            catlog(c("Check",ggfile,":"),fileLog)
            ppi <- 200
            ggsave(ggfile,gg,width=11, height=8, unit="in",limitsize = FALSE)
            catlog(c("\n"),fileLog)
        }
    } # END  for(ss in site)
    if(return_table){
        ggNbCapt <- dcast(ggNbCapt[,c("SP","YEAR","ABUND")],SP~YEAR)
        ggTable <- merge(ggTable,ggNbCapt,by="SP")
        ggTable <- ggTable[order(ggTable$ABUNDsite_sum_ad,decreasing=TRUE),]
        return(ggTable)

    }
} ### END speciesRelativeAbund.site




##' Graphe de variation d'abondance annuelle par espece et par site
##'
##' .. content for \details{} ..
##' @title abundanceSpeciesYear.site
##' @param d data
##' @param site vecteur de site si pas tous les sites
##' @param limitTime ?????
##' @param fileLog nom du fichier de log
##' @return NULL
##' @author Romain Lorrilliere

abundanceSpeciesYear.site <- function(d,site=NULL,species=NULL,nom_sp=NULL,limitTime=TRUE,fileLog="log.txt",print.fig=TRUE,save.fig=FALSE,add_title=FALSE,facet_title=TRUE)
{
                                        # site="9"

    require(ggplot2)

    aggTableNat <- read.csv2("data_France/N_adulte_sp_France.csv")

    if(is.null(site)) site <- sort(as.character(unique(d$NEW.ID_PROG)))

    ## boucle pour figure par site
    for(ss in site) {

        ## selection des adultes
        dhAd <- subset(d,NEW.ID_PROG == ss & AGE_first == "AD")
        hab <- as.character(dhAd$HABITAT[1])
        lesAnnees <- as.numeric(as.character(unique(dhAd$YEAR)))
        if(is.null(species)) listSP.s <- sort(as.character(unique(dhAd$ESPECE))) else listSP.s <- species



        for(sp in listSP.s) {

            dhAdSp <- subset(dhAd,ESPECE==sp)
            du.bague <- unique(subset(dhAdSp,select=c("BAGUE","ESPECE","NEW.ID_PROG","YEAR")))
            du.hab <- unique(subset(dhAdSp,select=c("NEW.ID_PROG","HABITAT")))

            t.abund <- tapply(du.bague$BAGUE,list(du.bague$YEAR),length)
            t.abund <- ifelse(is.na(t.abund),0,t.abund)
            t.abund <- data.frame(t.abund)
                                        #   t.abund <- reshape(t.abund, direction = "long",
                                        #                      idvar = "",
                                        #                      ids=rownames(t.abund),
                                        #                      varying = 1:ncol(t.abund),sep="")
                                        #            colnames(t.abund) <- c("YEAR","ABUND")

            if(nrow(t.abund) != length(lesAnnees)) {
                t.site <- rbind(data.frame(YEAR=as.numeric(rownames(t.abund)),ABUND=t.abund[,1]),data.frame(YEAR=lesAnnees[!(lesAnnees%in%as.numeric(rownames(t.abund)))],ABUND=0))
                t.site <- t.site[order(t.site$YEAR),]
            } else {
                t.site <- data.frame(YEAR=as.numeric(rownames(t.abund)),ABUND=t.abund[,1])
                t.site <- t.site[order(t.site$YEAR),]
            }

            t.nbnf.session <- unique(subset(dhAd,select=c("NEW.ID_PROG","YEAR","SESSION","FS")))
            t.fs <- data.frame(aggregate(FS~YEAR, data=t.nbnf.session, sum, na.rm=FALSE))
            t.fs <- subset(t.fs,!is.na(FS))


            t.site <- merge(t.site,t.fs,by=c("YEAR"))

            t.site$ABUND.ADJUST <- t.site$ABUND / t.site$FS*(120*3)



            minYear <- max(min(t.site$YEAR)-1,min(aggTableNat$YEAR))
            maxYear <- min(max(t.site$YEAR)+1,max(aggTableNat$YEAR))

            aggTable.s <- subset(aggTableNat,HABITAT == hab & ESPECE == sp & YEAR >= minYear & YEAR <= maxYear)
            ggSite <- data.frame(YEAR = t.site$YEAR,HABITAT = paste("Station:",ss),CIinf=NA,CIquart_inf=NA,med=t.site$ABUND.ADJUST,CIquart_sup=NA,CIsup=NA,ESPECE=sp)

            aggTable.s <- rbind(aggTable.s,ggSite)

            if(add_title) {
                if(!is.null(nom_sp)) sp_fig <- nom_sp[sp == listSP.s] else sp_fig <- sp
                title_txt <- paste(sp_fig,": Station ",ss,sep="")
            } else {
                title_txt <- ""
            }

            if(facet_title) if(!is.null(nom_sp)) aggTable.s$facet <- nom_sp[sp == listSP.s] else aggTable.s$facet <- sp

            gg <- ggplot(aggTable.s,aes(x=YEAR,y=med,colour=HABITAT,fill=HABITAT))
            if(!is.null(facet_title)) gg <- gg + facet_wrap(.~facet)
            gg <- gg + geom_ribbon(aes(ymin=CIinf,ymax=CIsup),alpha=.2,colour = NA)
            gg <- gg + geom_line(aes(y=CIquart_inf),size=0.6,alpha=.6)+ geom_line(aes(y=CIquart_sup),size=0.6,alpha=.6)
            gg <- gg + geom_line(size=1.5,alpha=1)
            gg <- gg + geom_point(data=ggSite,size=3)
            gg <- gg + scale_colour_manual(values =setNames(c("#07307b","#d71818"),c(hab,paste("Station:",ss))))
            gg <- gg + scale_fill_manual(values= setNames(c("#07307b","#d71818"),c(hab,paste("Station:",ss))),guide=FALSE)
            gg <- gg + coord_cartesian(xlim=c(minYear,maxYear)) + scale_x_continuous(breaks=pretty_breaks())
            gg <- gg + theme(legend.position="none")
            gg <- gg + labs(title=title_txt,
                            x="Année",y="Nombre d'individus adultes capturés",
                            colour="")

            if(print.fig) print(gg)

            if(save.fig) {

                ggfile <- paste("output/",ss,"/N_adulte_Site_",ss,"_",sp,".png",sep="")

                catlog(c("Check",ggfile,":"),fileLog)
                ggsave(ggfile,gg)
                catlog(c("\n"),fileLog)
            }

        } # END  for(sp in listSP.s)
    } # END  for(ss in site)

} ### END abundanceSpeciesYear.site




abundanceYear.site <- function(d,site=NULL,fileLog="log.txt",print.fig=TRUE,save.fig=FALSE,add_title=TRUE)
{
    require(ggplot2)

                                        #tabSpeQuant <- read.csv2("output/quantileEspece_France_.csv",stringsAsFactors=FALSE)

    if(is.null(site)) site <- sort(unique(d$NEW.ID_PROG))

    aggTable <- read.csv2("data_France/N_adulte_France_.csv")

    dhAd <- subset(d,AGE_first == "AD")

    du.bague <- unique(subset(dhAd,select=c("BAGUE","NEW.ID_PROG","YEAR")))
    du.hab <- unique(subset(dhAd,select=c("NEW.ID_PROG","HABITAT")))
    t.abund <- tapply(du.bague$BAGUE,list(du.bague$NEW.ID_PROG,du.bague$YEAR),length)
    t.abund <- ifelse(is.na(t.abund),0,t.abund)
    t.abund <- data.frame(t.abund)
    t.abund <- reshape(t.abund, direction = "long",
                       idvar = "NEW.ID_PROG",
                       ids=rownames(t.abund),
                       varying = 1:ncol(t.abund),sep="")
    colnames(t.abund) <- c("YEAR","ABUND","NEW.ID_PROG")

    t.nbnf.session <- unique(subset(dhAd,select=c("NEW.ID_PROG","YEAR","SESSION","FS")))
    t.nbnf <- aggregate(FS~NEW.ID_PROG+YEAR, data=t.nbnf.session, sum, na.rm=FALSE)

    t.fs.output <- tapply(t.nbnf.session$FS,list(t.nbnf.session$NEW.ID_PROG,t.nbnf.session$YEAR), sum)
                                        #t.fs.saisi <- tapply(t.nbnf.session$FS,list(t.nbnf.session$NEW.ID_PROG,t.nbnf.session$YEAR), sum)

    t.fs <- data.frame(NEW.ID_PROG = rep(rownames(t.fs.output),ncol(t.fs.output)),
                       YEAR = rep(colnames(t.fs.output),each=nrow(t.fs.output)),
                       FS.OUPUT = as.vector(as.matrix(t.fs.output)))
    t.fs <- subset(t.fs,!is.na(FS.OUPUT))


    t.abund <- merge(t.abund,t.fs,by=c("NEW.ID_PROG","YEAR"))
    t.abund <- merge(t.abund,du.hab,by="NEW.ID_PROG")

    t.abund$ABUND.ADJUST <- t.abund$ABUND / t.abund$FS.OUPUT*(120*3)

    for(ss in site) {
                                        # catlog(c("site:",s,"\n"),fileLog)


        tSite <- subset(t.abund, NEW.ID_PROG==ss)
        habitat <- as.character(tSite$HABITAT[1])
        minYear <- max(min(tSite$YEAR)-1,min(t.abund$YEAR))
        maxYear <- min(max(tSite$YEAR)+1,max(t.abund$YEAR))

        aggTable.s <- subset(aggTable,HABITAT==habitat)
        aggTable.s <- subset(aggTable.s,YEAR>=minYear & YEAR<=maxYear)

        ggSite <- data.frame(YEAR = tSite$YEAR,HABITAT = paste("Station:",ss),CIinf=NA,CIquart_inf=NA,med=tSite$ABUND.ADJUST,CIquart_sup=NA,CIsup=NA)

        aggTable.s <- rbind(aggTable.s,ggSite)
        aggTable.s$HABITAT <- as.character(aggTable.s$HABITAT)

        if(add_title) title_txt <- paste("Captures toutes espèces\npour la station ",ss,sep="") else title_txt <- ""

     #   browser()
        gg <- ggplot(aggTable.s,aes(x=YEAR,y=med,colour=HABITAT,fill=HABITAT))
        gg <- gg + geom_ribbon(aes(ymin=CIinf,ymax=CIsup),alpha=.2,colour = NA)
        gg <- gg + geom_line(aes(y=CIquart_inf),size=0.6,alpha=.6)+ geom_line(aes(y=CIquart_sup,color=HABITAT),size=0.6,alpha=.6)
        gg <- gg + geom_line(size=1.5,alpha=1)
        gg <- gg + geom_point(data=ggSite,size=3)

        gg <- gg + scale_colour_manual(values =setNames(c("#07307b","#d71818"),c(habitat,paste("Station:",ss))))
        gg <- gg + scale_fill_manual(values= setNames(c("#07307b","#d71818"),c(habitat,paste("Station:",ss))),guide=FALSE)

        ##gg <- gg + scale_colour_manual(breaks=c(habitat,paste("Station:",ss)),values =c("#db6363","#08306b"))
        ##gg <- gg + scale_fill_manual(breaks=c(habitat,paste("Station:",ss)),values =c("#db6363","#08306b"),guide=FALSE)
        gg <- gg + theme(legend.position="none")
        gg <- gg + labs(title=title_txt,
                        x="Année",y="Nombre d'individus adultes capturés",
                        colour="")

        if(print.fig) print(gg)

        if(save.fig) {
            ggfile <- paste("output/",ss,"/N_adulte_Site_",ss,".png",sep="")
            catlog(c("Check",ggfile,":"),fileLog)
            ggsave(ggfile,gg)
            catlog(c("\n"),fileLog)
        }
    }# END for(ss in site)
} ### END abundanceYear.site











productivityYear.site <- function(d,site=NULL,limitTime=TRUE,fileLog="log.txt",print.fig=TRUE,save.fig=FALSE,add_title=TRUE)
{
    require(ggplot2)
                                        #site="193"


    dp <- unique(subset(d,AGE_first != "VOL" & MIGRATION != "" ,select = c("MIGRATION","YEAR","NEW.ID_PROG","HABITAT","BAGUE","AGE_first")))

    dNbAge <- aggregate(BAGUE ~ (MIGRATION + YEAR + NEW.ID_PROG + HABITAT + AGE_first), data=dp,length)

    dNbJuv <- subset(dNbAge,AGE_first=="JUV")

    dNbAd <- subset(dNbAge,AGE_first=="AD")

    dNb <- merge(dNbJuv,dNbAd,by=c("MIGRATION","YEAR","NEW.ID_PROG","HABITAT"),all=TRUE)
    dNb$BAGUE.x[is.na(dNb$BAGUE.x)] <- 0
    dNb$BAGUE.y[is.na(dNb$BAGUE.y)] <- 0
    dNb$PROD <- dNb$BAGUE.x / (dNb$BAGUE.y)
    dNb$PROD[dNb$PROD == Inf] <- NA

                                        #   ggTable.abund <- na.omit(ggTable.abund)

    aggTable <- read.csv2("data_France/Productivite_all_.csv")

    if(is.null(site)) site <- sort(as.character(unique(d$NEW.ID_PROG)))



                                        #  print(site)
    for(ss in site) {
                                        #  catlog(c("site:",s,"\n"),fileLog)
        tSite <- subset(dNb, NEW.ID_PROG==ss)
        habitat <- as.character(tSite$HABITAT[1])
        minYear <- max(min(tSite$YEAR)-1,min(dp$YEAR))
        maxYear <- min(max(tSite$YEAR)+1,max(dp$YEAR))

        aggTable.s <- subset(aggTable,HABITAT==habitat)
        aggTable.s <- subset(aggTable.s,YEAR>=minYear & YEAR<=maxYear)

        ggSite <- data.frame(YEAR = tSite$YEAR,HABITAT= paste("Station:",ss),MIGRATION=tSite$MIGRATION,
                             CIinf=NA,CIquart_inf=NA,med=tSite$PROD,CIquart_sup=NA,CIsup=NA)
        aggTable.s <- rbind(aggTable.s,ggSite)

        if(add_title) title_txt <-  paste("Station",ss) else title_txt <- ""

        gg <- ggplot(aggTable.s,aes(x=YEAR,y=med,colour=HABITAT,fill=HABITAT))+ facet_grid(MIGRATION~.,scales="free_y")
        gg <- gg + geom_ribbon(aes(ymin=CIinf,ymax=CIsup),alpha=.2,colour = NA )
        gg <- gg + geom_line(alpha=.8,size=1.5)
        gg <- gg + geom_line(aes(y=CIquart_inf),alpha=.5,size=.8)+geom_line(aes(y=CIquart_sup),alpha=.5,size=.8)
        gg <- gg + geom_point(data=subset(aggTable.s,HABITAT==paste("Station:",ss)),size=2)
        gg <- gg + scale_x_continuous(breaks=pretty_breaks())
        gg <- gg + scale_colour_manual(values =setNames(c("#07307b","#d71818"),c(habitat,paste("Station:",ss))))
        gg <- gg + scale_fill_manual(values= setNames(c("#07307b","#d71818"),c(habitat,paste("Station:",ss))),guide=FALSE)
        gg <- gg + labs(title=title_txt,
                        x="Année",y="Productivité: Njuv/Nad")
        gg <- gg + theme(legend.position="none")

        if(print.fig) print(gg)


        if(save.fig) {
            ggfile <- paste("output/",ss,"/Productivite_Site_",ss,".png",sep="")
            catlog(c("Check",ggfile,":"),fileLog)
            ggsave(ggfile,gg)
            catlog(c("\n"),fileLog)
        }

    }# END for(ss in site)
}### END productivityYear.site




productivityYearSpecies.site <- function(d,site=NULL,species=NULL,nom_sp=NULL,fileLog="log.txt",print.fig=TRUE,save.fig=FALSE,add_title=TRUE,facet = TRUE,seuilsp=0)
{
    require(ggplot2)
### d <- read.csv2("output/data.csv",stringsAsFactors=FALSE)

    if(!is.null(nom_sp)) t_nom_sp <- data.frame(species=species,nom_sp=nom_sp)

    tabSpeQuant <- read.csv2("data_france/quantileEspece_France_.csv",stringsAsFactors=FALSE)

    dp <- unique(subset(d,AGE_first != "VOL" & MIGRATION != "",select = c("ESPECE","MIGRATION","YEAR","NEW.ID_PROG","HABITAT","BAGUE","AGE_first")))

    dNbAge <- aggregate(BAGUE ~ (ESPECE + MIGRATION + YEAR + NEW.ID_PROG + HABITAT + AGE_first), data=dp,length)

    dNbJuv <- subset(dNbAge,AGE_first=="JUV")

    dNbAd <- subset(dNbAge,AGE_first=="AD")

    dNb <- merge(dNbJuv,dNbAd,by=c("ESPECE","MIGRATION","YEAR","NEW.ID_PROG","HABITAT"),all=TRUE)
    dNb$BAGUE.x[is.na(dNb$BAGUE.x)] <- 0
    dNb$BAGUE.y[is.na(dNb$BAGUE.y)] <- 0
    dNb$PROD <- dNb$BAGUE.x / (dNb$BAGUE.y)
    dNb$PROD[dNb$PROD == Inf] <- NA


    aggTable <- aggregate(PROD ~ (YEAR + HABITAT+ESPECE + MIGRATION), data= dNb, quantile,c(0.025,.25,0.5,.75,0.975))
    aggTable <- data.frame(aggTable[,1:4],aggTable[5][[1]][,1:5])
    colnames(aggTable)[5:9] <- c("CIinf","CIquart_inf","med","CIquart_sup","CIsup")

    for(hh in unique(aggTable$HABITAT)) {
        tabSpeHab <- subset(tabSpeQuant,HABITAT==hh)
        listSP <- sort(as.character(tabSpeHab$SP[tabSpeHab$PROP_STATION_CAPTURE>seuilsp]))
        aggTable.h <- subset(aggTable,HABITAT==hh & ESPECE %in% listSP)
        if (hh== unique(aggTable$HABITAT)[1]) aggTable.all <- aggTable.h else aggTable.all <- rbind(aggTable.all,aggTable.h)
    }# END for(hh in unique(aggTable$HABITAT))


    if(is.null(site))site <- sort(as.character(unique(d$NEW.ID_PROG)))

    for(ss in site) {
        dNbsite <- subset(dNb,NEW.ID_PROG==ss)

        hh <- as.character(dNbsite$HABITAT[1])
        tabSpeHab <- subset(tabSpeQuant,HABITAT==hh)

        listSP <- sort(as.character(tabSpeHab$SP[tabSpeHab$PROP_STATION_CAPTURE>seuilsp]))
        dNbsite <- subset(dNbsite,ESPECE %in% listSP)

        dNbsite$NB <- dNbsite$BAGUE.x + dNbsite$BAGUE.y
        aggNbSp <- aggregate(NB~(YEAR+ESPECE),data=dNbsite,sum)
        aggSp <- aggregate(NB~ESPECE,data=aggNbSp,FUN= median)

        if(is.null(species)) listSP.s <- sort(as.character(aggSp$ESPECE[aggSp$NB>10])) else listSP.s <- species

        dNbsite <- subset(dNbsite,ESPECE %in% listSP.s)

        if(nrow(dNbsite) > 0) {
            minYear <- max(min(dNbsite$YEAR)-1,min(aggTable.all$YEAR))
            maxYear <- min(max(dNbsite$YEAR)+1,max(aggTable.all$YEAR))

            aggTable.h <- subset(aggTable.all,HABITAT==hh & ESPECE %in% listSP.s & YEAR >= minYear & YEAR <= maxYear)

            ggSite <- data.frame(YEAR = dNbsite$YEAR,HABITAT= paste("Station:",ss),ESPECE=dNbsite$ESPECE,MIGRATION=dNbsite$MIGRATION,
                                 CIinf=NA,CIquart_inf=NA,med=dNbsite$PROD,CIquart_sup=NA,CIsup=NA)
            aggTable.s <- rbind(aggTable.h,ggSite)




            for(sp in listSP.s) {


                if(add_title) {
                    if(!is.null(nom_sp)) sp_fig <- as.character(t_nom_sp$nom_sp[t_nom_sp$species == sp]) else sp_fig <- sp
                    title_txt <- paste(sp_fig,": Station ",ss,sep="")
                } else {
                    title_txt <- ""
                }

                aggTable.ss <- subset(aggTable.s,ESPECE == sp)

                if(nrow(aggTable.ss)>0) {
                      } #END if(nrow(aggTable.ss)>0)

                if(facet) if(!is.null(nom_sp)) aggTable.ss$facet <- as.character(t_nom_sp$nom_sp[t_nom_sp$species == sp]) else aggTable.ss$facet <- sp

                gg <- ggplot(aggTable.ss,aes(x=YEAR,y=med,colour=HABITAT,fill=HABITAT))

                gg <- gg + geom_ribbon(aes(x=YEAR,ymin=CIinf,ymax=CIsup),alpha=.2,colour = NA )
                gg <- gg + geom_line(aes(y=CIquart_inf),alpha=.5,size=.8)+geom_line(aes(y=CIquart_sup),alpha=.5,size=.8)
                gg <- gg + geom_line(alpha=.8,size=1.5)

                gg <- gg + geom_point(data=subset(aggTable.ss,HABITAT==paste("Station:",ss)),size=2)
                gg <- gg + scale_colour_manual(values =setNames(c("#07307b","#d71818"),c(hh,paste("Station:",ss))))
                gg <- gg + scale_fill_manual(values= setNames(c("#07307b","#d71818"),c(hh,paste("Station:",ss))),guide=FALSE)
 if(!is.null(facet)) gg <- gg + facet_wrap(.~facet,scales="free_y")

                  gg <- gg + coord_cartesian(xlim=c(minYear,maxYear)) + scale_x_continuous(breaks=pretty_breaks())
                gg <- gg + labs(title=title_txt,
                                x="Année",y="Productivité: Njuv/Nad")
                gg <- gg + theme(legend.position="none")

                if(print.fig) print(gg)


                if(save.fig) {
                    ggfile <- paste("output/",ss,"/Productivite_Espece_Site_",ss,".png",sep="")
                    catlog(c("Check",ggfile,":"),fileLog)
                    ggsave(ggfile,gg)
                    catlog(c("\n"),fileLog)
                }

            }# END for(sp in listSP.s)
        }# END   if(nrow(dNbsite) > 0)
    }# END for(ss in site) {
}### END productivityYearSpecies.site






bodyCondition.site <- function(d,site=NULL,community_level=TRUE,species_level=TRUE,species=NULL,nom_sp=NULL,limitTime=TRUE,seuilAbondanceAnnee=30,seuilAbondanceAnneeSite=10,fileLog="log.txt",print.fig=TRUE,save.fig=FALSE,add_title=TRUE,facet=TRUE,return.table=FALSE)
{


    require(ggplot2)

    if(is.null(site))
        site <- sort(unique(d$NEW.ID_PROG))


    if(community_level) {
        aggTable <-  read.csv2("data_France/bodyCondition_France.csv")
        aggTable$gr <- paste("FRANCE,",aggTable$AGE_first)
        aggTable$SITE <- "FRANCE"
    }

    if(species_level) {
        aggTable.sp <- read.csv2("data_France/bodyCondition_France_sp_.csv")
        aggTable.sp$gr <- paste("FRANCE,",aggTable.sp$AGE_first)
        aggTable.sp$SITE <- "FRANCE"
    }


    for(s in site) {
        tSite <- subset(d,AGE_first!="VOL" & NEW.ID_PROG==s & (!(is.na(MA_borne))) & (!(is.na(LP_indice_borne))))
        h <- tSite$HABITAT[1]

        if(community_level) {

            if(nrow(tSite)>10) {

                aggTable_s <- aggregate(MA_indice_borne ~ AGE_first + YEAR + HABITAT,tSite,quantile, c(0.025,0.25,0.5,0.75,0.975))
                if(nrow(aggTable_s)==1) {
                    aggTable_s <- as.data.frame(c(aggTable_s[,1:3],aggTable_s[4][[1]][,1:5]))
                    colnames(aggTable_s)[1:3] <- c("AGE_first","YEAR","HABITAT")
                } else {
                    aggTable_s <- data.frame(aggTable_s[,1:3],aggTable_s[4][[1]][,1:5])
                }

                colnames(aggTable_s)[4:8] <- c("CIinf","CIquart_inf","med","CIquart_sup","CIsup")

                aggTable_s$gr <- paste("Site ",s,",",aggTable_s$AGE_first, sep="")
                aggTable_s$SITE <- s
                aggTableTot <- rbind(subset(aggTable,HABITAT==h),aggTable_s)
                if(print.fig |save.fig) {

                    minYear <- max(min(aggTable_s$YEAR)-1,min(aggTable$YEAR))
                    maxYear <- min(max(aggTable_s$YEAR)+1,max(aggTable$YEAR))


                    if(add_title) title_txt <- paste("Station ",s,": toutes espèces confondues",sep="") else title_txt <- ""

                    gg <- ggplot(data=aggTableTot,aes(x=YEAR,y=med,colour=gr,fill=gr)) + facet_grid(AGE_first~.,scales="free_y")
                    gg <- gg + geom_ribbon(aes(ymin=CIinf,ymax=CIsup),alpha=.4,colour = NA)
                    gg <- gg + geom_point(size=2,alpha=.8) + geom_line(size=1.5,alpha=.8)
                    gg <- gg + geom_line(aes(y=CIquart_inf),alpha=.5,size=.8)+geom_line(aes(y=CIquart_sup),alpha=.5,size=.8)
                    gg <- gg + scale_colour_manual(values =c("#07307b","#0c5ef6","#c10909","#ea5d18"),name="")
                    gg <- gg + scale_fill_manual(values =c("#07307b","#0c5ef6","#c10909","#ea5d18"),guide=FALSE)
                    gg <- gg + labs(title=title_txt,x="Année",y="Condition corporelle: (MA-MA_mean_sp)/MA_mean_sp")
                    gg <- gg + coord_cartesian(xlim=c(minYear,maxYear)) + scale_x_continuous(breaks=pretty_breaks())
                    gg <- gg + theme(legend.position="none")

                    if(print.fig) print(gg)

                    if(save.fig) {
                        ggfile <- paste("output/",s,"/bodyCondition_site",s,".png",sep="")
                        catlog(c("Check",ggfile,":"),fileLog)
                        ggsave(ggfile,gg)
                        catlog(c("\n"),fileLog)
                    }
                } # END if(print.fig |save.fig)
            } # END  if(nrow(tSite)>10)

        } # END if(community_level) {

        if(species_level) {

            if(!is.null(nom_sp)) t_nom_sp <- data.frame(species=species,nom_sp=nom_sp)

            if(is.null(species)) {

                t.seuil<- aggregate(BAGUE ~ SP + YEAR, data = unique(subset(tSite,select=c("BAGUE","SP","YEAR"))), FUN = length)
                t.seuil<- aggregate(BAGUE ~ SP , data =  t.seuil, FUN = median)
                t.seuil <- subset( t.seuil,BAGUE > 10)#length(unique(d.a$NEW.ID_PROG))*.25)

                tS.seuil <- subset(tSite,SP %in% t.seuil$SP)
                list_sp <- unique(t.seuil$SP)

            } else {
                list_sp <- species

                tS.seuil <- subset(tSite, SP %in% list_sp)
            } # END ELSE if(is.null(species))

            if(nrow(tS.seuil)>0) {

                tS.seuil$MA_LP <- tS.seuil$MA_borne/tS.seuil$LP_indice_borne

                aggTable.sp_s <- aggregate(MA_LP ~ SP + YEAR + AGE_first + HABITAT, tS.seuil, quantile, c(0.025,0.25,0.5,0.75,0.975))

                if(nrow(aggTable.sp_s)==1) {
                    aggTable.sp_s <- as.data.frame(c(aggTable.sp_s[,1:4],aggTable.sp_s[5][[1]][,1:5]))
                    colnames(aggTable.sp_s)[1:4] <- c("SP","YEAR","AGE_first","HABITAT")
                } else {
                    aggTable.sp_s <- data.frame(aggTable.sp_s[,1:4],aggTable.sp_s[5][[1]][,1:5])
                }

                colnames(aggTable.sp_s)[5:9] <- c("CIinf","CIquart_inf","med","CIquart_sup","CIsup")

                aggTable.sp_s$gr <- paste("Site ",s,",",aggTable.sp_s$AGE_first, sep="")
                aggTable.sp_s$SITE <- s

                aggTable.sp <- aggTable.sp[,colnames(aggTable.sp_s)]
                aggTableTot.sp <- rbind(subset(aggTable.sp,SP %in% list_sp & HABITAT==h),aggTable.sp_s)

              if(print.fig |save.fig) {

                    plot_local_sp(aggTableTot.sp,species,nom_sp,print.fig,save.fig,add_title,facet,facet_group="AGE_first",y_lab="Condition corporelle: Masse/(Ecart à la taille moyenne + 1)",x_lab="Année",title_txt="",vecCol=c("#07307b","#0c5ef6","#c10909","#ea5d18"), minYear =min(aggTable.sp_s$YEAR)-1,maxYear = max(aggTable.sp_s$YEAR))

                } # END if(print.fig |save.fig)
        } else {  # END if(nrow(tS.seuil)>0)
            aggTableTot.sp <- tS.seuil
        }# END ELSE if(nrow(tS.seuil)>0)
    } # END  if(species_level)
} # END for(s in site)
if(return.table) {
    l.ret <- list()

    if(community_level) l.ret[["community"]] <- aggTableTot
    if(species_level) l.ret[["species"]] <- aggTableTot.sp
}

}### END bodyCondition.site




returnRate.site <- function(d,site=NULL,community_level=TRUE,species_level=TRUE,species=NULL,nom_sp=NULL,seuilAbondanceAnnee=30,seuilAbondanceAnneeSite=10,fileLog="log.txt",print.fig=TRUE,save.fig=FALSE,add_title=TRUE,facet=TRUE)
{
    require(ggplot2)

    if(is.null(site)) {
        site <- sort(unique(d$NEW.ID_PROG))
        habitat <- NULL
    } else {
        d <- subset(d,NEW.ID_PROG %in% site)
        habitat <- as.character(unique(d$HABITAT))
    }

    if(!is.null(habitat)) d <- subset(d,HABITAT == habitat)


    vecHab <- unique(d$HABITAT)
    for(h in vecHab)
    {

        d.a0 <- unique(subset(d,AGE_first=="AD" & HABITAT==h,select=c("BAGUE","SP","NEW.ID_PROG","YEAR","MIGRATION")))
        d.a2 <- unique(subset(d,AGE_first=="AD" & HABITAT==h,select=c("BAGUE","NEW.ID_PROG","YEAR","MIGRATION")))
        d.a2$YEAR <- d.a2$YEAR - 1
        d.a2$RETURN <- TRUE
        d.a <- merge(d.a0,d.a2, by=c("BAGUE","NEW.ID_PROG","YEAR","MIGRATION"),all.x=TRUE)
        d.a$RETURN[is.na(d.a$RETURN)]<- FALSE

        d.j0 <- unique(subset(d,AGE_first=="JUV",select=c("BAGUE","SP","NEW.ID_PROG","YEAR","MIGRATION")))
        d.j2 <- unique(subset(d,AGE_first=="AD",select=c("BAGUE","NEW.ID_PROG","YEAR","MIGRATION")))
        d.j2$YEAR <- d.j2$YEAR - 1
        d.j2$RETURN <- TRUE
        d.j <- merge(d.j0,d.j2, by=c("BAGUE","NEW.ID_PROG","YEAR","MIGRATION"),all.x=TRUE)
        d.j$RETURN[is.na(d.j$RETURN)]<- FALSE


        if(community_level) {
            aggTable <-  read.csv2("data_France/returnRate_all_France.csv")
            aggTable$gr <- paste("FRANCE,",aggTable$AGE_first)
            aggTable$SITE <- "FRANCE"
            aggTable <- subset(aggTable,HABITAT == h)




            d.ret <- NULL


            if(nrow(d.a)>0) {
                d.ret.a <- aggregate(RETURN ~ NEW.ID_PROG + YEAR + MIGRATION, data = d.a, FUN = returnRateAssessment)
                d.compt.a <- aggregate(BAGUE ~ NEW.ID_PROG + YEAR + MIGRATION , data = d.a, FUN = length)
                d.ret.a <- merge(d.ret.a,d.compt.a,by=c("NEW.ID_PROG","YEAR","MIGRATION"))
                d.ret.a$AGE_first <- "AD"

                d.ret <- rbind(d.ret,
                           d.ret.a)

           }

            if(nrow(d.j)>0) {
            d.ret.j <- aggregate(RETURN ~ NEW.ID_PROG + YEAR + MIGRATION , data = d.j, FUN = returnRateAssessment)
            d.compt.j <- aggregate(BAGUE ~ NEW.ID_PROG + YEAR + MIGRATION, data = d.j, FUN = length)
            d.ret.j <- merge(d.ret.j,d.compt.j,by=c("NEW.ID_PROG","YEAR","MIGRATION"))
              d.ret.j$AGE_first <- "JUV"


            d.ret <- rbind(d.ret,
                           d.ret.j)

            }





            if (h == vecHab[1]) aggTableAll.tot <- aggTable else aggTableAll.tot <- rbind(aggTableAll.tot,aggTable)
            if (h == vecHab[1]) d.ret.tot <- d.ret else d.ret.tot <- rbind(d.ret.tot,d.ret)
        }#END if(community_level)
        if(species_level) {


            aggTable.sp <- read.csv2("data_France/returnRate_sp_France.csv")
            aggTable.sp$gr <- paste("FRANCE,",aggTable.sp$AGE_first)
            aggTable.sp$SITE <- "FRANCE"
            aggTable.sp <- subset(aggTable.sp,HABITAT == h)


            dsp.ret.a <- aggregate(RETURN ~ NEW.ID_PROG + YEAR + SP + MIGRATION, data = d.a, FUN = returnRateAssessment)
            dsp.compt.a <- aggregate(BAGUE ~ NEW.ID_PROG + YEAR + SP + MIGRATION , data = d.a, FUN = length)
            dsp.ret.a <- merge(dsp.ret.a,dsp.compt.a,by=c("NEW.ID_PROG","YEAR","SP","MIGRATION"))

            dsp.ret.j <- aggregate(RETURN ~ NEW.ID_PROG + YEAR + SP + MIGRATION , data = d.j, FUN = returnRateAssessment)
            dsp.compt.j <- aggregate(BAGUE ~ NEW.ID_PROG + YEAR + SP + MIGRATION, data = d.j, FUN = length)
            dsp.ret.j <- merge(dsp.ret.j,dsp.compt.j,by=c("NEW.ID_PROG","YEAR","SP","MIGRATION"))


            dsp.ret.a$AGE_first <- "AD"
            dsp.ret.j$AGE_first <- "JUV"


            dsp.ret <- rbind(dsp.ret.a,
                             dsp.ret.j)

            dsp.medcompt <- aggregate(BAGUE ~ NEW.ID_PROG + SP + YEAR , data = dsp.ret, FUN = sum)
            dsp.medcompt <- aggregate(BAGUE ~ NEW.ID_PROG + SP , data = dsp.medcompt, FUN = median)

            dsp.medSeuil <- subset(dsp.medcompt,BAGUE>10)

            dsp.ret <- dsp.ret[paste(dsp.ret$NEW.ID_PROG,dsp.ret$SP) %in% paste(dsp.medSeuil$NEW.ID_PROG,dsp.medSeuil$SP), ]

            if(h == vecHab[1]) aggTableSP.tot <- aggTable.sp else aggTableSP.tot <- rbind(aggTableSP.tot,aggTable.sp)

            if(h == vecHab[1]) dsp_ret.tot <- dsp.ret else   dsp_ret.tot <- rbind(dsp_ret.tot,dsp.ret)

        }

    }


    for(s in site) {

        if(community_level) {
            d.ret.s <- subset(d.ret.tot,NEW.ID_PROG==s)
            if(length(unique(d.ret.s$YEAR))>1) {
                h <- d$HABITAT[1]

                d.ret.s <- subset(d.ret.s,YEAR<max(d.ret.s$YEAR))
                minYear <- max(min(d.ret.s$YEAR)-1,min(d$YEAR))
                maxYear <- min(max(d.ret.s$YEAR)+1,max(d$YEAR))

                d.ret.s <- subset(d.ret.s,BAGUE>10)

                if(nrow(d.ret.s)>0) {
                    if(add_title) title_txt <- paste("Station ",s,": toutes espèces confondues",sep="") else title_txt <- ""

                    aggTableAll.tot$YEAR <- aggTableAll.tot$YEAR + 1
                    d.ret.s$YEAR <- d.ret.s$YEAR + 1
                    gg <- ggplot(subset(aggTableAll.tot,HABITAT==h & MIGRATION !=""),aes(x=YEAR,y=med,colour=MIGRATION,fill=MIGRATION))+ facet_grid(AGE_first~MIGRATION,scales="free_y")
                    gg <- gg + geom_ribbon(aes(ymin=CIinf,ymax=CIsup),alpha=.4,colour = NA)
                    gg <- gg + geom_line(size=1.5,alpha=.8)
                    gg <- gg + geom_line(aes(y=CIquart_inf),alpha=.5,size=.8)+geom_line(aes(y=CIquart_sup),alpha=.5,size=.8)
                    gg <- gg + scale_colour_manual(values =c("#07307b","#0c5ef6"))
                    gg <- gg + scale_fill_manual(values =c("#07307b","#0c5ef6"),guide=FALSE)
                    gg <- gg + coord_cartesian(xlim=c(minYear,maxYear)) + scale_x_continuous(breaks=pretty_breaks())
                    gg <- gg + geom_line(data=subset(d.ret.s, MIGRATION !=""), aes(y=RETURN),colour="#d71818",size=1.8,alpha=.5)
                    gg <- gg + geom_point(data=subset(d.ret.s, MIGRATION !=""), aes(y=RETURN),colour="#d71818",size=2)
                    gg <- gg + labs(title=title_txt,
                                    x="Année",y="Taux de retour")
                    gg <- gg + theme(legend.position="none")

                    if(print.fig) print(gg)

                    if(save.fig) {
                        ggfile <- paste("output/",s,"/ReturnRate_Site_",s,".png",sep="")
                        catlog(c("Check",ggfile,":"),fileLog)
                        ggsave(ggfile,gg,width=7, height=6)
                        catlog(c("\n"),fileLog)
                    }


                }#END if(nrow(d.ret.s)>0)

            }# END if(length(unique(d.ret.s$YEAR))>1)
        }# END if(community_level)

        if(species_level) {

            if(!is.null(nom_sp)) t_nom_sp <- data.frame(species=species,nom_sp=nom_sp)
            dsp.ret.s <- subset(dsp_ret.tot,NEW.ID_PROG==s)
            if(length(unique(dsp.ret.s$YEAR))>1) {
                if(nrow(dsp.ret.s)>0) {

                    dsp.ret.s <- subset(dsp.ret.s,YEAR<max(dsp.ret.s$YEAR))

                    minYear <- max(min(dsp.ret.s$YEAR)-1,min(d$YEAR))
                    maxYear <- min(max(dsp.ret.s$YEAR)+1,max(d$YEAR))


                    compt <- aggregate(BAGUE ~ SP + YEAR, data=dsp.ret.s, sum)
                    compt <- aggregate(BAGUE ~ SP, data=compt, median)



                    if(is.null(species))   spEligible <- compt$SP[compt$BAGUE>10] else spEligible <- intersect(species,unique(compt$SP))
                    if(length(spEligible)>0) {
                        dsp.ret.s <- subset(dsp.ret.s,SP %in% spEligible)

                        for(sp in spEligible) {

                            if(add_title) {
                                if(!is.null(nom_sp)) sp_fig <- as.character(t_nom_sp$nom_sp[t_nom_sp$species == sp]) else sp_fig <- sp
                                title_txt <- paste(sp_fig,": Station ",ss,sep="")
                            } else {
                                title_txt <- ""
                            }

                            aggTableTot.sp.sp <- subset(aggTableSP.tot,HABITAT==h & SP == sp)
                            dsp.ret.sp <- subset(dsp.ret.s,SP == sp)

                            if(facet){
                                if(nrow(aggTableTot.sp.sp)>0) {
                                    aggTableTot.sp.sp$YEAR <- aggTableTot.sp.sp$YEAR + 1
                                    if(!is.null(nom_sp)) aggTableTot.sp.sp$facet <- as.character(t_nom_sp$nom_sp[t_nom_sp$species == sp]) else aggTableTot.sp.sp$facet <- sp
                                } else {
                                    if(!is.null(nom_sp)) dsp.ret.sp$facet <- as.character(t_nom_sp$nom_sp[t_nom_sp$species == sp]) else dsp.ret.sp$facet <- sp
                                }

                            } #END if(facet)

                            dsp.ret.sp$YEAR <-  dsp.ret.sp$YEAR + 1
                            if(nrow(aggTableTot.sp.sp)>0) {
                                gg <- ggplot( aggTableTot.sp.sp ,aes(x=YEAR,y=med,colour=MIGRATION,fill=MIGRATION))
                                gg <- gg + geom_ribbon(aes(ymin=CIinf,ymax=CIsup),alpha=.4,colour = NA)
                                gg <- gg + geom_line(size=1.5,alpha=.8)
                                gg <- gg + geom_line(aes(y=CIquart_inf),alpha=.5,size=.8)+geom_line(aes(y=CIquart_sup),alpha=.5,size=.8)
                                gg <- gg + scale_colour_manual(values =c("#07307b","#0c5ef6"))
                                gg <- gg + scale_fill_manual(values =c("#07307b","#0c5ef6"),guide=FALSE)
                            } else {
                                gg <- ggplot( dsp.ret.sp ,aes(x=YEAR,y=RETURN))

                            }
                            if(facet) gg <- gg + facet_grid(AGE_first~facet,scales="free_y") else gg <- gg + facet_grid(AGE_first~.,scales="free_y")

                            gg <- gg + coord_cartesian(xlim=c(minYear,maxYear)) + scale_x_continuous(breaks=pretty_breaks())#trunc(seq(minYear,maxYear,length.out= min(length(minYear:maxYear),8))))
                            gg <- gg + geom_line(data=dsp.ret.sp, aes(y=RETURN),colour="#d71818",size=1.8,alpha=.5)
                            gg <- gg + geom_point(data=dsp.ret.sp, aes(y=RETURN),colour="#d71818",size=2)
                            gg <- gg + theme(legend.position="none")

                            gg <- gg + labs(title=title_txt,
                                            x="Année",y="Taux de retour de t-1 à t")

                            if(print.fig) print(gg)

                            if(save.fig) {
                                ggfile <- paste("output/",s,"/ReturnRate_sp_Site_",s,".png",sep="")
                                catlog(c("Check",ggfile,":"),fileLog)
                                ggsave(ggfile,gg,width=7, height=7)
                                catlog(c("\n"),fileLog)
                            }


                        }#END  for(sp in spEligible)
                    }#END  if(length(spEligible)>0)
                }#END  if(nrow(dsp.ret.s)>0)
            }#END if(length(unique(dsp.ret.s$YEAR))>1)
        }#END  if(species_level)
    }#END for(s in site)
}###END returnRate.site





plot_local_sp <- function(dgg,species=NULL,nom_sp=NULL,print.fig=TRUE,save.fig=FALSE,add_title=FALSE,
                          facet_sp=TRUE,facet_group="AGE_first",
                          y_lab="Condition corporelle: Masse/(Ecart à la taille moyenne + 1)",
                          x_lab="Année",title_txt="",vecCol=c("#07307b","#0c5ef6","#c10909","#ea5d18"),
                          minYear = NULL, maxYear = NULL) {


    if(!is.null(nom_sp)) t_nom_sp <- data.frame(species=species,nom_sp=nom_sp)

    if(is.null(species)) species <- unique(dgg$SP)

    if(!is.null(facet_group)) dgg$facet_group <- dgg[,facet_group]

    for(sp in species) {

        dgg.sp <- subset(dgg,SP==sp)

        if(facet_sp) {
            if(!is.null(nom_sp)) {
                dgg.sp$facet <- as.character(t_nom_sp$nom_sp[t_nom_sp$species == sp])
            }else {
                dgg.sp$facet <- sp
            }
        }


        if(is.null(minYear)) minYear <- min(dgg.sp[grep("Site",dgg.sp$gr),"YEAR"])-1
        minYear <- max(minYear,min(dgg.sp$YEAR))

        if(is.null(maxYear)) maxYear <- max(dgg.sp[grep("Site",dgg.sp$gr),"YEAR"])+1
        maxYear <- min(maxYear,max(dgg.sp$YEAR))


        gg <- ggplot(data=dgg.sp,aes(x=YEAR,y=med,colour=gr,fill=gr))

        if(facet_sp) {
            if(!is.null(facet_group)) {
                gg <- gg + facet_grid(facet_group ~ facet,scales="free_y")
            } else {
                gg <- gg + facet_grid(.~ facet,scales="free_y")
            }
        } else {
            if(!is.null(facet_group))
                gg <- gg + facet_grid(facet_group ~. ,scales="free_y")
        }

        gg <- gg + geom_ribbon(aes(ymin=CIinf,ymax=CIsup),alpha=.4,colour = NA)
        gg <- gg + geom_line(size=1.5,alpha=.8)
        gg <- gg + geom_line(aes(y=CIquart_inf),alpha=.5,size=.8)+geom_line(aes(y=CIquart_sup),alpha=.5,size=.8)
        gg <- gg + geom_point(data=dgg.sp[grep("Site",dgg.sp$gr),],size=2.5)
        gg <- gg + geom_point(data=dgg.sp[grep("Site",dgg.sp$gr),],colour="white",size=1,alpha=0.5)
        gg <- gg + scale_colour_manual(values =vecCol,name="")
        gg <- gg + scale_fill_manual(values =vecCol,guide=FALSE)
        gg <- gg + labs(title=title_txt,y=y_lab,x=x_lab)
        gg <- gg + coord_cartesian(xlim=c(minYear,maxYear)) + scale_x_continuous(breaks=pretty_breaks())
        gg <- gg + theme(legend.position="none")

        if(print.fig) print(gg)


        if(save.fig) {
            ggfile <- paste("output/",s,"/bodyCondition_sp_site",s,"_",sp,".png",sep="")
            catlog(c("Check",ggfile,":"),fileLog)
            ggsave(ggfile,gg)
            catlog(c("\n"),fileLog)
        }

    }# END for(sp in species)
}### END plot_local_sp





pheno.all <- function(d,habitat=NULL,fileLog="log.txt") # !!!!!!  NON STABILISE !!!!!
{
    require(ggplot2)
    ##     d <- read.csv2("output/data.csv")
    ##     ddd <- d
    ##     d <- ddd


    if(!is.null(habitat)) d <- subset(d,HABITAT == habitat)

    dj <- subset(d,AGE_first=="JUV")
    dd <- subset(d,AGE_first != "VOL")
    vecHab <- unique(d$HABITAT)
    aggMin <- aggregate(JULIANDAY~NEW.ID_PROG + YEAR + MIGRATION + HABITAT,data= dj, min)

    aggTable <- aggregate(JULIANDAY ~ YEAR + MIGRATION + HABITAT, aggMin,quantile,c(0.025,0.25,0.5,0.75,0.975))
    aggTable <- data.frame(aggTable[,1:3],aggTable[4][[1]][,1:5])
    colnames(aggTable)[4:8] <- c("CIinf","CIquart_inf","med","CIquart_sup","CIsup")



    gg <- ggplot(subset(aggTable),aes(x=YEAR,y=med,colour=MIGRATION,fill=MIGRATION))+ facet_grid(HABITAT~MIGRATION) +scale_x_continuous(breaks=pretty_breaks())
    gg <- gg + geom_ribbon(aes(ymin=CIinf,ymax=CIsup),alpha=.4,colour = NA)
    gg <- gg + geom_line(size=1.5,alpha=.8)
    gg <- gg + geom_line(aes(y=CIquart_inf),alpha=.5,size=.8)+geom_line(aes(y=CIquart_sup),alpha=.5,size=.8)
    gg <- gg + scale_colour_manual(values =c("#07307b","#0c5ef6"))
    gg <- gg + scale_fill_manual(values =c("#07307b","#0c5ef6"),guide=FALSE)

    gg <- gg + labs(list(title=paste("Variation de la date de la premiere capture de JUV\ntoutes espèces confondue",sep=""),
                         x="Année",y="Jour Julien"))



    ggfile <- paste("output/France/PhenologieBrut_all.png",sep="")
    catlog(c("Check",ggfile,":"),fileLog)
    ggsave(ggfile,gg,width=6, height=7)
    catlog(c("\n"),fileLog)


    dmin <- subset(d,paste(d$NEW.ID_PROG,d$YEAR,d$JULIANDAY,d$MIGRATION ) %in% paste(aggMin$NEW.ID_PROG,aggMin$YEAR,aggMin$JULIANDAY,aggMin$MIGRATION ))

    proportionJuv <- function(X) length(which(X=="JUV"))/length(X)


    aggProp <- aggregate(AGE_first~NEW.ID_PROG + YEAR +JULIANDAY + MIGRATION + HABITAT + SESSION, dd,FUN = proportionJuv)




    aggPropSub <- subset(aggProp,AGE_first > 0 & AGE_first < 1)

    aggPropSub$gr <- paste(aggPropSub$NEW.ID_PROG,aggPropSub$YEAR,aggPropSub$MIGRATION, aggPropSub$HABITAT)
    uProgYear <- unique(subset(aggPropSub,select=c("gr","NEW.ID_PROG","YEAR","MIGRATION","HABITAT")))

    penteProp <- function(X) {

        md <- try(lm(AGE_first ~ JULIANDAY,d=X),silent=TRUE)
        if(class(md)[1] != "try-error") {
            smd <- summary(md)
            pente <- smd$coef[2,1]
            return(pente)
        }

    }


    for(g in unique(aggPropSub$gr)) {
        xx <- subset(aggPropSub,gr==g)
        if(nrow(xx)>1) {
            pp <- penteProp(xx)
            if(g== unique(aggPropSub$gr)[1]) tPente <- data.frame(gr=g,pente=pp) else tPente <- rbind(tPente, data.frame(gr=g,pente=pp))
        }

    }


    tPente <- subset(tPente, !(is.na(pente)))
    tPente <-  subset(tPente, pente>0)
    tPente <- merge(tPente,uProgYear,by="gr")
    pente <- mean(tPente$pente)

    gg <- ggplot(aggPropSub,aes(x=JULIANDAY,y=AGE_first,group=paste(NEW.ID_PROG,YEAR))) +geom_line(aes(groupe=NEW.ID_PROG),alpha=0.2)+ geom_point(aes(colour=SESSION),alpha=.5) + facet_grid(MIGRATION~HABITAT)
    gg <- gg + labs(list(title=paste("Proportion de JUV au court de la saison",sep=""),
                         y="Proportion de JUV",x="Jours Julien"))

    ggfile <- paste("output/France/ProportionDeJUV_all.png",sep="")
    catlog(c("Check",ggfile,":"),fileLog)
    ggsave(ggfile,gg,width=6, height=7)
    catlog(c("\n"),fileLog)




    gg <- ggplot(tPente,aes(x=pente))+geom_histogram() +  facet_grid(MIGRATION~HABITAT) + geom_vline(xintercept = pente,colour="red",size=2,alpha=.5)
    gg <- gg + labs(list(title=paste("Pente de correction de la date de première capture de jeune",sep=""),
                         y="",x="Pente de la proportion de jeunes au court de la saison de baguage (par site et annee)"))
    ggfile <- paste("output/France/PenteDeCorrection_all.png",sep="")
    catlog(c("Check",ggfile,":"),fileLog)
    ggsave(ggfile,gg,width=6, height=7)
    catlog(c("\n"),fileLog)


    aggPropMin <- aggregate(AGE_first~NEW.ID_PROG + YEAR +JULIANDAY + MIGRATION + HABITAT + SESSION, dmin,FUN = proportionJuv)


    aggPropMin$JUV_JULIANDAY <- (aggPropMin$JULIANDAY*pente - aggPropMin$AGE_first)/pente


    aggTable2 <- aggregate(JUV_JULIANDAY ~ YEAR + MIGRATION + HABITAT, aggPropMin,quantile,c(0.025,0.25,0.5,0.75,0.975))
    aggTable2 <- data.frame(aggTable2[,1:3],aggTable2[4][[1]][,1:5])
    colnames(aggTable2)[4:8] <- c("CIinf","CIquart_inf","med","CIquart_sup","CIsup")




    gg <- ggplot(aggTable2,aes(x=YEAR,y=med,colour=MIGRATION,fill=MIGRATION))+ facet_grid(HABITAT~MIGRATION) +scale_x_continuous(breaks=pretty_breaks())
    gg <- gg + geom_ribbon(aes(ymin=CIinf,ymax=CIsup),alpha=.4,colour = NA)
    gg <- gg + geom_line(size=1.5,alpha=.8)
    gg <- gg + geom_line(aes(y=CIquart_inf),alpha=.5,size=.8)+geom_line(aes(y=CIquart_sup),alpha=.5,size=.8)
    gg <- gg + scale_colour_manual(values =c("#07307b","#0c5ef6"))
    gg <- gg + scale_fill_manual(values =c("#07307b","#0c5ef6"),guide=FALSE)

    gg <- gg + labs(list(title=paste("Variation de la date corrigé de la premiere capture de JUV\ntoutes espèces confondue",sep=""),
                         x="Année",y="Jour Julien"))

    gg

    ggfile <- paste("output/France/PhenologieCorrige_all.png",sep="")
    catlog(c("Check",ggfile,":"),fileLog)
    ggsave(ggfile,gg,width=6, height=7)
    catlog(c("\n"),fileLog)

    gg <- ggplot(aggPropMin, aes(x=JULIANDAY, y=JUV_JULIANDAY, colour=AGE_first)) + geom_point() + scale_colour_gradient(low="red",high = "yellow")
    gg






    aggMin <- aggregate(JULIANDAY~NEW.ID_PROG + BAGUE + YEAR + MIGRATION + HABITAT,data= dj, min)

    aggTable <- aggregate(JULIANDAY ~ YEAR + MIGRATION + HABITAT, aggMin,quantile,c(0.025,0.25,0.5,0.75,0.975))
    aggTable <- data.frame(aggTable[,1:3],aggTable[4][[1]][,1:5])
    colnames(aggTable)[4:8] <- c("CIinf","CIquart_inf","med","CIquart_sup","CIsup")



    gg <- ggplot(subset(aggTable),aes(x=YEAR,y=med,colour=MIGRATION,fill=MIGRATION))+ facet_grid(HABITAT~MIGRATION) +scale_x_continuous(breaks=pretty_breaks())
    gg <- gg + geom_ribbon(aes(ymin=CIinf,ymax=CIsup),alpha=.4,colour = NA)
    gg <- gg + geom_line(size=1.5,alpha=.8)
    gg <- gg + geom_line(aes(y=CIquart_inf),alpha=.5,size=.8)+geom_line(aes(y=CIquart_sup),alpha=.5,size=.8)
    gg <- gg + scale_colour_manual(values =c("#07307b","#0c5ef6"))
    gg <- gg + scale_fill_manual(values =c("#07307b","#0c5ef6"),guide=FALSE)

    gg <- gg + labs(list(title=paste("Variation de la date de la premiere capture de JUV\ntoutes espèces confondue",sep=""),
                         x="Année",y="Jour Julien"))

    gg

    ggfile <- paste("output/France/PhenologieBrut2_all.png",sep="")
    catlog(c("Check",ggfile,":"),fileLog)
    ggsave(ggfile,gg,width=6, height=7)
    catlog(c("\n"),fileLog)




}





expPDF <- function(d,site=NULL,system="linux",fileLog="log.txt") {
    if(is.null(site)) {
        site <- sort(unique(d$NEW.ID_PROG))
    }

    year <- max(d$YEAR)

    for(s in site) {
        catlog(c("\n -- Station: ",s,"\n---------------\n"),fileLog)
        makePDF(s,year,system)

    }
}
