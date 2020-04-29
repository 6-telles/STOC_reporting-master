

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

    if(is.null(aggAllSession)) {
        fileSave <- "data_France/summarySession.csv"
        aggAllSession <- read.csv2(fileSave,stringsAsFactors=FALSE)
    }

    if(is.null(aggSession)) {
        fileSave <- "data_France/historicSession.csv"
        aggSession <- read.csv2(fileSave,stringsAsFactors=FALSE)
    }

    if(is.null(aggSessionRef)) {
        fileSave <- "data_France/SessionReference.csv"
        aggSessionRef <-  read.csv2(fileSave,stringsAsFactors=FALSE)
    }


    aggSession$SESSION <- as.character(aggSession$SESSION)
    aggSessionRef$SESSION <- as.character(aggSessionRef$SESSION)
    aggAllSession$SESSION <- as.character(aggAllSession$SESSION)




  if(is.null(site)) site <- sort(as.character(unique(d$NEW.ID_PROG)))

    for(ss in site)
    {
    #
        aggSession.s <- subset(aggSession,NEW.ID_PROG == ss & !is.na(JULIANDAY))
        minYear <- max(min(aggSession.s$YEAR)-2,min(aggSession$YEAR))
        maxYear <- min(max(aggSession.s$YEAR)+2,max(aggSession$YEAR))

        aggSessionRef.s <- subset(aggSessionRef,NEW.ID_PROG == ss )


        vcolour <- c("0"="#737373","1" = "#1c30f3","2" = "#951cf3","3" = "#f90d28","0" = "#1e1112")
        vdate <- c("05-01","05-15","06-01","06-15","07-01","07-15","08-01")
        vdate <- as.numeric(format(as.Date(paste(2000,vdate,sep="-")),"%j"))-1
        vdate <- sort(c(vdate,aggSessionRef.s$JULIANDAY))

        if(add_title) title_txt <- paste("Date des sessions selectionnées pour la station",ss) else title_txt <- ""

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

        if(save.fig) {
        ggfile <- paste("output/",ss,"/session_",ss,".png",sep="")
        catlog(c("Check",ggfile,":"),fileLog)
        ggsave(ggfile,gg)
        catlog(c("\n"),fileLog)
        }

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
carteStation <- function(site=NULL,d,add_local=TRUE,fileLog=NULL,print.fig=TRUE,save.fig=FALSE,add_title=TRUE) {

    require(rgdal)
    require(ggmap)
    require(maptools)

    library(OpenStreetMap)
    library(sf)
    library(gridExtra)
    require(maps)

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
    #  browser ()
   colnames(coordAll)[1] <- "NEW.ID_PROG"
    coordAll2 <- merge(coordAll,typeAll,by="NEW.ID_PROG",all=TRUE)
    coordAll2 <- merge(coordAll2,anneeAll,by="NEW.ID_PROG",all=TRUE)


    for(ss in site)
    {
        h <- as.character(coordAll2$HABITAT[coordAll2$NEW.ID_PROG==ss])
        fy <- max(coordAll2$FIRST.YEAR[coordAll2$NEW.ID_PROG==ss] - 3,min(d$YEAR))
        ly <- min(coordAll2$LAST.YEAR[coordAll2$NEW.ID_PROG==ss] + 3,max(d$YEAR))


        coordAllh <- subset(coordAll2,HABITAT == h & FIRST.YEAR <= ly & LAST.YEAR >= fy)
        nbs <- length(unique(coordAllh$NEW.ID_PROG))-1
        dcoord.s <- subset(coordAll2,NEW.ID_PROG == ss)

        coordAllh$DUREE <- apply(subset(coordAllh,select=c("FIRST.YEAR","LAST.YEAR")),1,FUN = function(X) (min(X[2],ly) - max(X[1],fy) + 1))
                                        #browser()

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





speciesRelativeAbund.site <- function(d,site=NULL,fileLog="log.txt",print.fig=TRUE,save.fig=FALSE,add_title=TRUE) {

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

    ggTableH <- subset(ggTableH,PROP_STATION_CAPTURE>.2)

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


        ggTableS <- aggregate(ggNbCapt$ABUND,by=list(ggNbCapt$SP),quantile,.75)
        colnames(ggTableS) <- c("SP","ABUNDsite75")
        ggTableS$SP <- as.character(ggTableS$SP)

        ggTable <- merge(ggTableS,ggTableHab,by="SP")

        if(add_title) title_txt <- paste("Site ",ss) else title_txt <- ""

        gg <- ggplot(ggTable,aes(x=reorder(SP, (1-ABUNDsite75)),y=ABUND50))
        gg <- gg + geom_linerange(aes(ymin=ABUND025,ymax=ABUND975),colour="#08306b",alpha=.5)
        gg <- gg + geom_crossbar(aes(ymin = ABUND25, ymax = ABUND75), width = 0.5,alpha=.5,colour="#08306b",fill="#08306b")
        gg <- gg + theme(axis.text.x  = element_text(angle=90,vjust=.5))
        gg <- gg + labs(x="Espèces caputrées dans au moins 1/5 des stations",y="Nombre d'individus capturés par an",title=title_txt)
        gg <- gg + geom_jitter(data=ggNbCapt,aes(x=SP,y=ABUND,colour=YEAR),size=2,alpha=.9,width = .2)
        gg <- gg +scale_y_log10(breaks=c(0,1,2,5,10,20,50,100,200,400))
        gg <- gg + scale_colour_continuous(low="#ffa200",high="#960000")#"#07307b","#0c5ef6","#c10909","#ea5d18"

        if(print.fig) print(gg)

        if(save.fig) {
            ggfile <- paste("output/",ss,"/nbCapture_site_",ss,".png",sep="")
            catlog(c("Check",ggfile,":"),fileLog)
            ppi <- 200
            ggsave(ggfile,gg,width=11, height=8, unit="in",limitsize = FALSE)
            catlog(c("\n"),fileLog)
        }
    } # END  for(ss in site)

} ### END speciesRelativeAbund.site






##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param d
##' @param site
##' @param fileLog
##' @return
##' @author Romain Lorrilliere
speciesRelativeAbund.site_grgr<- function(d,site=NULL,fileLog="log.txt",print.fig=TRUE,save.fig=FALSE) {
    require(ggplot2)
    dsp <- read.csv2("library/sp.csv",stringsAsFactors=FALSE)
    tableCouleur <- subset(dsp,select=c("SP","COLOUR"))

    if(is.null(site)) site <- unique(d$NEW.ID_PROG)

    du.bague <- unique(subset(d,select=c("SP","BAGUE","HABITAT")))
    t.contingency.sp <- t(table(du.bague$SP,du.bague$HABITAT))
    t.contRelat <- t(t.contingency.sp / rowSums(t.contingency.sp))

    nbH <- ncol(t.contRelat)
    vech <- colnames(t.contRelat)

    for(h in 1:nbH) {
        th <- data.frame(SP = rownames(t.contRelat),HABITAT = colnames(t.contRelat)[h], RELATIVE_ABUNDANCE = t.contRelat[,h])
        v <- sort(th$RELATIVE_ABUNDANCE,decreasing = TRUE)
        vcum <- cumsum(v)
        vseuil <- v[min(min(which(trunc(vcum*100)>=66))+1)]
        th[th$RELATIVE_ABUNDANCE<vseuil,"SP"] <- NA
        if (h==1) ggTable <- th else ggTable <-  rbind(ggTable,th)
    }
    ggTable <- data.frame(NEW.ID_PROG = "Tout",ggTable)
    ggTable <- ggTable[order(ggTable$RELATIVE_ABUNDANCE,decreasing = TRUE),]

                                        # print(site)
    for(ss in site) {
                                        #    catlog(c("site:",s,"\n"),fileLog)
        dhs <- subset(d,NEW.ID_PROG == s)
        habitat <- dhs$HABITAT[1]
        dh <- subset(d,HABITAT == habitat)
        ggTable.spALL <-  subset(ggTable,HABITAT == habitat)
        ggTable.spALL$NEW.ID_PROG <- paste("France",habitat)

        du.bague <- unique(subset(dhs,select=c("SP","BAGUE")))
        t.contingency.sp <- sort(table(du.bague$SP),decreasing = TRUE)
        ggTable.sp <- data.frame(SP = rownames(t.contingency.sp),t.contingency.sp)
        colnames(ggTable.sp) <- c("SP","ABUNDANCE")


        ggTable.sp$RELATIVE_ABUNDANCE <- ggTable.sp$ABUNDANCE / sum(ggTable.sp$ABUNDANCE)
        ggTable.sp$RELATIVE_ABUNDANCE_CUM <- cumsum(ggTable.sp$RELATIVE_ABUNDANCE)
        if(nrow(ggTable.sp)<3) ggTable.sp$SP[(min(which(trunc(ggTable.sp$RELATIVE_ABUNDANCE_CUM*100)>=66))+1):nrow(ggTable.sp)] <- NA
        ggTable.sp$NEW.ID_PROG <- paste("Site:",ss)

        ggTable.sp$HABITAT <- habitat
        ggTable.sp <- subset(ggTable.sp,select=c("NEW.ID_PROG","SP","HABITAT","RELATIVE_ABUNDANCE"))

        ggTable.sp <- rbind(ggTable.spALL,ggTable.sp)

        tc <- subset(tableCouleur,SP %in% unique(ggTable.sp$SP))
                                        #browser()
        gg <- ggplot(data=ggTable.sp,aes(x=NEW.ID_PROG,y = RELATIVE_ABUNDANCE,
                                         fill = SP)) +
            geom_bar(width = .95,stat = "identity") +
            coord_polar(theta="y",start=pi/2) +
            scale_fill_manual(values = tc$COLOUR,breaks=tc$SP,name="Code espèces") +
            labs(x="",y="") +
            ggtitle(paste("Abondance relative\n","Site:",ss))

        ggfile <- paste("output/abondanceRelative_",ss,".png",sep="")
        catlog(c("Check",ggfile,":"),fileLog)
        ggsave(ggfile,gg)
        catlog(c("\n"),fileLog)


    }

}




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

abundanceSpeciesYear.site <- function(d,site=NULL,limitTime=TRUE,fileLog="log.txt",print.fig=TRUE,save.fig=FALSE,add_title=FALSE)
{
                                        # site="9"

    require(ggplot2)

    tabSpeQuant <- read.csv2("output/quantileEspece_France_.csv",stringsAsFactors=FALSE)

    habitat <- unique(d$HABITAT)

    aggTableNat <- data.frame(SP=NULL,YEAR=NULL,HABITAT=NULL,CIinf=NULL,CIquart_inf=NULL,med=NULL,CIquart_sup=NULL,CIsup=NULL)

    ## calcul reference nationnale par habitat et par espece
    for(h in habitat)
    {

        tabSpeHab <- subset(tabSpeQuant,HABITAT == h)

        listSP <- tabSpeHab$SP[tabSpeHab$PROP_STATION_CAPTURE>.2]

        if(h == habitat[1]) listSP.h <- data.frame(SP=listSP,HABITAT=h) else listSP.h <- rbind(listSP.h,data.frame(SP=listSP,HABITAT=h))

        ## calcul pour le espece capture dans au moins 20 pour cent des stations
        for(sp in listSP) {



            dhAd <- subset(d,HABITAT == h & AGE_first == "AD" & ESPECE == sp)
            du.bague <- unique(subset(dhAd,select=c("BAGUE","ESPECE","NEW.ID_PROG","YEAR")))
            du.hab <- unique(subset(dhAd,select=c("NEW.ID_PROG","HABITAT")))

            t.abund <- tapply(du.bague$BAGUE,list(du.bague$NEW.ID_PROG,du.bague$YEAR),length)
            t.abund <- ifelse(is.na(t.abund),0,t.abund)
            t.abund <- data.frame(t.abund)
            t.abund <- reshape(t.abund, direction = "long",
                               idvar = "NEW.ID_PROG",
                               ids=rownames(t.abund),
                               varying = 1:ncol(t.abund),sep="")
            colnames(t.abund) <- c("YEAR","ABUND","NEW.ID_PROG")




            t.nbnf.session <- unique(subset(dhAd,select=c("NEW.ID_PROG","YEAR","SESSION","FS.OUTPUT")))

            t.nbnf <- aggregate(FS.OUTPUT~NEW.ID_PROG+YEAR, data=t.nbnf.session, sum, na.rm=FALSE)

            t.fs.output <- tapply(t.nbnf.session$FS.OUTPUT,list(t.nbnf.session$NEW.ID_PROG,t.nbnf.session$YEAR), sum)
                                        #t.fs.saisi <- tapply(t.nbnf.session$FS,list(t.nbnf.session$NEW.ID_PROG,t.nbnf.session$YEAR), sum)

            t.fs <- data.frame(NEW.ID_PROG = rep(rownames(t.fs.output),ncol(t.fs.output)),
                               YEAR = rep(colnames(t.fs.output),each=nrow(t.fs.output)),
                               FS.OUPUT = as.vector(as.matrix(t.fs.output)))
            t.fs <- subset(t.fs,!is.na(FS.OUPUT))


            t.abund <- merge(t.abund,t.fs,by=c("NEW.ID_PROG","YEAR"))
                                        #    t.abund.all <- data.frame(t.abund,HABITAT = "tout")
            t.abund <- merge(t.abund,du.hab,by="NEW.ID_PROG")
                                        #    if(is.null(habitat))      t.abund <- rbind(t.abund,t.abund.all)

            t.abund$ABUND.ADJUST <- t.abund$ABUND / t.abund$FS.OUPUT*(120*3)

            ggTable.abund <- t.abund

                                        #   ggTable.abund <- na.omit(ggTable.abund)
           ## if(sp=="MERAPI") browser()
            aggTable <- aggregate(ABUND.ADJUST ~ (YEAR + HABITAT), data= ggTable.abund, quantile,c(0.025,0.25,0.5,0.75,0.975))
            aggTable <- data.frame(aggTable[,1:2],aggTable[3][[1]][,1:5])
            colnames(aggTable)[3:7] <- c("CIinf","CIquart_inf","med","CIquart_sup","CIsup")

            aggTableNat <- rbind(aggTableNat,data.frame(SP=sp,aggTable))
        }
    }
                                        #print(site)
    if(is.null(site)) site <- sort(as.character(unique(d$NEW.ID_PROG)))

    ## boucle pour figure par site
    for(ss in site) {

        ## selection des adultes
        dhAd <- subset(d,NEW.ID_PROG == ss & AGE_first == "AD")
        hab <- as.character(dhAd$HABITAT[1])
        lesAnnees <- as.numeric(as.character(unique(dhAd$YEAR)))
        listSP.s <- sort(as.character(unique(dhAd$ESPECE)[unique(dhAd$ESPECE)%in%subset(listSP.h,HABITAT==hab)$SP]))

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

            t.nbnf.session <- unique(subset(dhAd,select=c("NEW.ID_PROG","YEAR","SESSION","FS.OUTPUT")))
            t.fs <- data.frame(aggregate(FS.OUTPUT~YEAR, data=t.nbnf.session, sum, na.rm=FALSE))
                                        #        browser()
            t.fs <- subset(t.fs,!is.na(FS.OUTPUT))


            t.site <- merge(t.site,t.fs,by=c("YEAR"))

            t.site$ABUND.ADJUST <- t.site$ABUND / t.site$FS.OUTPUT*(120*3)



            minYear <- max(min(t.site$YEAR)-2,min(aggTableNat$YEAR))
            maxYear <- min(max(t.site$YEAR)+2,max(aggTableNat$YEAR))

            aggTable.s <- subset(aggTableNat,HABITAT==hab & SP == sp & YEAR>=minYear & YEAR<=maxYear)
                                        #     aggTable.s <- subset(aggTable.s,)

                                        #ggSite <- data.frame(YEAR = tSite$YEAR,HABITAT=habitat,FS_CAT = paste("Station:",s,"| FS déduit"),CIinf=NA ,med=tSite$ABUND.ADJUST.DEDUIT,CIsup=NA)
            ggSite <- data.frame(SP=sp,YEAR = t.site$YEAR,HABITAT = paste("Station:",ss),CIinf=NA,CIquart_inf=NA,med=t.site$ABUND.ADJUST,CIquart_sup=NA,CIsup=NA)

            aggTable.s <- rbind(aggTable.s,ggSite)

            if(add_title) title_txt <- paste(sp,": Station ",ss,sep="") else title_txt <- ""

            gg <- ggplot(aggTable.s,aes(x=YEAR,y=med,colour=HABITAT,fill=HABITAT))

            gg <- gg + geom_ribbon(aes(ymin=CIinf,ymax=CIsup),alpha=.2,colour = NA)
            gg <- gg + geom_line(aes(y=CIquart_inf),size=0.6,alpha=.6)+ geom_line(aes(y=CIquart_sup),size=0.6,alpha=.6)
            gg <- gg + geom_line(size=1.5,alpha=1)
            gg <- gg + geom_point(data=ggSite,size=3)
                                        #   gg <- gg + geom_line(data = ggSite,mapping = aes(x=YEAR,y=med,colour=HABITAT),size=1.8,alpha=.8)

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

##
##
##
##
##   t.nbnf.session <- unique(subset(dhAd,select=c("NEW.ID_PROG","YEAR","SESSION","FS.OUTPUT")))
##   t.nbnf <- aggregate(FS.OUTPUT~NEW.ID_PROG+YEAR, data=t.nbnf.session, sum, na.rm=FALSE)
##
##   t.fs.output <- tapply(t.nbnf.session$FS.OUTPUT,list(t.nbnf.session$NEW.ID_PROG,t.nbnf.session$YEAR), sum)
##                                       #t.fs.saisi <- tapply(t.nbnf.session$FS,list(t.nbnf.session$NEW.ID_PROG,t.nbnf.session$YEAR), sum)
##
##   t.fs <- data.frame(NEW.ID_PROG = rep(rownames(t.fs.output),ncol(t.fs.output)),
##                      YEAR = rep(colnames(t.fs.output),each=nrow(t.fs.output)),
##                      FS.OUPUT = as.vector(as.matrix(t.fs.output)))
##   t.fs <- subset(t.fs,!is.na(FS.OUPUT))
##
##
##   t.abund <- merge(t.abund,t.fs,by=c("NEW.ID_PROG","YEAR"))
##   t.abund <- merge(t.abund,du.hab,by="NEW.ID_PROG")
##
##   t.abund$ABUND.ADJUST <- t.abund$ABUND / t.abund$FS.OUPUT*(120*3)
##   ggTable.abund <- rbind(t.abund)
##
##                                       #   ggTable.abund <- na.omit(ggTable.abund)
##
##   aggTable <- aggregate(ABUND.ADJUST ~ (YEAR + HABITAT), data= ggTable.abund, quantile,c(0.025,0.25,0.5,0.75,0.975))
##   aggTable <- data.frame(aggTable[,1:2],aggTable[3][[1]][,1:5])
##   colnames(aggTable)[3:7] <- c("CIinf","CIquart_inf","med","CIquart_sup","CIsup")
##

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

    t.nbnf.session <- unique(subset(dhAd,select=c("NEW.ID_PROG","YEAR","SESSION","FS.OUTPUT")))
    t.nbnf <- aggregate(FS.OUTPUT~NEW.ID_PROG+YEAR, data=t.nbnf.session, sum, na.rm=FALSE)

    t.fs.output <- tapply(t.nbnf.session$FS.OUTPUT,list(t.nbnf.session$NEW.ID_PROG,t.nbnf.session$YEAR), sum)
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
        minYear <- max(min(tSite$YEAR)-2,min(t.abund$YEAR))
        maxYear <- min(max(tSite$YEAR)+2,max(t.abund$YEAR))

        aggTable.s <- subset(aggTable,HABITAT==habitat)
        aggTable.s <- subset(aggTable.s,YEAR>=minYear & YEAR<=maxYear)

                                        #ggSite <- data.frame(YEAR = tSite$YEAR,HABITAT=habitat,FS_CAT = paste("Station:",s,"| FS déduit"),CIinf=NA ,med=tSite$ABUND.ADJUST.DEDUIT,CIsup=NA)
#browser()

        ggSite <- data.frame(YEAR = tSite$YEAR,HABITAT = paste("Station:",ss),CIinf=NA,CIquart_inf=NA,med=tSite$ABUND.ADJUST,CIquart_sup=NA,CIsup=NA)

        aggTable.s <- rbind(aggTable.s,ggSite)
        aggTable.s$HABITAT <- as.character(aggTable.s$HABITAT)

        if(add_title) title_txt <- paste("Captures toutes espèces\npour la station ",ss,sep="") else title_txt <- ""

        gg <- ggplot(aggTable.s,aes(x=YEAR,y=med,colour=HABITAT,fill=HABITAT))
        gg <- gg + geom_ribbon(aes(ymin=CIinf,ymax=CIsup),alpha=.2,colour = NA)
        gg <- gg + geom_line(aes(y=CIquart_inf),size=0.6,alpha=.6)+ geom_line(aes(y=CIquart_sup,color=HABITAT),size=0.6,alpha=.6)
        gg <- gg + geom_line(size=1.5,alpha=1)
        gg <- gg + geom_point(data=ggSite,size=3)
                                        #   gg <- gg + geom_line(data = ggSite,mapping = aes(x=YEAR,y=med,colour=HABITAT),size=1.8,alpha=.8)
        gg <- gg + scale_colour_manual(values =setNames(c("#07307b","#d71818"),c(habitat,paste("Station:",ss))))
        gg <- gg + scale_fill_manual(values= setNames(c("#07307b","#d71818"),c(habitat,paste("Station:",ss))),guide=FALSE)

        ##gg <- gg + scale_colour_manual(breaks=c(habitat,paste("Station:",ss)),values =c("#db6363","#08306b"))
        ##gg <- gg + scale_fill_manual(breaks=c(habitat,paste("Station:",ss)),values =c("#db6363","#08306b"),guide=FALSE)
        gg <- gg + theme(legend.position="none")
        gg <- gg + labs(title=title_txt,
                             x="Année",y="Nombre d'individus adultes capturés",
                             colour="")
        ##  browser()                                        #

        if(print.fig) print(gg)

        if(save.fig) {
            ggfile <- paste("output/",ss,"/N_adulte_Site_",ss,".png",sep="")
            catlog(c("Check",ggfile,":"),fileLog)
            ggsave(ggfile,gg)
            catlog(c("\n"),fileLog)
        }
    }# END for(ss in site)


                                        # return( ggTable.abund)

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
    dNb$PROD <- dNb$BAGUE.x / (dNb$BAGUE.x + dNb$BAGUE.y)

                                        #   ggTable.abund <- na.omit(ggTable.abund)

    aggTable <- read.csv2("data_France/Productivite_all_.csv")

    if(is.null(site)) site <- sort(as.character(unique(d$NEW.ID_PROG)))



                                        #  print(site)
    for(ss in site) {
                                        #  catlog(c("site:",s,"\n"),fileLog)
        tSite <- subset(dNb, NEW.ID_PROG==ss)
        habitat <- as.character(tSite$HABITAT[1])
        minYear <- max(min(tSite$YEAR)-3,min(dp$YEAR))
        maxYear <- min(max(tSite$YEAR)+3,max(dp$YEAR))

        aggTable.s <- subset(aggTable,HABITAT==habitat)
        aggTable.s <- subset(aggTable.s,YEAR>=minYear & YEAR<=maxYear)

        ggSite <- data.frame(YEAR = tSite$YEAR,HABITAT= paste("Station:",ss),MIGRATION=tSite$MIGRATION,
                             CIinf=NA,CIquart_inf=NA,med=tSite$PROD,CIquart_sup=NA,CIsup=NA)
        aggTable.s <- rbind(aggTable.s,ggSite)

        if(add_title) title_txt <-  paste("Station",ss) else title_txt <- ""

        gg <- ggplot(aggTable.s,aes(x=YEAR,y=med,colour=HABITAT,fill=HABITAT))+ facet_grid(~MIGRATION)
        gg <- gg + geom_ribbon(aes(ymin=CIinf,ymax=CIsup),alpha=.2,colour = NA )
        gg <- gg + geom_line(alpha=.8,size=1.5)
        gg <- gg + geom_line(aes(y=CIquart_inf),alpha=.5,size=.8)+geom_line(aes(y=CIquart_sup),alpha=.5,size=.8)
        gg <- gg + geom_point(data=subset(aggTable.s,HABITAT==paste("Station:",ss)),size=2)
        gg <- gg + scale_x_continuous(breaks=pretty_breaks())
        gg <- gg + scale_colour_manual(values =setNames(c("#07307b","#d71818"),c(habitat,paste("Station:",ss))))
        gg <- gg + scale_fill_manual(values= setNames(c("#07307b","#d71818"),c(habitat,paste("Station:",ss))),guide=FALSE)
        gg <- gg + labs(list(title=title_txt,
                             x="Année",y="Productivité: Njuv/(Nad + Njuv)"))
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




productivityYearSpecies.site <- function(d,site=NULL,fileLog="log.txt",print.fig=TRUE,save.fig=FALSE,add_title=TRUE)
{
    require(ggplot2)
### d <- read.csv2("output/data.csv",stringsAsFactors=FALSE)

    tabSpeQuant <- read.csv2("data_france/quantileEspece_France_.csv",stringsAsFactors=FALSE)


    dp <- unique(subset(d,AGE_first != "VOL" & MIGRATION != "",select = c("ESPECE","MIGRATION","YEAR","NEW.ID_PROG","HABITAT","BAGUE","AGE_first")))

    dNbAge <- aggregate(BAGUE ~ (ESPECE + MIGRATION + YEAR + NEW.ID_PROG + HABITAT + AGE_first), data=dp,length)

    dNbJuv <- subset(dNbAge,AGE_first=="JUV")

    dNbAd <- subset(dNbAge,AGE_first=="AD")

    dNb <- merge(dNbJuv,dNbAd,by=c("ESPECE","MIGRATION","YEAR","NEW.ID_PROG","HABITAT"),all=TRUE)
    dNb$BAGUE.x[is.na(dNb$BAGUE.x)] <- 0
    dNb$BAGUE.y[is.na(dNb$BAGUE.y)] <- 0
    dNb$PROD <- dNb$BAGUE.x / (dNb$BAGUE.x + dNb$BAGUE.y)


    aggTable <- aggregate(PROD ~ (YEAR + HABITAT+ESPECE + MIGRATION), data= dNb, quantile,c(0.025,.25,0.5,.75,0.975))
    aggTable <- data.frame(aggTable[,1:4],aggTable[5][[1]][,1:5])
    colnames(aggTable)[5:9] <- c("CIinf","CIquart_inf","med","CIquart_sup","CIsup")

    for(hh in unique(aggTable$HABITAT)) {
        tabSpeHab <- subset(tabSpeQuant,HABITAT==hh)
        listSP <- sort(as.character(tabSpeHab$SP[tabSpeHab$PROP_STATION_CAPTURE>.2]))
        aggTable.h <- subset(aggTable,HABITAT==hh & ESPECE %in% listSP)
        if (hh== unique(aggTable$HABITAT)[1]) aggTable.all <- aggTable.h else aggTable.all <- rbind(aggTable.all,aggTable.h)
    }


    if(is.null(site))site <- sort(as.character(unique(d$NEW.ID_PROG)))

    for(ss in site) {
                                        #  browser()
        dNbsite <- subset(dNb,NEW.ID_PROG==ss)

        hh <- as.character(dNbsite$HABITAT[1])
        tabSpeHab <- subset(tabSpeQuant,HABITAT==hh)
        listSP <- sort(as.character(tabSpeHab$SP[tabSpeHab$PROP_STATION_CAPTURE>.2]))
        dNbsite <- subset(dNbsite,ESPECE %in% listSP)

        dNbsite$NB <- dNbsite$BAGUE.x + dNbsite$BAGUE.y
        aggNbSp <- aggregate(NB~(YEAR+ESPECE),data=dNbsite,sum)
        aggSp <- aggregate(NB~ESPECE,data=aggNbSp,FUN= median)
        listSP.s <- sort(as.character(aggSp$ESPECE[aggSp$NB>10]))

        dNbsite <- subset(dNbsite,ESPECE %in% listSP.s)

        if(nrow(dNbsite) > 0) {
            minYear <- max(min(dNbsite$YEAR)-2,min(aggTable.all$YEAR))
            maxYear <- min(max(dNbsite$YEAR)+2,max(aggTable.all$YEAR))



            aggTable.h <- subset(aggTable.all,HABITAT==hh & ESPECE %in% listSP.s & YEAR >= minYear & YEAR <= maxYear)



            ggSite <- data.frame(YEAR = dNbsite$YEAR,HABITAT= paste("Station:",ss),ESPECE=dNbsite$ESPECE,MIGRATION=dNbsite$MIGRATION,
                                 CIinf=NA,CIquart_inf=NA,med=dNbsite$PROD,CIquart_sup=NA,CIsup=NA)
            aggTable.s <- rbind(aggTable.h,ggSite)

              if(add_title) title_txt <-  paste("Station",ss) else title_txt <- ""


            gg <- ggplot(aggTable.s,aes(x=YEAR,y=med,colour=HABITAT,fill=HABITAT))+ facet_wrap(~ESPECE)
            gg <- gg + geom_ribbon(aes(x=YEAR,ymin=CIinf,ymax=CIsup),alpha=.2,colour = NA )
            gg <- gg + geom_line(aes(y=CIquart_inf),alpha=.5,size=.8)+geom_line(aes(y=CIquart_sup),alpha=.5,size=.8)
            gg <- gg + geom_line(alpha=.8,size=1.5)
            gg <- gg + geom_point(data=subset(aggTable.s,HABITAT==paste("Station:",ss)),size=2)
            gg <- gg + scale_colour_manual(values =setNames(c("#07307b","#d71818"),c(hh,paste("Station:",ss))))
            gg <- gg + scale_fill_manual(values= setNames(c("#07307b","#d71818"),c(hh,paste("Station:",ss))),guide=FALSE)
            gg <- gg + coord_cartesian(xlim=c(minYear,maxYear)) + scale_x_continuous(breaks=pretty_breaks())
            gg <- gg + labs(list(title=title_txt,
                                 x="Année",y="Productivité: Njuv/(Nad + Njuv)"))
            gg <- gg + theme(legend.position="none")

            if(print.fig) print(gg)


            if(save.fig) {
                ggfile <- paste("output/",ss,"/Productivite_Espece_Site_",ss,".png",sep="")
                catlog(c("Check",ggfile,":"),fileLog)
                ggsave(ggfile,gg)
                catlog(c("\n"),fileLog)
            }
        }
    }

}






bodyCondition.site<- function(d,site=NULL,limitTime=TRUE,seuilAbondanceAnnee=30,seuilAbondanceAnneeSite=10,fileLog="log.txt",print.fig=TRUE,save.fig=FALSE,add_title=TRUE)
{
    ##   site="193"
    ##   d <- read.csv2("output/data.csv")
    ##   ddd <- d
    ##   d <- ddd

    require(ggplot2)

    if(is.null(site))
        site <- sort(unique(d$NEW.ID_PROG)) else
                                                d <- subset(d,HABITAT %in% unique(subset(d,NEW.ID_PROG %in% site)$HABITAT))
    vecHab <- unique(d$HABITAT)

    aggTable <- aggregate(MA_indice_borne ~ AGE_first + YEAR + HABITAT,subset(d,AGE_first!="VOL"),quantile, c(0.025,0.25,0.5,0.75,0.975))
    aggTable <- data.frame(aggTable[,1:3],aggTable[4][[1]][,1:5])
    colnames(aggTable)[4:8] <- c("CIinf","CIquart_inf","med","CIquart_sup","CIsup")




    for(h in vecHab) {
        dh <- subset(d,HABITAT==h & AGE_first != "VOL")
        dh.seuil<- aggregate(BAGUE ~ SP + YEAR, data = unique(subset(dh,select=c("BAGUE","SP","YEAR"))), FUN = length)
        dh.seuil<- aggregate(BAGUE ~ SP , data = dh.seuil, FUN = median)
                                        # dh.seuil <- subset( dh.seuil,BAGUE > 50)#length(unique(d.a$NEW.ID_PROG))*.25)

        dh <- subset(dh,SP %in% dh.seuil$SP)


        dh$MA_LP <- dh$MA_borne/dh$LP_indice_borne

        aggTable.spMALP <- aggregate(MA_LP ~ SP + YEAR + AGE_first + HABITAT, dh, quantile, c(0.025,0.25,0.5,0.75,0.975))
        aggTable.spMALP <- data.frame(aggTable.spMALP[,1:4],aggTable.spMALP[5][[1]][,1:5])
        colnames(aggTable.spMALP)[5:9] <- c("CIinf","CIquart_inf","med","CIquart_sup","CIsup")

        if(h == vecHab[1]) aggTable.sp <- aggTable.spMALP else aggTable.sp <- rbind(aggTable.sp,aggTable.spMALP)



    }

    aggTable$gr <- paste("FRANCE,",aggTable$AGE_first)
    aggTable.sp$gr <- paste("FRANCE,",aggTable.sp$AGE_first)
    aggTable$SITE <- "FRANCE"
    aggTable.sp$SITE <- "FRANCE"


    for(s in site) {
                                        #  print(s)
############################### ICI
                                        #   catlog(c("site:",s,"\n"),fileLog)
        tSite <- subset(d,AGE_first!="VOL" & NEW.ID_PROG==s & (!(is.na(MA_borne))) & (!(is.na(LP_indice_borne))))
        if(nrow(tSite)>10) {
            h <- tSite$HABITAT[1]


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


            minYear <- max(min(aggTable_s$YEAR)-3,min(aggTable$YEAR))
            maxYear <- min(max(aggTable_s$YEAR)+3,max(aggTable$YEAR))


            gg <- ggplot(data=aggTableTot,aes(x=YEAR,y=med,colour=gr,fill=gr)) + facet_grid(HABITAT~AGE_first,scales="free")
            gg <- gg + geom_ribbon(aes(ymin=CIinf,ymax=CIsup),alpha=.4,colour = NA)
            gg <- gg + geom_line(size=1.5,alpha=.8)
            gg <- gg + geom_line(aes(y=CIquart_inf),alpha=.5,size=.8)+geom_line(aes(y=CIquart_sup),alpha=.5,size=.8)
            gg <- gg + scale_colour_manual(values =c("#07307b","#0c5ef6","#c10909","#ea5d18"),name="")
            gg <- gg + scale_fill_manual(values =c("#07307b","#0c5ef6","#c10909","#ea5d18"),guide=FALSE)
            gg <- gg + labs(list(title=paste("Station ",s,": toutes espèces confondues",sep=""),x="Année",y="Condition corporelle: (MA-MA_mean_sp)/MA_mean_sp"))
            gg <- gg + coord_cartesian(xlim=c(minYear,maxYear)) + scale_x_continuous(breaks=pretty_breaks())
            gg <- gg + theme(legend.position="none")

            ggfile <- paste("output/",s,"/bodyCondition_site",s,".png",sep="")

            catlog(c("Check",ggfile,":"),fileLog)
            ggtry <- try(ggsave(ggfile,gg),silent = TRUE)
            if(class(ggtry)[1] == "try-error") catlog(c("Pas de figure\n"),fileLog) else catlog(c("\n"),fileLog)

            t.seuil<- aggregate(BAGUE ~ SP + YEAR, data = unique(subset(tSite,select=c("BAGUE","SP","YEAR"))), FUN = length)
            t.seuil<- aggregate(BAGUE ~ SP , data =  t.seuil, FUN = median)
            t.seuil <- subset( t.seuil,BAGUE > 10)#length(unique(d.a$NEW.ID_PROG))*.25)

            tS.seuil <- subset(tSite,SP %in% t.seuil$SP)


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
                aggTableTot.sp <- rbind(subset(aggTable.sp,SP%in%t.seuil$SP & HABITAT==h),aggTable.sp_s)

                minYear <- max(min(aggTable.sp_s$YEAR)-3,min(aggTable.sp$YEAR))
                maxYear <- min(max(aggTable.sp_s$YEAR)+3,max(aggTable.sp$YEAR))


                tPoint <- data.frame(SP = tS.seuil$SP,YEAR=tS.seuil$YEAR,HABITAT=tS.seuil$HABITAT,
                                     med= tS.seuil$MA_LP,gr= paste("Site ",s,",",tS.seuil$AGE_first, sep=""),
                                     AGE_first=tS.seuil$AGE_first)
                                        #  browser()
                gg <- ggplot(data=aggTableTot.sp,aes(x=YEAR,y=med,colour=gr,fill=gr)) + facet_grid(SP~AGE_first,scales="free")
                gg <- gg + geom_ribbon(aes(ymin=CIinf,ymax=CIsup),alpha=.4,colour = NA)
                gg <- gg + geom_line(size=1.5,alpha=.8)
                                        # gg <- gg + geom_point(size=1)
                gg <- gg + geom_line(aes(y=CIquart_inf),alpha=.5,size=.8)+geom_line(aes(y=CIquart_sup),alpha=.5,size=.8)
                                        #      gg <- gg + geom_point(aes(y=CIquart_inf),alpha=.5,size=.5)+geom_point(aes(y=CIquart_sup),alpha=.5,size=.5)
                gg <- gg + geom_point(data=tPoint,size=.5,alpha=.5)
                gg <- gg + scale_colour_manual(values =c("#07307b","#0c5ef6","#c10909","#ea5d18"),name="")
                gg <- gg + scale_fill_manual(values =c("#07307b","#0c5ef6","#c10909","#ea5d18"),guide=FALSE)
                gg <- gg + labs(list(title=paste("Station",s),y="Condition corporelle: Masse/(Ecart à la taille moyenne + 1)",x="Année"))
                gg <- gg + coord_cartesian(xlim=c(minYear,maxYear)) + scale_x_continuous(breaks=pretty_breaks())
                gg <- gg + theme(legend.position="none")


                ggfile <- paste("output/",s,"/bodyCondition_sp_site",s,".png",sep="")

                catlog(c("Check",ggfile,":"),fileLog)
                ggtry <- try(ggsave(ggfile,gg,width=6, height=8),silent = TRUE)
                if(class(ggtry)[1] == "try-error") catlog(c("Pas de figure\n"),fileLog) else catlog(c("\n"),fileLog)
                catlog(c("\n"),fileLog)


            }
        }
    }
}




returnRate.site <- function(d,site=NULL,seuilAbondanceAnnee=30,seuilAbondanceAnneeSite=10,fileLog="log.txt",print.fig=TRUE,save.fig=FALSE)
{
    require(ggplot2)

    if(is.null(site)) {
        site <- sort(unique(d$NEW.ID_PROG))
        habitat <- NULL
    } else {
        habitat <- as.character(unique(subset(d,NEW.ID_PROG %in% site)$HABITAT))
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


        d.ret.a <- aggregate(RETURN ~ NEW.ID_PROG + YEAR + MIGRATION, data = d.a, FUN = returnRateAssessment)
        d.compt.a <- aggregate(BAGUE ~ NEW.ID_PROG + YEAR + MIGRATION , data = d.a, FUN = length)
        d.ret.a <- merge(d.ret.a,d.compt.a,by=c("NEW.ID_PROG","YEAR","MIGRATION"))

        d.ret.j <- aggregate(RETURN ~ NEW.ID_PROG + YEAR + MIGRATION , data = d.j, FUN = returnRateAssessment)
        d.compt.j <- aggregate(BAGUE ~ NEW.ID_PROG + YEAR + MIGRATION, data = d.j, FUN = length)
        d.ret.j <- merge(d.ret.j,d.compt.j,by=c("NEW.ID_PROG","YEAR","MIGRATION"))


        d.ret.a$AGE_first <- "AD"
        d.ret.j$AGE_first <- "JUV"


        d.ret <- rbind(d.ret.a,
                       d.ret.j)


        aggTable <- aggregate(RETURN ~ (YEAR + AGE_first + MIGRATION), data= subset(d.ret,BAGUE>10), quantile,c(0.025,0.25,0.5,0.75,0.975))
        aggTable <- data.frame(aggTable[,1:3],aggTable[4][[1]][,1:5])
        colnames(aggTable)[4:8] <- c("CIinf","CIquart_inf","med","CIquart_sup","CIsup")
        aggTable <- subset(aggTable,YEAR<max(aggTable$YEAR))
        aggTable$HABITAT <- h

        if (h == vecHab[1]) aggTableAll.tot <- aggTable else aggTableAll.tot <- rbind(aggTableAll.tot,aggTable)
        if (h == vecHab[1]) d.ret.tot <- d.ret else d.ret.tot <- rbind(d.ret.tot,d.ret)

        dsp.ret.a <- aggregate(RETURN ~ NEW.ID_PROG + YEAR + SP+ MIGRATION, data = d.a, FUN = returnRateAssessment)
        dsp.compt.a <- aggregate(BAGUE ~ NEW.ID_PROG + YEAR + SP+ MIGRATION , data = d.a, FUN = length)
        dsp.ret.a <- merge(dsp.ret.a,dsp.compt.a,by=c("NEW.ID_PROG","YEAR","SP","MIGRATION"))

        dsp.ret.j <- aggregate(RETURN ~ NEW.ID_PROG + YEAR + SP+ MIGRATION , data = d.j, FUN = returnRateAssessment)
        dsp.compt.j <- aggregate(BAGUE ~ NEW.ID_PROG + YEAR + SP+ MIGRATION, data = d.j, FUN = length)
        dsp.ret.j <- merge(dsp.ret.j,dsp.compt.j,by=c("NEW.ID_PROG","YEAR","SP","MIGRATION"))


        dsp.ret.a$AGE_first <- "AD"
        dsp.ret.j$AGE_first <- "JUV"


        dsp.ret <- rbind(dsp.ret.a,
                         dsp.ret.j)

                                        # dsp.ret <- subset(dsp.ret,BAGUE>5)

                                        #u.dsp.ret <- subset(dsp.ret,select=c("NEW.ID_PROG","SP","AGE",)))
                                        #  dsp.medcompt <- aggregate(BAGUE ~ NEW.ID_PROG + SP + AGE_first, data = dsp.ret, FUN = median)

        dsp.medcompt <- aggregate(BAGUE ~ NEW.ID_PROG + SP + YEAR , data = dsp.ret, FUN = sum)
        dsp.medcompt <- aggregate(BAGUE ~ NEW.ID_PROG + SP , data = dsp.medcompt, FUN = median)

        dsp.medSeuil <- subset(dsp.medcompt,BAGUE>10)

                                        #   dsp.ret <- dsp.ret[paste(dsp.ret$NEW.ID_PROG,dsp.ret$SP,dsp.ret$AGE_first) %in% paste(dsp.medSeuil$NEW.ID_PROG,dsp.medSeuil$SP,dsp.medSeuil$AGE_first), ]
        dsp.ret <- dsp.ret[paste(dsp.ret$NEW.ID_PROG,dsp.ret$SP) %in% paste(dsp.medSeuil$NEW.ID_PROG,dsp.medSeuil$SP), ]

                                        #  dsp.medSeuil.compt <- aggregate(NEW.ID_PROG ~ SP, data = dsp.medSeuil, FUN = length)

                                        #    dsp.medSeuil.compt <- subset( dsp.medSeuil.compt,NEW.ID_PROG > 30)#length(unique(d.a$NEW.ID_PROG))*.25)

                                        #   dsp.ret <- dsp.ret[which(paste(dsp.ret$SP) %in% paste( dsp.medSeuil.compt$SP)),]

        aggTableSP <- aggregate(RETURN ~ (YEAR + AGE_first + SP + MIGRATION), data= dsp.ret, quantile,c(0.025,0.25,0.5,0.75,0.975))
        aggTableSP <- data.frame(aggTableSP[,1:4],aggTableSP[5][[1]][,1:5])
        colnames(aggTableSP)[5:9] <- c("CIinf","CIquart_inf","med","CIquart_sup","CIsup")
        aggTableSP <- subset(aggTableSP,YEAR<max(aggTableSP$YEAR))
        aggTableSP$HABITAT <- h


        if(h == vecHab[1]) aggTableSP.tot <- aggTableSP else aggTableSP.tot <- rbind(aggTableSP.tot,aggTableSP)
        if (h == vecHab[1]) dsp.ret.tot <- dsp.ret else dsp.ret.tot <- rbind(dsp.ret.tot,dsp.ret)





    }



    for(s in site) {
                                        #    catlog(c(s,"\n"),fileLog)
        d.ret.s <- subset(d.ret.tot,NEW.ID_PROG==s)
        if(length(unique(d.ret.s$YEAR))>1) {

            h <- subset(d,NEW.ID_PROG==s)$HABITAT[1]
            d.ret.s <- subset(d.ret.s,YEAR<max(d.ret.s$YEAR))
            minYear <- max(min(d.ret.s$YEAR)-3,min(d$YEAR))
            maxYear <- min(max(d.ret.s$YEAR)+3,max(d$YEAR))

            d.ret.s <- subset(d.ret.s,BAGUE>10)

            if(nrow(d.ret.s)>0) {

                gg <- ggplot(subset(aggTableAll.tot,HABITAT==h),aes(x=YEAR,y=med,colour=MIGRATION,fill=MIGRATION))+ facet_grid(MIGRATION~AGE_first)
                gg <- gg + geom_ribbon(aes(ymin=CIinf,ymax=CIsup),alpha=.4,colour = NA)
                gg <- gg + geom_line(size=1.5,alpha=.8)
                gg <- gg + geom_line(aes(y=CIquart_inf),alpha=.5,size=.8)+geom_line(aes(y=CIquart_sup),alpha=.5,size=.8)
                gg <- gg + scale_colour_manual(values =c("#07307b","#0c5ef6"))
                gg <- gg + scale_fill_manual(values =c("#07307b","#0c5ef6"),guide=FALSE)
                gg <- gg + coord_cartesian(xlim=c(minYear,maxYear)) + scale_x_continuous(breaks=pretty_breaks())#trunc(seq(minYear,maxYear,length.out= min(length(minYear:maxYear),10))))
                gg <- gg + geom_line(data=d.ret.s, aes(y=RETURN),colour="#d71818",size=1.8,alpha=.5)
                gg <- gg + geom_point(data=d.ret.s, aes(y=RETURN),colour="#d71818",size=2)
                gg <- gg + labs(list(title=paste("Station ",s,": toutes espèces confondue",sep=""),
                                     x="Année",y="Taux de retour"))
                gg <- gg + theme(legend.position="none")

                ggfile <- paste("output/",s,"/ReturnRate_Site_",s,".png",sep="")
                catlog(c("Check",ggfile,":"),fileLog)
                ggsave(ggfile,gg,width=7, height=6)
                catlog(c("\n"),fileLog)

            }
        }


        dsp.ret.s <- subset(dsp.ret.tot,NEW.ID_PROG==s)
        if(length(unique(dsp.ret.s$YEAR))>1) {
            if(nrow(dsp.ret.s)>0) {
                dsp.ret.s <- subset(dsp.ret.s,YEAR<max(dsp.ret.s$YEAR))

                compt <- aggregate(BAGUE ~ SP + YEAR, data=dsp.ret.s, sum)
                compt <- aggregate(BAGUE ~ SP, data=compt, median)

                spEligible <- compt$SP[compt$BAGUE>10]
                if(length(spEligible)>0) {
                    dsp.ret.s <- subset(dsp.ret.s,SP %in% spEligible)



                    gg <- ggplot(subset(aggTableSP.tot,(HABITAT==h & SP %in% spEligible)),aes(x=YEAR,y=med,colour=MIGRATION,fill=MIGRATION))+facet_grid(SP~AGE_first)
                    gg <- gg + geom_ribbon(aes(ymin=CIinf,ymax=CIsup),alpha=.4,colour = NA)
                    gg <- gg + geom_line(size=1.5,alpha=.8)
                    gg <- gg + geom_line(aes(y=CIquart_inf),alpha=.5,size=.8)+geom_line(aes(y=CIquart_sup),alpha=.5,size=.8)
                    gg <- gg + scale_colour_manual(values =c("#07307b","#0c5ef6"))
                    gg <- gg + scale_fill_manual(values =c("#07307b","#0c5ef6"),guide=FALSE)
                    gg <- gg + coord_cartesian(xlim=c(minYear,maxYear)) + scale_x_continuous(breaks=pretty_breaks())#trunc(seq(minYear,maxYear,length.out= min(length(minYear:maxYear),8))))
                    gg <- gg + geom_line(data=dsp.ret.s, aes(y=RETURN),colour="#d71818",size=1.8,alpha=.5)
                    gg <- gg + geom_point(data=dsp.ret.s, aes(y=RETURN),colour="#d71818",size=2)
                    gg <- gg + theme(legend.position="none")

                    gg <- gg + labs(list(title=paste("Station ",s,sep=""),
                                         x="Année",y="Taux de retour à t+1"))


                    ggfile <- paste("output/",s,"/ReturnRate_sp_Site_",s,".png",sep="")
                    catlog(c("Check",ggfile,":"),fileLog)
                    ggsave(ggfile,gg,width=7, height=7)
                    catlog(c("\n"),fileLog)
                }
            }
        }
    }



}






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
