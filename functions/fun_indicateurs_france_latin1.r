
source("functions/fun_generic_latin1.r")

speciesRelativeAbund.all <- function(d,fileLog=NULL, habitat=NULL,print=TRUE, print.fig=FALSE,save.fig=TRUE,save.data_france=TRUE) {
    require(ggplot2)
    dsp <- read.csv2("library/sp.csv",stringsAsFactors=FALSE)

    habitatDemande <-  habitat
    if(is.null(habitat)) habitats <- unique(as.character(d$HABITAT)) else habitats <- habitat


    for(habitat in habitats)
    {

        dh <- subset(d,HABITAT == habitat)


        du.bague <- unique(subset(dh,AGE_first == "AD",select=c("SP","BAGUE","NEW.ID_PROG","YEAR")))
        tableNbCapt <- data.frame(table(du.bague$SP,du.bague$NEW.ID_PROG,du.bague$YEAR))
        colnames(tableNbCapt) <- c("SP","NEW.ID_PROG","YEAR","ABUND")
        du.bagueSite <- unique(subset(dh,select=c("SP","BAGUE","NEW.ID_PROG")))
        tableNbCaptSite <- data.frame(table(du.bagueSite$SP,du.bagueSite$NEW.ID_PROG))
        colnames(tableNbCaptSite) <- c("SP","NEW.ID_PROG","ABUND_site")

        du.bagueSite <- unique(subset(dh,select=c("BAGUE","NEW.ID_PROG","YEAR")))
        tableNbCaptYear <- data.frame(table(du.bagueSite$NEW.ID_PROG,du.bagueSite$YEAR))
        colnames(tableNbCaptYear) <- c("NEW.ID_PROG","YEAR","capt_site_year")


        tableStationYear <- unique(subset(dh,select=c("NEW.ID_PROG","YEAR")))
        nbStationYear <- data.frame(table(tableStationYear$YEAR))
        colnames(nbStationYear) <- c("YEAR","NB_STATION")
        tableNbCapt <- merge(tableNbCapt,tableStationYear)

        tableNbCapt <- merge(tableNbCapt,tableNbCaptSite,by=c("SP","NEW.ID_PROG"))
        tableNbCapt <- subset(tableNbCapt,ABUND_site>0)

        nbSite_sp <- aggregate(NEW.ID_PROG ~ SP , unique(tableNbCapt[,c("SP","NEW.ID_PROG")]),length)
        colnames(nbSite_sp)[2] <- "nb_site"
        nbSite_sp$prop_site <- nbSite_sp$nb_site / length(unique(tableNbCapt$NEW.ID_PROG))


        nbSiteCaptYear <- aggregate(ifelse(tableNbCapt$ABUND>0,1,0),by=list(tableNbCapt$SP,tableNbCapt$YEAR),sum)
        colnames(nbSiteCaptYear) <- c("SP","YEAR","NB_STATION_CAPTURE")

        tableCaptYear <- merge(nbSiteCaptYear,nbStationYear,by=c("YEAR"))
        tableCaptYear$PROP_STATION_CAPTURE <- tableCaptYear$NB_STATION_CAPTURE / tableCaptYear$NB_STATION

        tableNbCapt <- merge(tableNbCapt,tableNbCaptYear,by=c("NEW.ID_PROG","YEAR"))
        tableNbCapt <- subset(tableNbCapt,capt_site_year>0)

        ggTableQuant <- aggregate(tableNbCapt$ABUND,by=list(tableNbCapt$SP,tableNbCapt$YEAR),quantile,c(0.025,.25,.5,.75,0.90,0.975))

        ggTable <- data.frame(ggTableQuant[,1:2],ggTableQuant[,3][,1:6])
        colnames(ggTable) <- c("SP","YEAR","ABUND025","ABUND25","ABUND50","ABUND75","ABUND90","ABUND975")
        ggTable <- merge(ggTable,tableCaptYear,by=c("SP","YEAR"))

        ggTable$SP <- as.character(ggTable$SP)
        ggTable <- merge(ggTable,dsp,by="SP")

        ggTable <- aggregate(subset(ggTable, select=-c(SP,YEAR,HABITAT_SP,MIGRATION)),by=list(SP=ggTable$SP),median)

        ggTable$SP <- as.character(ggTable$SP)
        ggTable <- merge(ggTable,dsp,by="SP")
        ggTable <- merge(ggTable,nbSite_sp, by ="SP")


        if(habitat==habitats[1]) tableSpQuant <- data.frame(HABITAT=habitat,ggTable) else tableSpQuant <- rbind(tableSpQuant,data.frame(HABITAT=habitat,ggTable))

        ggTable$SP <- factor(ggTable$SP)
        ggTable <- subset(ggTable,PROP_STATION_CAPTURE>.2)

        gg <- ggplot(ggTable,aes(x=reorder(SP, (1-ABUND75)),y=ABUND50,fill=HABITAT_SP,colour=HABITAT_SP))
        gg <- gg +geom_linerange(aes(ymin=ABUND025,ymax=ABUND975))+ geom_crossbar(aes(ymin = ABUND25, ymax = ABUND75), width = 0.5)
        gg <- gg + theme(axis.text.x  = element_text(angle=90,vjust=.5),legend.position="none")
        gg <- gg + labs(x="Espèce",y="Nombre d'individus adultes capturés par station et par an\n(parmi les stations qui capture l'espèce)",title=paste("Capture dans les sites de type",habitat))
        gg <- gg +scale_y_log10(breaks=c(0,1,2,5,10,20,50,100,200,400))#+coord_cartesian(ylim=c(0,50)) # scale_y_continuous(breaks=seq(0,80,10))
        gg <- gg + scale_fill_manual(breaks=c("Aquatique","Terrestre"),values=c("#077be7","#076d0d"))
        gg <- gg + scale_colour_manual(breaks=c("Aquatique","Terrestre"),values=c("#05529a","#073e0d"))


        if(print.fig) print(gg)
                                        #  gg
        if(save.fig) {
            ggfile <- paste("output/France/nbCapture_France",habitat,".png",sep="")
            catlog(c("Check",ggfile,":"),fileLog)
            ppi <- 300
            ggsave(ggfile,gg,width=11, height=8, unit="in",limitsize = FALSE)
            catlog(c("DONE \n"),fileLog)
        }

    }

    if(save.data_france){

        file <- paste0("data_France/quantileEspece_France_",habitatDemande,".csv")
        catlog(c("  -> ",file,"\n"),fileLog)
        write.csv2(tableSpQuant,file,row.names=FALSE)
    }

}







##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param d
##' @param habitat
##' @param fileLog
##' @return
##' @author Romain Lorrilliere
##'
abundanceYear.all <- function(d,habitat=NULL,fileLog="log.txt",print.fig=FALSE,save.fig=TRUE,save.data_france=TRUE)
{
    require(ggplot2)


    dhAd <- subset(d,AGE_first == "AD")

    if(!is.null(habitat)) dhAd <- subset(dhAd,HABITAT == habitat)


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



##    t.fs.output <- tapply(t.nbnf.session$FS,list(t.nbnf.session$NEW.ID_PROG,t.nbnf.session$YEAR), sum)
##                                        #t.fs.saisi <- tapply(t.nbnf.session$FS,list(t.nbnf.session$NEW.ID_PROG,t.nbnf.session$YEAR), sum)
##
##    t.fs <- data.frame(NEW.ID_PROG = rep(rownames(t.fs.output),ncol(t.fs.output)),
##                       YEAR = rep(colnames(t.fs.output),each=nrow(t.fs.output)),
##                       FS.OUPUT = as.vector(as.matrix(t.fs.output)))
##    t.fs <- subset(t.fs,!is.na(FS.OUPUT))
    ##



    t.fs <- aggregate(FS~NEW.ID_PROG + YEAR + HABITAT,data= unique(subset(d,select=c("NEW.ID_PROG","HABITAT","YEAR","SESSION","FS","FS.OUTPUT"))),sum)

    if(!is.null(habitat))  t.fs  <- subset( t.fs ,HABITAT == habitat)


    t.fs <- t.fs[,c("NEW.ID_PROG","YEAR","FS")]
    t.abund <- merge(t.abund,t.fs,by=c("NEW.ID_PROG","YEAR"))
    t.abund.all <- data.frame(t.abund,HABITAT = "tout")
    t.abund <- merge(t.abund,du.hab,by="NEW.ID_PROG")
    if(is.null(habitat))      t.abund <- rbind(t.abund,t.abund.all)

    t.abund$ABUND.ADJUST <- t.abund$ABUND / t.abund$FS*(120*3)
    ggTable.abund <- rbind(t.abund)

                                        #   ggTable.abund <- na.omit(ggTable.abund)

    aggTable <- aggregate(ABUND.ADJUST ~ (YEAR + HABITAT), data= ggTable.abund, quantile,c(0.025,0.25,0.5,0.75,0.975))
    aggTable <- data.frame(aggTable[,1:2],aggTable[3][[1]][,1:5])
    colnames(aggTable)[3:7] <- c("CIinf","CIquart_inf","med","CIquart_sup","CIsup")


    gg <- ggplot(aggTable,aes(x=YEAR,y=CIsup))
    gg <- gg + geom_ribbon(data = aggTable,
                           aes(x=YEAR,ymin=CIinf,ymax=CIsup),alpha=.2,colour = NA,fill="#08306b")
                                        #  gg <- gg + geom_jitter(data = ggTable.abund,mapping = aes(x=YEAR,y=ABUND.ADJUST),colour="#08306b",alpha=.1,size=2,width=.5)
    gg <- gg + geom_line(aes(x=YEAR,y=CIquart_inf),colour="#08306b",size=0.6,alpha=.6)+ geom_line(aes(x=YEAR,y=CIquart_sup),colour="#08306b",size=0.6,alpha=.6)
    gg <- gg + geom_line(aes(x=YEAR,y=med),colour="#08306b",size=1.5,alpha=1)

    gg <- gg + labs(list(title="Nombre d'adulte capturés (toutes espèces confondues)\npour 1 station standard (3 sessions et 120m de filet)",
                         x="Année",y="N/120m",
                         colour="")) + facet_wrap(~HABITAT,nrow = 2)

    if(print.fig) print(gg)

    if(save.fig) {
        ggfile <- paste("output/France/N_adulte_France_",habitat,".png",sep="")
        catlog(c("Check",ggfile,":"),fileLog)
        ggsave(ggfile,gg)
        catlog(c("\n"),fileLog)
    }

    if(save.data_france){
        file <- paste0("data_France/N_adulte_France_",habitat,".csv")
        catlog(c("  -> ",file,"\n"),fileLog)
        write.csv2(aggTable,file,row.names=FALSE)
    }


                                        # return( ggTable.abund)

}










abundanceSpeciesYear.all <- function(d,habitat=NULL,fileLog="log.txt",print.fig=FALSE,save.fig=TRUE,save.data_france=TRUE)
{
    require(ggplot2)

    tabSpeQuant <- read.csv2("data_France/quantileEspece_France_.csv",stringsAsFactors=FALSE)

    if(is.null(habitat)) habitat <- unique(d$HABITAT)


    aggTable_all <- NULL


    for(h in habitat)
    {
        cat("\n",h,"\n-------------\n\n")
        tabSpeHab <- subset(tabSpeQuant,HABITAT == h)
        listSP <- tabSpeHab$SP[tabSpeHab$PROP_STATION_CAPTURE>0]
        for(sp in listSP) {
            cat("\n",sp,"\n")

            dhAd <- subset(d,HABITAT == h & AGE_first == "AD" & ESPECE == sp)
            du.bague <- unique(subset(dhAd,select=c("BAGUE","ESPECE","NEW.ID_PROG","YEAR")))
            if(nrow(du.bague)>30) {
                du.hab <- unique(subset(dhAd,select=c("NEW.ID_PROG","HABITAT")))
                t.abund <- tapply(du.bague$BAGUE,list(du.bague$NEW.ID_PROG,du.bague$YEAR),length)
                t.abund <- ifelse(is.na(t.abund),0,t.abund)
                t.abund <- data.frame(t.abund)
                t.abund <- reshape(t.abund, direction = "long",
                                   idvar = "NEW.ID_PROG",
                                   ids=rownames(t.abund),
                                   varying = 1:ncol(t.abund),sep="")
                colnames(t.abund) <- c("YEAR","ABUND","NEW.ID_PROG")

                if(nrow(t.abund)>1) {


                    t.nbnf.session <- unique(subset(dhAd,select=c("NEW.ID_PROG","YEAR","SESSION","FS")))
                    t.nbnf <- aggregate(FS~NEW.ID_PROG+YEAR, data=t.nbnf.session, sum, na.rm=FALSE)

                    t.fs.output <- tapply(t.nbnf.session$FS,list(t.nbnf.session$NEW.ID_PROG,t.nbnf.session$YEAR), sum)
                                        #t.fs.saisi <- tapply(t.nbnf.session$FS,list(t.nbnf.session$NEW.ID_PROG,t.nbnf.session$YEAR), sum)

                    t.fs <- data.frame(NEW.ID_PROG = rep(rownames(t.fs.output),ncol(t.fs.output)),
                                       YEAR = rep(colnames(t.fs.output),each=nrow(t.fs.output)),
                                       FS.OUPUT = as.vector(as.matrix(t.fs.output)))
                    t.fs <- subset(t.fs,!is.na(FS.OUPUT))


                    t.abund <- merge(t.abund,t.fs,by=c("NEW.ID_PROG","YEAR"))
                                        # t.abund.all <- data.frame(t.abund,HABITAT = "tout")
                    t.abund <- merge(t.abund,du.hab,by="NEW.ID_PROG")
                                        # if(is.null(habitat))      t.abund <- rbind(t.abund,t.abund.all)

                    t.abund$ABUND.ADJUST <- t.abund$ABUND / t.abund$FS.OUPUT*(120*3)
                    ggTable.abund <- rbind(t.abund)

                                        #   ggTable.abund <- na.omit(ggTable.abund)

                    aggTable <- aggregate(ABUND.ADJUST ~ (YEAR + HABITAT), data= ggTable.abund, quantile,c(0.025,0.25,0.5,0.75,0.975))
                    aggTable <- data.frame(aggTable[,1:2],aggTable[3][[1]][,1:5])


                    colnames(aggTable)[3:7] <- c("CIinf","CIquart_inf","med","CIquart_sup","CIsup")


                    gg <- ggplot(aggTable,aes(x=YEAR,y=CIsup))
                    gg <- gg + geom_ribbon(data = aggTable,
                                           aes(x=YEAR,ymin=CIinf,ymax=CIsup),alpha=.2,colour = NA,fill="#08306b")
                                        #  gg <- gg + geom_jitter(data = ggTable.abund,mapping = aes(x=YEAR,y=ABUND.ADJUST),colour="#08306b",alpha=.1,size=2,width=.5)
                    gg <- gg + geom_line(aes(x=YEAR,y=CIquart_inf),colour="#08306b",size=0.6,alpha=.6)+ geom_line(aes(x=YEAR,y=CIquart_sup),colour="#08306b",size=0.6,alpha=.6)
                    gg <- gg + geom_line(aes(x=YEAR,y=med),colour="#08306b",size=1.5,alpha=1)

                    gg <- gg + labs(title=paste(sp,sep=""),
                                    x="Année",y="Nombre d'individus adultes capturés",
                                    colour="")



                    if(print.fig) print(gg)


                    if(save.fig) {
                        ggfile <- paste("output/France/N_adulte_France_",h,"_",sp,".png",sep="")
                        catlog(c("Check",ggfile,":"),fileLog)
                        ggsave(ggfile,gg)
                        catlog(c("\n"),fileLog)
                    }

                    if(save.data_france) {
                        aggTable$HABITAT <- h
                        aggTable$ESPECE <- sp
                        aggTable_all <- rbind(aggTable_all,aggTable)

                    }
                }
            }
        }# END for(sp in listSP)
    }# END  for(h in habitat)
                                        # return( ggTable.abund)

    if(save.data_france){
        file <- paste0("data_France/N_adulte_sp_France.csv")
        catlog(c("  -> ",file,"\n"),fileLog)
        write.csv2(aggTable_all,file,row.names=FALSE)
    }

} ### abundanceSpeciesYear.all







##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param d
##' @param habitat
##' @param fileLog
##' @return
##' @author Romain Lorrilliere

productivityYear.all <- function(d,habitat=NULL,fileLog="log.txt",print.fig=FALSE,save.fig=TRUE,save.data_france=TRUE)
{
    require(ggplot2)
###d <- read.csv2("output/data.csv")
    habitatDemande <- as.character(habitat)
    dp <- unique(subset(d,AGE_first != "VOL" & MIGRATION != "" ,select = c("MIGRATION","YEAR","NEW.ID_PROG","HABITAT","BAGUE","AGE_first")))
    if(!is.null(habitat)) dp <- subset(dp,HABITAT == habitat)

    dNbAge <- aggregate(BAGUE ~ (MIGRATION + YEAR + NEW.ID_PROG + HABITAT + AGE_first), data=dp,length)

    dNbJuv <- subset(dNbAge,AGE_first=="JUV")

    dNbAd <- subset(dNbAge,AGE_first=="AD")

    dNb <- merge(dNbJuv,dNbAd,by=c("MIGRATION","YEAR","NEW.ID_PROG","HABITAT"),all=TRUE)
    dNb$BAGUE.x[is.na(dNb$BAGUE.x)] <- 0
    dNb$BAGUE.y[is.na(dNb$BAGUE.y)] <- 0
    dNb$PROD <- dNb$BAGUE.x / (dNb$BAGUE.y)
    dNb$PROD[dNb$PROD == Inf] <- NA


    aggTable <- aggregate(PROD ~ (YEAR + HABITAT+MIGRATION), data= subset(dNb,!is.na(PROD)), quantile,c(0.025,.25,0.5,.75,0.975))
    aggTable <- data.frame(aggTable[,1:3],aggTable[4][[1]][,1:5])
    colnames(aggTable)[4:8] <- c("CIinf","CIquart_inf","med","CIquart_sup","CIsup")


    gg <- ggplot(aggTable,aes(x=YEAR,y=med,colour=MIGRATION,fill=MIGRATION))+ facet_grid(HABITAT~MIGRATION)
    gg <- gg + geom_ribbon(data = aggTable,
                           aes(x=YEAR,ymin=CIinf,ymax=CIsup),alpha=.2,colour = NA )
    gg <- gg + geom_line(alpha=.8,size=1.5)
    gg <- gg + geom_line(aes(y=CIquart_inf),alpha=.5,size=.8)+geom_line(aes(y=CIquart_sup),alpha=.5,size=.8)
    gg <- gg + scale_colour_manual(values =c("#07307b","#0c5ef6"))
    gg <- gg + scale_fill_manual(values =c("#07307b","#0c5ef6"),guide=FALSE)
    gg <- gg + scale_x_continuous(breaks=pretty_breaks())
    gg <- gg + labs(list(title="Productivité\n pour 1 station standard (3 sessions)",
                         x="Année",y="Njuv/(Nad + Njuv)"))

    if(print.fig) print(gg)

    if(save.fig) {
        ggfile <- paste("output/France/productivite_all_",habitatDemande,".png",sep="")
        catlog(c("Check",ggfile,":"),fileLog)
        ggsave(ggfile,gg)
        catlog(c("\n"),fileLog)
    }

    if(save.data_france) {
        file <- paste0("data_France/productivite_all_",habitatDemande,".csv")
        catlog(c("  -> ",file,"\n"),fileLog)
        write.csv2(aggTable,file,row.names=FALSE)
    }


} ### END productivityYear.all









productivityYearSpecies.all <- function(d,habitat=NULL,fileLog="log.txt",print.fig=FALSE,save.fig=TRUE,save.data_france=TRUE)
{
    require(ggplot2)
### d <- read.csv2("output/data.csv",stringsAsFactors=FALSE)

    tabSpeQuant <- read.csv2("data_France/quantileEspece_France_.csv",stringsAsFactors=FALSE)

    aggTable_all <- NULL

    if(!is.null(habitat)) dp <- subset(dp,HABITAT == habitat)


    dp <- unique(subset(d,AGE_first != "VOL" & MIGRATION != "",select = c("ESPECE","MIGRATION","YEAR","NEW.ID_PROG","HABITAT","BAGUE","AGE_first")))

    dNbAge <- aggregate(BAGUE ~ (ESPECE + MIGRATION + YEAR + NEW.ID_PROG + HABITAT + AGE_first), data=dp,length)

    dNbJuv <- subset(dNbAge,AGE_first=="JUV")

    dNbAd <- subset(dNbAge,AGE_first=="AD")

    dNb <- merge(dNbJuv,dNbAd,by=c("ESPECE","MIGRATION","YEAR","NEW.ID_PROG","HABITAT"),all=TRUE)
    dNb$BAGUE.x[is.na(dNb$BAGUE.x)] <- 0
    dNb$BAGUE.y[is.na(dNb$BAGUE.y)] <- 0

    dNb$PROD <- dNb$BAGUE.x / (dNb$BAGUE.y)
    dNb$PROD[dNb$PROD == Inf] <- NA


    aggTable <- aggregate(PROD ~ (YEAR + HABITAT+ESPECE + MIGRATION), data= subset(dNb,!is.na(PROD)), quantile,c(0.025,.25,0.5,.75,0.975))
    aggTable <- data.frame(aggTable[,1:4],aggTable[5][[1]][,1:5])
    colnames(aggTable)[5:9] <- c("CIinf","CIquart_inf","med","CIquart_sup","CIsup")

    for(hh in unique(aggTable$HABITAT)) {
        tabSpeHab <- subset(tabSpeQuant,HABITAT==hh)
        listSP <- sort(as.character(tabSpeHab$SP[tabSpeHab$PROP_STATION_CAPTURE>0.05]))
        aggTable.h <- subset(aggTable,HABITAT==hh & ESPECE %in% listSP)


        gg <- ggplot(aggTable.h,aes(x=YEAR,y=med,colour=MIGRATION,fill=MIGRATION))+ facet_wrap(~ESPECE)
        gg <- gg + geom_ribbon(aes(x=YEAR,ymin=CIinf,ymax=CIsup),alpha=.2,colour = NA )
        gg <- gg + geom_line(alpha=.8,size=1.5)
        gg <- gg + geom_line(aes(y=CIquart_inf),alpha=.5,size=.8)+geom_line(aes(y=CIquart_sup),alpha=.5,size=.8)
        gg <- gg + scale_x_continuous(breaks=pretty_breaks())
        gg <- gg + scale_colour_manual(values =c("#07307b","#0c5ef6"))
        gg <- gg + scale_fill_manual(values =c("#07307b","#0c5ef6"),guide=FALSE)
        ## gg <- gg + scale_colour_manual(values =setNames(c("#07307b","#d71818"),c(hh,paste("Station:",ss))))
        ## gg <- gg + scale_fill_manual(values= setNames(c("#07307b","#d71818"),c(hh,paste("Station:",ss))),guide=FALSE)
        ##   gg <- gg + scale_colour_manual(values =c("#07307b","#0c5ef6"))
        ## gg <- gg + scale_fill_manual(values =c("#07307b","#0c5ef6"),guide=FALSE)

        gg <- gg + labs(list(title=paste("Productivité:",hh,"\n pour 3 sessions"),
                             x="Année",y="Njuv/(Nad + Njuv)"))

        if(print.fig) print(gg)

        if(save.fig) {
            ggfile <- paste("output/France/productivite_Espece_FRANCE_",hh,".png",sep="")
            catlog(c("Check",ggfile,":"),fileLog)
            ggsave(ggfile,gg)
            catlog(c("\n"),fileLog)
        }
    } # END for(hh in unique(aggTable$HABITAT))

    if(save.data_france) {
        file <- paste0("data_France/productivite_Espece_FRANCE.csv")
        catlog(c("  -> ",file,"\n"),fileLog)
        write.csv2(aggTable,file,row.names=FALSE)
    }
}











bodyCondition.all <- function(d,habitat=NULL,do.all=TRUE,do.sp=TRUE,seuilAbondanceAnnee=50,fileLog="log.txt",print.fig=FALSE,save.fig=TRUE,save.data_france=TRUE)
{

    require(ggplot2)
    ##    d <- read.csv2("output/data.csv")
    ##    ddd <- d
    ##    d <- ddd

    if(do.all) {
        aggTable.MA <- aggregate(MA_indice_borne ~ AGE_first + YEAR + HABITAT,subset(d,AGE_first!="VOL"),quantile, c(0.025,0.25,0.5,0.75,0.975))
        aggTable.MA <- data.frame(aggTable.MA[,1:3],aggTable.MA[4][[1]][,1:5])
        colnames(aggTable.MA)[4:8] <- c("CIinf","CIquart_inf","med","CIquart_sup","CIsup")

        gg <- ggplot(data=aggTable.MA,aes(x=YEAR,y=med,colour=AGE_first,fill=AGE_first)) + facet_grid(HABITAT~AGE_first,scales="free")
        gg <- gg + geom_ribbon(aes(ymin=CIinf,ymax=CIsup),alpha=.4,colour = NA)
        gg <- gg + geom_line(size=1.1,alpha=.8)
        gg <- gg + geom_line(aes(y=CIquart_inf),alpha=.5,size=.8)+geom_line(aes(y=CIquart_sup),alpha=.5,size=.8)
        gg <- gg + scale_colour_manual(values =c("#07307b","#0c5ef6"),name="AGE")
        gg <- gg + scale_fill_manual(values =c("#07307b","#0c5ef6"),guide=FALSE)
        gg <- gg + labs(list(title="Condition corporelle des individus\ntoutes espèces confondue",x="Année",y="(MA-MA_mean_sp)/MA_mean_sp"))

        if(print.fig) print(gg)

        if(save.fig) {
            ggfile <- paste("output/France/bodyCondition_all.png",sep="")

            catlog(c("Check",ggfile,":"),fileLog)
            ggsave(ggfile,gg,width=5, height=6)
            catlog(c("\n"),fileLog)
        }


        if(save.data_france){
            file <- paste0("data_France/bodyCondition_France.csv")
            catlog(c("  -> ",file,"\n"),fileLog)
            write.csv2(aggTable.MA,file,row.names=FALSE)
        }

    } # END if(do.all)

    if(do.sp) {
        if(is.null(habitat))
            vecHab <- as.character(unique(d$HABITAT))

        aggTable_all <- NULL

        for(h in vecHab) {
            dh <- subset(d,HABITAT==h & AGE_first != "VOL")
            dh.seuil<- aggregate(BAGUE ~ SP + YEAR, data = unique(subset(dh,select=c("BAGUE","SP","YEAR"))), FUN = length)
            dh.seuil<- aggregate(BAGUE ~ SP , data = dh.seuil, FUN = median)

            dh.seuil <- subset( dh.seuil,BAGUE > 50)#length(unique(d.a$NEW.ID_PROG))*.25)

            dh <- subset(dh,SP %in% dh.seuil$SP)


            dh$MA_LP <- dh$MA_borne/dh$LP_indice_borne

            aggTable.spMALP <- aggregate(MA_LP ~ SP + YEAR + AGE_first + MIGRATION, dh, quantile, c(0.025,0.25,0.5,0.75,0.975))
            aggTable.spMALP <- data.frame(aggTable.spMALP[,1:4],aggTable.spMALP[5][[1]][,1:5])
            colnames(aggTable.spMALP)[5:9] <- c("CIinf","CIquart_inf","med","CIquart_sup","CIsup")


            gg <- ggplot(aggTable.spMALP,aes(x=YEAR,y=med,colour=AGE_first, fill=AGE_first)) + facet_wrap(~SP,scales="free",ncol=3)
            gg <- gg + geom_ribbon(aes(ymin=CIinf,ymax=CIsup),alpha=.4,colour = NA)
            gg <- gg + geom_line(size=1,alpha=.8)
            gg <- gg + geom_line(aes(y=CIquart_inf),alpha=.5,size=.5)+geom_line(aes(y=CIquart_sup),alpha=.5,size=.5)
            gg <- gg + scale_x_continuous(breaks=pretty_breaks())
            gg <- gg + scale_colour_manual(values =c("#07307b","#0c5ef6"),name="AGE")
            gg <- gg + scale_fill_manual(values =c("#07307b","#0c5ef6"),guide=FALSE)
            gg <- gg + labs(list(title=paste("Condition corporelle des individus pour les stations de type ",h,sep=""),x="Année",y="Masse/(Ecart à la taille moyenne + 1)"))


            if(print.fig) print(gg)

            if(save.fig) {
                ggfile <- paste("output/France/bodyCondition_sp_",h,".png",sep="")
                catlog(c("Check",ggfile,":"),fileLog)
                ggsave(ggfile,gg,width=8, height=10)
                catlog(c("\n"),fileLog)
            }

            if(save.data_france) {

                aggTable.spMALP$HABITAT <- h

                aggTable_all <- rbind(aggTable_all,aggTable.spMALP)
            }

        } # END for(h in vecHab)

        if(save.data_france){

            file <- paste0("data_France/bodyCondition_France_sp_.csv")
            catlog(c("  -> ",file,"\n"),fileLog)
            write.csv2(aggTable_all,file,row.names=FALSE)

        }
    } # END   if(do.sp)

} ### END bodyCondition.all







returnRate.all <- function(d,habitat=NULL,do.all=TRUE,do.sp=TRUE,seuilAbondanceAnnee=30,fileLog="log.txt",print.fig=FALSE,save.fig=TRUE,save.data_france=TRUE)
{
    require(ggplot2)

    if(!is.null(habitat)) d <- subset(d,HABITAT == habitat)

    vecHab <- unique(d$HABITAT)
    for(h in vecHab)
    {

        if(do.all) {
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

        } # END if(do.all)

        if(do.sp) {

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

            dsp.medSeuil.compt <- aggregate(NEW.ID_PROG ~ SP, data = dsp.medSeuil, FUN = length)

            dsp.medSeuil.compt <- subset( dsp.medSeuil.compt,NEW.ID_PROG > 10)#length(unique(d.a$NEW.ID_PROG))*.25)

            dsp.ret <- dsp.ret[which(paste(dsp.ret$SP) %in% paste( dsp.medSeuil.compt$SP)),]

            aggTableSP <- aggregate(RETURN ~ (YEAR + AGE_first + SP + MIGRATION), data= dsp.ret, quantile,c(0.025,0.25,0.5,0.75,0.975))
            aggTableSP <- data.frame(aggTableSP[,1:4],aggTableSP[5][[1]][,1:5])
            colnames(aggTableSP)[5:9] <- c("CIinf","CIquart_inf","med","CIquart_sup","CIsup")
            aggTableSP <- subset(aggTableSP,YEAR<max(aggTableSP$YEAR))
            aggTableSP$HABITAT <- h


            if(h == vecHab[1]) aggTableSP.tot <- aggTableSP else aggTableSP.tot <- rbind(aggTableSP.tot,aggTableSP)

        } # END if(do.sp)

    }# END    for(h in vecHab)

    for(h in vecHab) {

        if(do.all) {

            gg <- ggplot(subset(aggTableAll.tot,HABITAT==h & MIGRATION != ""),aes(x=YEAR,y=med,colour=MIGRATION,fill=MIGRATION))+ facet_grid(AGE_first~MIGRATION,scale="free_y")
            gg <- gg + geom_ribbon(aes(ymin=CIinf,ymax=CIsup),alpha=.4,colour = NA)
            gg <- gg + geom_line(size=1.5,alpha=.8)
            gg <- gg + geom_line(aes(y=CIquart_inf),alpha=.5,size=.8)+geom_line(aes(y=CIquart_sup),alpha=.5,size=.8)
            gg <- gg + scale_colour_manual(values =c("#07307b","#0c5ef6"))
            gg <- gg + scale_fill_manual(values =c("#07307b","#0c5ef6"),guide=FALSE)
            gg <- gg + scale_x_continuous(breaks=pretty_breaks())
            gg <- gg + labs(title=paste("Taux de retour sur les sites de type ",h,"\npar site, toutes espèces confondue",sep=""),
                            x="Année",y="Taux contrôle à t+1")

            if(print.fig) print(gg)

            if(save.fig) {
                ggfile <- paste("output/France/returnRate_all_",h,".png",sep="")
                catlog(c("Check",ggfile,":"),fileLog)
                ggsave(ggfile,gg,width=7, height=6)
                catlog(c("\n"),fileLog)
            }

        } # if(do.all)

        if(do.sp) {

            gg <- ggplot(subset(aggTableSP.tot,HABITAT==h),aes(x=YEAR,y=med,colour=MIGRATION,fill=MIGRATION))+facet_grid(SP~AGE_first)
            gg <- gg + geom_ribbon(aes(ymin=CIinf,ymax=CIsup),alpha=.4,colour = NA)
            gg <- gg + geom_line(size=1.5,alpha=.8)
            gg <- gg + geom_line(aes(y=CIquart_inf),alpha=.5,size=.8)+geom_line(aes(y=CIquart_sup),alpha=.5,size=.8)
            gg <- gg + scale_colour_manual(values =c("#07307b","#0c5ef6"))
            gg <- gg + scale_fill_manual(values =c("#07307b","#0c5ef6"),guide=FALSE)
            gg <- gg + labs(title=paste("Taux de retour sur les sites de type ",h,"\npour chaque espèces éligible",sep=""),
                            x="Année",y="Taux contrôle à t+1")

            if(print.fig) print(gg)

            if(save.fig) {
                ggfile <- paste("output/France/returnRate_sp_",h,".png",sep="")
                catlog(c("Check",ggfile,":"),fileLog)
                ggsave(ggfile,gg,width=7, height=9)
                catlog(c("\n"),fileLog)
            }

        } # END if(do.sp)

    } # END for(h in vecHab)


    if(save.data_france) {
        if(do.all) {
            file <- paste0("data_France/returnRate_all_France.csv")
            catlog(c("  -> ",file,"\n"),fileLog)
            write.csv2(aggTableAll.tot,file,row.names=FALSE)
        }

        if(do.sp) {
            file <- paste0("data_France/returnRate_sp_France.csv")
            catlog(c("  -> ",file,"\n"),fileLog)
            write.csv2(aggTableSP.tot,file,row.names=FALSE)

        }
    }# END if(save.data_france)
} ### END returnRate.all



