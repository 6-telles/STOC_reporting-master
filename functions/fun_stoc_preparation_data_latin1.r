

### les 2 fonctions
### fileLog <- paste("log",format(start, "%Y-%m-%d_%HH%M"),".txt",sep="")
### d <- import(file,lastYear=2019,decimalData=".",fileDataClean="data.csv",fileLog, seuilAvorteDuree=4,seuilAvorteEvenement=5,seuilExclusionDelai=10,dateRefDefaut=c(138,165,188))
### d <- select3sessions(d,fileData3sessions="data3session.csv",fileLog,seuilAvorteEvenement=5,seuilExclusionDelai=10,dateRefDefaut=c(138,165,188)






### file of data importation and format it to prepare the reporting
                                        # file: the name of the file
                                        # lastYear: last year used to reporting
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param file
##' @param lastYear
##' @param separateurData
##' @param decimalData
##' @param fileDataClean
##' @param fileLog
##' @param seuilAvorteDuree
##' @param seuilAvorteEvenement
##' @param seuilExclusionDelai
##' @param dateRefDefaut
##' @return
##' @author Romain Lorrilliere
import <- function(file="stoc_20161011.txt",lastYear=NULL,
                   decimalData=".",fileDataClean="data.csv",fileLog="log.txt",
                   seuilAvorteDuree= 3, seuilAvorteEvenement=4,seuilExclusionDelai = 10,dateRefDefaut =c(138,165,188)) {


                                   ##        fileLog="log.txt" #####
                                   ##
                                   ##        decimalData="." #####
                                   ##        file = "stoc_20180616.txt" #####
                                   ##        lastYear = NULL #####
                                   ##    seuilAvorteDuree= 3; seuilAvorteEvenement=4;seuilExclusionDelai = 10;dateRefDefaut =c(138,165,188) ###


    catlog(c("\n====================================\n              Suppression error files if exists\n==================================== \n\n"),fileLog)
    if (file.exists("output_preparation/WARNING_DATA.csv")) {
        file.remove("output_preparation/WARNING_DATA.csv")
        catlog(c("    -> WARNING_DATA.csv removed\n"),fileLog)
    } else {  catlog(c(" --> OK\n"),fileLog)}

### importations
    catlog(c("\n====================================\n              IMPORTATION\n==================================== \n\nfile : ",
             file,"\n\n"),fileLog)
                                        # d: dataset
    file <- paste("donnees/",file,sep="")
    d <-read.delim(file, header = TRUE, sep = "\t" ,dec=decimalData, na = "",stringsAsFactors=FALSE)
    if(ncol(d)==1)     d <-read.delim(file, header = TRUE, sep = ";" ,dec=decimalData, na = "",stringsAsFactors=FALSE)

    if(ncol(d)==1)     d <-read.delim(file, header = TRUE, sep = "," ,dec=".", na = "",stringsAsFactors=FALSE)

    d <- d %>% mutate_if(is.character, Encoding_utf8)

    if(length(grep("..",colnames(d)[1]))>0) colnames(d)[1] <- gsub(".\\.{2}","",colnames(d)[1])


    catlog(c("    => Initial number of lines: ",nrow(d),"\n"),fileLog)
   ##browser()
    dsp <- read.csv2("library/sp.csv",stringsAsFactors=FALSE)
                                        #browser()

    altitude <- read.csv("library/coordonnees_altitude_site.csv",stringsAsFactors=FALSE)
    rownames(altitude) <- altitude$NEW.ID_PROG
    colnames(altitude)[2:3] <- c("LONG2","LAT2")

   ## minmax <- read.csv("library/measuresMinMax_2017-05-22.csv",stringsAsFactors=FALSE)





### new column SP for SP code without ssp information
    catlog(c("\n====================================\n\n - Ajout de colonnes:\n------------------------------------\n"),fileLog)
    catlog(c("    -> SP avec les 6 premiers caractere de ESPECE\n"),fileLog)
    d$SP <-  substr(d$ESPECE,1,6)

    ## new colum H
    catlog(c("    -> H pour la tranche horaire de la capture\n"),fileLog)
    d$H <-  as.numeric(substring(d$HEURE,1,2))

    ## format of date
    d$DATE<-as.Date(d$DATE, format = "%d/%m/%Y")

    ## new column JULIANDAY
    catlog(c("    -> JULIANDAY\n"),fileLog)
    d$JULIANDAY <- yday(d$DATE)

    ## new colum MONTH
    catlog(c("    -> MONTH\n"),fileLog)
    d$MONTH <- month(d$DATE)

    ## new column YEAR
    catlog(c("    -> YEAR\n"),fileLog)
    d$YEAR <- year(d$DATE)

                                       # dd <- d

    if(is.null(lastYear)) lastYear <- max(d$YEAR)

    catlog(c("\n====================================\n              RESUME DE LA TABLE IMPORTEE\n==================================== \n\nfile : ",
             file,"\n\n"),fileLog)
    tCont.bague <- aggregate(BAGUE~ID_PROG + YEAR,d,length)
    colnames(tCont.bague)[3] <- "NB_LIGNE_IMPORT"
    tCont.session <- aggregate(DATE~ID_PROG + YEAR ,unique(subset(d,select=c(ID_PROG,DATE,YEAR))),length)
    colnames(tCont.session)[3] <- "NB_DATE_IMPORT"
    tCont <- merge(tCont.bague,tCont.session,by=c("ID_PROG","YEAR"))
    tCont <- tCont[order(tCont$ID_PROG,tCont$YEAR),]

### data subset
    selectedColumns <- c("ACTION","CENTRE","BAGUE","DATE","YEAR","MONTH","JULIANDAY","HEURE","H",
                         "ESPECE","SP","SEXE","CS","AGE","DEPT","LOCALITE",
                         "LIEUDIT","LP","MA","THEME.SESSION","THEME","BAGUEUR","BG",
                         "COND.REPR","CIRC.REPR","NF","PC","PI","ID_PROG","NEW.ID_PROG","FS","HS","DS","cId_Data","LON","LAT",
                         "PRECISION_LOC","ALTITUDE","COMMUNES","REGION","BIOGEO")

    d$NEW.ID_PROG <- NA
    d$PRECISION_LOC <- NA
    d$ALTITUDE <- NA
    d$COMMUNES <- NA
    d$REGION <- NA
    d$BIOGEO <- NA


  ### fixing strange error in ACTION field (due to encoding)
    catlog(c("\n====================================\n\n - Checking et correction: ACTION field character encoding\n------------------------------------\n"),fileLog)



    ## de data sans ACTION
    de <- subset(d,is.na(ACTION))
    if(nrow(de) > 0) {
     de.warning <- data.frame(error = "ACTION",commmentError="NA",suppression= "ligne", subset(de,select = selectedColumns))

        lwiNA <- nrow(de.warning)

        catlog(c("\n !!! WARNING MESSAGE:",lwiNA," lines excluded\n",
            "Check WARNING_DATA.csv in output_preparation/ dirctory error category : 'ACTION - NA'!!\n"),fileLog)
        if (file.exists("output_preparation/WARNING_DATA.csv"))
            write.table(de.warning,"output_preparation/WARNING_DATA.csv",
                        row.names=FALSE,sep=";",quote=FALSE,append=TRUE,
                        col.names=FALSE) else
                                             write.table(de.warning,"output_preparation/WARNING_DATA.csv",
                                                         row.names=FALSE,sep=";",quote=FALSE,append=FALSE)

    }
    ## data conservees
    d <-subset(d,!(is.na(ACTION)))

                                        # code.action: vector of the possible code for the ACTION field with the good encoding
    code.action <- c("B","C","R")
    ua <- unique(d$ACTION)
    while(length(ua[!(ua %in% code.action)])>0) {
         catlog("\n Il y a des erreurs d'encodage, nous allons corriger ces erreurs",fileLog)
        catlog("\n Veuillez renseigner a quel code font reference le(s) code(s) suivant:\n (Taper le code en majuscule puis [ENTRER])\n",fileLog)
        for (ia in which(!(ua %in% code.action))) {
            catlog(paste(ua[ia],":\n"),fileLog)
            A <-readline(paste("Taper le code",ua[ia],"en majuscule puis [ENTRER]"))
            d$ACTION[d$ACTION == ua[ia]] <- A
        }

         ua <- unique(d$ACTION)
    }
    catlog(c(" --> OK, probleme d encodage corrige \n"),fileLog)

    warning.act <- subset(d,ACTION %nin% code.action)
    la <- nrow(warning.act)
    if(la > 0)
    {
        catlog(c(" !!! WARNING MESSAGE:",la,"lines with a problem in the ACTION character encoding \n"),fileLog)
        catlog(c("They are different from \n 42 -> 'B' \n 43 -> 'C' \n 52 -> 'R' \n  \n"),fileLog)
        catlog(c("==> Check WARNING_DATA.csv in output_preparation/ directory !!\n\n"),fileLog)
        charToStringRaw <- function(X) paste(charToRaw(X),collapse =" ")
        warning.act <- data.frame(error ="encoding action",commmentError=paste("encoding action:",apply(data.frame(warning.act$ACTION),1,charToStringRaw)),suppression= "ligne",subset(warning.act,select=selectedColumns))

        if (file.exists("output_preparation/WARNING_DATA.csv"))
            write.table(warning.act,"output_preparation/WARNING_DATA.csv",
                        row.names=FALSE,sep=";",quote=FALSE,append=TRUE,
                        col.names=FALSE) else
                                             write.table(warning.act,"output_preparation/WARNING_DATA.csv",
                                                         row.names=FALSE,sep=";",quote=FALSE,append=FALSE)


        d <- subset(d,ACTION %in% code.action)
        catlog(c("    => Current number of lines: ",nrow(d),"\n"),fileLog)
    } else  {
        catlog(c(" --> OK\n"),fileLog)
    }



    catlog(c("\n====================================\n\n - Selection :\n------------------------------------\n"),fileLog)
    catlog(c("      -> colonne : [",paste(selectedColumns,collapse=","),"]\n"),fileLog)

    cnd <- colnames(d)
    c_manquante <- selectedColumns[!(selectedColumns %in% cnd)]
    if(length(c_manquante)>0) {
         catlog("\n ATTENTION : colonne attendue non presente dans le tableau d'origine:\n",fileLog)
         catlog(c(paste(c_manquante,collapse = " , "),"\n"),fileLog)
         catlog("Ajout de ces colonnes avec des valeurs NA or id for cId_Data\n",fileLog)

         d <- cbind(d,
                   as.data.frame(matrix(NA,
                                        nrow=nrow(d),
                                        ncol=length(c_manquante))))
         colnames(d)[(ncol(d)-length(c_manquante)+1):ncol(d)] <- c_manquante
         if("cId_Data" %in% c_manquante) d$cId_Data <- 1:nrow(d)
    }
   #browser()
    d <- subset(d,select=selectedColumns)


    ## select year lower and equal to lastYear

    ## de data apres dernière annee lastYear
    de <- subset(d,YEAR>lastYear)
    if(nrow(de) > 0) {
     de.warning <- data.frame(error = "YEAR",commmentError="hors limit",suppression= "ligne", subset(de,select = selectedColumns))

        lwiNA <- nrow(de.warning)

        catlog(c("\n !!! WARNING MESSAGE:",lwiNA," lines excluded\n",
            "Check WARNING_DATA.csv in output_preparation/ dirctory error category : 'YEAR - hors limite'!!\n"),fileLog)
        if (file.exists("output_preparation/WARNING_DATA.csv"))
            write.table(de.warning,"output_preparation/WARNING_DATA.csv",
                        row.names=FALSE,sep=";",quote=FALSE,append=TRUE,
                        col.names=FALSE) else
                                             write.table(de.warning,"output_preparation/WARNING_DATA.csv",
                                                         row.names=FALSE,sep=";",quote=FALSE,append=FALSE)

    }
    ## data conservees
    d <-subset(d,YEAR <= lastYear)

### select data STOC

    catlog(c("      -> data STOC : \n"),fileLog)

    ## de data non STOC
    de <- d[setdiff(1:nrow(d),union(union(grep("STOC",d$THEME),grep("STOC",d$THEME.SESSION)),grep("STOC",toupper(d$LIEUDIT)))),]

    if(nrow(de) > 0) {
     de.warning <- data.frame(error = "THEME",commmentError="non STOC",suppression= "ligne", subset(de,select = selectedColumns))

        lwiNA <- nrow(de.warning)

        catlog(c("\n !!! WARNING MESSAGE:",lwiNA," lines excluded\n",
            "Check WARNING_DATA.csv in output_preparation/ dirctory error category : 'THEME - non STOC'!!\n"),fileLog)
        if (file.exists("output_preparation/WARNING_DATA.csv"))
            write.table(de.warning,"output_preparation/WARNING_DATA.csv",
                        row.names=FALSE,sep=";",quote=FALSE,append=TRUE,
                        col.names=FALSE) else
                                             write.table(de.warning,"output_preparation/WARNING_DATA.csv",
                                                         row.names=FALSE,sep=";",quote=FALSE,append=FALSE)

    }
    ## d data STOC conservees
    d <- d[union(union(grep("STOC",d$THEME),grep("STOC",d$THEME.SESSION)),grep("STOC",toupper(d$LIEUDIT))),]

### select data STOC in spring

      catlog(c("      -> data STOC in spring : \n"),fileLog)
    ## de data non printemps
      de <- subset(d,MONTH < 4 | MONTH > 7)
    if(nrow(de) > 0) {
     de.warning <- data.frame(error = "MONTH",commmentError="hors printemps",suppression= "ligne", subset(de,select = selectedColumns))

        lwiNA <- nrow(de.warning)

        catlog(c("\n !!! WARNING MESSAGE:",lwiNA," lines excluded\n",
            "Check WARNING_DATA.csv in output_preparation/ dirctory error category : 'MONTH - hors printemps'!!\n"),fileLog)
        if (file.exists("output_preparation/WARNING_DATA.csv"))
            write.table(de.warning,"output_preparation/WARNING_DATA.csv",
                        row.names=FALSE,sep=";",quote=FALSE,append=TRUE,
                        col.names=FALSE) else
                                             write.table(de.warning,"output_preparation/WARNING_DATA.csv",
                                                         row.names=FALSE,sep=";",quote=FALSE,append=FALSE)

    }
    ## d data printemps conservees
    d <- subset(d, MONTH >= 4 & MONTH <= 7)
    catlog(c("    => Current number of lines: ",nrow(d),"\n"),fileLog)

### select morning hours
    catlog(c("      -> Morning hours: \n"),fileLog)

    ## de data sans heure saisie
    de <- subset(d,is.na(H))

    if(nrow(de) > 0) {
     de.warning <- data.frame(error = "HEURE",commmentError="non saisie",suppression= "ligne", subset(de,select = selectedColumns))

        lwiNA <- nrow(de.warning)

        catlog(c("\n !!! WARNING MESSAGE:",lwiNA," lines excluded\n",
            "Check WARNING_DATA.csv in output_preparation/ dirctory error category : 'HEURE - non saisie'!!\n"),fileLog)
        if (file.exists("output_preparation/WARNING_DATA.csv"))
            write.table(de.warning,"output_preparation/WARNING_DATA.csv",
                        row.names=FALSE,sep=";",quote=FALSE,append=TRUE,
                        col.names=FALSE) else
                                             write.table(de.warning,"output_preparation/WARNING_DATA.csv",
                                                         row.names=FALSE,sep=";",quote=FALSE,append=FALSE)

    }


    ## data avec heure saisie
    d <- subset(d,!(is.na(H)))

    ## de data non du matin
    de <- subset(d, H < 5 | H > 13)

    if(nrow(de) > 0) {
     de.warning <- data.frame(error = "HEURE",commmentError="pas le matin",suppression= "ligne", subset(de,select = selectedColumns))

        lwiNA <- nrow(de.warning)

        catlog(c("\n !!! WARNING MESSAGE:",lwiNA," lines excluded\n",
            "Check WARNING_DATA.csv in output_preparation/ dirctory error category : 'HEURE - pas le matin'!!\n"),fileLog)
        if (file.exists("output_preparation/WARNING_DATA.csv"))
            write.table(de.warning,"output_preparation/WARNING_DATA.csv",
                        row.names=FALSE,sep=";",quote=FALSE,append=TRUE,
                        col.names=FALSE) else
                                             write.table(de.warning,"output_preparation/WARNING_DATA.csv",
                                                         row.names=FALSE,sep=";",quote=FALSE,append=FALSE)

    }

    ## d data du matin conservees
    d <- subset(d, H >= 5 & H <= 13)

    catlog(c("    => Current number of lines: ",nrow(d),"\n"),fileLog)

    tCont2.bague <- aggregate(BAGUE~ID_PROG + YEAR,d,length)
    colnames(tCont2.bague)[3] <- "NB_LIGNE__PRINTPS_MATIN"
    tCont2.session <- aggregate(DATE~ID_PROG + YEAR ,unique(subset(d,select=c(ID_PROG,DATE,YEAR))),length)
    colnames(tCont2.session)[3] <- "NB_DATE__PRINTPS_MATIN"
    tCont2 <- merge(tCont2.bague,tCont2.session,by=c("ID_PROG","YEAR"))
    tCont <- merge(tCont,tCont2,by=c("ID_PROG","YEAR"),all=TRUE)
    tCont <- tCont[order(tCont$ID_PROG,tCont$YEAR),]


### check ID_PROG empty
    catlog(c("\n====================================\n\n - Checking: ID_PROG should not empty \n------------------------------------\n"),fileLog)

    catlog(c("    --    -> Recuperation de ID_PROG dans LIEUDIT si possible\n"),fileLog)

    idProgLieudit <- as.numeric(str_extract(d$LIEUDIT, "[0-9]{1,}"))
    i.lieuDit.num <- which(!is.na(idProgLieudit))
    i.lieuDit.STOC <- grep("STATION STOC",toupper(d$LIEUDIT))
    i.lieuDit <- intersect(i.lieuDit.num,i.lieuDit.STOC)

    i.ID_PROGna <- which(is.na(d$ID_PROG))
    i <- intersect(i.ID_PROGna,i.lieuDit)

    d$ID_PROG[i] <- idProgLieudit[i]
    catlog(c("    -> Recuperation de ",length(i)," ligne(s)\n"),fileLog)

    ## d_idNA data sans ID_PROG
    d_idNA <- subset(d,is.na(ID_PROG),select=c("ID_PROG","DEPT","LOCALITE",
                                               "LIEUDIT","THEME.SESSION","THEME",
                                               "BAGUEUR","YEAR"))

    if(nrow(d_idNA)>0)
    {
        du_idNA <- unique(d_idNA)
        du_idNA <- du_idNA[order(du_idNA$DEPT, du_idNA$LOCALITE,
                                 du_idNA$LIEUDIT, du_idNA$BAGUEUR,
                                 du_idNA$YEAR, du_idNA$THEME.SESSION,du_idNA$THEME),]
        lidNA <- nrow(du_idNA)
        catlog(c("\n !!! WARNING MESSAGE:",lidNA," location(s) without ID_PROG\n",
            "These data are exclude\n ==> ",
            "Check warning_notID_PROG.csv in output_preparation/ dirctory !!\n"),fileLog)

        write.csv2(du_idNA,"output_preparation/warning_notID_PROG.csv",
                   row.names=FALSE,quote=FALSE)
        d.warning.idNA <- data.frame(error = "ID_PROG null",commmentError="",suppression= "ligne", subset(d,is.na(ID_PROG),select = selectedColumns))

        lwiNA <- nrow(d.warning.idNA)

        catlog(c("\n !!! WARNING MESSAGE:",lwiNA," lines excluded\n",
            "Check WARNING_DATA.csv in output_preparation/ dirctory error category : 'ID null'!!\n"),fileLog)
        if (file.exists("output_preparation/WARNING_DATA.csv"))
            write.table(de.warning,"output_preparation/WARNING_DATA.csv",
                        row.names=FALSE,sep=";",quote=FALSE,append=TRUE,
                        col.names=FALSE) else
                                             write.table(de.warning,"output_preparation/WARNING_DATA.csv",
                                                         row.names=FALSE,sep=";",quote=FALSE,append=FALSE)


        d <- subset(d,!is.na(ID_PROG))

        catlog(c("    => Current number of lines: ",nrow(d),"\n"),fileLog)
    } else {
        catlog(c(" --> OK\n"),fileLog)
    }


### Correction of programe
    catlog(c("\n====================================\n\n - Correction: split and deletion of programme \n------------------------------------\n    informations in 'library/newID_PROG.csv' \n"),fileLog)

    ## d pour besoin local on enleve temporairement la colonne NEW.ID_PROG
    d <- subset(d,select=-(NEW.ID_PROG))
    ## dProg importation des regle de split de station (ID_PROG) selon questionnaire de Manon
    dProg <- read.csv("library/newID_PROG.csv")
    dProg$LAST.YEAR[is.na(dProg$LAST.YEAR)] <- lastYear
    dProg$FIRST.YEAR[is.na(dProg$FIRST.YEAR)] <- min(d$YEAR)

    dProg$nbYear <- dProg$LAST.YEAR - dProg$FIRST.YEAR + 1
    t.idprog <- data.frame(ID_PROG = NULL,YEAR=NULL,NEW.ID_PROG=NULL)

    for(i in 1:nrow(dProg)) {
        print(i)
        t.idprog <- rbind(t.idprog,
                          data.frame(ID_PROG = rep(dProg$ID_PROG[i],dProg$nbYear[i]),
                                     YEAR=dProg$FIRST.YEAR[i] : dProg$LAST.YEAR[i],
                                     NEW.ID_PROG=rep(dProg$NEW.ID_PROG[i],dProg$nbYear[i])))
        }
    catlog(c("    -> New column: NEW.ID_PROG\n"),fileLog)
    d <- merge(d,t.idprog,by = c("ID_PROG","YEAR"),all.x=TRUE)
    d$NEW.ID_PROG <- ifelse(is.na(d$NEW.ID_PROG),as.character(d$ID_PROG),
                            as.character(d$NEW.ID_PROG))
    catlog(c(" !!! WARNING MESSAGE:",length(which(d$NEW.ID_PROG=="XX"))," line(s) deleted\n"),fileLog)

      de <- subset(d,NEW.ID_PROG == "XX")
    if(nrow(de) > 0) {
     de.warning <- data.frame(error = "NEW.ID_PROG",commmentError="split alert",suppression= "ligne", subset(de,select = selectedColumns))

        lwiNA <- nrow(de.warning)

        catlog(c("\n !!! WARNING MESSAGE:",lwiNA," lines excluded\n",
            "Check WARNING_DATA.csv in output_preparation/ dirctory error category : 'NEW.ID_PROG' !!\n"),fileLog)
        if (file.exists("output_preparation/WARNING_DATA.csv"))
            write.table(de.warning,"output_preparation/WARNING_DATA.csv",
                        row.names=FALSE,sep=";",quote=FALSE,append=TRUE,
                        col.names=FALSE) else
                                             write.table(de.warning,"output_preparation/WARNING_DATA.csv",
                                                         row.names=FALSE,sep=";",quote=FALSE,append=FALSE)

    }

    ## d data conserve avec NEW.ID_PROG valide
    d <- subset(d,NEW.ID_PROG != "XX")

    catlog(c("    => Current number of lines: ",nrow(d),"\n"),fileLog)


    tCont2.bague <- aggregate(BAGUE~ID_PROG + NEW.ID_PROG + YEAR,d,length)
    colnames(tCont2.bague)[4] <- "NB_LIGNE_NEW_ID"
    tCont2.session <- aggregate(DATE~ NEW.ID_PROG + YEAR ,unique(subset(d,select=c(ID_PROG,NEW.ID_PROG,DATE,YEAR))),length)
    colnames(tCont2.session)[3] <- "NB_DATE_NEW_ID"
    tCont2 <- merge(tCont2.bague,tCont2.session,by=c("NEW.ID_PROG","YEAR"))
    tCont2 <- tCont2[order(tCont2$ID_PROG,tCont2$YEAR),]
    tCont <- merge(tCont,tCont2,by=c("ID_PROG","YEAR"),all=TRUE)
    tCont <- tCont[order(tCont$ID_PROG,tCont$YEAR),]

    write.csv(tCont,"output_preparation/tableContingenceStation.csv",row.names=FALSE)

       catlog(c("\n====================================\n\n - Add : Altitude and LON LAT\n------------------------------------\n"),fileLog)



    coordinates(altitude) <- ~ LONG2 + LAT2
    proj4string(altitude) <- CRS("+init=epsg:2154") # lambert 93

    ## conversion en Lamber étendu
    ## altitude fichier propre de Manon des altitude et des coordonnee importé en debut de script
    altitude <- spTransform(altitude,CRS("+proj=longlat +ellps=WGS84")) #transformation en WGS84

    altitude <- data.frame(altitude@data,as.data.frame(altitude@coords))


 ###  browser()


    newCol <- c("PRECISION_LOC","ALTITUDE","COMMUNES","REGION","BIOGEO")

    d <- subset(d,select=setdiff(selectedColumns,newCol))

    d <- merge(d,altitude,by="NEW.ID_PROG",all.x=TRUE)
    d$PRECISION_LOC <- NA
    d$LON[!is.na(d$LONG2)] <- d$LONG2[!is.na(d$LONG2)]
    d$LAT[!is.na(d$LAT2)] <- d$LAT2[!is.na(d$LAT2)]
    d$PRECISION_LOC[!is.na(d$LAT2)] <- d$PRECISION[!is.na(d$LAT2)]
    d$PRECISION_LOC[is.na(d$LAT2)] <- "commune"

    d <- subset(d,select=selectedColumns)

### new column HABITAT
                                        #browser()
    catlog(c("\n====================================\n\n - Checking : Table 'species' complete\n------------------------------------\n"),fileLog)

    d <- merge(d,dsp,by="SP",all=TRUE)
    v.warningSp <- unique(subset(d,is.na(HABITAT_SP))$SP)

    if(length(v.warningSp)>0)
        catlog(c("\n !!! WARNING MESSAGE:",length(v.warningSp)," species are not in the species file\n",
            "Check and add them in the sp.csv in library/ dirctory !!\n"),fileLog) else
                                                                              catlog(c(" --> OK\n"),fileLog)
    catlog(c("\n - Delete SPESPE\n"),fileLog)
    de <- subset(d,SP == "SPESPE")
    if(nrow(de) > 0) {
     de.warning <- data.frame(error = "SP",commmentError="SPESPE",suppression= "ligne", subset(de,select = selectedColumns))

        lwiNA <- nrow(de.warning)

        catlog(c("\n !!! WARNING MESSAGE:",lwiNA," lines excluded\n",
            "Check WARNING_DATA.csv in output_preparation/ dirctory error category : 'SP - SPESPE'!!\n"),fileLog)
        if (file.exists("output_preparation/WARNING_DATA.csv"))
            write.table(de.warning,"output_preparation/WARNING_DATA.csv",
                        row.names=FALSE,sep=";",quote=FALSE,append=TRUE,
                        col.names=FALSE) else
                                             write.table(de.warning,"output_preparation/WARNING_DATA.csv",
                                                         row.names=FALSE,sep=";",quote=FALSE,append=FALSE)

    }

    ## exclusion des ligne dont l'espece est indetermine SPESPE
    d <- subset(d,SP != "SPESPE")
     catlog(c("    => Current number of lines: ",nrow(d),"\n"),fileLog)





    catlog(c("\n====================================\n\n - Selection:\n------------------------------------\n"),fileLog)
### deletions of recovries


    catlog(c("\n - Delete recovery data\n"),fileLog)

    ## de data de reprise exclues
    de <- subset(d,ACTION == "R")
    if(nrow(de) > 0) {
     de.warning <- data.frame(error = "ACTION",commmentError="R",suppression= "ligne", subset(de,select = selectedColumns))

        lwiNA <- nrow(de.warning)

        catlog(c("\n !!! WARNING MESSAGE:",lwiNA," lines excluded\n",
            "Check WARNING_DATA.csv in output_preparation/ dirctory error category : 'ACTION - R'!!\n"),fileLog)
        if (file.exists("output_preparation/WARNING_DATA.csv"))
            write.table(de.warning,"output_preparation/WARNING_DATA.csv",
                        row.names=FALSE,sep=";",quote=FALSE,append=TRUE,
                        col.names=FALSE) else
                                             write.table(de.warning,"output_preparation/WARNING_DATA.csv",
                                                         row.names=FALSE,sep=";",quote=FALSE,append=FALSE)

    }

    # d data baguage et controle conservees
    d <- subset(d,ACTION != "R")

    catlog(c("    => Current number of lines: ",nrow(d),"\n"),fileLog)

### select only good control condition

    catlog(c("\n - Select only good control condition\n"),fileLog)



    ## de pas la bonne methode de controle
     de <- subset(d,(COND.REPR !=8 | CIRC.REPR != 20))
    if(nrow(de) > 0) {
     de.warning <- data.frame(error = "COND.REPR CIRC.REPR",commmentError="diff 8 20",suppression= "ligne", subset(de,select = selectedColumns))

        lwiNA <- nrow(de.warning)

        catlog(c("\n !!! WARNING MESSAGE:",lwiNA," lines excluded\n",
            "Check WARNING_DATA.csv in output_preparation/ dirctory error category : 'COND.REPR CIRC.REPR - diff 8 20'!!\n"),fileLog)
        if (file.exists("output_preparation/WARNING_DATA.csv"))
            write.table(de.warning,"output_preparation/WARNING_DATA.csv",
                        row.names=FALSE,sep=";",quote=FALSE,append=TRUE,
                        col.names=FALSE) else
                                             write.table(de.warning,"output_preparation/WARNING_DATA.csv",
                                                         row.names=FALSE,sep=";",quote=FALSE,append=FALSE)

    }



    ##  d data avec les bonnes condition et circonstance de controle
    d <- subset(d,(COND.REPR==8 & CIRC.REPR==20) | is.na(COND.REPR) & is.na(CIRC.REPR==20))

    catlog(c("    => Current number of lines: ",nrow(d),"\n"),fileLog)



### exclusion of data the ring ID is wrong due to scientific format
    catlog(c("\n====================================\n\n - Checking: Ring ID\n------------------------------------\n"),fileLog)
                                        # warning.ring: ring ID with an error exported in a file log "warning_ring.csv"

    warning.ring1 <- unique(d$BAGUE[grep("E",substr(d$BAGUE,5,11))])
    vec.ring2 <- gsub("\\.","",unique(d$BAGUE))
    i.ringA <- which(nchar(vec.ring2)<7)
    i.ringB <- setdiff(1:length(vec.ring2), grep("[a-zA-Z]",vec.ring2,perl=TRUE))
    warning.ring2 <- unique(d$BAGUE)[intersect(i.ringA,i.ringB)]
    ## warning.ring numeros de bague merdique
    warning.ring <- union(warning.ring1,warning.ring2)

                                        # lb: amount of ring ID needed to display and export
    lb <- length(warning.ring)
    ## if lb > 0 writing the ring ID in "warning_ring.csv"
    if(lb > 0)
    {
        catlog(c(" !!! WARNING MESSAGE:",lb,"ring ID are wrong\n ==> Check warning_ring.csv in output_preparation/ dirctory !!\n"),fileLog)
        t.warning.ring <-data.frame(BAGUE=warning.ring)
        write.csv2(t.warning.ring,"output_preparation/warning_ring.csv",row.names=FALSE,na="",quote=FALSE)

        d.warning.ring <- data.frame(error = "BAGUE",commmentError="",suppression= "ligne",subset(d,BAGUE %in% warning.ring & CENTRE == "FRP",select=selectedColumns))
        catlog(c(nrow(d.warning.ring)," row deleted !!\n==> Check WARNING_DATA.csv in output_preparation/ dirctory !!\n"),fileLog)

        if (file.exists("output_preparation/WARNING_DATA.csv"))
            write.table(d.warning.ring,"output_preparation/WARNING_DATA.csv",
                        row.names=FALSE,sep=";",quote=FALSE,append=TRUE,
                        col.names=FALSE) else
                                             write.table(d.warning.ring,"output_preparation/WARNING_DATA.csv",
                                                         row.names=FALSE,sep=";",quote=FALSE,append=FALSE)



        d <- subset(d,BAGUE %nin% warning.ring & CENTRE == "FRP")
        catlog(c("    => Current number of lines: ",nrow(d),"\n"),fileLog)
    } else  {
        catlog(c(" --> OK\n"),fileLog)
    }



### research rings with several species
    catlog(c("\n====================================\n\n - Checking: Ring id for several species\n------------------------------------\n"),fileLog)
                                        # t.contingency.ring: contingency table between species and ring
    t.contingency.ring <- table(d$BAGUE,d$SP)
                                        # rnames: row names of contengency table
    rnames <- rownames(t.contingency.ring)
                                        # cnames: col names of contengency table
    cnames <- colnames(t.contingency.ring)

    ## t.ring.contingency to binary format
    t.contingency.ring <- data.frame(ifelse(t.contingency.ring > 0,1,0))
    rownames(t.contingency.ring) <- rnames
    colnames(t.contingency.ring) <- cnames

                                        # warning.species_ring: vector of ring ID corresponding with several species
    warning.species_ring <- rownames(t.contingency.ring)[rowSums(t.contingency.ring)>1]
    lsr <- length(warning.species_ring)
    if(lsr > 0)
    {
        catlog(c(" !!! WARNING MESSAGE:",lsr,"ring ID correspond to several species\n ==> Check WARNING_DATA.csv in output_preparation/ directory !!\n\n"),fileLog)
        t.warning.species_ring <- data.frame(error = "bague sur plusieur espece", commmentError="",suppression= "ligne",subset(d,BAGUE %in% warning.species_ring, select=selectedColumns))
        t.warning.species_ring <- t.warning.species_ring[order(t.warning.species_ring$BAGUE,t.warning.species_ring$DATE),]

        if (file.exists("output_preparation/WARNING_DATA.csv"))
            write.table(t.warning.species_ring,"output_preparation/WARNING_DATA.csv",
                        row.names=FALSE,sep=";",quote=FALSE,append=TRUE,
                        col.names=FALSE) else
                                             write.table(t.warning.species_ring,"output_preparation/WARNING_DATA.csv",
                                                         row.names=FALSE,sep=";",quote=FALSE,append=FALSE)


        d <- subset(d,BAGUE %nin% warning.species_ring)
        catlog(c("    => Current number of lines: ",nrow(d),"\n"),fileLog)
    } else  {
        catlog(c(" --> OK\n"),fileLog)
    }


### double ringing
    catlog(c("\n====================================\n\n - Checking: Double ringing\n------------------------------------\n"),fileLog)
                                        # t.contingency.d_ring: contingency table of ring ID
    t.contingency.d_ring <-data.frame(table(subset(d,ACTION=="B")$BAGUE))
                                        # warning.d_ring:  ring ID ringed at least two times
    warning.d_ring <- as.character(t.contingency.d_ring$Var1[which(t.contingency.d_ring$Freq > 1)])
                                        # ldr amount of double ringing
    ldr <- length(warning.d_ring)
    if(ldr > 0)
    {
        catlog(c(" !!! WARNING MESSAGE:",ldr,"ring have been ringed at least two time\n"),fileLog)
        catlog(c(" ==> Check WARNING_DATA.csv in output_preparation/ directory !!\n"),fileLog)
        t.warning.d_ring <- data.frame(error = "double baguage", commmentError="",suppression= "ligne",subset(d,BAGUE %in% warning.d_ring,select = selectedColumns))
        t.warning.d_ring <- t.warning.d_ring[order(t.warning.d_ring$BAGUE,t.warning.d_ring$DATE),]

        if (file.exists("output_preparation/WARNING_DATA.csv"))
            write.table(t.warning.d_ring,"output_preparation/WARNING_DATA.csv",
                        row.names=FALSE,sep=";",quote=FALSE,append=TRUE,
                        col.names=FALSE) else
                                             write.table(t.warning.d_ring,"output_preparation/WARNING_DATA.csv",
                                                         row.names=FALSE,sep=";",quote=FALSE,append=FALSE)


        d <- subset(d,BAGUE %nin% warning.d_ring)
        catlog(c("    => Current number of lines: ",nrow(d),"\n"),fileLog)

    } else  {
        catlog(c(" --> OK\n"),fileLog)
    }

## dd <- d

### age managing
    catlog(c("\n====================================\n\n - Checking: Age\n------------------------------------\n"),fileLog)
                                        # d$AGE correcting
    d$AGE <- ifelse(is.na(d$AGE),"VOL",ifelse(d$AGE %in% c("?","??","???","VOL"),"VOL",d$AGE))
                                        # line.juv line numbers of juveniles
    line.juv <-  union(setdiff(grep("1",d$AGE),grep("+",d$AGE,fixed="TRUE")),grep("PUL",d$AGE))
                                        # bague_year.juv vector of identifiant bague year of juveniles
    bague_year.juv <- unique(paste(d$BAGUE,d$YEAR)[line.juv])
                                        # line.ad line number of adules
    line.ad <-  union(grep("+",d$AGE,fixed=TRUE),grep("[2-9]",d$AGE,perl =TRUE))
                                        # bague_year.ad vector of identifiant bague year of adultes
    bague_year.ad <- unique(paste(d$BAGUE,d$YEAR)[line.ad])

    catlog(c("    -- add column AGE_stage (JUV, AD, VOL)\n"),fileLog)
    d$AGE_stage <- "VOL"
    d$AGE_stage[paste(d$BAGUE,d$YEAR) %in%  bague_year.juv] <- "JUV"
    d$AGE_stage[paste(d$BAGUE,d$YEAR) %in%  bague_year.ad] <- "AD"

    catlog(c("    -- add column AGE_first (JUV, AD, VOL)\n"),fileLog)
    ringing_year <- aggregate(d$YEAR,by=list(d$BAGUE),min)
    vec_ringing_year <- ringing_year[,2]
    names(vec_ringing_year) <- ringing_year[,1]
    first_catch_date<- aggregate(strptime(paste(d$DATE,d$HEURE),"%Y-%m-%d %H:%M"),by=list(d$BAGUE),min)
    colnames(first_catch_date) <- c("BAGUE","DATE_HEURE")
    table_first_age <- subset(d,paste(BAGUE,strptime(paste(d$DATE,d$HEURE),"%Y-%m-%d %H:%M")) %in% paste(first_catch_date$BAGUE,first_catch_date$DATE_HEURE),
                              select=c("BAGUE","AGE","DATE","HEURE"))
    vec_first_age <- table_first_age$AGE
    names(vec_first_age) <- table_first_age$BAGUE

    vec_first_age <- ifelse(vec_first_age=="VOL","VOL",ifelse(vec_first_age %in% c("PUL","1A","1A?"),"JUV","AD"))

    ## AGE_fist  age construit a partir de la premiere capture
    d$AGE_first <- ifelse(d$YEAR == vec_ringing_year[d$BAGUE],vec_first_age[d$BAGUE],"AD")


 ###################################################################

### Error in sex
    catlog(c("\n====================================\n\n - Checking: Sex\n------------------------------------\n"),fileLog)


    d$SEXE <- ifelse(is.na(d$SEXE),"?",d$SEXE)
    d$SEXE_FIRST <- NA
    d$SEXE_FIRST_INCERTITUDE <- FALSE

   # d.S_ADfm <- d[-(grep("?",d$SEXE,fixed=TRUE)),]
    d.S_ADfm <- subset(d,AGE_first=="AD")

    first_catch_date<- aggregate(strptime(paste(d.S_ADfm$DATE,d.S_ADfm$HEURE),"%Y-%m-%d %H:%M"),by=list(d.S_ADfm$BAGUE),min)
    colnames(first_catch_date) <- c("BAGUE","DATE_HEURE")
    table_first_sex_ad <- subset(d.S_ADfm,paste(BAGUE,strptime(paste(d.S_ADfm$DATE,d.S_ADfm$HEURE),"%Y-%m-%d %H:%M")) %in% paste(first_catch_date$BAGUE,first_catch_date$DATE_HEURE),
                              select=c("BAGUE","SEXE","DATE","HEURE"))

    table_first_sex_ad <- subset(table_first_sex_ad,select=c("BAGUE","SEXE"))
    colnames(table_first_sex_ad)[2] <- "SEXE_FIRST_VOL"

    d <- merge(d,table_first_sex_ad,by="BAGUE",all=TRUE)

    d$SEXE_FIRST[grep("M",d$SEXE_FIRST_VOL,fixed=TRUE)] <- "M"
    d$SEXE_FIRST[grep("F",d$SEXE_FIRST_VOL,fixed=TRUE)] <- "F"
    d$SEXE_FIRST[d$SEXE_FIRST_VOL == "?"] <- "U"
    d$SEXE_FIRST_INCERTITUDE[grep("?",d$SEXE_FIRST_VOL,fixed=TRUE)] <- TRUE

    d <- d[,which(colnames(d)!="SEXE_FIRST_VOL")]

    d.SJ <- subset(d,is.na(SEXE_FIRST))


    d.SJvol <- subset(d.SJ,AGE_first=="VOL")


    first_catch_date<- aggregate(strptime(paste(d.SJvol$DATE,d.SJvol$HEURE),"%Y-%m-%d %H:%M"),by=list(d.SJvol$BAGUE),min)
    colnames(first_catch_date) <- c("BAGUE","DATE_HEURE")
    table_first_sex_ad <- subset(d.SJvol,paste(BAGUE,strptime(paste(d.SJvol$DATE,d.SJvol$HEURE),"%Y-%m-%d %H:%M")) %in% paste(first_catch_date$BAGUE,first_catch_date$DATE_HEURE),
                              select=c("BAGUE","SEXE","DATE","HEURE"))

    table_first_sex_ad <- subset(table_first_sex_ad,select=c("BAGUE","SEXE"))
    colnames(table_first_sex_ad)[2] <- "SEXE_FIRST_VOL"

    d <- merge(d,table_first_sex_ad,by="BAGUE",all=TRUE)

    d$SEXE_FIRST[grep("M",d$SEXE_FIRST_VOL,fixed=TRUE)] <- "M"
    d$SEXE_FIRST[grep("F",d$SEXE_FIRST_VOL,fixed=TRUE)] <- "F"
    d$SEXE_FIRST[d$SEXE_FIRST_VOL == "?"] <- "U"
    d$SEXE_FIRST_INCERTITUDE[grep("?",d$SEXE_FIRST_VOL,fixed=TRUE)] <- TRUE

    d <- d[,which(colnames(d)!="SEXE_FIRST_VOL")]


    d.SJ <- subset(d,is.na(SEXE_FIRST))
    d.SJ_fm <- d.SJ[-(grep("?",d.SJ$SEXE,fixed=TRUE)),]

    u.SJ_fm <- unique(subset(d.SJ_fm,select=c("BAGUE","SEXE")))
    table.SJ_fm <- table(u.SJ_fm$BAGUE)

    table.SJ <- data.frame(BAGUE=names(table.SJ_fm)[table.SJ_fm>1],SEXE_JUV = "?")

    u.SJ_fm <- subset(u.SJ_fm,!(BAGUE %in% names(table.SJ_fm)[table.SJ_fm>1]), select=c("BAGUE","SEXE"))
    colnames(u.SJ_fm)[2] <- "SEXE_JUV"

    table.SJ <- rbind(table.SJ,u.SJ_fm)

    d <- merge(d,table.SJ,by="BAGUE",all=TRUE)

    d$SEXE_FIRST[grep("M",d$SEXE_JUV,fixed=TRUE)] <- "M"
    d$SEXE_FIRST[grep("F",d$SEXE_JUV,fixed=TRUE)] <- "F"
    d$SEXE_FIRST[d$SEXE_JUV == "?"] <- "U"
    d$SEXE_FIRST_INCERTITUDE[grep("?",d$SEXE_JUV,fixed=TRUE)] <- TRUE

    d <- d[,which(colnames(d)!="SEXE_JUV")]




   d.SJ <- subset(d,is.na(SEXE_FIRST) & SEXE != "?")

    u.SJ_fm <- unique(subset(d.SJ_fm,select=c("BAGUE","SEXE")))
    table.SJ_fm <- table(u.SJ_fm$BAGUE)

    u.SJ_fm <- subset(u.SJ_fm,!(BAGUE %in% names(table.SJ_fm)[table.SJ_fm>1]), select=c("BAGUE","SEXE"))
    colnames(u.SJ_fm)[2] <- "SEXE_JUV"

    if(length(names(table.SJ_fm)[table.SJ_fm>1])>0) {
        table.SJ <- data.frame(BAGUE=names(table.SJ_fm)[table.SJ_fm>1],SEXE_JUV = "?")
           table.SJ <- rbind(table.SJ,u.SJ_fm)

    } else {
          table.SJ <- u.SJ_fm
        }


    d <- merge(d,table.SJ,by="BAGUE",all=TRUE)

    d$SEXE_FIRST[grep("M",d$SEXE_JUV,fixed=TRUE)] <- "M"
    d$SEXE_FIRST[grep("F",d$SEXE_JUV,fixed=TRUE)] <- "F"
    d$SEXE_FIRST[d$SEXE_JUV == "?"] <- "U"
    d$SEXE_FIRST_INCERTITUDE[grep("?",d$SEXE_JUV,fixed=TRUE)] <- TRUE

    d <- d[,which(colnames(d)!="SEXE_JUV")]

    d$SEXE_FIRST_INCERTITUDE[is.na(d$SEXE_FIRST)] <- TRUE
    d$SEXE_FIRST[is.na(d$SEXE_FIRST)] <- "U"


### SEXE_UNIQUE autre methode de correction du sex

   d.Sfm <- d[-(grep("?",d$SEXE,fixed=TRUE)),]
    t.contingency.Sfm <- table(d.Sfm$BAGUE,d.Sfm$SEXE)
    t.sex <- data.frame(BAGUE = rownames(t.contingency.Sfm),
                        NEWSEX = colnames(t.contingency.Sfm)[apply(t.contingency.Sfm,1,function(X) match(max(X),X))],
                        SEX.CONFIDENCE = apply(t.contingency.Sfm,1,max)/rowSums(t.contingency.Sfm),
                        NB.DATA.FOR.SEX = rowSums(t.contingency.Sfm),
                        SEX.CONFIDENCE.CAT = "OK",stringsAsFactors=FALSE)
    t.sex$SEX.CONFIDENCE.CAT[t.sex$SEX.CONFIDENCE < 1] <- "PROB"
    d.Si <- subset(d,d$BAGUE %nin% t.sex$BAGUE)
    d.Si <- d.Si[d.Si$SEXE != "?",]
    d.Si$SEXE <- substr(d.Si$SEXE,1,1)
    t.contingency.Si <- table(d.Si$BAGUE,d.Si$SEXE)
    t.sex <- rbind(t.sex,
                   data.frame(BAGUE = rownames(t.contingency.Si),
                              NEWSEX = colnames(t.contingency.Si)[apply(t.contingency.Si,1,function(X) match(max(X),X))],
                              SEX.CONFIDENCE = apply(t.contingency.Si,1,max)/rowSums(t.contingency.Si),
                              NB.DATA.FOR.SEX = rowSums(t.contingency.Si),
                              SEX.CONFIDENCE.CAT = "UNCERTAIN",stringsAsFactors=FALSE))
    t.sex$SEX.CONFIDENCE.CAT[t.sex$SEX.CONFIDENCE < 1 & t.sex$SEX.CONFIDENCE.CAT == "UNCERTAIN"] <- "VERY_UNCERTAIN"

     d.Su <- subset(d,d$BAGUE %nin% t.sex$BAGUE)
    t.contingency.Su <- table(d.Su$BAGUE,d.Su$SEXE)
    t.sex <- rbind(t.sex,
                   data.frame(BAGUE = rownames(t.contingency.Su),
                              NEWSEX = "U",
                              NB.DATA.FOR.SEX = rowSums(t.contingency.Su),
                              SEX.CONFIDENCE = 1,
                              SEX.CONFIDENCE.CAT = "UNKNOWN",stringsAsFactors=FALSE))

    d <- merge(d,t.sex,by="BAGUE")
    lse <- length(which(t.sex$SEX.CONFIDENCE<1))

    if ( lse == 0)
    {
        cat(" --> OK\n")
    } else {
        cat(" !!! WARNING MESSAGE:",
            lse,"induvidual(s) do(es) not have a single value of sex\n",
            cat(" ==> Check warning_sex.csv in output_preparation/ directory !!\n\n"))
        t.warning.sex <- subset(d,BAGUE %in% t.sex$BAGUE[which(t.sex$SEX.CONFIDENCE<1)])
        t.warning.sex <- t.warning.sex[order(t.warning.sex$BAGUE,t.warning.sex$DATE),]
        write.csv2(t.warning.sex,"output_preparation/warning_sex.csv",row.names=FALSE,na="",quote=FALSE)
    }


### news columns SEX.CONFIDENCE.CAT and GROUP
    cat("    -- add column SEX.CONFIDENCE\n")
    cat("    -- add column SEX.CONFIDENCE.CAT (UNKNOWN, PROB, CRED)\n")
                                        #   d <- merge(d,dsp,by="SP")
    cat("    -- add column SEXE2 U if SEX.CONFIDENCE<0.8 \n")
    d$SEXE2 <- ifelse(d$SEX.CONFIDENCE<.8,"U",d$SEXE)





#### Mesure checking
    catlog(c("\n====================================\n\n - Checking: MA\n------------------------------------\n"),fileLog)



                                        # browser()
    ## tMAmean table de reference des quantiles de masse par espece et par age
    tMAmean <- aggregate(MA~SP+AGE_first,data=d,quantile,c(.01,.5,.99))
    tMAmean <- data.frame(tMAmean[,1:2],tMAmean[3][[1]][,1:3])
    colnames(tMAmean)[3:5] <- c("MA_01","MA_mean","MA_99")

    tMAmean$MA_seuilmin <- floor(tMAmean$MA_01 * 0.75)
    tMAmean$MA_seuilmax <- ceiling(tMAmean$MA_99 * 1.25)

     #browser()
    catlog(c("    -- add column MA_01,MA_mean, MA_99,MA_seuilmin ,MA_seuilmax,MA_borne\n"),fileLog)
    d <- merge(d,tMAmean,by=c("SP","AGE_first"))
    d$MA_borne <- ifelse(d$MA > d$MA_seuilmax | d$MA < d$MA_seuilmin ,NA,d$MA)

    t.warning.ma <- subset(d,is.na(d$MA_borne),select = selectedColumns)
    lma <- nrow(t.warning.ma)



    if(lma > 0)
    {
        catlog(c(" !!! WARNING MESSAGE:",lma,"MA seem aberrant\n"),fileLog)
        catlog(c(" ==> Check WARNING_DATA.csv in output_preparation/ directory !!\n"),fileLog)
        t.warning.ma <- data.frame(error = "MA aberrante", commmentError="",suppression= "Valeur ignoree", t.warning.ma)
        t.warning.ma <-  t.warning.ma[order( t.warning.ma$BAGUE, t.warning.ma$DATE),]

        if (file.exists("output_preparation/WARNING_DATA.csv"))
            write.table( t.warning.ma,"output_preparation/WARNING_DATA.csv",
                        row.names=FALSE,sep=";",quote=FALSE,append=TRUE,
                        col.names=FALSE) else
                                             write.table( t.warning.ma,"output_preparation/WARNING_DATA.csv",
                                                         row.names=FALSE,sep=";",quote=FALSE,append=FALSE)



        catlog(c("    => Current number of lines: ",nrow(d),"\n"),fileLog)

    } else  {
        catlog(c(" --> OK\n"),fileLog)
    }

    catlog(c("    -- add column MA_indice_trunc, MA_indice_borne\n"),fileLog)
    d$MA_indice_borne<- (d$MA_borne - d$MA_mean)/d$MA_mean



    catlog(c("\n====================================\n\n - Checking: LP\n------------------------------------\n"),fileLog)
## dd <- d

    ## tLPmean table de reference des quantiles de longueur d'aile par espece et par age
    tLPmean <- aggregate(LP~SP+AGE_first,data=d,quantile,c(.01,.5,.99))
    tLPmean <- data.frame(tLPmean[,1:2],tLPmean[3][[1]][,1:3])
    colnames(tLPmean)[3:5] <- c("LP_01","LP_mean","LP_99")

    tLPmean$LP_seuilmin <- floor(tLPmean$LP_01 * 0.75)
    tLPmean$LP_seuilmax <- ceiling(tLPmean$LP_99 * 1.25)



    catlog(c("    -- add column LP_01, LP_mean, LP_99,LP_seuilmin ,LP_seuilmax, LP_trunc\n"),fileLog)
    d <- merge(d,tLPmean,by=c("SP","AGE_first"))
    d$LP_borne <- ifelse(d$LP > d$LP_seuilmax | d$LP < d$LP_seuilmin ,NA,d$LP)

    t.warning.lp <- subset(d,is.na(d$LP_borne),select = selectedColumns)

    llp <- nrow(t.warning.lp)



    if(llp > 0)
    {
        catlog(c(" !!! WARNING MESSAGE:",llp,"LP seem aberrant\n"),fileLog)
        catlog(c(" ==> Check WARNING_DATA.csv in output_preparation/ directory !!\n"),fileLog)
        t.warning.lp <- data.frame(error = "LP aberrante", commmentError="",suppression= "Valeur tronquée", t.warning.lp)
        t.warning.lp <-  t.warning.ma[order( t.warning.lp$BAGUE, t.warning.lp$DATE),]

        if (file.exists("output_preparation/WARNING_DATA.csv"))
            write.table( t.warning.lp,"output_preparation/WARNING_DATA.csv",
                        row.names=FALSE,sep=";",quote=FALSE,append=TRUE,
                        col.names=FALSE) else
                                             write.table( t.warning.lp,"output_preparation/WARNING_DATA.csv",
                                                         row.names=FALSE,sep=";",quote=FALSE,append=FALSE)



        catlog(c("    => Current number of lines: ",nrow(d),"\n"),fileLog)

    } else  {
        catlog(c(" --> OK\n"),fileLog)
    }



    catlog(c("    -- add column LP_year_borne\n"),fileLog)
    tLPyear_borne <- aggregate(LP_borne~BAGUE+YEAR,data=d,median)
    tLPyear_borne<- data.frame(tLPyear_borne[,1:2],tLPyear_borne[3][[1]])
    colnames(tLPyear_borne)[3] <- c("LP_year_borne")
    d <- merge(d,tLPyear_borne,by=c("BAGUE","YEAR"),all=TRUE)


    catlog(c("    -- add column LP_indice_borne, LP_indice_trunc\n"),fileLog)
    d$LP_indice_borne<- (d$LP_year_borne- d$LP_mean)/d$LP_mean + 1






    d <- d[order(d$DATE,d$NEW.ID_PROG,d$HEURE),]


    d$BG <- ifelse(is.na(d$BG),d$BAGUEUR,d$BG)
    d$BG <- ifelse(is.null(d$BG),d$BAGUEUR,d$BG)
      d$BG <- ifelse(d$BG=="",d$BAGUEUR,d$BG)

   # write.csv2(d,"dataCeline_2017.csv",row.names=FALSE)

    catlog(c("\n====================================\n\n - Ajout de colonnes:\n------------------------------------\n"),fileLog)
                                        #browser()
    catlog(c("    -> HABITAT (STOC-rozo, Aquatique, Terrestre)\n"),fileLog)

    d$HABITAT <- NA

    d$HABITAT[grep("ROZO",paste(d$THEME,d$THEME.SESSION))] <- "STOC-rozo"

    du <- unique(subset(d,select=c(NEW.ID_PROG,BAGUE,HABITAT_SP)))

    t.contingency.habitat <- table(du$NEW.ID_PROG,du$HABITAT_SP)

    t.palu <- data.frame(NEW.ID_PROG=rownames(t.contingency.habitat),
                         Aquatique = (t.contingency.habitat[,"Aquatique"]/rowSums(t.contingency.habitat)) > .5)

    d <- merge(d,t.palu,by="NEW.ID_PROG")
    d$HABITAT <- ifelse(is.na(d$HABITAT),ifelse(d$Aquatique,"Aquatique","Terrestre"),d$HABITAT)

    catlog(c("      -> Exclusion des data 'Aquatique' avant 2000 \n"),fileLog)
    d <- d[-which(d$HABITAT == "Aquatique" & d$YEAR < 2000),]
    catlog(c("    => Current number of lines: ",nrow(d),"\n"),fileLog)


### exclusion of several mist nets

    catlog(c("\n - Correction: deletion of mist nets \n------------------------------------\n    informations in 'library/excluded_nets.csv' \n"),fileLog)

    # fichier exclusion de filet issu des questionnaires de Manon
    dNets <- read.csv("library/excluded_nets.csv")

    for(i in 1: nrow(dNets)) {
        d$NF[d$ID_PROG==dNets$ID_PROG[i] & d$NF >= dNets$FIRST.NF] <- -9999
        d$NF[d$ID_PROG==dNets$ID_PROG[i] & d$NF <= dNets$LAST.NF] <- 9999
    }

##### mettre les lignes exlue dans le table de WARNINGS

    catlog(c(" !!! WARNING MESSAGE:",length(which(d$NF==-9999))," line(s) deleted\n"),fileLog)


    d <- subset(d,NF != -9999)
    catlog(c("    => Current number of lines: ",nrow(d),"\n"),fileLog)


### first and last year per NEW.ID_PROG
    catlog(c("\n====================================\n\n - Ajout de colonnes:\n------------------------------------\n"),fileLog)
    catlog(c("    -> FIRST.YEAR\n"),fileLog)
    catlog(c("    -> LAST.YEAR\n"),fileLog)
    catlog(c("    -> NB.YEAR pour le nombre d annees du programme\n"),fileLog)

    t.firstLastYear <- data.frame(NEW.ID_PROG = names(tapply(d$YEAR,factor(d$NEW.ID_PROG),min)),
                                  FIRST.YEAR=tapply(d$YEAR,factor(d$NEW.ID_PROG),min),
                                  LAST.YEAR=tapply(d$YEAR,factor(d$NEW.ID_PROG),max))
    d <- merge(d,t.firstLastYear,by="NEW.ID_PROG")
    d$NB.YEARS <-  d$LAST.YEAR - d$FIRST.YEAR  + 1

### at least 2 consecutive years
    catlog(c("\n====================================\n\n - Selection des programmes de au moins 2 ans: \n------------------------------------\n"),fileLog)

    ## dd <- d


    station1year <- subset(unique(subset(d,select=c(NEW.ID_PROG,FIRST.YEAR,YEAR,NB.YEARS))),NB.YEARS==1)
    l1y <- nrow(station1year)
     if(l1y>0)
    {
        catlog(c(" !!! WARNING MESSAGE:",l1y," session with only 1 year\n",
            "These data are exclude\n ==> ",
            "Check WARNING_station1year.csv in output_preparation/ dirctory  !!\n"),fileLog)
        write.csv2(station1year,"output_preparation/WARNING_sation1year.csv",row.names=FALSE)

        d <- subset(d,NB.YEARS > 1)
        catlog(c("    => Current number of lines: ",nrow(d),"\n"),fileLog)


    } else {
        catlog(c(" --> OK\n"),fileLog)
    }




### curious ID number of mist nets
    catlog(c("\n====================================\n\n - Checking: ID number of mist nets \n------------------------------------\n"),fileLog)
    t.nets <- data.frame(error = "NF",commmentError="",suppression= "No",subset(d,NF>100,select = selectedColumns))
    ln <- nrow(t.nets)
    if(ln>0)
    {
        catlog(c(" !!! WARNING MESSAGE:",ln," number of mist nets are strange\n",
            "These data are not exclude\n ==> ",
            "Check WARNING_DATA.csv in output_preparation/ dirctory  error category : 'NF'!!\n"),fileLog)
        catlog(c("    => Current number of lines: ",nrow(d),"\n"),fileLog)

        if (file.exists("output_preparation/WARNING_DATA.csv"))
            write.table(t.nets,"output_preparation/WARNING_DATA.csv",
                        row.names=FALSE,sep=";",quote=FALSE,append=TRUE,
                        col.names=FALSE) else
                                             write.table(t.nets,"output_preparation/WARNING_DATA.csv",
                                                         row.names=FALSE,sep=";",quote=FALSE,append=FALSE)


    } else {
        catlog(c(" --> OK\n"),fileLog)
    }

##dd <- d

### curious ID number of mist nets
    catlog(c("\n====================================\n\n - Estimation de FS.DEDUIT \n------------------------------------\n"),fileLog)
    catlog(c(" -> Adding new columns\n"),fileLog)
    catlog(c("    --> NB.NF mist net number assessed in data\n"),fileLog)
    du.nf <- unique(subset(d,select=c("NEW.ID_PROG","YEAR","NF")))
    t.contingency.nf <-as.data.frame(table(du.nf$NEW.ID_PROG,du.nf$YEAR))
    colnames(t.contingency.nf) <- c("NEW.ID_PROG","YEAR","NB.NF")
    d <- merge(d,t.contingency.nf,by=c("NEW.ID_PROG","YEAR"))
    catlog(c("    --> FS.DEDUIT mist net length assessed in data\n"),fileLog)
    d$FS.DEDUIT <- d$NB.NF * 12
    d$FS.DEDUIT <- ifelse(d$HABITAT == "STOC-rozo",120,d$FS.DEDUIT)



    catlog(c("\n====================================\n\n - Checking: FS \n------------------------------------\n"),fileLog)

    dnf <- subset(d,select=c("NEW.ID_PROG","YEAR","DATE","FS","FS.DEDUIT"))
    dnf <- na.omit(dnf)
    dnf <- unique(dnf)
    dnf$DIFF <- dnf$FS.DEDUIT - dnf$FS

    dnf.error <- dnf[which((dnf$FS<120 & dnf$DIFF >=36)|dnf$FS>500),]
    lnferror <- nrow(dnf.error)
    if(lnferror>0){
                                        #      for(i in 1:lnferror)
                                        #          if(i == 1) d.warning.nf <- d[d$NEW.ID_PROG == dnf.error$NEW.ID_PROG[i] & d$YEAR == dnf.error$YEAR[i] & d$FS == dnf.error$FS[i],] else d.warning.nf <- rbind(d.warning.nf,d[d$NEW.ID_PROG == dnf.error$NEW.ID_PROG[i] & d$YEAR == dnf.error$YEAR[i] & d$FS == dnf.error$FS[i],])

        d.warning.nf <- d[which(paste(d$NEW.ID_PROG,d$DATE) %in% paste(dnf.error$NEW.ID_PROG,dnf.error$DATE)),]
        d.warning.nf <- data.frame(error = "FS ERROR",commmentError=paste("FS.DEDUIT =",d.warning.nf$FS.DEDUIT),suppression= "Value",subset(d.warning.nf,select=selectedColumns))



        catlog(c(" !!! WARNING MESSAGE:",lnferror," value of FS are certainly errors\n",
            "These data are replaced by NA \n ==> ",
            "Check warning_FS_ERROR.csv in output_preparation/ dirctory !!\n",
            "This corresponds to",nrow(d.warning.nf),"lines in the database\n",
            "Check WARNING_DATA.csv in output_preparation/ dirctory  error category : 'FS ERROR'!!\n\n"),fileLog)

        write.csv2(dnf.error,"output_preparation/warning_FS_error.csv",
                   row.names=FALSE,na="",quote=FALSE)

        if (file.exists("output_preparation/WARNING_DATA.csv"))
            write.table(d.warning.nf,"output_preparation/WARNING_DATA.csv",
                        row.names=FALSE,sep=";",quote=FALSE,append=TRUE,
                        col.names=FALSE) else
                                             write.table(d.warning.nf,"output_preparation/WARNING_DATA.csv",
                                                         row.names=FALSE,sep=";",quote=FALSE,append=FALSE)



        d[d$cId_Data %in% d.warning.nf$cId_Data,"FS"] <- NA
    }

    ## netoyage +  message erreur + sortie erreur
    dnf <- subset(d,select=c("NEW.ID_PROG","YEAR","DATE","FS","FS.DEDUIT"))
    dnf <- na.omit(dnf)
    dnf <- unique(dnf)
    dnf$DIFF <- dnf$FS.DEDUIT - dnf$FS
    dnf.strange <- dnf[abs(dnf$DIFF)>=36,]
    ## avertissement + sortie avertissement

    lnferror <- nrow(dnf.strange)
    if(lnferror>0){

        d.warning.nf <- d[which(paste(d$NEW.ID_PROG,d$DATE) %in% paste(dnf.strange$NEW.ID_PROG,dnf.strange$DATE)),]

                                        #     for(i in 1:lnferror)
                                        #         if(i == 1) d.warning.nf <- d[d$NEW.ID_PROG == dnf.strange$NEW.ID_PROG[i] & d$YEAR == dnf.strange$YEAR[i] & d$FS == dnf.strange$FS[i],] else d.warning.nf <- rbind(d.warning.nf,d[d$NEW.ID_PROG == dnf.strange$NEW.ID_PROG[i] & d$YEAR == dnf.strange$YEAR[i] & d$FS == dnf.strange$FS[i],])

        d.warning.nf <- data.frame(error ="FS STRANGE",commmentError=paste("FS.DEDUIT =",d.warning.nf$FS.DEDUIT),suppression= "No",subset(d.warning.nf,select=selectedColumns))


        catlog(c(" !!! WARNING MESSAGE:",lnferror," value of FS are strange because very lower than FS.DEDUIT\n",
            "These data are not replaced \n ==> ",
            "Check warning_FS_STRANGE.csv in output_preparation/ dirctory !!\n",
            "This corresponds to",nrow(d.warning.nf),"lines in the database\n",
            "Check WARNING_DATA.csv in output_preparation/ dirctory  error category : 'FS STRANGE'!!\n"),fileLog)

        write.csv2(dnf.strange,"output_preparation/warning_FS_strange.csv",
                   row.names=FALSE,na="",quote=FALSE)


        if (file.exists("output_preparation/WARNING_DATA.csv"))
            write.table(d.warning.nf,"output_preparation/WARNING_DATA.csv",
                        row.names=FALSE,sep=";",quote=FALSE,append=TRUE,
                        col.names=FALSE) else
                                             write.table(d.warning.nf,"output_preparation/WARNING_DATA.csv",
                                                         row.names=FALSE,sep=";",quote=FALSE,append=FALSE)


    }

    dnf <- subset(d,select=c("NEW.ID_PROG","YEAR","FS","FS.DEDUIT","HABITAT"))
    dnf <- na.omit(dnf)
    dnf <- unique(dnf)
    dnf$DIFF <- dnf$FS.DEDUIT - dnf$FS
   # browser()
    m <- lm(FS.DEDUIT ~ FS, dnf)
    a = format(coef(m)[1], digits = 2)
    b = format(coef(m)[2], digits = 2)
    r = format(summary(m)$r.squared, digits = 3)
    text.equa <- paste("FS.DEDUIT = ",a," + ",b," FS\nr^2 = ",r,sep="")

    ggnf1 <- ggplot(dnf,aes(x=FS,y=FS.DEDUIT,colour=as.factor(YEAR)))+geom_abline(slope=1,intercept = 0,colour="gray")+ coord_fixed(xlim=c(0,400),ylim=c(0,400),ratio=1)
    ggnf1 <- ggnf1 + geom_smooth(method="lm",colour="red")+ annotate("text", label = text.equa,x=80,y=400,size=3,colour="red")+ geom_point()
    ggnf1 <- ggnf1 + labs(list(title="Estimation de FS a partir des toutes les données",colour="Années"))

    ggfile <- paste("output_preparation/estimationFS_all.png",sep="")
    catlog(c("Check",ggfile,":"),fileLog)
    ggsave(ggfile,ggnf1)
    catlog(c("\n"),fileLog)


    ggnf2 <- ggplot(dnf,aes(x=DIFF)) + geom_histogram(binwidth = 12) + labs(list(title="Distribution de FS.DEDUIT - FS pour toute les données",x="FS.DEDUIT - FS"))

    ggfile <- paste("output_preparation/estimationFS_histograme_all.png",sep="")
    catlog(c("Check",ggfile,":"),fileLog)
    ggsave(ggfile,ggnf2)
    catlog(c("\n"),fileLog)

    d$FS.OUTPUT_PREPARATION <- ifelse(d$YEAR<=2010,d$FS.DEDUIT,ifelse(is.na(d$FS),d$FS.DEDUIT,d$FS))



    write.csv2(d,paste("output_preparation/",fileDataClean,sep=""),row.names=FALSE,na="",quote=FALSE)
    catlog(paste("\n====================================\n\nDATA exported :output_preparation/",fileDataClean,"\n",sep=""),fileLog)
    catlog(c("\n\n    => Final number of lines: ",nrow(d),"\n"),fileLog)

    return(d)


}

##' Selection des 3 sessions
##'
##' .. content for \details{} ..
##' @title
##' @param d
##' @param fileDataClean
##' @param fileLog
##' @param seuilAvorteDuree
##' @param seuilAvorteEvenement
##' @param seuilExclusionDelai
##' @param dateRefDefaut
##' @return
##' @author Romain Lorrilliere
select3sessions <- function(d,fileDataClean="data3session.csv",fileLog="log.txt",
                   seuilAvorteDuree= 3, seuilAvorteEvenement=4,seuilExclusionDelai = 10,dateRefDefaut =c(138,165,188)) {

    ## fileDataClean="data3session.csv";fileLog="log.txt"
    ## seuilAvorteDuree= 3; seuilAvorteEvenement=4;seuilExclusionDelai = 10;dateRefDefaut =c(138,165,188)

    selectedColumns <- c("ACTION","CENTRE","BAGUE","DATE","YEAR","MONTH","JULIANDAY","HEURE","H",
                         "ESPECE","SP","SEXE","AGE","DEPT","LOCALITE",
                         "LIEUDIT","LP","MA","THEME.SESSION","THEME","BAGUEUR","BG",
                         "COND.REPR","CIRC.REPR","NF","PC","PI","ID_PROG","NEW.ID_PROG",
                         "FS","HS","DS","cId_Data","LON","LAT",
                         "PRECISION_LOC","ALTITUDE","COMMUNES","REGION","BIOGEO")


### new column NB.SESSION of number of session during the year per place
### and select NB.SESSION > 2


    catlog(c("\n====================================\n\n - Checking: nubmer of sessions:\n------------------------------------\n"),fileLog)

    du <- unique(subset(d,select=c("NEW.ID_PROG","YEAR","DATE")))
    t.nbSession <- data.frame(table(du$NEW.ID_PROG,du$YEAR))
    colnames(t.nbSession) <- c("NEW.ID_PROG","YEAR","NB.SESSION")
    t.nbSession <- subset(t.nbSession,NB.SESSION > 0)
    t.nbSession <- t.nbSession[order(t.nbSession$NEW.ID_PROG,t.nbSession$YEAR),]


    d <- merge(d,t.nbSession,by=c("NEW.ID_PROG","YEAR"),all.x = TRUE)

    altitude <- unique(subset(d,select=c("NEW.ID_PROG","ALTITUDE","BIOGEO")))
    altitude$BIOGEO[is.na(altitude$BIOGEO)] <- ""
    rownames(altitude) <- altitude$NEW.ID_PROG
    altitude$ALTITUDE[is.na(altitude$ALTITUDE)] <- 0
    altitude$BIOGEO <- as.character(altitude$BIOGEO)

    catlog(c("\n - Selection des donnees issue d annee avec au moins 3 sessions dans un programme: \n"),fileLog)



    ## dd <- d

   #


    t.nbSessionMostFreq <-  aggregate(as.numeric(t.nbSession$NB.SESSION),
                                      list(t.nbSession$NEW.ID_PROG),
                                      get_mode)

    colnames(t.nbSessionMostFreq) <- c("NEW.ID_PROG",
                                       "NB.SESSION.MAX_MOST.FREQUENT")

    t.nbSession <- merge(t.nbSession,t.nbSessionMostFreq,by="NEW.ID_PROG")

    t.nbSession$DIFF.NB.SESSION <- t.nbSession$NB.SESSION - t.nbSession$NB.SESSION.MAX_MOST.FREQUENT


    write.csv2(t.nbSession,"output_preparation/nbSessionYear.csv",row.names=FALSE)


    t.warning.session <- subset(t.nbSession,abs(DIFF.NB.SESSION)>1.5)

    lpbs <- length(unique(t.warning.session$NEW.ID_PROG))
    if(lpbs > 0)
    {
        catlog(c("\n !!! WARNING MESSAGE:",
            lpbs,
            "site have a very variable number of sessions\n",
            "These data are not exclude\n ==> ",
            "Check warning_nbSession.csv in output_preparation/ dirctory !!\n"),fileLog)
        t.warning.session <- t.warning.session[order(
            t.warning.session$NEW.ID_PROG,t.warning.session$YEAR),]
        write.csv2(t.warning.session,"output_preparation/warning_nbSession.csv",
                   row.names=FALSE,na="",quote=FALSE)
        catlog(c("    => Current number of lines: ",nrow(d),"\n"),fileLog)

    }else {
        catlog(c(" --> OK\n"),fileLog)
    }

    catlog(c("\n====================================\n\n - Fiche sessions par station:\n------------------------------------\n"),fileLog)

## dd <- d

    #browser()

    agg1 <- aggregate(BAGUE~NEW.ID_PROG+YEAR + JULIANDAY,data=d,length)
    colnames(agg1)[4] <- "nb_evenement"
    agg2 <- aggregate(HS~NEW.ID_PROG+YEAR + JULIANDAY,data=subset(d,!is.na(HS)), get_mode_max)
    agg3 <- aggregate(DS~NEW.ID_PROG+YEAR + JULIANDAY,data=subset(d,!is.na(HS)), get_mode_max)
    agg4 <- aggregate(HEURE~NEW.ID_PROG+YEAR + JULIANDAY,data=d,FUN = function(x) format(min(strptime(x, format="%H:%M")),"%H:%M"))
    colnames(agg4)[4] <- "HEURE_MIN"
    agg5 <- aggregate(HEURE~NEW.ID_PROG+YEAR + JULIANDAY,data=d,FUN = function(x) format(max(strptime(x, format="%H:%M")),"%H:%M"))
    colnames(agg5)[4] <- "HEURE_MAX"

    aggSession <- merge(
        merge(
            merge(
                merge(agg1,agg2,by=c("NEW.ID_PROG","YEAR","JULIANDAY"),all=TRUE),
                agg3,by=c("NEW.ID_PROG","YEAR","JULIANDAY"),all=TRUE),
            agg4,by=c("NEW.ID_PROG","YEAR","JULIANDAY"),all=TRUE),
        agg5,by=c("NEW.ID_PROG","YEAR","JULIANDAY"),all=TRUE)

    aggSession <- aggSession[order(aggSession$NEW.ID_PROG,aggSession$YEAR,aggSession$JULIANDAY),]
    aggSession$DUREE <- round(as.numeric(difftime(strptime(aggSession$HEURE_MAX,format="%H:%M"),strptime(aggSession$HEURE_MIN,format="%H:%M"),units="hours")),2)
    DSnum <-as.numeric(difftime(strptime(aggSession$DS,format="%H:%M"),strptime("00:00",format="%H:%M"),units="hours"))
    aggSession$DUREE_CONSIDEREE<- round(ifelse(is.na(aggSession$DS),aggSession$DUREE,ifelse(DSnum<aggSession$DUREE,aggSession$DUREE,DSnum)),2)
    aggSession$VALIDE <- TRUE
    aggSession$VALIDE[aggSession$DUREE_CONSIDEREE < seuilAvorteDuree  | aggSession$nb_evenement < seuilAvorteEvenement] <- FALSE
    aggSession$VALIDE[aggSession$DS > seuilAvorteDuree] <- TRUE
#### Ajouter ERREUR quand DSnum < DUREE

    t.nbSessionMostFreq$TYPE <- ifelse(t.nbSessionMostFreq$NB.SESSION.MAX_MOST.FREQUENT == 4,4,3)

    stations <- sort(unique(d$NEW.ID_PROG))

    tDateRef <-data.frame(matrix(ncol = 3,nrow=length(stations)))
    colnames(tDateRef) <- paste("SESSION",1:3,sep="_")
    rownames(tDateRef) <- stations


   catlog(" 1) Calcul des dates de reference pour les stations à 3 et plus de 4 sessions\n",fileLog)

    t.nbSession3 <- subset(t.nbSessionMostFreq,TYPE==3)
    t.nbSessionYear3 <- subset(t.nbSession,NEW.ID_PROG %in% t.nbSession3$NEW.ID_PROG)

    aggSession3 <- subset(aggSession,NEW.ID_PROG %in% t.nbSession3$NEW.ID_PROG)
    aggSession3$SESSION <- NA

    t.nbSessionYear3.1 <- subset(t.nbSessionYear3,NB.SESSION==3)
    session3.conserve <- subset(aggSession3,paste(NEW.ID_PROG,YEAR) %in% paste(t.nbSessionYear3.1$NEW.ID_PROG,t.nbSessionYear3.1$YEAR))
    t.nbSessionYear3.1 <- subset(t.nbSessionYear3,NB.SESSION>3)
    session3.sup <- subset(aggSession3,paste(NEW.ID_PROG,YEAR) %in% paste(t.nbSessionYear3.1$NEW.ID_PROG,t.nbSessionYear3.1$YEAR))



    stations3 <- sort(as.character(unique(t.nbSession3$NEW.ID_PROG)))

    for(ss in stations3) {
        cat(ss,"|")
      #  browser()
        session3.conserve.ss <- subset(session3.conserve,NEW.ID_PROG == ss)
        ## calcul des dates de references pour la station
        if(nrow(session3.conserve.ss) > 0) {
            if(nrow(session3.conserve.ss) == 3) {
                tDateRef[ss,] <- sort(session3.conserve.ss$JULIANDAY)
            } else {
                tDateRef[ss,] <- round(colMeans(aggregate(JULIANDAY ~ NEW.ID_PROG + YEAR,
                                           session3.conserve.ss,
                                           FUN = function(X) c(min(X),X[X != min(X) & X != max(X)],max(X)))[3][[1]][,1:3]))
            }
        } else {
            ## si pas d annee a 3 session on utilise les date de references national
            tDateRef[ss,] <- dateRefDefaut
           # browser()
            if(ss %in% rownames(altitude)) # decalage des date pour les station mediteraneennne et d altitude
                if(altitude[ss,"ALTITUDE"]>=800 | altitude[ss,"BIOGEO"]=="mediterraneen"| altitude[ss,"BIOGEO"]=="alpin") tDateRef[ss,] <- tDateRef[ss,] + 15
        }

    }

cat("\n")

    catlog(" 2)  Calcul des dates de reference pour les stations à 4 sessions\n",fileLog)

    t.nbSession4 <- subset(t.nbSessionMostFreq,TYPE==4)
    t.nbSessionYear4 <- subset(t.nbSession,NEW.ID_PROG %in% t.nbSession4$NEW.ID_PROG)

    aggSession4 <- subset(aggSession,NEW.ID_PROG %in% t.nbSession4$NEW.ID_PROG)
    aggSession4$SESSION <- NA

    t.nbSessionYear4.1 <- subset(t.nbSessionYear4,NB.SESSION==4)
    session4.conserve <- subset(aggSession4,paste(NEW.ID_PROG,YEAR) %in% paste(t.nbSessionYear4.1$NEW.ID_PROG,t.nbSessionYear4.1$YEAR))

    stations4 <- as.character(sort(unique(t.nbSession4$NEW.ID_PROG)))


    for(ss in stations4) {
        session4.conserve.ss <- subset(session4.conserve,NEW.ID_PROG == ss)
        mJulian <- matrix(session4.conserve.ss$JULIANDAY,ncol=4,byrow = TRUE)
        mmJulian <- round(colMeans(mJulian))
        julianRef <- dateRefDefaut
         if(ss %in% rownames(altitude) )# decalage des date pour les station mediteraneennne et d altitude
             if(altitude[ss,"ALTITUDE"]>=800 | altitude[ss,"BIOGEO"]=="mediterraneen"| altitude[ss,"BIOGEO"]=="alpin") julianRef <- julianRef + 15
    ## calcul des dates de references pour la station
        opt <- as.matrix(rbind(rbind(mmJulian[1:3], mmJulian[-1]),rbind(mmJulian[c(1,2,4)],mmJulian[c(1,3,4)])))

       tDateRef[ss,] <- opt[which.min(rowSums(abs(opt-matrix(julianRef,nrow=4,ncol=3,byrow=TRUE)))+c(5,0,0,0)),]
    }




    catlog(c("\n====================================\n\n - the 3 sessions assigning:\n------------------------------------\n"),fileLog)

    session.select <- data.frame(unique(subset(d,select=c("NEW.ID_PROG","YEAR"))),SESSION_1=NA,SESSION_2=NA,SESSION_3=NA)
    rownames(session.select) <- paste(session.select$NEW.ID_PROG,session.select$YEAR,sep="_")


    aggSession3 <- subset(aggSession,NEW.ID_PROG %in% t.nbSession3$NEW.ID_PROG)
    aggSession3$SESSION <- NA

    t.nbSessionYear3 <- subset(t.nbSession,NB.SESSION==3)
    session.conserve <- subset(aggSession,paste(NEW.ID_PROG,YEAR) %in% paste(t.nbSessionYear3$NEW.ID_PROG,t.nbSessionYear3$YEAR))
    t.nbSessionYearSup <- subset(t.nbSession,NB.SESSION>3)
    session.sup <- subset(aggSession,paste(NEW.ID_PROG,YEAR) %in% paste(t.nbSessionYearSup$NEW.ID_PROG,t.nbSessionYearSup$YEAR))


    for(ss in stations) {
       ## cat(ss,"")
        dateRef <- tDateRef[ss,]
        nbsession.ss <- subset(t.nbSession, NEW.ID_PROG == ss)
        if(nrow(nbsession.ss)>0) {

            years <- as.numeric(as.character(unique(nbsession.ss[nbsession.ss$NB.SESSION>2,]$YEAR)))
            for(y in years) {
#cat(y,"")
                                        # browser()
                tabSessionY <- aggSession[aggSession$YEAR == y & aggSession$NEW.ID_PROG == ss,]
                julianSession <-  NA
                for(i in 1:3)
                {
                                        #cat(i)

                    julianSSY <- tabSessionY$JULIANDAY[abs(tabSessionY$JULIANDAY - tDateRef[ss,i]) < 16 & tabSessionY$VALIDE]
                    if(length(julianSSY)>0) {
                        otherJulianSSY <- tabSessionY$JULIANDAY[tabSessionY$JULIANDAY>max(julianSSY)]
                        if(i<3 & length(otherJulianSSY)< (3-i)) julianSSY <- tabSessionY$JULIANDAY[abs(tabSessionY$JULIANDAY - tDateRef[ss,i]) < 16]
                        if(i>1 & (!is.na(julianSession))) julianSSY <- julianSSY[julianSSY > (julianSession + 6)]
                    }

                    ## si au moins une session valide
                    if(length(julianSSY)>0) {
                        diffJulian <- abs(julianSSY - tDateRef[ss,i])
                        julianSession <- julianSSY[which(diffJulian==min(diffJulian))]
                        ## si deux dates optimales
                        if(length(julianSession)>1)
                            julianSession <- subset(tabSessionY,JULIANDAY %in% julianSession)[order(subset(tabSessionY,JULIANDAY %in% julianSession)$nb_evenement,decreasing=TRUE),"JULIANDAY"][1]
                    } else {
                        ## si pas de session valide
                        julianSSY <- tabSessionY$JULIANDAY[abs(tabSessionY$JULIANDAY - tDateRef[ss,i]) < 16]
                        if(i>1 & (!is.na(julianSession))) julianSSY <- julianSSY[julianSSY > (julianSession + 6)]
                        if(length(julianSSY)>0) {
                            diffJulian <- abs(julianSSY - tDateRef[ss,i])
                            julianSession <- julianSSY[which(diffJulian==min(diffJulian))]
                            ## si deux dates optimales
                            if(length(julianSession)>1)
                                julianSession <- subset(tabSessionY,JULIANDAY %in% julianSession)[order(subset(tabSessionY,JULIANDAY %in% julianSession)$nb_evenement,decreasing=TRUE),"JULIANDAY"][1]

                        } else {
                            julianSession <-  NA
                        }
                    }
                    session.select[session.select$NEW.ID_PROG == ss & session.select$YEAR == y,2+i] <- julianSession
                } # for(i in 1:3)
            } # for(y in years)
        } # if(nrow(nbsession.ss)>0)
    } # for(ss in stations)

    session.select[apply(session.select[,3:5],1,FUN = function(X) any(is.na(X))),3:5] <- NA



        library(reshape)

    ggSession <- melt(session.select,id=c("NEW.ID_PROG","YEAR"))
    colnames(ggSession)[3:4] <- c("SESSION","JULIANDAY")
    ggSession <- na.omit(ggSession)
    ggSession$SESSION <- substr(ggSession$SESSION,9,10)
    aggSession <- merge(aggSession,ggSession,by=c("NEW.ID_PROG","YEAR","JULIANDAY"),all=TRUE)

    aggSession$SESSION[is.na(aggSession$SESSION)] <- 0


    aggSessionRef <- melt(data.frame(NEW.ID_PROG=row.names(tDateRef),tDateRef),id="NEW.ID_PROG")
    colnames(aggSessionRef)[2:3] <- c("SESSION","JULIANDAY")
 aggSessionRef$SESSION <- substr(aggSessionRef$SESSION,9,10)

    aggAllSession <- aggregate(JULIANDAY ~ (YEAR + SESSION), data= ggSession, quantile,c(0.025,0.25,0.5,0.75,0.975))
    aggAllSession <- data.frame(aggAllSession[,1:2],aggAllSession[3][[1]][,1:5])
    colnames(aggAllSession)[3:7] <- c("CIinf","CIquart_inf","med","CIquart_sup","CIsup")

    catlog(c(" --> OK\n"),fileLog)
    fileSave <- "data_France/historicSession.csv"
    catlog(c("\n - Historic of session saved in file :\n "),fileLog)
    write.csv2(aggSession,fileSave,row.names=FALSE,na="",quote=FALSE)
    catlog(c("\n   --> ",fileSave,"\n"),fileLog)

    catlog(c(" --> OK\n"),fileLog)
    fileSave <- "data_France/SessionReference.csv"
    catlog(c("\n - Historic of session saved in file :\n "),fileLog)
    write.csv2(aggSessionRef,fileSave,row.names=FALSE,na="",quote=FALSE)
    catlog(c("\n   --> ",fileSave,"\n"),fileLog)


    fileSave <- "data_France/summarySession.csv"
    catlog(c("\n - Summary session historic saved in file :\n "),fileLog)
    write.csv2(aggAllSession,fileSave,row.names=FALSE,na="",quote=FALSE)
    catlog(c("\n   --> ",fileSave,"\n"),fileLog)




   catlog(c("\n====================================\n\n - the 3 sessions selection\n------------------------------------\n"),fileLog)

    tabSession <- subset(aggSession,select=c("NEW.ID_PROG","YEAR","JULIANDAY","SESSION"))

    d <- merge(d,tabSession,by=c("NEW.ID_PROG","YEAR","JULIANDAY"))

      de <- subset(d,SESSION == 0)
    if(nrow(de) > 0) {
     de.warning <- data.frame(error = "SESSION",commmentError="non selectionnee",suppression= "ligne", subset(de,select = selectedColumns))

        lwiNA <- nrow(de.warning)

        catlog(c("\n !!! WARNING MESSAGE:",lwiNA," lines excluded\n",
            "Check WARNING_DATA.csv in output_preparation/ dirctory error category : 'SESSION - not selected'!!\n"),fileLog)
        if (file.exists("output_preparation/WARNING_DATA.csv"))
            write.table(de.warning,"output_preparation/WARNING_DATA.csv",
                        row.names=FALSE,sep=";",quote=FALSE,append=TRUE,
                        col.names=FALSE) else
                                             write.table(de.warning,"output_preparation/WARNING_DATA.csv",
                                                         row.names=FALSE,sep=";",quote=FALSE,append=FALSE)

    }


    d <- subset(d,SESSION != 0)

    catlog(c(" --> OK\n"),fileLog)
    catlog(c("    => Current number of lines: ",nrow(d),"\n"),fileLog)




   catlog(c("\n====================================\n\n - Station with at least 3 year\n------------------------------------\n"),fileLog)


       de <- subset(d,NB.YEARS < 3 )
    if(nrow(de) > 0) {
     de.warning <- data.frame(error = "NB.YEARs",commmentError="moins de 3 annees",suppression= "ligne", subset(de,select = selectedColumns))

        lwiNA <- nrow(de.warning)

        catlog(c("\n !!! WARNING MESSAGE:",lwiNA," lines excluded\n",
            "Check WARNING_DATA.csv in output_preparation/ dirctory error category : 'SESSION - not selected'!!\n"),fileLog)
        if (file.exists("output_preparation/WARNING_DATA.csv"))
            write.table(de.warning,"output_preparation/WARNING_DATA.csv",
                        row.names=FALSE,sep=";",quote=FALSE,append=TRUE,
                        col.names=FALSE) else
                                             write.table(de.warning,"output_preparation/WARNING_DATA.csv",
                                                         row.names=FALSE,sep=";",quote=FALSE,append=FALSE)

    }

    d <- subset(d,NB.YEARS >= 3)

    catlog(c(" --> OK\n"),fileLog)
    catlog(c("    => Current number of lines: ",nrow(d),"\n"),fileLog)





    catlog(c("\n====================================\n\n - Figure: FS sur les data STOC \n------------------------------------\n"),fileLog)

    dnf <- subset(d,select=c("NEW.ID_PROG","YEAR","FS","FS.DEDUIT","HABITAT"))
    dnf <- na.omit(dnf)
    dnf <- unique(dnf)
    dnf$DIFF <- dnf$FS.DEDUIT - dnf$FS

    m <- lm(FS.DEDUIT ~ FS, dnf,subset=HABITAT=="Terrestre")
    a = format(coef(m)[1], digits = 2)
    b = format(coef(m)[2], digits = 2)
    r = format(summary(m)$r.squared, digits = 3)
    text.equa <- paste("FS_terrestre.DEDUIT = ",a," + ",b," FS_terrestre\nr^2 = ",r,sep="")

    ggnf1 <- ggplot(dnf,aes(x=FS,y=FS.DEDUIT,colour=HABITAT))+geom_abline(slope=1,intercept = 0,colour="gray")+ coord_fixed(xlim=c(0,400),ylim=c(0,400),ratio=1)
    ggnf1 <- ggnf1 + geom_smooth(method="lm")+ annotate("text", label = text.equa,x=80,y=400,size=3,colour="red")+ geom_point()
    ggnf1 <- ggnf1 + labs(list(title="Estimation de FS a partir des data STOC",colour="Habitat"))

    ggfile <- paste("output_preparation/estimationFS_STOC.png",sep="")
    catlog(c("Check",ggfile,":"),fileLog)
    ggsave(ggfile,ggnf1)
    catlog(c("\n"),fileLog)


    ggnf2 <- ggplot(dnf,aes(x=DIFF)) + geom_histogram(binwidth = 12) + labs(list(title="Distribution de FS.DEDUIT - FS pour les DATA STOC",x="FS.DEDUIT - FS"))

    ggfile <- paste("output_preparation/estimationFS_histogrameSTOC.png",sep="")
    catlog(c("Check",ggfile,":"),fileLog)
    ggsave(ggfile,ggnf2)
    catlog(c("\n"),fileLog)








    write.csv2(d,paste("output_preparation/",fileDataClean,sep=""),row.names=FALSE,na="",quote=FALSE)
    catlog(paste("\n====================================\n\nDATA exported :output_preparation/",fileDataClean,"\n",sep=""),fileLog)
    catlog(c("\n\n    => Final number of lines: ",nrow(d),"\n"),fileLog)

    return(d)

}# end function import()
