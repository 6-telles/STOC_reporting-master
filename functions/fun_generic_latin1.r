

vecPackage=c("stringr","Hmisc","plyr","ggplot2","lubridate","grid","gridExtra","scales","rgdal","sp","ggmap","dplyr","data.table")
ip <- installed.packages()[,1]

for(p in vecPackage){
    if (!(p %in% ip))
        install.packages(pkgs=p,repos = "http://cran.univ-paris1.fr/",dependencies=TRUE)
library(p,character.only=TRUE)
}




##' ..cleaning french multi-octet characters file come from MS windows interface ..
##'
##' .. maybe not required ..
##' @title correctCaracteres
##' @param v: vector that contains french multi-octet characters
##' @return v modifié
##' @author Romain Lorrilliere
correctCaracteres <- function(v)
{
    v <-  as.character(v)
                                        # translation table to transform french multi-octet characters to simple characters
    translationTable <- data.frame(code = c("\xe7","\xe9","\xb0","°","\U3e61653c","\U3e39653c","\xea","\xe8","\U3e39653c"),trad=c("c","e","","","e","e","e","e","e"))
    translationTable$code <- as.character(translationTable$code)
    translationTable$trad <- as.character(translationTable$trad)
    for (i in 1:nrow(translationTable)) vv <- sub(tranOslationTable$code[i],translationTable$trad[i],vv)
    return(v)
}



##' .. recherche de ligne dupliquées ..
##'
##' ..  ..
##' @title duplicationScanning
##' @param d: data.frame
##' @param focus_field: vecteur des champs à tester
##' @return les lignes dupliquée
##' @author Romain Lorrilliere
duplicationScanning <-  function(d,focus_field=c("ID_PROG","ACTION","BAGUE","SP","DATE","HEURE","NF","AGE","SEXE"))
{
    dupli <- duplicated(d)
    ddupli <- d[dupli,]
    if(nrow(ddupli) > 0)
    {
        cat("\n !!! WARNING MESSAGE: duplication in database \n")

    }
    return(ddupli)

}


### realise un affichage ecran et une sauvegarde du log
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title catlog
##' @param txt : CHAR chaine de caractère à ecrire
##' @param fileLog : CHAR nom du fichier
##' @return
##' @author Romain Lorrilliere
catlog <- function(txt,fileLog) {
    cat(txt)
    if(!is.null(fileLog)) cat(txt,file=paste0("output_log/",fileLog),append = TRUE)
}




##' recherche les stations de baguage ayant fait une mise à jour de leur données
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title stationMAJ
##' @param d DATAFRAME les données de baguages
##' @param fileLog
##' @return vSite: CHAR[] des noms des stations qui ont des nouvelles donnees
##' @author Romain Lorrilliere
sationMAJ <- function(d,fileLog) {
    vSite <- as.character(unique(subset(d,YEAR==max(d$YEAR))$NEW.ID_PROG))
    return(vSite)


}

##' utile pour recherche les eureurs dans les data
##'
##' .. content for \details{} ..
##' @title exploreData
##' @param file
##' @param fileDataClean
##' @param fileData3sessions
##' @return NULL
##' @author Romain Lorrilliere
exploreData <- function(file="stoc_20180616.txt",fileDataClean="data.csv",fileData3sessions = "data3session.csv") {
    fileBrut="stoc_full_20190425";fileDataClean="data.csv";fileData3sessions = "data3session.csv"
    separateurData="\t" ;    decimalData="." #####

    file <- paste("donnees/",fileBrut,sep="")
    d1 <-read.delim(file, header = TRUE, sep = separateurData ,dec=decimalData, na = "",stringsAsFactors=FALSE)
    file <- paste("output/",fileDataClean,sep="")
    d2 <- read.csv2(file)
    file <- paste("output/",fileData3sessions,sep="")
    d3 <- read.csv2(file)


    dw <- read.csv2("output/WARNING_DATA.csv")

}





get_mode_max <- function(x) {
    ux <- unique(x)
    tab.x <- tabulate(match(x, ux))
    ux <- as.numeric(ux[tab.x == max(tab.x)])
    return(max(ux))
}


get_mode <- function(x) {
    ux <- unique(x)
    tab.x <- tabulate(match(x, ux))
    ux <- ux[tab.x == max(tab.x)]
    return(ux[1])
}


nbSessionMostFreq <- function(X) {
    require(plyr)
    cx <- count(X)
    y <- cx$x[cx$freq==max(cx$freq)]
    if(length(y)>1)y <- min(ifelse(y==2,NA,y),na.rm=TRUE)
    return(y)
}

mostFreq <- function(X) {
    require(plyr)
    cx <- count(X)
    y <- as.character(cx$x[cx$freq==max(cx$freq)])
    if(length(y)>1) y <- NA
    return(y)
}



Encoding_utf8 <- function(x) {
    Encoding(x) <- "UTF-8"
    return(x)
}



### verification de la presences des repertoire necessaire au script
### si absence la fonction les cree
checkRepertories <-  function(rep) {
### si pas de rerpertoire les creer
    contenuDir <-  dir("output/")
    if (!(rep %in% contenuDir))
        dir.create(paste("output/",rep,sep=""))
}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @return
##' @author Romain Lorrilliere
repertoireHabitat <- function() {
    checkRepertories("France")
}



##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param d
##' @param site
##' @return
##' @author Romain Lorrilliere
repertoireSite <-  function(d,site=NULL) {
    if(is.null(site)) lesSites <- unique(as.character(d$NEW.ID_PROG)) else lesSites <- site
    for(s in lesSites) checkRepertories(s)

}




eligibleSpecies <- function(x) max(nchar(unlist(strsplit(x,"0"))))>4
eligibleSpeciesSite <- function(x) max(nchar(unlist(strsplit(x,"0"))))>2


returnRateAssessment <- function(x) return(length(x[x])/length(x))





expDataStation <- function(d,site=NULL) {
    if(is.null(site)) {
        site <- sort(unique(d$NEW.ID_PROG))
    }

    w <- read.csv2("output/WARNING_DATA.csv")
##browser()
    listChampExport <- c("cId_Data","NEW.ID_PROG","ID_PROG","DEPT","LOCALITE", "LIEUDIT","FIRST.YEAR","LAST.YEAR","NB.YEARS","THEME.SESSION","THEME","HABITAT","YEAR","NB.SESSION","FS.DEDUIT","FS.OUTPUT","NB.NF","DATE","MONTH","JULIANDAY","SESSION","BAGUEUR","FS","HS","DS","HEURE","H","NF","CENTRE","BAGUE","ACTION","SP","ESPECE","SEXE","AGE","AGE_first","LP","LP_borne","LP_mean","LP_indice_borne","MA","MA_borne","MA_mean","MA_indice_borne","LP_01", "LP_99","LP_seuilmin","LP_seuilmax","MA_01","MA_99","MA_seuilmin","MA_seuilmax","HABITAT_SP","MIGRATION")

    for(ss in site) {
        ds <- subset(d,NEW.ID_PROG == ss,select=listChampExport)
        ws <- subset(w,NEW.ID_PROG == ss)

        write.csv2(ds,paste("output/",ss,"/DATA_",ss,".csv",sep=""),row.names=FALSE,na="",quote=FALSE)
        write.csv2(ws,paste("output/",ss,"/WARNING_",ss,".csv",sep=""),row.names=FALSE,na="",quote=FALSE)

    }
}





save <- function() {
 if(save.data_france){
        file <- paste0("data_France/quantileEspece_France_",habitatDemande,".csv")
        catlog(c("  -> ",file,"\n"),fileLog)
        write.csv2(tableSpQuant,file,row.names=FALSE)
 }

}




                                      # Estimdatefin() :void
                                        # -----------------------
                                        # estime et affiche la duré de simulation restante
                                        # et la date et l'heure de la fin de simulation
                                        # utilise dans un process BOUCLE

                                        # + h1: {DATE} date de début de simulation
                                        # + repTot : {INT} nombre de répétitions total
                                        # + repNow : {INT} nombre de répétitions effectuées

estimDateFin <- function(h1,repTot,repNow){
                                        # h2 {DATE} date et heure de l'instant
  h2 <- Sys.time()
                                        #  diffSec {DIFFTIME} temps écoulé entre h1 et h2 en sec
  diffSec <- difftime(h2,h1,units="secs")
                                        # timeByStep {DIFFTIME} temps par step
  timeByStep <- diffSec / repNow
                                        # timeToEnd {DIFFTIME} estimation duré simulation
  timeToEnd <- (repTot - repNow) * timeByStep
                                        # dateEnd {DATE} estimation date de fin
  dateEnd <- format(Sys.time()+ as.numeric(timeToEnd,units="secs") ,format="%d/%m/%y %H:%M'%S")
                                        # timeEstimate {FLOAT} nombre d'heures restantes
  timeEstimate <-  round(as.numeric(timeToEnd,units="hours"),1)
                                        # si moins d'1 heure
  if (timeEstimate < 1){
                                        # timeEstimata {FLOAT} nombre de minutes restantes
    timeEstimate <-  round(as.numeric(timeToEnd,units="mins"))
    cat("  * Estim:",timeEstimate,"minute(s) -> Fin batch:",dateEnd,"*\n")

  }
  else  cat("  * Estim:",timeEstimate,"heure(s) -> Fin batch:",dateEnd,"*\n")
}




