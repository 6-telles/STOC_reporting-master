
####################################################################
###                                                              ###
###                      STOC reporting                          ###
###                                                              ###
####################################################################


## instalation automatiques des packages

vecPackage=c("stringr","Hmisc","plyr","ggplot2","lubridate","grid","gridExtra","scales","rgdal","sp","ggmap")
ip <- installed.packages()[,1]

for(p in vecPackage)
    if (!(p %in% ip))
        install.packages(pkgs=p,repos = "http://cran.univ-paris1.fr/",dependencies=TRUE)


### required libraries
require(stringr)
require(Hmisc)
require(lubridate)
require(plyr)
require(ggplot2)
require(grid)
require(gridExtra)
require(scales)
library(rgdal)
library(sp)
library(ggmap)


if("STOC_reporting" %in% dir()) setwd("STOC_reporting")

source("library/makepdf.r",encoding = "UTF-8")
                                        #d <-  read.csv2("output/data.csv")


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
    cat(txt,file=paste("log/",fileLog),append = TRUE)
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


#######################################################################################################

### fonction principale
### 
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title mainSTOCreporting
##' @param file CHAR nom du fichier des données brut dans le rep donnees
##' @param fileDataClean CHAR nom du fichier des donnees propre pour ecriture et lecture
##' @param fileData3sessions CHAR nom du fichier des donnees propre des 3 sessions selectionnées pour ecriture et lecture
##' @param lastYear NULL ou NUM dernière annees conciderée
##' @param importationData CHAR parmi c("brut","clean","3sessions","non") permet de selectionné les données importées
##' @param all BOOL pour faire les graphes a l'echelle national
##' @param local BOOL pour faire les graphes a l'echelle de la station
##' @param site NULL ou CHAR[] nom des stations a traiter
##' @param seuilAbondanceAnneeAll NUM seuil  nombre d'individus pour une annees
##' @param seuilAbondanceAnneeSite NUM seuil nombre d'individus pour un site pour une annees
##' @param seuilAvorteDuree NUM duree min en heure d'une session 
##' @param seuilAvorteEvenement NUM nombre de capture min pour valider une session
##' @param seuilExclusionDelai NUM nombre de jour min entre 2 sessions
##' @param dateRefDefaut NUM[3] 3 date julien de reference des sessions
##' @param selectedSessionPlot BOOL fabrication des sortie et graphes sessions conservées
##' @param carte BOOL fabrication des graphes carte
##' @param abondanceRelative BOOL fabrication des graphes boite à moustache
##' @param variationAbondance BOOL fabrication des graphes variation d'abondance aggrégé
##' @param variationAbondanceEspece BOOL fabrication des graphes variation abondance espece
##' @param productivite BOOL fabrication des graphes productivité aggregé
##' @param productiviteEspece BOOL fabrication des graphes productivité espece
##' @param conditionCorporelle BOOL fabrication des graphes condition coporelle
##' @param retour BOOL fabrication des graphes taux de retour
##' @param pdf BOOL fabrication des pdf
##' @param dataStation BOOL fabrication des tables de donnees pour chaque sation
##' @param onlyNew BOOL analyse seulement les stations qui ont de nouvelles donnnees
##' @return NULL
##' @author Romain Lorrilliere
mainSTOCreporting <- function(file="stoc_20180616.txt",fileDataClean="data.csv",fileData3sessions = "data3session.csv",
                              lastYear=NULL,importationData="brut",all=TRUE,local=TRUE,site=NULL,
                              seuilAbondanceAnneeAll=30,seuilAbondanceAnneeSite=10,
                              seuilAvorteDuree= 4,seuilAvorteEvenement=5,seuilExclusionDelai = 10,dateRefDefaut =c(138,165,188),
                              selectedSessionPlot=TRUE,carte = TRUE,abondanceRelative=TRUE ,variationAbondance=TRUE,variationAbondanceEspece=TRUE,
                              productivite=TRUE,productiviteEspece=TRUE,conditionCorporelle=TRUE,retour=TRUE,
                              pdf=TRUE,dataStation=TRUE,onlyNew=FALSE) {
    start <- Sys.time()    ## heure de demarage est utiliser comme identifiant par defaut

    fileLog <- paste("log",format(start, "%Y-%m-%d_%HH%M"),".txt",sep="")

    
    catlog(c("\n###############################################\n                   STOC REPORTING\n###############################################\n"),fileLog)
    
    catlog(c("\n\n     # Debut du process:",format(start, "%Y-%m-%d %H:%M\n")),fileLog)

    catlog(c("\n          fichier log:",fileLog,"\n"),fileLog)


## DEBUG declaration parametres   
    file="stoc_20180616.txt";fileDataClean="data.csv" #####
   lastYear=2018;importationData="brut";all=TRUE;local=TRUE;site=NULL #####
    seuilAbondanceAnneeAll=30;seuilAbondanceAnneeSite=10 #####
    selectedSessionPlot=TRUE;abondanceRelative=TRUE;variationAbondance=TRUE;productivite=TRUE;conditionCoporelle=TRUE;retour=TRUE #####

    if(is.null(lastYear)) {

        lastYear <- year(Sys.Date())-1
        cat(" lastYear =",lastYear,"\n\n")
    }
    
    catlog(c("\n###############################################\n                   IMPORTATION\n###############################################\n"),fileLog)
    if(importationData == "brut") {
        d <- import(file,lastYear,decimalData=".",fileDataClean,fileLog,
                    seuilAvorteDuree,seuilAvorteEvenement,seuilExclusionDelai,dateRefDefaut)
        d <- select3sessions(d,fileData3sessions,fileLog,
                             seuilAvorteDuree,seuilAvorteEvenement,seuilExclusionDelai,dateRefDefaut)
    } else {
        if(importationData == "clean") {
            file <- paste("output/",fileDataClean,sep="")
            d <- read.csv2(file)
            catlog(c("  <-- ",file,"\n"),fileLog)
            d <- select3sessions(d,fileData3sessions,fileLog,
                                 seuilAvorteDuree,seuilAvorteEvenement,seuilExclusionDelai,dateRefDefaut)
            
        } else {
            if(importationData == "3sessions") {
                file <- paste("output/",fileData3sessions,sep="")
                d <- read.csv2(file)
                catlog(c("  <-- ",file,"\n"),fileLog)
            } else {
               if(importationData != "non")
                stop("\n Methode d'importation non reconue !!!\n")}
        }
    }

    if(onlyNew) {
        catlog(c("\n###############################################\n                  RECHERCHE STATION MISE A JOUR\n###############################################\n"),fileLog)
        siteN <- sationMAJ(d,fileLog)
        site <- site[site %in% siteN] 


    }
    
    
    catlog(c("\n###############################################\n                   VERIFICATION DES REPERTOIRES DE SORTIE\n###############################################\n"),fileLog)
    if(all) repertoireHabitat()
    if(local) repertoireSite(d,site)


    if(selectedSessionPlot) {
        catlog(c("\n###############################################\n                  SESSIONS SELECTIONNEES\n###############################################\n"),fileLog)
        fileSave <- "output/summarySession.csv"
        aggAllSession <- read.csv2(fileSave,stringsAsFactors=FALSE)
        fileSave <- "output/historicSession.csv"
        aggSession <- read.csv2(fileSave,stringsAsFactors=FALSE)
        fileSave <- "output/SessionReference.csv"
        aggSessionRef <-  read.csv2(fileSave,stringsAsFactors=FALSE)
        
        selectedSession.site(site=site,d,fileLog,aggAllSession,aggSession,aggSessionRef)
    }


    
    if(dataStation) {
        catlog(c("\n###############################################\n                  DATA STATION\n###############################################\n"),fileLog)
        expDataStation(d,site=site)
    }

   if(carte) {
        catlog(c("\n###############################################\n                  CARTE\n###############################################\n"),fileLog)
        carteStation(d,site=site,fileLog=fileLog)
    }


    
    
    if(abondanceRelative){
        catlog(c("\n###############################################\n                   ABONDANCE RELATIVE\n###############################################\n"),fileLog)
        if(all) speciesRelativeAbund.allV2(d,fileLog=fileLog)
        if(local) speciesRelativeAbund.siteV2(d,site=site,fileLog=fileLog)
    }


    
    if(variationAbondance) {
        catlog(c("\n###############################################\n                   VARIATION ABONDANCE\n###############################################\n"),fileLog)
        if(all)  abundanceYear.all(d,fileLog=fileLog)
        if(local)abundanceYear.site(d,site=site,fileLog=fileLog)
    }

    if(variationAbondanceEspece) {
        catlog(c("\n###############################################\n                   VARIATION ABONDANCE PAR ESPECE\n###############################################\n"),fileLog)
        if(all)  abundanceSpeciesYear.all(d,fileLog=fileLog)
        if(local)abundanceSpeciesYear.site(d,site=site,fileLog=fileLog)
    }

    if(productivite) {
        catlog(c("\n###############################################\n                   PRODUCTIVITEE\n###############################################\n"),fileLog)
        if(all) productivityYear.all(d,fileLog=fileLog)
        if(local) productivityYear.site(d,site=site,fileLog=fileLog)
    }

    if(productiviteEspece) {
        catlog(c("\n###############################################\n                   PRODUCTIVITEE PAR ESPECE\n###############################################\n"),fileLog)
        if(all) productivityYearSpecies.all(d,fileLog=fileLog)
        if(local) productivityYearSpecies.site(d,site=site,fileLog=fileLog)
    }

    
    if(conditionCorporelle) {
        catlog(c("\n###############################################\n                   CONDITION CORPORELLE\n###############################################\n"),fileLog)
        if(all) bodyCondition.all(d,seuilAbondanceAnnee=seuilAbondanceAnneeAll,fileLog=fileLog)
        if(local)  bodyCondition.site(d,site=site,seuilAbondanceAnnee=seuilAbondanceAnneeAll,seuilAbondanceAnneeSite,fileLog=fileLog)
    }
    if(retour) {
        catlog(c("\n###############################################\n                  TAUX DE RETOUR\n###############################################\n"),fileLog)
        if(all) returnRate.all(d,seuilAbondanceAnnee=seuilAbondanceAnneeAll,fileLog=fileLog)
        if(local) returnRate.site(d,site=site,seuilAbondanceAnnee=seuilAbondanceAnneeAll,seuilAbondanceAnneeSite,fileLog=fileLog)
    }

    
    if(pdf) {
        catlog(c("\n###############################################\n                 PDF\n###############################################\n"),fileLog)
        expPDF(d,site=site,fileLog=fileLog)
    }


    end <- Sys.time() ## heure de fin 
    
    catlog(c("\n\n\n     # Fin du process:",format(end, "%Y-%m-%d %H:%M\n")),fileLog)
    diffT <- difftime(end,start,units="hours")
    if(diffT >= 1) catlog(c("     #      ==> Duree:",round(difftime(end,start,units="hours"),2),"heures \n"),fileLog) else catlog(c("     #      ==> Duree:",round(difftime(end,start,units="mins"),2),"minutes \n"),fileLog)
    
    

    
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
    if (file.exists("output/WARNING_DATA.csv")) {
        file.remove("output/WARNING_DATA.csv")
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
    
    catlog(c("    => Initial number of lines: ",nrow(d),"\n"),fileLog)
                                        #browser()
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
            "Check WARNING_DATA.csv in output/ dirctory error category : 'ACTION - NA'!!\n"),fileLog)
        if (file.exists("output/WARNING_DATA.csv"))
            write.table(de.warning,"output/WARNING_DATA.csv",
                        row.names=FALSE,sep=";",quote=FALSE,append=TRUE,
                        col.names=FALSE) else
                                             write.table(de.warning,"output/WARNING_DATA.csv",
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
        catlog(c("==> Check WARNING_DATA.csv in output/ directory !!\n\n"),fileLog)
        charToStringRaw <- function(X) paste(charToRaw(X),collapse =" ")
        warning.act <- data.frame(error ="encoding action",commmentError=paste("encoding action:",apply(data.frame(warning.act$ACTION),1,charToStringRaw)),suppression= "ligne",subset(warning.act,select=selectedColumns))
        
        if (file.exists("output/WARNING_DATA.csv"))
            write.table(warning.act,"output/WARNING_DATA.csv",
                        row.names=FALSE,sep=";",quote=FALSE,append=TRUE,
                        col.names=FALSE) else
                                             write.table(warning.act,"output/WARNING_DATA.csv",
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
            "Check WARNING_DATA.csv in output/ dirctory error category : 'YEAR - hors limite'!!\n"),fileLog)
        if (file.exists("output/WARNING_DATA.csv"))
            write.table(de.warning,"output/WARNING_DATA.csv",
                        row.names=FALSE,sep=";",quote=FALSE,append=TRUE,
                        col.names=FALSE) else
                                             write.table(de.warning,"output/WARNING_DATA.csv",
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
            "Check WARNING_DATA.csv in output/ dirctory error category : 'THEME - non STOC'!!\n"),fileLog)
        if (file.exists("output/WARNING_DATA.csv"))
            write.table(de.warning,"output/WARNING_DATA.csv",
                        row.names=FALSE,sep=";",quote=FALSE,append=TRUE,
                        col.names=FALSE) else
                                             write.table(de.warning,"output/WARNING_DATA.csv",
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
            "Check WARNING_DATA.csv in output/ dirctory error category : 'MONTH - hors printemps'!!\n"),fileLog)
        if (file.exists("output/WARNING_DATA.csv"))
            write.table(de.warning,"output/WARNING_DATA.csv",
                        row.names=FALSE,sep=";",quote=FALSE,append=TRUE,
                        col.names=FALSE) else
                                             write.table(de.warning,"output/WARNING_DATA.csv",
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
            "Check WARNING_DATA.csv in output/ dirctory error category : 'HEURE - non saisie'!!\n"),fileLog)
        if (file.exists("output/WARNING_DATA.csv"))
            write.table(de.warning,"output/WARNING_DATA.csv",
                        row.names=FALSE,sep=";",quote=FALSE,append=TRUE,
                        col.names=FALSE) else
                                             write.table(de.warning,"output/WARNING_DATA.csv",
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
            "Check WARNING_DATA.csv in output/ dirctory error category : 'HEURE - pas le matin'!!\n"),fileLog)
        if (file.exists("output/WARNING_DATA.csv"))
            write.table(de.warning,"output/WARNING_DATA.csv",
                        row.names=FALSE,sep=";",quote=FALSE,append=TRUE,
                        col.names=FALSE) else
                                             write.table(de.warning,"output/WARNING_DATA.csv",
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
            "Check warning_notID_PROG.csv in output/ dirctory !!\n"),fileLog)
        
        write.csv2(du_idNA,"output/warning_notID_PROG.csv",
                   row.names=FALSE,quote=FALSE)
        d.warning.idNA <- data.frame(error = "ID_PROG null",commmentError="",suppression= "ligne", subset(d,is.na(ID_PROG),select = selectedColumns))

        lwiNA <- nrow(d.warning.idNA)

        catlog(c("\n !!! WARNING MESSAGE:",lwiNA," lines excluded\n",
            "Check WARNING_DATA.csv in output/ dirctory error category : 'ID null'!!\n"),fileLog)
        if (file.exists("output/WARNING_DATA.csv"))
            write.table(de.warning,"output/WARNING_DATA.csv",
                        row.names=FALSE,sep=";",quote=FALSE,append=TRUE,
                        col.names=FALSE) else
                                             write.table(de.warning,"output/WARNING_DATA.csv",
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
            "Check WARNING_DATA.csv in output/ dirctory error category : 'NEW.ID_PROG' !!\n"),fileLog)
        if (file.exists("output/WARNING_DATA.csv"))
            write.table(de.warning,"output/WARNING_DATA.csv",
                        row.names=FALSE,sep=";",quote=FALSE,append=TRUE,
                        col.names=FALSE) else
                                             write.table(de.warning,"output/WARNING_DATA.csv",
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

    write.csv(tCont,"output/tableContingenceStation.csv",row.names=FALSE)

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
            "Check WARNING_DATA.csv in output/ dirctory error category : 'SP - SPESPE'!!\n"),fileLog)
        if (file.exists("output/WARNING_DATA.csv"))
            write.table(de.warning,"output/WARNING_DATA.csv",
                        row.names=FALSE,sep=";",quote=FALSE,append=TRUE,
                        col.names=FALSE) else
                                             write.table(de.warning,"output/WARNING_DATA.csv",
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
            "Check WARNING_DATA.csv in output/ dirctory error category : 'ACTION - R'!!\n"),fileLog)
        if (file.exists("output/WARNING_DATA.csv"))
            write.table(de.warning,"output/WARNING_DATA.csv",
                        row.names=FALSE,sep=";",quote=FALSE,append=TRUE,
                        col.names=FALSE) else
                                             write.table(de.warning,"output/WARNING_DATA.csv",
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
            "Check WARNING_DATA.csv in output/ dirctory error category : 'COND.REPR CIRC.REPR - diff 8 20'!!\n"),fileLog)
        if (file.exists("output/WARNING_DATA.csv"))
            write.table(de.warning,"output/WARNING_DATA.csv",
                        row.names=FALSE,sep=";",quote=FALSE,append=TRUE,
                        col.names=FALSE) else
                                             write.table(de.warning,"output/WARNING_DATA.csv",
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
        catlog(c(" !!! WARNING MESSAGE:",lb,"ring ID are wrong\n ==> Check warning_ring.csv in output/ dirctory !!\n"),fileLog)
        t.warning.ring <-data.frame(BAGUE=warning.ring)
        write.csv2(t.warning.ring,"output/warning_ring.csv",row.names=FALSE,na="",quote=FALSE)

        d.warning.ring <- data.frame(error = "BAGUE",commmentError="",suppression= "ligne",subset(d,BAGUE %in% warning.ring & CENTRE == "FRP",select=selectedColumns))
        catlog(c(nrow(d.warning.ring)," row deleted !!\n==> Check WARNING_DATA.csv in output/ dirctory !!\n"),fileLog)

        if (file.exists("output/WARNING_DATA.csv"))
            write.table(d.warning.ring,"output/WARNING_DATA.csv",
                        row.names=FALSE,sep=";",quote=FALSE,append=TRUE,
                        col.names=FALSE) else
                                             write.table(d.warning.ring,"output/WARNING_DATA.csv",
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
        catlog(c(" !!! WARNING MESSAGE:",lsr,"ring ID correspond to several species\n ==> Check WARNING_DATA.csv in output/ directory !!\n\n"),fileLog)
        t.warning.species_ring <- data.frame(error = "bague sur plusieur espece", commmentError="",suppression= "ligne",subset(d,BAGUE %in% warning.species_ring, select=selectedColumns))
        t.warning.species_ring <- t.warning.species_ring[order(t.warning.species_ring$BAGUE,t.warning.species_ring$DATE),]

        if (file.exists("output/WARNING_DATA.csv"))
            write.table(t.warning.species_ring,"output/WARNING_DATA.csv",
                        row.names=FALSE,sep=";",quote=FALSE,append=TRUE,
                        col.names=FALSE) else
                                             write.table(t.warning.species_ring,"output/WARNING_DATA.csv",
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
        catlog(c(" ==> Check WARNING_DATA.csv in output/ directory !!\n"),fileLog)
        t.warning.d_ring <- data.frame(error = "double baguage", commmentError="",suppression= "ligne",subset(d,BAGUE %in% warning.d_ring,select = selectedColumns))
        t.warning.d_ring <- t.warning.d_ring[order(t.warning.d_ring$BAGUE,t.warning.d_ring$DATE),]

        if (file.exists("output/WARNING_DATA.csv"))
            write.table(t.warning.d_ring,"output/WARNING_DATA.csv",
                        row.names=FALSE,sep=";",quote=FALSE,append=TRUE,
                        col.names=FALSE) else
                                             write.table(t.warning.d_ring,"output/WARNING_DATA.csv",
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
            cat(" ==> Check warning_sex.csv in output/ directory !!\n\n"))
        t.warning.sex <- subset(d,BAGUE %in% t.sex$BAGUE[which(t.sex$SEX.CONFIDENCE<1)])
        t.warning.sex <- t.warning.sex[order(t.warning.sex$BAGUE,t.warning.sex$DATE),]
        write.csv2(t.warning.sex,"output/warning_sex.csv",row.names=FALSE,na="",quote=FALSE)
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
        catlog(c(" ==> Check WARNING_DATA.csv in output/ directory !!\n"),fileLog)
        t.warning.ma <- data.frame(error = "MA aberrante", commmentError="",suppression= "Valeur ignoree", t.warning.ma)
        t.warning.ma <-  t.warning.ma[order( t.warning.ma$BAGUE, t.warning.ma$DATE),]

        if (file.exists("output/WARNING_DATA.csv"))
            write.table( t.warning.ma,"output/WARNING_DATA.csv",
                        row.names=FALSE,sep=";",quote=FALSE,append=TRUE,
                        col.names=FALSE) else
                                             write.table( t.warning.ma,"output/WARNING_DATA.csv",
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
        catlog(c(" ==> Check WARNING_DATA.csv in output/ directory !!\n"),fileLog)
        t.warning.lp <- data.frame(error = "LP aberrante", commmentError="",suppression= "Valeur tronquée", t.warning.lp)
        t.warning.lp <-  t.warning.ma[order( t.warning.lp$BAGUE, t.warning.lp$DATE),]

        if (file.exists("output/WARNING_DATA.csv"))
            write.table( t.warning.lp,"output/WARNING_DATA.csv",
                        row.names=FALSE,sep=";",quote=FALSE,append=TRUE,
                        col.names=FALSE) else
                                             write.table( t.warning.lp,"output/WARNING_DATA.csv",
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
            "Check WARNING_station1year.csv in output/ dirctory  !!\n"),fileLog)
        write.csv2(station1year,"output/WARNING_sation1year.csv",row.names=FALSE)
        
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
            "Check WARNING_DATA.csv in output/ dirctory  error category : 'NF'!!\n"),fileLog)
        catlog(c("    => Current number of lines: ",nrow(d),"\n"),fileLog)

        if (file.exists("output/WARNING_DATA.csv"))
            write.table(t.nets,"output/WARNING_DATA.csv",
                        row.names=FALSE,sep=";",quote=FALSE,append=TRUE,
                        col.names=FALSE) else
                                             write.table(t.nets,"output/WARNING_DATA.csv",
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
            "Check warning_FS_ERROR.csv in output/ dirctory !!\n",
            "This corresponds to",nrow(d.warning.nf),"lines in the database\n",
            "Check WARNING_DATA.csv in output/ dirctory  error category : 'FS ERROR'!!\n\n"),fileLog)

        write.csv2(dnf.error,"output/warning_FS_error.csv",
                   row.names=FALSE,na="",quote=FALSE)

        if (file.exists("output/WARNING_DATA.csv"))
            write.table(d.warning.nf,"output/WARNING_DATA.csv",
                        row.names=FALSE,sep=";",quote=FALSE,append=TRUE,
                        col.names=FALSE) else
                                             write.table(d.warning.nf,"output/WARNING_DATA.csv",
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
            "Check warning_FS_STRANGE.csv in output/ dirctory !!\n",
            "This corresponds to",nrow(d.warning.nf),"lines in the database\n",
            "Check WARNING_DATA.csv in output/ dirctory  error category : 'FS STRANGE'!!\n"),fileLog)

        write.csv2(dnf.strange,"output/warning_FS_strange.csv",
                   row.names=FALSE,na="",quote=FALSE)
        

        if (file.exists("output/WARNING_DATA.csv"))
            write.table(d.warning.nf,"output/WARNING_DATA.csv",
                        row.names=FALSE,sep=";",quote=FALSE,append=TRUE,
                        col.names=FALSE) else
                                             write.table(d.warning.nf,"output/WARNING_DATA.csv",
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
    
    ggfile <- paste("output/estimationFS_all.png",sep="")
    catlog(c("Check",ggfile,":"),fileLog)
    ggsave(ggfile,ggnf1)
    catlog(c("\n"),fileLog)


    ggnf2 <- ggplot(dnf,aes(x=DIFF)) + geom_histogram(binwidth = 12) + labs(list(title="Distribution de FS.DEDUIT - FS pour toute les données",x="FS.DEDUIT - FS"))
    
    ggfile <- paste("output/estimationFS_histograme_all.png",sep="")
    catlog(c("Check",ggfile,":"),fileLog)
    ggsave(ggfile,ggnf2)
    catlog(c("\n"),fileLog)

    d$FS.OUTPUT <- ifelse(d$YEAR<=2010,d$FS.DEDUIT,ifelse(is.na(d$FS),d$FS.DEDUIT,d$FS))


    
    write.csv2(d,paste("output/",fileDataClean,sep=""),row.names=FALSE,na="",quote=FALSE)
    catlog(paste("\n====================================\n\nDATA exported :output/",fileDataClean,"\n",sep=""),fileLog)
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
                        "COND.REPR","CIRC.REPR","NF","PC","PI","ID_PROG","NEW.ID_PROG","FS","HS","DS","cId_Data","LON","LAT",
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

    ## dd <- d
    
    t.nbSessionMostFreq <-  aggregate(t.nbSession$NB.SESSION,
                                      list(t.nbSession$NEW.ID_PROG),
                                      nbSessionMostFreq) 
    colnames(t.nbSessionMostFreq) <- c("NEW.ID_PROG",
                                       "NB.SESSION.MEDIAN_MOST.FREQUENT")
    
    t.nbSession <- merge(t.nbSession,t.nbSessionMostFreq,by="NEW.ID_PROG")

    t.nbSession$DIFF.NB.SESSION <- t.nbSession$NB.SESSION - t.nbSession$NB.SESSION.MEDIAN_MOST.FREQUENT 


    write.csv2(t.nbSession,"output/nbSessionYear.csv",row.names=FALSE)

    
    t.warning.session <- subset(t.nbSession,abs(DIFF.NB.SESSION)>1.5)

    lpbs <- length(unique(t.warning.session$NEW.ID_PROG))
    if(lpbs > 0)
    {
        catlog(c("\n !!! WARNING MESSAGE:",
            lpbs,
            "site have a very variable number of sessions\n",
            "These data are not exclude\n ==> ",
            "Check warning_nbSession.csv in output/ dirctory !!\n"),fileLog)
        t.warning.session <- t.warning.session[order(
            t.warning.session$NEW.ID_PROG,t.warning.session$YEAR),]
        write.csv2(t.warning.session,"output/warning_nbSession.csv",
                   row.names=FALSE,na="",quote=FALSE)
        catlog(c("    => Current number of lines: ",nrow(d),"\n"),fileLog)
        
    }else {
        catlog(c(" --> OK\n"),fileLog)
    }

    catlog(c("\n====================================\n\n - Fiche sessions par station:\n------------------------------------\n"),fileLog)

## dd <- d

    agg1 <- aggregate(BAGUE~NEW.ID_PROG+YEAR + JULIANDAY,data=d,length)
    colnames(agg1)[4] <- "nb_evenement"
    agg2 <- aggregate(HS~NEW.ID_PROG+YEAR + JULIANDAY,data=subset(d,!is.na(HS)),mostFreq)
    agg3 <- aggregate(DS~NEW.ID_PROG+YEAR + JULIANDAY,data=subset(d,!is.na(HS)),mostFreq)
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
    
    t.nbSessionMostFreq$TYPE <- ifelse(t.nbSessionMostFreq$NB.SESSION.MEDIAN_MOST.FREQUENT == 4,4,3)

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
    fileSave <- "output/historicSession.csv"
    catlog(c("\n - Historic of session saved in file :\n "),fileLog)
    write.csv2(aggSession,fileSave,row.names=FALSE,na="",quote=FALSE)
    catlog(c("\n   --> ",fileSave,"\n"),fileLog)

    catlog(c(" --> OK\n"),fileLog)
    fileSave <- "output/SessionReference.csv"
    catlog(c("\n - Historic of session saved in file :\n "),fileLog)
    write.csv2(aggSessionRef,fileSave,row.names=FALSE,na="",quote=FALSE)
    catlog(c("\n   --> ",fileSave,"\n"),fileLog)

    
    fileSave <- "output/summarySession.csv"
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
            "Check WARNING_DATA.csv in output/ dirctory error category : 'SESSION - not selected'!!\n"),fileLog)
        if (file.exists("output/WARNING_DATA.csv"))
            write.table(de.warning,"output/WARNING_DATA.csv",
                        row.names=FALSE,sep=";",quote=FALSE,append=TRUE,
                        col.names=FALSE) else
                                             write.table(de.warning,"output/WARNING_DATA.csv",
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
            "Check WARNING_DATA.csv in output/ dirctory error category : 'SESSION - not selected'!!\n"),fileLog)
        if (file.exists("output/WARNING_DATA.csv"))
            write.table(de.warning,"output/WARNING_DATA.csv",
                        row.names=FALSE,sep=";",quote=FALSE,append=TRUE,
                        col.names=FALSE) else
                                             write.table(de.warning,"output/WARNING_DATA.csv",
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
    
    ggfile <- paste("output/estimationFS_STOC.png",sep="")
    catlog(c("Check",ggfile,":"),fileLog)
    ggsave(ggfile,ggnf1)
    catlog(c("\n"),fileLog)


    ggnf2 <- ggplot(dnf,aes(x=DIFF)) + geom_histogram(binwidth = 12) + labs(list(title="Distribution de FS.DEDUIT - FS pour les DATA STOC",x="FS.DEDUIT - FS"))
    
    ggfile <- paste("output/estimationFS_histogrameSTOC.png",sep="")
    catlog(c("Check",ggfile,":"),fileLog)
    ggsave(ggfile,ggnf2)
    catlog(c("\n"),fileLog)


    



    
    
    write.csv2(d,paste("output/",fileDataClean,sep=""),row.names=FALSE,na="",quote=FALSE)
    catlog(paste("\n====================================\n\nDATA exported :output/",fileDataClean,"\n",sep=""),fileLog)
    catlog(c("\n\n    => Final number of lines: ",nrow(d),"\n"),fileLog)
    
    return(d)

}# end function import()


############################################################################################################
############################################################################################################

###                                     ENFIN DES GRAPHES                                               ###

############################################################################################################
############################################################################################################

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
selectedSession.site <- function(site=NULL,d,fileLog,aggAllSession,aggSession,aggSessionRef) {


##    fileSave <- "output/summarySession.csv"  #####
##    aggAllSession <- read.csv2(fileSave,stringsAsFactors=FALSE) #####
##    fileSave <- "output/historicSession.csv" #####
##    aggSession <- read.csv2(fileSave,stringsAsFactors=FALSE) #####
##    fileSave <- "output/SessionReference.csv" #####
##    aggSessionRef <-  read.csv2(fileSave,stringsAsFactors=FALSE) #####
    
    aggSession$SESSION <- as.character(aggSession$SESSION)
    aggSessionRef$SESSION <- as.character(aggSessionRef$SESSION)
    aggAllSession$SESSION <- as.character(aggAllSession$SESSION)
    


    
  if(is.null(site)) site <- sort(as.character(unique(d$NEW.ID_PROG)))
    
    for(ss in site)
    {
    #    browser()
        aggSession.s <- subset(aggSession,NEW.ID_PROG == ss & !is.na(JULIANDAY))
        minYear <- max(min(aggSession.s$YEAR)-2,min(aggSession$YEAR))
        maxYear <- min(max(aggSession.s$YEAR)+2,max(aggSession$YEAR))        

        aggSessionRef.s <- subset(aggSessionRef,NEW.ID_PROG == ss )

        
        vcolour <- c("1" = "#1c30f3","2" = "#951cf3","3" = "#f90d28","0" = "#1e1112")
        vdate <- c("05-01","05-15","06-01","06-15","07-01","07-15","08-01")
        vdate <- as.numeric(format(as.Date(paste(2000,vdate,sep="-")),"%j"))-1
        vdate <- sort(c(vdate,aggSessionRef.s$JULIANDAY))
        
        gg <- ggplot(data=aggAllSession,aes(x=YEAR,y=med,colour=SESSION,fill=SESSION,group=paste(SESSION)))
        gg <- gg + geom_ribbon(aes(ymin=CIinf,ymax=CIsup),alpha=.2,colour=NA)
        gg <- gg + geom_line(aes(y=CIquart_inf),size=0.6,alpha=.3)+ geom_line(aes(y=CIquart_sup),size=0.6,alpha=.3)
        gg <- gg + geom_line(size=1.5,alpha=.3)
        gg <- gg + geom_hline(data=aggSessionRef.s,aes(yintercept=JULIANDAY,colour=SESSION),size=1,linetype="dotted")
        gg <- gg + geom_line(data=subset(aggSession.s,SESSION != 0),aes(y=JULIANDAY),size=1.2)
        gg <- gg + geom_point(data=aggSession.s,aes(y=JULIANDAY,shape=VALIDE),size=2.5)
        gg <- gg + scale_fill_manual(  values=vcolour,limits=c("1","2","3"),name = "Session")
        gg <- gg + scale_colour_manual(values=vcolour,limits=c("1","2","3"),name = "Session")
        gg <- gg + scale_shape_manual(values=c("TRUE"=19,"FALSE"=8),label=c("TRUE"="oui","FALSE"="non"),breaks=c(TRUE,FALSE),name="Session valide")
        gg <- gg + coord_cartesian(xlim=c(minYear,maxYear)) + scale_x_continuous(breaks=pretty_breaks())
        gg <- gg + scale_y_continuous(labels = function(x) format(as.Date(as.character(x), "%j"), "%d-%m"),breaks = vdate)
        gg <- gg + labs(list(title=paste("Date des sessions selectionnées pour la station",ss),
                             x="Année",y="Date"))
        
        ggfile <- paste("output/",ss,"/session_",ss,".png",sep="")
        catlog(c("Check",ggfile,":"),fileLog)
        ggsave(ggfile,gg)
        catlog(c("\n"),fileLog)

        
        cmdMove <- paste("cp ",ggfile," lesSessions",sep="")
        system(cmdMove)
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
carteStation <- function(site=NULL,d,fileLog) {

    require(rgdal)
    require(ggmap)
    require(maptools)

     france <- map_data("france")
    if(is.null(site)) site <- sort(as.character(unique(d$NEW.ID_PROG)))

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
        gg <- ggplot(coordAllh,aes(LON,LAT,colour=DUREE))#,colour=creation))+
        gg <- gg+ geom_polygon( data=france, aes(x=long, y=lat, group = group),colour="gray", fill="white",size=0.3 )
        gg <- gg + geom_point(data = coordAll2,shape=1,size=1,colour="black")
        gg <- gg + geom_point(data = dcoord.s, colour="red",size=4)
        gg <- gg + geom_point(size=2,shape=19)
        gg <- gg + labs(x="",y="",title=paste("Localisation de la station ",ss,"\n et des ",nbs," stations de référence de type ",h, "\n suivies entre ",fy," et ",ly,sep=""))
        gg <- gg + scale_colour_gradient(low = "#59b7ff",high = "#142b41", name="Nombre\nd'années\nde suivi")

      
        ggfile <- paste("output/",ss,"/carte_",ss,".png",sep="")
        catlog(c("Check",ggfile,":"),fileLog)
        ggsave(ggfile,gg)#,height = 10.5,width = 13)
        catlog(c("\n"),fileLog)
        
        
    }
}








speciesRelativeAbund.allV2 <- function(d,fileLog, habitat=NULL) {
    require(ggplot2)
    dsp <- read.csv2("library/sp.csv",stringsAsFactors=FALSE)
    habitatDemande <-  habitat
    if(is.null(habitat)) habitats <- unique(as.character(d$HABITAT)) else habitats <- habitat

    for(habitat in habitats)
    {
        dh <- subset(d,HABITAT == habitat)

                                        #       browser()
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
                                        #  gg
        
        ggfile <- paste("output/France/nbCapture_France",habitat,".png",sep="")
        catlog(c("Check",ggfile,":"),fileLog)
        ppi <- 300
        ggsave(ggfile,gg,width=11, height=8, unit="in",limitsize = FALSE)
        catlog(c("\n"),fileLog)
        
    }
    write.csv2(tableSpQuant,paste("output/quantileEspece_France_",habitatDemande,".csv",sep=""),row.names=FALSE)
}



speciesRelativeAbund.siteV2 <- function(d,site=NULL,fileLog="log.txt") {
    require(ggplot2)
    dsp <- read.csv2("library/sp.csv",stringsAsFactors=FALSE)
    
    if(is.null(site)) {
        site <- sort(unique(d$NEW.ID_PROG))
        habitats <- unique(d$HABITAT)
        
    } else {
        habitats <- unique(subset(d,NEW.ID_PROG %in% site)$HABITAT)
        
    }

    ggTableH <- read.csv2("output/quantileEspece_France_.csv",stringsAsFactors=FALSE)
    ggTableH$HABITAT <- as.character(ggTableH$HABITAT)
    ggTableH$SP <- as.character(ggTableH$SP)

    ggTableH <- subset(ggTableH,PROP_STATION_CAPTURE>.2)
    
    for(ss in site)
    {
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
        
        
        gg <- ggplot(ggTable,aes(x=reorder(SP, (1-ABUNDsite75)),y=ABUND50)) 
        gg <- gg +geom_linerange(aes(ymin=ABUND025,ymax=ABUND975),colour="#08306b",alpha=.5)+ geom_crossbar(aes(ymin = ABUND25, ymax = ABUND75), width = 0.5,alpha=.5,colour="#08306b",fill="#08306b")
        gg <- gg + theme(axis.text.x  = element_text(angle=90,vjust=.5))
        gg <- gg + labs(x="Espèces caputrées dans au moins 1/5 des stations",y="Nombre d'individus capturés par an",title=paste("Site ",ss))
        gg <- gg + geom_jitter(data=ggNbCapt,aes(x=SP,y=ABUND,colour=YEAR),size=2,alpha=.9,width = .2)
        gg <- gg +scale_y_log10(breaks=c(0,1,2,5,10,20,50,100,200,400))
        gg <- gg + scale_colour_continuous(low="#ffa200",high="#960000")#"#07307b","#0c5ef6","#c10909","#ea5d18"
        
        ggfile <- paste("output/",ss,"/nbCapture_site_",ss,".png",sep="")
        catlog(c("Check",ggfile,":"),fileLog)
        ppi <- 200
        ggsave(ggfile,gg,width=11, height=8, unit="in",limitsize = FALSE)
        catlog(c("\n"),fileLog)
    }
    
}




speciesRelativeAbund.all <- function(d,habitat=NULL) {
    require(ggplot2)
    dsp <- read.csv2("library/sp.csv",stringsAsFactors=FALSE)
    tableCouleur <- subset(dsp,!is.na(TOP),select=c("SP","COLOUR"))
    dh <- d
    if(!is.null(habitat)) dh <- subset(dh,HABITAT == habitat)
    
    du.bague <- unique(subset(dh,select=c("SP","BAGUE","HABITAT")))
    t.contingency.sp <- t(table(du.bague$SP,du.bague$HABITAT))
    t.contRelat <- t(t.contingency.sp / rowSums(t.contingency.sp))

    nbH <- ncol(t.contRelat)
    vech <- colnames(t.contRelat)
    
    for(h in 1:nbH) {
        th <- data.frame(SP = rownames(t.contRelat),HABITAT = colnames(t.contRelat)[h], RELATIVE_ABUNDANCE = t.contRelat[,h])
        v <- sort(th$RELATIVE_ABUNDANCE,decreasing = TRUE)
        vcum <- cumsum(v)
                                        #vseuil <- v[min(min(which(trunc(vcum*100)>=66))+1)]
                                        #th[th$RELATIVE_ABUNDANCE<vseuil,"SP"] <- NA
        if (h==1) ggTable <- th else ggTable <-  rbind(ggTable,th)
    }

    if(nbH>1) {
        th <- data.frame(SP = rownames(t.contRelat),HABITAT = "Tout", RELATIVE_ABUNDANCE = rowSums(t.contRelat)/nbH)
        v <- sort(th$RELATIVE_ABUNDANCE,decreasing = TRUE)
        vcum <- cumsum(v)
        vseuil <- v[min(min(which(trunc(vcum*100)>=66))+1)]
        th[th$RELATIVE_ABUNDANCE<vseuil,"SP"] <- NA
        ggTable <-  rbind(ggTable,th)
    }

    ggTable <- ggTable[order(ggTable$RELATIVE_ABUNDANCE,decreasing = TRUE),]


    tc <- subset(tableCouleur,SP %in% unique(ggTable$SP))
    gg <- ggplot(data=ggTable,aes(x="",y = RELATIVE_ABUNDANCE, fill = SP)) + facet_wrap(~HABITAT,nrow=2)+
        geom_bar(width = 1,stat = "identity") +
        coord_polar(theta="y",start=pi/2) +
        scale_fill_manual(values = tc$COLOUR,breaks=tc$SP,name="Code espèces") +
        labs(x="",y="") +  ggtitle("Abondance relative")
    
    ggfile <- paste("output/France/abondanceRelative_France",habitat,".png",sep="")
    catlog(c("Check",ggfile,":"),fileLog)
    ggsave(ggfile,gg)
    catlog(c("\n"),fileLog)
    
    
}


##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param d 
##' @param site 
##' @param fileLog 
##' @return 
##' @author Romain Lorrilliere
speciesRelativeAbund.site <- function(d,site=NULL,fileLog="log.txt") {
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
abundanceYear.all <- function(d,habitat=NULL,fileLog="log.txt")
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
    

    
    t.nbnf.session <- unique(subset(dhAd,select=c("NEW.ID_PROG","YEAR","SESSION","FS.OUTPUT")))
    t.nbnf <- aggregate(FS.OUTPUT~NEW.ID_PROG+YEAR, data=t.nbnf.session, sum, na.rm=FALSE)

    t.fs.output <- tapply(t.nbnf.session$FS.OUTPUT,list(t.nbnf.session$NEW.ID_PROG,t.nbnf.session$YEAR), sum)
                                        #t.fs.saisi <- tapply(t.nbnf.session$FS,list(t.nbnf.session$NEW.ID_PROG,t.nbnf.session$YEAR), sum)

    t.fs <- data.frame(NEW.ID_PROG = rep(rownames(t.fs.output),ncol(t.fs.output)),
                       YEAR = rep(colnames(t.fs.output),each=nrow(t.fs.output)),
                       FS.OUPUT = as.vector(as.matrix(t.fs.output)))
    t.fs <- subset(t.fs,!is.na(FS.OUPUT))
    
    
    t.abund <- merge(t.abund,t.fs,by=c("NEW.ID_PROG","YEAR"))
    t.abund.all <- data.frame(t.abund,HABITAT = "tout")
    t.abund <- merge(t.abund,du.hab,by="NEW.ID_PROG")
    if(is.null(habitat))      t.abund <- rbind(t.abund,t.abund.all)
    
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

    gg <- gg + labs(list(title="Nombre d'adulte capturés (toutes espèces confondues)\npour 1 station standard (3 sessions et 120m de filet)",
                         x="Année",y="N/120m",
                         colour="")) + facet_wrap(~HABITAT,nrow = 2)

    ggfile <- paste("output/France/N_adulte_France_",habitat,".png",sep="")
    catlog(c("Check",ggfile,":"),fileLog)
    ggsave(ggfile,gg)
    catlog(c("\n"),fileLog)
    
                                        # return( ggTable.abund)
    
}




abundanceSpeciesYear.all <- function(d,habitat=NULL,fileLog="log.txt")
{
    require(ggplot2)

    tabSpeQuant <- read.csv2("output/quantileEspece_France_.csv",stringsAsFactors=FALSE)

    if(is.null(habitat)) habitat <- unique(d$HABITAT) 

    for(h in habitat)
    {
        tabSpeHab <- subset(tabSpeQuant,HABITAT == h)
        listSP <- tabSpeHab$SP[tabSpeHab$PROP_STATION_CAPTURE>.2]
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
            
            gg <- gg + labs(list(title=paste(sp,sep=""),
                                 x="Année",y="Nombre d'individus adultes capturés",
                                 colour=""))
            
            ggfile <- paste("output/France/N_adulte_France_",h,"_",sp,".png",sep="")
            catlog(c("Check",ggfile,":"),fileLog)
            ggsave(ggfile,gg)
            catlog(c("\n"),fileLog)
        }
    }
                                        # return( ggTable.abund)
    
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

abundanceSpeciesYear.site <- function(d,site=NULL,limitTime=TRUE,fileLog="log.txt")
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
    for(ss in site)
    {

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


                                        #
            
            
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
              gg <- gg + labs(list(title=paste(sp,": Station ",ss,sep=""),
                                 x="Année",y="Nombre d'individus adultes capturés",
                                 colour="")) 
                                        # 

            ggfile <- paste("output/",ss,"/N_adulte_Site_",ss,"_",sp,".png",sep="")

            catlog(c("Check",ggfile,":"),fileLog)
            ggsave(ggfile,gg)
            catlog(c("\n"),fileLog)
            
        }
    }

    
    
    
    
}




abundanceYear.site <- function(d,site=NULL,fileLog="log.txt")
{
    require(ggplot2)

                                        #tabSpeQuant <- read.csv2("output/quantileEspece_France_.csv",stringsAsFactors=FALSE)

    if(is.null(site)) site <- sort(unique(d$NEW.ID_PROG))

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
    ggTable.abund <- rbind(t.abund)

                                        #   ggTable.abund <- na.omit(ggTable.abund)

    aggTable <- aggregate(ABUND.ADJUST ~ (YEAR + HABITAT), data= ggTable.abund, quantile,c(0.025,0.25,0.5,0.75,0.975))
    aggTable <- data.frame(aggTable[,1:2],aggTable[3][[1]][,1:5])
    colnames(aggTable)[3:7] <- c("CIinf","CIquart_inf","med","CIquart_sup","CIsup")    

    for(ss in site) {
                                        # catlog(c("site:",s,"\n"),fileLog)

        
        tSite <- subset(t.abund, NEW.ID_PROG==ss)
        habitat <- as.character(tSite$HABITAT[1])
        minYear <- max(min(tSite$YEAR)-2,min(t.abund$YEAR))
        maxYear <- min(max(tSite$YEAR)+2,max(t.abund$YEAR))        

        aggTable.s <- subset(aggTable,HABITAT==habitat)
        aggTable.s <- subset(aggTable.s,YEAR>=minYear & YEAR<=maxYear)
        
                                        #ggSite <- data.frame(YEAR = tSite$YEAR,HABITAT=habitat,FS_CAT = paste("Station:",s,"| FS déduit"),CIinf=NA ,med=tSite$ABUND.ADJUST.DEDUIT,CIsup=NA)
        ggSite <- data.frame(YEAR = tSite$YEAR,HABITAT = paste("Station:",ss),CIinf=NA,CIquart_inf=NA,med=tSite$ABUND.ADJUST,CIquart_sup=NA,CIsup=NA)
        
        aggTable.s <- rbind(aggTable.s,ggSite) 
        aggTable.s$HABITAT <- as.character(aggTable.s$HABITAT)

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
        gg <- gg + labs(list(title=paste("Captures toutes espèces\npour la station ",ss,sep=""),
                             x="Année",y="Nombre d'individus adultes capturés",
                             colour="")) 
                                        #  browser()                                        # 
        ggfile <- paste("output/",ss,"/N_adulte_Site_",ss,".png",sep="")
        catlog(c("Check",ggfile,":"),fileLog)
        ggsave(ggfile,gg)
        catlog(c("\n"),fileLog)
        
        
        
    }

    
                                        # return( ggTable.abund)
    
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

productivityYear.all <- function(d,habitat=NULL,fileLog="log.txt")
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
    dNb$PROD <- dNb$BAGUE.x / (dNb$BAGUE.x + dNb$BAGUE.y)
    
    
    aggTable <- aggregate(PROD ~ (YEAR + HABITAT+MIGRATION), data= dNb, quantile,c(0.025,.25,0.5,.75,0.975))
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
    
    ggfile <- paste("output/France/Productivite_all_",habitatDemande,".png",sep="")
    catlog(c("Check",ggfile,":"),fileLog)
    ggsave(ggfile,gg)
    catlog(c("\n"),fileLog)
    
    
}





productivityYear.site <- function(d,site=NULL,limitTime=TRUE,fileLog="log.txt")
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

    aggTable <- aggregate(PROD ~ (YEAR + HABITAT+MIGRATION), data= dNb, quantile,c(0.025,.25,0.5,.75,0.975))
    aggTable <- data.frame(aggTable[,1:3],aggTable[4][[1]][,1:5])
    colnames(aggTable)[4:8] <- c("CIinf","CIquart_inf","med","CIquart_sup","CIsup")



    
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
        

        gg <- ggplot(aggTable.s,aes(x=YEAR,y=med,colour=HABITAT,fill=HABITAT))+ facet_grid(~MIGRATION)
        gg <- gg + geom_ribbon(aes(ymin=CIinf,ymax=CIsup),alpha=.2,colour = NA )
        gg <- gg + geom_line(alpha=.8,size=1.5)
        gg <- gg + geom_line(aes(y=CIquart_inf),alpha=.5,size=.8)+geom_line(aes(y=CIquart_sup),alpha=.5,size=.8)
        gg <- gg + geom_point(data=subset(aggTable.s,HABITAT==paste("Station:",ss)),size=2)
        gg <- gg + scale_x_continuous(breaks=pretty_breaks())
        gg <- gg + scale_colour_manual(values =setNames(c("#07307b","#d71818"),c(habitat,paste("Station:",ss))))
        gg <- gg + scale_fill_manual(values= setNames(c("#07307b","#d71818"),c(habitat,paste("Station:",ss))),guide=FALSE)
        gg <- gg + labs(list(title=paste("Station",ss),
                             x="Année",y="Productivité: Njuv/(Nad + Njuv)"))
        gg <- gg + theme(legend.position="none")

      ggfile <- paste("output/",ss,"/Productivite_Site_",ss,".png",sep="")
        catlog(c("Check",ggfile,":"),fileLog)
        ggsave(ggfile,gg)
        catlog(c("\n"),fileLog)
        
        
        
    }
    
    
    
}



productivityYearSpecies.all <- function(d,habitat=NULL,fileLog="log.txt")
{
    require(ggplot2)
### d <- read.csv2("output/data.csv",stringsAsFactors=FALSE)

    tabSpeQuant <- read.csv2("output/quantileEspece_France_.csv",stringsAsFactors=FALSE)
    
    if(!is.null(habitat)) dp <- subset(dp,HABITAT == habitat)


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


        
        ggfile <- paste("output/France/Productivite_Espece_FRANCE_",hh,".png",sep="")
        catlog(c("Check",ggfile,":"),fileLog)
        ggsave(ggfile,gg)
        catlog(c("\n"),fileLog)
        
    }
}





productivityYearSpecies.site <- function(d,site=NULL,fileLog="log.txt")
{
    require(ggplot2)
### d <- read.csv2("output/data.csv",stringsAsFactors=FALSE)

    tabSpeQuant <- read.csv2("output/quantileEspece_France_.csv",stringsAsFactors=FALSE)
    

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
            
            gg <- ggplot(aggTable.s,aes(x=YEAR,y=med,colour=HABITAT,fill=HABITAT))+ facet_wrap(~ESPECE)
            gg <- gg + geom_ribbon(aes(x=YEAR,ymin=CIinf,ymax=CIsup),alpha=.2,colour = NA )
            gg <- gg + geom_line(aes(y=CIquart_inf),alpha=.5,size=.8)+geom_line(aes(y=CIquart_sup),alpha=.5,size=.8)
            gg <- gg + geom_line(alpha=.8,size=1.5)
            gg <- gg + geom_point(data=subset(aggTable.s,HABITAT==paste("Station:",ss)),size=2)
            gg <- gg + scale_colour_manual(values =setNames(c("#07307b","#d71818"),c(hh,paste("Station:",ss))))
            gg <- gg + scale_fill_manual(values= setNames(c("#07307b","#d71818"),c(hh,paste("Station:",ss))),guide=FALSE)
            gg <- gg + coord_cartesian(xlim=c(minYear,maxYear)) + scale_x_continuous(breaks=pretty_breaks())
            gg <- gg + labs(list(title=paste("Station",ss),
                                 x="Année",y="Productivité: Njuv/(Nad + Njuv)"))
            gg <- gg + theme(legend.position="none")

            ggfile <- paste("output/",ss,"/Productivite_Espece_Site_",ss,".png",sep="")
            catlog(c("Check",ggfile,":"),fileLog)
            ggsave(ggfile,gg)
            catlog(c("\n"),fileLog)

        }
    }

}








eligibleSpecies <- function(x) max(nchar(unlist(strsplit(x,"0"))))>4
eligibleSpeciesSite <- function(x) max(nchar(unlist(strsplit(x,"0"))))>2

bodyCondition.all<- function(d,habitat=NULL,seuilAbondanceAnnee=50,fileLog="log.txt")
{
    
    require(ggplot2)
    ##    d <- read.csv2("output/data.csv")
    ##    ddd <- d
    ##    d <- ddd
    
    
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

    ggfile <- paste("output/France/bodyCondition_all.png",sep="")
    
    catlog(c("Check",ggfile,":"),fileLog)
    ggsave(ggfile,gg,width=5, height=6)
    catlog(c("\n"),fileLog)

    if(is.null(habitat))
        vecHab <- as.character(unique(d$HABITAT))
    
    
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

        
        ggfile <- paste("output/France/bodyCondition_sp_",h,".png",sep="")
        
        catlog(c("Check",ggfile,":"),fileLog)
        ggsave(ggfile,gg,width=8, height=10)
        catlog(c("\n"),fileLog)

        
    }
    
}





bodyCondition.site<- function(d,site=NULL,limitTime=TRUE,seuilAbondanceAnnee=30,seuilAbondanceAnneeSite=10,fileLog="log.txt")
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



returnRateAssessment <- function(x) return(length(x[x])/length(x))


returnRate.all <- function(d,habitat=NULL,seuilAbondanceAnnee=30,fileLog="log.txt")
{
    require(ggplot2)
    
    
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
        
        dsp.medSeuil.compt <- subset( dsp.medSeuil.compt,NEW.ID_PROG > 30)#length(unique(d.a$NEW.ID_PROG))*.25)
        
        dsp.ret <- dsp.ret[which(paste(dsp.ret$SP) %in% paste( dsp.medSeuil.compt$SP)),]
        
        aggTableSP <- aggregate(RETURN ~ (YEAR + AGE_first + SP + MIGRATION), data= dsp.ret, quantile,c(0.025,0.25,0.5,0.75,0.975))
        aggTableSP <- data.frame(aggTableSP[,1:4],aggTableSP[5][[1]][,1:5])
        colnames(aggTableSP)[5:9] <- c("CIinf","CIquart_inf","med","CIquart_sup","CIsup")
        aggTableSP <- subset(aggTableSP,YEAR<max(aggTableSP$YEAR))
        aggTableSP$HABITAT <- h
        
        
        if(h == vecHab[1]) aggTableSP.tot <- aggTableSP else aggTableSP.tot <- rbind(aggTableSP.tot,aggTableSP)
        
        
        
        
        

    }

    
    
    for(h in vecHab) {


        gg <- ggplot(subset(aggTableAll.tot,HABITAT==h),aes(x=YEAR,y=med,colour=MIGRATION,fill=MIGRATION))+ facet_grid(MIGRATION~AGE_first)
        gg <- gg + geom_ribbon(aes(ymin=CIinf,ymax=CIsup),alpha=.4,colour = NA)
        gg <- gg + geom_line(size=1.5,alpha=.8)
        gg <- gg + geom_line(aes(y=CIquart_inf),alpha=.5,size=.8)+geom_line(aes(y=CIquart_sup),alpha=.5,size=.8)
        gg <- gg + scale_colour_manual(values =c("#07307b","#0c5ef6"))
        gg <- gg + scale_fill_manual(values =c("#07307b","#0c5ef6"),guide=FALSE)
    gg <- gg + scale_x_continuous(breaks=pretty_breaks())
        gg <- gg + labs(list(title=paste("Taux de retour sur les sites de type ",h,"\npar site, toutes espèces confondue",sep=""),
                             x="Année",y="Taux contrôle à t+1"))
        
        ggfile <- paste("output/France/ReturnRate_all_",h,".png",sep="")
        catlog(c("Check",ggfile,":"),fileLog)
        ggsave(ggfile,gg,width=7, height=6)
        catlog(c("\n"),fileLog)
        
        gg <- ggplot(subset(aggTableSP.tot,HABITAT==h),aes(x=YEAR,y=med,colour=MIGRATION,fill=MIGRATION))+facet_grid(SP~AGE_first)
        gg <- gg + geom_ribbon(aes(ymin=CIinf,ymax=CIsup),alpha=.4,colour = NA)
        gg <- gg + geom_line(size=1.5,alpha=.8)
        gg <- gg + geom_line(aes(y=CIquart_inf),alpha=.5,size=.8)+geom_line(aes(y=CIquart_sup),alpha=.5,size=.8)
        gg <- gg + scale_colour_manual(values =c("#07307b","#0c5ef6"))
        gg <- gg + scale_fill_manual(values =c("#07307b","#0c5ef6"),guide=FALSE)
        gg <- gg + labs(list(title=paste("Taux de retour sur les sites de type ",h,"\npour chaque espèces éligible",sep=""),
                             x="Année",y="Taux contrôle à t+1"))
        
        ggfile <- paste("output/France/ReturnRate_sp_",h,".png",sep="")
        catlog(c("Check",ggfile,":"),fileLog)
        ggsave(ggfile,gg,width=7, height=9)
        catlog(c("\n"),fileLog)
    }
    
}




returnRate.site <- function(d,site=NULL,seuilAbondanceAnnee=30,seuilAbondanceAnneeSite=10,fileLog="log.txt")
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
