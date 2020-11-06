
## installe les packages nécessaires si besoin
vecPackage=c("stringr","Hmisc","plyr","ggplot2","lubridate","grid","gridExtra","scales","rgdal","sp","ggmap","dplyr","data.table")
ip <- installed.packages()[,1]

for(p in vecPackage){
    if (!(p %in% ip))
        install.packages(pkgs=p,repos = "http://cran.univ-paris1.fr/",dependencies=TRUE)
library(p,character.only=TRUE)
}



## Permet de changer certaines chaines de caractères que R pourrait ne pas comprendre en un caractère lisible et compréhensible auquel ça correspond
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


## Vérifie s'il y a des doublons parmi les lignes de données et si c'est le cas, il renvoie la ligne concernée ainsi qu'un warning
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



## Trouve l'année la plus récente du tableau de données, cherche toutes les stations qui ont récolté des données cette année-là 
## puis crée Vsite qui contient les noms de ces stations sous forme d'une chaine de caractères 
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

## Stocke les données brutes, nettoyées et 3 sessions sous un format dataframe bien aéré et lisible de façon à pouvoir détecter les erreurs plus aisément (je crois)
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
    fileBrut="Extrait.txt";fileDataClean="data.csv";fileData3sessions = "data3session.csv"
    separateurData="\t" ;    decimalData="." #####

    file <- paste("data_new/",fileBrut,sep="")
    d1 <-read.delim(file, header = TRUE, sep = separateurData ,dec=decimalData, na = "",stringsAsFactors=FALSE)
    file <- paste("output/",fileDataClean,sep="")
    d2 <- read.csv2(file)
    file <- paste("output/",fileData3sessions,sep="")
    d3 <- read.csv2(file)


    dw <- read.csv2("output/WARNING_DATA.csv")

}




## Prend un jeu de données, supprime les lignes duppliquées, compte le nombre de lignes qui restent et renvoie le max de ?
get_mode_max <- function(x) {
    ux <- unique(x)                               ## supprime les doublons dans x
    tab.x <- tabulate(match(x, ux))               ## compte et répertorie le nombre de fois où les lignes de la version ux et x sont identiques
    ux <- as.numeric(ux[tab.x == max(tab.x)])     ## met sous format numérique le nombre max de fois où les lignes de la version ux et x sont identiques (je crois)
    return(max(ux))
}

## Prend un jeu de données, supprime les lignes duppliquées, compte le nombre de lignes qui restent et renvoie ?
get_mode <- function(x) {
    ux <- unique(x)                        ## supprime les doublons dans x
    tab.x <- tabulate(match(x, ux))        ## compte et répertorie le nombre de fois où les lignes de la version ux et x sont identiques
    ux <- ux[tab.x == max(tab.x)]          ## renvoie le nombre max de fois où les lignes de la version ux et x sont identiques (je crois)
    return(ux[1])
}

## Prend un jeu de données, compte le nombre d'occurences et en prend le vecteur avec les valeurs max, puis il en renvoie le minimum (à revoir) 
nbSessionMostFreq <- function(X) {
    require(plyr)
    cx <- count(X)                                           ## compte le nombre de fois où chaque valeur apparaît
    y <- cx$x[cx$freq==max(cx$freq)]                         ## trouve le vecteur avec la valeur maximum pour la colonne freq
    if(length(y)>1)y <- min(ifelse(y==2,NA,y),na.rm=TRUE)    ## si le vecteur a plus d'une valeur, il renvoie le minimum de ce vecteur ou NA si une velur est égale à 2
    return(y)
}

## Prend un jeu de données, compte le nombre d'occurences et en prend le vecteur avec les valeurs max avant de le convertir en caractères, 
## puis il en renvoie le caractère s'il n'y en a qu'un, NA s'il y en avait plusieurs
mostFreq <- function(X) {
    require(plyr)
    cx <- count(X)
    y <- as.character(cx$x[cx$freq==max(cx$freq)])
    if(length(y)>1) y <- NA
    return(y)
}


## Donne le language dans lequel coder ?
Encoding_utf8 <- function(x) {
    Encoding(x) <- "UTF-8"
    return(x)
}


## Cherche les répertoires dont le script a besoin, s'ils n'existent pas la fonction les crée
### verification de la presence des repertoires necessaires au script
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


## Regarde si un site en particulier a été demandé ou si tous l'ont été. Vérifie que le répertoire propre à chaque station existe
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param d
##' @param site
##' @return
##' @author Romain Lorrilliere
repertoireSite <-  function(d,site=NULL) {
    if(is.null(site)) lesSites <- unique(as.character(d$NEW.ID_PROG)) else lesSites <- site    ## si tous les sites sont observés, il stocke dans lesSites l'ID de chaque station une seule fois, sinon il stock les stations demandées
    for(s in lesSites) checkRepertories(s)        ## il vérifie qu'un répertoire existe pour ce(s) site(s)

}



## vérifie qu'un vecteur de chaînes de caractères possède au moins une chaîne de plus de n caractères (n=4 pour eligibleSpecies, n=2 pour eligibleSpeciesSite). Renvoie True ou False selon le résultat
eligibleSpecies <- function(x) max(nchar(unlist(strsplit(x,"0"))))>4           ## Prend en argument un vecteur avec plusieurs chaines de caractères, il les sépare, compte leur nb de caractères et prend le nb max. Vérifie s'il est >4
eligibleSpeciesSite <- function(x) max(nchar(unlist(strsplit(x,"0"))))>2       ## Prend en argument un vecteur avec plusieurs chaines de caractères, il les sépare, compte leur nb de caractères et prend le nb max. Vérifie s'il est >2


returnRateAssessment <- function(x) return(length(x[x])/length(x))




## Trie les stations dans l'ordre croissant (en supprimant d'éventuels doublons) et stocke leur ID dans la variable Site
expDataStation <- function(d,site=NULL) {
    if(is.null(site)) {                         ## si jamais on n'a pas demandé de station précise
        site <- sort(unique(d$NEW.ID_PROG))     ## trie les ID des stations dans l'ordre croissant après avoir supprimé les ID en double
    }

    w <- read.csv2("output/WARNING_DATA.csv")
##browser()
    ## stocke le nom de toutes les colonnes du tableau dans la variable listChampExport
    listChampExport <- c("cId_Data","NEW.ID_PROG","ID_PROG","DEPT","LOCALITE", "LIEUDIT","FIRST.YEAR","LAST.YEAR","NB.YEARS","THEME.SESSION","THEME","HABITAT","YEAR","NB.SESSION","FS.DEDUIT","FS.OUTPUT","NB.NF","DATE","MONTH","JULIANDAY","SESSION","BAGUEUR","FS","HS","DS","HEURE","H","NF","CENTRE","BAGUE","ACTION","SP","ESPECE","SEXE","AGE","AGE_first","LP","LP_borne","LP_mean","LP_indice_borne","MA","MA_borne","MA_mean","MA_indice_borne","LP_01", "LP_99","LP_seuilmin","LP_seuilmax","MA_01","MA_99","MA_seuilmin","MA_seuilmax","HABITAT_SP","MIGRATION")

    for(ss in site) {
        ds <- subset(d,NEW.ID_PROG == ss,select=listChampExport)    ## stocke uniquement les informations de la station dont l'ID correspond à la boucle
        ws <- subset(w,NEW.ID_PROG == ss)                           ## je n'ai pas réussi à comprendre la liste d'avant car pour moi cela sélectionne intégralement les données de la station

        write.csv2(ds,paste("output/",ss,"/DATA_",ss,".csv",sep=""),row.names=FALSE,na="",quote=FALSE)     ## crée un fichier output/n°station/DATA_n°station.csv avec juste les infos de cette station sous forme de tableau (je crois)
        write.csv2(ws,paste("output/",ss,"/WARNING_",ss,".csv",sep=""),row.names=FALSE,na="",quote=FALSE)  ## crée un fichier output/n°station/WARNING_n°station.csv avec juste les infos de cette station sous forme de tableau (je crois)

    }
}




## sauvegarde les données du tableau de données tableSpQuant dans un nouveau fichier
save <- function() {
 if(save.data_france){
        file <- paste0("data_France/quantileEspece_France_",habitatDemande,".csv")   ## file permet de stocker le futur nom du fichier qui dépend de l'habitat demandé
        catlog(c("  -> ",file,"\n"),fileLog)                                   
        write.csv2(tableSpQuant,file,row.names=FALSE)                                ## crée un nouveau fichier avec tableSpQuant dont le nom correspondra à la variable file
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




