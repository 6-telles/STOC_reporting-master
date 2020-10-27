#######################################################################################################

### fonction principale
###
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title mainSTOCreporting
##' @param file CHAR nom du fichier des données brutes dans le rep données
##' @param fileDataClean CHAR nom du fichier des données propres pour écriture et lecture
##' @param fileData3sessions CHAR nom du fichier des données propres des 3 sessions selectionnées pour écriture et lecture
##' @param lastYear NULL ou NUM dernière année considerée
##' @param importationData CHAR parmis c("brut","clean","3sessions","non") permet de sélectionner les données importées
##' @param all BOOL pour faire les graphes à l'échelle nationale
##' @param local BOOL pour faire les graphes à l'échelle de la station
##' @param site NULL ou CHAR[] nom des stations à traiter
##' @param seuilAbondanceAnneeAll NUM seuil  nombre d'individus pour une année
##' @param seuilAbondanceAnneeSite NUM seuil nombre d'individus pour un site pour une année
##' @param seuilAvorteDuree NUM durée min en heure d'une session
##' @param seuilAvorteEvenement NUM nombre de captures min pour valider une session
##' @param seuilExclusionDelai NUM nombre de jours min entre 2 sessions
##' @param dateRefDefaut NUM[3] 3 dates julien de référence des sessions
##' @param selectedSessionPlot BOOL fabrication des sorties et graphes sessions conservées
##' @param carte BOOL fabrication des graphes carte
##' @param abondanceRelative BOOL fabrication des graphes boîte à moustache
##' @param variationAbondance BOOL fabrication des graphes variation d'abondance aggrégé
##' @param variationAbondanceEspece BOOL fabrication des graphes variation abondance espèces
##' @param productivite BOOL fabrication des graphes productivité aggregé
##' @param productiviteEspece BOOL fabrication des graphes productivité espèces
##' @param conditionCorporelle BOOL fabrication des graphes condition corporelle
##' @param retour BOOL fabrication des graphes taux de retour
##' @param pdf_france BOOL fabrication du pdf national
##' @param pdf_local BOOL fabrication des pdf des sites
##' @param dataStation BOOL fabrication des tables de données pour chaque sation
##' @param onlyNew BOOL analyse seulement les stations qui ont de nouvelles données
##' @return NULL
##' @author Romain Lorrilliere
mainSTOCreporting <- function(file="Extrait.txt",fileDataClean="data.csv",fileData3sessions = "data3session.csv",
                              lastYear=NULL,importationData="brut",all=TRUE,local=TRUE,site=NULL,site_start_from=NULL,
                              seuilAbondanceAnneeAll=30,seuilAbondanceAnneeSite=10,
                              seuilAvorteDuree= 4,seuilAvorteEvenement=5,seuilExclusionDelai = 10,dateRefDefaut =c(138,165,188),
                              selectedSessionPlot=TRUE,carte = TRUE,abondanceRelative=TRUE ,variationAbondance=TRUE,variationAbondanceEspece=TRUE,
                              productivite=TRUE,productiviteEspece=TRUE,conditionCorporelle=TRUE,retour=TRUE,
                              pdf_france=FALSE,dataStation=TRUE,onlyNew=TRUE,pdf_local=TRUE) {
    start <- Sys.time()    ## heure de démarrage est utilisée comme identifiant par défaut

    fileLog <- paste("log",format(start, "%Y-%m-%d_%HH%M"),".txt",sep="")

## Concatène les logs et les stocke dans le dossier output log 
    catlog(c("\n###############################################\n                   STOC REPORTING\n###############################################\n"),fileLog)

    catlog(c("\n\n     # Debut du process:",format(start, "%Y-%m-%d %H:%M\n")),fileLog)

    catlog(c("\n          fichier log:",fileLog,"\n"),fileLog)


    ## ##############################
    ## DEBUG déclaration paramètres
## file="test2.csv";fileDataClean="data.csv";fileData3sessions = "data3session.csv" #####
##    lastYear=NULL;importationData="brut";all=TRUE;local=TRUE;site=NULL #####
##    seuilAbondanceAnneeAll=30;seuilAbondanceAnneeSite=10 #####
##    seuilAvorteDuree= 4;seuilAvorteEvenement=5;seuilExclusionDelai = 10;dateRefDefaut =c(138,165,188)
##    selectedSessionPlot=TRUE;abondanceRelative=TRUE;variationAbondance=TRUE;productivite=TRUE;conditionCoporelle=TRUE;retour=TRUE #####
##
    ## #######################
## Si année non précisée, prise de l'année précédente à l'année courante de l'ordinateur et affichage de celle-ci
    if(is.null(lastYear)) {

        lastYear <- year(Sys.Date())-1
        cat(" lastYear =",lastYear,"\n\n")
    }
 ## Marque début d'importation des données, stockage dans log 
    catlog(c("\n###############################################\n                   IMPORTATION\n###############################################\n"),fileLog)
 ## Importation des données brutes en excluant les sessions non conformes (durée trop courte, nombre de sessions de captures<3, etc) 
    if(importationData == "brut") {
        d <- import(file,lastYear,decimalData=".",fileDataClean,fileLog,
                    seuilAvorteDuree,seuilAvorteEvenement,seuilExclusionDelai,dateRefDefaut)
 ## Choix de trois sessions à utiliser pour chaque station
        d <- select3sessions(d,fileData3sessions,fileLog,
                             seuilAvorteDuree,seuilAvorteEvenement,seuilExclusionDelai,dateRefDefaut)
 ## Si données non brutes, importation des données nettoyées dans le dossier Data_DB/
    } else {
        if(importationData == "clean") {
            file <- paste("data_DB/",fileDataClean,sep="")
          ## Lecture des données 
            d <- read.table(file,sep="\t",dec=".",stringsAsFactor=FALSE,header=TRUE)
            catlog(c("  <-- ",file,"\n"),fileLog)
          ## Sélection de trois sessions par station
            d <- select3sessions(d,fileData3sessions,fileLog,
                                 seuilAvorteDuree,seuilAvorteEvenement,seuilExclusionDelai,dateRefDefaut)
## Si prise des données 3sessions (déjà nettoyées et déjà sélectionnées)
        } else {
            if(importationData == "3sessions") {
                file <- paste("data_DB/",fileData3sessions,sep="")
                d <- read.table(file,sep="\t",dec=".",stringsAsFactor=FALSE,header=TRUE)
                catlog(c("  <-- ",file,"\n"),fileLog)
              ## Si autre que non rentré dans importation data, affichage message d'erreur
            } else {
                if(importationData != "non")
                    stop("\n Methode d'importation non reconue !!!\n")}
        }
    }
## Si prise de toutes les données pour faire une référence nationale
    if(all) {

        catlog(c("\nVERIFICATION DES REPERTOIRES DE SORTIE\n"),fileLog)
      ## A QUOI CA SERT
        repertoireHabitat()
## Si les graphes sont conservés, procédure non disponible
        if(selectedSessionPlot) {
            catlog(c("\nSESSIONS SELECTIONNEES\n"),fileLog)
            catlog(c("\nProcedure non disponible pour l'instant\n"),fileLog)
            ##  selectedSession.site(site=NULL,d,fileLog)
        }
## Si création de cartes, procédure non disponible
        if(carte) {
            catlog(c("\nCARTE\n"),fileLog)
            catlog(c("\nProcedure non disponible pour l'instant\n"),fileLog)
             carteAll(d,fileLog=fileLog)
        }
## Si fabrication des graphiques boite à moustache, sauvegarde de la figure d'abondance relative dans le dossier data_france
        if(abondanceRelative){
            catlog(c("\nABONDANCE RELATIVE\n"),fileLog)
            speciesRelativeAbund.all(d,fileLog=fileLog,print.fig=FALSE,save.fig=TRUE,save.data_france=TRUE)
        }
## Si graphes variation d'abondance aggrégé créés sauvegarde dans data_france
        if(variationAbondance) {
            catlog(c("\nVARIATION ABONDANCE\n"),fileLog)
            abundanceYear.all(d,fileLog=fileLog,print.fig=TRUE,save.fig=TRUE,save.data_france=TRUE)
        }
## Si fabrication des graphes variation d'abondance des espèces sauvegarde dans data_france
        if(variationAbondanceEspece) {
            catlog(c("\nVARIATION ABONDANCE PAR ESPECE\n"),fileLog)
            abundanceSpeciesYear.all(d,fileLog=fileLog,print.fig=FALSE,save.fig=TRUE,save.data_france=TRUE)
         }
## Si fabrication des graphes productivité aggregé, sauvegarde dans data_france
        if(productivite) {
            catlog(c("\nPRODUCTIVITE\n"),fileLog)
            productivityYear.all(d,fileLog=fileLog,print.fig=FALSE,save.fig=TRUE,save.data_france=TRUE)
        }
## Si fabrication des graphes productivité espece, sauvegarde dans data_france
        if(productiviteEspece) {
            catlog(c("\nPRODUCTIVITE PAR ESPECE\n"),fileLog)
            productivityYearSpecies.all(d,fileLog=fileLog,print.fig=FALSE,save.fig=TRUE,save.data_france=TRUE)


        }

## Si fabrication des graphes condition coporelle, sauvegarde dans data_france
        if(conditionCorporelle) {
            catlog(c("\nCONDITION CORPORELLE\n"),fileLog)
            bodyCondition.all(d,do.all=TRUE,do.sp=TRUE,seuilAbondanceAnnee=seuilAbondanceAnneeAll,fileLog=fileLog,print.fig=FALSE,save.fig=TRUE,save.data_france=TRUE)
        }

## Si fabrication des graphes taux de retour, sauvegarde dans data_france
        if(retour) {
            catlog(c("\nTAUX DE RETOUR\n"),fileLog)
            returnRate.all(d,do.all=TRUE,do.sp=TRUE,seuilAbondanceAnnee=seuilAbondanceAnneeAll,fileLog=fileLog,print.fig=FALSE,save.fig=TRUE,save.data_france=TRUE)

        }

## Si fabrication du pdf national, sauvegarde dans data_france
        if(pdf_france) {
            catlog(c("\nPDF\n"),fileLog)
            ##  expPDF(d,site=site,fileLog=fileLog)
            catlog(c("\nProcedure non disponible pour l'instant\n"),fileLog)
        }

    }# END  if(all)

## Si All= False, fabrication de graphes à l'échelle des stations
    if(local) {


## Identification de la station (A REVOIR)
        dsite <- data.frame(NEW.ID_PROG= unique(d$NEW.ID_PROG))


        dsite$id_station_num <-  gsub("[a-z]","",dsite$NEW.ID_PROG)
        dsite$id_station_letter <-  gsub("[0-9]","",dsite$NEW.ID_PROG)
        dsite$id_station_letter <- ifelse(nchar(dsite$id_station_letter) == 0,"_",dsite$id_station_letter)
        dsite$id_station_num <- str_pad(dsite$id_station_num, 4, pad = "0")
        dsite$id_station_txt <- paste0(dsite$id_station_num,dsite$id_station_letter)
## Liste des noms des stations dans l'ordre croissant
        dsite <- dsite[order(dsite$id_station_txt),]

## Si seulement analyse des stations qui ont de nouvelles donnnees, recherche de stations à mettre à jour
        if(onlyNew) {
            catlog(c("\nRECHERCHE STATION MISE A JOUR\n"),fileLog)
          ## Création d'une liste des noms des stations qui ont des nouvelles données
            siteN <- sationMAJ(d,fileLog)
          ## Si station à traiter non précisée, remplacement de site par siteN et traitement de toutes les stations à mettre à jour, sinon, uniquement traitement de la station choisie
            if(is.null(site)) site <- siteN else site <- site[site %in% siteN]
          ## Si une station de départ est donnée, suppression des stations avant celle-ci
            if(!is.null(site_start_from)) site <- site[which(site == site_start_from):length(site)]
        } else { 
          ## Si on considère toutes les stations, tri de leur nom dans l'ordre croissant
            if(is.null(site)) site <- sort(unique(d$NEW.ID_PROG))
          ## Si une station de départ est donnée, suppression des stations avant celle-ci
            if(!is.null(site_start_from)) site <- site[which(site == site_start_from):length(site)]

        }

## Liste des nouveaux noms des stations dans l'ordre croissant
        dsite <- subset(dsite,NEW.ID_PROG %in% site)
## Définit le nombre de stations étudiées
        nbsite <- nrow(dsite)
## Création d'un tableau numérotant chaque station avec en face son nouveau nom
        dsite2 <- data.frame(i=1:nbsite,site=dsite$NEW.ID_PROG)
        print(dsite2)

        catlog(c("\nVERIFICATION DES REPERTOIRES DE SORTIE\n"),fileLog)
## Liste des stations 
        repertoireSite(d,site)
## Si les pdf des sites sont fabriqués
        if(pdf_local){
            catlog(c("\nCREATION DES RAPPORTS\n"),fileLog)
## Heure de création
            h1 <- Sys.time()
## POURQUOI I IN 1:2 ?
            for(i in 1:2) {
                ss <- as.character(dsite2$site[i])

                catlog(c("\n",i,"/",nbsite," site:",ss,"\n"),fileLog)

                ds <- subset(d,NEW.ID_PROG == ss)
                run.rmd(id_station = ss)
              ## Affichage durée estimée pour la création du pdf de la station
                cat("\n\n######################################\n\n")
                estimDateFin(h1,nbsite,i)
                cat("\n\n######################################\n\n")
            }
        } else { # ELSE if(pdf.local)
## Si fabrication des sorties et des graphes pour les sessions conservées, affichage des sessions sélectionnées pour chaque station
            if(selectedSessionPlot) {
                catlog(c("\nSESSIONS SELECTIONNEES\n"),fileLog)
                selectedSession.site(site=site,d,fileLog,print.fig=TRUE,save.fig=FALSE,add_title=FALSE)
            }
## Si fabrication des tables de données pour chaque sation, renvoie la liste des nouveaux noms des stations avec suppression des éléments identiques 
            if(dataStation) {
                catlog(c("\nDATA STATION\n"),fileLog)
                expDataStation(d,site=site,print.fig=TRUE,save.fig=FALSE)
            }
## Si fabrication des graphes carte, chargement des outils pour afficher la carte
            if(carte) {
                catlog(c("\nCARTE\n"),fileLog)
                carteStation(d,site=site,fileLog=fileLog,print.fig=TRUE,save.fig=TRUE,add_title=FALSE)
            }
## Si fabrication des graphes boîte à moustache, voir et comprendre fonction speciesRelativeAbund.site dans le fichier fun_indicateurs_local_latin1.r
            if(abondanceRelative){
                catlog(c("\nABONDANCE RELATIVE\n"),fileLog)
                tab_ab <- speciesRelativeAbund.site(d,site=site,col_nomsp = "nom_fr",fileLog=fileLog,print.fig=TRUE,save.fig=FALSE,add_title=FALSE,return_table=TRUE)
            }
## Si fabrication des graphes variation d'abondance aggrégé, voir et comprendre fonction abundanceYear.site dans le fichier fun_indicateurs_local_latin1.r
            if(variationAbondance) {
                catlog(c("\nVARIATION ABONDANCE\n"),fileLog)

                abundanceYear.site(d,site=site,fileLog=fileLog,print.fig=TRUE,save.fig=FALSE,add_title=FALSE)

            }
## Si fabrication des graphes variation d'abondance des espèces, voir et comprendre fonction abundanceSpeciesYear.site dans le fichier fun_indicateurs_local_latin1.r
            if(variationAbondanceEspece) {
                catlog(c("\nVARIATION ABONDANCE PAR ESPECE\n"),fileLog)
                abundanceSpeciesYear.site(d,site=site,species=vec_sp,nom_sp=sp_nom,fileLog=fileLog,print.fig=TRUE,save.fig=FALSE,add_title=FALSE,facet=sp_nom)
                 ## abundanceSpeciesYear.site(d,site=site,species="PHYCOL",nom_sp=NULL,fileLog=fileLog,print.fig=TRUE,save.fig=FALSE,add_title=FALSE,facet=TRUE)
            }
## Si fabrication des graphes productivité aggregé, voir et comprendre fonction productivityYear.site dans le fichier fun_indicateurs_local_latin1.r
            if(productivite) {
                catlog(c("\nPRODUCTIVITEE\n"),fileLog)
                productivityYear.site(d,site=site,fileLog=fileLog,print.fig=TRUE,save.fig=FALSE,add_title=FALSE)
            }
## Si fabrication des graphes productivité espèces, voir et comprendre fonction productivityYearSpecies.site dans le fichier fun_indicateurs_local_latin1.r
            if(productiviteEspece) {
                catlog(c("\nPRODUCTIVITEE PAR ESPECE\n"),fileLog)
                productivityYearSpecies.site(d,site=site,col_nomsp = "nom_fr",fileLog=fileLog,print.fig=TRUE,save.fig=FALSE,add_title=FALSE,facet=TRUE)
              ##  productivityYearSpecies.site(d,site=site,species="PHYCOL",nom_sp = NULL,fileLog=fileLog,print.fig=TRUE,save.fig=FALSE,add_title=FALSE,facet=TRUE)

## productivityYearSpecies.site(d,site=site,species="PHYCOL",nom_sp = "Pouillot véloce",fileLog=fileLog,print.fig=TRUE,save.fig=FALSE,add_title=FALSE,facet=TRUE)

            }

## Si fabrication des graphes condition corporelle, voir et comprendre fonction bodyCondition.site dans le fichier fun_indicateurs_local_latin1.r
            if(conditionCorporelle) {
                catlog(c("\nCONDITION CORPORELLE\n"),fileLog)

                bodyCondition.site(d,site=site,community_level=TRUE,species_level=FALSE,fileLog=fileLog,print.fig=TRUE,save.fig=FALSE,add_title=TRUE)


 bodyCondition.site(d,site=site,community_level=FALSE,species_level=TRUE,fileLog=fileLog,print.fig=TRUE,save.fig=FALSE,add_title=TRUE)

                bodyCondition.site(d,site=site,seuilAbondanceAnnee=seuilAbondanceAnneeAll,seuilAbondanceAnneeSite,fileLog=fileLog,add_title=FALSE)

              d_body <- bodyCondition.site(d,site=site,community_level=FALSE,species_level=TRUE,species=NULL,nom_sp=NULL,fileLog=fileLog,print.fig=TRUE,save.fig=FALSE,add_title=FALSE,facet=TRUE,return.table=TRUE)

                plot_local_sp(d_body,print.fig=TRUE,save.fig=FALSE,facet_sp=TRUE,facet_group="AGE_first",y_lab="Condition corporelle: Masse/(Ecart à la taille moyenne + 1)",x_lab="Année",title_txt="",vecCol=c("#07307b","#0c5ef6","#c10909","#ea5d18"), minYear = min(subset(d,NEW.ID_PROG == site)$YEAR)-1,maxYear = max(subset(d,NEW.ID_PROG == site)$YEAR))
            }
## Si fabrication des graphes taux de retour, voir et comprendre fonction returnRate.site dans le fichier fun_indicateurs_local_latin1.r
            if(retour) {
                catlog(c("\nTAUX DE RETOUR\n"),fileLog)
                returnRate.site(d,site=site,community_level=TRUE,species_level=FALSE,species=NULL,nom_sp=NULL,fileLog=fileLog,print.fig=TRUE,save.fig=FALSE,add_title=FALSE,facet=TRUE)
##site <- "205"
       ###         returnRate.site(d,site=site,community_level=TRUE,species_level=FALSE,species=NULL,nom_sp=NULL,fileLog=fileLog,print.fig=TRUE,save.fig=FALSE,add_title=FALSE,facet=TRUE)

            returnRate.site(d,site="202",community_level=FALSE,species_level=TRUE,species=NULL,nom_sp=NULL,fileLog=fileLog,print.fig=TRUE,save.fig=FALSE,add_title=FALSE,facet=TRUE)



            }
        } # END ELSE if(pdf_.local)



    }






















## Affichage heure de fin et durée de la fabrication du rapport
    end <- Sys.time() ## heure de fin

    catlog(c("\n\n\n     # Fin du process:",format(end, "%Y-%m-%d %H:%M\n")),fileLog)
    diffT <- difftime(end,start,units="hours")
    if(diffT >= 1) catlog(c("     #      ==> Duree:",round(difftime(end,start,units="hours"),2),"heures \n"),fileLog) else catlog(c("     #      ==> Duree:",round(difftime(end,start,units="mins"),2),"minutes \n"),fileLog)




}

