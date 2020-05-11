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
##' @param seuilAbondanceAnneeSite NUM seuil nombre d'ind
##' ividus pour un site pour une annees
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
##' @param pdf_france BOOL fabrication du pdf national
##' @param pdf_local BOOL fabrication des pdf des sites
##' @param dataStation BOOL fabrication des tables de donnees pour chaque sation
##' @param onlyNew BOOL analyse seulement les stations qui ont de nouvelles donnnees
##' @return NULL
##' @author Romain Lorrilliere
mainSTOCreporting <- function(file="test2.csv",fileDataClean="data.csv",fileData3sessions = "data3session.csv",
                              lastYear=NULL,importationData="brut",all=TRUE,local=TRUE,site=NULL,
                              seuilAbondanceAnneeAll=30,seuilAbondanceAnneeSite=10,
                              seuilAvorteDuree= 4,seuilAvorteEvenement=5,seuilExclusionDelai = 10,dateRefDefaut =c(138,165,188),
                              selectedSessionPlot=TRUE,carte = TRUE,abondanceRelative=TRUE ,variationAbondance=TRUE,variationAbondanceEspece=TRUE,
                              productivite=TRUE,productiviteEspece=TRUE,conditionCorporelle=TRUE,retour=TRUE,
                              pdf_france=FALSE,dataStation=TRUE,onlyNew=TRUE,pdf_local=TRUE) {
    start <- Sys.time()    ## heure de demarage est utiliser comme identifiant par defaut

    fileLog <- paste("log",format(start, "%Y-%m-%d_%HH%M"),".txt",sep="")


    catlog(c("\n###############################################\n                   STOC REPORTING\n###############################################\n"),fileLog)

    catlog(c("\n\n     # Debut du process:",format(start, "%Y-%m-%d %H:%M\n")),fileLog)

    catlog(c("\n          fichier log:",fileLog,"\n"),fileLog)


    ## ##############################
    ## DEBUG declaration parametres
## file="test2.csv";fileDataClean="data.csv";fileData3sessions = "data3session.csv" #####
##    lastYear=NULL;importationData="brut";all=TRUE;local=TRUE;site=NULL #####
##    seuilAbondanceAnneeAll=30;seuilAbondanceAnneeSite=10 #####
##    seuilAvorteDuree= 4;seuilAvorteEvenement=5;seuilExclusionDelai = 10;dateRefDefaut =c(138,165,188)
##    selectedSessionPlot=TRUE;abondanceRelative=TRUE;variationAbondance=TRUE;productivite=TRUE;conditionCoporelle=TRUE;retour=TRUE #####
##
    ## #######################

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
            file <- paste("data_DB/",fileDataClean,sep="")
            d <- read.table(file,sep="\t",dec=".",stringsAsFactor=FALSE,header=TRUE)
            catlog(c("  <-- ",file,"\n"),fileLog)
            d <- select3sessions(d,fileData3sessions,fileLog,
                                 seuilAvorteDuree,seuilAvorteEvenement,seuilExclusionDelai,dateRefDefaut)

        } else {
            if(importationData == "3sessions") {
                file <- paste("data_DB/",fileData3sessions,sep="")
                d <- read.table(file,sep="\t",dec=".",stringsAsFactor=FALSE,header=TRUE)
                catlog(c("  <-- ",file,"\n"),fileLog)
            } else {
                if(importationData != "non")
                    stop("\n Methode d'importation non reconue !!!\n")}
        }
    }

    if(all) {

        catlog(c("\nVERIFICATION DES REPERTOIRES DE SORTIE\n"),fileLog)
        repertoireHabitat()

        if(selectedSessionPlot) {
            catlog(c("\nSESSIONS SELECTIONNEES\n"),fileLog)
            catlog(c("\nProcedure non disponible pour l'instant\n"),fileLog)
            ##  selectedSession.site(site=NULL,d,fileLog)
        }

        if(carte) {
            catlog(c("\nCARTE\n"),fileLog)
            catlog(c("\nProcedure non disponible pour l'instant\n"),fileLog)
            ## carteStation(d,site=site,fileLog=fileLog)
        }

        if(abondanceRelative){
            catlog(c("\nABONDANCE RELATIVE\n"),fileLog)
            speciesRelativeAbund.all(d,fileLog=fileLog,print.fig=FALSE,save.fig=TRUE,save.data_france=TRUE)
        }

        if(variationAbondance) {
            catlog(c("\nVARIATION ABONDANCE\n"),fileLog)
            abundanceYear.all(d,fileLog=fileLog,print.fig=FALSE,save.fig=TRUE,save.data_france=TRUE)
        }

        if(variationAbondanceEspece) {
            catlog(c("\nVARIATION ABONDANCE PAR ESPECE\n"),fileLog)
            abundanceSpeciesYear.all(d,fileLog=fileLog,print.fig=FALSE,save.fig=TRUE,save.data_france=TRUE)
         }

        if(productivite) {
            catlog(c("\nPRODUCTIVITEE\n"),fileLog)
            productivityYear.all(d,fileLog=fileLog,print.fig=FALSE,save.fig=TRUE,save.data_france=TRUE)
        }

        if(productiviteEspece) {
            catlog(c("\nPRODUCTIVITEE PAR ESPECE\n"),fileLog)
            productivityYearSpecies.all(d,fileLog=fileLog,print.fig=FALSE,save.fig=TRUE,save.data_france=TRUE)
        }


        if(conditionCorporelle) {
            catlog(c("\nCONDITION CORPORELLE\n"),fileLog)
            bodyCondition.all(d,do.all=TRUE,do.sp=TRUE,seuilAbondanceAnnee=seuilAbondanceAnneeAll,fileLog=fileLog,print.fig=FALSE,save.fig=TRUE,save.data_france=TRUE)
        }


        if(retour) {
            catlog(c("\nTAUX DE RETOUR\n"),fileLog)
            returnRate.all(d,do.all=TRUE,do.sp=TRUE,seuilAbondanceAnnee=seuilAbondanceAnneeAll,fileLog=fileLog,print.fig=FALSE,save.fig=TRUE,save.data_france=TRUE)

        }


        if(pdf_france) {
            catlog(c("\nPDF\n"),fileLog)
            ##  expPDF(d,site=site,fileLog=fileLog)
            catlog(c("\nProcedure non disponible pour l'instant\n"),fileLog)
        }

    }# END  if(all)


    if(local) {

        if(onlyNew) {
            catlog(c("\nRECHERCHE STATION MISE A JOUR\n"),fileLog)
            siteN <- sationMAJ(d,fileLog)
            if(is.null(site)) site <- siteN else site <- site[site %in% siteN]

        }

        catlog(c("\nVERIFICATION DES REPERTOIRES DE SORTIE\n"),fileLog)
        repertoireSite(d,site)

        if(pdf_local){

            for(s in site) {
                ds <- subset(d,NEW.ID_PROG == s)
                run.rmd(id_station = s)

            }
        } else { # ELSE if(pdf.local)

            if(selectedSessionPlot) {
                catlog(c("\nSESSIONS SELECTIONNEES\n"),fileLog)
                selectedSession.site(site=site,d,fileLog,print.fig=TRUE,save.fig=FALSE,add_title=FALSE)
            }

            if(dataStation) {
                catlog(c("\nDATA STATION\n"),fileLog)
                expDataStation(d,site=site,print.fig=TRUE,save.fig=FALSE)
            }

            if(carte) {
                catlog(c("\nCARTE\n"),fileLog)
                carteStation(d,site=site,fileLog=fileLog,print.fig=TRUE,save.fig=TRUE,add_title=FALSE)
            }

            if(abondanceRelative){
                catlog(c("\nABONDANCE RELATIVE\n"),fileLog)
                tab_ab <- speciesRelativeAbund.site(d,site=site,col_nomsp = "nom_fr",fileLog=fileLog,print.fig=TRUE,save.fig=FALSE,add_title=FALSE,return_table=TRUE)
            }

            if(variationAbondance) {
                catlog(c("\nVARIATION ABONDANCE\n"),fileLog)

                abundanceYear.site(d,site=site,fileLog=fileLog,print.fig=TRUE,save.fig=FALSE,add_title=FALSE)

            }

            if(variationAbondanceEspece) {
                catlog(c("\nVARIATION ABONDANCE PAR ESPECE\n"),fileLog)
                abundanceSpeciesYear.site(d,site=site,species=vec_sp,nom_sp=sp_nom,fileLog=fileLog,print.fig=TRUE,save.fig=FALSE,add_title=FALSE,facet=sp_nom)
                  abundanceSpeciesYear.site(d,site=site,species="PHYCOL",nom_sp=NULL,fileLog=fileLog,print.fig=TRUE,save.fig=FALSE,add_title=FALSE,facet=TRUE)
            }

            if(productivite) {
                catlog(c("\nPRODUCTIVITEE\n"),fileLog)
                productivityYear.site(d,site=site,fileLog=fileLog,print.fig=TRUE,save.fig=FALSE,add_title=FALSE)
            }

            if(productiviteEspece) {
                catlog(c("\nPRODUCTIVITEE PAR ESPECE\n"),fileLog)
                productivityYearSpecies.site(d,site=site,col_nomsp = "nom_fr",fileLog=fileLog,print.fig=TRUE,save.fig=FALSE,add_title=FALSE,facet=TRUE)
                productivityYearSpecies.site(d,site=site,species="PHYCOL",nom_sp = NULL,fileLog=fileLog,print.fig=TRUE,save.fig=FALSE,add_title=FALSE,facet=TRUE)

            }


            if(conditionCorporelle) {
                catlog(c("\nCONDITION CORPORELLE\n"),fileLog)

                bodyCondition.site(d,site=site,community_level=TRUE,species_level=FALSE,fileLog=fileLog,print.fig=TRUE,save.fig=FALSE,add_title=TRUE)


 bodyCondition.site(d,site=site,community_level=FALSE,species_level=TRUE,fileLog=fileLog,print.fig=TRUE,save.fig=FALSE,add_title=TRUE)


                bodyCondition.site(d,site=site,seuilAbondanceAnnee=seuilAbondanceAnneeAll,seuilAbondanceAnneeSite,fileLog=fileLog,add_title=FALSE)

bodyCondition.site(d,site=site,community_level=FALSE,species_level=TRUE,species="SYLATR",nom_sp=NULL,fileLog=fileLog,print.fig=TRUE,save.fig=FALSE,add_title=FALSE,facet=TRUE)

            }
            if(retour) {
                catlog(c("\nTAUX DE RETOUR\n"),fileLog)
                returnRate.site(d,site=site,seuilAbondanceAnnee=seuilAbondanceAnneeAll,seuilAbondanceAnneeSite,fileLog=fileLog,add_title=FALSE)
site <- "205"
                returnRate.site(d,site=site,community_level=TRUE,species_level=FALSE,species=NULL,nom_sp=NULL,fileLog=fileLog,print.fig=TRUE,save.fig=FALSE,add_title=FALSE,facet=TRUE)

                returnRate.site(d,site=site,community_level=FALSE,species_level=TRUE,species=NULL,nom_sp=NULL,fileLog=fileLog,print.fig=TRUE,save.fig=FALSE,add_title=FALSE,facet=TRUE)



            }
        } # END ELSE if(pdf_.local)



    }























    end <- Sys.time() ## heure de fin

    catlog(c("\n\n\n     # Fin du process:",format(end, "%Y-%m-%d %H:%M\n")),fileLog)
    diffT <- difftime(end,start,units="hours")
    if(diffT >= 1) catlog(c("     #      ==> Duree:",round(difftime(end,start,units="hours"),2),"heures \n"),fileLog) else catlog(c("     #      ==> Duree:",round(difftime(end,start,units="mins"),2),"minutes \n"),fileLog)




}

