
makePDF <- function(s="2b",year=2016,system="linux") {
#browser()
    lesFichierFigures <- dir(paste("output/",s,sep=""))
    lesFichierFigures <- lesFichierFigures[grep(".png",lesFichierFigures)]
    
    nomFig.ses <- lesFichierFigures[grep("session",lesFichierFigures)]
    nomFig.carte <- lesFichierFigures[grep("carte",lesFichierFigures)]
    
    nomFig.nb <- lesFichierFigures[grep("nbCapture",lesFichierFigures)]

    nomFig.ab_all<- lesFichierFigures[grep("N_adulte",lesFichierFigures)]
    subNom <- substr(nomFig.ab_all,start=15,stop=nchar(nomFig.ab_all))
    nomFig.ab.gr <- nomFig.ab_all[grep("_",subNom,invert = TRUE)]
    nomFig.ab.sp <- nomFig.ab_all[grep("_",subNom)]

    nomFig.prod.gr <- lesFichierFigures[grep("Productivite_Site",lesFichierFigures)]
    nomFig.prod.sp <- lesFichierFigures[grep("Productivite_Espece",lesFichierFigures)]

    nomFig.bc.gr <- lesFichierFigures[grep("bodyCondition_site",lesFichierFigures)]
    nomFig.bc.sp <- lesFichierFigures[grep("bodyCondition_sp",lesFichierFigures)]

    nomFig.ret.gr <- lesFichierFigures[grep("ReturnRate_Site",lesFichierFigures)]
    nomFig.ret.sp <- lesFichierFigures[grep("ReturnRate_sp",lesFichierFigures)]


    entete <- c("\\documentclass[a4paper,11pt]{article}",
                "\\usepackage[utf8x]{inputenc}",
                "\\usepackage[T1]{fontenc}",
                "\\usepackage[french]{babel}",
                "\\usepackage{graphicx}",
                "\\usepackage{subcaption}",
                "\\usepackage[hmargin=2.5cm,vmargin=3cm]{geometry}",
                "\\usepackage{hyperref}",
                paste("\\graphicspath{{../output/",s,"/}}",sep=""),
                "% Title Page",
                paste("\\title{Indicateurs de fonctionnement des populations d'oiseaux communs issus du Suivi Temporel des Oiseaux Communs par Capture (STOC Capture) :\\\\ ",
                "Infographie actualisée pour la station n\\textdegree",s,", \\\\ ",
                "contenant les données du printemps ",year,sep=""),
                "\\\\ \\vspace{100}",
                "\\\\ \\bigskip \\includegraphics[width=.5\\textwidth]{../../makePDFs/logo_mnhn_crbpo}",
                "}",
                "\\author{Romain Lorrilliere\\thanks{\\href{mailto: lorrilliere@mnhn.fr}{\\textit{lorrilliere@mnhn.fr}}}, ",
                "Pierre-Yves Henry\\thanks{\\href{mailto: pierre-yves.henry@mnhn.fr}{\\textit{pierre-yves.henry@mnhn.fr}}},", 
                "et le CRBPO\\thanks{Centre de Recherches sur la Biologie des Populations d'Oiseaux\\newline Museum National D'histoire Naturelle \\newline",
                "43 rue Buffon - Bâtiment 135, CP 135,75005 Paris \\newline \\url{http://crbpo.mnhn.fr/}}}")
    
    startDoc <- c("\\begin{document}","\\maketitle \\clearpage", "\\tableofcontents", "\\listoffigures \\clearpage")

    intro <- "\\input{intro.tex}"

    startResult <- "\\section{Variation temporelle des indicateurs}"
    tex.lect <- "\\input{lecture.tex}"
	  
    endDoc <- "\\end{document}"


    np <- "\\newpage "


if(length(nomFig.carte)>0) {
        fig.carte <- c("\\begin{figure}[H]",
                    "\\centering",
                    paste("\\includegraphics[width=.75\\textwidth]{",nomFig.carte,"}",sep=""),
                    paste("\\caption [La localisation des stations]",
                          " {Localisation de la station ",s, 
                          ", (cercle rouge) et des stations du même type dont les données sont utilisées pour calculer les valeurs références des indicateurs. Le dégradé de bleu indique le nombre d'années de suivies qui sont considérés pour le calcul des indicateurs. Les stations STOC-Capture non concidérées sont indiquées par un petit cercle vide. }",sep=""),  
                    "\\label{carte}",
                    "\\end{figure}") } else {fig.carte <- ""}



    
        if(length(nomFig.ses)>0) {
        fig.ses <- c("\\begin{figure}[H]",
                    "\\centering",
                    paste("\\includegraphics[width=.75\\textwidth]{",nomFig.ses,"}",sep=""),
                    paste("\\caption [Les sessions]",
                          " {Les 3 sessions selectionées pour la station ",s,
                          ". Les lignes pointillées représentent les dates de références pour la station calculer à partir des dates locales de session. Les points de couleur (bleu pour la session 1, violet pour la session 2 et rouge pour la session 3) indiquent les sessions concervées pour l'analyse des indicateurs. En couleur pale est indiqué la distribution des dates des sessions à l'échelle nationale (en gras la médiane, en trait fin les 50\\% autour de la médiane et en ombré la zone des 95\\%)}",sep=""),  
                    "\\label{ses}",
                    "\\end{figure}") } else {fig.ses <- ""}


    
    
 
    
    if(length(nomFig.nb)>0) {
        tex.com <- "\\input{communaute.tex}"
        fig.com <- c("\\begin{figure}[H]",
                    "\\centering",
                    paste("\\includegraphics[width=.75\\textwidth]{",nomFig.nb,"}",sep=""),
                    paste("\\caption [Les espèces capturées]",
                          " {Nombre d'individus capturés par espèce sur la station ",s,
                          ", les points dégradés du jaune au rouge (correspondant aux années) représentent les données locales, les boxplots bleus représentent la distribution nationale de ces captures sur les stations qui sont du même type et qui capturent l'espèce}",sep=""),  
                    "\\label{com}",
                    "\\end{figure}") } else {fig.com <- "" ; tex.com <- ""}

    
    if(length(nomFig.ab.gr)>0) {
        tex.ab <- "\\input{abond.tex}"
        fig.ab.gr <- c("\\begin{figure}[H]",
                       "\\centering",
                       paste("\\includegraphics[width=.8\\textwidth]{",nomFig.ab.gr,"}",sep=""),
                       paste("\\caption [Variation du nombre d'adultes] {Variation du nombre d'adultes capturés pour la station ",s," ramenée à un effort de suivi de 120m de filets et 3 sessions matinales. En bleu, la variation nationale des quantiles de l'indicateur  (médiane, 50\\% et 95\\%), en rouge la variation pour la station ",s,"}",sep=""),  
                       "\\label{ab}",
                       "\\end{figure}") } else {fig.ab.gr <- "";tex.ab=""}




    if(length(nomFig.ab.sp)>0) {
        fig.ab.sp <- c("\\begin{figure}[H]",
                       "\\centering")
        for(f in 1:length(nomFig.ab.sp)) {
            nf <- nomFig.ab.sp[f]
            sp <- substr(nf,nchar(nf)-9,nchar(nf)-4)
            subfig <- c("\\begin{subfigure}[b]{0.45\\textwidth}",
                        paste("\\includegraphics[width=\\textwidth]{",nf,"}",sep=""),
                        paste("\\caption{",sp,"}",sep=""),
                        paste("\\label{",paste("ab",sp,sep=""),"}",sep=""),
                        "\\end{subfigure}")
            if(f %% 6 == 0) subfig <- c(subfig,"\\end{figure}","\\clearpage","\\begin{figure}[H]","\\ContinuedFloat")
            fig.ab.sp <- c(fig.ab.sp,subfig)
        }
        fig.ab.sp <- c(fig.ab.sp,paste("\\caption [Variation du nombre d'adultes capturés par espèce] {Variation du nombre d'adultes capturés pour les principales espèces de la station ",s," ramenée à un effort de suivi de 120m de filets et 3 sessions matinales. En bleu, la variation nationale des quantiles de l'indicateur  (médiane, 50\\% et 95\\%), en rouge la variation pour la station ",s,"}",sep=""),  
                       "\\label{abSP}",
                       "\\end{figure}") } else {fig.ab.sp <- ""}



    if(length(nomFig.prod.gr)>0) {
        tex.prod <- "\\input{prod.tex}"
        fig.prod.gr <- c("\\begin{figure}[H]",
                         "\\centering",
                         paste("\\includegraphics[width=.8\\textwidth]{",nomFig.prod.gr,"}",sep=""),
                         paste("\\caption [Variation de la productivité] {Variation de la productivité globale pour la station ",s,". La productivité est calculée comme suit: $\\frac{N_{juv}}{N_{juv}+N_{ad}}$. En bleu, la variation nationale des quantiles de l'indicateur (médiane, 50\\% et 95\\%), en rouge la variation pour la station ",s,"}",sep=""),  
                         "\\label{prod}",
                         "\\end{figure}") } else {fig.prod.gr <- "";tex.prod=""}


    if(length(nomFig.prod.sp)>0) {
        fig.prod.sp <- c("\\begin{figure}[H]",
                         "\\centering",
                         paste("\\includegraphics[width=1\\textwidth]{",nomFig.prod.sp,"}",sep=""),
                         paste("\\caption [Variation de la productivité par espèce] {Variation de la productivité pour les principales espèces de la station ",s,". La productivité est calculée comme suit: $\\frac{N_{juv}}{N_{juv}+N_{ad}}$. En bleu, la variation nationale des quantiles de l'indicateur (médiane, 50\\% et 95\\%), en rouge la variation pour la station ",s,"}",sep=""),  
                         "\\label{prodSP}",
                         "\\end{figure}") } else {fig.prod.sp <- ""}



    if(length(nomFig.bc.gr)>0) {
        tex.bc <- "\\input{conditionCorps.tex}"
        fig.bc.gr <- c("\\begin{figure}[H]",
                         "\\centering",
                         paste("\\includegraphics[width=.9\\textwidth]{",nomFig.bc.gr,"}",sep=""),
                         paste("\\caption [Variation de la condition corporelle globale] {Variation de la condition corporelle globale pour la station ",s,". La condition corporelle d'un individu $i$ est la standardisation de sa masse $M_i$ par la masse de moyenne $\\bar{m}$ de son espèce $s$ et de sa classe d'age $a$ comme suit: $ \\textit{Condition corporelle}_i =  \\frac{M_{i,s,a} -\\bar{m}_{s,a}}{\\bar{m}_{s,a}}$. En bleu, la variation des quantiles de l'indicateur national (médiane, 50\\% et 95\\%), en rouge et orange la variation pour la station ",s,"}",sep=""),  
                         "\\label{bc}",
                         "\\end{figure}") } else {fig.bc.gr <- "";tex.bc <- ""}


    if(length(nomFig.bc.sp)>0) {
        fig.bc.sp <- c("\\begin{figure}[H]",
                         "\\centering",
                         paste("\\includegraphics[width=.75\\textwidth]{",nomFig.bc.sp,"}",sep=""),
                       paste("\\caption [Variation de la condition corporelle pour les principales espèces] {Variation de la condition corporelle spécifique des principales espèces de la station ",s,". La condition corporelle spécifique d'un individu $i$ est la standardisation de sa masse $M_i$ par l'écart de sa LP $\\breve{l}$ à la taille moyenne de la LP $\\bar{l}$ de son espèce $s$ et de sa classe d'age $a$ comme suit:",
                             " $\\textit{Condition corporelle spécifique}_i = \\frac{M_{i,s,a}}{\\breve{l}_{s,a}}$ où $\\breve{l}_{s,a} = \\frac{L_{i,s,a}-\\bar{l}_{s,a}}{\\bar{l}_{s,a}}+1$. En bleu, la variation des quantiles de l'indicateur national (médiane, 50\\% et 95\\%), en rouge et orange la variation pour la station ",s,"}",sep=""),  
                         "\\label{bcSP}",
                         "\\end{figure}") } else {fig.bc.sp <- ""}




    if(length(nomFig.ret.gr)>0) {
        tex.ret <- "\\input{retour.tex}"
        fig.ret.gr <- c("\\begin{figure}[H]",
                         "\\centering",
                         paste("\\includegraphics[width=1\\textwidth]{",nomFig.ret.gr,"}",sep=""),
                         paste("\\caption [Variation du taux de retour global] {Variation du taux de retour sur l'ensemble des espèces pour la station ",s,". Le taux de retour de l'année $t$ est la proportion des individus qui sont revus à l'année $t+1$. En bleu, la variation des quantiles de l'indicateur national (médiane, 50\\% et 95\\%), en rouge et orange la variation pour la station ",s,"}",sep=""),  
                         "\\label{ret}",
                         "\\end{figure}") } else {fig.ret.gr <- "";tex.ret <- ""}


    if(length(nomFig.ret.sp)>0) {
        fig.ret.sp <- c("\\begin{figure}[H]",
                         "\\centering",
                         paste("\\includegraphics[width=1\\textwidth]{",nomFig.ret.sp,"}",sep=""),
                         paste("\\caption [Variation du taux de retour pour les principales espèces] {Variation du taux de retour pour la station ",s,". Le taux de retour de l'année $t$ est la proportion des individus qui sont revus à l'année $t+1$. En bleu, la variation des quantiles de l'indicateur national (médiane, 50\\% et 95\\%), en rouge et orange la variation pour la station ",s,"}",sep=""),  
                         "\\label{retSP}",
                         "\\end{figure}") } else {fig.ret.sp <- ""}



#browser()
    

    fileConn<-file(paste("makePDFs/station_",s,".tex",sep=""))
    writeLines(c(entete,
                 startDoc,
                 intro,
                 fig.carte,
                 fig.ses,
                 np,
                 startResult,
                 tex.com,
                 fig.com,
                 tex.lect,
                 tex.ab,
                 fig.ab.gr,
                 fig.ab.sp,
                 np,
                 tex.prod,
                 fig.prod.gr,
                 fig.prod.sp,
                 np,
                 tex.bc,
                 fig.bc.gr,
                 fig.bc.sp,
                 np,
                 tex.ret,
                 fig.ret.gr,
                 fig.ret.sp,
                 endDoc),
               fileConn)
    close(fileConn)

    if(system == "linux") {
        fileTex <- paste("station_",s,".tex",sep="")
        
        setwd("makePDFs")
        cmdTex <- paste("pdflatex -interaction=batchmode ",fileTex,sep="")
        system(cmdTex)
        system(cmdTex)
      
        filePDF <- paste("station_",s,".pdf",sep="")
        cmdMove <- paste("cp ",filePDF," ../output/",s,sep="")
        system(cmdMove)
        cmdMove <- paste("cp ",filePDF," ../lesPDFs",sep="")
        system(cmdMove)
        
        setwd("../")
    }
    
}


