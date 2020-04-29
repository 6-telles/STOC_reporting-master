
### run.rmd()


## source("functions/fun_generic_latin1.r")
source("functions/fun_indicateurs_local_latin1.r")

vecPackage=c("data.table","devtools","dplyr","ggplot2","ggpubr","lubridate","reshape2","knitr","kableExtra","pander")

ip <- installed.packages()[,1]

for(p in vecPackage) {
    if (!(p %in% ip))
        install.packages(pkgs=p,repos = "http://cran.univ-paris1.fr/",dependencies=TRUE)
    require(p,character.only=TRUE)
}



run.rmd <- function(file.rmd="functions/rmd_stoc_reporting_utf8.rmd",rep.out="output_rapport_pdf",file.data="data_DB/data3session.csv",file.out=NULL,year = NULL, id_station = "205",format_output="pdf"){


file.rmd="functions/rmd_stoc_reporting_utf8.rmd";rep.out="output_rapport_pdf";file.data="data_DB/data3session.csv";file.out=NULL;year = 2018; id_station = "205";format_output="pdf"


     if(is.null(year)) year <- as.numeric(substr(Sys.time(),1,4))
    if(is.null(file.out)) file.out <- paste0("stoc_reporting_",id_station,"_",year,"_",format(Sys.time(),"%Y-%m-%d_%H-%M"),".",format_output)
    if(!is.null(rep.out)) file.out <- paste0(rep.out,"/",file.out)
     format <- paste0(format_output,"_document")

     cat("rmd :",file.rmd,"\n")
     cat("file.data :",file.data,"\n")
     cat("id_station :",id_station,"\n")
     cat("output :",file.out,"\n")
     if(!(rep.out) %in% dir()) {
         cat("\n Le repertoire de sortie:",rep.out,"est manquant\n")
         dir.create(rep.out,showWarnings=FALSE)
         cat("\n Répertoire créer !!\n")
     }

    rep <- "C:/git/STOC_reporting/"
    file_data <- "data_DB/data3session.csv"
    fileLog <- NULL
    site <- "210"
    year <- 2018
    save.fig <- FALSE

    rmarkdown::render(file.rmd,output_file=file.out,output_format = format,clean=FALSE,encoding="utf-8",params = list(set_rep = rep, set_file_data = file_data,set_fileLog = fileLog,set_site = site, set_year = year, set_save_fig = save.fig))

     cat("DONE !!!\n")
}




##
##```{r fig.ab, eval=TRUE,fig.align = 'center', out.width='75%',fig.cap=paste("\\label{fig:ab}Variation du nombre d'adultes capturés pour le station",site," comparé aux STOC-Capture de type ",hab_txt,",sep=""),fig.scap="Variation du nombre d'adultes"}
##
##speciesRelativeAbund.site(d,site=site,fileLog=fileLog,print.fig=TRUE,save.fig=FALSE,add_title=FALSE))
##
##```
##
##
##```{r fig.absp, eval=TRUE,fig.align = 'center', out.width='75%',fig.cap=paste("\\label{fig:ab}Variation du nombre d'adultes capturés pour le station",site," comparé aux STOC-Capture de type ",hab_txt,",sep=""),fig.scap="Variation du nombre d'adultes par espèce"}
##
##speciesRelativeAbund.site(d,site=site,fileLog=fileLog,print.fig=TRUE,save.fig=FALSE,add_title=FALSE))
##
##```
##
