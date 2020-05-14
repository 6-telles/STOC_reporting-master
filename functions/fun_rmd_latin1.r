
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



run.rmd <- function(file.rmd="functions/rmd_stoc_reporting_utf8.rmd",rep.out="output_rapport_pdf",file.data="data_DB/data3session.csv",file.out=NULL,year = NULL, id_station = "205",format_output="pdf",prefixe_file="",fileLog =NULL,save.fig=FALSE){


#file.rmd="functions/rmd_stoc_reporting_utf8.rmd";rep.out="output_rapport_pdf";file.data="data_DB/data3session.csv";file.out=NULL;year = 2018; id_station = "205";format_output="pdf"


    if(is.null(year)) year <- as.numeric(substr(Sys.time(),1,4))
    if(is.null(prefixe_file)) prefixe_file <- paste0(format(Sys.time(),"%Y-%m-%d_%H-%M"),"_")
    if(is.null(file.out)) file.out <- paste0("stoc_reporting_",prefixe_file,year,"_",id_station,".",format_output)
    if(!is.null(rep.out)) file.out <- paste0(rep.out,"/",file.out)
     format <- paste0(format_output,"_document")

    rep <- getwd()

     cat("rmd :",file.rmd,"\n")
    cat("file.data :",file.data,"\n")
    cat("rep:", rep,"\n")
     cat("id_station :",id_station,"\n")
     cat("output :",file.out,"\n")
     if(!(rep.out) %in% dir()) {
         cat("\n Le repertoire de sortie:",rep.out,"est manquant\n")
         dir.create(rep.out,showWarnings=FALSE)
         cat("\n Répertoire créer !!\n")
     }

      rmarkdown::render(file.rmd,output_file=file.out,output_dir=rep.out,output_format = format,clean=FALSE,encoding="utf-8",params = list(set_rep = rep, set_file_data = file.data,set_fileLog = fileLog,set_site = id_station, set_year = year, set_save_fig = save.fig))

     cat("DONE !!!\n")
}



