
# run.rmd()

run.rmd <- function(file.rmd="stoc_reporting.rmd",rep.out="rapport_pdf",file.out=NULL,year = NULL, id_station = "205",format_output="pdf"){
     if(is.null(year)) year <- as.numeric(substr(Sys.time(),1,4))
    if(is.null(file.out)) file.out <- paste0("stoc_reporting_",id_station,"_",year,"_",format(Sys.time(),"%Y-%m-%d_%H-%M"),".",format_output)
    if(!is.null(rep.out)) file.out <- paste0(rep.out,"/",file.out)
     format <- paste0(format_output,"_document")

    cat("rmd :",file.rmd,"\n")
    cat("id_station :",id_station,"\n")
     cat("output :",file.out,"\n")
        if(!(rep.out) %in% dir()) {
        cat("\n Le repertoire de sortie:",rep.out,"est manquant\n")
        dir.create(rep.out,showWarnings=FALSE)
        cat("\n Répertoire créer !!\n")
        }

    rmarkdown::render(file.rmd,output_file=file.out,output_format = format,clean=FALSE,encoding="utf-8",
                      params = list(year=year,id_station= id_station))#,set_output=format_output))#,set_title=title))
    cat("DONE !!!\n")
}


