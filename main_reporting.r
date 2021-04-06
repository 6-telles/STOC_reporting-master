source("functions/fun_generic_latin1.r")
source("functions/fun_indicateurs_france_latin1.r")
source("functions/fun_indicateurs_local_latin1.r")
source("functions/fun_rmd_latin1.r")
source("functions/fun_mainSTOCreporting_latin1.r")
source("functions/fun_stoc_preparation_data_latin1.r")

mainSTOCreporting(importationData="brut",all=FALSE,local=TRUE,site="203",pdf_local=TRUE,abondanceRelative=TRUE ,variationAbondance=TRUE,variationAbondanceEspece=TRUE,productivite=TRUE,productiviteEspece=TRUE,conditionCorporelle=TRUE,retour=TRUE,onlyNew=TRUE)
