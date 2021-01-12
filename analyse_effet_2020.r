library(data.table)

d <- fread("data_DB/TableauStats.csv")
head(d)
## age en colonne
dd <- dcast(d, HABITAT + REGBIOGEO + SP + YEAR ~ AGE_first, value.var="Nb")
setDT(dd)
## on remplace les NA par 0
dd[is.na(dd)] <- 0
## Calcul de la productivit¨¦
dd[,prod := JUV / AD]
## ajout d¡¯un champ ann¨¦e en texte de _2020 pour que 2020 soit consid¨¦r¨¦ dans l¡¯intercept du model stat
dd[,year_txt := as.character(YEAR)]
dd[YEAR == 2020, year_txt := "_2020"]
head(dd)

## selection de donn¨¦es finies pour le mod¨¨le
d_mod  <- dd[prod != Inf ,]

md <- glm(prod ~ year_txt , data = d_mod)
summary(md)
