library(data.table)

d <- fread("data_DB/TableauStats.csv")
head(d)
dd <- dcast(d, HABITAT + REGBIOGEO + SP + YEAR ~ AGE_first, value.var="Nb")
setDT(dd)
dd[is.na(dd)] <- 0
dd[,prod := JUV / AD]
dd[,year_txt := as.character(YEAR)]
dd[YEAR == 2020, year_txt := "_2020"]
head(dd)

d_mod  <- dd[prod != Inf ,]

md <- glm(prod ~ year_txt , data = d_mod)
summary(md)
