     for(ss in unique(session.conserve$NEW.ID_PROG)) {
        session.cons.ss <- subset( session.conserve, NEW.ID_PROG == ss)
        for(y in unique(session.cons.ss$YEAR)) {
            tYear <- subset(session.cons.ss,YEAR == y)
            session.select[paste(ss,y,sep="_"),3:5] <- sort(tYear$JULIANDAY)
        }
    }

     
     
     
     
        dateRef <- tDateRef[ss,]
         session.sup.ss <- subset( session.sup, NEW.ID_PROG == ss)

        
        if(nrow(session.sup.ss)>0) {
            years <- unique(session.sup.ss$YEAR)
            for(y in years) {
                ## recherche des date de session optimal pour chaque annee
                 tYear <- subset(session.sup.ss,YEAR == y)
                if(length(which(tYear$VALIDE)) == 3) {
                    ##  seulement 3 sessions valides
                    session.select[paste(ss,y,sep="_"),3:5] <- sort(subset(tYear,VALIDE)$JULIANDAY)
                } else { #else if(length(which(tYear$VALIDE)) == 3) 
                    if(length(which(tYear$VALIDE))>3) {
                        ## plus de 3 sessions valides
                            tYear <- subset(tYear,VALIDE)
                      
                        vSession <- tYear$JULIANDAY
                        vi1 <- vSession[1:(length(vSession)-2)]
                        vi2 <- vSession[2:(length(vSession)-1)]
                        vi3 <- vSession[3:(length(vSession))]
                        
                        di <- expand.grid(i1=vi1,i2=vi2,i3=vi3)
                        di <- as.matrix(subset(di,(i1 + seuilExclusionDelai) < i2 & (i2 + seuilExclusionDelai) < i3))
                        ## si pas de solution avec au moins 15 jours d ecart entre session
                        if(nrow(di)<1)  {
                            di <- expand.grid(i1=vi1,i2=vi2,i3=vi3)
                            di <- as.matrix(subset(di,(i1) < i2 & (i2) < i3))
                            }
                        dRef <- matrix(rep(as.numeric(dateRef),each=nrow(di)),ncol=3)
                        ddif <- abs(di-dRef)
                        sumDiff <- rowSums(ddif)
                        i <- which(sumDiff==min(sumDiff))
                        if(length(i)>1) {
                            ## si deux solutions optomale on choisi celle avec le plus d evenement de capture
                            sumEv <- 0
                            for(ii in i) {
                                sumI <- sum(tYear$nb_evenement[tYear$JULIANDAY %in% di[ii,]])
                                if(sumI > sumEv) {
                                    i2 <- ii
                                    sumEv <-  sumI
                                } #end if(sumI > sumEv)
                            } # end for(ii in i)
                        i <-i2 
                        } #end if(length(i) > 1)
                        session.select[paste(ss,y,sep="_"),3:5] <- di[i,]
                        
                        
                    } else { #else if(length(which(tYear$VALIDE))>3)
                        ## si moins de 3 sessions valides
                        vSession <- tYear$JULIANDAY
                        vi1 <- vSession[1:(length(vSession)-2)]
                        vi2 <- vSession[2:(length(vSession)-1)]
                        vi3 <- vSession[3:(length(vSession))]
                       di <- expand.grid(i1=vi1,i2=vi2,i3=vi3)
                        di <- as.matrix(subset(di,(i1 + seuilExclusionDelai) < i2 & (i2 + seuilExclusionDelai) < i3))
                        if(nrow(di)<1) {
                            di <- expand.grid(i1=vi1,i2=vi2,i3=vi3)
                            di <- as.matrix(subset(di,(i1) < i2 & (i2) < i3))
                        } #end if(nrow(di)<1)
                        vValide <- tYear$VALIDE
                         if(length(vSession[vValide])>0) {
                            ## recherche pour avoir tout les session valide
                            di.sub <- subset(di,apply(di,1,FUN = function(x) all(vSession[vValide] %in% x)))
                            if(nrow(di.sub) == 0) {
                             
                                ## au moins une session valide
                                di.sub <- subset(di,apply(di,1,FUN = function(x) any(vSession[vValide] %in% x)))
                            }
                            if(nrow(di.sub) > 0) di <- di.sub
                          
                        }#end if(length(vSession[vValide])>0)

                          dRef <- matrix(rep(as.numeric(dateRef),each=nrow(di)),ncol=3)
                        ddif <- abs(di-dRef)
                        sumDiff <- rowSums(ddif)
                        i <- which(sumDiff==min(sumDiff))
                        if(length(i)>1) {
                            ## si deux solutions optomale on choisi celle avec le plus d evenement de capture
                            sumEv <- 0
                            for(ii in i) {
                                sumI <- sum(tYear$nb_evenement[tYear$JULIANDAY %in% di[ii,]])
                                if(sumI > sumEv) {
                                    i2 <- ii
                                    sumEv <-  sumI
                                }# end  if(sumI > sumEv)
                            }#end for(ii in i)
                        i <-i2 
                        } # end if(length(i)>1)
                      session.select[paste(ss,y,sep="_"),3:5] <- di[i,]
 
                    } #end else if(length(which(tYear$VALIDE))>3)
                 } # end year
            }
            
        }

