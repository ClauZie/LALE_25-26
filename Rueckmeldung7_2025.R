# Vorbereitungen ----------------------------------------------------------
rm(list = ls())

myPath <- c('C:/Rcz/libR')
#myPath <- c('C:/Rat/libR')
.libPaths(myPath)

setwd("C:/RShare/git/LALE5_24-25")

library(readxl)
library(tibble) # add_column
library(haven) # write_sav
library(stringi)
library(kableExtra)
library(dplyr)
library(knitr)
library(pdftools)
library(RColorBrewer)

##### Schulrueckmeldung ####


schulrueckmeldung_LALE7 <- function(Auswertung, lale_empirisch, schule, schuljahr, leitidee_tabelle, regelstandard, pdf=FALSE, logo_dir = "/home/philipp/Desktop/LALE 5 -RCode/Code/Files", word, Legenden=Legenden, ext)
{
  
  leitidee_tabelle<-lapply(leitidee_tabelle,apply,2,as.character)
  
  ## Variable mittel_leitidee muss die Spaltennamen ML1 bis ML5 enthalten analog zu den Namen in lale_empirisch
  mittel_leitidee <- names(leitidee_tabelle)
  
  ## Farben fuer die Balken der verschiedenen Schulen
  farben <- c("#376092","#b9cde5","#17375e","#c0c0c0","#8eb4e3","#4f81bd","#a0a0a0","#7788bb","#474951","#020611")
  farben_schule <- farben[1:(nrow(unique(lale_empirisch[lale_empirisch$SCHULNUMMER==schule,"KLASSE"]))+2)]
  
  ## Vergleichsgruppe wird gesetzt f\"ur die jeweilige Schule
  referenzgruppe <- as.character(unlist(unique(lale_empirisch[which(lale_empirisch$SCHULNUMMER==schule),"referenzgruppe"])))
  vergleichsschulen <- do.call(paste,cbind("\\item Schule",unique(lale_empirisch[grep(referenzgruppe,lale_empirisch$Vergleichsgruppe),"SCHULNUMMER"]),"-",unique(lale_empirisch[grep(referenzgruppe,lale_empirisch$Vergleichsgruppe),"NAME_SCHULE"])))
  
  ## Die Schulart wird ausgelesen f\"ur die jeweilige Schule
  if (unlist(unique(lale_empirisch[which(lale_empirisch$SCHULNUMMER==schule),"schulart"]))==1){schulart <- 1; names(schulart) <- "Oberschulen"}
  if (unlist(unique(lale_empirisch[which(lale_empirisch$SCHULNUMMER==schule),"schulart"]))==2){schulart <- 2; names(schulart) <- "Gymnasien"}
  
  ## Wird die Oberschule mit im Schulvergleich aufgef\"uhrt oder nicht siehe Seite 6 im pdf
  if (referenzgruppe==1){Oberschulenvergleich <- paste("Zur Einordnung werden Ihnen zus\"atzlich der Wert aller", nrow(unique(lale_empirisch[which(lale_empirisch$schulart==schulart),"SCHULNUMMER"])), "Oberschulen und der Wert aller",  nrow(unique(lale_empirisch[,"SCHULNUMMER"])), "an LALE teilnehmenden Schulen angezeigt.")} else {Oberschulenvergleich <- paste("Zur Einordnung wird Ihnen zus\"atzlich der Wert aller",nrow(unique(lale_empirisch[,"SCHULNUMMER"])), "an LALE teilnehmenden Schulen angezeigt.")}
  
  ## Ist der Unterschied zur Vergleichsgruppe bedeutsam oder nicht bedeutsam
  if (abs(round(mean(unlist(lale_empirisch[which(lale_empirisch$SCHULNUMMER==schule),"delwlemeanbista"]),na.rm=TRUE))-round(mean(unlist(lale_empirisch[grep(referenzgruppe,lale_empirisch$Vergleichsgruppe),"delwlemeanbista"]),na.rm=TRUE)))<30) {bedeutsam <- "NICHT BEDEUTSAM"} else {bedeutsam <- "BEDEUTSAM"}
  if (abs(round(mean(unlist(lale_empirisch[which(lale_empirisch$SCHULNUMMER==schule),"mathewle_Bista"]),na.rm=TRUE))-round(mean(unlist(lale_empirisch[grep(referenzgruppe,lale_empirisch$Vergleichsgruppe),"mathewle_Bista"]),na.rm=TRUE)))<30) {bedeutsam_mathe <- "NICHT BEDEUTSAM"} else {bedeutsam_mathe <- "BEDEUTSAM"}
  
  ## Werte f\"ur Tabelle Bedeutsam nicht Bedeutsam werden berechnet f\"ur Seite 7 und 14 im pdf
  klassenwert <- function(klasse, spalte="delwlemeanbista")
  {if (abs(round(mean(unlist(lale_empirisch[which(lale_empirisch$Klassen_kenn==klasse),spalte]),na.rm=TRUE))-round(mean(unlist(lale_empirisch[grep(referenzgruppe,lale_empirisch$Vergleichsgruppe),spalte]),na.rm=TRUE)))<30) 
  {return("NICHT BEDEUTSAM")} else {return("BEDEUTSAM")}}
  
  ## Erklaerende Grafiken speziell fuer LALE7 
  Grafik_A <- function(data_Start=445, data_Ende=524)
  {
    par(oma=c(0,0,0,0),mar=c(4,3,3,2))
    w<-1
    m <- barplot(data_Start, col = NA, border=F, axes = TRUE, horiz = T, plot=T, width=w, space=2, xlim = c(200,800), ylim=c(1.2,3.5),las=1)
    abline(v = c(300,400,500,600,700), col = "darkgray", lty = 1, xpd = F)
    rect(xleft = data_Start, xright = data_Ende, ybottom = m-w/2, ytop = m+w/2, col =c("#8eb4e3"), xpd=T)
    segments(x0 = data_Start, y0 = m-w/2-0.3, x1 = data_Start, y1 = m+w/2+0.3, lwd=3)
    text(paste("(",sprintf("%+g",round(data_Ende)-round(data_Start)),")",sep=""), x = data_Ende+sign(data_Ende-data_Start)*65, y = m)
    text(round(data_Start), x = data_Start+sign(data_Ende-data_Start)*-20, y = m)
    text(round(data_Ende), x = data_Ende+sign(data_Ende-data_Start)*20, y = m)
  }
  
  # Grafik zur Erklaerung der Alternative zu Kompetenzstufen
  Grafik_B <- function(regelstandard, farben)
  {
    farben = farben[-1]
    legend.vector <- seq(200,800, 100)
    
    par(oma=c(0,0,0,0),mar=c(0,0,0,0))
    m<-plot(1,ylim=c(1,4), xlim=c(150,850),pch=NA,axes=F,type = "n")
    # legend.vector <- c(regelstandard[-c(1,length(regelstandard))],800)
    
    rect(xleft = legend.vector[-length(legend.vector)], xright = legend.vector[-1], ybottom = rep(2,7), ytop = rep(2.9,7), col = farben, xpd=T)
    text(x=legend.vector,y=rep(3.2,length(legend.vector)),labels=c("",legend.vector[-c(1,length(legend.vector))],""), cex=1.2, xpd=T)
    # text(x=legend.vector,y=rep(3.2,length(legend.vector)),labels=c("",regelstandard[-c(1,2,length(regelstandard))],""), cex=1.2, xpd=T)
  }
  
  ## Code f\"ur Plot 1,2,6 und 7 der Schulr\"uckmeldung: Barplot
  schule_plot1_2_6_7 <- function(lale_empirisch,klasse=TRUE, schule, referenzgruppe, Datenspalte, schulart_wert=schulart)
  {
    par(mar = c(3, 3, 3, 3)) # par(mar = c(bottom, left, top, right))
    if (klasse==TRUE) 
    {klassen_in_schule <- split(lale_empirisch[which(lale_empirisch$SCHULNUMMER==schule),c("KLASSE",Datenspalte)],lale_empirisch[which(lale_empirisch$SCHULNUMMER==schule),c("KLASSE",Datenspalte)][,1])
    data <- c(lapply(lapply(klassen_in_schule,"[",c(-1)),unlist), lale_empirisch[grep(referenzgruppe,lale_empirisch$Vergleichsgruppe),Datenspalte])
    klassen_wert <- unlist(lapply(lapply(lapply(klassen_in_schule,"[",c(-1)),unlist),mean,na.rm=T))-mean(unlist(lale_empirisch[grep(referenzgruppe,lale_empirisch$Vergleichsgruppe),Datenspalte]),na.rm=T)
    if (length(klassen_in_schule)>5){cex_x_axis <- 1-((length(klassen_in_schule)-5)/10)}else{cex_x_axis <- 1} }
    else 
    {data <- c(lale_empirisch[which(lale_empirisch$SCHULNUMMER==schule),Datenspalte], lale_empirisch[grep(referenzgruppe,lale_empirisch$Vergleichsgruppe),Datenspalte], lale_empirisch[which(lale_empirisch$schulart==schulart_wert),Datenspalte], lale_empirisch[,Datenspalte])
    klassen_wert <- diff(unlist(lapply(data,mean,na.rm=T)[c(2,1)]))}
    data <- lapply(data,na.omit)
    data <- lapply(data,unlist)
    barplot(unlist(lapply(data,mean,na.rm=TRUE)), col = NA, border = NA, space=1, axes = FALSE, ylim=c(100,800), axisnames=FALSE, xpd=FALSE)
    abline(h = c(100,200,300,400,500,600,700), col = "darkgray", lty = 1)
    barplot(unlist(lapply(data,mean,na.rm=TRUE)), axes = FALSE, add = TRUE, space=1, col = c("#10253f","#375a84","#8eb4e3"), ylim=c(100,800), xpd=FALSE, axisnames=FALSE)
    axis(side = 2, tick = TRUE, col = "darkgray", at = c(100,200,300,400,500,600,700,800), labels = c(100,200,300,400,500,600,700,800), lty = 1, las = 1)
    m <- barplot(unlist(lapply(data,mean,na.rm=TRUE)), col = NA, border = NA, axes = FALSE, space=1, plot=FALSE)
    text(x = m, y = unlist(lapply(data,mean,na.rm=TRUE))+50, labels = round(unlist(lapply(data,mean,na.rm=TRUE)),0))
    if (klasse==TRUE) {axis(cex.axis = cex_x_axis, side = 1, at = m, tick=FALSE, labels = c(paste(names(klassen_in_schule),"\n( N =",lapply(klassen_in_schule,nrow),")"), paste("Vergleichsgruppe\n( N =",length(data[[length(data)]]),")")))}
    if (klasse==FALSE) {axis(side = 1, at = m, tick=FALSE, labels = c(paste("Ihre Schule\n( N =",length(data[[1]]),")"), paste("Vergleichsgruppe\n( N =",length(data[[2]]),")"), paste(nrow(unique(lale_empirisch[which(lale_empirisch$schulart==schulart_wert),"SCHULNUMMER"])),names(schulart_wert),"\n( N =",length(data[[3]]),")"), paste(nrow(unique(lale_empirisch[,"SCHULNUMMER"])),"Schulen\n( N =",length(data[[4]]),")")))}
    
    # Bedeutsam - Pfeile
    if(TRUE%in%(klassen_wert>=30)){arrows(x0 = m[which(klassen_wert>30)], y0 = 800, y1 = 850, x1 = m[which(klassen_wert>30)], xpd=T, lwd = 2, cex =10)}
    if(TRUE%in%(klassen_wert<=(-30))){arrows(x0 = m[which(klassen_wert<(-30))], y0 = 850, y1 = 800, x1 = m[which(klassen_wert<(-30))], xpd=T, lwd = 2, cex =10)}
    
  }
  
  ## Plot 3, 4 und 8, 9 Schulr\"uckmeldung: Stacked-Barplot
  schule_plot3_4_8_9 <- function(lale_empirisch,klasse=TRUE,schule=schule,referenzgruppe,Datenspalte,regelstandard,schulart_wert=schulart, farben)
  {
    par(oma = c(6, 4, 2, 0)) # par(mar = c(bottom, left, top, right))
    
    if (klasse==TRUE) 
    {klassen_in_schule <- split(lale_empirisch[which(lale_empirisch$SCHULNUMMER==schule),c("KLASSE",Datenspalte)],lale_empirisch[which(lale_empirisch$SCHULNUMMER==schule),c("KLASSE",Datenspalte)][,1])
    data <- c(lapply(lapply(klassen_in_schule,"[",c(-1)),unlist), lale_empirisch[grep(referenzgruppe,lale_empirisch$Vergleichsgruppe),Datenspalte])
    if (length(klassen_in_schule)>5){cex_x_axis <- 1-((length(klassen_in_schule)-5)/10)}else{cex_x_axis <- 1} }
    else 
    {data <- c(lale_empirisch[which(lale_empirisch$SCHULNUMMER==schule),Datenspalte], lale_empirisch[grep(referenzgruppe,lale_empirisch$Vergleichsgruppe),Datenspalte], lale_empirisch[which(lale_empirisch$schulart==schulart_wert),Datenspalte], lale_empirisch[,Datenspalte])}
    
    data <- lapply(data,na.omit)
    data <- lapply(data,unlist)
    data.matrix <-as.matrix(100*do.call(cbind,(lapply(lapply(lapply(data,cut,breaks=regelstandard[-2]),table),prop.table))))
    
    barplot(data.matrix, col = NA, border = NA, axes = FALSE, main = "", space=1, width=1, axisnames=FALSE, horiz=T)
    m <- barplot(data.matrix, plot=T, axes = FALSE, add = TRUE, col=farben, space=1, width=1, axisnames=FALSE, horiz=T)
    perc <- paste(round(data.matrix),"%",sep="")
    perc[which(perc=="0%")]<- ""
    
    text(y = m, x = t(apply(data.matrix,2,cumsum)-data.matrix/2), labels = t(matrix(perc,ncol=length(data))), xpd = TRUE, col=c("black"))
    
    if (klasse==TRUE) {axis(cex.axis = cex_x_axis, side = 2, at = m, las=1, tick=FALSE, labels = c(paste(names(klassen_in_schule),"\n( N =",lapply(klassen_in_schule,nrow),")"), paste("Vergleichsgruppe\n( N =",length(data[[length(data)]]),")")))}
    if (klasse==FALSE) {axis(side = 2, at = m, las=1, tick=FALSE, labels = c(paste("Ihre Schule\n( N =",length(data[[1]]),")"), paste("Vergleichsgruppe\n( N =",length(data[[2]]),")"), paste(nrow(unique(lale_empirisch[which(lale_empirisch$schulart==schulart_wert),"SCHULNUMMER"])),names(schulart_wert),"\n( N =",length(data[[3]]),")"), paste(nrow(unique(lale_empirisch[,"SCHULNUMMER"])),"Schulen\n( N =",length(data[[4]]),")")))}
    
    legend.vector <- c(regelstandard[-c(1,length(regelstandard))],800)
    legend.vector <- legend.vector-legend.vector[1]
    legend.vector <- legend.vector/max(legend.vector)*100
    rect(xleft = legend.vector[-length(legend.vector)], xright = legend.vector[-1], ybottom = rep(-.5,7), ytop = rep(0,7), col = farben, xpd=T)
    text(x=legend.vector,y=rep(0.3,length(legend.vector)),labels=c("",regelstandard[-c(1,2,length(regelstandard))],""), cex=1.2, xpd=T)
  }
  
  ## Zusaetzlicher Plot 12.2021 fuer Regelstandards
  schule_plot_4_3 <- function(lale_empirisch,schule=schule,referenzgruppe,Datenspalte,regelstandard,schulart_wert=schulart)
  {
    
    ## Die R\"ander um den Plot werden definiert
    farben <- c("#FF9933","#FFFFFF")
    par(oma = c(0, 0, 5, 0)) # par(mar = c(bottom, left, top, right))
    
    klassen_in_schule <- split(lale_empirisch[which(lale_empirisch$SCHULNUMMER==schule),c("KLASSE",Datenspalte)],lale_empirisch[which(lale_empirisch$SCHULNUMMER==schule),c("KLASSE",Datenspalte)][,1])
    data <- c(lapply(lapply(klassen_in_schule,"[",c(-1)),unlist), lale_empirisch[which(lale_empirisch$SCHULNUMMER==schule),Datenspalte], lale_empirisch[grep(referenzgruppe,lale_empirisch$Vergleichsgruppe),Datenspalte])
    if (length(klassen_in_schule)>5){cex_x_axis <- 1-((length(klassen_in_schule)-5)/10)}else{cex_x_axis <- 1}
    
    # Entferne NAs
    data <- lapply(data,na.omit)
    data <- lapply(data,unlist)
    data.matrix <-as.matrix(100*do.call(cbind,(lapply(lapply(lapply(data,cut,breaks=regelstandard),table),prop.table))))
    data.matrix.N <-as.matrix(do.call(cbind,(lapply(lapply(data,cut,breaks=regelstandard),table))))
    barplot(data.matrix, col = NA, border = NA, axes = FALSE, main = "", space=1, width=1, axisnames=FALSE)
    abline(h = c(0,20,40,60,80,100), col = "darkgray", lty = 1)
    barplot(data.matrix, axes = FALSE, add = TRUE, col=farben, space=1, width=1, axisnames=FALSE)
    axis(side = 2, tick = TRUE, at = c(0,20,40,60,80,100), labels = paste(c(0,20,40,60,80,100), rep("%",6)), lty = 1, las = 1)
    perc <- paste(round(data.matrix),"%\nN=",data.matrix.N,sep="")
    perc[which(data.matrix.N==0)]<- ""
    m <- barplot(data.matrix, plot = FALSE , space=1, width=1, axisnames=FALSE)
    
    text(x = m, y = t(apply(data.matrix,2,cumsum)-data.matrix/2), labels = t(matrix(perc,ncol=length(data))), xpd = TRUE)
    axis(cex.axis = cex_x_axis, side = 1, at = m, tick=FALSE, labels = c(paste(names(klassen_in_schule),"\n( N =",lapply(klassen_in_schule,nrow),")"),paste("Ihre Schule\n( N =",length(data[[length(data)-1]]),")"), paste("VG\n( N =",length(data[[length(data)]]),")")))
    
    par(oma = c(0,0,0,0), new = TRUE)
    legend(x.intersp=1.5,legend=c("unter Regelstandard","mindestens Regelstandard"), fill = farben, x = "topleft", bty="n", ncol = 2)
    
  }
  ## Plot 4.1 und 9.1 Schulr\"uckmeldung: Stacked-Barplot mit n.v.
  
  schule_plot41_91 <- function(lale_empirisch,referenzgruppe,schule,Datenspalte,regelstandard, farben)
  {
    ## Die R\"ander um den Plot werden definiert
    ## muss gemacht werden da die grenzen nicht richtig stimmen bzw. nicht logisch sind in der Angabe
    #regelstandard <- regelstandard[-2]
    par(oma = c(6, 4, 2, 0)) # par(mar = c(bottom, left, top, right))
    klassen_in_schule <- split(lale_empirisch[which(lale_empirisch$SCHULNUMMER==schule),c("KLASSE",Datenspalte)],lale_empirisch[which(lale_empirisch$SCHULNUMMER==schule),c("KLASSE",Datenspalte)][,1])
    data <- c(lapply(lapply(klassen_in_schule,"[",c(-1)),unlist), lale_empirisch[grep(referenzgruppe,lale_empirisch$Vergleichsgruppe),Datenspalte])
    data <- lapply(data,unlist)
    data.NA <- lapply(lapply(data,cut,breaks=regelstandard[-2]),table)
    data.NA <- mapply(append,lapply(lapply(data,is.na),sum),data.NA,SIMPLIFY=FALSE)
    data.matrix <-as.matrix(100*do.call(cbind,(lapply(data.NA,prop.table))))
    barplot(data.matrix, col = NA, border = NA, axes = FALSE, main = "", space=1, width=1, axisnames=FALSE, horiz=T)
    m <- barplot(data.matrix, plot=T, axes = FALSE, add = TRUE, col=farben, space=1, width=1, axisnames=FALSE, horiz=T)
    perc <- paste(round(data.matrix),"%",sep="")
    perc[which(perc=="0%")]<- ""
    
    text(y = m, x = t(apply(data.matrix,2,cumsum)-data.matrix/2), labels = t(matrix(perc,ncol=length(data))), xpd = TRUE, col=c("black"))
    
    if (length(klassen_in_schule)>5){cex_x_axis <- 1-((length(klassen_in_schule)-5)/10)}else{cex_x_axis <- 1} 
    axis(cex.axis=cex_x_axis, side = 2, at = m, las=1, tick=FALSE, labels = c(paste(names(klassen_in_schule),"\n( N =",lapply(klassen_in_schule,nrow),")"), paste("Vergleichsgruppe\n( N =",length(data[[length(data)]]),")")))
    
    legend.vector <- c(regelstandard[-c(1,length(regelstandard))],800)
    legend.vector <- legend.vector-legend.vector[1]
    legend.vector <- legend.vector/max(legend.vector)*100
    rect(xleft = legend.vector[-length(legend.vector)], xright = legend.vector[-1], ybottom = rep(-.5,7), ytop = rep(0,7), col = farben[-1], xpd=T)
    text(x=legend.vector,y=rep(0.3,length(legend.vector)),labels=c("",regelstandard[-c(1,2,length(regelstandard))],""), cex=1.2, xpd=T)
    legend(x=0, y=max(m)+2.5, ,legend=c("n.v. = Wert nicht vorhanden"), fill = farben[1], bty="n", xpd=TRUE)
    
  }
  
  schule_plot5 <- function(lale_empirisch,klasse,schule,referenzgruppe,Datenspalte)
  {
    ## Die R\"ander um den Plot werden definiert
    par(mar = c(3, 5, 3, 3),oma = c(0,0,2,0)) # par(mar = c(bottom, left, top, right))
    
    klassen_in_schule <- split(lale_empirisch[which(lale_empirisch$SCHULNUMMER==schule),c("KLASSE",Datenspalte)],lale_empirisch[which(lale_empirisch$SCHULNUMMER==schule),c("KLASSE",Datenspalte)][,1])
    data <- c(lapply(klassen_in_schule,"[",c(-1)),list(lale_empirisch[which(lale_empirisch$SCHULNUMMER==schule),Datenspalte], lale_empirisch[grep(referenzgruppe,lale_empirisch$Vergleichsgruppe),Datenspalte]))
    perc.text <- do.call("cbind",lapply(lapply(lapply(data,colMeans,na.rm=T),matrix,nrow=3),function(x) x*100))
    data.matrix <- matrix(apply(perc.text, 2,function(x) c(x,rep(0,6))),nrow=6)[,-seq(0,100,3)]
    
    barplot(data.matrix, col = NA, border = NA, axes = FALSE, ylim=c(0,110), space=c(0.5,0.1))
    abline(h = c(0,20,40,60,80,100), col = "darkgray", lty = 1)
    m <- barplot(plot=T, data.matrix, axes = FALSE, add = TRUE, ylim=c(0,110), col = c("#10253f","#375a84","#8eb4e3","#464E51","gray","lightgray"), space=c(0.5,0.1))
    axis(side = 2, tick = TRUE, at = c(0,20,40,60,80,100), labels = paste(c(0,20,40,60,80,100), rep("%",6)), lty = 1, las = 1)
    perc <- matrix(paste(round(perc.text),"%"),nrow=3)
    perc[which(perc=="0 %")]<- ""
    if (length(klassen_in_schule)>5){cex_x_axis <- 0.7-((length(klassen_in_schule)-5)/10)}else{cex_x_axis <- 0.7}
    text(x = m, y = t(apply(perc.text,2,cumsum)-perc.text/2), labels = t(perc), cex = cex_x_axis, xpd = TRUE, col = c("white"))
    
    axis(cex.axis = cex_x_axis, side = 1, at = colMeans(matrix(m,nrow=2,byrow=F)), tick=FALSE, labels = c(paste(names(klassen_in_schule),"\n( N =",lapply(klassen_in_schule,nrow),")"),paste("Ihre Schule\n( N =",nrow(data[[length(data)-1]]),")"), paste("Vergleichsgruppe\n( N =",nrow(data[[length(data)]]),")")))
    par(oma = c(0,0,0,0), mar = c(3,5,3,3),new = TRUE)
    legend(x.intersp=1.5,title=c("Sachtext: Die Meisterdiebe"),legend=c(Legenden[1,4],Legenden[1,3],Legenden[1,2]), fill =  c("lightgray","gray","#464E51"), y = 145, x = median(m), bty="n", ncol = 1, xpd=T)
    legend(x.intersp=1.5,title=c("Sachtext: Bionik"),legend=c(Legenden[1,4],Legenden[1,3],Legenden[1,2]), fill =c("#8eb4e3","#375a84","#10253f"), y = 145, x = 1, bty="n", ncol = 1, xpd = T)
  }
  
  ## Code f\"ur die Plots in der Tabelle auf Seite 12 Abbildung im pdf ohne Caption
  
  schule_plot5.1234 <- function(lale_empirisch,klasse,schule,referenzgruppe,Datenspalte=c("ML5_Richtig_Mathe","ML5_Falsch_Mathe","ML5_99_Mathe"))
  {
    par(mar = c(2, 4, 2, 0)) # par(mar = c(bottom, left, top, right))
    klassen_in_schule <- split(lale_empirisch[which(lale_empirisch$SCHULNUMMER==schule),c("KLASSE",Datenspalte)],lale_empirisch[which(lale_empirisch$SCHULNUMMER==schule),c("KLASSE",Datenspalte)][,1])
    data <- c(lapply(klassen_in_schule,"[",c(-1)),list(Schule=lale_empirisch[which(lale_empirisch$SCHULNUMMER==schule),Datenspalte], Vergleichsgruppe=lale_empirisch[grep(referenzgruppe,lale_empirisch$Vergleichsgruppe),Datenspalte]))
    data.matrix <- do.call("cbind",lapply(lapply(lapply(data,colMeans, na.rm=TRUE),matrix,nrow=3),function(x) x*100))
    
    m <- barplot(plot=T, data.matrix, col = NA, border = NA, axes = FALSE, ylim=c(0,110), axisnames=FALSE)
    abline(h = c(0,20,40,60,80,100), col = "darkgray", lty = 1)
    barplot(data.matrix, axes = FALSE, add = TRUE,  col = c("#10253f","#375a84","#8eb4e3"), ylim=c(0,110), axisnames=FALSE)
    axis(side = 2, tick = TRUE, at = c(0,20,40,60,80,100), labels = paste(c(0,20,40,60,80,100), rep("%",6)), lty = 1, las = 1)
    perc <- matrix(paste(round(data.matrix),"%"),nrow=3)
    perc[which(perc=="0 %")]<- ""
    text(x = m, y = t(apply(data.matrix,2,cumsum)-data.matrix/2), labels = t(perc), cex = 1.2, xpd = TRUE, col = c("white"))
    axis(side = 1, at = m, tick = FALSE, labels = c(names(klassen_in_schule),"Schule","VG"))
    legend(inset=c(0,-0.25), legend=c(Legenden[2,2],Legenden[2,3],Legenden[2,4]), fill = c("#10253f","#375a84","#8eb4e3"), x = "topleft", bty="n", ncol = 3, xpd=TRUE)
    
  }
  
  ## Plot 10 Mathematik - Mittlere Leistungswerte nach Leitideen (BISTA-Punkte); Horizontaler gruppierter Barplot 
  
  schule_plot10 <- function(lale_empirisch,klasse,schule,referenzgruppe,Datenspalte=c("ML1","ML2","ML3","ML4","ML5"))
  { 
    Datenspalte <- rev(as.character(Datenspalte))
    opar = par(oma = c(1, 8, 7, 1), mar=c(3,0,0,2)) # par(mar = c(bottom, left, top, right))
    klassen_in_schule <- split(lale_empirisch[which(lale_empirisch$SCHULNUMMER==schule),c("KLASSE",Datenspalte)],lale_empirisch[which(lale_empirisch$SCHULNUMMER==schule),c("KLASSE",Datenspalte)][,1])
    data <- c(lapply(klassen_in_schule,"[",c(-1)),list(lale_empirisch[which(lale_empirisch$SCHULNUMMER==schule),Datenspalte], lale_empirisch[grep(referenzgruppe,lale_empirisch$Vergleichsgruppe),Datenspalte]))
    data <- rev(data)
    data.matrix <- t(do.call(cbind,lapply(data,colMeans,na.rm=TRUE))) 
    
    m <- barplot(plot=T, data.matrix, col = NA, border = NA, axes = FALSE, xlim=c(100,800), horiz=T, beside=T, axisnames=FALSE)
    abline(v = c(100,200,300,400,500,600,700), col = "darkgray", lty = 1, xpd=FALSE)
    barplot(data.matrix, axes = FALSE, add = TRUE, col = rev(farben_schule), ylim=c(0,4), xlim=c(100,800), horiz=T, beside=T, xpd=FALSE, axisnames=FALSE)
    axis(side = 1, tick = TRUE, at = c(100,200,300,400,500,600,700,800), labels = c(100,200,300,400,500,600,700,800), lty = 1, las = 1)
    text(y = m, x = data.matrix-30, labels = round(data.matrix), col="white", cex = 0.8)
    axis(las=1, side = 2, at = colMeans(m), labels = c("Leitidee 5:\nDaten und Zufall", "Leitidee 4:\nRaum und Form","Leitidee 3:\nFunktionaler\nZusammenhang ","Leitidee 2:\nMessen","Leitidee 1:\nZahl")) #2024 laut KMK geaendert
    
    par(oma = c(0,0,0,0), new = TRUE)
    legend(x.intersp=1,text.width = 80, legend=c(paste(names(klassen_in_schule),"\n( N =",lapply(klassen_in_schule,nrow),")"),paste("Ihre Schule\n( N =",nrow(data[[2]]),")"), paste("Vergleichs\ngruppe\n( N =",nrow(data[[1]]),")")), fill = farben_schule, x = "topleft", bty="n",ncol=6)
  }  
  
  schule_plot11_12_13_14_15  <- function(lale_empirisch,klasse,schule,referenzgruppe,Datenspalte=c("MA0005","MA0018","MA0016"),Leitidee=c("L1"),Aufgabennamen=c("Test1","Test2","Test3"))
  {
    klassen_in_schule <- lale_empirisch[which(lale_empirisch$SCHULNUMMER==schule),c("KLASSE",Datenspalte)]
    entferne.NA <- which(colSums(!sapply(klassen_in_schule,is.na))==0)
    if (length(entferne.NA)>0){
      Datenspalte <- Datenspalte[-entferne.NA]
      Aufgabennamen <- Aufgabennamen[-entferne.NA]}
    
    Aufgabennamen <- (as.character(Aufgabennamen)[order(Datenspalte)])
    Datenspalte <- sort(as.character(Datenspalte))
    opar = par(oma = c(3, 10, 3, 14), mar=c(0,0,0,2)) # par(mar = c(bottom, left, top, right))
    klassen_in_schule <- split(lale_empirisch[which(lale_empirisch$SCHULNUMMER==schule),c("KLASSE",Datenspalte)],lale_empirisch[which(lale_empirisch$SCHULNUMMER==schule),c("KLASSE",Datenspalte)][,1])
    data <- c(lapply(klassen_in_schule,"[",c(-1)),list(lale_empirisch[which(lale_empirisch$SCHULNUMMER==schule),Datenspalte], lale_empirisch[grep(referenzgruppe,lale_empirisch$Vergleichsgruppe),Datenspalte]))
    data <- rev(data)
    data <- lapply(data, function(x) sapply(x, factor, level=c(1,0,99,NA)))
    data.matrix <- lapply(lapply(data,function(x) apply(x,2, function(x) table(factor(x, levels=c(1,0,99))))), prop.table, margin=2)
    data.matrix <- as.matrix(do.call(rbind, lapply(data.matrix,function(x) t(x)*100)))
    data.matrix <- as.matrix(data.matrix[order(row.names(data.matrix),decreasing = F),])
    
    data.matrix.N <- lapply(lapply(data,function(x) apply(x,2, function(x) table(factor(x, levels=c(1,0,99))))), colSums)
    data.matrix.N <- as.matrix(do.call(rbind, lapply(data.matrix.N,function(x) t(x))))
    N.labels <- paste(rep(c("VG", "Schule", rev(names(klassen_in_schule))),length(Aufgabennamen)),rep(" (N=",length(Aufgabennamen)),data.matrix.N, rep(")",length(Aufgabennamen)),sep="")
    
    barplot(t(data.matrix), col = NA, border = NA, axes = FALSE, xlim=c(0,110), horiz=T, beside=F, space = rep(c(.5,rep(0.1,length(klassen_in_schule)+1))), axisnames = F)
    abline(v = c(0,20,40,60,80,100), col = "darkgray", lty = 1, xpd=FALSE)
    m <- barplot(plot=T, t(data.matrix), axes = FALSE, add = TRUE, col = c("#10253f","#375a84","#8eb4e3"), xlim=c(0,110), horiz=T, beside=F, xpd=TRUE, space = rep(c(.5,rep(0.1,length(klassen_in_schule)+1))), axisnames = F)
    axis(side = 1, tick = TRUE, at = c(0,20,40,60,80,100), labels = paste(c(0,20,40,60,80,100), rep("%",6)), lty = 1, las = 1)
    perc <- paste(round(data.matrix),"%",sep="")
    perc[which(perc=="0%")]<- ""
    text(y = m, x = t(apply(data.matrix,1,cumsum))-data.matrix/2, labels = perc, xpd=TRUE, col="white")
    axis(las=1, side = 4, at = m[seq(round((length(klassen_in_schule)+2)/2),length(m),(length(klassen_in_schule)+2))], labels = as.character(Aufgabennamen), pos=100, tick=F)
    axis(las=1, side = 2, at = m, labels = N.labels, , cex.axis=0.6, pos=-5)
    
    par(oma = c(0,0,0,0), mar=c(0,0,0,0),new = TRUE)
    legend(legend=c(Legenden[1,2],Legenden[1,3],Legenden[1,4]), fill = c("#10253f","#375a84","#8eb4e3"), x = "topleft", bty="n", ncol = 3)
    
  }
  
  ## Kondition um die letzten Balkendiagramme nach Leitideen zu splitten
  if (nrow(unique(lale_empirisch[which(lale_empirisch$SCHULNUMMER==schule),"KLASSE"])) > 2) {N_Klassen <- "\\klassenlongtrue" }else{ N_Klassen <- "\\klassenlongfalse"}
  
  ## Remove NAs in plot chunk in Rnw file, if not set NAs will be used for plotting
  lale_emp_DL <- lale_empirisch[which(!is.na(lale_empirisch$delwlemeanbista)==TRUE),]
  lale_emp_MA <- lale_empirisch[which(!is.na(lale_empirisch$mathewle_Bista)==TRUE),]
  
  ## Nur fuer LALE7, die vergelichende Grafik 1 Leistungsentwicklung klasse=FALSE
  ## Nur fuer LALE7, die vergelichende Grafik 2 Leistungsentwicklung der Parallelklassen klasse=TRUE
  
  klasse_plot_LALE7_2 <- function(lale_empirisch,klasse,schule,referenzgruppe,schulart_wert=schulart,Datenspalte_Start,Datenspalte_Ende)
  {
    ## Entferne alle Individuen mit NAS aus LALE 5 im individuellen Plot bleiben sie erhalten
    lale_empirisch <- lale_empirisch[which(!is.na(lale_empirisch[,Datenspalte_Start])==TRUE),]
    
    par(mar = c(3, 10, 3, 1)) # par(mar = c(bottom, left, top, right))
    
    if (klasse==TRUE) 
    {data_Start_N <- split(lale_empirisch[which(lale_empirisch$SCHULNUMMER==schule),c("KLASSE",Datenspalte_Start)],lale_empirisch[which(lale_empirisch$SCHULNUMMER==schule),c("KLASSE",Datenspalte_Start)][,1])
    data_Start <- c(unlist(lapply(lapply(lapply(data_Start_N,"[",c(-1)),unlist),mean,na.rm=TRUE)),Vergleichsgruppe=colMeans(lale_empirisch[grep(referenzgruppe,lale_empirisch$Vergleichsgruppe),Datenspalte_Start],na.rm=T))
    data_Ende_N <- split(lale_empirisch[which(lale_empirisch$SCHULNUMMER==schule),c("KLASSE",Datenspalte_Ende)],lale_empirisch[which(lale_empirisch$SCHULNUMMER==schule),c("KLASSE",Datenspalte_Ende)][,1])
    data_Ende <- c(unlist(lapply(lapply(lapply(data_Ende_N,"[",c(-1)),unlist),mean,na.rm=TRUE)),Vergleichsgruppe=colMeans(lale_empirisch[grep(referenzgruppe,lale_empirisch$Vergleichsgruppe),Datenspalte_Ende],na.rm=T))
    names(data_Start)<-c(paste(names(data_Start_N),"\n( N =",lapply(data_Start_N,nrow),")"), paste("Vergleichsgruppe\n( N =",nrow(lale_empirisch[grep(referenzgruppe,lale_empirisch$Vergleichsgruppe),Datenspalte_Start]),")"))
    }else{
      data_Start_N <- lapply(list(lale_empirisch[which(lale_empirisch$SCHULNUMMER==schule),Datenspalte_Start], lale_empirisch[grep(referenzgruppe,lale_empirisch$Vergleichsgruppe),Datenspalte_Start], lale_empirisch[which(lale_empirisch$schulart==schulart_wert),Datenspalte_Start], lale_empirisch[,Datenspalte_Start]),na.omit)
      data_Start <- unlist(lapply(lapply(data_Start_N,unlist),mean,na.rm=TRUE))
      data_Ende_N <- lapply(list(lale_empirisch[which(lale_empirisch$SCHULNUMMER==schule),Datenspalte_Ende], lale_empirisch[grep(referenzgruppe,lale_empirisch$Vergleichsgruppe),Datenspalte_Ende], lale_empirisch[which(lale_empirisch$schulart==schulart_wert),Datenspalte_Ende], lale_empirisch[,Datenspalte_Ende]),na.omit)
      data_Ende <- unlist(lapply(lapply(data_Ende_N,unlist),mean,na.rm=TRUE))
      names(data_Start) <- c(paste("Ihre Schule\n( N =",nrow(data_Start_N[[1]]),")"), paste("Vergleichsgruppe\n( N =",nrow(data_Start_N[[2]]),")"), paste(nrow(unique(lale_empirisch[which(lale_empirisch$schulart==schulart_wert),"SCHULNUMMER"])),names(schulart_wert),"\n( N =",nrow(data_Start_N[[3]]),")"), paste(nrow(unique(lale_empirisch[,"SCHULNUMMER"])),"Schulen\n( N =",nrow(data_Start_N[[4]]),")"))
    }
    
    w <- 1.5
    m <- barplot(data_Start, col = NA, border=F, axes = TRUE, horiz = T, plot=T, width=w, space=2, xlim = c(200,800), las=1)
    abline(v = c(300,400,500,600,700), col = "darkgray", lty = 1, xpd = F)
    rect(xleft = data_Start, xright = data_Ende, ybottom = m-w/2, ytop = m+w/2, col =c("#376092","#b9cde5","#17375e","#c0c0c0","#8eb4e3"), xpd=T)
    segments(x0 = data_Start, y0 = m-w/2-0.3, x1 = data_Start, y1 = m+w/2+0.3, lwd=3)
    text(paste("(",sprintf("%+g",round(data_Ende)-round(data_Start)),")",sep=""), x = data_Ende+sign(data_Ende-data_Start)*65, y = m)
    text(round(data_Start), x = data_Start+sign(data_Ende-data_Start)*-20, y = m)
    text(round(data_Ende), x = data_Ende+sign(data_Ende-data_Start)*20, y = m)
  }
  
  ## Erstelle .Rnw File fuer knit2pdf Konvertierung
  
  inc <- function(plot){ if (plot%in%ext$Grafik) 
  { 
    return(paste0("\\begin{figure} 
                     \\textbf{",ext[which(ext$Grafik==plot),"Legende"],"}
                     \\newline 
                     \\includegraphics[width=",ext[which(ext$Grafik==plot),"Breit"],"cm",
                  ",height=",ext[which(ext$Grafik==plot),"Hoch"],"cm",
                  "]{",logo_dir,"/LALE/data/",ext[which(ext$Grafik==plot),"Dateiname"],"} 
                     \\end{figure}"
    )
    )
  }}
  
  inc.tab <- function(plot){ if (plot%in%ext$Grafik) 
  { 
    return(paste0("\\includegraphics[width=",ext[which(ext$Grafik==plot),"Breit"],"cm",
                  ",height=",ext[which(ext$Grafik==plot),"Hoch"],"cm",
                  "]{",logo_dir,"/LALE/data/",ext[which(ext$Grafik==plot),"Dateiname"],"}"
    )
    )
  }}
  
  ## ADD True Argumente
  
  
  filename <- file(paste("LALE-Rueckmeldung_Schule_",schule,".Rnw",sep=""))
  
  
  
  ## Latex Dokument wird mittels writeChar in die Datei (filename) geschrieben 
  
  writeChar("\\documentclass[a4paper, 11pt]{article}
    
                        \\usepackage[includeheadfoot, top=0.5in, bottom=0.5in, left=0.5in, right=0.5in]{geometry}
                        \\usepackage[utf8]{inputenc}
                        \\setlength{\\headheight}{30pt}
                        \\usepackage{fancyhdr}
                        \\usepackage{booktabs}
                        \\usepackage{graphicx}
                        \\usepackage[ngerman]{babel}
                        \\pagestyle{fancy}
                        \\usepackage[table]{xcolor}
                        \\pagenumbering{gobble}
                        \\usepackage{helvet}
                        \\renewcommand{\\familydefault}{\\sfdefault}
                        \\pagenumbering{arabic}
                        \\usepackage{grffile}
                        \\usepackage{enumitem}
                        \\usepackage[compact]{titlesec}
	
	                      
                        \\chead{Schulr\\\"uckmeldung\\\\}
                        \\lhead{Schule  \\Sexpr{schule} \\newline}
                        \\rhead{\\includegraphics[width=70px,height=25px]{\"\\Sexpr{paste0(logo_dir,\"/LALE7\")}\".jpg}}
                        
                        \\fancypagestyle{Leer}
                        {
                        \\fancyfoot[C]{\\thepage}
                        \\fancyfoot[L]{Schuljahr \\Sexpr{schuljahr}}
                        }
                        
                        \\fancypagestyle{Lesen}
                        {
                        \\fancyfoot[C]{\\thepage}
                        \\fancyfoot[L]{Schuljahr \\Sexpr{schuljahr}}
                        \\fancyfoot[R]{Deutsch-Leseverstehen}
                        }
                        
                        \\fancypagestyle{Mathe}
                        {
                        \\fancyfoot[C]{\\thepage}
                        \\fancyfoot[L]{Schuljahr \\Sexpr{schuljahr}}
                        \\fancyfoot[R]{Mathematik}
                        }
                        
                        \\begin{document}
                        
                        \\begin{titlepage}
	                      \\centering
              {\\Large Lernausganglagenerhebung 7}
	                      \\hrule
	                      \\vspace*{4cm}
	                      \\includegraphics[width=0.8\\textwidth]{\"\\Sexpr{paste0(logo_dir,\"/LALE7\")}\".jpg}\\par
	                      \\vspace*{3cm}
                        {\\LARGE Schulr\"uckmeldung \\Sexpr{schuljahr} \\par}
                        \\vspace{1.5cm}
                        {\\LARGE Schule \\Sexpr{schule} \\par}
	                      \\vfill
                        \\end{titlepage}
                        
                        \\pagestyle{Leer}
                        \\begin{flushleft}
                        \\tableofcontents
                        \\label{}
                        
                        \\vspace{\\fill}
                        
                        Oktober 2024
                        \\vspace{1cm}
                        
                        \\begin{minipage}{0.25\\textwidth}
                        \\includegraphics[width=\\textwidth]{\\Sexpr{paste0(logo_dir,\"/IQHB1.png\")}}
                        \\end{minipage}%
                        %
                        \\begin{minipage}{0.25\\textwidth}
                        \\includegraphics[width=\\textwidth]{\\Sexpr{paste0(logo_dir,\"/ifbq.jpg\")}}
                        \\end{minipage}%
                        %
                        \\begin{minipage}{0.25\\textwidth}
                        \\hspace{0.5cm} \\includegraphics[width=1.1\\textwidth]{\\Sexpr{paste0(logo_dir,\"/SKB_TS.png\")}}
                        \\end{minipage}
                        \\newpage
                        
                        
                        
                        Liebe Kolleg:innen,
                        \\newline
                        \\newline
                        wir freuen uns, Ihnen die Ergebnisse der Lernausganglagenerhebung in der 7. Jahrgangsstufe (LALE 7) mitteilen zu k\"onnen. LALE wird \"uber eine Kooperation des IQHB mit dem IfBQ Hamburg und dem PL Rheinland-Pfalz erm\"oglicht. Die Urheberrechte an den Testaufgaben liegen beim IfBQ Hamburg.
                        \\newline
                        \\newline
                        Das Format der R\"uckmeldungen orientiert sich am Hamburger KERMIT-R\"uckmeldeformat, wurde jedoch an einigen Stellen an spezifische W\"unsche aus den Bremer Schulen angepasst. Das Format wurde mit der LALE AG abgestimmmt.
                        \\newline
                        \\newline
                        Die R\"uckmeldung ist \\textbf{inklusiv}. Die Ergebnisse von Sch\"uler:innen mit sonderp\"adagogischem F\"orderbedarf gehen in die Wertung ein. Das Gleiche trifft zu f\"ur die Ergebnisse von Sch\"uler:innen, die weniger als 12 Monate in Deutschland leben und am Test teilgenommen haben. Dies ist unabh\"angig davon, ob sie ein spezielles Testheft (E-Heft) bearbeitet haben oder nicht. Eine Ausnahme gilt f\"ur Sch\"uler:innen mit F\"orderbedarf im Bereich Wahrnehmungs- und Entwicklungsf\"orderung. Eine Teilnahme fand nur in Einzelf\"allen an wenigen Schulen statt. F\"ur die bessere Vergleichbarkeit wird zwar eine individuelle Sch\"ulerr\"uckmeldung erstellt, aber die Ergebnisse gehen nicht in die Klassen- und Schulwertung ein.
                        \\newline
                        \\newline
                        Die \\textbf{Pseudonyme} der einzelnen \\textbf{Sch\"uler:innen} k\"onnen Sie im Sch\"ulerverzeichnis \"uber das Men\"u â€žDrucken â€“ Auswerten -$>$ Klassen â€“ Berichte -$>$ Klassenlisten -$>$ Klassenliste LALE 7â€œ erneut aufrufen. Sollte sich seit der Testung die Klassenzuordnung ge\"andert haben, ist diese \"Anderung automatisch in der neuen Liste bzw. im R\"uckmeldeformat ber\"ucksichtigt (Stichtag 27.09.2024).
                        \\newline
                        \\newline
                        \\textbf{Kontakt}
                        \\newline
                        \\newline
                        Wenn Sie Fragen oder Anmerkungen zu den R\"uckmeldungen haben, freuen wir uns, wenn Sie Kontakt zu uns aufnehmen.
                        \\newline
                        \\newline
                        \\begin{tabular}{p{7.5cm}p{7.5cm}}
                        Dr. Claudia Zierul & Andrea Timcke\\\\
                        IQHB & IQHB\\\\
                        Tel.: 0421 361-32049 & Tel.: 0421 361-30704 \\\\
                        E-Mail: claudia.zierul@iqhb.bremen.de & E-Mail: andrea.timcke@iqhb.bremen.de\\\\
                        \\end{tabular}
                        \\newline
                        \\newline
                        \\newline
                        \\newline
                        \\begin{minipage}{0.45\\textwidth}
                        \\textbf{Vernetzung und Material auf itslearning}\\\\
                        \\\\
                        Es gibt auf itslearning einen Kurs zur Vernetzung und zum Materialaustausch. Sie finden diesen Kurs unter Kurse, auf â€žWeitere Kurse suchen$>$Kurskatalogâ€œ klicken und dort â€žLALE 5 + 7 Netzwerkâ€œ eingeben.\\\\
                        \\end{minipage}
                        \\hfill
                        \\begin{minipage}{0.45\\textwidth}\\begin{flushright}\\includegraphics[width=\\textwidth]{\\Sexpr{paste0(logo_dir,\"/itslearning.jpg\")}}\\end{flushright}\\end{minipage}
                        \\newpage
                        Im itslearning-Kurs zu LALE 5 + 7:
                        \\begin{itemize}
                        \\item wird auf aktuelle Fortbildungen und Unterst\"utzungsformate hingewiesen.
                        \\item finden Sie Materialien aus LALE-Veranstaltungen, zum Hintergrund des Verfahrens und zur Weiterarbeit.
                        \\item haben Sie die M\"oglichkeit, sich im Forum auszutauschen.
                        \\item finden Sie Kontaktdaten der Ansprechpersonen der anderen LALE-Schulen.
                        \\end{itemize}
                        \\vspace{1cm}
                        {\\Large\\textbf{Hinweise zu den R\"uckmeldeformaten von LALE 7}}
                        \\newline
                        \\newline
                        Ein paar allgemeine Erkl\"arungen zu den R\"uckmeldeformaten von LALE 7 m\"ochten wir Ihnen vorab geben:
                        \\newline
                        \\newline
                        \\textbf{498 Punkte? - Punkte auf der BISTA-Skala}
                        \\newline
                        \\newline
                        F\"ur eine erste Einsch\"atzung werden Ihnen die Punkte auf der BISTA-Skala (Messskala der Bildungsstandards) zur\"uckgemeldet. Interessant ist f\"ur Sie an dieser Stelle also nicht der Wert an sich, sondern die Einordnung zu den Vergleichswerten. 
                        \\newline
                        \\newline
                        Die LALE 7-Ergebnisse werden Ihnen auf derselben Skala zur\"uckgemeldet wie die LALE 5-Ergebnisse vor zwei Jahren. Durch die Abbildung beider Werte in einer Grafik kann man ablesen, wie sich die diesj\"ahrigen Siebtkl\"assler entwickelt haben: 
                        <<Grafik_A, fig.width=7, fig.height=3, echo=FALSE, background=NA, results=\"asis\">>=
                        Grafik_A(data_Start=445, data_Ende=524)
                        @
                        
                      \\newpage
                      \\textbf{Leistungsstufen (LALE 7) statt Kompetenzstufen (LALE 5)}
                      \\newline
                      \\newline
                        Da es f\"ur den siebten Jahrgang kein anwendbares Kompetenzstufenmodell gibt (bei LALE 5 wird der Bezug zu den Bildungsstandards f\"ur Ende 4. Jahrgangstufe hergestellt), k\"onnen die Ergebnisse bei LALE 7 nicht in einer Kompetenzstufenverteilung dargestellt werden. Stattdessen wurden sechs Leistungsstufen gebildet:
                        <<Grafik_B, fig.width=7, fig.height=1.5, echo=FALSE, background=NA, results=\"asis\">>=
                        Grafik_B(regelstandard=regelstandard$regelstandard_Mathe, farben=regelstandard$Farben)
                        @
                        
                        Im Gegensatz zu den Kompetenzstufen, k\"onnen die Leistungsstufen nicht inhaltlich interpretiert werden. Aber wie die Kompetenzstufen lassen sie einen Vergleich der Leistungen zu, z.B. â€žWie viel Prozent der Sch\"uler:innen liegen an unserer Schule in der untersten Leistungsstufe? Wie viel Prozent sind es in der Vergleichsgruppe?â€œ
                        \\newline
                        \\newline
                        \\textbf{Warum werden doch Sch\"uler:innen unter Regelstandard mit Bezug auf das Kompetenzstufenmodell Ende der 4. Jahrgangsstufe zur\"uckgemeldet?}
                        \\newline
                        \\newline
                        Auch wenn das Kompetenzstufenmodell der Bildungsstandards f\"ur das Ende der 4. Jahrgangsstufe in der 7. Jahrgangsstufe inhaltlich nicht mehr anwendbar ist, so kann man dennoch auf Basis der Punktwerte Sch\"uler:innen identifizieren, die den Wert f\"ur ein Erreichen der Regelstandards auch in der 7. Jahrgangsstufe nicht erzielt haben.
                        \\newline
                        \\newline
                        \\textbf{80\\% - L\"osungsh\"aufigkeiten der Aufgaben}
                        \\newline
                        \\newline
                        Es ist kaum m\"oglich im LALE-7-Test 100 Prozent der Aufgaben richtig zu l\"osen. Damit LALE 7 \"uber alle Sch\"uler:innen m\"oglichst genau misst, liegt die mittlere L\"osungsh\"aufigkeit \"uber alle Testhefte bei etwa 50 Prozent, Sch\"uler:innen der 7. Klasse k\"onnen also im Mittel 50 Prozent der Aufgaben richtig l\"osen. Das heisst, mit 80 Prozent liegt man bereits deutlich \"uber dem Durchschnitt.
                        \\newline
                        \\newline
                        \\textbf{Wer ist meine Vergleichsgruppe?}
                        \\newline
                        \\newline
                        Ihre Vergleichsgruppe setzt sich aus Ihrer Schule und anderen \\Sexpr{names(schulart)} mit
                        \"ahnlichem sozialen Kontext zusammen. Sie soll Ihnen bei der Einordnung Ihrer Ergebnisse
                        helfen und einen â€žfairenâ€œ  Vergleich erm\"oglichen. In diesem Jahr besteht Ihre
                        Vergleichsgruppe aus den folgenden Schulen:
                        \\newline
                        \\newline
                        \\begin{itemize}
                        <<results=\"asis\", echo=FALSE>>=
                        cat(vergleichsschulen, sep=\"\\n\")
                        @
                        \\end{itemize}
                       
                        Zus\"atzlich werden Ihnen der Wert aller \\Sexpr{nrow(unique(lale_empirisch[which(lale_empirisch$schulart==schulart),\"SCHULNUMMER\"]))} \\Sexpr{names(schulart)} und der Wert aller \\Sexpr{nrow(unique(lale_empirisch[,\"SCHULNUMMER\"]))} an LALE teilnehmenden Schulen angezeigt.

                        \\newpage
                        \\pagestyle{Lesen}
                        \\section{Deutsch-Leseverstehen: Mittlere Leistungswerte}
                        
                        Die Sch\"uler:innen Ihrer Schule erzielten in Deutsch-Leseverstehen durchschnittlich \\Sexpr{round(mean(unlist(lale_empirisch[which(lale_empirisch$SCHULNUMMER==schule),\"delwlemeanbista\"]),na.rm=TRUE))} Punkte (siehe Abbildung 1). In Ihrer Schule haben \\Sexpr{nrow(lale_emp_DL[which(lale_emp_DL$SCHULNUMMER==schule),])} Sch\"uler:innen (N=\\Sexpr{nrow(lale_emp_DL[which(lale_emp_DL$SCHULNUMMER==schule),\"delwlemeanbista\"])}) teilgenommen.
                        \\newline
                        \\newline
                        Schwarze Pfeile zeigen an, ob der mittlere Leistungswert Ihrer Schule bedeutsam \"uber ($\\uparrow$) oder bedeutsam unter ($\\downarrow$) dem Mittelwert der Vergleichsgruppe liegt. 
                        Das Ergebnis Ihrer Schule unterscheidet sich \\Sexpr{bedeutsam} vom Mittelwert der Vergleichsgruppe (siehe Seite 4).
                        \\newline
                        \\newline
                        Mittelwertunterschiede ab 30 Punkten gelten als bedeutsam. 
                        \\newline
                        \\newline
                        \\Sexpr{Oberschulenvergleich}
                        \\newline
                        \\newline
                        \\textbf{Abbildung 1: Deutsch-Leseverstehen - Mittlere Leistungswerte (BISTA-Punkte)}
                        <<Plot1, fig.width=9, fig.height=7, echo=FALSE, background=NA, results=\"asis\", eval=any(is.na(ext[which(ext$Grafik==\"Plot1\"),\"ADD\"]),ext[which(ext$Grafik==\"Plot1\"),\"ADD\"])>>=
                        schule_plot1_2_6_7(lale_emp_DL,klasse=FALSE,schule=schule,referenzgruppe=referenzgruppe,Datenspalte=\"delwlemeanbista\")
                        box(which = \"outer\")
                        @
                        
                        \\Sexpr{inc(\"Plot1\")}
                        
                        \\newpage
                        
                        \\section{Deutsch-Leseverstehen: Mittlere Leistungswerte der Parallelklassen}
                         
                        Die mittleren Leistungswerte der Parallelklassen werden in Abbildung 2 gezeigt. 
                        
                        Schwarze Pfeile zeigen an, ob der mittlere Leistungswert einer Klasse bedeutsam \"uber ($\\uparrow$) oder bedeutsam unter ($\\downarrow$) dem Mittelwert der Vergleichsgruppe (siehe Seite 4) liegt. 
                        
                        Vom Mittelwert der Vergleichsgruppe unterscheiden sich die Ergebnisse  
                        \\vspace{0.2cm}
                        \\Sexpr{kable(booktabs=T, escape=F, bottomrule=\"\", toprule=\"\", midrule=\"\", do.call(rbind,lapply(seq(1:length(sort(unique(lale_empirisch[lale_empirisch$SCHULNUMMER==schule,]$KLASSE)))),function(x) paste(\"$*$ der Klasse\",sort(unique(lale_empirisch[lale_empirisch$SCHULNUMMER==schule,]$KLASSE))[x],klassenwert(sort(unique(lale_empirisch[lale_empirisch$SCHULNUMMER==schule,]$Klassen_kenn))[x], spalte=\"delwlemeanbista\"),\"\\n\"))),\"latex\") %>% collapse_rows(columns=1,latex_hline=\"none\")}                       
                        \\vspace{0.2cm}
                        Mittelwertunterschiede ab 30 Punkten gelten als bedeutsam. 
                        \\newline
                        \\newline
                        \\textbf{Abbildung 2: Deutsch-Leseverstehen - Mittlere Leistungswerte der Parallelklassen (BISTA-Punkte)}
                        <<Plot2, fig.width=9, fig.height=7, echo=FALSE, background=NA, results=\"asis\", eval=any(is.na(ext[which(ext$Grafik==\"Plot2\"),\"ADD\"]),ext[which(ext$Grafik==\"Plot2\"),\"ADD\"])>>=
                         schule_plot1_2_6_7(lale_emp_DL,klasse=TRUE,schule=schule,referenzgruppe=referenzgruppe,Datenspalte=\"delwlemeanbista\")
                        box(which = \"outer\")
                        @
                        
                        \\Sexpr{inc(\"Plot2\")}
                        
                        \\newpage
                        
                        \\section{Deutsch-Leseverstehen: Leistungsverteilungen}
                        
                        Die Leistungsverteilung Ihrer Sch\"uler:innen wird in Abbildung 3 dargestellt. Zum Vergleich sind die Ergebnisse der Vergleichsgruppe (siehe Seite 4) aufgenommen. (Erl\"auterungen zu den Leistungsstufen siehe Seite 3 und 4)
                        \\newline
                        \\newline
                        \\textbf{Abbildung 3: Deutsch-Leseverstehen - Leistungsverteilungen}
                        <<Plot3, fig.width=9, fig.height=7, echo=FALSE, background=NA, results=\"asis\", eval=any(is.na(ext[which(ext$Grafik==\"Plot3\"),\"ADD\"]),ext[which(ext$Grafik==\"Plot3\"),\"ADD\"])>>=
                         schule_plot3_4_8_9(lale_emp_DL,klasse=FALSE,schule=schule,referenzgruppe=referenzgruppe,Datenspalte=\"delwlemeanbista\",regelstandard=regelstandard$regelstandard_Lesen, farben=regelstandard$Farben[-1])
                        box(which = \"outer\")
                        @ 
                         
                        Abbildung 3 zeigt zum Beispiel, dass \\Sexpr{round(prop.table(table(cut(unlist(lale_emp_DL[which(lale_emp_DL$SCHULNUMMER==schule),\"delwlemeanbista\"]),regelstandard$regelstandard_Lesen)))*100)[[5]]} Prozent der Sch\"uler:innen Ihrer Schule die Leistungsstufe 500 bis 600 Punkte erreicht hat. In Ihrer Vergleichsgruppe sind im Vergleich dazu \\Sexpr{round(prop.table(table(cut(unlist(lale_emp_DL[grep(referenzgruppe,lale_emp_DL$Vergleichsgruppe),\"delwlemeanbista\"]),regelstandard$regelstandard_Lesen)))*100)[[5]]} Prozent in dieser Leistungsstufe.

                        \\Sexpr{inc(\"Plot3\")}
                        
                        \\newpage
                        
                        \\section{Deutsch-Leseverstehen: Leistungsverteilungen der Parallelklassen}

                        Die Leistungsverteilung Ihrer Sch\"uler:innen der Parallelklassen wird in Abbildung 4.1 dargestellt.
                        \\newline
                        \\newline
                        \\textbf{Abbildung 4.1: Deutsch-Leseverstehen - Leistungsverteilungen der Parallelklassen}
                        <<Plot4_1, fig.width=9, fig.height=7, echo=FALSE, background=NA, results=\"asis\", eval=any(is.na(ext[which(ext$Grafik==\"Plot4\"),\"ADD\"]),ext[which(ext$Grafik==\"Plot4\"),\"ADD\"])>>=
                         schule_plot3_4_8_9(lale_emp_DL,klasse=TRUE,schule=schule,referenzgruppe=referenzgruppe,Datenspalte=\"delwlemeanbista\",regelstandard=regelstandard$regelstandard_Lesen, farben=regelstandard$Farben[-1])
                        box(which = \"outer\")
                        @
                        
                        \\Sexpr{inc(\"Plot4.1\")}
                        
                        \\newpage
                        
                        {\\Large \\textbf{Deutsch-Leseverstehen: Leistungsverteilung der Parallelklassen mit nicht vorhandenen Werten (n.v.)}}

                        Einzelne Sch\"uler:innen haben so wenig Aufgaben bearbeitet (weniger als 10), dass keine Sch\"atzung der Kompetenz m\"oglich ist, der Wert ist also nicht vorhanden (n.v.). In der Leistungsverteilung Ihrer Sch\"uler:innen der Parallelklassen in Abbildung 4.2 sind diese Sch\"uler:innen ohne Wert aufgenommen.
                        \\newline
                        \\newline
                        \\textbf{Abbildung 4.2: Deutsch-Leseverstehen - Leistungsverteilungen der Parallelklassen inkl. Sch\"uler:innen, die keinen Wert erhalten haben}
                        <<Plot4_2, fig.width=9, fig.height=7, echo=FALSE, background=NA, results=\"asis\", eval=any(is.na(ext[which(ext$Grafik==\"Plot4.2\"),\"ADD\"]),ext[which(ext$Grafik==\"Plot4.2\"),\"ADD\"])>>=
                         schule_plot41_91(lale_empirisch[which(lale_empirisch[,\"TN\"]==1),],referenzgruppe=referenzgruppe,schule=schule,Datenspalte=\"delwlemeanbista\",regelstandard=regelstandard$regelstandard_Lesen, farben=regelstandard$Farben)
                        box(which = \"outer\")
                        @
                        
                        \\Sexpr{inc(\"Plot4.2\")}
                        
                        \\newpage
                        
                        \\section{Deutsch-Leseverstehen: Anteil der Sch\"uler:innen unter Regelstandard auf Basis des Kompetenzstufenmodells Ende der 4. Jahrgangsstufe}
                        
                        Abbildung 5 zeigt Ihnen, welcher Anteil der Sch\"uler:innen in den Parallelklassen Ihrer Schule den Regelstandard auf Basis des Kompetenzstufenmodells Ende der 4. Jahrgangsstufe im LALE 7-Test nicht erreicht hat (orange). Zum Vergleich werden Ihnen auch die Werte Ihrer Schule und der Vergleichsgruppe angegeben.\\newline\\newline
                        Das Kompetenzstufenmodell f\"ur das Ende der 4. Jahrgangsstufe kann inhaltlich in der 7. Jahrgangsstufe nicht mehr angewendet werden. Auf Basis der in LALE 7 erreichten Punktwerte kann aber festgestellt werden, welche Sch\"uler:innen den notwendigen Wert f\"ur das Erreichen des Regelstandards Ende Klasse 4 zu Beginn der 7. Jahrgangsstufe verfehlt haben.
                        \\newline
                        \\newline
                        \\textbf{Abbildung 5: Deutsch-Leseverstehen - Anteil der Sch\"uler:innen unter Regelstandard auf Basis des Kompetenzstufenmodells Ende der 4. Jahrgangsstufe}
                        <<Plot4_3, fig.width=9, fig.height=7, echo=FALSE, background=NA, results=\"asis\", eval=any(is.na(ext[which(ext$Grafik==\"Plot5\"),\"ADD\"]),ext[which(ext$Grafik==\"Plot5\"),\"ADD\"])>>=
                        schule_plot_4_3(lale_emp_DL,schule=schule,referenzgruppe=referenzgruppe,Datenspalte=\"delwlemeanbista\",regelstandard=c(0,465,999))
                        box(which = \"outer\")
                        @
                        
                        \\Sexpr{inc(\"Plot5\")}
                        
                        \\newpage
                        
                        \\section{Deutsch-Leseverstehen: Mittlere Leistungsentwicklungen}
                        Abbildung 6 zeigt die mittlere Leistungsentwicklung Ihrer Schule seit Beginn der f\"unften Jahrgangsstufe. Bei der Ermittlung der durchschnittlichen Leistungsentwicklungen werden jeweils nur die Sch\"uler:innen ber\"ucksichtigt, die an beiden Testungen teilgenommen haben.
                        \\newline
                        \\newline
                        \\textbf{Abbildung 6: Deutsch-Leseverstehen - Mittlere Leistungsentwicklungen}
                        <<Plot7_1, fig.width=9, fig.height=7, echo=FALSE, background=NA, results=\"asis\", eval=any(is.na(ext[which(ext$Grafik==\"Plot6\"),\"ADD\"]),ext[which(ext$Grafik==\"Plot6\"),\"ADD\"])>>=
                         klasse_plot_LALE7_2(lale_emp_DL,klasse=FALSE,schule=schule,referenzgruppe=referenzgruppe,Datenspalte_Start=\"delwlemeanbista_LALE5\",Datenspalte_Ende=\"delwlemeanbista\")
                        box(which = \"outer\")
                        @
                        
                        \\Sexpr{inc(\"Plot6\")}
                        
                        \\newpage
                        
                        \\section{Deutsch-Leseverstehen: Mittlere Leistungsentwicklungen der \\newline Parallelklassen}
                        Abbildung 7 zeigt die mittleren Leistungsentwicklungen der Parallelklassen seit Beginn der f\"unften Jahrgangsstufe. Bei der Ermittlung der durchschnittlichen Leistungsentwicklungen werden jeweils nur die Sch\"uler:innen ber\"ucksichtigt, die an beiden Testungen teilgenommen haben.
                        \\newline
                        \\newline
                        \\textbf{Abbildung 7: Deutsch-Leseverstehen - Mittlere Leistungsentwicklungen der Parallelklassen}
                        <<Plot7_2, fig.width=9, fig.height=7, echo=FALSE, background=NA, results=\"asis\", eval=any(is.na(ext[which(ext$Grafik==\"Plot7\"),\"ADD\"]),ext[which(ext$Grafik==\"Plot7\"),\"ADD\"])>>=
                         klasse_plot_LALE7_2(lale_emp_DL,klasse=TRUE,schule=schule,referenzgruppe=referenzgruppe,Datenspalte_Start=\"delwlemeanbista_LALE5\",Datenspalte_Ende=\"delwlemeanbista\")
                        box(which = \"outer\")
                        @
                        
                        \\Sexpr{inc(\"Plot7\")}
                        
                        \\newpage
                        
                        \\section{Deutsch-Leseverstehen: Hinweise zu den Aufgaben f\"ur die Fachkonferenz und L\"osungsh\"aufigkeiten}
                        \\markboth{Deutsch-Leseverstehen: Hinweise zu den Aufgaben f\"ur die Fachkonferenz und L\"osungsh\"aufigkeiten}{Deutsch-Leseverstehen: Hinweise zu den Aufgaben f\"ur die Fachkonferenz und L\"osungsh\"aufigkeiten}
                        
                        Die Testaufgaben werden nicht ver\"offentlicht. Um Ihnen dennoch eine inhaltliche interpretierbare Ergebnisdarstellung zu liefern, haben wir zus\"atzliche Hinweise zu den Aufgaben zusammengestellt. 
                        \\newline
                        \\newline
                        In der folgenden Tabelle werden die Lesetexte kurz charakterisiert. Die mittlere L\"osungsh\"aufigkeit der Parallelklassen wird dann f\"ur die beiden Texte im Vergleich zur Schule und zur Vergleichsgruppe dargestellt. 
                        \\newline
                        \\newline
                        \\renewcommand{\\arraystretch}{1.8}
                        \\begin{small}
                        \\begin{tabular}{|p{2cm}|p{7.57cm}|p{7.57cm}|}
                        \\hline
                         & \\cellcolor{lightgray} Sachtext: Bionik & \\cellcolor{gray}Sachtext: Die Meisterdiebe\\\\ \\hline
                        Texttyp & Naturwissenschaftlicher Bericht aus einer Jugendzeitung & Mittellanger Text\\\\ \\hline
                        Merkmale &  \\begin{itemize}[leftmargin=*]
                                    \\item mittlere Informationsdichte
                                    \\item klare Struktur mit teils komplexem Satzbau
                                    \\item wissenschaftlicher â€“ aber verst\"andlich beschriebener â€“ einfacher Wortschatz
                                   \\end{itemize}
                                &
                                   \\begin{itemize}[leftmargin=*]
                                    \\item hohe Informationsdichte 
                                    \\item Ereignisse werden chronologisch erz\"ahlt
                                    \\item Identifikationsfiguren vorhanden
                                   \\end{itemize}
  
                                  \\\\ \\hline
                        Bildungs- \\newline standards & 
                                   \\begin{itemize}[leftmargin=*]
                                    \\item Wortbebedeutungen kl\"aren (3.2.3)
                                    \\item zentrale Aussagen erschliessen (3.3.4)
                                    \\item Informationen zielgerichtet entnehmen, ordnen, vergleichen, pr\"ufen und erg\"anzen (3.4.3)
                                    \\item aus Sach- und Gebrauchstexten begr\"undete Schlussfolgerungen ziehen (3.4.6)
                                    \\item Intention eines Textes erkennen, insbesondere Zusammenhang zwischen Autorenintention, Textmerkmalen, Leseerwartungen und Wirkungen (3.4.5)
                                   \\end{itemize}
                                &
                                   \\begin{itemize}[leftmargin=*]
                                    \\item gezielt einzelne Informationen suchen (3.3.2)
                                    \\item Texte genau lesen (3.3.3)
                                    \\item zentrale Aussagen eines Textes erfassen und wiedergeben (3.3.6)
                                   \\end{itemize}  
                                  \\\\ \\hline
                        \\end{tabular}
                        \\end{small}
                        \\renewcommand{\\arraystretch}{1}
                        \\newline
                        \\newline
                        \\textbf{Abbildung 8: Deutsch-Leseverstehen - L\"osungsh\"aufigkeiten der Aufgaben der beiden Texte in den Parallelklassen}
                        <<Plot5, fig.width=9, fig.height=3, echo=FALSE, background=NA, results=\"asis\", eval=any(is.na(ext[which(ext$Grafik==\"Plot8\"),\"ADD\"]),ext[which(ext$Grafik==\"Plot8\"),\"ADD\"])>>=
                         schule_plot5(lale_emp_DL,klasse=klasse,schule=schule,referenzgruppe=referenzgruppe,Datenspalte=c(\"Bionik_Richtig_Deutsch\",\"Bionik_Falsch_Deutsch\",\"Bionik_99_Deutsch\",\"Die Meisterdiebe_Richtig_Deutsch\",\"Die Meisterdiebe_Falsch_Deutsch\",\"Die Meisterdiebe_99_Deutsch\"))
                        box(which = \"outer\")
                        @
                        
                        \\Sexpr{inc(\"Plot8\")}
                        
                        \\newpage
                        
                        \\section{Deutsch-Leseverstehen: L\"osungsh\"aufigkeiten zum Stufenmodell zur Aufgabenbeurteilung von Aufgaben (Zabka, 2008) f\"ur die Fachkonferenz}

                        Die Aufgaben aus dem Testbereich Deutsch-Leseverstehen wurden zus\"atzlich nach den unterschiedlichen Teilkompetenzen zum Leseverstehen kategorisiert, die zur Beantwortung der Aufgaben gefordert werden. Dadurch erhalten Sie noch einmal Hinweise darauf, welche Teilkompetenzen bei Ihren Sch\"uler:innen besonders gut ausgepr\"agt sind und welche nicht. Eine ausf\"uhrliche Dokumentation des Stufenmodells finden Sie auf itslearning im Kurs \"LALE 5 + 7 Netzwerk\".
                        \\vspace{0.2cm}
                        \\begin{tabular}{|p{9.5cm}|p{8.2cm}|}
                        \\hline
                        Teilkompetenz & L\"osungsh\"aufigkeiten der Aufgaben\\\\ \\hline
                        \\noindent\\parbox[t][-1.5cm][b]{\\hsize}{\\footnotesize \\textbf{1. Manifeste Information und Informationsverkn\"upfungen verstehen} \\newline Einzelne manifeste Informationen m\"ussen in einem begrenzten Bereich (Abschnitt) des Lesetextes identifiziert werden.}
                        & 
                      <<Plot5_1, fig.width=6, fig.height=2.5, echo=FALSE, background=NA, results=\"asis\", eval=any(is.na(ext[which(ext$Grafik==\"Plot8.1\"),\"ADD\"]),ext[which(ext$Grafik==\"Plot8.1\"),\"ADD\"])>>=
                        schule_plot5.1234(lale_emp_DL,klasse,schule,referenzgruppe=referenzgruppe,Datenspalte=c(\"Z1_Zabka_Richtig\",\"Z1_Zabka_Falsch\",\"Z1_Zabka_99\"))
                        @
                        \\Sexpr{inc.tab(\"Plot8.1\")}
                        \\\\ \\hline
                        \\noindent\\parbox[t][-0.5cm][b]{\\hsize}{\\footnotesize \\textbf{2. Informationen und Informationsverkn\"upfungen lokal erschliessen} \\newline Die zu suchenden Informationen sind nicht zwingend manifest, sondern m\"ussen erschlossen werden. Dabei m\"ussen zuweilen auch (zwei) Informationen \"uber den gesamten Text hinweg zusammengef\"ugt werden.}
                        & 
                      <<Plot5_2, fig.width=6, fig.height=2.5, echo=FALSE, background=NA, results=\"asis\", eval=any(is.na(ext[which(ext$Grafik==\"Plot8.2\"),\"ADD\"]),ext[which(ext$Grafik==\"Plot8.2\"),\"ADD\"])>>=
                        schule_plot5.1234(lale_emp_DL,klasse,schule,referenzgruppe=referenzgruppe,Datenspalte=c(\"Z2_Zabka_Richtig\",\"Z2_Zabka_Falsch\",\"Z2_Zabka_99\"))
                        @
                        \\Sexpr{inc.tab(\"Plot8.2\")}
                        \\\\ \\hline
                        \\noindent\\parbox[t][-1cm][b]{\\hsize}{\\footnotesize \\textbf{3. Globale Zusammenh\"ange verstehen} \\newline Ein koh\"arentes Mindestverst\"andnis des Textes soll vorhanden sein, indem Informations-, Argumentations- oder Handlungszusammenh\"ange verstanden werden. Diese Zusammenh\"ange k\"onnen \"uber den Text verstreut sein und Schlussfolgerungen erfordern.}
                        & 
                      <<Plot5_3, fig.width=6, fig.height=2.5, echo=FALSE, background=NA, results=\"asis\", eval=any(is.na(ext[which(ext$Grafik==\"Plot8.3\"),\"ADD\"]),ext[which(ext$Grafik==\"Plot8.3\"),\"ADD\"])>>=
                        schule_plot5.1234(lale_emp_DL,klasse,schule,referenzgruppe=referenzgruppe,Datenspalte=c(\"Z3_Zabka_Richtig\",\"Z3_Zabka_Falsch\",\"Z3_Zabka_99\")) 
                        @
                        \\Sexpr{inc.tab(\"Plot8.3\")}
                        \\\\ \\hline
                        \\noindent\\parbox[t][-0.1cm][b]{\\hsize}{\\footnotesize \\textbf{4. Informationen in Begriffen und Vorstellungen zusammenfassen} \\newline \"Ubergeordnete Begriffe und Vorstellungen zur Zusammenfassung und Interpretation des Textes sollen vorhanden sein. Diese Vorstellungen k\"onnen sich beispielsweise auf den Ort und die Umst\"ande der dargestellten Wirklichkeit, die Gef\"uhle und Einstellungen der \\newline Figuren oder auf die Hauptaussage des gesamten Textes beziehen.}
                        & 
                      <<Plot5_4, fig.width=6, fig.height=2.5, echo=FALSE, background=NA, results=\"asis\", eval=any(is.na(ext[which(ext$Grafik==\"Plot8.4\"),\"ADD\"]),ext[which(ext$Grafik==\"Plot8.4\"),\"ADD\"])>>=
                        schule_plot5.1234(lale_emp_DL,klasse,schule,referenzgruppe=referenzgruppe,Datenspalte=c(\"Z4_Zabka_Richtig\",\"Z4_Zabka_Falsch\",\"Z4_Zabka_99\"))
                        @
                        \\Sexpr{inc.tab(\"Plot8.4\")}
                        \\\\ \\hline
                        \\noindent\\parbox[t][-1cm][b]{\\hsize}{\\footnotesize \\textbf{5. Sprach- und Textgestaltung interpretieren} \\newline Sprachbewusstes Textverstehen: wesentliche rhetorische und stilistische Texteigenschaften sollen wahrgenommen werden und text\"ubergreifende Strukturen (z.B. Funktion von Textsorten) zur Interpretation des Textes genutzt werden.}
                        & 
                      <<Plot5_5, fig.width=6, fig.height=2.5, echo=FALSE, background=NA, results=\"asis\", eval=any(is.na(ext[which(ext$Grafik==\"Plot8.5\"),\"ADD\"]),ext[which(ext$Grafik==\"Plot8.5\"),\"ADD\"])>>=
                        schule_plot5.1234(lale_emp_DL,klasse,schule,referenzgruppe=referenzgruppe,Datenspalte=c(\"Z5_Zabka_Richtig\",\"Z5_Zabka_Falsch\",\"Z5_Zabka_99\"))
                        @
                        \\Sexpr{inc.tab(\"Plot8.5\")}
                        \\\\ \\hline
                        \\end{tabular}
                        
                        

                        \\newpage
                        \\pagestyle{Mathe}
                        \\section{Mathematik: Mittlere Leistungswerte}

                        Die Sch\"uler:innen Ihrer Schule erzielten in Mathematik durchschnittlich \\Sexpr{round(mean(unlist(lale_empirisch[which(lale_empirisch$SCHULNUMMER==schule),\"mathewle_Bista\"]), na.rm=TRUE))} Punkte (siehe Abbildung 6). In Ihrer Schule haben \\Sexpr{nrow(lale_emp_MA[which(lale_emp_MA$SCHULNUMMER==schule),])} Sch\"uler:innen (N=\\Sexpr{nrow(lale_emp_MA[which(lale_emp_MA$SCHULNUMMER==schule),])}) teilgenommen.
                        \\newline
                        \\newline
                        Schwarze Pfeile zeigen an, ob der mittlere Leistungswert Ihrer Schule bedeutsam \"uber ($\\uparrow$) oder bedeutsam unter ($\\downarrow$) dem Mittelwert der Vergleichsgruppe liegt. 
                        \\newline
                        \\newline
                        Das Ergebnis Ihrer Schule unterscheidet sich \\Sexpr{bedeutsam_mathe} vom Mittelwert der Vergleichsgruppe (siehe Seite 4).
                       \\newline
                        \\newline
                        Mittelwertunterschiede ab 30 Punkten gelten als bedeutsam.
                        \\newline
                        \\newline
                        \\Sexpr{Oberschulenvergleich}
                        \\newline
                        \\newline
                        \\textbf{Abbildung 9: Mathematik - Mittlere Leistungswerte (BISTA-Punkte)}
                        <<Plot6, fig.width=9, fig.height=7, echo=FALSE, background=NA, results=\"asis\", eval=any(is.na(ext[which(ext$Grafik==\"Plot9\"),\"ADD\"]),ext[which(ext$Grafik==\"Plot9\"),\"ADD\"])>>=
                         schule_plot1_2_6_7(lale_emp_MA,klasse=FALSE,schule=schule,referenzgruppe=referenzgruppe,Datenspalte=\"mathewle_Bista\")
                        box(which = \"outer\")
                        @
                        
                        \\Sexpr{inc(\"Plot9\")}
                        
                        \\newpage
                        \\section{Mathematik: Mittlere Leistungswerte der Parallelklassen}

                        Die mittleren Leistungswerte der Parallelklassen werden in Abbildung 10 gezeigt. 
                        
                        Schwarze Pfeile zeigen an, ob der mittlere Leistungswert einer Klasse bedeutsam \"uber ($\\uparrow$) oder bedeutsam unter ($\\downarrow$) dem Mittelwert der Vergleichsgruppe (siehe Seite 4) liegt. 
                        
                        Vom Mittelwert der Vergleichsgruppe unterscheiden sich die Ergebnisse  
                        \\vspace{0.2cm}
                        \\Sexpr{kable(booktabs=T, escape=F, bottomrule=\"\", toprule=\"\", midrule=\"\", do.call(rbind,lapply(seq(1:length(sort(unique(lale_empirisch[lale_empirisch$SCHULNUMMER==schule,]$KLASSE)))),function(x) paste(\"$*$ der Klasse\",sort(unique(lale_empirisch[lale_empirisch$SCHULNUMMER==schule,]$KLASSE))[x],klassenwert(sort(unique(lale_empirisch[lale_empirisch$SCHULNUMMER==schule,]$Klassen_kenn))[x], spalte=\"mathewle_Bista\"),\"\\n\"))),\"latex\") %>% collapse_rows(columns=1,latex_hline=\"none\")}                        
                        \\vspace{0.2cm}
                        Mittelwertunterschiede ab 30 Punkten gelten als bedeutsam.
                        \\newline
                        \\newline
                        \\textbf{Abbildung 10: Mathematik - Mittlere Leistungswerte der Parallelklassen (BISTA-Punkte)}
                        <<Plot7, fig.width=9, fig.height=7, echo=FALSE, background=NA, results=\"asis\", eval=any(is.na(ext[which(ext$Grafik==\"Plot10\"),\"ADD\"]),ext[which(ext$Grafik==\"Plot10\"),\"ADD\"])>>=
                         schule_plot1_2_6_7(lale_emp_MA,klasse=TRUE,schule=schule,referenzgruppe=referenzgruppe,Datenspalte=\"mathewle_Bista\")
                        box(which = \"outer\")
                        @
                        
                        \\Sexpr{inc(\"Plot10\")}
                        
                        \\newpage
                        \\section{Mathematik: Leistungsverteilungen}

                        Die Leistungsverteilung Ihrer Sch\"uler:innen wird in Abbildung 11 dargestellt. Zum Vergleich sind die Ergebnisse der Vergleichsgruppe (siehe Seite 4) aufgenommen. (Erl\"auterungen zu den Leistungsstufen siehe Seite 3 und 4)
                        \\newline
                        \\newline
                        \\textbf{Abbildung 11: Mathematik - Leistungsverteilungen}
                        <<Plot8, fig.width=9, fig.height=7, echo=FALSE, background=NA, results=\"asis\", eval=any(is.na(ext[which(ext$Grafik==\"Plot11\"),\"ADD\"]),ext[which(ext$Grafik==\"Plot11\"),\"ADD\"])>>=
                         schule_plot3_4_8_9(lale_emp_MA,klasse=FALSE,schule=schule,referenzgruppe=referenzgruppe,Datenspalte=\"mathewle_Bista\",regelstandard=regelstandard$regelstandard_Mathe, farben=regelstandard$Farben[-1])
                        box(which = \"outer\")
                        @ 
                        Abbildung 11 zeigt zum Beispiel, dass \\Sexpr{round(prop.table(table(cut(unlist(lale_emp_MA[which(lale_emp_MA$SCHULNUMMER==schule),\"mathewle_Bista\"]),regelstandard$regelstandard_Mathe)))*100)[[5]]} Prozent der Sch\"uler:innen Ihrer Schule die Leistungsstufe 500 bis 600 Punkte erreicht hat. In Ihrer Vergleichsgruppe sind im Vergleich dazu \\Sexpr{round(prop.table(table(cut(unlist(lale_emp_MA[grep(referenzgruppe,lale_emp_MA$Vergleichsgruppe),\"mathewle_Bista\"]),regelstandard$regelstandard_Mathe)))*100)[[5]]} Prozent in dieser Leistungsstufe.
                        
                        \\Sexpr{inc(\"Plot11\")}
                        
                        \\newpage 
                        \\section{Mathematik: Leistungsverteilungen der Parallelklassen}

                        Die Leistungsverteilung Ihrer Sch\"uler:innen der Parallelklassen wird in Abbildung 11.1 dargestellt.
                        \\newline
                        \\newline
                        \\textbf{Abbildung 11.1: Mathematik - Leistungsverteilungen der Parallelklassen}
                        <<Plot9_1, fig.width=9, fig.height=7, echo=FALSE, background=NA, results=\"asis\", eval=any(is.na(ext[which(ext$Grafik==\"Plot11.1\"),\"ADD\"]),ext[which(ext$Grafik==\"Plot11.1\"),\"ADD\"])>>=
                         schule_plot3_4_8_9(lale_emp_MA,klasse=TRUE,schule=schule,referenzgruppe=referenzgruppe,Datenspalte=\"mathewle_Bista\",regelstandard=regelstandard$regelstandard_Mathe, farben=regelstandard$Farben[-1])
                        box(which = \"outer\")
                        @
                        
                        \\Sexpr{inc(\"Plot11.1\")}
                        
                        \\newpage
                        
                        {\\Large \\textbf{Mathematik: Leistungsverteilung der Parallelklassen mit nicht vorhandenen Werten (n.v.)}}

                        Einzelne Sch\"uler:innen haben so wenig Aufgaben bearbeitet (weniger als 10), dass keine Sch\"atzung der Kompetenz m\"oglich ist, der Wert ist also nicht vorhanden (n.v.). In der Leistungsverteilung Ihrer Sch\"uler:innen der Parallelklassen in Abbildung 11.2 sind diese Sch\"uler:innen ohne Wert aufgenommen.
                        \\newline
                        \\newline
                        \\textbf{Abbildung 11.2: Mathematik - Leistungsverteilungen der Parallelklassen inkl. Sch\"uler:innen, die keinen Wert erhalten haben}
                        <<Plot9_2, fig.width=9, fig.height=7, echo=FALSE, background=NA, results=\"asis\", eval=any(is.na(ext[which(ext$Grafik==\"Plot11.2\"),\"ADD\"]),ext[which(ext$Grafik==\"Plot11.2\"),\"ADD\"])>>=
                         schule_plot41_91(lale_empirisch[which(lale_empirisch[,\"TN\"]==1),],schule=schule,referenzgruppe=referenzgruppe,Datenspalte=\"mathewle_Bista\",regelstandard=regelstandard$regelstandard_Mathe, farben=regelstandard$Farben)
                        box(which = \"outer\")
                        @
                        
                        \\Sexpr{inc(\"Plot11.2\")}
                        
                        \\newpage
                        
                        \\section{Mathematik: Anteil der Sch\"uler:innen unter Regelstandard auf Basis des Kompetenzstufenmodells Ende der 4. Jahrgangsstufe}
                        
                        Abbildung 12 zeigt Ihnen, welcher Anteil der Sch\"uler:innen in den Parallelklassen Ihrer Schule den Regelstandard auf Basis des Kompetenzstufenmodells Ende der 4. Jahrgangsstufe im LALE 7-Test nicht erreicht hat (orange). Zum Vergleich werden Ihnen auch die Werte Ihrer Schule und der Vergleichsgruppe angegeben.\\newline\\newline
                        Das Kompetenzstufenmodell f\"ur das Ende der 4. Jahrgangsstufe kann inhaltlich in der 7. Jahrgangsstufe nicht mehr angewendet werden. Auf Basis der in LALE 7 erreichten Punktwerte kann aber festgestellt werden, welche Sch\"uler:innen den notwendigen Wert f\"ur das Erreichen des Regelstandards Ende Klasse 4 zu Beginn der 7. Jahrgangsstufe verfehlt haben.
                        \\newline
                        \\newline
                        \\textbf{Abbildung 12:  Mathematik - Anteil der Sch\"uler:innen unter Regelstandard auf Basis des Kompetenzstufenmodells Ende der 4. Jahrgangsstufe}
                        <<Plot11_3, fig.width=9, fig.height=7, echo=FALSE, background=NA, results=\"asis\", eval=any(is.na(ext[which(ext$Grafik==\"Plot12\"),\"ADD\"]),ext[which(ext$Grafik==\"Plot12\"),\"ADD\"])>>=
                        schule_plot_4_3(lale_emp_MA,schule=schule,referenzgruppe=referenzgruppe,Datenspalte=\"mathewle_Bista\",regelstandard=c(0,460,999))
                        box(which = \"outer\")
                        @
                        
                        \\Sexpr{inc(\"Plot12\")}
                        
                        \\newpage
                        
                        \\section{Mathematik: Mittlere Leistungsentwicklungen}
                        Abbildung 13 zeigt die mittlere Leistungsentwicklung Ihrer Schule seit Beginn der f\"unften Jahrgangsstufe. Bei der Ermittlung der durchschnittlichen Leistungsentwicklungen werden jeweils nur die Sch\"uler:innen ber\"ucksichtigt, die an beiden Testungen teilgenommen haben.
                        \\newline
                        \\newline
                        \\textbf{Abbildung 13: Mathematik - Mittlere Leistungsentwicklungen}
                        <<Plot9_2_1, fig.width=9, fig.height=7, echo=FALSE, background=NA, results=\"asis\", eval=any(is.na(ext[which(ext$Grafik==\"Plot13\"),\"ADD\"]),ext[which(ext$Grafik==\"Plot13\"),\"ADD\"])>>=
                         klasse_plot_LALE7_2(lale_emp_MA,klasse=FALSE,schule=schule,referenzgruppe=referenzgruppe,Datenspalte_Start=\"mathewle_Bista_LALE5\",Datenspalte_Ende=\"mathewle_Bista\")
                        box(which = \"outer\")
                        @
                        
                        \\Sexpr{inc(\"Plot13\")}
                        
                        \\newpage
                        
                        \\section{Mathematik: Mittlere Leistungsentwicklungen der Parallelklassen}
                        Abbildung 14 zeigt die mittleren Leistungsentwicklungen der Parallelklassen seit Beginn der f\"unften Jahrgangsstufe. Bei der Ermittlung der durchschnittlichen Leistungsentwicklungen werden jeweils nur die Sch\"uler:innen ber\"ucksichtigt, die an beiden Testungen teilgenommen haben.
                        \\newline
                        \\newline
                        \\textbf{Abbildung 14: Mathematik - Mittlere Leistungsentwicklungen der Parallelklassen}
                        <<Plot9_2_2, fig.width=9, fig.height=7, echo=FALSE, background=NA, results=\"asis\", eval=any(is.na(ext[which(ext$Grafik==\"Plot14\"),\"ADD\"]),ext[which(ext$Grafik==\"Plot14\"),\"ADD\"])>>=
                         klasse_plot_LALE7_2(lale_emp_MA,klasse=TRUE,schule=schule,referenzgruppe=referenzgruppe,Datenspalte_Start=\"mathewle_Bista_LALE5\",Datenspalte_Ende=\"mathewle_Bista\")
                        box(which = \"outer\")
                        @
                        
                        \\Sexpr{inc(\"Plot14\")}
                        
                        \\newpage
                        \\section{Mathematik: Mittlere Leistungswerte nach Leitideen f\"ur die Fachkonferenz}

                        Abbildung 15 zeigt die mittleren Leistungswerte der Parallelklassen differenziert nach den inhaltsbezogenen mathematischen Anforderungen (Leitideen). Zum Vergleich sind die Ergebnisse Ihrer Schule und der Vergleichsgruppe (siehe Seite 4) aufgenommen.
                        \\newline
                        \\newline
                        \\textbf{Abbildung 15: Mathematik - Mittlere Leistungswerte nach Leitideen (BISTA-Punkte)}
                        <<Plot10, fig.width=9, fig.height=9, echo=FALSE, background=NA, results=\"asis\", eval=any(is.na(ext[which(ext$Grafik==\"Plot15\"),\"ADD\"]),ext[which(ext$Grafik==\"Plot15\"),\"ADD\"])>>=
                        schule_plot10(lale_emp_MA,klasse=klasse,schule=schule,referenzgruppe=referenzgruppe,Datenspalte=mittel_leitidee)
                        @
                        
                        \\Sexpr{inc(\"Plot15\")}
                        
                        \\newpage
                        \\section{Mathematik: L\"osungsh\"aufigkeiten f\"ur die Fachkonferenz}
                        Die Testaufgaben werden nicht ver\"offentlicht. Um Ihnen dennoch eine inhaltlich interpretierbare Ergebnisdarstellung zu liefern, haben wir in der folgenden \"Ubersicht eine Umschreibung jeder Aufgabe aufgenommen.
                        \\newline
                        Den Abbildungen 16-20 ist zu entnehmen, wie viel Prozent der Sch\"uler:innen der Parallelklassen die jeweiligen Aufgaben gel\"ost, nicht gel\"ost oder nicht bearbeitet haben.
                        \\newline
                        \\newline
                        Da die Sch\"uler:innen Testhefte mit unterschiedlicher Zusammenstellung von Aufgaben bearbeitet haben, finden Sie an den Aufgaben verschiedene Angaben f\"ur die Anzahl der Sch\"uler:innen (N). Diesen k\"onnen Sie entnehmen, wie viele Sch\"uler:innen ein Testheft, das diese Aufgabe enthielt, bekommen haben.
                        \\newline
                        \\newif\\ifklassenlong
                        
                        \\Sexpr{N_Klassen}
                        
                        \\ifklassenlong
                        \\textbf{Abbildung 16a: Mathematik - L\"osungsh\"aufigkeiten I - Leitidee 1 - Teil 1}
                        <<Plot11a, fig.width=9, fig.height=8.7, echo=FALSE, background=NA, results=\"asis\", eval=any(is.na(ext[which(ext$Grafik==\"Plot16a\"),\"ADD\"]),ext[which(ext$Grafik==\"Plot16a\"),\"ADD\"])>>=
                        schule_plot11_12_13_14_15(lale_emp_MA,klasse=klasse,schule=schule,referenzgruppe=referenzgruppe,Datenspalte=leitidee_tabelle[[1]][seq(1:(0.5*length(leitidee_tabelle[[1]][,1]))),1],Leitidee=c(\"L1\"),Aufgabennamen=leitidee_tabelle[[1]][seq(1:(0.5*length(leitidee_tabelle[[1]][,1]))),2])
                        @
                        \\Sexpr{inc(\"Plot16a\")}
                        \\newpage
                        \\textbf{Abbildung 16b: Mathematik - L\"osungsh\"aufigkeiten I - Leitidee 1 - Teil 2}
                        <<Plot11b, fig.width=9, fig.height=11.7, echo=FALSE, background=NA, results=\"asis\", eval=any(is.na(ext[which(ext$Grafik==\"Plot16b\"),\"ADD\"]),ext[which(ext$Grafik==\"Plot16b\"),\"ADD\"])>>=
                        schule_plot11_12_13_14_15(lale_emp_MA,klasse=klasse,schule=schule,referenzgruppe=referenzgruppe,Datenspalte=leitidee_tabelle[[1]][-seq(1:(0.5*length(leitidee_tabelle[[1]][,1]))),1],Leitidee=c(\"L1\"),Aufgabennamen=leitidee_tabelle[[1]][-seq(1:(0.5*length(leitidee_tabelle[[1]][,1]))),2])
                        @
                        \\Sexpr{inc(\"Plot16b\")}
                        \\newpage
                        \\textbf{Abbildung 17a: Mathematik - L\"osungsh\"aufigkeiten II - Leitidee 2 - Teil 1}
                        <<Plot12a, fig.width=9, fig.height=11.7, echo=FALSE, background=NA, results=\"asis\", eval=any(is.na(ext[which(ext$Grafik==\"Plot17a\"),\"ADD\"]),ext[which(ext$Grafik==\"Plot17a\"),\"ADD\"])>>=
                        schule_plot11_12_13_14_15(lale_emp_MA,klasse=klasse,schule=schule,referenzgruppe=referenzgruppe,Datenspalte=leitidee_tabelle[[2]][seq(1:(0.5*length(leitidee_tabelle[[2]][,1]))),1],Leitidee=c(\"L2\"),Aufgabennamen=leitidee_tabelle[[2]][seq(1:(0.5*length(leitidee_tabelle[[2]][,1]))),2])
                        @
                        \\Sexpr{inc(\"Plot17a\")}
                        \\newpage
                        \\textbf{Abbildung 17b: Mathematik - L\"osungsh\"aufigkeiten II - Leitidee 2 - Teil 2}
                        <<Plot12b, fig.width=9, fig.height=11.7, echo=FALSE, background=NA, results=\"asis\", eval=any(is.na(ext[which(ext$Grafik==\"Plot17b\"),\"ADD\"]),ext[which(ext$Grafik==\"Plot17b\"),\"ADD\"])>>=
                        schule_plot11_12_13_14_15(lale_emp_MA,klasse=klasse,schule=schule,referenzgruppe=referenzgruppe,Datenspalte=leitidee_tabelle[[2]][-seq(1:(0.5*length(leitidee_tabelle[[2]][,1]))),1],Leitidee=c(\"L2\"),Aufgabennamen=leitidee_tabelle[[2]][-seq(1:(0.5*length(leitidee_tabelle[[2]][,1]))),2])
                        @
                        \\Sexpr{inc(\"Plot17b\")}
                        \\newpage
                        \\textbf{Abbildung 18a: Mathematik - L\"osungsh\"aufigkeiten III - Leitidee 3 - Teil 1}
                        <<Plot13a, fig.width=9, fig.height=11.7, echo=FALSE, background=NA, results=\"asis\", eval=any(is.na(ext[which(ext$Grafik==\"Plot18a\"),\"ADD\"]),ext[which(ext$Grafik==\"Plot18a\"),\"ADD\"])>>=
                        schule_plot11_12_13_14_15(lale_emp_MA,klasse=klasse,schule=schule,referenzgruppe=referenzgruppe,Datenspalte=leitidee_tabelle[[3]][seq(1:(0.5*length(leitidee_tabelle[[3]][,1]))),1],Leitidee=c(\"L3\"),Aufgabennamen=leitidee_tabelle[[3]][seq(1:(0.5*length(leitidee_tabelle[[3]][,1]))),2])
                        @
                        \\Sexpr{inc(\"Plot18a\")}
                        \\newpage
                        \\textbf{Abbildung 18b: Mathematik - L\"osungsh\"aufigkeiten III - Leitidee 3 - Teil 2}
                        <<Plot13b, fig.width=9, fig.height=11.7, echo=FALSE, background=NA, results=\"asis\", eval=any(is.na(ext[which(ext$Grafik==\"Plot18b\"),\"ADD\"]),ext[which(ext$Grafik==\"Plot18b\"),\"ADD\"])>>=
                        schule_plot11_12_13_14_15(lale_emp_MA,klasse=klasse,schule=schule,referenzgruppe=referenzgruppe,Datenspalte=leitidee_tabelle[[3]][-seq(1:(0.5*length(leitidee_tabelle[[3]][,1]))),1],Leitidee=c(\"L3\"),Aufgabennamen=leitidee_tabelle[[3]][-seq(1:(0.5*length(leitidee_tabelle[[3]][,1]))),2])
                        @
                        \\Sexpr{inc(\"Plot18b\")}
                        \\newpage
                        \\textbf{Abbildung 19a: Mathematik - L\"osungsh\"aufigkeiten IV - Leitidee 4 - Teil 1}
                        <<Plot14a, fig.width=9, fig.height=11.7, echo=FALSE, background=NA, results=\"asis\", eval=any(is.na(ext[which(ext$Grafik==\"Plot19a\"),\"ADD\"]),ext[which(ext$Grafik==\"Plot19a\"),\"ADD\"])>>=
                        schule_plot11_12_13_14_15(lale_emp_MA,klasse=klasse,schule=schule,referenzgruppe=referenzgruppe,Datenspalte=leitidee_tabelle[[4]][seq(1:(0.5*length(leitidee_tabelle[[4]][,1]))),1],Leitidee=c(\"L4\"),Aufgabennamen=leitidee_tabelle[[4]][seq(1:(0.5*length(leitidee_tabelle[[4]][,1]))),2])
                        @
                        \\Sexpr{inc(\"Plot19a\")}
                        \\newpage
                        \\textbf{Abbildung 19b: Mathematik - L\"osungsh\"aufigkeiten IV - Leitidee 4 - Teil 2}
                        <<Plot14b, fig.width=9, fig.height=11.7, echo=FALSE, background=NA, results=\"asis\", eval=any(is.na(ext[which(ext$Grafik==\"Plot19b\"),\"ADD\"]),ext[which(ext$Grafik==\"Plot19b\"),\"ADD\"])>>=
                        schule_plot11_12_13_14_15(lale_emp_MA,klasse=klasse,schule=schule,referenzgruppe=referenzgruppe,Datenspalte=leitidee_tabelle[[4]][-seq(1:(0.5*length(leitidee_tabelle[[4]][,1]))),1],Leitidee=c(\"L4\"),Aufgabennamen=leitidee_tabelle[[4]][-seq(1:(0.5*length(leitidee_tabelle[[4]][,1]))),2])
                        @
                        \\Sexpr{inc(\"Plot19b\")}
                        \\newpage
                        \\textbf{Abbildung 20a: Mathematik - L\"osungsh\"aufigkeiten V - Leitidee 5 - Teil 1}
                        <<Plot15a, fig.width=9, fig.height=11.7, echo=FALSE, background=NA, results=\"asis\", eval=any(is.na(ext[which(ext$Grafik==\"Plot20a\"),\"ADD\"]),ext[which(ext$Grafik==\"Plot20a\"),\"ADD\"])>>=
                        schule_plot11_12_13_14_15(lale_emp_MA,klasse=klasse,schule=schule,referenzgruppe=referenzgruppe,Datenspalte=leitidee_tabelle[[5]][seq(1:(0.5*length(leitidee_tabelle[[5]][,1]))),1],Leitidee=c(\"L5\"),Aufgabennamen=leitidee_tabelle[[5]][seq(1:(0.5*length(leitidee_tabelle[[5]][,1]))),2])
                        @
                        \\Sexpr{inc(\"Plot20a\")}
                        \\newpage
                        \\textbf{Abbildung 20b: Mathematik - L\"osungsh\"aufigkeiten V - Leitidee 5 - Teil 2}
                        <<Plot15b, fig.width=9, fig.height=11.7, echo=FALSE, background=NA, results=\"asis\", eval=any(is.na(ext[which(ext$Grafik==\"Plot20b\"),\"ADD\"]),ext[which(ext$Grafik==\"Plot20b\"),\"ADD\"])>>=
                        schule_plot11_12_13_14_15(lale_emp_MA,klasse=klasse,schule=schule,referenzgruppe=referenzgruppe,Datenspalte=leitidee_tabelle[[5]][-seq(1:(0.5*length(leitidee_tabelle[[5]][,1]))),1],Leitidee=c(\"L5\"),Aufgabennamen=leitidee_tabelle[[5]][-seq(1:(0.5*length(leitidee_tabelle[[5]][,1]))),2])
                        @
                        \\Sexpr{inc(\"Plot20b\")}
                        \\else
                        \\textbf{Abbildung 16: Mathematik - L\"osungsh\"aufigkeiten I - Leitidee 1}
                        <<Plot11, fig.width=9, fig.height=8.7, echo=FALSE, background=NA, results=\"asis\", eval=any(is.na(ext[which(ext$Grafik==\"Plot16\"),\"ADD\"]),ext[which(ext$Grafik==\"Plot16\"),\"ADD\"])>>=
                        schule_plot11_12_13_14_15(lale_emp_MA,klasse=klasse,schule=schule,referenzgruppe=referenzgruppe,Datenspalte=leitidee_tabelle[[1]][,1],Leitidee=c(\"L1\"),Aufgabennamen=leitidee_tabelle[[1]][,2])
                        @
                        \\Sexpr{inc(\"Plot16\")}
                        \\newpage
                        \\textbf{Abbildung 17: Mathematik - L\"osungsh\"aufigkeiten II - Leitidee 2}
                        <<Plot12, fig.width=9, fig.height=11.7, echo=FALSE, background=NA, results=\"asis\", eval=any(is.na(ext[which(ext$Grafik==\"Plot17\"),\"ADD\"]),ext[which(ext$Grafik==\"Plot17\"),\"ADD\"])>>=
                        schule_plot11_12_13_14_15(lale_emp_MA,klasse=klasse,schule=schule,referenzgruppe=referenzgruppe,Datenspalte=leitidee_tabelle[[2]][,1],Leitidee=c(\"L2\"),Aufgabennamen=leitidee_tabelle[[2]][,2])
                        @
                        \\Sexpr{inc(\"Plot17\")}
                        \\newpage
                        \\textbf{Abbildung 18: Mathematik - L\"osungsh\"aufigkeiten III - Leitidee 3}
                        <<Plot13, fig.width=9, fig.height=11.7, echo=FALSE, background=NA, results=\"asis\", eval=any(is.na(ext[which(ext$Grafik==\"Plot18\"),\"ADD\"]),ext[which(ext$Grafik==\"Plot18\"),\"ADD\"])>>=
                        schule_plot11_12_13_14_15(lale_emp_MA,klasse=klasse,schule=schule,referenzgruppe=referenzgruppe,Datenspalte=leitidee_tabelle[[3]][,1],Leitidee=c(\"L3\"),Aufgabennamen=leitidee_tabelle[[3]][,2])
                        @
                        \\Sexpr{inc(\"Plot18\")}
                        \\newpage
                        \\textbf{Abbildung 19: Mathematik - L\"osungsh\"aufigkeiten IV - Leitidee 4}
                        <<Plot14, fig.width=9, fig.height=11.7, echo=FALSE, background=NA, results=\"asis\", eval=any(is.na(ext[which(ext$Grafik==\"Plot19\"),\"ADD\"]),ext[which(ext$Grafik==\"Plot19\"),\"ADD\"])>>=
                        schule_plot11_12_13_14_15(lale_emp_MA,klasse=klasse,schule=schule,referenzgruppe=referenzgruppe,Datenspalte=leitidee_tabelle[[4]][,1],Leitidee=c(\"L4\"),Aufgabennamen=leitidee_tabelle[[4]][,2])
                        @
                        \\Sexpr{inc(\"Plot19\")}
                        \\newpage
                        \\textbf{Abbildung 20: Mathematik - L\"osungsh\"aufigkeiten V - Leitidee 5}
                        <<Plot15, fig.width=9, fig.height=11.7, echo=FALSE, background=NA, results=\"asis\", eval=any(is.na(ext[which(ext$Grafik==\"Plot20\"),\"ADD\"]),ext[which(ext$Grafik==\"Plot20\"),\"ADD\"])>>=
                        schule_plot11_12_13_14_15(lale_emp_MA,klasse=klasse,schule=schule,referenzgruppe=referenzgruppe,Datenspalte=leitidee_tabelle[[5]][,1],Leitidee=c(\"L5\"),Aufgabennamen=leitidee_tabelle[[5]][,2])
                        @
                        \\Sexpr{inc(\"Plot20\")}
                        \\fi
                        \\end{flushleft}
                        \\pagestyle{Mathe}

                        \\end{document}",con=filename)
  
  close(filename)
  if (pdf==TRUE) {knit2pdf(paste("LALE-Rueckmeldung_Schule_",schule,".Rnw",sep=""))}
  if (word==TRUE) {system(paste("latex2rtf ","LALE-Rueckmeldung_Schule_",schule,".tex",sep=""))}
  
}



####### Klassenrueckmeldung ########

#testen
#lale_empirisch = lale_empirisch;Auswertung = Auswertung;klasse = y; schuljahr = schuljahr; regelstandard = regelstandard; leitidee_tabelle = leitidee_tabelle_emp; pdf = pdf; logo_dir = logo_dir; Fuenf_Prozent = Fuenf_Prozent; word = word; Legenden=Legenden; Klasse_Grafiken=Klasse_Grafiken[which(paste0(Klasse_Grafiken$Schule,"_",Klasse_Grafiken$Klasse)==y),]; extschueler=Schueler_Grafiken;


klassenrueckmeldung_LALE7 <- function(lale_empirisch, Auswertung, klasse, schuljahr, leitidee_tabelle, regelstandard, pdf = FALSE, logo_dir = "/home/philipp/Desktop/LALE 5 -RCode/Code/Files", Fuenf_Prozent, word, Legenden=Legenden, Klasse_Grafiken, extschueler)
{
  
  leitidee_tabelle<-lapply(leitidee_tabelle,apply,2,as.character)
  mittel_leitidee <- names(leitidee_tabelle)
  schule <- unlist(unique(lale_empirisch[which(lale_empirisch$Klassen_kenn==klasse),"SCHULNUMMER"]))
  referenzgruppe <- as.character(unlist(unique(lale_empirisch[which(lale_empirisch$SCHULNUMMER==schule),"referenzgruppe"])))
  vergleichsschulen <- do.call(paste,cbind("\\item Schule",unique(lale_empirisch[grep(referenzgruppe,lale_empirisch$Vergleichsgruppe),"SCHULNUMMER"]),"-",unique(lale_empirisch[grep(referenzgruppe,lale_empirisch$Vergleichsgruppe),"NAME_SCHULE"])))
  
  ## Die Schulart wird ausgelesen f\"ur die jeweilige Schule
  if (unlist(unique(lale_empirisch[which(lale_empirisch$SCHULNUMMER==schule),"schulart"]))==1){schulart <- 1; names(schulart) <- "Oberschulen"}
  if (unlist(unique(lale_empirisch[which(lale_empirisch$SCHULNUMMER==schule),"schulart"]))==2){schulart <- 2; names(schulart) <- "Gymnasien"}
  
  ## Erklaerende Grafiken speziell fuer LALE7 
  Grafik_A <- function(data_Start=445, data_Ende=524)
  {
    par(oma=c(0,0,0,0),mar=c(4,3,3,2))
    w<-1
    m <- barplot(data_Start, col = NA, border=F, axes = TRUE, horiz = T, plot=T, width=w, space=2, xlim = c(200,800), ylim=c(1.2,3.5),las=1)
    abline(v = c(300,400,500,600,700), col = "darkgray", lty = 1, xpd = F)
    rect(xleft = data_Start, xright = data_Ende, ybottom = m-w/2, ytop = m+w/2, col =c("#8eb4e3"), xpd=T)
    segments(x0 = data_Start, y0 = m-w/2-0.3, x1 = data_Start, y1 = m+w/2+0.3, lwd=3)
    text(paste("(",sprintf("%+g",round(data_Ende)-round(data_Start)),")",sep=""), x = data_Ende+sign(data_Ende-data_Start)*65, y = m)
    text(round(data_Start), x = data_Start+sign(data_Ende-data_Start)*-20, y = m)
    text(round(data_Ende), x = data_Ende+sign(data_Ende-data_Start)*20, y = m)
  }
  
  Grafik_B <- function(regelstandard, farben)
  {
    farben = farben[-1]
    legend.vector <- seq(200,800, 100)
    
    par(oma=c(0,0,0,0),mar=c(0,0,0,0))
    m<-plot(1,ylim=c(1,4), xlim=c(150,850),pch=NA,axes=F,type = "n")
    # legend.vector <- c(regelstandard[-c(1,length(regelstandard))],800)
    
    rect(xleft = legend.vector[-length(legend.vector)], xright = legend.vector[-1], ybottom = rep(2,7), ytop = rep(2.9,7), col = farben, xpd=T)
    text(x=legend.vector,y=rep(3.2,length(legend.vector)),labels=c("",legend.vector[-c(1,length(legend.vector))],""), cex=1.2, xpd=T)
    # text(x=legend.vector,y=rep(3.2,length(legend.vector)),labels=c("",regelstandard[-c(1,2,length(regelstandard))],""), cex=1.2, xpd=T)
  }
  
  ## Plot 1 und 5 Klassenr\"uckmeldung: Barplot: Abbildung 1: Deutsch-Leseverstehen - Mittlere Leistungswerte (BISTA-Punkte)
  ## Barplot: Abbildung 5: Mathematik - Mittlere Leistungswerte (BISTA-Punkte)
  klasse_plot1_5 <- function(lale_empirisch,klasse,schule,referenzgruppe,Datenspalte)
  {
    par(mar = c(3, 3, 3, 0)) # par(mar = c(bottom, left, top, right))
    data <- list(lale_empirisch[which(lale_empirisch$Klassen_kenn==klasse),Datenspalte], lale_empirisch[which(lale_empirisch$SCHULNUMMER==schule),Datenspalte], lale_empirisch[grep(referenzgruppe,lale_empirisch$Vergleichsgruppe),Datenspalte])
    data <- lapply(data,na.omit)
    data <- lapply(data,unlist)
    klassen_wert <- diff(unlist(lapply(data,mean,na.rm=T)[c(1,3)]))
    barplot(unlist(lapply(data,mean,na.rm=TRUE)), col = NA, border = NA, axes = FALSE, ylim=c(100,800), xlim=c(0,5))
    abline(h = c(100,200,300,400,500,600,700), col = "darkgray", lty = 1)
    barplot(unlist(lapply(data,mean,na.rm=TRUE)), axes = FALSE, add = TRUE, width = 1, space=.5, col = c("#10253f","#8eb4e3","#375a84"), xlim=c(0,5), ylim=c(100,800), xpd=FALSE)
    axis(side = 2, tick = TRUE, at = c(100,200,300,400,500,600,700,800), labels = c(100,200,300,400,500,600,700,800), lty = 1, las = 1)
    m <- barplot(unlist(lapply(data,mean,na.rm=TRUE)), col = NA, border = NA, axes = FALSE, width = 1, space=.5, xlim=c(0,5), plot=FALSE)
    text(x = m, y = unlist(lapply(data,mean,na.rm=TRUE))+50, labels = round(unlist(lapply(data,mean,na.rm=TRUE)),0), xpd=TRUE)
    text(x = m, y = unlist(lapply(data,mean,na.rm=TRUE))+50, labels = round(unlist(lapply(data,mean,na.rm=TRUE)),0), xpd=TRUE)
    axis(side = 1, at = m, tick=FALSE, labels = c(paste("Ihre Klasse\n( N =",length(data[[1]]),")"),paste("Ihre Schule\n( N =",length(data[[2]]),")"), paste("Vergleichsgruppe\n( N =",length(data[[3]]),")")))
    
    # Bedeutsam - Pfeile
    if(TRUE%in%(klassen_wert>=30)){arrows(x0 = m[which(klassen_wert>30)], y0 = 850, y1 = 800, x1 = m[which(klassen_wert>30)], xpd=T, lwd = 2, cex =10)}
    if(TRUE%in%(klassen_wert<=(-30))){arrows(x0 = m[which(klassen_wert<(-30))], y0 = 800, y1 = 850, x1 = m[which(klassen_wert<(-30))], xpd=T, lwd = 2, cex =10)}
  }
  
  ## Plot 2 und 6 Klassenr\"uckmeldung: Stacked-Barplot: Abbildung 2: Deutsch-Leseverstehen - Leistungsverteilungen
  ## Stacked-Barplot: Abbildung 6: Mathematik - Leistungsverteilungen
  
  klasse_plot2_6 <- function(lale_empirisch,klasse,schule,referenzgruppe,Datenspalte,regelstandard, farben)
  {
    par(oma = c(3, 3.5, 2, 1)) # par(mar = c(bottom, left, top, right))
    ## muss gemacht werden da die grenzen nicht richtig stimmen bzw. nicht logisch sind in der Angabe
    # regelstandard <- regelstandard[-2]
    data <- lapply(lapply(list(lale_empirisch[which(lale_empirisch$Klassen_kenn==klasse),Datenspalte], lale_empirisch[which(lale_empirisch$SCHULNUMMER==schule),Datenspalte], lale_empirisch[grep(referenzgruppe,lale_empirisch$Vergleichsgruppe),Datenspalte]),na.omit),unlist)
    data.matrix <- 100*matrix(
      c(prop.table(table(cut(data[[1]],regelstandard[-2]))),
        prop.table(table(cut(data[[2]],regelstandard[-2]))),
        prop.table(table(cut(data[[3]],regelstandard[-2]))))
      ,ncol=3)
    m <- barplot(data.matrix, col = farben, axes = FALSE, width = 1, space=.5, main = "", horiz=T, plot=T)
    axis(side = 2, at = m, tick=FALSE, labels = c(paste("Ihre Klasse\n( N =",length(data[[1]]),")"),paste("Ihre Schule\n( N =",length(data[[2]]),")"), paste("Vergleichsgruppe\n( N =",length(data[[3]]),")")), las=2)
    perc <- paste(round(data.matrix),"%",sep="")
    perc[which(perc=="0%")]<- ""
    text(y = m, x = t(apply(data.matrix,2,cumsum)-data.matrix/2), labels = t(matrix(perc,ncol=3)), xpd = TRUE, col=c("black"), cex=1.2)
    
    legend.vector <- c(regelstandard[-c(1,length(regelstandard))],800)
    legend.vector <- legend.vector-legend.vector[1]
    legend.vector <- legend.vector/max(legend.vector)*100
    rect(xleft = legend.vector[-length(legend.vector)], xright = legend.vector[-1], ybottom = rep(-.4,7), ytop = rep(0.1,7), col = farben, xpd=T)
    text(x=legend.vector,y=rep(0.3,length(legend.vector)),labels=c("",regelstandard[-c(1,2,length(regelstandard))],""), cex=1.2, xpd=T)
  }
  
  ## Plot 3 und 7 Klassenr\"uckmeldung: Scatterplot: Abbildung 3: Abbildung 3: Deutsch-Leseverstehen - Individuelle Ergebnisse
  ## Stacked-Barplot: Abbildung 7: Mathematik - Individuelle Ergebnisse
  
  klasse_plot3_7 <- function(lale_empirisch,klasse,Datenspalte,regelstandard, farben, grenzwert)
  {
    par(mar = c(8, 5, 3, 1), oma = c(2, 0, 0, 0), xpd=TRUE) # par(mar = c(bottom, left, top, right))
    data <- na.omit(lale_empirisch[which(lale_empirisch$Klassen_kenn==klasse),c("Pseudonym",Datenspalte)])
    data <- as.data.frame(matrix(unlist(data),ncol=2))
    data <- data[order(data[,2]),]
    data[,2] <- as.numeric(as.character(data[,2]))
    data.text <- data[,2]
    data[which(data[,2]>800),2] <- 800 
    if (length(data[,1])>14){rec_form<-(length(data[,1])-20)/10}else{rec_form<--0.2}
    if (length(data[,1])>14){plot(seq(1:length(data[,1]))~data[,2], xlim=c(100,800), axes=F, xlab="", ylab="", pch = 19)}else{plot(seq(1:length(data[,1]))~data[,2], xlim=c(100,800), ylim=c(1,15),axes=F, xlab="", ylab="", pch = 19)}
    abline(v = c(200,300,400,500,600,700,800), col = "darkgray", lty = 1, xpd=F)
    abline(v=grenzwert, col="black", lty=2, xpd=F)
    pos <- data[,2]+25
    text(seq(1:length(data[,1]))~pos, labels= as.character(round(data.text,0)))
    axis(side=2, at=seq(1:length(data[,1])), labels = data[,1], las=1)
    axis(side=1, at=seq(100,800,100))
    mtext("Leistungswerte", side=1, line=0, cex=1.3, outer=TRUE)
    
    rect(xleft = c(100,regelstandard[-c(1,2,length(regelstandard))]), xright = c(regelstandard[-c(1,2,length(regelstandard))],800+50), ybottom = rep(-2-rec_form,length(regelstandard)-2), ytop =rep(-3-rec_form,length(regelstandard)-2), col = farben)
  }
  
  klasse_plot4 <- function(lale_empirisch,klasse,schule,referenzgruppe,Datenspalte)
  {
    par(mar = c(3, 5, 3, 3),oma = c(0, 0, 2, 0)) # par(mar = c(bottom, left, top, right))
    data <- list(lale_empirisch[which(lale_empirisch$Klassen_kenn==klasse),Datenspalte], lale_empirisch[which(lale_empirisch$SCHULNUMMER==schule),Datenspalte], lale_empirisch[grep(referenzgruppe,lale_empirisch$Vergleichsgruppe),Datenspalte])
    perc.text <- do.call("cbind",lapply(lapply(lapply(data,colMeans,na.rm=T),matrix,nrow=3),function(x) x*100))
    data.matrix <- matrix(apply(perc.text, 2,function(x) c(x,rep(0,6))),nrow=6)[,-seq(0,100,3)]
    
    barplot(data.matrix, col = NA, border = NA, axes = FALSE, ylim=c(0,110), space=c(0.5,0.1))
    abline(h = c(0,20,40,60,80,100), col = "darkgray", lty = 1)
    m <- barplot(plot=T, data.matrix, axes = FALSE, add = TRUE, ylim=c(0,110), col = c("#10253f","#375a84","#8eb4e3","#464E51","gray","lightgray"), space=c(0.5,0.1))
    axis(side = 2, tick = TRUE, at = c(0,20,40,60,80,100), labels = paste(c(0,20,40,60,80,100), rep("%",6)), lty = 1, las = 1)
    perc <- matrix(paste(round(perc.text),"%"),nrow=3)
    perc[which(perc=="0 %")]<- ""
    text(x = m, y = t(apply(perc.text,2,cumsum)-perc.text/2), labels = t(perc), cex = 1.2, xpd = TRUE, col = c("white"))
    axis(side = 1, at = colMeans(matrix(m,nrow=2,byrow=F)) , tick=FALSE, labels = c(paste("Ihre Klasse\n( N =",nrow(data[[1]]),")"),paste("Ihre Schule\n( N =",nrow(data[[2]]),")"), paste("Vergleichsgruppe\n( N =",nrow(data[[3]]),")")))
    
    par(oma = c(0,0,0,0), mar = c(3,5,3,3),new = TRUE)
    legend(x.intersp=1.5,title=c("Sachtext: Die Meisterdiebe"),legend=c(Legenden[1,4],Legenden[1,3],Legenden[1,2]), fill = c("lightgray","gray","#464E51"), y = 145, x = 5, bty="n", ncol = 1, xpd = T)
    legend(x.intersp=1.5,title=c("Sachtext: Bionik"),legend=c(Legenden[1,4],Legenden[1,3],Legenden[1,2]), fill = c("#8eb4e3","#375a84","#10253f"), y = 145, x = 1, bty="n", ncol = 1, xpd = T)
    
    
  }
  
  klasse_plot4.1234 <- function(lale_empirisch,klasse,schule,referenzgruppe,Datenspalte)
  {
    par(mar = c(2, 4, 2, 0)) # par(mar = c(bottom, left, top, right))
    data <- list(lale_empirisch[which(lale_empirisch$Klassen_kenn==klasse),Datenspalte], lale_empirisch[which(lale_empirisch$SCHULNUMMER==schule),Datenspalte], lale_empirisch[grep(referenzgruppe,lale_empirisch$Vergleichsgruppe),Datenspalte])
    data.matrix <- do.call("cbind",lapply(lapply(lapply(data,colMeans, na.rm=TRUE),matrix,nrow=3),function(x) x*100))
    
    barplot(data.matrix, col = NA, border = NA, axes = FALSE, ylim=c(0,110), xlim=c(0,4))
    abline(h = c(0,20,40,60,80,100), col = "darkgray", lty = 1)
    m <- barplot(plot=T, data.matrix, axes = FALSE, add = TRUE,  col = c("#10253f","#375a84","#8eb4e3"), ylim=c(0,110), xlim=c(0,4))
    axis(side = 2, tick = TRUE, at = c(0,20,40,60,80,100), labels = paste(c(0,20,40,60,80,100), rep("%",6)), lty = 1, las = 1)
    perc <- matrix(paste(round(data.matrix),"%"),nrow=3)
    perc[which(perc=="0 %")]<- ""
    text(x = m, y = t(apply(data.matrix,2,cumsum)-data.matrix/2), labels = t(perc), cex = 1.2, xpd = TRUE, col = c("white"))
    axis(side = 1, at = m, tick = FALSE, labels = c("Ihre Klasse","Ihre Schule","Vergleichsgruppe"), xpd=T)
    legend(inset=c(0,-0.25), legend=c(Legenden[2,2],Legenden[2,3],Legenden[2,4]), fill = c("#10253f","#375a84","#8eb4e3"), x = "topleft", bty="n", ncol = 3, xpd=TRUE)
  }
  
  klasse_plot8 <- function(lale_empirisch,klasse,schule,referenzgruppe,Datenspalte=c("ML1","ML2","ML3","ML4","ML5"))
  {
    par(oma = c(1, 8, 6, 1), mar=c(3,0,0,2)) # par(mar = c(bottom, left, top, right))
    Datenspalte <- rev(as.character(Datenspalte))
    data <- list(lale_empirisch[which(lale_empirisch$Klassen_kenn==klasse),Datenspalte], lale_empirisch[which(lale_empirisch$SCHULNUMMER==schule),Datenspalte], lale_empirisch[grep(referenzgruppe,lale_empirisch$Vergleichsgruppe),Datenspalte])
    ## remove NAs
    data <- lapply(data,na.omit)
    data <- rev(data)
    data.matrix <- lapply(lapply(lapply(data,unlist),matrix,ncol=5),colMeans,na.rm=TRUE)
    data.matrix <- do.call(rbind,data.matrix)
    
    barplot(data.matrix, col = NA, border = NA, axes = FALSE, xlim=c(100,800), horiz=T, beside=T)
    abline(v = c(100,200,300,400,500,600,700), col = "darkgray", lty = 1, xpd=FALSE)
    barplot(data.matrix, axes = FALSE, add = TRUE, col = c("#10253f","#8eb4e3","#375a84"), ylim=c(0,4), xlim=c(100,800), horiz=T, beside=T, xpd=FALSE)
    axis(side = 1, tick = TRUE, at = c(100,200,300,400,500,600,700,800), labels = c(100,200,300,400,500,600,700,800), lty = 1, las = 1)
    m <- barplot(data.matrix, col = NA, border = NA, axes = FALSE, plot=FALSE, horiz=T, beside=T)
    text(y = m, x = data.matrix-30, labels = round(data.matrix), col="white")
    axis(las=1, side = 2, at = colMeans(m), labels = c("Leitidee 5:\nDaten und Zufall","Leitidee 4:\nRaum und Form","Leitidee 3:\nFunktionaler\nZusammenhang","Leitidee 2:\nMessen","Leitidee 1:\nZahl")) #2024 laut KMK geaendert
    opar = par(oma = c(0,0,0,0), new = TRUE)
    # Vorsicht daten werden automatisch gedreht mit rev, Farben drehe ich nun manuell
    legend(legend=c(paste("Ihre Klasse\n( N =",nrow(data[[3]]),")"),paste("Ihre Schule\n( N =",nrow(data[[2]]),")"), paste("Vergleichsgruppe\n( N =",nrow(data[[1]]),")")), fill = c("#375a84","#8eb4e3","#10253f"), x = "topleft", bty="n", ncol = 3)
  }  
  
  klasse_plot9_10_11_12_13  <- function(lale_empirisch,klasse,schule,referenzgruppe,Datenspalte=c("MA0005","MA0016","MA0018"),Leitidee=c("L1"),Aufgabennamen=c("Test1","Test2","Test3"))
  {
    klassen_in_schule <- lale_empirisch[which(lale_empirisch$Klassen_kenn==klasse),Datenspalte]
    entferne.NA <- which(colSums(!sapply(klassen_in_schule,is.na))==0)
    if (length(entferne.NA)>0){
      Datenspalte <- Datenspalte[-entferne.NA]
      Aufgabennamen <- Aufgabennamen[-entferne.NA]}
    
    Aufgabennamen <- (as.character(Aufgabennamen)[order(Datenspalte)])
    Datenspalte <- sort(as.character(Datenspalte))
    opar = par(oma = c(3, 10, 3, 14), mar=c(0,0,0,2)) # par(mar = c(bottom, left, top, right))
    data <- list(lale_empirisch[which(lale_empirisch$Klassen_kenn==klasse),Datenspalte], lale_empirisch[which(lale_empirisch$SCHULNUMMER==schule),Datenspalte], lale_empirisch[grep(referenzgruppe,lale_empirisch$Vergleichsgruppe),Datenspalte])
    data <- rev(data)
    data <- lapply(data, function(x) sapply(x, factor, level=c(1,0,99,NA)))
    data.matrix <- lapply(lapply(data,function(x) apply(x,2, function(x) table(factor(x, levels=c(1,0,99))))), prop.table, margin=2)
    data.matrix <- as.matrix(do.call(rbind, lapply(data.matrix,function(x) t(x)*100)))
    data.matrix <- as.matrix(data.matrix[order(row.names(data.matrix),decreasing = F),])
    
    data.matrix.N <- lapply(lapply(data,function(x) apply(x,2, function(x) table(factor(x, levels=c(1,0,99))))), colSums)
    data.matrix.N <- as.matrix(do.call(rbind, lapply(data.matrix.N,function(x) t(x))))
    N.labels <- paste(rep(c("VG (N=", "Schule (N=", "Klasse (N="),length(Aufgabennamen)),data.matrix.N, rep(")",length(Aufgabennamen)),sep="")
    
    barplot(t(data.matrix), col = NA, border = NA, axes = FALSE, xlim=c(0,110), horiz=T, beside=F, space = rep(c(.5,.1,.1)), axisnames = F)
    abline(v = c(0,20,40,60,80,100), col = "darkgray", lty = 1, xpd=FALSE)
    m <- barplot(plot=T, t(data.matrix), axes = FALSE, add = TRUE, col = c("#10253f","#375a84","#8eb4e3"), xlim=c(0,110), horiz=T, beside=F, xpd=TRUE, , space = rep(c(.5,.1,.1)), axisnames = F)
    axis(side = 1, tick = TRUE, at = c(0,20,40,60,80,100), labels = paste(c(0,20,40,60,80,100), rep("%",6)), lty = 1, las = 1)
    perc <- paste(round(data.matrix),"%",sep="")
    perc[which(perc=="0%")]<- ""
    text(y = m, x = t(apply(data.matrix,1,cumsum))-data.matrix/2, labels = perc, xpd=TRUE, col="white")
    axis(las=1, side = 4, at = m[seq(2,length(m),3)], labels = Aufgabennamen, pos=100, tick=F)
    axis(las=1, side = 2, at = m, labels = N.labels, cex.axis=0.8, pos=-5)
    
    par(oma = c(0,0,0,0), mar=c(0,0,0,0),new = TRUE)
    # Achtung Daten sind automatisch gedreht, Farben in der Legende manuell
    legend(legend=c(Legenden[1,2],Legenden[1,3],Legenden[1,4]), fill = c("#10253f","#375a84","#8eb4e3"), x = "topleft", bty="n", ncol = 3)
    
  }
  
  ## Nur fuer LALE7, die vergleichende Grafik 1
  klasse_plot_LALE7_1 <- function(lale_empirisch,klasse,schule,referenzgruppe,Datenspalte_Start,Datenspalte_Ende)
  {
    ## Entferne alle Individuen mit NAS aus LALE 5 im individuellen Plot bleiben sie erhalten
    lale_empirisch <- lale_empirisch[which(!is.na(lale_empirisch[,Datenspalte_Start])==TRUE),]
    par(mar = c(3, 10, 3, 1)) # par(mar = c(bottom, left, top, right))
    data_Start_N <- lapply(list(lale_empirisch[which(lale_empirisch$Klassen_kenn==klasse),Datenspalte_Start], lale_empirisch[which(lale_empirisch$SCHULNUMMER==schule),Datenspalte_Start], lale_empirisch[grep(referenzgruppe,lale_empirisch$Vergleichsgruppe),Datenspalte_Start]),na.omit)
    data_Start <- round(unlist(lapply(lapply(data_Start_N,unlist),mean,na.rm=TRUE)))
    data_Ende_N <- lapply(list(lale_empirisch[which(lale_empirisch$Klassen_kenn==klasse),Datenspalte_Ende], lale_empirisch[which(lale_empirisch$SCHULNUMMER==schule),Datenspalte_Ende], lale_empirisch[grep(referenzgruppe,lale_empirisch$Vergleichsgruppe),Datenspalte_Ende]),na.omit)
    data_Ende <- round(unlist(lapply(lapply(data_Ende_N,unlist),mean,na.rm=TRUE)))
    w <- 1.5
    m <- barplot(data_Start, col = NA, border=F, axes = TRUE, horiz = T, plot=T, width=w, space=2, xlim = c(200,800), ylim = c(0,15))
    abline(v = c(300,400,500,600,700), col = "darkgray", lty = 1, xpd = F)
    rect(xleft = data_Start, xright = data_Ende, ybottom = m-w/2, ytop = m+w/2, col =c("#376092","#b9cde5","#17375e","#c0c0c0","#8eb4e3"), xpd=T)
    segments(x0 = data_Start, y0 = m-w/2-0.3, x1 = data_Start, y1 = m+w/2+0.3, lwd=3)
    text(paste("(",sprintf("%+g",round(data_Ende)-round(data_Start)),")",sep=""), x = data_Ende+sign(data_Ende-data_Start)*65, y = m)
    text(round(data_Start), x = data_Start+sign(data_Ende-data_Start)*-20, y = m)
    text(round(data_Ende), x = data_Ende+sign(data_Ende-data_Start)*20, y = m)
    axis(las = 1, side = 2, at = m, tick=FALSE, labels = c(paste("Ihre Klasse\n( N =",nrow(data_Start_N[[1]]),")"),paste("Ihre Schule\n( N =",nrow(data_Start_N[[2]]),")"), paste("Vergleichsgruppe\n( N =",nrow(data_Start_N[[3]]),")")))
  }
  
  ## Nur fuer LALE7 die vergleichende Grafik 2 (Abb 5)
  
  klasse_plot_LALE7_2 <- function(lale_empirisch,klasse,schule,Datenspalte_Start,Datenspalte_Ende)
  {
   par(mar = c(2, 5, 2, 2)) # par(mar = c(bottom, left, top, right))
    data_Start <- lale_empirisch[which(lale_empirisch$Klassen_kenn==klasse),c("Pseudonym",Datenspalte_Start)]
    data_Ende <- lale_empirisch[which(lale_empirisch$Klassen_kenn==klasse),c("Pseudonym",Datenspalte_Ende)]
    data_Start[which(data_Start[,2]>800),2] <- 800 
    data_Ende[which(data_Ende[,2]>800),2] <- 800 
    data_Start[,2] <- round(data_Start[,2],digits=0)
    data_Ende[,2] <- round(data_Ende[,2],digits=0)
    suppressWarnings(data_Start <- data_Start[order(data_Ende[,2],decreasing = F),])
    suppressWarnings(data_Ende <- data_Ende[order(data_Ende[,2],decreasing = F),])
    diff.vector <- unlist(sign(data_Ende[,2]-data_Start[,2]))
    diff.vector[is.na(diff.vector)] <- 1
    diff.vector[which(diff.vector==0)] <- 1
    B.S <- unlist(data_Start[,2])
    B.E <- unlist(data_Ende[,2])
    plot(seq(1,nrow(data_Start[,1]))~B.S, pch=18, cex = 3, col = "gray", xlab = "Leistungswerte", xlim = c(100,800), axes = F, ylab="")
    axis(las = 1, side = 2, at = seq(1:nrow(data_Start)), tick = T, labels = data_Start$Pseudonym)
    axis(las = 1, side = 1, at = seq(100,800,100), tick=T, labels = seq(100,800,100))
    points(y=seq(1:nrow(data_Ende)), x=B.E, pch=16, cex = 3, col = "darkgray")
    text(round(B.S), x=B.S-(30*diff.vector), y=seq(1:nrow(data_Start)))
    text(round(B.E), x=B.E+(30*diff.vector), y=seq(1:nrow(data_Ende)))
    arrows(x0=B.S, x1=B.E, y1=seq(1:nrow(data_Start)), y0=seq(1:nrow(data_Ende)), length=0.1, angle=20, lwd=4, col=as.character(factor(sign(B.E-B.S),levels=c(1,-1),labels=c("green","red"))))
    }
  
  
  ## Erstelle .Rnw File fuer knit2pdf Konvertierung
  
  ## Ist der Unterschied zur Vergleichsgruppe bedeutsam oder nicht bedeutsam
  if (abs(round(mean(unlist(lale_empirisch[which(lale_empirisch$Klassen_kenn==klasse),"delwlemeanbista"]),na.rm=TRUE))-round(mean(unlist(lale_empirisch[grep(referenzgruppe,lale_empirisch$Vergleichsgruppe),"delwlemeanbista"]),na.rm=TRUE)))<30) {bedeutsam_vergleich <- "* NICHT BEDEUTSAM"} else {bedeutsam_vergleich <- "* BEDEUTSAM"}
  if (abs(round(mean(unlist(lale_empirisch[which(lale_empirisch$Klassen_kenn==klasse),"delwlemeanbista"]),na.rm=TRUE))-round(mean(unlist(lale_empirisch[which(lale_empirisch$SCHULNUMMER==schule),"delwlemeanbista"]),na.rm=TRUE)))<30) {bedeutsam_schule <- "* NICHT BEDEUTSAM"} else {bedeutsam_schule <- "* BEDEUTSAM"}
  
  if (abs(round(mean(unlist(lale_empirisch[which(lale_empirisch$Klassen_kenn==klasse),"mathewle_Bista"]),na.rm=TRUE))-round(mean(unlist(lale_empirisch[grep(referenzgruppe,lale_empirisch$Vergleichsgruppe),"mathewle_Bista"]),na.rm=TRUE)))<30) {bedeutsam_vergleich_mathe <- "* NICHT BEDEUTSAM"} else {bedeutsam_vergleich_mathe <- "* BEDEUTSAM"}
  if (abs(round(mean(unlist(lale_empirisch[which(lale_empirisch$Klassen_kenn==klasse),"mathewle_Bista"]),na.rm=TRUE))-round(mean(unlist(lale_empirisch[which(lale_empirisch$SCHULNUMMER==schule),"mathewle_Bista"]),na.rm=TRUE)))<30) {bedeutsam_schule_mathe <- "* NICHT BEDEUTSAM"} else {bedeutsam_schule_mathe <- "* BEDEUTSAM"}
  
  klassenwert <- function(klasse, spalte="delwlemeanbista")
  {if (abs(round(mean(unlist(lale_empirisch[which(lale_empirisch$Klassen_kenn==klasse),spalte]),na.rm=TRUE))-round(mean(unlist(lale_empirisch[grep(referenzgruppe,lale_empirisch$Vergleichsgruppe),spalte]),na.rm=TRUE)))<30) 
  {return("NICHT BEDEUTSAM")} else {return("BEDEUTSAM")}}
  
  filename <- file(paste("LALE-Rueckmeldung_Schule_",schule,"_Klasse_",klasse,".Rnw",sep=""))
  
  lale_emp_DL <- lale_empirisch[which(!is.na(lale_empirisch$delwlemeanbista)==TRUE),]
  lale_emp_MA <- lale_empirisch[which(!is.na(lale_empirisch$mathewle_Bista)==TRUE),]
  
  ## Funktion zur nachtraeglichen Aenderung oder Einfuegen von Grafiken
  
  inc <- function(plot){ if (plot%in%Klasse_Grafiken$Grafik) 
  { 
    return(paste0("\\begin{figure} 
                     \\textbf{",Klasse_Grafiken[which(Klasse_Grafiken$Grafik==plot),"Legende"],"}
                     \\newline 
                     \\includegraphics[width=",Klasse_Grafiken[which(Klasse_Grafiken$Grafik==plot),"Breit"],"cm",
                  ",height=",Klasse_Grafiken[which(Klasse_Grafiken$Grafik==plot),"Hoch"],"cm",
                  "]{",logo_dir,"/LALE/data/",Klasse_Grafiken[which(Klasse_Grafiken$Grafik==plot),"Dateiname"],"} 
                     \\end{figure}"
    )
    )
  }}
  
  inc.tab <- function(plot){ if (plot%in%Klasse_Grafiken$Grafik) 
  { 
    return(paste0("\\includegraphics[width=",Klasse_Grafiken[which(Klasse_Grafiken$Grafik==plot),"Breit"],"cm",
                  ",height=",Klasse_Grafiken[which(Klasse_Grafiken$Grafik==plot),"Hoch"],"cm",
                  "]{",logo_dir,"/LALE/data/",Klasse_Grafiken[which(Klasse_Grafiken$Grafik==plot),"Dateiname"],"}"
    )
    )
  }}
  
  add <- function(plot){}
  
  dir.create(klasse)
  setwd(klasse)
  
  ## Latex Document ####
  
  writeChar("\\documentclass[a4paper, 11pt]{article}
    
                        \\usepackage{booktabs}
                        \\usepackage{longtable}
                        \\usepackage{array}
                        \\usepackage{multirow, hhline}
                        \\usepackage{wrapfig}
                        \\usepackage{float}
                        \\usepackage{colortbl}
                        \\usepackage{pdflscape}
                        \\usepackage{tabu}
                        \\usepackage{threeparttable}
                        \\usepackage{threeparttablex}
                        \\usepackage[normalem]{ulem}
                        \\usepackage{makecell}
                        \\usepackage{amssymb}
                        \\usepackage{textcomp}
                        \\usepackage{grffile}
                        \\usepackage{enumitem}
                        \\usepackage[compact]{titlesec}
                        \\usepackage{pifont}
                        
                        \\usepackage[includeheadfoot, top=0.5in, bottom=0.5in, left=0.5in, right=0.5in]{geometry}
                        \\usepackage[utf8]{inputenc}
                        \\usepackage{graphicx}
                        \\usepackage{fancyhdr}
                        \\pagestyle{fancy}
                        \\pagenumbering{gobble}
                        \\usepackage[ngerman]{babel}
                        \\usepackage{helvet}
                        \\usepackage{tabularx}
                        \\usepackage{pifont}
                        \\setlength{\\headheight}{30pt}
                        
                        
                        \\usepackage[table]{xcolor}
                      
                        
                        \\renewcommand{\\familydefault}{\\sfdefault}
                        
                        \\usepackage{titletoc}
                       
                        \\pagenumbering{arabic}
                        
	
	
                        \\chead{Klassenr\\\"uckmeldung\\\\}
                        \\lhead{
                                Schule  \\Sexpr{unique(lale_empirisch[which(lale_empirisch$Klassen_kenn==klasse),\"SCHULNUMMER\"])} \\newline
                                Klasse \\Sexpr{unique(lale_empirisch[which(lale_empirisch$Klassen_kenn==klasse),\"KLASSE\"])}}
                        \\rhead{\\includegraphics[width=70px,height=25px]{\\Sexpr{paste0(logo_dir,\"/LALE7.jpg\")}}}

                        \\fancypagestyle{Leer}
                        {
                        \\fancyfoot[C]{\\thepage}
                        \\fancyfoot[L]{Schuljahr \\Sexpr{schuljahr}}
                        }
                        
                         \\fancypagestyle{Lesen}
                        {
                        \\fancyfoot[C]{\\thepage}
                        \\fancyfoot[L]{Schuljahr \\Sexpr{schuljahr}}
                        \\fancyfoot[R]{Deutsch-Leseverstehen}
                        }
                        
                        \\fancypagestyle{Mathe}
                        {
                        \\fancyfoot[C]{\\thepage}
                        \\fancyfoot[L]{Schuljahr \\Sexpr{schuljahr}}
                        \\fancyfoot[R]{Mathematik}
                        }
                        
                        \\fancypagestyle{Uberblick}
                        {
                        \\fancyfoot[C]{\\thepage}
                        \\fancyfoot[L]{Schuljahr \\Sexpr{schuljahr}}
                        \\fancyfoot[R]{Ergebnisse im \\\"Uberblick}
                        }
                        
                        \\newcommand{\\resetHeadWidth}{\\f@nch@setoffs}
                      


                        \\begin{document}
                        
                        \\begin{titlepage}
	                      \\centering
              {\\Large Lernausganglagenerhebung 7}
	                      \\hrule
	                      \\vspace*{4cm}
	                      \\includegraphics[width=0.8\\textwidth]{\\Sexpr{paste0(logo_dir,\"/LALE7.jpg\")}}\\par
	                      \\vspace*{3cm}
                        {\\LARGE Klassenr\"uckmeldung \\Sexpr{schuljahr} \\par}
                        \\vspace{1.5cm}
                        {\\LARGE Schule \\Sexpr{schule} \\par}
                        \\vspace{1cm}
                        {\\LARGE Klasse \\Sexpr{unique(lale_empirisch[which(lale_empirisch$Klassen_kenn==klasse),\"KLASSE\"])} \\par}
	                      \\vfill
                        \\end{titlepage}


                        \\begin{flushleft}
                        \\pagestyle{Leer}
                        \\tableofcontents
                        \\label{}
                        
                        \\vspace{\\fill}
                        
                        Oktober 2024
                        \\vspace{1cm}
                        
                        \\begin{minipage}{0.25\\textwidth}
                        \\includegraphics[width=\\textwidth]{\\Sexpr{paste0(logo_dir,\"/IQHB1.png\")}}
                        \\end{minipage}%
                        %
                        \\begin{minipage}{0.25\\textwidth}
                        \\includegraphics[width=\\textwidth]{\\Sexpr{paste0(logo_dir,\"/ifbq.jpg\")}}
                        \\end{minipage}%
                        %
                        \\begin{minipage}{0.25\\textwidth}
                        \\hspace{0.5cm} \\includegraphics[width=1.1\\textwidth]{\\Sexpr{paste0(logo_dir,\"/SKB_TS.png\")}}
                        \\end{minipage}
                        \\newpage
                        
                         
                        Liebe Kolleg:innen,
                        \\newline
                        \\newline
                        wir freuen uns, Ihnen die Ergebnisse der Lernausganglagenerhebung in der 7. Jahrgangsstufe (LALE 7) mitteilen zu k\"onnen. LALE wird \"uber eine Kooperation des IQHB mit dem IfBQ Hamburg und dem PL Rheinland-Pfalz erm\"oglicht. Die Urheberrechte an den Testaufgaben liegen beim IfBQ Hamburg.
                        \\newline
                        \\newline
                        Das Format der R\"uckmeldungen orientiert sich am Hamburger KERMIT-R\"uckmeldeformat, wurde jedoch an einigen Stellen an spezifische W\"unsche aus den Bremer Schulen angepasst. Das Format wurde mit der LALE AG abgestimmmt.
                        \\newline
                        \\newline
                        Die R\"uckmeldung ist \\textbf{inklusiv}. Die Ergebnisse von Sch\"uler:innen mit sonderp\"adagogischem F\"orderbedarf gehen in die Wertung ein. Das Gleiche trifft zu f\"ur die Ergebnisse von Sch\"uler:innen, die weniger als 12 Monate in Deutschland leben und am Test teilgenommen haben. Dies ist unabh\"angig davon, ob sie ein spezielles Testheft (E-Heft) bearbeitet haben oder nicht. Eine Ausnahme gilt f\"ur Sch\"uler:innen mit F\"orderbedarf im Bereich Wahrnehmungs- und Entwicklungsf\"orderung. Eine Teilnahme fand nur in Einzelf\"allen an wenigen Schulen statt. F\"ur die bessere Vergleichbarkeit wird zwar eine individuelle Sch\"ulerr\"uckmeldung erstellt, aber die Ergebnisse gehen nicht in die Klassen- und Schulwertung ein.
                        \\newline
                        \\newline
                        Die \\textbf{Pseudonyme} der einzelnen \\textbf{Sch\"uler:innen} k\"onnen Sie im Sch\"ulerverzeichnis \"uber das Men\"u unter â€žDrucken â€“ Auswerten -$>$ Klassen â€“ Berichte -$>$ Klassenlisten -$>$ Klassenliste LALE 7â€œ erneut aufrufen. Sollte sich seit der Testung die Klassenzuordnung ge\"andert haben, ist diese \"Anderung automatisch in der neuen Liste bzw. im R\"uckmeldeformat ber\"ucksichtigt (Stichtag 27.09.2024).
                        \\newline
                        \\newline
                        \\textbf{Kontakt}
                        \\newline
                        \\newline
                        Wenn Sie Fragen oder Anmerkungen zu den R\"uckmeldungen haben, freuen wir uns, wenn Sie Kontakt zu uns aufnehmen.
                        \\newline
                        \\newline
                        \\begin{tabular}{p{7.5cm}p{7.5cm}}
                        Dr. Claudia Zierul & Andrea Timcke\\\\
                        IQHB & IQHB\\\\
                        Tel.: 0421 361-32049 & Tel.: 0421 361-30704 \\\\
                        E-Mail: claudia.zierul@iqhb.bremen.de & E-Mail: andrea.timcke@iqhb.bremen.de\\\\
                        \\end{tabular}
                        \\newline
                        \\newline
                        \\newline
                        \\newline
                        \\begin{minipage}{0.45\\textwidth}
                        \\textbf{Vernetzung und Material auf itslearning}\\\\
                        \\\\
                        Es gibt auf itslearning einen Kurs zur Vernetzung und zum Materialaustausch. Sie finden diesen Kurs unter Kurse, auf â€žWeitere Kurse suchen$>$Kurskatalogâ€œ klicken und dort â€žLALE 5 + 7 Netzwerkâ€œ eingeben.\\\\
                        \\end{minipage}
                        \\hfill
                        \\begin{minipage}{0.45\\textwidth}\\begin{flushright}\\includegraphics[width=\\textwidth]{\\Sexpr{paste0(logo_dir,\"/itslearning.jpg\")}}\\end{flushright}\\end{minipage}
                        \\newpage
                        Im itslearning-Kurs zu LALE 5 + 7:
                        \\begin{itemize}
                        \\item wird auf aktuelle Fortbildungen und Unterst\"utzungsformate hingewiesen.
                        \\item finden Sie Materialien aus LALE-Veranstaltungen, zum Hintergrund des Verfahrens und zur Weiterarbeit.
                        \\item haben Sie die M\"oglichkeit, sich im Forum auszutauschen.
                        \\item finden Sie Kontaktdaten der Ansprechpersonen der anderen LALE-Schulen.
                        \\end{itemize}
                        \\vspace{1cm}
                        {\\Large\\textbf{Hinweise zu den R\"uckmeldeformaten von LALE 7}}
                        \\newline
                        \\newline
                        Ein paar allgemeine Erkl\"arungen zu den R\"uckmeldeformaten von LALE 7 m\"ochten wir Ihnen vorab geben:
                        \\newline
                        \\newline
                        \\textbf{498 Punkte - Punkte auf der BISTA-Skala}
                        \\newline
                        \\newline
                        F\"ur eine erste Einsch\"atzung werden Ihnen die Punkte auf der BISTA-Skala (Messskala der Bildungsstandards) zur\"uckgemeldet. Interessant ist f\"ur Sie an dieser Stelle also nicht der Wert an sich, sondern die Einordnung zu den Vergleichswerten. 
                                                \\newline
                        \\newline
                        Die LALE 7-Ergebnisse werden Ihnen auf derselben Skala zur\"uckgemeldet wie die LALE 5-Ergebnisse vor zwei Jahren. Durch die Abbildung beider Werte in einer Grafik kann man ablesen, wie sich die diesj\"ahrigen Siebtkl\"assler entwickelt haben: 
                        <<Grafik_A, fig.width=7, fig.height=3, echo=FALSE, background=NA, results=\"asis\">>=
                        Grafik_A(data_Start=445, data_Ende=524)
                        @
                        
                      \\newpage
                      \\textbf{Leistungsstufen (LALE 7) statt Kompetenzstufen (LALE 5)}
                      \\newline
                      \\newline
                        Da es f\"ur den siebten Jahrgang kein anwendbares Kompetenzstufenmodell gibt (bei LALE 5 wird der Bezug zu den Bildungsstandards f\"ur Ende 4. Jahrgangstufe hergestellt), k\"onnen die Ergebnisse bei LALE 7 nicht in einer Kompetenzstufenverteilung dargestellt werden. Stattdessen wurden sechs Leistungsstufen gebildet:
                        <<Grafik_B, fig.width=7, fig.height=1.5, echo=FALSE, background=NA, results=\"asis\">>=
                        Grafik_B(regelstandard=regelstandard$regelstandard_Mathe, farben=regelstandard$Farben)
                        @
                        
                        Im Gegensatz zu den Kompetenzstufen, k\"onnen die Leistungsstufen nicht inhaltlich interpretiert werden. Aber wie die Kompetenzstufen lassen sie einen Vergleich der Leistungen zu, z.B. â€žWie viel Prozent der Sch\"uler:innen liegen an unserer Schule in der untersten Leistungsstufe? Wie viel Prozent sind es in der Vergleichsgruppe?â€œ
                        \\newline
                        \\newline
                        \\textbf{Warum werden doch Sch\"uler:innen unter Regelstandard mit Bezug auf das Kompetenzstufenmodell Ende der 4. Jahrgangsstufe zur\"uckgemeldet?}
                        \\newline
                        \\newline
                        Auch wenn das Kompetenzstufenmodell der Bildungsstandards f\"ur das Ende der 4. Jahrgangsstufe in der 7. Jahrgangsstufe inhaltlich nicht mehr anwendbar ist, so kann man dennoch auf Basis der Punktwerte Sch\"uler:innen identifizieren, die den Wert f\"ur ein Erreichen der Regelstandards auch in der 7. Jahrgangsstufe nicht erzielt haben.
                        \\newline
                        \\newline
                        \\textbf{80\\% - L\"osungsh\"aufigkeiten der Aufgaben}
                        \\newline
                        \\newline
                        Es ist kaum m\"oglich im LALE-7-Test 100 Prozent der Aufgaben richtig zu l\"osen. Damit LALE 7 \"uber alle Sch\"uler:innen m\"oglichst genau misst, liegt die mittlere L\"osungsh\"aufigkeit \"uber alle Testhefte bei etwa 50 Prozent, Sch\"uler:innen der 7. Klasse k\"onnen also im Mittel 50 Prozent der Aufgaben richtig l\"osen. Das heisst, mit 80 Prozent liegt man bereits deutlich \"uber dem Durchschnitt.
                        \\newline
                        \\newline
                        \\textbf{Wer ist meine Vergleichsgruppe?}
                        \\newline
                        \\newline
                        Ihre Vergleichsgruppe setzt sich aus Ihrer Schule und anderen \\Sexpr{names(schulart)} mit
                        \"ahnlichem sozialen Kontext zusammen. Sie soll Ihnen bei der Einordnung Ihrer Ergebnisse
                        helfen und einen â€žfairenâ€œ  Vergleich erm\"oglichen. In diesem Jahr besteht Ihre
                        Vergleichsgruppe aus den folgenden Schulen:
                        \\newline
                        \\newline
                        \\begin{itemize}
                        <<results=\"asis\", echo=FALSE>>=
                        cat(vergleichsschulen, sep=\"\\n\")
                        @
                        \\end{itemize}
                        
                        \\newpage
                        \\pagestyle{Lesen}
                        \\section{Deutsch-Leseverstehen: Mittlere Leistungswerte}
                        Die Sch\"uler:innen Ihrer Klasse erzielten in Deutsch-Leseverstehen durchschnittlich \\Sexpr{round(mean(unlist(lale_emp_DL[which(lale_emp_DL$Klassen_kenn==klasse),\"delwlemeanbista\"]),na.rm=TRUE))} Punkte (siehe Abbildung 1). In Ihrer Klasse haben \\Sexpr{nrow(lale_empirisch[which(lale_emp_DL$Klassen_kenn==klasse),])} Sch\"uler:innen (N=\\Sexpr{nrow(lale_emp_DL[which(lale_emp_DL$Klassen_kenn==klasse),])}) teilgenommen.
                        \\newline
                        \\newline
                        Schwarze Pfeile zeigen an, ob der mittlere Leistungswert Ihrer Klasse bedeutsam \"uber ($\\uparrow$) oder bedeutsam unter ($\\downarrow$) dem Mittelwert der Vergleichsgruppe liegt. 
                        \\vspace{0.5cm}
                        Das Ergebnis Ihrer Klasse unterscheidet sich                  \\newline
                        \\Sexpr{bedeutsam_schule} vom Mittelwert der Schule.\\newline
                        \\Sexpr{bedeutsam_vergleich} vom Mittelwert der Vergleichsgruppe. 
                        \\newline
                        \\newline
                        Mittelwertunterschiede ab 30 Punkten gelten als bedeutsam. 
                        \\newline
                        \\newline
                        \\textbf{Abbildung 1: Deutsch-Leseverstehen - Mittlere Leistungswerte (BISTA-Punkte)}
                        <<Plot1, fig.width=9, fig.height=7, echo=FALSE, background=NA, results=\"asis\", eval=any(is.na(Klasse_Grafiken[which(Klasse_Grafiken$Grafik==\"Plot1\"),\"ADD\"]),Klasse_Grafiken[which(Klasse_Grafiken$Grafik==\"Plot1\"),\"ADD\"])>>=
                        klasse_plot1_5(lale_emp_DL,klasse=klasse,schule=schule,referenzgruppe=referenzgruppe,Datenspalte=\"delwlemeanbista\")
                        box(which = \"outer\")
                        @
                        
                        \\Sexpr{inc(\"Plot1\")}
                        
                        \\newpage
                        
                        \\section{Deutsch-Leseverstehen: Leistungsverteilungen}
                        Die Leistungsverteilung Ihrer Sch\"uler:innen wird in Abbildung 2 dargestellt. Zum Vergleich sind die Ergebnisse Ihrer Schule und der Vergleichsgruppe (siehe Seite 4) aufgenommen. (Erl\"auterungen zu den Leistungsstufen siehe Seite 3 und 4)
                        \\newline
                        \\newline
                        \\textbf{Abbildung 2: Deutsch-Leseverstehen - Leistungsverteilungen}
                        <<Plot2, fig.width=9, fig.height=7, echo=FALSE, background=NA, results=\"asis\", eval=any(is.na(Klasse_Grafiken[which(Klasse_Grafiken$Grafik==\"Plot2\"),\"ADD\"]),Klasse_Grafiken[which(Klasse_Grafiken$Grafik==\"Plot2\"),\"ADD\"])>>=
                        klasse_plot2_6(lale_emp_DL,klasse=klasse,schule=schule,referenzgruppe=referenzgruppe,Datenspalte=\"delwlemeanbista\",regelstandard=regelstandard$regelstandard_Lesen, farben=regelstandard$Farben[-1])
                        box(which = \"outer\")
                        @
                        Hinweis: ein Sch\"uler oder eine Sch\"ulerin in Ihrer Klasse entspricht \\Sexpr{round((1/nrow(lale_emp_DL[which(lale_emp_DL$Klassen_kenn==klasse),]))*100)} \\%.
                        \\newline
                        \\newline
                        Abbildung 2 zeigt zum Beispiel, dass \\Sexpr{round(prop.table(table(cut(unlist(lale_emp_DL[which(lale_emp_DL$Klassen_kenn==klasse),\"delwlemeanbista\"]),regelstandard$regelstandard_Lesen)))*100)[[5]]} Prozent der Sch\"uler:innen Ihrer Klasse die Leistungsstufe 500 bis 600 Punkte erreicht hat. In Ihrer Vergleichsgruppe sind im Vergleich dazu \\Sexpr{round(prop.table(table(cut(unlist(lale_emp_DL[grep(referenzgruppe,lale_emp_DL$Vergleichsgruppe),\"delwlemeanbista\"]),regelstandard$regelstandard_Lesen)))*100)[[5]]} Prozent in dieser Leistungsstufe.
              
                        \\Sexpr{inc(\"Plot2\")}
                        
                        \\newpage
                        \\section{Deutsch-Leseverstehen: Individuelle Ergebnisse}
                        Die Verteilung der Leistungswerte in Ihrer Klasse wird in Abbildung 3 veranschaulicht. Die einzelnen Werte der Sch\"uler:innen sind mit ihrem Pseudonym aufgelistet und in aufsteigender Reihenfolge nach ihrem Leistungswert angeordnet. Die gestrichelte Linie zeigt Ihnen die rechnerische Grenze zwischen Mindeststandard und Regelstandard auf Basis des Kompetenzstufenmodells Ende der 4. Jahrgangsstufe, n\"ahere Erl\"auterungen siehe Seite 3 und 4.
                        \\newline
                        \\newline
                        \\textbf{Abbildung 3: Deutsch-Leseverstehen - Individuelle Ergebnisse}
                        <<Plot3, fig.height=8, echo=FALSE, background=NA, results=\"asis\", eval=any(is.na(Klasse_Grafiken[which(Klasse_Grafiken$Grafik==\"Plot3\"),\"ADD\"]),Klasse_Grafiken[which(Klasse_Grafiken$Grafik==\"Plot3\"),\"ADD\"])>>=
                        klasse_plot3_7(lale_emp_DL,klasse=klasse,Datenspalte=\"delwlemeanbista\",regelstandard=regelstandard$regelstandard_Lesen, farben=regelstandard$Farben[-1], grenzwert=465)
                        box(which = \"outer\")
                        @
                        \\vfill
                        
                        \\Sexpr{inc(\"Plot3\")}
                        
                        \\newpage
                        \\section{Deutsch-Leseverstehen: Mittlere Leistungsentwicklungen}
                        Abbildung 4 zeigt die mittlere Leistungsentwicklung Ihrer Klasse seit Beginn der f\"unften Jahrgangsstufe. Bei der Ermittlung der durchschnittlichen Leistungsentwicklungen werden jeweils nur die Sch\"uler:innen ber\"ucksichtigt, die an beiden Testungen teilgenommen haben.
                        \\newline
                        \\newline
                        \\textbf{Abbildung 4: Deutsch-Leseverstehen - Mittlere Leistungsentwicklungen}
                         <<Plot7.1, fig.width=9, fig.height=8, echo=FALSE, background=NA, results=\"asis\", eval=any(is.na(Klasse_Grafiken[which(Klasse_Grafiken$Grafik==\"Plot4\"),\"ADD\"]),Klasse_Grafiken[which(Klasse_Grafiken$Grafik==\"Plot4\"),\"ADD\"])>>=
                        klasse_plot_LALE7_1(lale_empirisch=lale_emp_DL,klasse=klasse,schule=schule,referenzgruppe=referenzgruppe,Datenspalte_Start=\"delwlemeanbista_LALE5\",Datenspalte_Ende=\"delwlemeanbista\")
                        box(which = \"outer\")
                        @
                        \\vfill
                        
                        \\Sexpr{inc(\"Plot4\")}
                        
                        \\newpage
                        \\section{Deutsch-Leseverstehen: Leistungsentwicklungen individuell}
                        Abbildung 5 zeigt Ihnen f\"ur die Sch\"uler:innen Ihrer Klasse, die schon an LALE5 teilgenommen haben, die Lernentwicklung seit der f\"unften Jahrgangsstufe (Erl\"auterungen siehe Seite 3 und 4). Die LALE 7-Werte sind als Kreise dargestellt und wie in Abbildung 3 in aufsteigender Reihenfolge angeordnet, die LALE 5-Werte sind als Rauten dargestellt. 
                        \\newline
                        \\newline
                        \\textbf{Abbildung 5: Deutsch-Leseverstehen - Leistungsentwicklungen individuell}
                         <<Plot7.2, fig.width=9, fig.height=8, echo=FALSE, background=NA, results=\"asis\", eval=any(is.na(Klasse_Grafiken[which(Klasse_Grafiken$Grafik==\"Plot5\"),\"ADD\"]),Klasse_Grafiken[which(Klasse_Grafiken$Grafik==\"Plot5\"),\"ADD\"])>>=
                        klasse_plot_LALE7_2(lale_empirisch=lale_emp_DL,klasse=klasse,schule=schule,Datenspalte_Start=\"delwlemeanbista_LALE5\",Datenspalte_Ende=\"delwlemeanbista\")
                        box(which = \"outer\")
                        @
                        \\vfill
                        
                        \\Sexpr{inc(\"Plot5\")}
                        
                        \\newpage
                        \\section[Deutsch-Leseverstehen: Hinweise zu den Aufgaben und L\"osungsh\"aufigkeiten]{Deutsch-Leseverstehen: Hinweise zu den Aufgaben und L\"osungsh\"aufigkeiten}
                        
                        Die Testaufgaben werden nicht ver\"offentlicht. Um Ihnen dennoch eine inhaltliche interpretierbare Ergebnisdarstellung zu liefern, haben wir zus\"atzliche Hinweise zu den Aufgaben zusammengestellt. 
                        \\newline
                        \\newline
                        In der folgenden Tabelle werden die Lesetexte kurz charakterisiert. Die mittlere L\"osungsh\"aufigkeit Ihrer Klasse wird dann f\"ur die beiden Texte im Vergleich zur Schule und zur Vergleichsgruppe dargestellt. 
                        
                        \\renewcommand{\\arraystretch}{1.8}
                        \\begin{small}
                        \\begin{tabular}{|p{2cm}|p{7.57cm}|p{7.57cm}|}
                        \\hline
                         & \\cellcolor{lightgray} Sachtext: Bionik & \\cellcolor{gray}Sachtext: Die Meisterdiebe\\\\ \\hline
                        Texttyp & Naturwissenschaftlicher Bericht aus einer Jugendzeitung & Mittellanger Text\\\\ \\hline
                        Merkmale &  \\begin{itemize}[leftmargin=*]
                                    \\item mittlere Informationsdichte
                                    \\item klare Struktur mit teils komplexem Satzbau
                                    \\item wissenschaftlicher â€“ aber verst\"andlich beschriebener â€“ einfacher Wortschatz
                                   \\end{itemize}
                                &
                                   \\begin{itemize}[leftmargin=*]
                                    \\item hohe Informationsdichte 
                                    \\item Ereignisse werden chronologisch erz\"ahlt
                                    \\item Identifikationsfiguren vorhanden
                                   \\end{itemize}
  
  
                                  \\\\ \\hline
                        Bildungs- \\newline standards & 
                                   \\begin{itemize}[leftmargin=*]
                                    \\item Wortbedeutungen kl\"aren (3.2.3)
                                    \\item zentrale Aussagen erschliessen (3.3.4)
                                    \\item Informationen zielgerichtet entnehmen, ordnen, vergleichen, pr\"ufen und erg\"anzen (3.4.3)
                                    \\item aus Sach- und Gebrauchstexten begr\"undete Schlussfolgerungen ziehen (3.4.6)
                                    \\item Intention eines Textes erkennen, insbesondere Zusammenhang zwischen Autorenintention, Textmerkmalen, Leseerwartungen und Wirkungen (3.4.5)
                                   \\end{itemize}
                                &
                                   \\begin{itemize}[leftmargin=*]
                                    \\item gezielt einzelne Informationen suchen (3.3.2)
                                    \\item Texte genau lesen (3.3.3)
                                    \\item zentrale Aussagen eines Textes erfassen und wiedergeben (3.3.6)
                                   \\end{itemize}  
                                  \\\\ \\hline
                        \\end{tabular}
                        \\end{small}
                        \\renewcommand{\\arraystretch}{1}
                        \\newline
                        \\newline
                        \\textbf{Abbildung 6: Deutsch-Leseverstehen - L\"osungsh\"aufigkeiten der Aufgaben der beiden Texte}
                        <<Plot4, fig.width=9, fig.height=3, echo=FALSE, background=NA, results=\"asis\", eval=any(is.na(Klasse_Grafiken[which(Klasse_Grafiken$Grafik==\"Plot6\"),\"ADD\"]),Klasse_Grafiken[which(Klasse_Grafiken$Grafik==\"Plot6\"),\"ADD\"])>>=
                        klasse_plot4(lale_emp_DL,klasse=klasse,schule=schule,referenzgruppe=referenzgruppe,Datenspalte=c(\"Bionik_Richtig_Deutsch\",\"Bionik_Falsch_Deutsch\",\"Bionik_99_Deutsch\",\"Die Meisterdiebe_Richtig_Deutsch\",\"Die Meisterdiebe_Falsch_Deutsch\",\"Die Meisterdiebe_99_Deutsch\"))
                        box(which = \"outer\")
                        @
                        
                        \\Sexpr{inc(\"Plot6\")}
                        
                        \\newpage
                        \\section{Deutsch-Leseverstehen: L\"osungsh\"aufigkeiten zum Stufenmodell zur Aufgabenbeurteilung von Aufgaben (Zabka, 2008)}
                        Die Aufgaben aus dem Testbereich Deutsch Leseverstehen wurden zus\"atzlich nach den unterschiedlichen Teilkompetenzen zum Leseverstehen kategorisiert, die zur Beantwortung der Aufgaben gefordert werden. Dadurch erhalten Sie noch einmal Hinweise darauf, welche Teilkompetenzen bei Ihren Sch\"uler:innen besonders gut ausgepr\"agt sind und welche nicht. Eine ausf\"uhrliche Dokumentation des Stufenmodells finden Sie auf itslearning im Kurs \"LALE 5 + 7 Netzwerk\".
                        \\vspace{0.2cm}
                        \\begin{tabular}{|p{9.5cm}|p{8.2cm}|}
                        \\hline
                        Teilkompetenz & L\"osungsh\"aufigkeiten der Aufgaben\\\\ \\hline
                        \\noindent\\parbox[t][-1.5cm][b]{\\hsize}{\\footnotesize\\textbf{1. Manifeste Information und Informationsverkn\"upfungen verstehen.} \\newline Einzelne manifeste Informationen m\"ussen in einem begrenzten Bereich (Abschnitt) des Lesetextes identifiziert werden.}
                        & 
                      <<Plot51, fig.width=6, fig.height=2.5, echo=FALSE, background=NA, results=\"asis\", eval=any(is.na(Klasse_Grafiken[which(Klasse_Grafiken$Grafik==\"Plot6.1\"),\"ADD\"]),Klasse_Grafiken[which(Klasse_Grafiken$Grafik==\"Plot6.1\"),\"ADD\"])>>=
                        klasse_plot4.1234(lale_emp_DL,klasse,schule,referenzgruppe=referenzgruppe,Datenspalte=c(\"Z1_Zabka_Richtig\",\"Z1_Zabka_Falsch\",\"Z1_Zabka_99\"))
                        @
                        \\Sexpr{inc.tab(\"Plot6.1\")}
                        \\\\ \\hline
                        \\noindent\\parbox[t][-0.5cm][b]{\\hsize}{\\footnotesize \\textbf{2. Informationen und Informationsverkn\"upfungen lokal erschliessen.} \\newline Die zu suchenden Informationen sind nicht zwingend manifest, sondern m\"ussen erschlossen werden. Dabei m\"ussen zuweilen auch (zwei) Informationen \"uber den gesamten Text hinweg zusammengef\"ugt werden.}
                        & 
                      <<Plot52, fig.width=6, fig.height=2.5, echo=FALSE, background=NA, results=\"asis\", eval=any(is.na(Klasse_Grafiken[which(Klasse_Grafiken$Grafik==\"Plot6.2\"),\"ADD\"]),Klasse_Grafiken[which(Klasse_Grafiken$Grafik==\"Plot6.2\"),\"ADD\"])>>=
                        klasse_plot4.1234(lale_emp_DL,klasse,schule,referenzgruppe=referenzgruppe,Datenspalte=c(\"Z2_Zabka_Richtig\",\"Z2_Zabka_Falsch\",\"Z2_Zabka_99\"))
                        @
                        \\Sexpr{inc.tab(\"Plot6.2\")}
                        \\\\ \\hline
                        \\noindent\\parbox[t][-1cm][b]{\\hsize}{\\footnotesize \\textbf{3. Globale Zusammenh\"ange verstehen.} \\newline Ein koh\"arentes Mindestverst\"andnis des Textes soll vorhanden sein, indem Informations-, Argumentations- oder Handlungszusammenh\"ange verstanden werden. Diese Zusammenh\"ange k\"onnen \"uber den Text verstreut sein und Schlussfolgerungen erfordern.}
                        & 
                      <<Plot53, fig.width=6, fig.height=2.5, echo=FALSE, background=NA, results=\"asis\", eval=any(is.na(Klasse_Grafiken[which(Klasse_Grafiken$Grafik==\"Plot6.3\"),\"ADD\"]),Klasse_Grafiken[which(Klasse_Grafiken$Grafik==\"Plot6.3\"),\"ADD\"])>>=
                        klasse_plot4.1234(lale_emp_DL,klasse,schule,referenzgruppe=referenzgruppe,Datenspalte=c(\"Z3_Zabka_Richtig\",\"Z3_Zabka_Falsch\",\"Z3_Zabka_99\"))
                        @
                        \\Sexpr{inc.tab(\"Plot6.3\")}
                        \\\\ \\hline
                        \\noindent\\parbox[t][-0.1cm][b]{\\hsize}{\\footnotesize \\textbf{4. Informationen in Begriffen und Vorstellungen zusammenfassen.} \\newline \"Ubergeordnete Begriffe und Vorstellungen zur Zusammenfassung und Interpretation des Textes sollen vorhanden sein. Diese Vorstellungen k\"onnen sich beispielsweise auf den Ort und die Umst\"ande der dargestellten Wirklichkeit, die Gef\"uhle und Einstellungen der \\newline Figuren oder auf die Hauptaussage des gesamten Textes beziehen.}
                        & 
                      <<Plot54, fig.width=6, fig.height=2.5, echo=FALSE, background=NA, results=\"asis\", eval=any(is.na(Klasse_Grafiken[which(Klasse_Grafiken$Grafik==\"Plot6.4\"),\"ADD\"]),Klasse_Grafiken[which(Klasse_Grafiken$Grafik==\"Plot6.4\"),\"ADD\"])>>=
                        klasse_plot4.1234(lale_emp_DL,klasse,schule,referenzgruppe=referenzgruppe,Datenspalte=c(\"Z4_Zabka_Richtig\",\"Z4_Zabka_Falsch\",\"Z4_Zabka_99\"))
                        @
                        \\Sexpr{inc.tab(\"Plot6.4\")}
                        \\\\ \\hline
                        \\noindent\\parbox[t][-1cm][b]{\\hsize}{\\footnotesize \\textbf{5. Sprach- und Textgestaltung interpretieren.} \\newline Sprachbewusstes Textverstehen: wesentliche rhetorische und stilistische Texteigenschaften sollen wahrgenommen werden und text\"ubergreifende Strukturen (z.B. Funktion von Textsorten) zur Interpretation des Textes genutzt werden.}
                        & 
                      <<Plot55, fig.width=6, fig.height=2.5, echo=FALSE, background=NA, results=\"asis\", eval=any(is.na(Klasse_Grafiken[which(Klasse_Grafiken$Grafik==\"Plot6.5\"),\"ADD\"]),Klasse_Grafiken[which(Klasse_Grafiken$Grafik==\"Plot6.5\"),\"ADD\"])>>=
                        klasse_plot4.1234(lale_emp_DL,klasse,schule,referenzgruppe=referenzgruppe,Datenspalte=c(\"Z5_Zabka_Richtig\",\"Z5_Zabka_Falsch\",\"Z5_Zabka_99\"))
                        @
                        \\Sexpr{inc.tab(\"Plot6.5\")}
                        \\\\ \\hline
                        \\end{tabular}
                        
                        
                        
                        \\newpage
                        \\pagestyle{Mathe}
                        \\section{Mathematik: Mittlere Leistungswerte}
                        Die Sch\"uler:innen Ihrer Klasse erzielten in Mathematik durchschnittlich \\Sexpr{round(mean(unlist(lale_emp_MA[which(lale_emp_MA$Klassen_kenn==klasse),\"mathewle_Bista\"]),na.rm=TRUE))} Punkte (siehe Abbildung 1). In Ihrer Klasse haben \\Sexpr{nrow(lale_emp_MA[which(lale_emp_MA$Klassen_kenn==klasse),])} Sch\"uler:innen (N=\\Sexpr{nrow(lale_emp_MA[which(lale_emp_MA$Klassen_kenn==klasse),])}) teilgenommen.
                        \\newline
                        \\newline
                        Schwarze Pfeile zeigen an, ob der mittlere Leistungswert Ihrer Klasse bedeutsam \"uber ($\\uparrow$) oder bedeutsam unter ($\\downarrow$) dem Mittelwert der Vergleichsgruppe liegt. 
                        Das Ergebnis Ihrer Klasse unterscheidet sich                  
                        \\newline
                        \\Sexpr{bedeutsam_schule_mathe} vom Mittelwert der Schule.
                        \\newline
                        \\Sexpr{bedeutsam_vergleich_mathe} vom Mittelwert der Vergleichsgruppe. 
                        \\newline
                        \\newline
                        Mittelwertunterschiede ab 30 Punkten gelten als bedeutsam. 
                        \\newline
                        \\newline
                        \\textbf{Abbildung 7: Mathematik - Mittlere Leistungswerte (BISTA-Punkte)}
                        <<Plot5, fig.width=9, fig.height=7, echo=FALSE, background=NA, results=\"asis\", eval=any(is.na(Klasse_Grafiken[which(Klasse_Grafiken$Grafik==\"Plot7\"),\"ADD\"]),Klasse_Grafiken[which(Klasse_Grafiken$Grafik==\"Plot7\"),\"ADD\"])>>=
                        klasse_plot1_5(lale_emp_MA,klasse=klasse,schule=schule,referenzgruppe=referenzgruppe,Datenspalte=\"mathewle_Bista\")
                        box(which = \"outer\")
                        @
                        
                        \\Sexpr{inc(\"Plot7\")}
                        
                        \\newpage
                        
                        \\section{Mathematik: Leistungsverteilungen}
                        Die Leistungsverteilung Ihrer Sch\"uler:innen wird in Abbildung 6 dargestellt. Zum Vergleich sind die Ergebnisse Ihrer Schule und der Vergleichsgruppe (siehe Seite 4) aufgenommen. (Erl\"auterungen zu den Leistungsstufen siehe Seite 3 und 4)
                        \\newline
                        \\newline
                        \\textbf{Abbildung 8: Mathematik - Leistungsverteilungen}
                        <<Plot6, fig.width=9, fig.height=7, echo=FALSE, background=NA, results=\"asis\", eval=any(is.na(Klasse_Grafiken[which(Klasse_Grafiken$Grafik==\"Plot8\"),\"ADD\"]),Klasse_Grafiken[which(Klasse_Grafiken$Grafik==\"Plot8\"),\"ADD\"])>>=
                        klasse_plot2_6(lale_emp_MA,klasse=klasse,schule=schule,referenzgruppe=referenzgruppe,Datenspalte=\"mathewle_Bista\",regelstandard=regelstandard$regelstandard_Mathe, farben=regelstandard$Farben[-1])
                        box(which = \"outer\")
                        @
                        
                        Hinweis: ein Sch\"uler oder eine Sch\"ulerin in Ihrer Klasse entspricht \\Sexpr{round((1/nrow(lale_emp_MA[which(lale_emp_MA$Klassen_kenn==klasse),]))*100)} \\%.
                        \\newline
                        \\newline
                        Abbildung 8 zeigt zum Beispiel, dass \\Sexpr{round(prop.table(table(cut(unlist(lale_emp_MA[which(lale_emp_MA$Klassen_kenn==klasse),\"mathewle_Bista\"]),regelstandard$regelstandard_Mathe)))*100)[[5]]} Prozent der Sch\"uler:innen Ihrer Klasse die Leistungsstufe 500 bis 600 Punkte erreicht hat. In Ihrer Vergleichsgruppe sind im Vergleich dazu \\Sexpr{round(prop.table(table(cut(unlist(lale_emp_MA[grep(referenzgruppe,lale_emp_MA$Vergleichsgruppe),\"mathewle_Bista\"]),regelstandard$regelstandard_Mathe)))*100)[[5]]} Prozent in dieser Leistungsstufe.

                        \\Sexpr{inc(\"Plot8\")}
                        
                        \\newpage
                        
                        
                        \\section{Mathematik: Individuelle Ergebnisse}
                        Die Verteilung der Leistungswerte in Ihrer Klasse wird in Abbildung 9 veranschaulicht. Die einzelnen Werte der Sch\"uler:innen sind mit ihrem Pseudonym aufgelistet und in aufsteigender Reihenfolge nach ihrem Leistungswert angeordnet. Die gestrichelte Linie zeigt Ihnen die rechnerische Grenze zwischen Mindeststandard und Regelstandard auf Basis des Kompetenzstufenmodells Ende der 4. Jahrgangsstufe, n\"ahere Erl\"auterungen siehe Seite 3 und 4.
                        \\newline
                        \\newline
                        \\textbf{Abbildung 9: Mathematik - Individuelle Ergebnisse}
                        <<Plot7, fig.height=8, echo=FALSE, background=NA, results=\"asis\", eval=any(is.na(Klasse_Grafiken[which(Klasse_Grafiken$Grafik==\"Plot9\"),\"ADD\"]),Klasse_Grafiken[which(Klasse_Grafiken$Grafik==\"Plot9\"),\"ADD\"])>>=
                        klasse_plot3_7(lale_emp_MA,klasse=klasse,Datenspalte=\"mathewle_Bista\",regelstandard=regelstandard$regelstandard_Mathe, farben=regelstandard$Farben[-1], grenzwert=460)
                        box(which = \"outer\")
                        @
                        
                        \\Sexpr{inc(\"Plot9\")}
                        
                        \\newpage
                        
                        \\section{Mathematik: Mittlere Leistungsentwicklungen}
                        Abbildung 10 zeigt die mittlere Leistungsentwicklung Ihrer Klasse seit Beginn der f\"unften Jahrgangsstufe. Bei der Ermittlung der durchschnittlichen Leistungsentwicklungen werden jeweils nur die Sch\"uler:innen ber\"ucksichtigt, die an beiden Testungen teilgenommen haben.
                        \\newline
                        \\newline
                        \\textbf{Abbildung 10: Mathematik - Mittlere Leistungsentwicklungen}
                         <<Plot7.1.1, fig.width=9, fig.height=8, echo=FALSE, background=NA, results=\"asis\", eval=any(is.na(Klasse_Grafiken[which(Klasse_Grafiken$Grafik==\"Plot10\"),\"ADD\"]),Klasse_Grafiken[which(Klasse_Grafiken$Grafik==\"Plot10\"),\"ADD\"])>>=
                        klasse_plot_LALE7_1(lale_empirisch=lale_emp_MA,klasse=klasse,schule=schule,referenzgruppe=referenzgruppe,Datenspalte_Start=\"mathewle_Bista_LALE5\",Datenspalte_Ende=\"mathewle_Bista\")
                        box(which = \"outer\")
                        @
                        \\vfill
                        
                        \\Sexpr{inc(\"Plot10\")}
                        
                        \\newpage
                        \\section{Mathematik: Leistungsentwicklungen individuell}
                        Abbildung 11 zeigt Ihnen f\"ur die Sch\"uler:innen Ihrer Klasse, die schon an LALE 5 teilgenommen haben, die Lernentwicklung seit der f\"unften Jahrgangsstufe (Erl\"auterungen siehe Seite 3 und 4). Die LALE 7-Werte sind als Kreise dargestellt und wie in Abbildung 3 in aufsteigender Reihenfolge angeordnet, die LALE 5-Werte sind als Rauten dargestellt.
                        \\newline
                        \\newline
                        \\textbf{Abbildung 11: Mathematik - Leistungsentwicklungen individuell}
                        <<Plot722, fig.width=9, fig.height=8, echo=FALSE, background=NA, results=\"asis\", eval=any(is.na(Klasse_Grafiken[which(Klasse_Grafiken$Grafik==\"Plot11\"),\"ADD\"]),Klasse_Grafiken[which(Klasse_Grafiken$Grafik==\"Plot11\"),\"ADD\"])>>=
                        klasse_plot_LALE7_2(lale_empirisch=lale_emp_MA,klasse=klasse,schule=schule,Datenspalte_Start=\"mathewle_Bista_LALE5\",Datenspalte_Ende=\"mathewle_Bista\")
                        box(which = \"outer\")
                        @
                        \\vfill
                        
                        \\Sexpr{inc(\"Plot11\")}
                        
                        \\newpage
                        \\section{Mathematik: Mittlere Leistungswerte nach Leitideen}
                        Abbildung 12 zeigt die mittleren Leistungswerte der Parallelklassen differenziert nach den inhaltsbezogenen mathematischen Anforderungen (Leitideen). Zum Vergleich sind die Ergebnisse Ihrer Schule und der Vergleichsgruppe (siehe Seite 4) aufgenommen. 
                        \\newline
                        \\newline
                        \\textbf{Abbildung 12: Mathematik - Mittlere Leistungswerte nach Leitideen (BISTA-Punkte)}
                        <<Plot8, fig.width=9, fig.height=8.7, echo=FALSE, background=NA, results=\"asis\", eval=any(is.na(Klasse_Grafiken[which(Klasse_Grafiken$Grafik==\"Plot12\"),\"ADD\"]),Klasse_Grafiken[which(Klasse_Grafiken$Grafik==\"Plot12\"),\"ADD\"])>>=
                        klasse_plot8(lale_emp_MA,klasse,schule,referenzgruppe=referenzgruppe,Datenspalte=mittel_leitidee)
                        @
                        
                        \\Sexpr{inc(\"Plot12\")}
                        
                        \\newpage
                        \\section{Mathematik: L\"osungsh\"aufigkeiten}
                        Die Testaufgaben werden nicht ver\"offentlicht. Um Ihnen dennoch eine inhaltlich interpretierbare Ergebnisdarstellung zu liefern, haben wir in der folgenden \"Ubersicht eine Umschreibung jeder Aufgabe aufgenommen.
                        \\newline
                        \\newline
                        Den Abbildungen 13-17 ist zu entnehmen, wie viel Prozent der Sch\"uler:innen Ihrer Klasse die jeweiligen Aufgaben gel\"ost, nicht gel\"ost oder nicht mehr bearbeitet haben.
                        \\newline
                        \\newline
                        Da die Sch\"uler:innen Testhefte mit unterschiedlicher Zusammenstellung von Aufgaben bearbeitet haben, finden Sie an den Aufgaben verschiedene Angaben f\"ur die Anzahl der Sch\"uler:innen (N). Diesen k\"onnen Sie entnehmen, wie viele Sch\"uler:innen ein Testheft, das diese Aufgabe enthielt, bekommen haben.
                        \\newline
                        \\newline
                        \\textbf{Abbildung 13: Mathematik - L\"osungsh\"aufigkeiten I - Leitidee 1}
                        <<Plot9, fig.width=9, fig.height=8.7, echo=FALSE, background=NA, results=\"asis\", eval=any(is.na(Klasse_Grafiken[which(Klasse_Grafiken$Grafik==\"Plot13\"),\"ADD\"]),Klasse_Grafiken[which(Klasse_Grafiken$Grafik==\"Plot13\"),\"ADD\"])>>=
                        klasse_plot9_10_11_12_13(lale_emp_MA,klasse=klasse,schule=schule,referenzgruppe=referenzgruppe,Datenspalte=leitidee_tabelle[[1]][,1],Leitidee=c(\"L1\"),Aufgabennamen=leitidee_tabelle[[1]][,2])
                        @
                        
                        \\Sexpr{inc(\"Plot13\")}
                        
                        \\newpage
                        \\textbf{Abbildung 14: Mathematik - L\"osungsh\"aufigkeiten II - Leitidee 2}
                        <<Plot10, fig.width=9, fig.height=11.7, echo=FALSE, background=NA, results=\"asis\", eval=any(is.na(Klasse_Grafiken[which(Klasse_Grafiken$Grafik==\"Plot14\"),\"ADD\"]),Klasse_Grafiken[which(Klasse_Grafiken$Grafik==\"Plot14\"),\"ADD\"])>>=
                        klasse_plot9_10_11_12_13(lale_emp_MA,klasse=klasse,schule=schule,referenzgruppe=referenzgruppe,Datenspalte=leitidee_tabelle[[2]][,1],Leitidee=c(\"L2\"),Aufgabennamen=leitidee_tabelle[[2]][,2])
                        @

                        \\Sexpr{inc(\"Plot14\")}
                        \\newpage
                        \\textbf{Abbildung 15: Mathematik - L\"osungsh\"aufigkeiten III - Leitidee 3}
                        <<Plot11, fig.width=9, fig.height=11.7, echo=FALSE, background=NA, results=\"asis\", eval=any(is.na(Klasse_Grafiken[which(Klasse_Grafiken$Grafik==\"Plot15\"),\"ADD\"]),Klasse_Grafiken[which(Klasse_Grafiken$Grafik==\"Plot15\"),\"ADD\"])>>=
                        klasse_plot9_10_11_12_13(lale_emp_MA,klasse=klasse,schule=schule,referenzgruppe=referenzgruppe,Datenspalte=leitidee_tabelle[[3]][,1],Leitidee=c(\"L3\"),Aufgabennamen=leitidee_tabelle[[3]][,2])
                        @

                        \\Sexpr{inc(\"Plot15\")}
                        
                        \\newpage
                        \\textbf{Abbildung 16: Mathematik - L\"osungsh\"aufigkeiten IV - Leitidee 4}
                        <<Plot12, fig.width=9, fig.height=11.7, echo=FALSE, background=NA, results=\"asis\", eval=any(is.na(Klasse_Grafiken[which(Klasse_Grafiken$Grafik==\"Plot16\"),\"ADD\"]),Klasse_Grafiken[which(Klasse_Grafiken$Grafik==\"Plot16\"),\"ADD\"])>>=
                        klasse_plot9_10_11_12_13(lale_emp_MA,klasse=klasse,schule=schule,referenzgruppe=referenzgruppe,Datenspalte=leitidee_tabelle[[4]][,1],Leitidee=c(\"L4\"),Aufgabennamen=leitidee_tabelle[[4]][,2])
                        @
                        
                        \\Sexpr{inc(\"Plot16\")}
                        
                        \\newpage
                        \\textbf{Abbildung 17: Mathematik - L\"osungsh\"aufigkeiten V - Leitidee 5}
                        <<Plot13, fig.width=9, fig.height=11.7, echo=FALSE, background=NA, results=\"asis\", eval=any(is.na(Klasse_Grafiken[which(Klasse_Grafiken$Grafik==\"Plot17\"),\"ADD\"]),Klasse_Grafiken[which(Klasse_Grafiken$Grafik==\"Plot17\"),\"ADD\"])>>=
                        klasse_plot9_10_11_12_13(lale_emp_MA,klasse=klasse,schule=schule,referenzgruppe=referenzgruppe,Datenspalte=leitidee_tabelle[[5]][,1],Leitidee=c(\"L5\"),Aufgabennamen=leitidee_tabelle[[5]][,2])
                        @
                        
                        \\Sexpr{inc(\"Plot17\")}
                        
                        
              \\end{flushleft}          
              \\end{document}",con=filename)
  
  close(filename)
  if (pdf==TRUE) {knit2pdf(paste("LALE-Rueckmeldung_Schule_",schule,"_Klasse_",klasse,".Rnw",sep=""))}
  if (word==TRUE) {system(paste("latex2rtf ","LALE-R\"uckmeldung_Schule_",schule,"_Klasse_",klasse,".tex",sep=""))}
  
  ## Schuelerrueckmeldung einzeln fuer alle Schueler der Schule x
  dir.create("Schueler")
  setwd("Schueler")
  schuelerrueckmeldung_LALE7(lale_empirisch[which(lale_empirisch$Klassen_kenn==klasse),], leitidee_tabelle = leitidee_tabelle, regelstandard = regelstandard, schuljahr = schuljahr, pdf = pdf, logo_dir = logo_dir, word = word, Legenden=Legenden, extschueler=extschueler)
  if (pdf==TRUE) {pdf_combine(list.files(pattern = "^Schule(.*)pdf$", recursive = TRUE), paste("Schuelerrueckmeldung_Schule_",schule,"_Klasse_",unlist(unique(lale_empirisch[which(lale_empirisch$Klassen_kenn==klasse),"KLASSE"])),".pdf",sep=""))}
  
  setwd("..")
  
  setwd("..")
}

###########Schuelerrueckmeldung##########

schuelerrueckmeldung_LALE7 <- function(lale_empirisch, leitidee_tabelle = leitidee_tabelle, regelstandard, schuljahr="2018/19", pdf=FALSE, logo_dir = "/home/philipp/Desktop/LALE 5 -RCode/Code/Files", word, Legenden=Legenden, extschueler)
  
{
  
  ## Plot 1 Sch\"ulerr\"uckmeldung: Skalenplot Deutsch Leseverstehen
  schueler_plot1 <- function(schueler_pseudonym, regelstandard, farben)
  {
    par(mar = c(2, 0.5, 2, 0.5))
    plot(data.frame(c(200,700)), xlim=c(100,800), main="Deutsch", pch=NA)
    abline(v = seq(100,800,100), col = "darkgray", lty = 1)
    rect(xleft = c(100,regelstandard[c(2:7)]), xright = regelstandard[c(-1)], ybottom = rep(0.8,7), ytop =rep(1.2,7), col = c(farben[2],farben[-1]))
    if (!is.na(lale_empirisch[which(lale_empirisch$Pseudonym==schueler_pseudonym),"delwlemeanbista"]))
    {if (lale_empirisch[which(lale_empirisch$Pseudonym==schueler_pseudonym),"delwlemeanbista"]>800)
    {points(800, 1, pch=23, col="black", bg="yellow", cex=1.5, xpd=TRUE)}
      else {points(lale_empirisch[which(lale_empirisch$Pseudonym==schueler_pseudonym),"delwlemeanbista"], 1, pch=23, col="black", bg="yellow", cex=1.5)}
    }
  }
  
  ## Plot 2 Sch\"ulerr\"uckmeldung: Skalenplot Mathmatik
  schueler_plot2 <- function(schueler_pseudonym, regelstandard, farben)
  {
    par(mar = c(2, 0.5, 2, 0.5))
    plot(data.frame(c(200,700)), xlim=c(100,800), main="Mathematik", pch=NA)
    abline(v = seq(100,800,100), col = "darkgray", lty = 1)
    rect(xleft = c(100,regelstandard[c(2:7)]), xright = regelstandard[c(-1)], ybottom = rep(0.8,7), ytop =rep(1.2,7), col = c(farben[2],farben[-1]))
    if (!is.na(lale_empirisch[which(lale_empirisch$Pseudonym==schueler_pseudonym),"mathewle_Bista"]))
    {if (lale_empirisch[which(lale_empirisch$Pseudonym==schueler_pseudonym),"mathewle_Bista"]>800) 
    {points(800, 1, pch=23, col="black", bg="yellow", cex=1.5, xpd=TRUE)}
      else{points(lale_empirisch[which(lale_empirisch$Pseudonym==schueler_pseudonym),"mathewle_Bista"], 1, pch=23, col="black", bg="yellow", cex=1.5)}
    }
  }
  
  ## Plot 3 Sch\"ulerr\"uckmeldung: Stacked Barplot Leseverstehen
  schueler_plot3 <- function(schueler_pseudonym)
  {
    par(mar = c(2, 3.5, 0.5, 0), xaxs = "i", xpd=TRUE) # par(mar = c(bottom, left, top, right))
    data <- 100*matrix(c(lale_empirisch[[which(lale_empirisch$Pseudonym==schueler_pseudonym),"Bionik_Richtig_Deutsch"]],
                         lale_empirisch[[which(lale_empirisch$Pseudonym==schueler_pseudonym),"Bionik_Falsch_Deutsch"]], 
                         lale_empirisch[[which(lale_empirisch$Pseudonym==schueler_pseudonym),"Bionik_99_Deutsch"]], 
                         lale_empirisch[[which(lale_empirisch$Pseudonym==schueler_pseudonym),"Die Meisterdiebe_Richtig_Deutsch"]],
                         lale_empirisch[[which(lale_empirisch$Pseudonym==schueler_pseudonym),"Die Meisterdiebe_Falsch_Deutsch"]], 
                         lale_empirisch[[which(lale_empirisch$Pseudonym==schueler_pseudonym),"Die Meisterdiebe_99_Deutsch"]]),
                       ncol = 2, dimnames = list(seq(1:3),c("Sachtext: Bionik","Sachtext: Die Meisterdiebe")))
    
    # Setzen auf NA wenn der Schueler komplett NA ist 
    if (is.na(lale_empirisch[which(lale_empirisch$Pseudonym==schueler_pseudonym),"delwlemeanbista"])==TRUE){data <-matrix(rep(0,6), ncol = 2, dimnames = list(seq(1:3),c("Sachtext: Bionik","Sachtext: Die Meisterdiebe")))}
    
    if (lale_empirisch[[which(lale_empirisch$Pseudonym==schueler_pseudonym),"HEFT"]]=="E")
    {
      data <- 100*matrix(c(lale_empirisch[[which(lale_empirisch$Pseudonym==schueler_pseudonym),"Schulkinotage_Richtig_Deutsch"]],
                           lale_empirisch[[which(lale_empirisch$Pseudonym==schueler_pseudonym),"Schulkinotage_Falsch_Deutsch"]], 
                           lale_empirisch[[which(lale_empirisch$Pseudonym==schueler_pseudonym),"Schulkinotage_99_Deutsch"]],
                           lale_empirisch[[which(lale_empirisch$Pseudonym==schueler_pseudonym),"Die Meisterdiebe_Richtig_Deutsch"]],
                           lale_empirisch[[which(lale_empirisch$Pseudonym==schueler_pseudonym),"Die Meisterdiebe_Falsch_Deutsch"]], 
                           lale_empirisch[[which(lale_empirisch$Pseudonym==schueler_pseudonym),"Die Meisterdiebe_99_Deutsch"]]),
                         ncol = 2, dimnames = list(seq(1:3),c("Sachtext: Schulkinotage","Sachtext: Die Meisterdiebe")))
      # Setzen auf NA wenn der Schueler komplett NA ist  
      if (is.na(lale_empirisch[which(lale_empirisch$Pseudonym==schueler_pseudonym),"delwlemeanbista"])==TRUE){data <-matrix(rep(0,6), ncol = 2, dimnames = list(seq(1:3),c("Sachtext: Schulkinotage","Sachtext: Die Meisterdiebe")))}
      
    }
    m <- barplot(data, plot=T, col = NA, border = NA, axes = FALSE, main = "", space=1, width=1.3, xlim=c(0,7), cex.axis = .8, ylim=c(0,100))
    abline(h = c(0,10,20,30,40,50,60,70,80,90,100), col = "darkgray", lty = 1, xpd=F)
    barplot(data, axes = FALSE, add = TRUE, col= c("#10253f","#375a84","#8eb4e3"), space=1, width=1.3, axisnames=FALSE)
    axis(side = 2, tick = TRUE, at = c(0,10,20,30,40,50,60,70,80,90,100), labels = paste(c(0,10,20,30,40,50,60,70,80,90,100), rep("%",11)), lty = 1, las = 1)
    perc <- paste(round(data),"%")
    perc[which(perc=="0 %")]<- ""
    text(x = m, y = t(apply(data,2,cumsum)-data/2), labels = t(matrix(perc,ncol=ncol(data))), xpd = TRUE, col="white")
    legend(x=5.5, y=100, y.intersp=1.5,legend=c(Legenden[2,4],Legenden[2,3],Legenden[2,2]), fill =  c("#8eb4e3","#375a84","#10253f"), bty="n", ncol = 1, cex=.8)
    
  }
  
  ## Plot 4 Sch\"ulerr\"uckmeldung: Stacked Barplot Mathematik
  schueler_plot4 <- function(schueler_pseudonym)
  {
    par(mar = c(2, 3.5, 0.5, 0), xaxs = "i", xpd=TRUE) # par(mar = c(bottom, left, top, right))
    data <- 100*matrix(c(lale_empirisch[[which(lale_empirisch$Pseudonym==schueler_pseudonym),"ML1_Richtig_Mathe"]],
                         lale_empirisch[[which(lale_empirisch$Pseudonym==schueler_pseudonym),"ML1_Falsch_Mathe"]],
                         lale_empirisch[[which(lale_empirisch$Pseudonym==schueler_pseudonym),"ML1_99_Mathe"]],
                         lale_empirisch[[which(lale_empirisch$Pseudonym==schueler_pseudonym),"ML2_Richtig_Mathe"]],
                         lale_empirisch[[which(lale_empirisch$Pseudonym==schueler_pseudonym),"ML2_Falsch_Mathe"]],
                         lale_empirisch[[which(lale_empirisch$Pseudonym==schueler_pseudonym),"ML2_99_Mathe"]],
                         lale_empirisch[[which(lale_empirisch$Pseudonym==schueler_pseudonym),"ML3_Richtig_Mathe"]],
                         lale_empirisch[[which(lale_empirisch$Pseudonym==schueler_pseudonym),"ML3_Falsch_Mathe"]],
                         lale_empirisch[[which(lale_empirisch$Pseudonym==schueler_pseudonym),"ML3_99_Mathe"]],
                         lale_empirisch[[which(lale_empirisch$Pseudonym==schueler_pseudonym),"ML4_Richtig_Mathe"]],
                         lale_empirisch[[which(lale_empirisch$Pseudonym==schueler_pseudonym),"ML4_Falsch_Mathe"]],
                         lale_empirisch[[which(lale_empirisch$Pseudonym==schueler_pseudonym),"ML4_99_Mathe"]],
                         lale_empirisch[[which(lale_empirisch$Pseudonym==schueler_pseudonym),"ML5_Richtig_Mathe"]],
                         lale_empirisch[[which(lale_empirisch$Pseudonym==schueler_pseudonym),"ML5_Falsch_Mathe"]],
                         lale_empirisch[[which(lale_empirisch$Pseudonym==schueler_pseudonym),"ML5_99_Mathe"]]),
                       ncol=5, dimnames = list(seq(1:3),c("Zahl\n ","Messen\n ","Funktionaler\nZusammenhang","Raum\nund Form","Daten\nund Zufall"))) #2024 laut KMK geaendert
    
    # Setzen auf NA wenn der Schueler komplett NA ist
    if (is.na(lale_empirisch[which(lale_empirisch$Pseudonym==schueler_pseudonym),"mathewle_Bista"])==TRUE){data <-matrix(rep(0,15), ncol=5, dimnames = list(seq(1:3),c("Zahl\n ","Messen\n ","Funktionaler\nZusammenhang","Raum\nund Form","Daten\nund Zufall")))} #2024 laut KMK geaendert
    
    m <- barplot(data, plot=T, col = NA, border = NA, axes = FALSE, main = "", space=0.5, width=1, xlim=c(0,8), cex.axis = .8, ylim=c(0,100))
    abline(h = c(0,10,20,30,40,50,60,70,80,90,100), col = "darkgray", lty = 1, xpd=F)
    barplot(data, axes = FALSE, add = TRUE, col= c("#10253f","#375a84","#8eb4e3"), space=0.5, width=1, axisnames=FALSE)
    axis(side = 2, tick = TRUE, at = c(0,10,20,30,40,50,60,70,80,90,100), labels = paste(c(0,10,20,30,40,50,60,70,80,90,100), rep("%",11)), lty = 1, las = 1)
    perc <- paste(round(data),"%")
    perc[which(perc=="0 %")]<- ""
    text(x = m, y = t(apply(data,2,cumsum)-data/2), labels = t(matrix(perc,ncol=ncol(data))), xpd = TRUE, col="white")
  }
  
  Mathe2 <- function(schueler_pseudonym){
    if (is.na(round(lale_empirisch[which(lale_empirisch$Pseudonym==schueler_pseudonym),"Mathe"]*100))==FALSE){Mathe2 <- paste("Du hast im Bereich Mathematik", round(lale_empirisch[which(lale_empirisch$Pseudonym==schueler_pseudonym),"Mathe"]*100), "Prozent der Aufgaben richtig gel\"ost.")}
    if (is.na(round(lale_empirisch[which(lale_empirisch$Pseudonym==schueler_pseudonym),"Mathe"]*100))==TRUE){Mathe2 <-"F\"ur den Bereich Mathematik sind f\"ur dich keine Werte vorhanden."}
    return(Mathe2)}
  
  Mathe1 <- function(schueler_pseudonym){                    
    if (is.na(lale_empirisch[which(lale_empirisch$Pseudonym==schueler_pseudonym),"mathewle_Bista"])==FALSE){Mathe1 <- paste("Im Bereich Mathematik hast du",round(lale_empirisch[which(lale_empirisch$Pseudonym==schueler_pseudonym),"mathewle_Bista"]), "Punkte erreicht.")}
    if (is.na(lale_empirisch[which(lale_empirisch$Pseudonym==schueler_pseudonym),"mathewle_Bista"])==TRUE){Mathe1 <-"F\"ur den Bereich Mathematik sind f\"ur dich keine Werte vorhanden."}
    return(Mathe1)}
  
  Deutsch1 <- function(schueler_pseudonym){
    if (is.na(lale_empirisch[which(lale_empirisch$Pseudonym==schueler_pseudonym),"delwlemeanbista"])==FALSE){Deutsch1 <- paste("Im Bereich Deutsch-Leseverstehen hast du", round(lale_empirisch[which(lale_empirisch$Pseudonym==schueler_pseudonym),"delwlemeanbista"]),"Punkte erreicht.")}
    if (is.na(lale_empirisch[which(lale_empirisch$Pseudonym==schueler_pseudonym),"delwlemeanbista"])==TRUE){Deutsch1 <- "F\"ur den Bereich Deutsch-Leseverstehen sind f\"ur dich keine Werte vorhanden."}
    return(Deutsch1)}
  
  Deutsch2 <- function(schueler_pseudonym){
    if (is.na(round(lale_empirisch[which(lale_empirisch$Pseudonym==schueler_pseudonym),"Deutsch"]*100))==FALSE){Deutsch2 <- paste("Du hast im Bereich Deutsch-Leseverstehen", round(lale_empirisch[which(lale_empirisch$Pseudonym==schueler_pseudonym),"Deutsch"]*100), "Prozent der Aufgaben richtig gel\"ost.")}
    if (is.na(round(lale_empirisch[which(lale_empirisch$Pseudonym==schueler_pseudonym),"Deutsch"]*100))==TRUE){Deutsch2 <-"F\"ur den Bereich Deutsch-Leseverstehen sind f\"ur dich keine Werte vorhanden."}
    return(Deutsch2)}
  
###Andreas Code####
  ## Plot 5 Sch\"ulerr\"uckmeldung: Skalenplot Deutsch fachliches Selbstkonzept 
  farben_blau <- c("#B0E2FF", "#B0E2FF", "#87CEFA", "#1874CD")
  
  schueler_plot5 <- function(schueler_pseudonym, farben_blau)
  {
    par(mar = c(2, 0.5, 2, 0.5))#setzt die R?nder (unten, links, oben, rechts)
    plot(x =data.frame( c(2,3)), xlim=c(0,4), ylim=c(0.5,1.5),axes=FALSE, main="Fachliches Selbstkonzept Deutsch", pch="*", col="orange", cex=2)
    rect(xleft = c(0,1,2, 3), xright = c(1,2,3,4), ybottom = rep(0.8,5), ytop =rep(1.2,5), col = farben_blau)
    
    
    if (!is.na(lale_empirisch[which(lale_empirisch$Pseudonym==schueler_pseudonym),"averageSSKD"]))
    {
      if (lale_empirisch[which(lale_empirisch$Pseudonym==schueler_pseudonym),"averageSSKD"]<=4)
      {
        if (lale_empirisch[which(lale_empirisch$Pseudonym==schueler_pseudonym),"LO0015"]==1)
        { points(lale_empirisch[which(lale_empirisch$Pseudonym==schueler_pseudonym),"averageSSKD"], 1, pch=23, col="black", bg="yellow", cex=2)
          text(x=1, y=0.6, "niedrig")
          text(x=2.5,y=0.6, "mittel")
          text(x=3.5, y=0.6, "hoch") 
          
        }  
        else if (lale_empirisch[which(lale_empirisch$Pseudonym==schueler_pseudonym),"LO0015"]!=1)
        {
          text(x=0.5, y=0.6, "Keine Einsicht")
        }
      }
      else
      {text(x=0.5, y=0.6, "Keine Daten")
      }  
      
    }  
  }  
  
  
  ## Plot 6 Sch\"ulerr\"uckmeldung: Skalenplot Mathe fachliches Selbstkonzept 
  
  
  
  
  schueler_plot6 <- function(schueler_pseudonym, farben_blau)
  {
    par(mar = c(2, 0.5, 2, 0.5))#setzt die R?nder (unten, links, oben, rechts)
    plot(x =data.frame( c(2,3)), xlim=c(0,4), ylim=c(0.5,1.5),axes=FALSE, main="Fachliches Selbstkonzept Mathematik", pch="*", col="orange", cex=2)
    rect(xleft = c(0,1,2, 3), xright = c(1,2,3,4), ybottom = rep(0.8,5), ytop =rep(1.2,5), col = farben_blau) 
    
    if (!is.na(lale_empirisch[which(lale_empirisch$Pseudonym==schueler_pseudonym),"averageSSKM"]))
    {
      if (lale_empirisch[which(lale_empirisch$Pseudonym==schueler_pseudonym),"averageSSKM"]<=4)
      {
        if (lale_empirisch[which(lale_empirisch$Pseudonym==schueler_pseudonym),"LO0015"]==1)
        { points(lale_empirisch[which(lale_empirisch$Pseudonym==schueler_pseudonym),"averageSSKM"], 1, pch=23, col="black", bg="yellow", cex=2)
          text(x=1, y=0.6, "niedrig")
          text(x=2.5,y=0.6, "mittel")
          text(x=3.5, y=0.6, "hoch") 
          
        }  
        else if (lale_empirisch[which(lale_empirisch$Pseudonym==schueler_pseudonym),"LO0015"]!=1)
        {
          text(x=0.5, y=0.6, "Keine Einsicht")
        }
      }
      else
      {text(x=0.5, y=0.6, "Keine Daten")
      }  
      
    }  
    
  }          
  
  
  
  
  ## Plot 7 Sch\"ulerr\"uckmeldung: Skalenplot Interesse Deutsch
  
  schueler_plot7 <- function(schueler_pseudonym, farben_blau)
  {
    par(mar = c(2, 0.5, 2, 0.5))#setzt die R?nder (unten, links, oben, rechts)
    plot(x =data.frame( c(2,3)), xlim=c(0,4), ylim=c(0.5,1.5),axes=FALSE, main="Interesse am Fach Deutsch", pch="*", col="orange", cex=2)
    rect(xleft = c(0,1,2, 3), xright = c(1,2,3,4), ybottom = rep(0.8,5), ytop =rep(1.2,5), col = farben_blau)
    
    
    if (!is.na(lale_empirisch[which(lale_empirisch$Pseudonym==schueler_pseudonym),"averageMotD"]))
    {
      if (lale_empirisch[which(lale_empirisch$Pseudonym==schueler_pseudonym),"averageMotD"]<=4)
      {
        if (lale_empirisch[which(lale_empirisch$Pseudonym==schueler_pseudonym),"LO0015"]==1)
        { points(lale_empirisch[which(lale_empirisch$Pseudonym==schueler_pseudonym),"averageMotD"], 1, pch=23, col="black", bg="yellow", cex=2)
          text(x=1, y=0.6, "niedrig")
          text(x=2.5,y=0.6, "mittel")
          text(x=3.5, y=0.6, "hoch") 
          
        }  
        else if (lale_empirisch[which(lale_empirisch$Pseudonym==schueler_pseudonym),"LO0015"]!=1)
        {
          text(x=0.5, y=0.6, "Keine Einsicht")
        }
      }
      else
      {text(x=0.5, y=0.6, "Keine Daten")
      }  
      
    }  
  }  
  
  
  ## Plot 8 Sch\"ulerr\"uckmeldung: Skalenplot Interesse Mathematik
  
  schueler_plot8 <- function(schueler_pseudonym, farben_blau)
  {
    par(mar = c(2, 0.5, 2, 0.5))#setzt die R?nder (unten, links, oben, rechts)
    plot(x =data.frame( c(2,3)), xlim=c(0,4), ylim=c(0.5,1.5),axes=FALSE, main="Interesse am Fach Mathematik", pch="*", col="orange", cex=2)
    rect(xleft = c(0,1,2, 3), xright = c(1,2,3,4), ybottom = rep(0.8,5), ytop =rep(1.2,5), col = farben_blau)
    
    
    if (!is.na(lale_empirisch[which(lale_empirisch$Pseudonym==schueler_pseudonym),"averageMotM"]))
    {
      if (lale_empirisch[which(lale_empirisch$Pseudonym==schueler_pseudonym),"averageMotM"]<=4)
      {
        if (lale_empirisch[which(lale_empirisch$Pseudonym==schueler_pseudonym),"LO0015"]==1)
        { points(lale_empirisch[which(lale_empirisch$Pseudonym==schueler_pseudonym),"averageMotM"], 1, pch=23, col="black", bg="yellow", cex=2)
          text(x=1, y=0.6, "niedrig")
          text(x=2.5,y=0.6, "mittel")
          text(x=3.5, y=0.6, "hoch") 
          
        }  
        else if (lale_empirisch[which(lale_empirisch$Pseudonym==schueler_pseudonym),"LO0015"]!=1)
        {
          text(x=0.5, y=0.6, "Keine Einsicht")
        }
      }
      else
      {text(x=0.5, y=0.6, "Keine Daten")
      }  
      
    }  
  }  
  
  
  
  
  
  ## Erstelle .Rnw File fuer knit2pdf Konvertierung
  
  inc <- function(plot){ if (plot%in%extschueler$Grafik) 
  { 
    return(paste0("\\begin{figure} 
                     \\textbf{",extschueler[which(extschueler$Grafik==plot),"Legende"],"}
                     \\newline 
                     \\includegraphics[width=",extschueler[which(extschueler$Grafik==plot),"Breit"],"cm",
                  ",height=",extschueler[which(extschueler$Grafik==plot),"Hoch"],"cm",
                  "]{",logo_dir,"/LALE/data/",extschueler[which(extschueler$Grafik==plot),"Dateiname"],"} 
                     \\end{figure}"
    )
    )
  }}
  
  schueler <- function(schueler_pseudonym, schuljahr, word=F, pdf=T)
  {
    
    ## Nur plots fuer Pseudonym setzen
    extschueler <- extschueler[which(extschueler$Pseudonym==schueler_pseudonym),]
    
    ## Setze und wechsele in das Verzeichnis der Arbeitsumgebung f\"ur die Auswertung; Alle Auswertungen an einem Tag nutzen das selbe Verzeichnis und \"uberschreiben alte Auswertungen dieses Datums
    dir.create(schueler_pseudonym)
    setwd(schueler_pseudonym)
    
    filename <- file(paste("Schule_",unique(lale_empirisch$SCHULNUMMER),"_Klasse_",unique(lale_empirisch$KLASSE),"_Pseudonym_",schueler_pseudonym,".Rnw",sep=""))
    
    ## Latex Document
    
    writeChar("\\documentclass[a4paper, 11pt]{article}
                        \\usepackage[includeheadfoot, top=0.5in, bottom=0.5in, left=1in, right=1in, headsep=.2in]{geometry}
                        \\setlength{\\headheight}{30pt}
                        \\usepackage{fancyhdr}
                        \\usepackage[utf8]{inputenc}
                        \\usepackage{graphicx}
                        \\pagestyle{fancy}
                        \\pagenumbering{gobble}
                        \\usepackage[ngerman]{babel}
                        \\usepackage{helvet}
                        \\usepackage[compact]{titlesec}
                        \\usepackage{multirow}
                        \\usepackage[table]{xcolor}
                        
                        \\renewcommand{\\familydefault}{\\sfdefault}
                        \\renewcommand*\\footnoterule{}
                        \\newcommand\\nomarkfoot[1]{%
                        \\begingroup
                        \\renewcommand\\thefootnote{}\\footnote{#1}%
                        \\addtocounter{footnote}{-1}%
                        \\endgroup
                        }
                        \\usepackage{grffile}
                        
                            \\makeatletter
                            \\newcommand*{\\bigs}[1]{{\\hbox{$\\left#1\\vbox to24\\p@{}\\right.\\n@space$}}}
                            \\makeatother
	
	
                        \\chead{Sch\\\"ulerr\\\"uckmeldung\\\\}
                        \\lhead{
                                Schule  \\Sexpr{lale_empirisch[which(lale_empirisch$Pseudonym==schueler_pseudonym),\"SCHULNUMMER\"]} \\newline
                                Klasse \\Sexpr{lale_empirisch[which(lale_empirisch$Pseudonym==schueler_pseudonym),\"KLASSE\"]}}
                        \\rhead{\\includegraphics[width=70px,height=25px]{\\Sexpr{paste0(logo_dir,\"/LALE7.jpg\")}}}


                        \\begin{document}
                        \\begin{flushright} Pseudonym: \\Sexpr{schueler_pseudonym} \\end{flushright}
                        Du hast Anfang des Schuljahres \\Sexpr{schuljahr} an dem LALE-Test f\"ur Sch\"uler:innen der siebten Klassen teilgenommen.
                        \\newline
                        \\newline
                        Hier ist dein Ergebnis:
                        \\begin{flushleft}
                        
                        \\Sexpr{Deutsch1(schueler_pseudonym)}
                        \\vspace{0.1cm}
                        <<Plot1, fig.width=7, fig.height=1.5, echo=FALSE, background=NA, results=\"asis\", eval=any(is.na(extschueler[which(extschueler$Grafik==\"Plot1\"),\"ADD\"]),extschueler[which(extschueler$Grafik==\"Plot1\"),\"ADD\"])>>=
                        schueler_plot1(schueler_pseudonym, regelstandard=regelstandard$regelstandard_Lesen, farben=regelstandard$Farben)
                        box(which = \"outer\")
                        @

                        \\Sexpr{Mathe1(schueler_pseudonym)}
                        \\vspace{0.1cm}
                        
                        \\Sexpr{inc(\"Plot1\")}
                        
                        <<Plot2, fig.width=7, fig.height=1.5, echo=FALSE, background=NA, results=\"asis\", eval=any(is.na(extschueler[which(extschueler$Grafik==\"Plot2\"),\"ADD\"]),extschueler[which(extschueler$Grafik==\"Plot2\"),\"ADD\"])>>=
                        schueler_plot2(schueler_pseudonym, regelstandard=regelstandard$regelstandard_Mathe, farben=regelstandard$Farben)
                        box(which = \"outer\")
                        @

                        \\nomarkfoot{Schuljahr \\Sexpr{schuljahr}}
                        \\centering \\textbf{\\large Leseverstehen: Anteil richtig gel\"oster Aufgaben nach Textart}
                        \\vspace{0.03cm}
                        
                        \\Sexpr{inc(\"Plot2\")}
                        
                        <<Plot3, fig.height=1.8, echo=FALSE, background=NA, results=\"asis\", eval=any(is.na(extschueler[which(extschueler$Grafik==\"Plot3\"),\"ADD\"]),extschueler[which(extschueler$Grafik==\"Plot3\"),\"ADD\"])>>=
                        schueler_plot3(schueler_pseudonym)
                        @
                        \\vspace{0cm}
                        \\begin{flushleft} \\Sexpr{Deutsch2(schueler_pseudonym)} \\end{flushleft} 
                        \\vspace{0.05cm}
                        \\centering \\textbf {\\large Mathematik: Anteil richtig gel\"oster Aufgaben nach Leitidee}
                        \\vspace{0.03cm}
                        
                        \\Sexpr{inc(\"Plot3\")}
                        
                         <<Plot4, fig.height=1.8, echo=FALSE, background=NA, results=\"asis\", eval=any(is.na(extschueler[which(extschueler$Grafik==\"Plot4\"),\"ADD\"]),extschueler[which(extschueler$Grafik==\"Plot4\"),\"ADD\"])>>=
                        schueler_plot4(schueler_pseudonym)
                        @
                        \\vspace{0cm}
                        \\begin{flushleft} \\Sexpr{Mathe2(schueler_pseudonym)} \\end{flushleft} 
                        \\end{flushleft}  
                        \\Sexpr{inc(\"Plot4\")}
                        
                        \\newpage
                        
                        \\begin{flushright} Pseudonym: \\Sexpr{schueler_pseudonym} \\end{flushright} 
                        Du hast Anfang des Schuljahres \\Sexpr{schuljahr} im Rahmen des LALE-Tests auch Fragen zu deinem\\newline
                        fachlichen Selbstkonzept und zu deiner Motivation beantwortet.\\newline
                        \\vspace{0.5cm}
                        Hier ist dein Ergebnis:
                        
                        
                        <<Plot5, fig.width=7, fig.height=1.5, echo=FALSE, background=NA, results=\"asis\", eval=any(is.na(extschueler[which(extschueler$Grafik==\"Plot5\"),\"ADD\"]),extschueler[which(extschueler$Grafik==\"Plot5\"),\"ADD\"])>>=
                        schueler_plot5(schueler_pseudonym, farben=farben_blau)
                        box(which = \"outer\")
                        @
                      
                        
                        \\Sexpr{inc(\"Plot5\")}
                        \\vspace{1cm}
                        
                        <<Plot6, fig.width=7, fig.height=1.5, echo=FALSE, background=NA, results=\"asis\", eval=any(is.na(extschueler[which(extschueler$Grafik==\"Plot6\"),\"ADD\"]),extschueler[which(extschueler$Grafik==\"Plot6\"),\"ADD\"])>>=
                        schueler_plot6(schueler_pseudonym, farben=farben_blau)
                        box(which = \"outer\")
                        @
                      
                        
                        \\Sexpr{inc(\"Plot6\")}
                        \\vspace{1cm}
                        
                        <<Plot7, fig.width=7, fig.height=1.5, echo=FALSE, background=NA, results=\"asis\", eval=any(is.na(extschueler[which(extschueler$Grafik==\"Plot7\"),\"ADD\"]),extschueler[which(extschueler$Grafik==\"Plot7\"),\"ADD\"])>>=
                        schueler_plot7(schueler_pseudonym, farben=farben_blau)
                        box(which = \"outer\")
                        @
                      
                        
                        \\Sexpr{inc(\"Plot7\")}
                        \\vspace{1cm}
              
                        <<Plot8, fig.width=7, fig.height=1.5, echo=FALSE, background=NA, results=\"asis\", eval=any(is.na(extschueler[which(extschueler$Grafik==\"Plot8\"),\"ADD\"]),extschueler[which(extschueler$Grafik==\"Plot8\"),\"ADD\"])>>=
                        schueler_plot8(schueler_pseudonym, farben=farben_blau)
                        box(which = \"outer\")
                        @
                      
                        
                        \\Sexpr{inc(\"Plot8\")}
                      
                      \\textbf{Erl\"auterungen}:\\newline
                      \\underline{Keine Einsicht}: Der/die Sch\"uler:in hat keine Einsicht in die Daten gew\"ahrt. Die Werte gehen in die Klassenwerte ein.\\newline
                      \\underline{Keine Daten}: F\"ur diese/n Sch\"uler:in liegen keine oder unvollst\"andige Daten vor.
                              \\end{document}",con=filename)
    
    close(filename)
    if (pdf==TRUE) {knit2pdf(paste("Schule_",unique(lale_empirisch$SCHULNUMMER),"_Klasse_",unique(lale_empirisch$KLASSE),"_Pseudonym_",schueler_pseudonym,".Rnw",sep=""))}
    if (word==TRUE) {system(paste("latex2rtf ","Schule_",unique(lale_empirisch$SCHULNUMMER),"_Klasse_",unique(lale_empirisch$KLASSE),"_Pseudonym_",schueler_pseudonym,".Rnw",sep=""))}
    setwd("..")
  }
  
  apply(lale_empirisch[,"Pseudonym"],1,schueler,schuljahr=schuljahr)
  
}



########## LALE Rueckmeldung ##########

#falls man im Skript testen will (function auskommentieren und in der Klammer weitermachen), ansonsten auskommentiert lassen
#lale_empirisch = x[[1]]; Auswertung = "LALE5"; schuljahr="2021/22"; Mathematik_Leitideen = x[[2]]; Deutsch_Leitideen = x[[3]]; logo_dir = "E:/Dezember2021/LALE_5_2021/logo"; Fuenf_Prozent = "ALLE"; word=FALSE; pdf=TRUE; Legenden="E:/Dezember2021/LALE_5_2021/Legenden.xlsx";Schulnummer="176"; Externe_Grafiken=NA

LALE_rueckmeldung <- function(lale_empirisch, Auswertung=c("LALE5","LALE7","LALE9"), schuljahr="2018/19", regelstandard=NA, Mathematik_Leitideen, Deutsch_Leitideen, logo_dir = "/home/philipp/Desktop/LALE 5 -RCode/Code/Files", Fuenf_Prozent="ALLE", word=FALSE, pdf=FALSE, Legenden = "Legenden.xlsx", Schulnummer=NA, Klassennummer=NA, Externe_Grafiken=NA)
{  
  ## Lade Legenden ueber externe Datei
  Legenden <- read_excel(Legenden)
  Legenden <- sapply(Legenden, function(x) gsub(pattern = "newline",replacement = "\n",x = x))
  
  Schule_Grafiken <- tibble(Grafik=character(),Dateiname=character(),Hoch=numeric(),Breit=numeric(),Schule=numeric(),Legende=character(),ADD=character())
  Klasse_Grafiken <- tibble(Grafik=character(),Dateiname=character(),Hoch=numeric(),Breit=numeric(),Schule=numeric(),Klasse=numeric(),Legende=character(),ADD=character())
  Schueler_Grafiken <- tibble(Grafik=character(),Dateiname=character(),Hoch=numeric(),Breit=numeric(),Pseudonym=character(),Legende=character(),ADD=character())
  
  ## Lade Extraplots ueber externe Datei
  if (is.na(Externe_Grafiken)==FALSE)
  {
    warning(paste0("Die Datei", Externe_Grafiken, " muss mindestens die Tabellenblaetter Schulrueckmeldung, Klassenreuckmeldung und Schuelerrueckmeldung enthalten. Dies gilt auch fuer die jeweiligen Teile der Rueckmeldung, wenn keine Grafiken eingefuegt werden sollen."))
    Schule_Grafiken <- read_excel(Externe_Grafiken, sheet="Schulrueckmeldung")
    Klasse_Grafiken <- read_excel(Externe_Grafiken, sheet="Klassenrueckmeldung")
    Schueler_Grafiken <- read_excel(Externe_Grafiken, sheet="Schuelerrueckmeldung")
    
    ## Stoppt den Prozess falls die Columns geaendert wurden
    s<-c("Grafik","Dateiname","Hoch","Breit","Schule","Legende","ADD")
    k<-c("Grafik","Dateiname","Hoch","Breit","Schule","Klasse","Legende","ADD")
    sch<-c("Grafik","Dateiname","Hoch","Breit","Pseudonym","Legende","ADD")
    if(any(colnames(Schule_Grafiken)!=s)|any(colnames(Klasse_Grafiken)!=k)|any(colnames(Schueler_Grafiken)!=sch)){stop("Es wurden Daten mittels Externe_Grafiken.xlsx angegeben, aber das Dateiformat enthaellt Fehler. Die Spalten muessen in jedem Tabellenblatt diese Formatierung haben: Grafik,Dateiname,Hoch,Breit,Schule/Klasse/Pseudonym,Legende,ADD")}
  }
  
  ## Aufruf einzelner Klassen und Schulen in der LALE Auswertung
  if (is.na(Klassennummer)==FALSE & is.na(Schulnummer)==TRUE) {Schulnummer<-substr(Klassennummer,1,3)}
  
  ### Starte Analyse
  ## Setze und wechsele in das Verzeichnis der Arbeitsumgebung fuer die Auswertung; Alle Auswertungen an einem Tag nutzen das selbe Verzeichnis und ueberschreiben alte Auswertungen dieses Datums
  dir.name <- paste(Auswertung,"_",substr(schuljahr,1,4),"_Rueckmeldung",substr(Sys.time(),1,10),sep="")
  dir.create(dir.name)
  setwd(dir.name)
  
  
  ## Setze die Fuenf_Prozent Grenzen fuer die Tabellen der Klassenrueckmeldung
  if (Fuenf_Prozent == "ALLE") {Fuenf_Prozent <- list(
    Mathe = c(quantile(lale_empirisch$mathewle_Bista,na.rm=T,0.05), quantile(lale_empirisch$mathewle_Bista,na.rm=T,0.95)),
    Lesen = c(quantile(lale_empirisch$delwlemeanbista,na.rm=T,0.05), quantile(lale_empirisch$delwlemeanbista,na.rm=T,0.95))) }
  
  ##  Falls die Daten nicht tibble sind, nrow und length Probleme
  if(is_tibble(lale_empirisch)==FALSE){lale_empirisch<-as_tibble(lale_empirisch)}
  
  
  ## Ueberpruefe ob die Pseudonyme in lale_empirisch sowie in der Referenz vorhanden sind.
  lale_empirisch <-  lale_empirisch[order(lale_empirisch$Pseudonym),]
  
  if (Auswertung=="LALE5"){
    if (!(is(regelstandard)=="list")[1]) {regelstandard <- list(regelstandard_Mathe = c(0,389.5,459.5,529.5,599.5,999), regelstandard_Lesen = c(0,390,465,540,615,999))}
    regel.farben <- c(n.v.="#dadada","unter Mindeststandard"="#ff6600","Mindeststandard"="#ffcc00","Regelstandard"="#99ff99","Regelstandard Plus"="#339933","Maximalstandard"="#003300")
    regelstandard <- c(regelstandard, list(Farben=regel.farben))
    
  }
  
  if (Auswertung=="LALE7"){
    ## Farben fuer Leistungswerte ## hier wird spaeter 200 entfernt fuer die Faerbung damit von 0-300 die untere klasse ist
    if (!(is(regelstandard)=="list")[1]) {regelstandard <- list(regelstandard_Mathe = c(0,200,300,400,500,600,700,999), regelstandard_Lesen = c(0,200,300,400,500,600,700,999))}
    regel.farben <- c(n.v.="#dadada","$<$300"="#254061","300-400"="#376092","400-500"="#95b3d7","500-600"="#dce6f2","600-700"="#b9cde5","$>$700"="#32618d")
    regelstandard <- c(regelstandard, list(Farben=regel.farben))
    # Schuelerr. erst im Klassenr. call
    schulrueckmeldung <- schulrueckmeldung_LALE7
    klassenrueckmeldung <- klassenrueckmeldung_LALE7
  }
  
  if (Auswertung=="LALE9"){
    if (!(is(regelstandard)=="list")[1]) {regelstandard <- list(regelstandard_Mathe = c(0,389.5,459.5,529.5,599.5,999), regelstandard_Lesen = c(0,390,465,540,615,999))}
    regel.farben <- c(n.v.="#dadada","unter Mindeststandard"="#ff6600","Mindeststandard"="#ffcc00","Regelstandard"="#99ff99","Regelstandard Plus"="#339933","Maximalstandard"="#003300")
    regelstandard <- c(regelstandard, list(Farben=regel.farben))
  }
  
 # print('hier bin ich')
  
  
  #### Nur einzelne Schulen werden ausgewertet
  if (is.na(Schulnummer)) {Schulnummer <- unique(lale_empirisch$SCHULNUMMER)}
  
  # fuer Tests
  # x = "176" # Zeile mit "lapply(Schulnummer, function(x))" skippen
  
  ## Schulrueckmeldung wird ausgefuehrt
  lapply(Schulnummer,function(x) # Loop: Schulnummern werden durchlaufen, Variable x erhaelt die einzelnen Werte von Schulnummer
  {
    
    ## Setze Arbeitsumgebung fuer die jeweilige Schule
    dir.name <- paste("LALE_",substr(schuljahr,1,4),"_Rueckmeldung_Schule_",x,sep="")
    dir.create(dir.name)
    setwd(dir.name)
    
    Mathematik_Leitideen_ober <- lapply(Mathematik_Leitideen, function(x) x[grep(1,x$schulart),])
    Mathematik_Leitideen_gym <- lapply(Mathematik_Leitideen, function(x) x[grep(2,x$schulart),])
    
    if (unique(lale_empirisch[which(lale_empirisch$SCHULNUMMER==x),"schulart"])==1){leitidee_tabelle_emp <- Mathematik_Leitideen_ober}
    if (unique(lale_empirisch[which(lale_empirisch$SCHULNUMMER==x),"schulart"])==2){leitidee_tabelle_emp <- Mathematik_Leitideen_gym}
    
    ## Schulrueckmeldung fuer Schule x (ab hier bis setwd auskommentieren, wenn man in die Unterskripte will)
    schulrueckmeldung(Auswertung=Auswertung, lale_empirisch = lale_empirisch, schule = x, schuljahr=schuljahr, regelstandard = regelstandard, leitidee_tabelle = leitidee_tabelle_emp, pdf = pdf, logo_dir = logo_dir, word = word, Legenden=Legenden, ext=Schule_Grafiken[which(Schule_Grafiken$Schule==x),])
    
    ## Klassenrueckmeldungen mittels lapply fuer alle Klassen in Klassen_kenn der Schule x
    ## Klassenrueckmeldung mittels Klassennummer fuer nur eine Klasse
    if (is.na(Klassennummer)==FALSE){
      klassenrueckmeldung(lale_empirisch = lale_empirisch, Auswertung = Auswertung, klasse = Klassennummer, schuljahr = schuljahr, regelstandard = regelstandard, leitidee_tabelle = leitidee_tabelle_emp, pdf = pdf, logo_dir = logo_dir, Fuenf_Prozent = Fuenf_Prozent, word = word, Legenden=Legenden, Klasse_Grafiken=Klasse_Grafiken[which(paste0(Klasse_Grafiken$Schule,"_",Klasse_Grafiken$Klasse)==Klassennummer),], extschueler=Schueler_Grafiken)
    } else {lapply(unlist(unique(lale_empirisch[which(lale_empirisch$SCHULNUMMER==x),"Klassen_kenn"])), function(y) {
      klassenrueckmeldung(lale_empirisch = lale_empirisch, Auswertung = Auswertung, klasse = y, schuljahr = schuljahr, regelstandard = regelstandard, leitidee_tabelle = leitidee_tabelle_emp, pdf = pdf, logo_dir = logo_dir, Fuenf_Prozent = Fuenf_Prozent, word = word, Legenden=Legenden, Klasse_Grafiken=Klasse_Grafiken[which(paste0(Klasse_Grafiken$Schule,"_",Klasse_Grafiken$Klasse)==y),], extschueler=Schueler_Grafiken)
    })}
    ## Gehe zurueck in die urspruengliche Arbeitsumgebung
    setwd("..")
  })
}


########## konvertiere SAV ############

## R Skript for Testing on LALE 5
## Author Philipp Kraemer / philipp.kraemer@posteo.de

## Funktion arbeitet u.a. die Schritte aus daten-sps-Datei.docx ab, ToDo:
## Brauchen wir TN_MA2 etc.?

#setwd('E:/Dezember2021/LALE_7_2021/')

#Auswertung = "LALE7"; Datei = "HB_L721.sav";
#Datei_Referenz = "L519_BISTA.sav"; 
#Mathematik_Leitideen = "Mathe_Leitideen_LALE7.xlsx"; 
#Deutsch_Leitideen = "Deutsch_Leitideen_LALE7.xlsx"; 
#Vergleichsgruppen = "Vergleichsgruppen_LALE7.xlsx"; schulnummer = FALSE; setze99 = TRUE

#Auswertung=c("LALE5","LALE7","LALE9");Datei_Referenz=NA; spalten_behalten="NA"; schulnummer=c("Entferne.6"); NA.Werte=NA; LSE.entferne="Ge"; Pseudonym.Regel="7Char"; entferne.NA.spalten=TRUE; Pseudonym.Spale="TestPseudonym"; bista_NA_Wert = 100; skip = FALSE; setze99 = FALSE
#Auswertung = "LALE7"; Datei_Referenz=NA; spalten_behalten="NA";schulnummer=c("Entferne.6"); NA.Werte=NA; LSE.entferne="Ge"; Pseudonym.Regel="7Char"; entferne.NA.spalten=TRUE; Pseudonym.Spale="TestPseudonym"; bista_NA_Wert = 100; skip = FALSE; setze99 = FALSE


konvertiere_sav <- function(Auswertung=c("LALE5","LALE7","LALE9"), Datei, Datei_Referenz=NA, Mathematik_Leitideen, Deutsch_Leitideen, Vergleichsgruppen, spalten_behalten="NA", schulnummer=c("Entferne.6"), NA.Werte=NA, LSE.entferne="Ge", Pseudonym.Regel="7Char", entferne.NA.spalten=TRUE, Pseudonym.Spale="TestPseudonym", bista_NA_Wert = 100, skip = FALSE, setze99 = FALSE)
{
  if (is.object(Mathematik_Leitideen)==FALSE) {Mathematik_Leitideen <- read_excel(Mathematik_Leitideen)}
  if (is.object(Deutsch_Leitideen)==FALSE) {Deutsch_Leitideen <- read_excel(Deutsch_Leitideen)}
  if (is.object(Vergleichsgruppen)==FALSE) {Vergleichsgruppen <- read_excel(Vergleichsgruppen)} # ohne if-befehl ausfuehren, klappt nur im function-kontext
  #Mathematik_Leitideen <- read_excel(Mathematik_Leitideen)
  #Deutsch_Leitideen <- read_excel(Deutsch_Leitideen)
  #Vergleichsgruppen <- read_excel(Vergleichsgruppen) # ohne if-befehl ausfuehren, klappt nur im function-kontext
  
  ## Checkt via is.object() ob es ein Dateiname oder ein Objekt mit korrektem class Attribut ist
  if (is.character(Datei)==TRUE){  
    lale_empirisch <- read_sav(Datei,  ## liest Daten in path
                               user_na = NA.Werte,
                               .name_repair = "unique") ## Spaltenbezeichner werden Einzigartig vergeben und ggfs. repariert   
  }else{
    lale_empirisch <- Datei
  }
  
  if (is.na(Datei_Referenz)==FALSE){
    if (is.character(Datei_Referenz)==TRUE){  
      lale_Referenz <- read_sav(Datei_Referenz,  ## liest Daten in path
                                user_na = NA.Werte,
                                .name_repair = "unique") ## Spaltenbezeichner werden Einzigartig vergeben und ggfs. repariert   
    }else{
      lale_Referenz <- Datei_Referenz
    }}
  
  ## Checkt ob die Leitideen auch im Datensatz zu finden sind XX
  if (any(Mathematik_Leitideen$Name%in%colnames(lale_empirisch)==FALSE)==TRUE){stop(paste("Nicht alle Mathematik Aufgaben finden sich in den Daten wieder; Ueberpruefen."))}## Mathematik_Leitideen$Name[!which(Mathematik_Leitideen$Name%in%colnames(lale_empirisch))])}
  if (any(Deutsch_Leitideen$Name%in%colnames(lale_empirisch)==FALSE)==TRUE){stop(paste("Nicht alle Deutsch Aufgaben finden sich in den Daten wieder; Ueberpruefen."))}## Deutsch_Leitideen$Name[!which(Deutsch_Leitideen$Name%in%colnames(lale_empirisch))])}
  
  # Fuegt 99 anstatt NA ein fuer alle nicht bearbeiteten Aufgaben aber im jeweiligen Heft vorhandenen
  
  setzeNAs <- function(lale_empirisch, leitideen)
  {
    
    
    all_leit<-which(colnames(lale_empirisch)%in%leitideen$Name) # Alle Aufgaben in den Daten
    lale_empirisch[,all_leit] <- lale_empirisch[,all_leit] %>% replace(.,is.na(.),99)
    for (i in 1:nrow(lale_empirisch)){
      x_leit<-which(names(lale_empirisch)%in%unlist(leitideen[grep(lale_empirisch[i,"HEFT"],leitideen$Heft),"Name"])) # Welche Aufgaben von Mensch 1 Heft 1 in Daten
      
      lale_empirisch[i,all_leit[!all_leit%in%x_leit]]<-NA}
    
    return(lale_empirisch)
  }
  
  if (setze99==TRUE){
    lale_empirisch <- setzeNAs(lale_empirisch,Deutsch_Leitideen)
    lale_empirisch <- setzeNAs(lale_empirisch,Mathematik_Leitideen)
    message("NAs wurden mit 99 ersetzt fuer die Aufgaben der jeweiligen Hefte zu den einzelnen Pseudonymen. ACHTUNG es kann nur korrekt funktionieren wenn die Aufgaben in den Leitideen korrekt den einzelnen Heften zugeordnet wurden.")
  }
  
  Mathematik_Leitideen$Beschreibung <- gsub(pattern = "newline",replacement = "\n",x = Mathematik_Leitideen$Beschreibung)
  
  ## Check der Inputdaten
  if (is.data.frame(Deutsch_Leitideen)==TRUE) { Deutsch_Leitideen_Zabka <- split(Deutsch_Leitideen,Deutsch_Leitideen$Zabka);  Deutsch_Leitideen <- split(Deutsch_Leitideen,Deutsch_Leitideen$Leitidee)}
  if (is.data.frame(Mathematik_Leitideen)==TRUE) { Mathematik_Leitideen <- split(Mathematik_Leitideen,Mathematik_Leitideen$Leitidee) }
  
  ### Erste Spalten Aenderungen
  ## aendere Spaltenname fuer das Pseudonym
  colnames(lale_empirisch)[colnames(lale_empirisch)==as.character(Pseudonym.Spale)] <- "Pseudonym"
  
  ## Teste fuer doppelte Pseudonyme
  Falsche.Pseudonyme <- lale_empirisch[which(table(lale_empirisch$Pseudonym)!=1),]
  if (nrow(Falsche.Pseudonyme)>=1) {warnings("Doppelte Pseudonyme wurde im Datensatz entdeckt.")
    return(Falsche.Pseudonyme)}
  ## Teste fuer fehlerhafte Pseudonyme
  if (Pseudonym.Regel=="7Char"){
    Fehlende.Pseudonyme <- lale_empirisch[which(nchar(lale_empirisch$Pseudonym)!=7),"Pseudonym"]
    if (nrow(Fehlende.Pseudonyme)>=1) 
    {warnings("Es wurden Pseudonym entdeckt die nicht der 7 Character Regel entsprechen.\n Die entsprechenden Pseudonyme sollten korrigiert werden oder diese Funktion muss mit Pseudonym.Regel==NA ausgefuehrt werden, um die Fehler zu ignorieren.")
      return(Fehlende.Pseudonyme)}
  }
  
  ## Aendert die Schulnummern im Datensatz
  # if (schulnummer=="Entferne.6") {
  #  lale_empirisch$SCHULNUMMER <- substring(lale_empirisch$SCHULNUMMER,2)
  #warning("Die erste Zahl in der Spalte SCHULNUMMER wurde entfernt. Soll dies nicht durchgefuehrt werden, bitte die Funktion erneut mit z.B. schulnummer==NA ausfuehren.")
  #}
  ## ueberprueft die Schulnummern im Datensatz
  if(any(sapply(lale_empirisch$SCHULNUMMER,nchar)!=3)) {warning("Es gibt Schulnummern mit einer Nummerierung < 100, bitte die Schulnummern ueberpruefen, eventuell treten hier Fehler auf. Ggfs. den Parameter schulnummer bitte anpassen.")}
  
  ## Fuege eine Spalte fuer die Schulart ein
  lale_empirisch <- cbind(lale_empirisch,schulart=as.numeric(as.character(factor(lale_empirisch$SCHULNUMMER, levels=Vergleichsgruppen$Schulnummer, labels=Vergleichsgruppen$Schulart))))
  
  ## Fuege eine Spalte fuer die Vergleichsgruppe ein
  lale_empirisch <- cbind(lale_empirisch,Vergleichsgruppe=factor(lale_empirisch$SCHULNUMMER, levels=Vergleichsgruppen$Schulnummer, labels=as.character(as.vector(Vergleichsgruppen$Vergleich))))
  lale_empirisch <- cbind(lale_empirisch,referenzgruppe=as.numeric(as.character(factor(lale_empirisch$SCHULNUMMER, levels=Vergleichsgruppen$Schulnummer, labels=as.character(as.vector(Vergleichsgruppen$Referenz))))))
  
  ## Fuege eine Spalte Klassen_kenn ein /factor or nonfactorXX
  lale_empirisch <- cbind(lale_empirisch,Klassen_kenn=paste(lale_empirisch$SCHULNUMMER, lale_empirisch$KLASSE, sep="_"))
  lale_empirisch$Klassen_kenn <- as.character(lale_empirisch$Klassen_kenn)
  
  ## Berechnet die Relativen Haeufigkeiten von 1,0 und NA fuer die jeweiligen leitideen
  
  Relativ.Leitidee <- function(pseudonym, leitidee.liste){
    Aufgaben.vector <- leitidee.liste[grep(lale_empirisch[which(lale_empirisch$Pseudonym==pseudonym),"HEFT"],leitidee.liste$Heft),"Name"]
    Relativ <- prop.table(table(factor(lale_empirisch[which(lale_empirisch$Pseudonym==pseudonym),which(colnames(lale_empirisch)%in%unlist(Aufgaben.vector))],levels=c(1,0,99,NA))))
    return(Relativ)
  }
  
  # Fuer Mathe
  Relativ_Mathe <- do.call("cbind",lapply(Mathematik_Leitideen,function(y)
  {do.call("rbind",lapply(lale_empirisch$Pseudonym,function(x) Relativ.Leitidee(x,y)))}))
  
  colnames(Relativ_Mathe) <- paste(rep(names(Mathematik_Leitideen),each=3),rep(c("_Richtig_Mathe","_Falsch_Mathe","_99_Mathe"),length(names(Mathematik_Leitideen))),sep="")
  lale_empirisch <- cbind(lale_empirisch,Relativ_Mathe)
  
  # Fuer Deutsch
  Relativ_Deutsch <- do.call("cbind",lapply(Deutsch_Leitideen,function(y)
  {do.call("rbind",lapply(lale_empirisch$Pseudonym,function(x) Relativ.Leitidee(x,y)))}))
  
  colnames(Relativ_Deutsch) <- paste(rep(names(Deutsch_Leitideen),each=3),rep(c("_Richtig_Deutsch","_Falsch_Deutsch","_99_Deutsch"),length(names(Deutsch_Leitideen))),sep="")
  lale_empirisch <- cbind(lale_empirisch,Relativ_Deutsch)
  
  # Fuer Deutsch Zabka Unterteilung
  Relativ_Deutsch_Zabka <- do.call("cbind",lapply(Deutsch_Leitideen_Zabka,function(y){do.call("rbind",lapply(lale_empirisch$Pseudonym,function(x) Relativ.Leitidee(x,y)))}))
  
  colnames(Relativ_Deutsch_Zabka) <- paste(rep(names(Deutsch_Leitideen_Zabka),each=3),rep(c("_Zabka_Richtig","_Zabka_Falsch","_Zabka_99"),length(names(Deutsch_Leitideen_Zabka))),sep="")
  lale_empirisch <- cbind(lale_empirisch, Relativ_Deutsch_Zabka)
  
  # Fuer Gesamt Mathe Mittelwert berechnen XX ueberpruefen Summiere alle 1 relativ zu allen 1,0,99 XXX
  Mathe <- lapply(lapply(apply(lale_empirisch[,do.call(rbind,Mathematik_Leitideen)$Name],1, table),prop.table),function(x) x["1"])
  Mathe <- do.call(rbind,Mathe) %>% replace(.,is.na(.),0)
  lale_empirisch <- add_column(lale_empirisch,Mathe=Mathe)
  
  # Fuer Gesamt Mathe Mittelwert berechnen XX ueberpruefen Summiere alle 1 relativ zu allen 1,0,99 XXX
  Deutsch <- lapply(lapply(apply(lale_empirisch[,do.call(rbind,Deutsch_Leitideen)$Name],1, table),prop.table),function(x) x["1"])
  Deutsch <- do.call(rbind,Deutsch) %>% replace(.,is.na(.),0)
  lale_empirisch <- add_column(lale_empirisch,Deutsch=Deutsch)
  
  if (length(spalten_behalten)>1){
    lale_empirisch <- lale_empirisch[,spalten_behalten]
    message(paste("Die Spalten",spalten_behalten,"werden in die Datei",paste(datei,"_konvertiert",sep=""),"geschrieben."))
  }
  
  LTest <-lale_empirisch
  
  ## Fuegt eine Variable LSV an die angibt ob E-Heft ausgegeben wurde oder nicht, d.h. Foerderbedarf bestand oder nicht
  if ("LSV" %in% colnames(lale_empirisch)==TRUE) {message("Spalte LSV bereits vorhanden in Datei")} else {
    lale_empirisch <- add_column(lale_empirisch,LSV=factor(lale_empirisch$HEFT,levels=c("A","B","C","D","E"), labels=c(0,0,0,0,1)))
    message("Spalte LSV dem Datensatz hinzugefuegt, Hefte A,B,C,D,E betrachtet; Heft E ist in LSV = 1 gesetzt.")}
  
  ## !!!!! Wieviele 0 in der fortlaufenden Nummerierung - gerade sind es 4 d.h. es gibt doppelte Nummerierungen wenn wir mehr als 999 Schueler haben
  ## Fuege eine Spalte Code_nr ein
  ## Kann Weg?
  lale_empirisch <- add_column(lale_empirisch, Code_nr=paste(lale_empirisch$SCHULNUMMER,factor(lale_empirisch$KLASSE,levels=c("5a","5b","5c","5d","5E","5e"),labels=c("01","02","03","04","05","05")),sprintf("%04d", seq(1:nrow(lale_empirisch))),sep=""))
  
  ## LSE "Ge" wird entfernt
  # spalte LSE existiert von anfang nicht!!!!!!!!!!!!!!!!!!!
  lale_empirisch <- lale_empirisch[lale_empirisch$LSE!=LSE.entferne,]
  warnings(paste("Pseudonyme mit dem K\"urzel",LSE.entferne,"in der Spalte LSE wurden aus dem Datensatz entfernt und m\"ussen ggfs. extra ausgewertet werden."))
  
  lale_empirisch <- add_column(lale_empirisch,NAME_SCHULE=factor(lale_empirisch$SCHULNUMMER,levels=Vergleichsgruppen$Schulnummer, labels=Vergleichsgruppen$Schule))
  
  ## Test auf fehlende Werte
  na_columns <- which(lapply(sapply(lale_empirisch,table),length)==0)
  if (length(na_columns)==0) {message("Spalten auf fehlende Werte ueberprueft.")} else {warnings(paste("Folgende Spalten enthalten nur NA Werte:",unlist(na_columns)))}
  
  ## Test auf Einzigartigkeit der Spalten Pseudonym; FK_LERNSTCOD
  if (TRUE%in%duplicated(lale_empirisch$Pseudonym)) {warnings(paste("Die folgenden Pseudonyme wurden doppelt vergeben:", as.vector(lale_empirisch[which(duplicated(lale_empirisch$Pseudonym)==TRUE),"Pseudonym"])))} else {message("In der Spalte Pseudonym wurden keine doppelten Werte gefunden.")}
  if (TRUE%in%duplicated(lale_empirisch$FK_LERNSTCOD)) {warnings(paste("Die folgenden Pseudonyme wurden doppelt vergeben:", as.vector(lale_empirisch[which(duplicated(lale_empirisch$FK_LERNSTCOD)==TRUE),"FK_LERNSTCOD"])))} else {message("In der Spalte FK_LERNSTCOD wurden keine doppelten Werte gefunden.")}
  
  ## Test auf Anzahl der Vergleichsgruppen pro Schule
  vergl <- table(lale_empirisch$SCHULNUMMER,lale_empirisch$referenzgruppe)
  message(paste("Es wurden",ncol(vergl),"Vergleichsgruppen gefunden."))
  if (any(rowSums(vergl!=0)>1)) {warnings(paste("Es wurde mehr als eine Vergleichsgruppe gefunden fuer Schulnummer:", rownames(vergl)[rowSums(vergl!=0)>1]))}
  if (any(rowSums(vergl!=0)==0)) {warnings(paste("Es wurde keine Vergleichsgruppe gefunden fuer Schulnummer:", rownames(vergl)[rowSums(vergl!=0)==0]))}
  
  ## Test auf Schulart Bezeichnung pro Schule
  schul_art <- table(lale_empirisch$SCHULNUMMER,lale_empirisch$schulart)
  if (any(rowSums(schul_art!=0)>1)) {warnings(paste("Es wurde mehr als eine Zuordnung zu einer Schulart gefunden fuer Schulnummer:", rownames(schul_art)[rowSums(schul_art !=0)>1]))}
  if (any(rowSums(schul_art!=0)==0)) {warnings(paste("Es wurde keine Zuordnung zu einer Schulart gefunden fuer Schulnummer:", rownames(schul_art)[rowSums(schul_art !=0)==0]))}
  
  ## Testet auf negative Werte und Werte unter 100 werde NA gesetzt
  ## Wenn BISTA NA oder bis 100 fuer Mathe oder Deutsch dann Mathe oder Deutsch jeweils komplett auf NA setzen
  d_bista_100 <- which(lale_empirisch$delwlemeanbista<=bista_NA_Wert)
  m_bista_100 <- which(lale_empirisch$mathewle_Bista<=bista_NA_Wert)
  m_bista_na <- which(is.na(lale_empirisch$mathewle_Bista))
  d_bista_na <- which(is.na(lale_empirisch$delwlemeanbista))
  
  ## Alle Relevanten Werte werden auf NA gesetzt.
  ## Will man zum Beispiel Balken in den einzelnen Schuelerrueckmeldungen erhalten auch wenn BISTA==NA dann muss man diese hier rausnehmen
  ## Fuer die Klassen und Schulreuckmeldungen wird jeweils nocmal auf BISTA NA getestet und alle NAs fallen hier raus
  ## Nur der Plot in dem Relative Haeufigkeiten fuer die n.v. Werte ausgegeben werden werden alle Werte herangezogen, hier aber mit der abfrage lale_empirisch TN==1; TN==0 fallen auch hier raus. 
  lale_empirisch[c(d_bista_100,d_bista_na),c("delwlemeanbista", "dlkomp", "DL_Kino_LH","DL_Meister_LH","DL_Bergrennen_LH","DL_manifeste_infos_LH","DL_lokal_erschliessen_LH","DL_globale_zshg_LH","DL_Vorstellung_LH","DL_Sprachgestaltung_LH")] <- NA
  lale_empirisch[c(m_bista_100,m_bista_na),c("mathewle_Bista", "matheKS", "ML1","ML2","ML3","ML4","ML5", "Mathe_L1", "Mathe_L2", "Mathe_L3", "Mathe_L4", "Mathe_L5")] <- NA
  lale_empirisch[c(d_bista_100,d_bista_na),c("Bergrennen_Richtig_Deutsch","Bergrennen_Falsch_Deutsch", "Bergrennen_99_Deutsch", "Schulkinotage_Richtig_Deutsch", "Schulkinotage_Falsch_Deutsch", "Schulkinotage_99_Deutsch", "Die Meisterdiebe_Richtig_Deutsch", "Die Meisterdiebe_Falsch_Deutsch", "Die Meisterdiebe_99_Deutsch", 
                                             "Z1_Zabka_Richtig", "Z1_Zabka_Falsch", "Z1_Zabka_99", 
                                             "Z2_Zabka_Richtig", "Z2_Zabka_Falsch", "Z2_Zabka_99", 
                                             "Z3_Zabka_Richtig", "Z3_Zabka_Falsch", "Z3_Zabka_99",
                                             "Z4_Zabka_Richtig", "Z4_Zabka_Falsch", "Z4_Zabka_99",
                                             "Z5_Zabka_Richtig", "Z5_Zabka_Falsch", "Z5_Zabka_99",
                                             "Deutsch")] <- NA
  lale_empirisch[c(m_bista_100,m_bista_na),c("ML1_Richtig_Mathe", "ML1_Falsch_Mathe", "ML1_99_Mathe",
                                             "ML2_Richtig_Mathe", "ML2_Falsch_Mathe", "ML2_99_Mathe",
                                             "ML3_Richtig_Mathe", "ML3_Falsch_Mathe", "ML3_99_Mathe",
                                             "ML4_Richtig_Mathe", "ML4_Falsch_Mathe", "ML4_99_Mathe",
                                             "ML5_Richtig_Mathe" ,"ML5_Falsch_Mathe" ,"ML5_99_Mathe",
                                             "Mathe")] <- NA
  
  
  # Finde Schulnummern mit mehr als 3 Ziffern und Klassen mit mehr als 2 Ziffern
  if (length(seq(1:nrow(lale_empirisch))[which(sapply(as.vector(lale_empirisch$SCHULNUMMER),nchar)>3)])>1) {warning(cat("Es wurden Schulnummern, die nicht aus 3 Ziffern bestehen, gefunden in den Zeilen:",seq(1:nrow(lale_empirisch))[which(sapply(as.vector(lale_empirisch$SCHULNUMMER),nchar)!=3)],"\n"))}
  if (length(seq(1:nrow(lale_empirisch))[which(sapply(as.vector(lale_empirisch$KLASSE),nchar)>2)])>1) {warning(cat("Es wurden Klassen, die nicht aus 2 Ziffern bestehen, gefunden in den Zeilen:",seq(1:nrow(lale_empirisch))[which(sapply(as.vector(lale_empirisch$KLASSE),nchar)!=2)],"\n"))}
  
  ## Test auf E-Schueler und Nicht E-Schueler
  ## Nicht E Schueler sollten NA haben in der Spalte DL_Bergrennen_LH und E-Heft Schueler in der Spalte DL_Meister_LH
  Nicht_E_Schueler_mit_Bergrennen <- as.vector(unlist(lale_empirisch[which(!lale_empirisch$HEFT=="E")[!is.na(lale_empirisch[which(!lale_empirisch$HEFT=="E"),"DL_Bergrennen_LH"])],"Pseudonym"]))
  E_Schueler_mit_Meisterdiebe <- as.vector(unlist(lale_empirisch[which(lale_empirisch$HEFT=="E")[!is.na(lale_empirisch[which(lale_empirisch$HEFT=="E"),"DL_Meister_LH"])],"Pseudonym"]))
  
  # Zusammenfassung der Schulen und Klassenkonfiguration
  message("\nFolgende Schulen und deren Klassen wurden geladen.")
  print(table(lale_empirisch$NAME_SCHULE,lale_empirisch$KLASSE))
  message("\nFolgende Schulnummern und deren Klassen wurden geladen.")
  print(table(lale_empirisch$SCHULNUMMER,lale_empirisch$KLASSE))
  
  if (Auswertung=="LALE5"){
    # Testen auf E Heft und nicht E Heft Schueler
    if (length(E_Schueler_mit_Meisterdiebe)+length(Nicht_E_Schueler_mit_Bergrennen)>0 & skip==FALSE)
      # {lale_empirisch <- list(E_Schueler_mit_Meisterdiebe=E_Schueler_mit_Meisterdiebe, Nicht_E_Schueler_mit_Bergrennen=Nicht_E_Schueler_mit_Bergrennen)
    {
      e_heft_kontrolle <- list(E_Schueler_mit_Meisterdiebe=E_Schueler_mit_Meisterdiebe, 
                               Nicht_E_Schueler_mit_Bergrennen=Nicht_E_Schueler_mit_Bergrennen)
      warning(paste("Es wurden Schueler mit E Heft gefunden die auch Werte beim Sachtext Meisterdiebe haben oder es wurden Schueler ohne E Heft gefunden die in der Spalte DL_Bergrennen_LH haben.\n Bitte Ueberpruefen Sie die Zuordnung es sollten Schueler ohne E-Heft in der Bergrennen Spalte NA Werte haben und Schueler mit E Heft in der Meisterdiebe Spalte NA Werte haben."))
    }
    
    if (length(E_Schueler_mit_Meisterdiebe)+length(Nicht_E_Schueler_mit_Bergrennen)>0 & skip==TRUE){
      warning("Es wurden Schueler mit E Heft gefunden die auch Werte beim Sachtext Meisterdiebe haben; oder es wurden Schueler ohne E Heft gefunden die in der Spalte DL_Bergrennen_LH Werte haben.\n Bitte Ueberpruefen Sie die Zuordnung. Es sollten Schueler ohne E-Heft in der Bergrennen Spalte NA Werte haben und Schueler mit E Heft in der Meisterdiebe Spalte NA Werte haben.")
      warning("Da skip=TRUE gewaehlt wurde, werden die Spalten nun alle auf NA gesetzt; jeweils fuer E-Heft Schueler die Meisterdiebe Spalte und fuer alle nicht E-Heft Schueler die Bergrennen Spalte.")
      lale_empirisch[which(!lale_empirisch$HEFT=="E"),"DL_Bergrennen_LH"] <- NA
      lale_empirisch[which(lale_empirisch$HEFT=="E"),"DL_Meister_LH"] <- NA
    }
    
    # Testen auf TN_DL und TN_MA
    falsch_dl <- c(which(lale_empirisch$dlkomp=="NA" & lale_empirisch$TN_DL!=0),which(lale_empirisch$dlkomp!="NA" & lale_empirisch$TN_DL==0))
    falsch_ma <- c(which(lale_empirisch$matheKS=="NA" & lale_empirisch$TN_MA!=0),which(lale_empirisch$matheKS!="NA" & lale_empirisch$TN_MA==0))
    
    warning(paste("Folgende Schueler haben problematische Kombinationen in den Spalten TN_DL und dlkomp",lale_empirisch[falsch_dl,"Pseudonym"]))
    warning(paste("Folgende Schueler haben problematische Kombinationen in den Spalten TN_MA und matheKS",lale_empirisch[falsch_ma,"Pseudonym"]))
  }
  
  lale_Referenz_out <- 0
  Vorjahr_Vergleich <- 0
  
  if (Auswertung=="LALE7"){
    # Gibt die Pseudonyme aus die nicht in beiden Jahren gleich vorhanden sind
    Vorjahr_Vergleich <- list(Fehlen_im_aktuellen_Jahr=lale_Referenz$Pseudonym[!which(lale_Referenz$Pseudonym%in%lale_empirisch$Pseudonym)],
                              Fehlen_im_Vorjahr=lale_empirisch$Pseudonym[!which(lale_empirisch$Pseudonym%in%lale_Referenz$Pseudonym)])
    
    # Haengt die beiden VorjahresBistas plus das Kontroll.Pseudonym an
    lale_Referenz_out <- lale_Referenz
    lale_Referenz <- as.data.frame(lale_Referenz[which(lale_Referenz$Pseudonym%in%lale_empirisch$Pseudonym),])
    lale_Referenz <- lale_Referenz[match(lale_empirisch$Pseudonym,lale_Referenz$Pseudonym),c("Pseudonym","delwlemeanbista","mathewle_Bista")]
    lale_Referenz[which(lale_Referenz$delwlemeanbista<=bista_NA_Wert),"delwlemeanbista"] <- NA
    lale_Referenz[which(lale_Referenz$mathewle_Bista<=bista_NA_Wert),"mathewle_Bista"] <- NA
    lale_empirisch <- add_column(lale_empirisch, Pseudonym_LALE5=lale_Referenz[,1], delwlemeanbista_LALE5=lale_Referenz[,2], mathewle_Bista_LALE5=lale_Referenz[,3])
  }
  
  ## Exportiert die Datei
  return(list(Daten=lale_empirisch,Mathematik_Leitideen=lapply(Mathematik_Leitideen,as.data.frame),Deutsch_Leitideen=lapply(Deutsch_Leitideen,as.data.frame),LALE_REFERENZ_VORJAHR=lale_Referenz_out,Unterschied_Pseudonyme_Vorjahr=Vorjahr_Vergleich))
}


####### Individuelle Entwicklung #######

IndividuelleEntwicklung <- function(lale_empirisch, Datenspalte_Ende=c(Deutsch="delwlemeanbista",Mathe="mathewle_Bista"), Datenspalte_Start=c(Deutsch="delwlemeanbista_LALE5",Mathe="mathewle_Bista_LALE5"), klasse="NA")
{
  IndEnt <- function(lale_empirisch,klasse,Datenspalte_Start,Datenspalte_Ende, Ordnung)
  {
    par(mar = c(2, 5, 2, 2)) # par(mar = c(bottom, left, top, right))
    data <- lale_empirisch[which(lale_empirisch$Klassen_kenn==klasse),c("Pseudonym",Datenspalte_Start,Datenspalte_Ende)]
    data[which(data[,2]>800),2] <- 800 
    data[which(data[,3]>800),3] <- 800 
    data[,2] <- round(data[,2],digits=0)
    data[,3] <- round(data[,3],digits=0)
    if(Ordnung=="LALE7_AUF")
    {
      data <- data[order(data[,3],decreasing = F),]
    }
    if(Ordnung=="LALE7_AB")
    {
      data <- data[order(data[,3],decreasing = T),]
    }
    if(Ordnung=="LALE5_AUF")
    {
      data <- data[order(data[,2],decreasing = F),]
    }
    if(Ordnung=="LALE5_AB")
    {
      data <- data[order(data[,2],decreasing = T),]
    }
    diff.vector <- unlist(sign(data[,3]-data[,2]))
    diff.vector[is.na(diff.vector)] <- 1
    diff.vector[which(diff.vector==0)] <- 1
    B.S <- unlist(data[,2])
    B.E <- unlist(data[,3])
    
    plot(seq(1,length(data[,2]))~B.S, pch=18, cex = 3, col = "gray", xlab = "Leistungswerte", xlim = c(100,800), axes = F, ylab="")
    axis(las = 1, side = 2, at = seq(1:length(data[,2])), tick = T, labels = data$Pseudonym)
    axis(las = 1, side = 1, at = seq(100,800,100), tick=T, labels = seq(100,800,100))
    points(y=seq(1:length(data[,3])), x=B.E, pch=16, cex = 3, col = "darkgray")
    text(round(B.S), x=B.S-(30*diff.vector), y=seq(1:length(data[,2])))
    text(round(B.E), x=B.E+(30*diff.vector), y=seq(1:length(data[,3])))
    arrows(x0=B.S, x1=B.E, y1=seq(1:length(data[,2])), y0=seq(1:length(data[,3])), length=0.1, angle=20, lwd=4, col=as.character(factor(sign(B.E-B.S),levels=c(1,-1),labels=c("green","red"))))
  }
  
  lale_emp_DL <- lale_empirisch[which(!is.na(lale_empirisch$delwlemeanbista)==TRUE),]
  lale_emp_MA <- lale_empirisch[which(!is.na(lale_empirisch$mathewle_Bista)==TRUE),]
  
  if (klasse=="NA"){klasse<-unique(lale_empirisch$Klassen_kenn)}
  
  for (i in 1:length(klasse))
  {
    pdf(paste("Bista_Mathe_",klasse[i],".pdf",sep=""))
    IndEnt(lale_emp_MA, klasse=klasse[i], Datenspalte_Start=Datenspalte_Start$Mathe, Datenspalte_Ende=Datenspalte_Ende$Mathe, Ordnung="LALE5_AUF")
    IndEnt(lale_emp_MA, klasse=klasse[i], Datenspalte_Start=Datenspalte_Start$Mathe, Datenspalte_Ende=Datenspalte_Ende$Mathe, Ordnung="LALE5_AB")
    IndEnt(lale_emp_MA, klasse=klasse[i], Datenspalte_Start=Datenspalte_Start$Mathe, Datenspalte_Ende=Datenspalte_Ende$Mathe, Ordnung="LALE7_AUF")
    IndEnt(lale_emp_MA, klasse=klasse[i], Datenspalte_Start=Datenspalte_Start$Mathe, Datenspalte_Ende=Datenspalte_Ende$Mathe, Ordnung="LALE7_AB")
    dev.off()
  }
  for (i in 1:length(klasse))
  {
    pdf(paste("Bista_Deutsch_",klasse[i],".pdf",sep=""))
    IndEnt(lale_emp_DL, klasse=klasse[i], Datenspalte_Start=Datenspalte_Start$Deutsch, Datenspalte_Ende=Datenspalte_Ende$Deutsch, Ordnung="LALE5_AUF")
    IndEnt(lale_emp_DL, klasse=klasse[i], Datenspalte_Start=Datenspalte_Start$Deutsch, Datenspalte_Ende=Datenspalte_Ende$Deutsch, Ordnung="LALE5_AB")
    IndEnt(lale_emp_DL, klasse=klasse[i], Datenspalte_Start=Datenspalte_Start$Deutsch, Datenspalte_Ende=Datenspalte_Ende$Deutsch, Ordnung="LALE7_AUF")
    IndEnt(lale_emp_DL, klasse=klasse[i], Datenspalte_Start=Datenspalte_Start$Deutsch, Datenspalte_Ende=Datenspalte_Ende$Deutsch, Ordnung="LALE7_AB")
    dev.off()
  }
}




# Programm laufen lassen --------------------------------------------------
## Regelstandards aendern und Farben setzen
regelstandard <- list(regelstandard_Mathe = c(0, 200, 300, 400, 500, 600, 700, 999),
                      regelstandard_Lesen = c(0, 200, 300, 400, 500, 600, 700, 999))
# alte Farben
# regel.farben <- c(n.v. = "#dadada", `$<300` = "#8a3f2d", 
#                  `300-400` = "#DD8047", `400-500` = "#D99E6a", 
#                  `500-600` = "#84C4FC", `600-700` = "#049DBF", 
#                  `$>$700` = "#0468BF")

# neue Farben
regel.farben <- c(n.v. = "#dadada", `$<300` = "#F1EEF6", 
                     `300-400` = "#D0D1E6", `400-500` = "#A6BDDB", 
                     `500-600` = "#74A9CF", `600-700` = "#2B8CBE", 
                     `$>$700` = "#045A8D")
regelstandard <- c(regelstandard, list(Farben = regel.farben))

setwd("C:/RShare/git/LALE5_24-25")

######## Konvertierung von SAV in LALE input Daten; siehe Helpfile
x<-konvertiere_sav(Auswertung = "LALE7", Datei = "L724_final_241009.sav",
                   Datei_Referenz = "L522_BISTA_24.sav", 
                   Mathematik_Leitideen = "Mathe_Leitideen_LALE7.xlsx", 
                   Deutsch_Leitideen = "Deutsch_Leitideen_LALE7.xlsx", 
                   Vergleichsgruppen = "Vergleichsgruppen_LALE7.xlsx", schulnummer = FALSE, setze99 = TRUE)


######ERstellung von individuellen plots im aktuellen Verzeichnis

# IndividuelleEntwicklung(x[[1]])

# #################
### Setze BISTA als NA f\"ur Schueler mit Mathe oder Deutsch == 0 ; bitte manuell ueberpruefen
x[[1]][which(x[[1]]$Deutsch==0),c("Pseudonym","delwlemeanbista", "Deutsch")] ## 2024:8
x[[1]][which(x[[1]]$Mathe==0),c("Pseudonym","mathewle_Bista", "Mathe")] ## 2024:1
# In diesem Fall 5 Werte f\"ur Deutsch
x[[1]][which(x[[1]]$Deutsch==0),"delwlemeanbista"]<-NA
x[[1]][which(x[[1]]$Mathe==0),"mathewle_Bista"]<-NA
## Einmal ueberpruefen
x[[1]][which(x[[1]]$Deutsch==0),c("Pseudonym","delwlemeanbista")]
x[[1]][which(x[[1]]$Mathe==0),c("Pseudonym","mathewle_Bista")]
# 
# #x[[1]][which(x[[1]]$Mathe==0),"mathewle_Bista"]<-"190.4808"

#X Auslesen
library(writexl)

write_xlsx(x$Daten,"L724_konvertiert.xlsx" )

setwd("C:/RShare/git/LALE5_24-25")

######## Starten der pdf Ausgabe #####
LALE_rueckmeldung(lale_empirisch = x$Daten, Auswertung = "LALE7", 
                  schuljahr="2024/25", Mathematik_Leitideen = x[[2]], 
                  Deutsch_Leitideen = x[[3]], 
                  logo_dir = "C:/RShare/git/LALE5_24-25/logo", 
                  Fuenf_Prozent = "ALLE", word=FALSE, 
                  pdf=TRUE, Legenden="C:/RShare/git/LALE5_24-25/Legenden.xlsx", 
                  regelstandard = regelstandard, Schulnummer = "176")


