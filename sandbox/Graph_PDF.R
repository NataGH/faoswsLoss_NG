#' Part of the FAO Loss Module
#' 
#' @author Alicia English
#' 

Graph_PDF <- function(DataIn,indexVar,var,timeVar,lag,LType){

  
dlpath <- file.path(Sys.getenv("R_SWS_SHARE_PATH") ,'plots')
pdffile <- file.path(dlpath, paste("Commodities_",as.character(Sys.Date()),".pdf", sep=""))
lossProtected <-  getLossData_LossDomain(areaVar,itemVar,yearVar,elementVar,selectedYear,'5126')    # Value_measuredElement_5016
lossProtected <- lossProtected %>% filter(loss_per_clean >0)
names(lossProtected) <- tolower(names(lossProtected))
pdf(file = pdffile, height = 11, width = 16)


for(j in 1:length(unique(lossProtected[,tolower(itemVar),with=F]))){
  for(i in 1:length(unique(lossProtected$areaVar))){
    ctry = unique(lossProtected$areaVar)[i]
    crp = unique(lossProtected$itemVar)[j]
    lossProtected[,flagplot:=0,with=T]
    lossProtected [itemVar ==  crp & areaVar == ctry,flagplot:=1,with=T]
    
    tmp <- lossProtected %>% filter(flagplot ==1)
    if(dim(tmp)[1] > 3){
      p = ggplot() + 
        geom_point(data= lossProtected[itemVar ==  crp,,], aes(x = timepointyears, y = value_measuredelement_5126, color = flagobservationstatus_measuredelement_5016 ))+
        geom_line(data= tmp[flagplot == 1  & areaVar ==ctry ,,], aes(x = timepointyears, y =value_measuredelement_5126, color = flagobservationstatus_measuredelement_5016 ), size =2)+
        xlab('timePointYears') + ylab('Loss (%)') +
        theme(axis.text.x = element_text(angle = 45, vjust = .5)) +
        theme(axis.text=element_text(size=12, face="bold"),
              axis.title=element_text(size=12,face="bold")) +
        scale_y_continuous(labels = percent)+
        scale_x_continuous(limits = c(2000, 2014), breaks = seq(2000, 2014, 2)) +
        ggtitle(paste(unique(tmp[flagplot == 1 ,,]$foodgroupname),unique(tmp[flagplot == 1 ,,]$itemVar), sep = ", "))
      print(p)
    }else{next}    
    
  }}

dev.off()