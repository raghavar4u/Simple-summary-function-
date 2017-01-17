DetailSummary<-function(data)
{
 Mean<-round(apply(data, 2, mean,na.rm=TRUE),2)
 Count<-apply(data, 2, length)
 Missing<-apply(data,2,function(x){sum(is.na(x))})
 Unique<-apply(data,2,function(x){length(unique(x))})
 qunat<-t(round(apply(data,2,na.rm=TRUE,quantile,prob=c(0,0.02,0.25,0.50,0.75,0.98,1)),2))
 Output<-cbind(Count,Unique,Missing,Mean,qunat)
 return(Output)
}


DetailSummary(mtcars)
DetailSummary(airquality)
