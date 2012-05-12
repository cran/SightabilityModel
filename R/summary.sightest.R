summary.sightest <-
function(object,...){
  z<-qnorm(1-object$alpha/2)
  if(object$CI.method=="normal"){
    temp<-rep(object$est[1],3)+c(0,-z,z)*sqrt(object$est[2])
  }
  else{  
    cfact<-exp(z*sqrt(log(1+object$est[2]/object$est[1]^2)))
    temp<-rep(object$est[1],3)*c(1,(1/cfact),cfact)
  }
  names(temp)<-NULL
  temp<-round(temp,0)
  cat("\n")
  temp2<-paste("Nhat = ",temp[1] , ";  ",100*(1-object$alpha),"% CI = (", temp[2], ", ", temp[3], ")")
  print(temp2[1])
  out.summary<-list(Nhat=temp[1], lcl=temp[2], ucl=temp[3])  
}
