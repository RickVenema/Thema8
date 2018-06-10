#Moyano et al 2012 Biogoesciences moisture function

moyano<-function(SOM,clay){
  
  
  # =====================================================================
  # ===== R Code for the Analysis of Soil Moisture-Respiration Data =====
  # =====================================================================
  # Note: this script  also analyzes water holding capacity: not in the paper given the low number of datasets
  # Note added by Remi Cardinael and Bertrand Guenet, the original script by Moyano et al was a bit simplified (but not the calculation)
  # to improve the computing time.
  
  # ===== Prepare Data
  # Read in the data files (MRD = moisture respiration data; DD = data description; funs = function indexes)
  MD<-read.csv("MRD.csv")
  DD<-read.csv("DD.csv")
  funs<-read.csv("funs.csv")
  # create the description data.frame by aggregating and binding 
  DD1<-aggregate(x = MD, by = list(MD$id), FUN = "mean")
  DD1<-subset(DD1,select=c(soil.t,bd:pd,co2.dw))
  DD<-cbind(DD, DD1)
  rm(DD1)
  # Transform the mwplog to a practical scale
  MD$mwplog<-(MD$mwplog)/5*(-1)+1
  # Subset some variables to work with 
  MDsub <- subset(MD, select=c(id,mdw,mvol,mps,mwhc,mwplog,min.rel))
  # mdw=gravimetric moisture, mvol=volumetric, mps=water saturation, mwhc=water holding capacity moisture, mwplog=log water potential
  # min.rel=relative carbon mineralization (note: C min. was previously normalized although doing this does not affect the analysis)
  
  # ===== Fit smooth curves to each dataset
  library(gam)
  mod.mdw<-list()
  mod.mvol<-list()
  mod.mwhc<-list()
  mod.mwplog<-list()
  mod.mps<-list()
  models<-list(mod.mdw=mod.mdw,mod.mvol=mod.mvol,mod.mps=mod.mps,mod.mwhc=mod.mwhc,mod.mwplog=mod.mwplog) # Make a list object for the models
  rm(mod.mdw,mod.mvol,mod.mps,mod.mwhc,mod.mwplog)
  for (i in 1:length(DD$id)) {
    WD <- MDsub[MDsub$id==i,] # create a working data frame
    ydata<-WD$min.rel
    for (j in 1:5) {
      xdata<-WD[,j+1]
      funid<-funs[i,j]
      # fit the functions:
      if (funid==1) { mod <- lm (ydata~xdata)} else
        if (funid==2) mod <- lm (ydata~xdata+I(xdata^2)) else
          if (funid==3) mod <- lm (ydata~xdata+I(xdata^2)+I(xdata^3)) else
            if (funid==7) {mod <- gam (ydata~s(xdata)); funs[i,j]<-7} else  mod <- NA
      models[[j]][i]<-list(mod) # save the model in a list
    }
  }
  rm(mod,i,j,funid,xdata,ydata,WD)
  
  # ===== predict values and store in a matrix
  # Need objects: DD, MDsub, models, funs
  # create matrices of length length(DD$id) by 110 and put into a list
  pred.mdw<-array(NA,c(length(DD$id),400))
  pred.mvol<-array(NA,c(length(DD$id),110))
  pred.mps<-array(NA,c(length(DD$id),110))
  pred.mwhc<-array(NA,c(length(DD$id),110))
  pred.mwplog<-array(NA,c(length(DD$id),110))
  pred<-list(pred.mdw=pred.mdw,pred.mvol=pred.mvol,pred.mps=pred.mps,pred.mwhc=pred.mwhc,pred.mwplog=pred.mwplog)
  rm(pred.mdw,pred.mvol,pred.mps,pred.mwhc,pred.mwplog)
  # get predicted values and store in matrix at the correct positions
  for (i in 1:length(DD$id)) {
    for (j in 1:5) {
      if (funs[i,j] > 0) {	# if there is a function fitted for this dataset then...
        x0<-min(MDsub[MDsub$id==i,j+1],na.rm=TRUE)
        x1<-max(MDsub[MDsub$id==i,j+1],na.rm=TRUE)
        x0<-ceiling(x0*100)/100 # round up at the second decimal
        x1<-ceiling(x1*100)/100
        xv<-seq(x0,x1,0.01) # create a sequence of values used to predict
        yv<-predict(models[[j]][[i]], list(xdata=xv))	# predict using the function fitted on the dataset
        im <- array(c(rep(i,length(xv)),seq(x0*100,x1*100,1)), dim=c(length(xv),2)) # create an index matrix (im) to position values in the matrix.
        pred[[j]][im]<-yv	# assign the predicted values using the index matrix
      }
    }
  }
  rm(i, im, j, x0,x1, xv, yv)
  
  # ===== Calculate the Proportional Change in Soil Respiration (PCSR)
  # Here we take values in the predicted tables at moisture x and divide it by the value at x-0.01
  # giving the change in respiration at each 0.01 range
  prePCSR <- list()
  PCSR <- list()
  for (i in 1:5) {
    A<-pred[[i]]
    B<-cbind(A[,2:ncol(A)],NA)
    C<-(B/A)
    prePCSR[i]<-list(C)
  }
  # average to get the correct PCSR at the given moisture point
  for (i in 1:5) {
    A<-prePCSR[[i]]
    B<-cbind(NA,A[,1:ncol(A)-1])
    C<-(A+B)/2
    PCSR[i]<-list(C)
  }
  PCSRnames <- list("PCSR.mdw","PCSR.mvol","PCSR.mps","PCSR.mwhc","PCSR.mwplog")
  names(PCSR)<-PCSRnames
  rm(PCSRnames,A,B,C,i,prePCSR)
  
  # =========================================================
  # ===== Linear Regressions with Soil Properties
  # Make lists for assigning names
  moistnames <- list("mdw","mvol","mps","mwhc","wplog")
  varnames <- list("bd", "corg", "ps", "clay", "silt", "sand")
  lmnames <-list()
  lst <- 1
  for (j in 1:5) {
    for (i in 1:6) {
      lmnames[lst] <- list(paste("lm", moistnames[[j]],varnames[[i]],sep="."))
      lst <- lst+1
    }
  }
  # Subset some variables to work with
  DDsub <- subset(DD, select=c(id,soil.t:sand,co2.dw,maxtime))
  DDsub[DDsub$c.org>0.05 | is.na(DDsub$c.org),]<-NA # select either mineral or organic soils
  # explore each variable separately: fit a linear regression at points along the moisture range
  lmfunc <- function (A) {if (sum(as.vector(!is.na(A))*as.vector(!is.na(DDsub[,i])))>2) {lm(A~DDsub[,i], na.action=na.exclude)} else return (NA)}
  lmsep<-list()
  lst <- 1
  for (j in 1:5) {
    for (i in 3:8) {
      ind <- seq(1,99,2) # select a subset of moisture points for applying regressions
      PCSR.tmp <- PCSR[[j]][,ind]
      x <- apply(PCSR.tmp, 2, lmfunc)
      lmsep[lst]<- list(x)
      lst <- lst+1
    }
  }
  names(lmsep) <- lmnames
  rm(lmfunc, moistnames,varnames,lmnames, x,i,j,lst,PCSR.tmp,ind)
  
  
  # =========================================================
  #====== Multiple Linear Regression =====
  #====  Model excluding bulk density and organic soils
  
  library(MASS)
  #======= Prepare the data
  lmdata.texorg<-list()
  moist<-rep(1:110,each=107); moist<-moist/100 # the moisture variable: repeat each value 107 times (the amount of datasets)
  DDsub<- subset(DD, select=c(id,ecosystem,c.org,clay,soil.t,maxtime)) # subset variables used
  DDrep<-DDsub
  for (i in 1:109) {DDrep<-rbind(DDrep,DDsub)} # repeat each case 110 time (the amount of moisture points)
  DDrep<-cbind(DDrep,moist)
  rm(i,DDsub,moist)
  for (j in c(1,2,3,4,5)) {
    pcsr<-as.vector(PCSR[[j]][,1:110])
    rm<-mean(pcsr,na.rm=TRUE); rsd<-sd(pcsr,na.rm=TRUE); pcsr[pcsr>rm+3*rsd]<-NA # remove extreme values?
    Data<-DDrep
    Data<-cbind(DDrep,pcsr)
    rm(pcsr)
    # Make a vector (x) that marks all rows where all variables are present, subset accordingly
    x<-rep(1,length(Data[,1]))
    for (i in c(3,4,7,8)) {AA<-as.vector(!is.na(Data[,i])); x<-x*AA}
    # x[Data$ecosystem=="Forest"]<-0 # select ecosystem?
    x[Data$c.org>0.05]<-0 # remove high c.org soils
    Data<-Data[x==1,]
    lmdata.texorg[j]<-list(Data)
  }
  # Fit Linear Models
  lm.moist.texorg<-list()
  lm.texorg<-list()
  for (j in c(1,2,3,4,5)) {
    lmmod<-stepAIC(lm(pcsr~moist+I(moist^2)+I(moist^3), data=lmdata.texorg[[j]]),k=log(length(lmdata.texorg[[j]]$pcsr)),trace=FALSE)
    lm.moist.texorg[j]<-list(lmmod)
    lmmod<-stepAIC(lm(pcsr~moist+I(moist^2)+I(moist^3)+moist*clay+c.org, data=lmdata.texorg[[j]]),k=log(length(lmdata.texorg[[j]]$pcsr)),trace=FALSE)
    lm.texorg[j]<-list(lmmod)
  }
  rm(i,j,AA,x,rsd,rm,Data,lmmod,DDrep)
  
  
  
  
  
  # ====== model and measures used
  model<-lm.texorg
  m1<-1
  m2<-2
  m3<-3
  m4<-5
  # ====== create dataframe for newdata
  
  for (j in c(m2)) {
    # ====== predict and simulate changing corg
    corgmat<-matrix(nrow=dim(z)[1],ncol=60)
    for (k in 1:as.numeric(length(SOM))) {
      #     simdata$ecosystem<-as.factor("Cultivated")
      simdata<-data.frame(moist=seq(0.02,1.2,0.02))
      simdata$c.org<-(SOM[k])/1000 # To convert from g kg-1 to g g-1
      simdata$clay<-clay[k]
      x<-predict(model[[j]], newdata=simdata)
      y<-vector(length=length(x))
      for (i in 1:length(y)){
        if (i==1) {y[i]<-x[i]} else {y[i]<-y[i-1]*x[i]}
      }
      y<-y/max(y)
      corgmat[k,]<-y
      # scale values to start at 0
      if (j!=5) {
        ind<-which.max(corgmat[k,])
        corgmat[k,1:ind]<-(corgmat[k,1:ind]-min(corgmat[k,1:ind]))
        corgmat[k,1:ind]<-corgmat[k,1:ind]/max(corgmat[k,1:ind])
        
      }
      
    }
    return(corgmat)
  }
}
