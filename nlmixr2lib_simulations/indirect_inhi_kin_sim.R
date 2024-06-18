library(nlmixr2)
library(rxode2)
library(ggplot2)
library(tidyverse)
library(xgxr)
library(ggPMX)

f <- function() {
  description <- "One compartment indirect response model with inhibition of kin."
  ini({
    lvc <- log(90)
    label("Central volume of distribution (Vc)")
    lkel <- log(0.3)
    label("Clearance (Cl)")
    lIC50 <- log(100)
    label("Drug concentration producing 50% of maximum inhibition at effect site (IC50)")
    limax <- log(1)
    label("Maximum inhibitory factor attributed to drug (Imax)")
    lkin <- log(9)
    label("Zero-order rate constant for production of drug response(1/d)")
    lkout <- log(0.3)
    label("First-order rate constant for loss of drug response")
    lr0 <- log(30);
  })
  model({
    r0 <- exp(lr0)
    vc <- exp(lvc)
    kel <- exp(lkel)
    IC50 <- exp(lIC50)
    imax <- exp(limax)
    kin <- exp(lkin)
    kout <- exp(lkout)
    
    d/dt(central) <- -(kel) * central
    Cc <- central/vc
    d/dt(effect) <- kin * (1 - imax * Cc/(Cc + IC50)) - kout * 
      effect
    effect(0) <- r0
    
  })
}



e1 <- et(amt=1000)%>%et(0,30,length.out=500)%>%et(id=1)%>%et(0.001)
e2 <- et(amt=10000)%>%et(0,30,length.out=500)%>%et(id=2)%>%et(0.001)
e3 <- et(amt=100000)%>%et(0,30,length.out=500)%>%et(id=3)%>%et(0.001)

e <- rbind(e1,e2,e3)

s <- rxSolve(f,e)
#plot(s,effect,xlab = "Time (hr)",ylab="Response",log="y")+ylim(10,35)+xlim(0,30)+theme_light(base_size = 18)+geom_point(size=2,shape=16,color="black")
plot(s,effect,xlab = "Time (hr)",ylab="Response",log="y")+ylim(10,35)+xlim(0,30)




  
  
  
  
  
  
  
  