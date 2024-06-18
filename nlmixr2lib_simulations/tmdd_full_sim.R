tmdd_2cmt_full_CLV <- function() {
  description <- "Two compartment TMDD model with full approximation. Parameterized with clearances and volumes."
  ini({
    
    lvc   <- log(0.05)  ; label("Central volume of distribution (Vc)")
    lcl   <- log(0.001) ; label("Clearance (CL)")
    lq     <- log(0.003) ; label("Intercompartmental clearance (Q)")
    lvp    <- log(0.1) ; label("Peripheral volume of distribution (Vp)")
    lksyn <- log(0.11) ; label("Target Synthesis rate (1/d)") 
    lkeDT <- log(0.003) ; label("Internalisation rate (1/d)")
    lkeT <- log(0.0089) ; label("Target Internalisation rate (1/d)")
    lkon  <- log(0.091) ; label("Drug-target association rate (Kon)")
    lkoff  <- log(0.001) ; label("Drug-target dissociation rate (Koff)")
    lT0   <- log(12) ; label("Intial target concentration (T0)")
  
  })
  model({
    
    vc   <- exp(lvc)
    cl   <- exp(lcl)
    q    <- exp(lq)
    vp   <- exp(lvp)
    ksyn <- exp(lksyn)
    keDT <- exp(lkeDT)
    keT <- exp(lkeT)
    kon  <- exp(lkon)
    koff   <- exp(lkoff)
    T0   <- exp(lT0)
  
    
    keD <- cl/vc
    k12 <- q/vc
    k21 <- q/vp
  
    
    Cc <-  central/vc
    target(0) <- T0
    DT(0)<- 0
    d/dt(central)    <-  - k12*central + k21*peripheral1- keD *central  + (- kon*Cc*target + koff*DT)*vc
    d/dt(peripheral1)<- k12*central - k21*peripheral1
    d/dt(target)          <- ksyn- keT *target - kon*Cc*target + koff*DT
    d/dt(DT)         <- -keDT*DT     + kon*Cc*target - koff*DT
    
    
   
  })
}

e1 <- et(amt=1.5)%>%et(0,500,length.out=100)%>%et(id=1)
e2 <- et(amt=5)%>%et(0,500,length.out=100)%>%et(id=2)
e3 <- et(amt=15)%>%et(0,500,length.out=100)%>%et(id=3)
e4 <- et(amt=45)%>%et(0,500,length.out=100)%>%et(id=4)

e <- rbind(e1,e2,e3,e4)

s <- rxSolve(tmdd_2cmt_full_CLV,e)
plot(s,Cc,xlab = "Time (hr)",ylab="Concentration (mg/L)",log="y")+xlim(0,600)