tmdd_1cmt_full_CLV <- function() {
  description <- "One compartment TMDD model with full approximation. Parameterized with Clearance and volume. "
  ini({
    
    lvc   <- 0.45  ; label("Central volume of distribution (Vc)")
    lcl   <- 0.04 ; label("Clearance (CL)")
    lksyn <- 0.1 ; label("Target Synthesis rate (1/d)") 
    lkeDT <- 0.4 ; label("Internalisation rate (1/d)")
    lkon  <- 0.37 ; label("Drug-target association rate (Kon)")
    lkd   <- 0.65 ; label("Dissociation constant (Kd) ")
    lT0   <- 0.36 ; label("Intial target concentration (T0)")
    lfdepot<- 0.4; label("Bioavailability (F)")
    propSd <- 0.5 ; label("Proportional residual error (frcentraltion)")
  })
  model({
    
    vc     <- exp(lvc)
    cl     <- exp(lcl)
    ksyn   <- exp(lksyn)
    keDT   <- exp(lkeDT)
    kon    <- exp(lkon)
    kd     <- exp(lkd)
    T0     <- exp(lT0)
    
    
    koff <- kd*kon 
    keT  <- ksyn/T0
    keD  <- cl/vc
    
    
    d/dt(central)    <- - keD *central  + (- kon*D*T + koff*DT)*vc
    d/dt(T)          <- ksyn- keT *T - kon*D*T + koff*DT
    d/dt(DT)         <- -keDT*DT     + kon*D*T - koff*DT
    
    Cc <-  central/Vc
    Cc ~ prop(propSd)
  })
}



e1 <- et(amt=1000)%>%et(0,30,length.out=500)%>%et(id=1)%>%et(0.001)
e2 <- et(amt=10000)%>%et(0,30,length.out=500)%>%et(id=2)%>%et(0.001)
e3 <- et(amt=100000)%>%et(0,30,length.out=500)%>%et(id=3)%>%et(0.001)

e <- rbind(e1,e2,e3)

s <- rxSolve(indirect_1cpt_inhi_kout_CLV,e)
#plot(s,effect,xlab = "Time (hr)",ylab="Response",log="y")+ylim(10,35)+xlim(0,30)+theme_light(base_size = 18)+geom_point(size=2,shape=16,color="black")
plot(s,effect,xlab = "Time (hr)",ylab="Response",log="y")+ylim(10,60)+xlim(0,30)
