tmdd_2cmt_mm <- function() {
  description <- "Two compartment TMDD model with Michaelis-Menten approximation.Absorption of the ligand is first-order (ka) "
  ini({
    lcl   <- log(0.0009) ; label("Clearance (CL)")
    lvc  <- log(0.05) ; label("Central volume of distribution (Vc)")
    lvm  <- log(0.0146); label("maximum target-mediated rate of elimination (mg/L/d)")
    lkm  <- log(3.68); label("Michaelis-Menten constant (mg/L)")
    lq     <- log(0.00307) ; label("Intercompartmental clearance (Q)")
    lvp    <- log(0.1) ; label("Peripheral volume of distribution (Vp)")
    
  })
  model({
    
    vm  <- exp(lvm)
    km  <- exp(lkm)
    vc  <- exp(lvc)
    cl <- exp(lcl)
    q <- exp(lq)
    vp <- exp(lvp)
    
    kel <- cl/vc
    k12 <- q/vc
    k21 <- q/vp
    
    
    
    d/dt(central)    <- -(vm/(km + central/vc))*central- k12*central + k21*peripheral1 - kel*central
    d/dt(peripheral1)<- k12*central - k21*peripheral1
    
    Cc <-  central/vc
  })
}

e1 <- et(amt=1.5)%>%et(0,500,length.out=100)%>%et(id=1)
e2 <- et(amt=5)%>%et(0,500,length.out=100)%>%et(id=2)
e3 <- et(amt=15)%>%et(0,500,length.out=100)%>%et(id=3)
e4 <- et(amt=45)%>%et(0,500,length.out=100)%>%et(id=4)

e <- rbind(e1,e2,e3,e4)

s <- rxSolve(tmdd_2cmt_mm,e)
plot(s,Cc,xlab = "Time (hr)",ylab="Concentration (mg/L)",log="y")+xlim(0,600)
