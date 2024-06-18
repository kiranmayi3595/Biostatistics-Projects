one.compartment <- function() {
  ini({
    tka <- log(1.57); label("Ka")
    tcl <- log(5); label("Cl")
    tv <- log(31.5); label("V")
    add.sd <- 0.7
  })
  # and a model block with the error specification and model specification
  model({
    ka <- exp(tka )
    cl <- exp(tcl)
    v <- exp(tv)
    d/dt(depot) <- -ka * depot
    d/dt(center) <- ka * depot - cl / v * center
    Cc <- center / v
    Cc ~ add(add.sd)
  })
}

e <- et(amt=25) %>%
  et(0,24,length.out = 100)

#wt <- data.frame(id=1:12, WT=72+rnorm(12, sd=12))

#e <- merge(e, wt)
s <- rxSolve(one.compartment,e)
plot(s,Cc)
plot(s,Cc,log="y")

library(dplyr)

da <- s%>%select(id,evid,cmt,amt,time,sim)%>%rename(dv=sim)
fitS <- nlmixr2(one.compartment,da,"saem")

#p1 <- nlmixr2::vpcPlot(fitS)
#p2 <- p1+ylab("Concentr")
#fitF <-  nlmixr2(one.compartment, theo_sd, "focei")
#fitS <-  nlmixr2(one.compartment, e, "saem")

#vpcPlot(fitS,n=500,xlab="Time(hr)",ylab="Concentration (mg/L)")
vpcPlot(fitS,xlab="Time(hr)",ylab="Concentration (mg/L)")
