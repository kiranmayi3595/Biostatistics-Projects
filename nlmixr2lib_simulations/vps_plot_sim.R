function() {
  description <- "Two compartment PK model with linear clearance using differential equations"
  ini({
    lka <- 0.45
    label("Absorption rate (Ka)")
    lcl <- 1
    label("Clearance (CL)")
    lvc <- 3
    label("Central volume of distribution (V)")
    lvp <- 5
    label("Peripheral volume of distribution (Vp)")
    lq <- 0.1
    label("Intercompartmental clearance (Q)")
    propSd <- c(0, 0.5)
    label("Proportional residual error (fraction)")
    lktr1 <- c(0, 0.05)
    label("First order transition rate (ktr1)")
    lktr2 <- c(0, 0.05)
    label("First order transition rate (ktr2)")
  })
  model({
    ktr1 <- exp(lktr1)
    ktr2 <- exp(lktr2)
    ka <- exp(lka)
    cl <- exp(lcl)
    vc <- exp(lvc)
    vp <- exp(lvp)
    q <- exp(lq)
    kel <- cl/vc
    k12 <- q/vc
    k21 <- q/vp
    d/dt(depot) <- -ka * depot
    d/dt(transit1) <- ka * depot - ktr1 * transit1
    d/dt(transit2) <- ktr1 * transit1 - ktr2 * transit2
    d/dt(central) <- ktr2 * transit2 - kel * central - k12 * 
      central + k21 * peripheral1
    d/dt(peripheral1) <- k12 * central - k21 * peripheral1
    Cc <- central/vc
    Cc ~ prop(propSd)
  })
}

e <- et(amt=25, ii=6, until=24*7*3, time=list(c(0, 1))) %>%
  et(list(
    c(1, 1.5),
    c(1.5, 2),
    c(2, 3),
    c(3, 4),
    c(4, 5),
    c(5, 6),
    c(6, 6.5),
    c(6.5, 7),
    c(7, 8),
    c(8, 9),
    c(9, 10),
    c(10, 11),
    c(11, 12),
    c(12, 18),
    c(18, 24)
  )) %>%
  et(id=1:12)

#wt <- data.frame(id=1:12, WT=72+rnorm(12, sd=12))

#e <- merge(e, wt)
s <- rxSolve(one.compartment,e,addDosing=TRUE)
library(dplyr)

da <- s%>%select(id,evid,cmt,amt,time,sim)%>%rename(dv=sim)
fitS <- nlmixr2(one.compartment,da,"saem")

#p1 <- nlmixr2::vpcPlot(fitS)
#p2 <- p1+ylab("Concentr")
#fitF <-  nlmixr2(one.compartment, theo_sd, "focei")
#fitS <-  nlmixr2(one.compartment, theo_sd, "saem")

vpcPlot(fitS,n=500,show=list(obs_dv=TRUE,
                             obs_median=TRUE,
                             sim_median=TRUE,
                             sim_median_ci=TRUE,
                             obs_ci=TRUE,
                             pi=TRUE
                             ),xlab="Time(hr)",ylab="Concentration (mg/L)")