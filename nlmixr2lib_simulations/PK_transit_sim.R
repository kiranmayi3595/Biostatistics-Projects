PK_transit_sim <- function() {
  description <- "One compartment PK model with linear clearance using differential equations"
  ini({
    lka <- 0.02
    label("First order absorption rate (ka)")
    lcl <- 1
    label("Clearance (CL)")
    lvc <- 3.45
    label("Central volume of distribution (V)")
    propSd <- c(0, 0.5)
    label("Proportional residual error (fraction)")
    lktr1 <- c(0, 0.05)
    label("First order transition rate (ktr1)")
    lktr2 <- c(0, 0.05)
    label("First order transition rate (ktr2)")
    lktr3 <- c(0, 0.05)
    label("First order transition rate (ktr3)")
    lfdepot <- 0.04
    label("Bioavailability (F)")
    #lalagD <- 0.09
    label("Lag time (lagD)")
  })
  model({
    ka <- exp(lka)
    fdepot <- exp(lfdepot)
    #lagD <- exp(lalagD)
    ktr1 <- exp(lktr1)
    ktr2 <- exp(lktr2)
    ktr3 <- exp(lktr3)
    ka <- exp(lka)
    cl <- exp(lcl)
    vc <- exp(lvc)
    kel <- cl/vc
    
    d/dt(depot) <- -ka * depot
    f(depot) <- fdepot
    #alag(depot) <- lagD
    d/dt(transit1) <- ka * depot - ktr1 * transit1
    d/dt(transit2) <- ktr1 * transit1 - ktr2 * transit2
    d/dt(transit3) <- ktr2 * transit2 - ktr3 * transit3
    d/dt(central) <- ktr3 * transit3 - kel *central
    
    Cc <- central/vc
    Cc ~ prop(propSd)
  })
}

e <- et(amt=25) %>%
  et(0,24,length.out = 100)

#wt <- data.frame(id=1:12, WT=72+rnorm(12, sd=12))

#e <- merge(e, wt)
s <- rxSolve(PK_transit_sim,e)
plot(s,Cc,xlab = "Time (hr)",ylab="Concentration (ng/ml)",log="y")+theme_light(base_size = 18)+geom_line(color="red")+geom_point(size=2,shape=16,color="black")


plot(s,Cc,log="y")


PK_transit_ten <- function() {
  description <- "One compartment PK model with linear clearance using differential equations"
  ini({
    lka <- 0.45
    label("Absorption rate (Ka)")
    lcl <- 1
    label("Clearance (CL)")
    lvc <- 3.45
    label("Central volume of distribution (V)")
    propSd <- c(0, 0.5)
    label("Proportional residual error (fraction)")
    lktr1 <- c(0, 0.05)
    label("First order transition rate (ktr1)")
    lktr2 <- c(0, 0.05)
    label("First order transition rate (ktr2)")
    lktr3 <- c(0, 0.05)
    label("First order transition rate (ktr3)")
    lktr4 <- c(0, 0.05)
    label("First order transition rate (ktr4)")
    lktr5 <- c(0, 0.05)
    label("First order transition rate (ktr5)")
    lktr6 <- c(0, 0.05)
    label("First order transition rate (ktr6)")
    lktr7 <- c(0, 0.05)
    label("First order transition rate (ktr7)")
    lktr8 <- c(0, 0.05)
    label("First order transition rate (ktr8)")
    lktr9 <- c(0, 0.05)
    label("First order transition rate (ktr9)")
    lktr10 <- c(0, 0.05)
    label("First order transition rate (ktr10)")
  })
  model({
    ktr1 <- exp(lktr1)
    ktr2 <- exp(lktr2)
    ktr3 <- exp(lktr3)
    ktr4 <- exp(lktr4)
    ktr5 <- exp(lktr5)
    ktr6 <- exp(lktr6)
    ktr7 <- exp(lktr7)
    ktr8 <- exp(lktr8)
    ktr9 <- exp(lktr9)
    ktr10 <- exp(lktr10)
    ka <- exp(lka)
    cl <- exp(lcl)
    vc <- exp(lvc)
    kel <- cl/vc
    d/dt(depot) <- -ka * depot
    d/dt(transit1) <- ka * depot - ktr1 * transit1
    d/dt(transit2) <- ktr1 * transit1 - ktr2 * transit2
    d/dt(transit3) <- ktr2 * transit2 - ktr3 * transit3
    d/dt(transit4) <- ktr3 * transit3 - ktr4 * transit4
    d/dt(transit5) <- ktr4 * transit4 - ktr5 * transit5
    d/dt(transit6) <- ktr5 * transit5 - ktr6 * transit6
    d/dt(transit7) <- ktr6 * transit6 - ktr7 * transit7
    d/dt(transit8) <- ktr7 * transit7 - ktr8 * transit8
    d/dt(transit9) <- ktr8 * transit8 - ktr9 * transit9
    d/dt(transit10) <- ktr9 * transit9 - ktr10 * transit10
    d/dt(central) <- ktr10 * transit10 - kel * central
    Cc <- central/vc
    Cc ~ prop(propSd)
  })
}


e <- et(amt=25) %>%
  et(0,24,length.out = 100)

#wt <- data.frame(id=1:12, WT=72+rnorm(12, sd=12))

#e <- merge(e, wt)
s <- rxSolve(PK_transit_ten,e)
plot(s,Cc,xlab = "Time (hr)",ylab="Concentration (ng/ml)",log="y")+theme_light(base_size = 18)+geom_line(color="red")+geom_point(size=2,shape=16,color="black")

plot(s,Cc,log="y")
