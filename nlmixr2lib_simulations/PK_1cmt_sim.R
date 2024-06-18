PK_1cmt_des <- function() {
  description <- "One compartment PK model with linear clearance using differential equations"
  ini({
    #lka <- 0.45 ; label("Absorption rate (Ka)")
    lcl <- 1 ; label("Clearance (CL)")
    lvc  <- 3.45 ; label("Central volume of distribution (V)")
    #lalag <- log (3);label("Lag time (lagD)")
    propSd <- 0.5 ; label("Proportional residual error (fraction)")
  })
  model({
    #ka <- exp(lka)
    cl <- exp(lcl)
    vc  <- exp(lvc)
    #lagD <- exp(lalag)
    
    kel <- cl / vc
    
    #d/dt(depot) <- -ka*depot
    #alag(depot) <- lagD
    d/dt(central) <- -kel*central
    
    Cc <- central / vc
    Cc ~ prop(propSd)
  })
}

e <- et(amt=25) %>%
  et(0,24,length.out = 100)

#wt <- data.frame(id=1:12, WT=72+rnorm(12, sd=12))

#e <- merge(e, wt)
s <- rxSolve(PK_1cmt_des,e)
plot(s,Cc,xlab = "Time (hr)",ylab="Concentration (ng/ml)",log="y")+theme_light(base_size = 18)+geom_line(color="red")+geom_point(size=2,shape=16,color="black")
plot(s$time,s$Cc,log="y",xlab = "Time (hr)",ylab="Concentration (ng/ml)",type="b",pch=16,col=4)%>%grid()
axis(side=1,at=seq(0,24,by=6))
plot(s,Cc,log="y",xlab = "Time (hr)",ylab="Concentration (ng/ml)",color="blue")


