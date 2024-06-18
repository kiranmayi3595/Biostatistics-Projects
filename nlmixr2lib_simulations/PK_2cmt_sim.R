PK_2cmt_des <- function() {
  description <- "Two compartment PK model with linear clearance using differential equations"
  ini({
    #lka <- 0.45 ; label("Absorption rate (Ka)")
    lcl <- 1 ; label("Clearance (CL)")
    lvc  <- 3 ; label("Central volume of distribution (V)")
    lvp  <- 5 ; label("Peripheral volume of distribution (Vp)")
    lq  <- 0.1 ; label("Intercompartmental clearance (Q)")
    propSd <- 0.5 ; label("Proportional residual error (fraction)")
  })
  model({
    #ka <- exp(lka)
    cl <- exp(lcl)
    vc <- exp(lvc)
    vp <- exp(lvp)
    q  <- exp(lq)
    
    kel <- cl/vc
    k12 <- q/vc
    k21 <- q/vp
    
    #d/dt(depot) <- -ka*depot
    d/dt(central) <- - kel*central - k12*central + k21*peripheral1
    d/dt(peripheral1) <- k12*central - k21*peripheral1
    Cc <- central / vc
    
    Cc ~ prop(propSd)
  })
}

e <- et(amt=25) %>%
  et(0,24,length.out = 100)

#wt <- data.frame(id=1:12, WT=72+rnorm(12, sd=12))

#e <- merge(e, wt)
s <- rxSolve(PK_2cmt_des,e)
plot(s,Cc,xlab = "Time (hr)",ylab="Concentration (ng/ml)",log="y")+theme_light(base_size = 18)+geom_line(color="red")+geom_point(size=2,shape=16,color="black")

plot(s$time,s$Cc,log="y",xlab = "Time (hr)",ylab="Concentration (ng/ml)",ylim=c(0.1,1.0),type="b",pch=16,col=4)%>%grid()
axis(side=1,at=seq(0,24,by=6))
axis(side=2,at=seq(0.1,1))
plot(s,Cc,log="y")