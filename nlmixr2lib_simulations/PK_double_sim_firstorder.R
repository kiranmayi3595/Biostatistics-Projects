PK_double_sim_11 <- function() {
  description <- "PK double absorption model with simultaneous first order absorptions"
  ini({
    lka1 <- 0.45 ; label("First order Absorption rate from first site (Ka)")
    lka2 <- 0.45 ; label("First order Absorption rate from second site (Ka)")
    lcl <- 1 ; label("Clearance (CL)")
    lvc  <- 3 ; label("Central volume of distribution (V)")
    propSd <- 0.5 ; label("Proportional residual error (fraction)")
    lgfdepot1 <- logit(0.7);
    lalag <- log (9); 
  })
  model({
    ka1 <- exp(lka1)
    ka2 <- exp(lka2)
    cl <- exp(lcl)
    vc <- exp(lvc)
    fdepot1 <- expit(lgfdepot1)
    alag <- exp(lalag)
    
    kel <- cl/vc
    
    d/dt(depot1) <- -ka1*depot1
    f(depot1) <- fdepot1
    d/dt(depot2) <- -ka2*depot2
    lag(depot2) <- alag
    f(depot2) <- 1-fdepot1
    d/dt(central) <-  ka1*depot1+ka2*depot2 - kel*central 
    
    Cc <- central / vc
    
    Cc ~ prop(propSd)
  })
}

e <- et(time=c(0,0.17,0.33,0.5,0.75,1,1.5,2,2.5,3,4,6,8,10,12,24))%>% 
  et(amt=100,cmt="depot1") %>% et(amt=100,cmt="depot2")

e <- et(time=c(0,0.17,0.33,0.5,0.75,1,1.5,2,2.5,3,4,6,8,10,12,24))%>% 
  et(amt=100,cmt="depot1") %>% et(amt=100,cmt="depot2")

#wt <- data.frame(id=1:12, WT=72+rnorm(12, sd=12))

#e <- merge(e, wt)
s <- rxSolve(PK_double_sim_11,e)
plot(s,Cc,xlab = "Time (hr)",ylab="Concentration (ng/ml)",log="y")+theme_light(base_size = 18)+geom_line(color="red")+geom_point(size=2,shape=16,color="black")


plot(s$time,s$Cc)