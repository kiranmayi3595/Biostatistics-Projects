df = data.frame("brand" = c("Samsung","Huawei","Apple","Xiaomi","OPPO","Other"),
                "share" = c(.2090,.1580,.1210,.0930,.0860,.3320))

library(ggplot2)
df.raw=read_excel("/Users/kiranmayivedantham/Documents/R_programming/nsf_employment_new.xlsx",sheet = 2, col_names = TRUE, col_types = NULL, na = "", skip = 0)
names(df.raw)
# Create a basic bar
pl = ggplot(df.raw, aes(x=Type, y=ninety_one)) 
pl=pl + geom_bar (width = 1, stat = "identity")
pl<- pl +theme_classic()
pl<-pl + theme(legend.position = "none")
#pl=pl+coord_polar()
pl
install.packages("randomcoloR")
library(randomcoloR)
n=5
palette=distinctColorPalette(n)
#create a pie chart
pl = ggplot(df.raw, aes(x='', y=ninety_one,fill=Type)) 
pl=pl + geom_bar (width = 1, stat = "identity")
pl=pl+geom_text(aes(label = paste0(round(ninety_one), "%")), position = position_stack(vjust = 0.5))
pl<- pl +theme_classic()
pl<-pl + theme(legend.position = "none")
pl=pl+coord_polar("y",start=0)
pl=pl+scale_fill_manual(values=c("#55DDE0", "#33658A", "#2F4858", "#F6AE2D", "#F26419"))
pl=pl+theme(axis.line=element_blank())
pl=pl+theme(axis.text=element_blank())
pl=pl+theme(axis.ticks=element_blank())
pl








