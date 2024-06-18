library(readxl)
nsf_commit<-read_excel("/Users/kiranmayivedantham/Documents/R_programming/nsf_commitments.xlsx")
glimpse(nsf_commit)


nsf_commit<-nsf_commit%>%
  separate(Commitment_year,into=c("commitment","year"))

glimpse(nsf_commit)
nsf_commit$year=as.integer(nsf_commit$year)



##NSF_EMPLOYED_pecentage of graduated phd students employed#
#Filter data by type of commitment: employment#
nsf_commit_employed<-nsf_commit%>%
  filter(commitment=="employed")

#Drop columns 3 and 4 from new filtered dataset#
nsf_commit_employed_new<-nsf_commit_employed[-c(3:4)]
glimpse(nsf_commit_employed_new)

#Combine all columns with percentages to be able to plot different fields in the same graph#
nsf_commit_employed_new=nsf_commit_employed_new %>% pivot_longer(cols=c('Life sciences', 'Physical sciences and earth sciences',
                                                'Mathematics and computer sciences','Psychology and social sciences','Engineering'),
                    names_to='Field',
                    values_to='Pecentage')

#Plot the Percentage of employed graduated PhD students as a function of year#
nsf_commit_employed_new_plot<-nsf_commit_employed_new%>%
  filter(Field=="Life sciences"|Field=="Physical sciences and earth sciences"|Field=="Mathematics and computer sciences"|Field=="Engineering")%>%
  ggplot(aes(x=year,y=Pecentage,color=Field))+
  geom_line(size=1)+geom_point(size=1)+
  labs(x="Year",y="Percentage(%)",title="Percentage of employed PhD students across fields",caption="Source:NSF survey of earned doctorates")
ggsave("Percentage of employed PhD students across fields.png",nsf_commit_employed_new_plot,width=6,height=4,dpi=600)

##NSF_postdoc_pecentage of graduated phd students going for postdoc #

#Filter data by type of commitment: postdoc#
nsf_commit_postdoc<-nsf_commit%>%
  filter(commitment=="postdoc")

#Drop columns 3 and 4 from new filtered dataset#
nsf_commit_postdoc<-nsf_commit_postdoc[-c(3,4)]
glimpse(nsf_commit_postdoc)

#Combine all columns with percentages to be able to plot different fields in the same graph#
nsf_commit_postdoc_new<-nsf_commit_postdoc%>%
  pivot_longer(cols=c('Life sciences', 'Physical sciences and earth sciences',
                      'Mathematics and computer sciences','Psychology and social sciences','Engineering'),
               names_to='Field',
               values_to='Pecentage')

#Plot the Percentage of graduated PhD student going for postdoc as a function of year#
nsf_commit_postdoc_new_plot<-nsf_commit_postdoc_new%>%
  filter(Field=="Life sciences"|Field=="Physical sciences and earth sciences"|Field=="Mathematics and computer sciences"|Field=="Engineering")%>%
  ggplot(aes(x=year,y=Pecentage,color=Field))+
  geom_line(size=1)+geom_point(size=1)+
  labs(x="Year",y="Percentage(%)",title="Percentage of PhD students going for postdoc across fields",caption="Source:NSF survey of earned doctorates")

ggsave("Percentage of graduated PhD students going for postdoc across fields.png",nsf_commit_postdoc_new_plot,width=6,height=4,dpi=600)


























nsf_employed_2021_plot<-nsf_commit_employed_new%>%
  filter(year==2021)%>%
  ggplot(aes(x=Field,y=Pecentage,fill=Field))+
  geom_bar(stat="identity")+labs(x="Field of study",y="Percentage (%)",title="Percentage of graduated PhD students choosing employment(%) in 2021",caption="NSF:Survey of earned doctorates")
nsf_employed_2021_plot
nsf_postdoc_2021_plot<-nsf_commit_postdoc_new%>%
  filter(year==2021)%>%
  ggplot(aes(x=Field,y=Pecentage,fill=Field))+
  geom_bar(stat="identity")+labs(x="Field of study",y="Percentage (%)",title="Percentage of graduated PhD students choosing postdoc(%) in 2021",caption="NSF:Survey of earned doctorates")

ggsave("Percentage of employed PhD_2021.png",nsf_employed_2021_plot, width =6, height =4, dpi=600)
ggsave("Percentage of postdoc PhD_2021.png",nsf_postdoc_2021_plot, width =6, height =4, dpi=600)


