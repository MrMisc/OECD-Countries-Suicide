getwd()
setwd("F:/Data/OECD_Countries_Suicide/")


S<-read.csv("DP_LIVE_06082022205006478.csv")


boxplot(Value~LOCATION, data=S)


suicidebycountry<-lm(Value~LOCATION+TIME, data=S)


summary(suicidebycountry)


suicidebycountry_I<-lm(Value~LOCATION*TIME, data=S)

summary(suicidebycountry_I)

#Functions to define at the start

highest<-function(vect_values,N){
  len = length(vect_values)
  return(sort(vect_values, partial = len-(N-1))[len-(N-1)])
}



#Let us facet wrap for each country and see who is increasing in Suicide rate
library(ggplot2); library(gridExtra)
ggplot(S, aes(TIME, Value, color = SUBJECT)) +
  geom_point() +
  geom_smooth(method="lm") +
  facet_wrap(~ LOCATION)


#Verify that we can print out the unique location codes for the dataframe
for(i in unique(S$LOCATION)){
  print(i)
}


#Note this shows how to separate based off of location
for(i in unique(S$LOCATION)){
  print(length(S[S$LOCATION==i,]))
}


#Let's print out all the estimated gradients
for(i in unique(S$LOCATION)){
  df<-S[S$LOCATION == i,]
  thing<-lm(data=df, Value~TIME)
  grad<-thing$coefficients[2]
  print(paste("For country code",i, "the suicide rate has been changing with appx", format(grad, nsmall = 2), "per year", sep=" " ))
}


#Make lists instead to manipulate around
NUM = 0
listofgrads = list()
listofnames = list()
for(i in unique(S$LOCATION)){
  NUM=NUM+1
  df<-S[S$LOCATION == i,]
  thing<-lm(data=df, Value~TIME)
  grad<-thing$coefficients[2]
  #print(paste("For country code",i, "the suicide rate has been changing with appx", format(grad, nsmall = 2), "per year", sep=" " ))
  print(format(grad, nsmall = 2))
  no<-format(grad, nsmall = 2)
  listofgrads[NUM]<-no
  listofnames[NUM]<-i
}

highest<-function(vect_values,N){
  len = length(vect_values)
  return(sort(vect_values, partial = len-(N-1))[len-(N-1)])
}
  
max(unlist(lapply(listofgrads, FUN=max)))
highest(unlist(listofgrads),2)

listofnames[match(highest(unlist(listofgrads),1), listofgrads)]
listofnames[match(highest(unlist(listofgrads),2), listofgrads)]


for (i in range(5)){
  print(i)
}


#Print the 5 highest increasing rates of suicide based off of OECD suicide information
for (i in 1:5){
  print(listofnames[match(highest(unlist(listofgrads),i), listofgrads)])
}

for (i in 1:5){
  print(highest(unlist(listofgrads),i))
}


#Let's look at different sections of KOREA
ggplot(S[S$LOCATION=="KOR",], aes(TIME, Value)) +
  geom_point() +
  geom_smooth(method="lm")


ggplot(S[S$LOCATION=="KOR" & S$TIME > 2000,], aes(TIME, Value)) +
  geom_point() +
  geom_smooth(method="lm")+
  facet_wrap(~ SUBJECT)

#JUST REALISED THAT THERE ARE 3 DATASETS FOR EACH 

ggplot(S[S$LOCATION=="KOR" ,], aes(TIME, Value)) +
  geom_point() +
  geom_smooth(method="lm")+
  facet_wrap(~ SUBJECT)

#after 2005
ggplot(S[S$LOCATION=="KOR"& S$TIME > 2005 ,], aes(TIME, Value)) +
  geom_point() +
  geom_smooth(method="lm")+
  facet_wrap(~ SUBJECT)





#So things are critically different for year 2005 onwards
ggplot(S[S$TIME>2005,], aes(TIME, Value, color = SUBJECT)) +
  geom_point() +
  geom_smooth(method="lm") +
  facet_wrap(~ LOCATION)

#Redo all times to control for sex
ggplot(S, aes(TIME, Value, color = SUBJECT)) +
  geom_point() +
  geom_smooth(method="lm") +
  facet_wrap(~ LOCATION)



#USA lookin sus on increasing. Who is increasing now, if at all?
p2005<-S[S$TIME>2005,]

#Let's print out all the estimated gradients
for(i in unique(p2005$LOCATION)){
  df<-p2005[p2005$LOCATION == i,]
  thing<-lm(data=df, Value~TIME)
  grad<-thing$coefficients[2]
  print(paste("For country code",i, "the suicide rate has been changing with appx", format(grad, nsmall = 2), "per year", sep=" " ))
}



NUM = 0
listof2grads = list()
listof2names = list()
for(i in unique(p2005$LOCATION)){
  NUM=NUM+1
  df<-p2005[p2005$LOCATION == i,]
  thing<-lm(data=df, Value~TIME)
  grad<-thing$coefficients[2]
  #print(paste("For country code",i, "the suicide rate has been changing with appx", format(grad, nsmall = 2), "per year", sep=" " ))
  print(format(grad, nsmall = 2))
  no<-format(grad, nsmall = 2)
  listof2grads[NUM]<-no
  listof2names[NUM]<-i
}

highest<-function(vect_values,N){
  len = length(vect_values)
  return(sort(vect_values, partial = len-(N-1))[len-(N-1)])
}


listof2names[match(highest(unlist(listof2grads),1), listof2grads)]
listof2names[match(highest(unlist(listof2grads),2), listof2grads)]


for (i in 1:12){
  print(listof2names[match(highest(unlist(listof2grads),i), listof2grads)])
  print(highest(unlist(listof2grads),i))
}


#Let us summarise this all into a function that prints out the highest rate suicide increases
#with controls for year of cutoff and number to countries to list
#also perhaps controls for gender

grad<-function(no = 5, year=1000, gender = "TOT"){
  p2005<-S[S$TIME>year & S$SUBJECT == gender,]
  NUM = 0
  listof2grads = c()
  listof2names = c()
  for(i in unique(p2005$LOCATION)){
    NUM=NUM+1
    df<-p2005[p2005$LOCATION == i,]
    thing<-lm(data=df, Value~TIME)
    grad<-thing$coefficients[2]
    #print(paste("For country code",i, "the suicide rate has been changing with appx", format(grad, nsmall = 2), "per year", sep=" " ))
    #print(format(grad, nsmall = 2))
    no<-format(grad, nsmall = 2)
    listof2grads<-c(listof2grads,no)
    listof2names<-c(listof2names,i)
  }
  return (data.frame(listof2grads, listof2names))
}

GRAD<-function(no = 5, year=1000, gender = "TOT"){
  highest<-function(vect_values,N){
    len = length(vect_values)
    return(sort(vect_values, partial = len-(N-1))[len-(N-1)])
  }
  listof2grads<-grad(no, year, gender)[1]
  listof2names<-grad(no, year, gender)[2]
  for (i in 1:no){
    #print(which(listof2grads==highest(unlist(listof2grads),i)))
    print(listof2names[which(listof2grads==highest(unlist(listof2grads),i)),])
    print(highest(unlist(listof2grads),i))
  }
}

GRAD(5,1000,"TOT")
GRAD(5,1000,"WOMEN")
GRAD(5,1000,"MEN")



GRAD(5,2000,"TOT")
GRAD(5,2000,"WOMEN")
GRAD(5,2000,"MEN")






for (i in c(1000,1980,1990,2000,2005,2010)){
  print(i)
  print("------")
  print(GRAD(5,i,"MEN"))
  print("######")
}


for (i in c(1000,1980,1990,2000,2005,2010) ){
  print(i)
  print("------")
  print(GRAD(5,i,"WOMEN"))
  print("######")
}


for (i in c(1000,1980,1990,2000,2005,2010)){
  print(i)
  print("------")
  print(GRAD(5,i,"TOT"))
  print("######")
}


#Copied attempt at world map
library(maps)
library(ggplot2)
world_map <- map_data("world")

p <- ggplot() + coord_fixed() +
  xlab("") + ylab("")

#Add map to base plot
base_world_messy <- p + geom_polygon(data=world_map, aes(x=long, y=lat, group=group), 
                                     colour="light green", fill="light green")

base_world_messy


cleanup <- 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill = 'white', colour = 'white'), 
        axis.line = element_line(colour = "white"), legend.position="none",
        axis.ticks=element_blank(), axis.text.x=element_blank(),
        axis.text.y=element_blank())

base_world <- base_world_messy + cleanup

base_world



S[,c('lat','long')]<-NA



S_backup<-S
#S %>% 



#KOREA>?


ggplot(S[S$LOCATION == 'KOR',], aes(TIME, Value, color = SUBJECT)) +
  geom_point() +
  geom_smooth(method="lm") +
  facet_wrap(~ LOCATION)

#Let us try to print out a facet of say only 4 countries that would be of interest to us

ggplot(S[S$LOCATION == c('USA','AUS'),], aes(TIME, Value, color = SUBJECT)) +
  geom_point() +
  geom_smooth(method="lm") +
  facet_wrap(~ LOCATION)

ggplot(S[S$LOCATION == c('JPN','KOR'),], aes(TIME, Value, color = SUBJECT)) +
  geom_point() +
  geom_smooth(method="lm") +
  facet_wrap(~ LOCATION)


ggplot(S[S$TIME>1995 & S$LOCATION == c('USA','AUS'),], aes(TIME, Value, color = SUBJECT)) +
  geom_point() +
  geom_smooth(method="lm") +
  facet_wrap(~ LOCATION)

ggplot(S[S$TIME>1995 & S$LOCATION == c('JPN','KOR'),], aes(TIME, Value, color = SUBJECT)) +
  geom_point() +
  geom_smooth(method="lm") +
  facet_wrap(~ LOCATION)
