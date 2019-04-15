library(dplyr)
library(magrittr)
#produce reference table
pM<-nba_draft_picks%>%
  group_by(X)%>%
  summarize(mean(as.numeric(Advanced.3), na.rm = TRUE))

pD<-nba_draft_picks%>%
  group_by(X)%>%
  summarize(sd(as.numeric(Advanced.3), na.rm = TRUE))

dork<-data.frame(pM, pD)

#initialize script
X=1
#select value from mean/sd table dork
G<-dork[X,]
#produce a vector of model runs
C<-1:1000000
#produce randoms
A<-rnorm(1000000, G$mean, G$sd)
#write dataframe
B<-data.frame(A, C, X)
#set index to 2
X<-X+1

#while script to produce the result as B
while(X<61){
  G<-dork[X,]
  #produce a vector of model runs
  C<-1:1000000
  #produce randoms
  A<-rnorm(1000000, G$mean, G$sd)
  #add row to the already initialized dataset
  B<-add_row(B, A, C, X)
  #move index ahhead
  X<-X+1  
}


#value truncate method
#select all players who megabust
B.1<-B%>%
  filter(A <= -5)
#select all others
B.2<-B%>%
  filter(A > -5)
#if you were worse than -5, your value is now -5
B.1<-B.1%>%
  mutate("A.1" = -5)
#carry through
B.2<-B.2%>%
  mutate("A.1" = A)
#this next step produces a lot of mess, but it's cleanable
B.3<-bind_rows(B.1, B.2)
colnames(B.3)[1]<-"O"
colnames(B.3)[5]<-"A"
G<-B
B<-B.3
#select the rows we actually want
B<-select(B, A, C, X)
B%>%
  group_by(X)%>%
  summarize(mean(A, na.rm=TRUE))

#initialize tank simulator
#t zero will be easily removed if needed
T=0
#IMPORTANT: WHY 999995
#because we need to sample starting at a RANDOM spot out of one-millon runs and then add up to 5
#if this number is any smaller you can end up with a poor model of the draft. 
R<-sample(1:999995, 1)
Q<-sample(1:5, 1, replace=TRUE)
#filter the dataset for value, grab the row for the assigned pic
Q<-B%>%
  filter(C==R)%>%
  slice(Q)
#produce a dataframe with that information, WZ
WZ<-data.frame(Q, T)
#begin model run
T=1

#tank modeler
while(T<1000){
  #select an individual row
  R<-sample(1:999995, 1)
  #assign a high lottery pick
  Q<-sample(1:5, 1, replace=TRUE)
  #filter the dataset for value, grab the row for the assigned pic
  Q<-B%>%
    filter(C==R)%>%
    slice(Q)
  #produce a dataframe with that information
  WX<-data.frame(Q, T)
  #WZ now has the reference line added
  WZ<-bind_rows(WZ,WX)
  #move to the NEXT DRAFT
  R<-R+1
  #select a new lottery pick
  Q<-sample(1:5, 1, replace=TRUE)
  W<-B%>%
    filter(C==R)%>%
    slice(Q)
  WX<-data.frame(W, T)
  WZ<-bind_rows(WZ,WX)
  R<-R+1
  Q<-sample(1:5, 1, replace=TRUE)
  E<-B%>%
    filter(C==R)%>%
    slice(Q)
  WX<-data.frame(E, T)
  WZ<-bind_rows(WZ,WX)
  R<-R+1
  Q<-sample(1:5, 1, replace=TRUE)
  Y<-B%>%
    filter(C==R)%>%
    slice(Q)
  WX<-data.frame(Y, T)
  WZ<-bind_rows(WZ,WX)
  R<-R+1
  Q<-sample(1:5, 1, replace=TRUE)
  U<-B%>%
    filter(C==R)%>%
    slice(Q)
  WX<-data.frame(U, T)
  WZ<-bind_rows(WZ,WX)
  #add to the index
  T<-T+1
}

#initialize draft simulator
#t zero will be easily removed if needed
T=0
R<-sample(1:999995, 1)
Q<-sample(5:25, 1, replace=TRUE)
#filter the dataset for value, grab the row for the assigned pic
Q<-B%>%
  filter(C==R)%>%
  slice(Q)
#produce a dataframe with that information, WZ
WP<-data.frame(Q, T)
#begin model run
T=1

#playoff modeler
while(T<1000){
  #select an individual row
  R<-sample(1:999995, 1)
  #assign a high lottery pick
  Q<-sample(5:25, 1, replace=TRUE)
  #filter the dataset for value, grab the row for the assigned pic
  Q<-B%>%
    filter(C==R)%>%
    slice(Q)
  #produce a dataframe with that information
  WX<-data.frame(Q, T)
  #WZ now has the reference line added
  WP<-bind_rows(WP,WX)
  #move to the NEXT DRAFT
  R<-R+1
  #select a new lottery pick
  Q<-sample(5:25, 1, replace=TRUE)
  W<-B%>%
    filter(C==R)%>%
    slice(Q)
  WX<-data.frame(W, T)
  WP<-bind_rows(WP,WX)
  R<-R+1
  Q<-sample(5:25, 1, replace=TRUE)
  E<-B%>%
    filter(C==R)%>%
    slice(Q)
  WX<-data.frame(E, T)
  WP<-bind_rows(WP,WX)
  R<-R+1
  Q<-sample(5:25, 1, replace=TRUE)
  Y<-B%>%
    filter(C==R)%>%
    slice(Q)
  WX<-data.frame(Y, T)
  WP<-bind_rows(WP,WX)
  R<-R+1
  Q<-sample(5:25, 1, replace=TRUE)
  U<-B%>%
    filter(C==R)%>%
    slice(Q)
  WX<-data.frame(U, T)
  WP<-bind_rows(WP,WX)
  #add to the index
  T<-T+1
}

#initialize draft simulator
#t zero will be easily removed if needed
T=0
R<-sample(1:999995, 1)
Q<-sample(1:25, 1, replace=TRUE)
#filter the dataset for value, grab the row for the assigned pic
Q<-B%>%
  filter(C==R)%>%
  slice(Q)
#produce a dataframe with that information, WZ
WO<-data.frame(Q, T)
#begin model run
T=1


#playoff modeler
while(T<1000){
  #select an individual row
  R<-sample(1:999995, 1)
  #assign a high lottery pick
  Q<-sample(1:25, 1, replace=TRUE)
  #filter the dataset for value, grab the row for the assigned pic
  Q<-B%>%
    filter(C==R)%>%
    slice(Q)
  #produce a dataframe with that information
  WX<-data.frame(Q, T)
  #WZ now has the reference line added
  WO<-bind_rows(WO,WX)
  #move to the NEXT DRAFT
  R<-R+1
  #select a new lottery pick
  Q<-sample(1:25, 1, replace=TRUE)
  W<-B%>%
    filter(C==R)%>%
    slice(Q)
  WX<-data.frame(W, T)
  WO<-bind_rows(WO,WX)
  R<-R+1
  Q<-sample(1:25, 1, replace=TRUE)
  E<-B%>%
    filter(C==R)%>%
    slice(Q)
  WX<-data.frame(E, T)
  WO<-bind_rows(WO,WX)
  R<-R+1
  Q<-sample(1:25, 1, replace=TRUE)
  Y<-B%>%
    filter(C==R)%>%
    slice(Q)
  WX<-data.frame(Y, T)
  WO<-bind_rows(WO,WX)
  R<-R+1
  Q<-sample(1:25, 1, replace=TRUE)
  U<-B%>%
    filter(C==R)%>%
    slice(Q)
  WX<-data.frame(U, T)
  WO<-bind_rows(WO,WX)
  #add to the index
  T<-T+1
}

#ANALYSIS AREA
#WO = random t 1-25
#WP = random t 5:25
#WZ = the tanker 1:5

#analysis - confgure t-test to verfiy no difference between bootstrap and original
testA<-B%>%
  group_by(X)%>%
  summarize(mean(A), sd(A))

#cross check against natural data
testB<-nba_draft_picks%>%
  group_by(X)%>%
  summarize(mean(as.numeric(Advanced.3), na.rm=TRUE),sd(as.numeric(Advanced.3), na.rm=TRUE))

#confirm difference B versus 
t.test(testB$`mean(as.numeric(Advanced.3), na.rm = TRUE)`, testA$`mean(A)`)

#if you don't like reading results
OLU<-t.test(testB$`mean(as.numeric(Advanced.3), na.rm = TRUE)`, testA$`mean(A)`)
#quick test of insig
OLU$p.value>.05
#or you can go bic
OLU$p.value>.5

#group non tankers by model run
WPB<-WP%>%
  group_by(X)%>%
  summarize(mean(A))
View(WPB)

#group non-tankers by model run
WZB<-WZ%>%
  group_by(T)%>%
  summarize(mean(A))
View(WZB)

#group random assigns
WOB<-WO%>%
  group_by(T)%>%
  summarize(mean(A))
View(WOB)

#ttest 
t.test(WZB$`mean(A)`, WPB$`mean(A)`)

#density
plot(density(WPB$`mean(A)`))
plot(density(WZB$`mean(A)`))
plot(density(WOB$`mean(A)`))


sd(WPB$`mean(A)`)
sd(WZB$`mean(A)`)
sd(WOB$`mean(A)`)
mean(WPB$`mean(A)`)
mean(WZB$`mean(A)`)
mean(WOB$`mean(A)`)

#remove all the busts
tankers<-WZ%>%
  filter(A>-15)
players<-WP%>%
  filter(A>-15)
randoms<-WO%>%
  filter(A>-15)

tankers<-tankers%>%
  group_by(T)%>%
  summarize(mean(A))

players<-players%>%
  group_by(T)%>%
  summarize(mean(A))

randoms<-randoms%>%
  group_by(T)%>%
  summarize(mean(A))

tank<-sample_n(tankers, 10) 
play<-sample_n(players, 10)
random<-sample_n(randoms, 10)

tank<-mutate(tank, type="tank")
play<-mutate(play, type="play")
random<-mutate(random, type="rando")
fullset<-bind_rows(tank, play, random)
colnames(fullset)[2]<-"A"
ggplot(fullset, aes(T, A, colour=type))+geom_jitter()

#fiter system adjusted to punish high flops
tankers<-WZ%>%
  filter(A>-30)
players<-WP%>%
  filter(A>-10)
randoms<-WO%>%
  filter(A>-10)

tankers<-tankers%>%
  group_by(T)%>%
  summarize(mean(A))

players<-players%>%
  group_by(T)%>%
  summarize(mean(A))

randoms<-randoms%>%
  group_by(T)%>%
  summarize(mean(A))

tank<-sample_n(tankers, 999) 
play<-sample_n(players, 999)
random<-sample_n(randoms, 999)

tank<-mutate(tank, type="tank")
play<-mutate(play, type="play")
random<-mutate(random, type="rando")
fullset<-bind_rows(tank, play, random)
colnames(fullset)[2]<-"A"
ggplot(fullset, aes(T, A, colour=type))+geom_jitter()

#density plots
Q<-fullset%>%
  filter(type=="tank")
plot(density(Q$A)) 

#density as kurt
library(moments)
kurtosis(Q$A)

#calculate standard deviation
completz<-fullset%>%
  mutate("zscore" = (A-8.29)/7.499)
         