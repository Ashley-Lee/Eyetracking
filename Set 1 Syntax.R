# Participant 002 (Final version)
Set_1_002 <- read.csv("~/R/Set_1_002.csv")


# Filter "fixation" only; delete Saccade, Instruction, and Fixation slide
Set_1_002 <-subset(Set_1_002, Set_1_002$GazeEventType =="Fixation")
#View(Set_1_002)

# Identify Trackloss (either Category 3 or 4 in either eye)
Set_1_002$trackloss <- ifelse(Set_1_002$ValidityLeft >= 3 | Set_1_002$ValidityRight >= 3, 1, 0)

# Delete Trackloss = 1 (include trackloss=0 only)
Set_1_002 <-subset(Set_1_002, Set_1_002$trackloss =="0")

#Bring the first 4 digits from MediaName variable (e.g., AP-N, N-Ap)
Set_1_002$Category = substr(Set_1_002$MediaName,1,4) 
#View(Set_1_002$Category)

#Save Ap-N, Av-N, N-Ap, and N-Av slides only (all instruction, fixation slides, blanks are deleted)
Set_1_002 = subset(Set_1_002,Set_1_002$Category == "Ap-N"|Set_1_002$Category == "Av-N"|Set_1_002$Category == "N-Ap"|Set_1_002$Category == "N-Av") 
#View(Set_1_002)

#Save into two datasets (App; Avd)
Set_1_002_App = subset(Set_1_002,Set_1_002$Category == "Ap-N"|Set_1_002$Category == "N-Ap")
Set_1_002_Avd = subset(Set_1_002,Set_1_002$Category == "Av-N"|Set_1_002$Category == "N-Av")

#View(Set_1_002_App)
#View(Set_1_002_Avd)

#Move data on all 20 Approach columns(10th, 12th, 14th, 16th...) into one column; same applies to Neu_App columns too. 
Set_1_002_App$App = as.vector(apply(Set_1_002_App[,c(10,12,14,16,20,24,28,32,36,40,45,49,53,57,61,65,69,73,77,81)],1,sum,na.rm=TRUE))
Set_1_002_App$Neu_App= as.vector(apply(Set_1_002_App[,c(11,13,15,17,21,25,29,33,37,41,44,48,52,56,60,64,68,72,76,80)],1,sum,na.rm=TRUE))

#View(Set_1_002_App)
#View(Set_1_002_App$App)
#View(Set_1_002_App$Neu_App)

#Move data on all 20 Avoidance columns(18th, 22th, 26th...) into one column; same applies to Neu_Avd columns too. 
Set_1_002_Avd$Avd = as.vector(apply(Set_1_002_Avd[,c(18,22,26,30,34,38,42,46,50,54,59,63,67,71,75,79,83,85,87,88)],1,sum,na.rm=TRUE))
Set_1_002_Avd$Neu_Avd = as.vector(apply(Set_1_002_Avd[,c(19,23,27,31,35,39,43,47,51,55,58,62,66,70,74,78,82,84,86,89)],1,sum,na.rm=TRUE))

#View(Set_1_002_Avd)
#View(Set_1_002_Avd$Avd)
#View(Set_1_002_Avd$Neu_Avd)


#Plot for App images (both N-App and App-N)

Prob_App=NULL
Count_App=NULL
Prob_Neu_App=NULL
Count_Neu_App=NULL
Prob_Neither_App=NULL
Count_Neither_App=NULL

TempCount_App = 0  
TempCount_Neu = 0  
TempCount_Neither = 0  

i = 1 
for (t in 1:(dim(Set_1_002_App)[1]-1)) # dim(Set_1_002_App)[1] = 30002 and minus 1 = 30002; in order to run the loop by 30002th. Then, else will take care of 30002th data.
{
  if (Set_1_002_App$MediaName[t] == Set_1_002_App$MediaName[t+1] ) # this means that "if the image on the first column is same as that of the second column (i.e., participants see the same image), run this roop. If participants see different image set, stop counting.
  {
    TempCount_App=TempCount_App+Set_1_002_App$App[t]
    TempCount_Neu=TempCount_Neu+Set_1_002_App$Neu_App[t]
    TempCount_Neither=TempCount_Neither+(Set_1_002_App$App[t]-1)*(Set_1_002_App$Neu_App[t]-1) # See neither happy nor neu: (0, 0); this formula becomes 1 only when Happy is 0 and Neu is 0 
  } 
  else 
  {
    TempCount_App=TempCount_App+Set_1_002_App$App[t]
    TempCount_Neu=TempCount_Neu+Set_1_002_App$Neu_App[t]
    TempCount_Neither=TempCount_Neither+(Set_1_002_App$App[t]-1)*(Set_1_002_App$Neu_App[t]-1) 
    
    Count_App[i]= TempCount_App
    Count_Neu_App[i]= TempCount_Neu
    Count_Neither_App[i]= TempCount_Neither
    
    Prob_App[i] = TempCount_App/(TempCount_App+TempCount_Neu+TempCount_Neither)
    Prob_Neu_App[i] = TempCount_Neu/(TempCount_App+TempCount_Neu+TempCount_Neither)
    Prob_Neither_App[i] = TempCount_Neither/(TempCount_App+TempCount_Neu+TempCount_Neither)
    
    i=i+1
    
    TempCount_App = 0  
    TempCount_Neu = 0  
    TempCount_Neither = 0  
  }
}

TempCount_App=TempCount_App+Set_1_002_App$App[t]
TempCount_Neu=TempCount_Neu+Set_1_002_App$Neu_App[t]
TempCount_Neither=TempCount_Neither+(Set_1_002_App$App[t]-1)*(Set_1_002_App$Neu_App[t]-1) 

Count_App[i]= TempCount_App
Count_Neu_App[i]= TempCount_Neu
Count_Neither_App[i]= TempCount_Neither

Prob_App[i] = TempCount_App/(TempCount_App+TempCount_Neu+TempCount_Neither)
Prob_Neu_App[i] = TempCount_Neu/(TempCount_App+TempCount_Neu+TempCount_Neither)
Prob_Neither_App[i] = TempCount_Neither/(TempCount_App+TempCount_Neu+TempCount_Neither)



#Plot for Avd images (both N-Avd and Avd-N)

Prob_Avd=NULL
Count_Avd=NULL
Prob_Neu_Avd=NULL
Count_Neu_Avd=NULL
Prob_Neither_Avd=NULL
Count_Neither_Avd=NULL

TempCount_Avd = 0  
TempCount_Neu = 0  
TempCount_Neither= 0  


i = 1 
for (t in 1:(dim(Set_1_002_Avd)[1]-1)) # dim(Set_1_002_Avd)[1] = 30002 and minus 1, and thus 30002 in order to run the loop
{
  if (Set_1_002_Avd$MediaName[t] == Set_1_002_Avd$MediaName[t+1] )
  {
    TempCount_Avd=TempCount_Avd+Set_1_002_Avd$Avd[t]
    TempCount_Neu=TempCount_Neu+Set_1_002_Avd$Neu_Avd[t]
    TempCount_Neither=TempCount_Neither+(Set_1_002_Avd$Avd[t]-1)*(Set_1_002_Avd$Neu_Avd[t]-1) #it becomes 1 only when Angry is 0 and Neu is 0 
  } 
  else 
  {
    TempCount_Avd=TempCount_Avd+Set_1_002_Avd$Avd[t]
    TempCount_Neu=TempCount_Neu+Set_1_002_Avd$Neu_Avd[t]
    TempCount_Neither=TempCount_Neither+(Set_1_002_Avd$Avd[t]-1)*(Set_1_002_Avd$Neu_Avd[t]-1) #it becomes 1 only when Angry is 0 and Neu is 0 
    
    Count_Avd[i]= TempCount_Avd
    Count_Neu_Avd[i]= TempCount_Neu
    Count_Neither_Avd[i]= TempCount_Neither
    
    Prob_Avd[i] = TempCount_Avd/(TempCount_Avd+TempCount_Neu+TempCount_Neither)
    Prob_Neu_Avd[i] = TempCount_Neu/(TempCount_Avd+TempCount_Neu+TempCount_Neither)
    Prob_Neither_Avd[i] = TempCount_Neither/(TempCount_Avd+TempCount_Neu+TempCount_Neither)
    
    i=i+1
    
    TempCount_Avd = 0  
    TempCount_Neu = 0  
    TempCount_Neither = 0  
  }
}

TempCount_Avd=TempCount_Avd+Set_1_002_Avd$Avd[t]
TempCount_Neu=TempCount_Neu+Set_1_002_Avd$Neu_Avd[t]
TempCount_Neither=TempCount_Neither+(Set_1_002_Avd$Avd[t]-1)*(Set_1_002_Avd$Neu_Avd[t]-1) #it becomes 1 only when Angry is 0 and Neu is 0 

Count_Avd[i]= TempCount_Avd
Count_Neu_Avd[i]= TempCount_Neu
Count_Neither_Avd[i]= TempCount_Neither

Prob_Avd[i] = TempCount_Avd/(TempCount_Avd+TempCount_Neu+TempCount_Neither)
Prob_Neu_Avd[i] = TempCount_Neu/(TempCount_Avd+TempCount_Neu+TempCount_Neither)
Prob_Neither_Avd[i] = TempCount_Neither/(TempCount_Avd+TempCount_Neu+TempCount_Neither)

# Prob to Perc

Perc_App = Prob_App*100 
Perc_Neu_App = Prob_Neu_App*100
Perc_Neither_App = Prob_Neither_App*100

Perc_Avd = Prob_Avd*100 
Perc_Neu_Avd = Prob_Neu_Avd*100
Perc_Neither_Avd = Prob_Neither_Avd*100

# Draw a plot for App images (l is alphabet, not number)

png(file="Set_1_002_App.png")

plot(Perc_App,type="b", col="brown2",pch=20,lwd=1,main="% of Visual Attention on App Images (Part 1)", sub="P#002",
     xlab="App Image Set Over Time", ylab="% of visual attention", xlim=c(1, 20), ylim=c(0,100)) 
lines(Perc_Neu_App,type="b", col="aquamarine4", pch=20,lwd=1)
lines(Perc_Neither_App,type="b",col="gray", pch=20,lwd=1)

legend("topleft", 
       legend = c("Approach", "Neutral", "Neither"), 
       col = c("brown2", "aquamarine4", "gray"),
       pch = c(20,20,20),
       pt.cex =1.5,
       cex = 0.8, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))

dev.off ()


# Draw a plot for Avd images (l is alphabet, not number)

png(file="Set_1_002_Avd.png")

plot(Perc_Avd,type="b", col="brown2",pch=20,lwd=1,main="% of Visual Attention on Avd Images (Part 1)", sub="P#002",
     xlab="Avd Image Set Over Time", ylab="% of visual attention", xlim=c(1, 20), ylim=c(0,100)) 
lines(Perc_Neu_Avd,type="b", col="aquamarine4", pch=20,lwd=1)
lines(Perc_Neither_Avd,type="b",col="gray", pch=20,lwd=1)

legend("topleft", 
       legend = c("Avoidance", "Neutral", "Neither"), 
       col = c("brown2", "aquamarine4", "gray"),
       pch = c(20,20,20),
       pt.cex =1.5,
       cex = 0.8, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))

dev.off ()

# Create and save a matrix

Perc_All_Matrix_002=cbind(Perc_App,Perc_Neu_App,Perc_Neither_App,Perc_Avd,Perc_Neu_Avd,Perc_Neither_Avd)
#View(Perc_All_Matrix_002)
write.csv(Perc_All_Matrix_002, "../R/Perc_Data_Clean/Set_1_Clean_Matrix_002.csv")


