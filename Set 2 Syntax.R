# Participant 067 (Final version)

Set_2_002 <- read.csv("~/R/Set_2_002.csv")
#View(Set_2_002)

# Filter "fixation" only; delete Saccade, Instruction, and Fixation slide
Set_2_002 <-subset(Set_2_002, Set_2_002$GazeEventType =="Fixation")


# Identify Trackloss (either Category 3 or 4 in either eye)
Set_2_002$trackloss <- ifelse(Set_2_002$ValidityLeft >= 3 | Set_2_002$ValidityRight >= 3, 1, 0)


# Delete Trackloss = 1 (include trackloss=0 only)
Set_2_002 <-subset(Set_2_002, Set_2_002$trackloss =="0")


#Bring the first 4 digits from MediaName variable (e.g., Ap-Av, Av-Ap only)
Set_2_002$Category = substr(Set_2_002$MediaName,1,5) 
#View(Set_2_002$Category)

#Save Ap-Av and Av-Ap slides only (all instruction, fixation slides, blanks are deleted)
Set_2_002 = subset(Set_2_002,Set_2_002$Category == "Ap-Av"|Set_2_002$Category == "Av-Ap") 
#View(Set_2_002)

#Move data on all 20 Approach AOI columns(11th, 13th, 15th..) into one column; same applies to Avd AOI columns too. 
Set_2_002$App = as.vector(apply(Set_2_002[,c(11,13,15,17,18,21,22,25,26,29,30,33,34,37,38,41,42,44,46,48)],1,sum,na.rm=TRUE))
Set_2_002$Avd = as.vector(apply(Set_2_002[,c(10,12,14,16,19,20,23,24,27,28,31,32,35,36,39,40,43,45,47,49)],1,sum,na.rm=TRUE))

#View(Set_2_002)
#View(Set_2_002$App)
#View(Set_2_002$Avd)

#Plot

Prob_App=NULL
Count_App=NULL
Prob_Avd=NULL
Count_Avd=NULL
Prob_Neither=NULL
Count_Neither=NULL

TempCount_App = 0  
TempCount_Avd = 0  
TempCount_Neither = 0  

i = 1 
for (t in 1:(dim(Set_2_002)[1]-1)) # dim(Set_2_002)[1] = 30067 and minus 1 = 30067; in order to run the loop by 30067th. Then, else will take care of 30067th data.
{
  if (Set_2_002$MediaName[t] == Set_2_002$MediaName[t+1] ) # this means that "if the image on the first column is same as that of the second column (i.e., participants see the same image), run this roop. If participants see different image set, stop counting.
  {
    TempCount_App=TempCount_App+Set_2_002$App[t]
    TempCount_Avd=TempCount_Avd+Set_2_002$Avd[t]
    TempCount_Neither=TempCount_Neither+(Set_2_002$App[t]-1)*(Set_2_002$Avd[t]-1) # See neither happy nor neu: (0, 0); this formula becomes 1 only when Happy is 0 and Neu is 0 
  } 
  else 
  {
    TempCount_App=TempCount_App+Set_2_002$App[t]
    TempCount_Avd=TempCount_Avd+Set_2_002$Avd[t]
    TempCount_Neither=TempCount_Neither+(Set_2_002$App[t]-1)*(Set_2_002$Avd[t]-1) 
    
    Count_App[i]= TempCount_App
    Count_Avd[i]= TempCount_Avd
    Count_Neither[i]= TempCount_Neither
    
    Prob_App[i] = TempCount_App/(TempCount_App+TempCount_Avd+TempCount_Neither)
    Prob_Avd[i] = TempCount_Avd/(TempCount_App+TempCount_Avd+TempCount_Neither)
    Prob_Neither[i] = TempCount_Neither/(TempCount_App+TempCount_Avd+TempCount_Neither)
    
    i=i+1
    
    TempCount_App = 0  
    TempCount_Avd = 0  
    TempCount_Neither = 0  
  }
}

TempCount_App=TempCount_App+Set_2_002$App[t]
TempCount_Avd=TempCount_Avd+Set_2_002$Avd[t]
TempCount_Neither=TempCount_Neither+(Set_2_002$App[t]-1)*(Set_2_002$Avd[t]-1) 

Count_App[i]= TempCount_App
Count_Avd[i]= TempCount_Avd
Count_Neither[i]= TempCount_Neither

Prob_App[i] = TempCount_App/(TempCount_App+TempCount_Avd+TempCount_Neither)
Prob_Avd[i] = TempCount_Avd/(TempCount_App+TempCount_Avd+TempCount_Neither)
Prob_Neither[i] = TempCount_Neither/(TempCount_App+TempCount_Avd+TempCount_Neither)


# Probability to Percentage

Perc_App = Prob_App*100 
Perc_Avd = Prob_Avd*100
Perc_Neither = Prob_Neither*100


# Draw a plot (l is alphabet, not number)

png(file="Set_2_002.png")

plot(Perc_App,type="b", col="brown2",pch=20,lwd=1,main="% of Visual Attention (Part 2)", sub="P#067",
     xlab="Image Set Over Time", ylab="% of visual attention", xlim=c(1, 20), ylim=c(0,100)) 
lines(Perc_Avd,type="b", col="aquamarine4", pch=20,lwd=1)
lines(Perc_Neither,type="b",col="gray", pch=20,lwd=1)

legend("topleft", 
       legend = c("Approach", "Avoidance", "Neither"), 
       col = c("brown2", "aquamarine4", "gray"),
       pch = c(20,20,20),
       pt.cex =1.5,
       cex = 0.8, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.03, 0.03))

dev.off ()

# Create and save a matrix

Perc_All_Matrix_067=cbind(Perc_App,Perc_Avd,Perc_Neither)
#View(Perc_All_Matrix_067)
write.csv(Perc_All_Matrix_067, "../R/Perc_Data_Clean/Set2_Clean_Matrix_067.csv")


