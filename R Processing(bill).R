setwd("C:/Users/willi/Desktop/Senior Design 2")
library(chron)
library(sqldf)
EMS = read.csv("EMS.csv")
STR = read.csv("bilHos.csv")

#for some reason the id column wasnt attaching to my STR file so I just made a new one and Cbound it
X = data.frame()
for (i in 1:length(STR$study_id)) {
  X = rbind(X, c(i))
}
names(X)<-c("X")
STR<-cbind(X,STR)

library(plyr)
EMS$id = EMS$c_id
EMS = rename(EMS,c('c_id'='tree'))
EMS = rename(EMS,c('id'='c_id'))
EMS = rename(EMS,c('tree'='id'))
EMS$id = as.character(EMS$id)
for (i in 1:nrow(EMS)) {
  EMS$id[i] = i
}
#these are the defiltered versions
EMS2 = EMS#[EMS$INCIDENT_TYPE == "STROKE VICTIM", ]
STR2 = STR
hosAmb = STR
hosAmb = data.frame(id = hosAmb$X, study_id = hosAmb$study_id, principaldiagnosis = hosAmb$jc_principaldiagnosis,arrdatetime = hosAmb$jc_arrdatetime, admitdate = hosAmb$jc_admitdate,dob = hosAmb$jc_dob,age =hosAmb$gs_age,gender = hosAmb$jc_gender,ethnic = hosAmb$sp_ethnic,race = hosAmb$gs_race,zip1 = hosAmb$gs_zip1)
amb = EMS2
dis.date = as.character(amb$DISPATCHED) 
dis.time = as.character(amb$DISPATCHED)
at.date = as.character(amb$AT.HOSPITAL) 
at.time = as.character(amb$AT.HOSPITAL) 
for (h in 1:nrow(amb)) {
  dis.date[h] = substr(dis.date[h],1,regexpr(' ',dis.date[h])[1]-1)
  dis.time[h] = substr(dis.time[h],regexpr(' ',dis.time[h])[1]+1,nchar(dis.time[h]))
  
  at.date[h] = substr(at.date[h],1,regexpr(' ',at.date[h])[1]-1)
  at.time[h] = substr(at.time[h],regexpr(' ',at.time[h])[1]+1,nchar(at.time[h]))
}
m = matrix(c(dis.date,dis.time),ncol=2)
car = chron(m[,1], paste(m[,2], 0, sep = ":")) 
amb$DISPATCHED = car

m = matrix(c(at.date,at.time),ncol=2)
car = chron(m[,1], paste(m[,2], 0, sep = ":")) 
amb$AT.HOSPITAL = car

#hos = data.frame(id = STR2$X, study_id = STR2$study_id, principaldiagnosis = STR2$jc_principaldiagnosis,arrdatetime = STR2$jc_arrdatetime, arrdate= STR2$jc_arrdatetime, arrtime = STR2$jc_arrdatetime, admitdate = STR2$jc_admitdate,dob = STR2$jc_dob,age = STR2$gs_age,gender = STR2$jc_gender,ethnic = STR2$sp_ethnic,race = STR2$gs_race,zip1 = STR2$gs_zip1)
hos = data.frame(id = STR2$X, study_id = STR2$study_id, principaldiagnosis = STR2$jc_principaldiagnosis,arrdatetime = STR2$jc_arrdatetime, admitdate = STR2$jc_admitdate,dob = STR2$jc_dob,age = STR2$gs_age,gender = STR2$jc_gender,ethnic = STR2$sp_ethnic,race = STR2$gs_race,zip1 = STR2$gs_zip1)
hos$gender = as.character(hos$gender)
hos$study_id = as.character(hos$study_id)
hos$dob = as.character(hos$dob)
hos$arrdatetime = as.character(hos$arrdatetime)
hos$admitdate = as.Date(hos$arrdate,format = "%m/%d/%Y")
hos$dob = as.Date(hos$arrdate,format = "%m/%d/%Y")

arrdate = as.character(STR2$jc_arrdatetime) 
arrtime = as.character(STR2$jc_arrdatetime)

#I changed most of these in excel
for (g in 1:length(hos$gender)) {
#  if (hos$gender[g] == "Male"){
#    hos$gender[g] <- "M"
#  }
#  else if (hos$gender[g] =="Female"){
#    hos$gender[g] <- "F"
#  }
#  hos$study_id[g] = substr(hos$study_id[g], 10, 10)
#  
  arrdate[g] = substr(arrdate[g],1,regexpr(' ',arrdate[g])[1]-1)
  arrtime[g] = substr(arrtime[g],regexpr(' ',arrtime[g])[1]+1,nchar(arrtime[g]))
  hos$id[g] = g
}

m = matrix(c(arrdate,arrtime),ncol=2)
car = chron(m[,1], paste(m[,2], 0, sep = ":")) 
hos$arrdatetime = car

write.csv(hos, file = "Hos.csv", row.names = FALSE)
write.csv(amb, file = "Amb.csv", row.names = FALSE)

#test = EMS2[EMS2$AGE == 88,]
#dog = STR2[STR2$gs_age == 88,]
#dog = car[car$jc_gender == "Male",]


######################################

match = DataFrame{hos.id = integer(), amb.id = character(), hos.arrival = chron(), amb.athos = chron(),hos.age = integer(), amb.age = integer(), hos.sex = character(), amb.sex = character())
for (i in 1:nrow(hos)) {
  x = hos$study_id[i]
  y = amb[amb$HOSPITAL == x, ]
  
  x = hos$gender[i]
  y = y[y$GENDER == x, ]
  #this incorporates fat finger error on didgits not just a range
  x = hos$age[i]
  v = [x]
  for (f in 1:3){
    v.append(x+f)
  }
  v = paste("c(", v,")", sep ="")
  v = eval(parse(text = v))
  y = subset(y, AGE %in% v)
  #this includes an extra comparison that check whether the ambulance arrived before the patient was recieved
  if (nrow(y) > 0) {
  for (j in 1:nrow(y)) {
    if (!is.na(hos$arrdatetime[i]) & !is.na(y$AT.HOSPITAL[j]) & abs(hos$arrdatetime[i] - y$AT.HOSPITAL[j]) <= 1/48 ){#} & hos$arrdatetime[i]>y$AT.HOSPITAL[j]) {
      a = data.frame(hos.id = hos$id[i],amb.id = y$id[j],hos.arrival = hos$arrdatetime[i],amb.athos = y$AT.HOSPITAL[j],hos.age = hos$age[i],amb.age= y$AGE[j],hos.sex = hos$gender[i],amb.sex = y$GENDER[j])
      match = rbind(match,a)
    }
  }
  }
}


write.csv(match, file = "match.csv", row.names = FALSE)
  
###########################################

#this is where i check if the hospital recorded them arriving in an ambulance
names(hosAmb)[1] <- "id"
names(match)[1]<-"hos_id"
check = sqldf("SELECT * FROM match INNER JOIN hosAmb ON match.hos_id = hosAmb.id")

val = data.frame()
for(v in 1:length(match$hos_id)){
  if(match$hos_id[v] %in% hosAmb$id){
    val = rbind(val, c(match$hos_id[v],1))
  }
  else{
    val = rbind(val,c(match$hos_id[v],0))
  }
}
names(val)<-c("hos_id","valid")
chkMatch<-cbind(match, val$valid)
write.csv(hosAmb, file = "hosAmb.csv", row.names = FALSE)
write.csv(match, file = "chkMatch.csv", row.names = FALSE)

############################################


hos = rename(hos, c('id' = 'hos.id'))
amb = rename(amb, c('id' = 'amb.id'))

x = sqldf("select * from hos inner join amb")
x$bin = 0

x$arrdatetime = chron(x$arrdatetime)
x$DISPATCHED = chron(x$DISPATCHED)
x$AT.HOSPITAL = chron(x$AT.HOSPITAL)

for (k in 1:nrow(match)) {
  x[which(x$amb.id == match$amb.id[k] & x$hos.id == match$hos.id[k]),'bin'] = 1
}

x$bin = as.factor(x$bin)
x$gender = as.factor(x$gender)
y = x
y$arrdatetime = as.numeric(y$arrdatetime)
y$admitdate = as.numeric(y$admitdate)
y$dob = as.numeric(y$dob)
y$DISPATCHED = as.numeric(y$DISPATCHED)
y$AT.HOSPITAL = as.numeric(y$AT.HOSPITAL)

fit = glm(bin ~ study_id + as.numeric(arrdatetime) + as.numeric(admitdate) + as.numeric(dob) + age + gender+race + as.numeric(DISPATCHED) + as.numeric(AT.HOSPITAL) + AGE + GENDER + HOSPITAL , data = x, family = "binomial")
y$pred = predict(fit, newdata = y, type="response")
