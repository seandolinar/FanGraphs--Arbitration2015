#No extra library load


####FUNCTIONS####################
#previous arbitration flag variable
prev_arb_parse <- function(df){
  
  df$prev_arb <- 0
  for (i in 1:nrow(df)) {
    
    name <- df$player[i]
    data.player <- df[which(df$player == name),]
    print(data.player)
    for (j in 1:nrow(data.player)){
      
      lag_salary <- data.player[which(data.player$year == df$year[i]-1),'salary']
      if (length(lag_salary) > 0){
        
        df$prev_arb[i] <- 1
      }  
    }
  }
  return(df)
}



##############
###Data Load##
##############
mult <- 1 #for cpi

#set your working directory
setwd('~/fg/projects/alex_arbitration/data_merge/') 

#data load -- PITCHERS
data.pitchers.FULL <- read.csv('2011_2015_data_pitchers_v03.csv')
data.pitchers.FULL$raise <- data.pitchers.FULL$salary - data.pitchers.FULL$lag_salary
data.pitchers.FULL$new_cpi <- (data.pitchers.FULL$cpi_deflat-1)*mult+1
data.pitchers.FULL$raise_adj <- data.pitchers.FULL$raise / data.pitchers.FULL$new_cpi
data.pitchers.FULL <- subset(data.pitchers.FULL,subset = data.pitchers.FULL$note != 'remove')
data.pitchers.FULL <- prev_arb_parse(data.pitchers.FULL)
data.pitchers.FULL$SV_sq <- data.pitchers.FULL$SV^2
data.pitchers.FULL$SP <- model.matrix(~data.pitchers.FULL$Pos)[,3]
data.pitchers.FULL$IP_sq <- data.pitchers.FULL$IP^2
data.pitchers.FULL$salary_adj <- data.pitchers.FULL$salary / data.pitchers.FULL$new_cpi


#data load -- HITTERS
data.hitters.FULL <- read.csv('2011_2015_data_hitter_morestats.csv')
data.hitters.FULL$raise <- data.hitters.FULL$salary - data.hitters.FULL$lag_salary
data.hitters.FULL <- subset(data.hitters.FULL,subset = data.hitters.FULL$note != 'remove')
data.hitters.FULL <- prev_arb_parse(data.hitters.FULL)
data.hitters.FULL$new_cpi <- (data.hitters.FULL$cpi_deflat-1)*mult+1
data.hitters.FULL$raise_adj <- data.hitters.FULL$raise / data.hitters.FULL$new_cpi
data.hitters.FULL$salary_adj <- data.hitters.FULL$salary / data.hitters.FULL$new_cpi

##############
#### EDA #####
##############

#salary
hist(data.pitchers.FULL$salary_adj)
hist(data.hitters.FULL$salary_adj)

hist(data.pitchers.FULL$C_WAR)
hist(data.hitters.FULL$C_WAR)

model.eda <- lm((salary_adj) ~ C_WAR, data=data.pitchers.FULL)
hist(model.eda$res)
plot(x=data.pitchers.FULL$C_WAR, model.eda$res)




hist(data.pitchers.FULL$WAR)
hist(data.hitters.FULL$WAR)



################
#### MODELS ####
################

#PITCHERS - SALARY

model.pitchers <- lm(I(log(salary_adj)) ~ C_WAR, data=data.pitchers.FULL)
summary(model.pitchers)

#coefficients
exp(model.pitchers$coefficients)

#residual check
plot(model.pitchers)
hist(model.pitchers$res)
plot(x=data.pitchers.FULL$C_WAR, model.pitchers$res)


newdata <- data.frame(C_WAR = seq(0,20,.01))
exp.df <- data.frame(x=newdata, y=exp(predict(newdata=newdata, model.pitchers)))

plot(x=data.pitchers.FULL$C_WAR, data.pitchers.FULL$salary_adj) #scatter plot
lines(exp.df) #fit line



#PITCHERS - RAISE

#data impute
hist(data.pitchers.FULL$raise_adj)
nrow(data.pitchers.FULL[which(data.pitchers.FULL$raise_adj <= 0),])
data.pitchers.FULL <- data.pitchers.FULL[which(data.pitchers.FULL$raise_adj > 0),]


#model
model.pitchers <- lm(I(log(raise_adj)) ~ WAR, data=data.pitchers.FULL)
summary(model.pitchers)
summary(data.pitchers.FULL$raise_adj)

#coefficients
exp(model.pitchers$coefficients)

#residual check
plot(model.pitchers)
hist(model.pitchers$res)

#plot
newdata <- data.frame(WAR = seq(0,6,.01))
exp.df <- data.frame(x=newdata, y=exp(predict(newdata=newdata, model.pitchers)))

plot(x=data.pitchers.FULL$WAR, data.pitchers.FULL$raise_adj) 
lines(exp.df$WAR, y=exp.df$y)



#HITTERS -- SALARY

model.hitters <- lm(I(log(salary_adj)) ~ C_WAR, data=data.hitters.FULL) #log-linear model for career WAR
summary(model.hitters)

#coefficients
exp(model.hitters$coefficients)

#residual check
plot(model.hitters)
plot(x=data.hitters.FULL$C_WAR, model.hitters$res)

#quick plot
newdata <- data.frame(C_WAR = seq(0,20,.01))
exp.df <- data.frame(x=newdata, y=exp(predict(newdata=newdata, model.hitters)))

plot(x=data.hitters.FULL$C_WAR, data.hitters.FULL$salary_adj) #scatter plot
lines(exp.df) #fit line


#HITTERS -- RAISE

#data impute
hist(data.hitters.FULL$raise_adj)
nrow(data.hitters.FULL[which(data.hitters.FULL$raise_adj <= 0),])
data.hitters.FULL <- data.hitters.FULL[which(data.hitters.FULL$raise_adj > 0),]

#model
model.pitchers <- lm(I(log(raise_adj)) ~ WAR, data=data.hitters.FULL)
summary(model.hitters)
summary(data.hitters.FULL$raise_adj)

#coefficients
exp(model.hitters$coefficients)

#residual check
plot(model.hitters)
hist(model.hitters$res)

#plot
newdata <- data.frame(WAR = seq(0,6,.01))
exp.df <- data.frame(x=newdata, y=exp(predict(newdata=newdata, model.hitters)))

plot(x=data.hitters.FULL$WAR, data.hitters.FULL$raise_adj) 
lines(exp.df$WAR, y=exp.df$y)





#######################
####PITCHERS BY POS####
#######################


#data transform
hist(data.pitchers.FULL$raise_adj)
data.RP <- data.pitchers.FULL[which(data.pitchers.FULL$Pos == 'RP'),]
data.SP <- data.pitchers.FULL[which(data.pitchers.FULL$Pos == 'SP'),]

#RAISE

#models
model.RP <- lm(I(log(raise_adj)) ~ WAR, data=data.RP)
summary(model.RP)

model.SP <- lm(I(log(raise_adj)) ~ WAR, data=data.SP)
summary(model.SP)
plot(model.SP)
hist(data.SP$raise_adj)

#coefficients
exp(model.RP$coefficients)
exp(model.SP$coefficients)

####PLOTS####
#RP
newdata <- data.frame(WAR = seq(0,6,.01))
exp.df <- data.frame(x=newdata, y=exp(predict(newdata=newdata, model.RP)))

plot(x=data.RP$WAR, data.RP$raise_adj) 
lines(exp.df$WAR, y=exp.df$y)

#SP
newdata <- data.frame(WAR = seq(0,6,.01))
exp.df <- data.frame(x=newdata, y=exp(predict(newdata=newdata, model.SP)))

plot(x=data.SP$WAR, data.SP$raise_adj) 
lines(exp.df$WAR, y=exp.df$y)





#SALARY

#models
model.RP <- lm(I(log(salary_adj)) ~ C_WAR, data=data.RP)
summary(model.RP)
plot(model.RP)

model.SP <- lm(I(log(salary_adj)) ~ C_WAR, data=data.SP)
summary(model.SP)
plot(model.SP)


#coefficients
exp(model.RP$coefficients)
exp(model.SP$coefficients)


####PLOTS####
#RP
newdata <- data.frame(C_WAR = seq(0,12,.01))
exp.df <- data.frame(x=newdata, y=exp(predict(newdata=newdata, model.RP)))

plot(x=data.RP$C_WAR, data.RP$salary_adj) 
lines(exp.df$C_WAR, y=exp.df$y)

#SP
newdata <- data.frame(C_WAR = seq(0,25,.01))
exp.df <- data.frame(x=newdata, y=exp(predict(newdata=newdata, model.SP)))

plot(x=data.SP$C_WAR, data.SP$salary_adj) 
lines(exp.df$C_WAR, y=exp.df$y)
