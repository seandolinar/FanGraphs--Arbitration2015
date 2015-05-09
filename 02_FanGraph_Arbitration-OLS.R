#library load
#install.packages('car')
library(car)



####FUNCTION DEFINITIONS###################################

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


#################################################



###############
###Data Load###
###############
mult <- 1

#set your working directory
setwd('/Users/seandolinar/fg/projects/alex_arbitration/R/150312/For_GIT/FanGraphs--Arbitration2015/') 
#data load -- PITCHERS
data.pitchers.FULL <- read.csv('data/2011_2015_data_pitchers.csv')
data.pitchers.FULL$raise <- data.pitchers.FULL$salary - data.pitchers.FULL$lag_salary
data.pitchers.FULL$new_cpi <- (data.pitchers.FULL$cpi_deflat-1)*mult+1
data.pitchers.FULL$raise_adj <- data.pitchers.FULL$raise / data.pitchers.FULL$new_cpi
data.pitchers.FULL <- subset(data.pitchers.FULL,subset = data.pitchers.FULL$note != 'remove')
data.pitchers.FULL <- prev_arb_parse(data.pitchers.FULL)
data.pitchers.FULL$SV_sq <- data.pitchers.FULL$SV^2
data.pitchers.FULL$SP <- model.matrix(~data.pitchers.FULL$Pos)[,3]
data.pitchers.FULL$IP_sq <- data.pitchers.FULL$IP^2
data.pitchers.FULL$salary_adj <- data.pitchers.FULL$salary / data.pitchers.FULL$new_cpi
data.pitchers.FULL$age <- as.numeric(substr(data.pitchers.FULL$Age.Rng,1,2))




#data load -- HITTERS
data.hitters.FULL <- read.csv('data/2011_2015_data_hitter.csv')
data.hitters.FULL$raise <- data.hitters.FULL$salary - data.hitters.FULL$lag_salary
data.hitters.FULL <- subset(data.hitters.FULL,subset = data.hitters.FULL$note != 'remove')
data.hitters.FULL <- prev_arb_parse(data.hitters.FULL)
data.hitters.FULL$new_cpi <- (data.hitters.FULL$cpi_deflat-1)*mult+1
data.hitters.FULL$raise_adj <- data.hitters.FULL$raise / data.hitters.FULL$new_cpi
data.hitters.FULL$salary_adj <- data.hitters.FULL$salary / data.hitters.FULL$new_cpi
data.hitters.FULL$arb_yr <- as.numeric(data.hitters.FULL$arb_yr)



######################
####Pitcher Model####
#####################

#OLS model
model.pitchers <- lm(raise_adj ~ IP + SV + RA9.WAR + arb_yr, data=data.pitchers.FULL) #RA9.WAR is better

print(summary(model.pitchers)) #summary
AIC(model.pitchers) #AIC check
plot(model.pitchers) #residual check
vif(model.pitchers) #multicollinearity check

#y_hat correlation check
cor(predict(object = model.pitchers, newdata=data.pitchers.FULL)*data.pitchers.FULL$new_cpi+data.pitchers.FULL$lag_salary,data.pitchers.FULL$salary)

#y_hat plot and OLS regression check
plot(predict(object = model.pitchers, newdata=data.pitchers.FULL)*data.pitchers.FULL$new_cpi+data.pitchers.FULL$lag_salary,data.pitchers.FULL$salary)
A <- I(predict(object = model.pitchers, newdata=data.pitchers.FULL)*data.pitchers.FULL$new_cpi+data.pitchers.FULL$lag_salary)
summary(lm(data.pitchers.FULL$salary ~ A))
summary(lm(data.pitchers.FULL$salary ~ 0 + A))



model.pitchers.traditional <- lm(raise_adj ~ IP + SV + W + I(IP*ERA) + SO, data=data.pitchers.FULL)
summary(model.pitchers.traditional)
vif(model.pitchers.traditional)

AIC(model.pitchers.traditional)
AIC(model.pitchers)


#####################
####Hitter Model####
#####################

#OLS model
model.hitters <- lm(raise_adj ~ PA + HR + RAR + age, data=data.hitters.FULL) #RAR exceeds WAR


summary(model.hitters)  #summary
AIC(model.hitters)      #AIC check
vif(model.hitters)      #multicollinearity check 
plot(model.hitters)     #residual check
mean(model.hitters$res) #residual check

#y_hat correlation check
cor(predict(object = model.hitters, newdata=data.hitters.FULL)*data.hitters.FULL$new_cpi+data.hitters.FULL$lag_salary,data.hitters.FULL$salary)

#y_hat plot and OLS regression check
plot(predict(object = model.hitters, newdata=data.hitters.FULL)*data.hitters.FULL$new_cpi+data.hitters.FULL$lag_salary,data.hitters.FULL$salary)
A <- I(predict(object = model.hitters, newdata=data.hitters.FULL)*data.hitters.FULL$new_cpi+data.hitters.FULL$lag_salary)
summary(lm(data.hitters.FULL$salary ~ A)) #OLS regression check
summary(lm(data.hitters.FULL$salary ~ 0 + A)) #forced zero OLS regression check


model.hitters.traditional <- lm(raise_adj ~ PA + HR + RBI + I(PA*AVG), data=data.hitters.FULL) #RAR exceeds WAR
summary(model.hitters.traditional)

AIC(model.hitters)
AIC(model.hitters.traditional)



#data out all


data.out <- rbind(cbind(data.hitters.FULL[,c('salary','lag_salary','cpi_deflat')], y_hat = predict(object = model.hitters, newdata=data.hitters.FULL)*data.hitters.FULL$new_cpi+data.hitters.FULL$lag_salary),
cbind(data.pitchers.FULL[,c('salary','lag_salary','cpi_deflat')], y_hat = predict(object = model.pitchers, newdata=data.pitchers.FULL)*data.pitchers.FULL$new_cpi+data.pitchers.FULL$lag_salary))

write.csv(data.out, file='OLS_y_y_hat.csv')

