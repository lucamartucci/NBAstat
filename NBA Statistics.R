library(caret)
#import team stat 22-23
ts2122 <- read.csv("ts2122.R")
attach(ts2122)
#delete unuseful data
ts2122$X.1 <- NULL
ts2122$X <- NULL
attach(ts2122)
View(ts2122)


#EXPLORATION
plot(PACE, WIN., xlab="PACE", ylab="WIN")
plot(FGA, WIN., xlab="FGA", ylab="WIN")
plot(X3PA, WIN., xlab="X3pa", ylab="WIN")
plot(SOS, WIN., xlab="SOS", ylab="WIN", ) #relevant
plot(eDIFF, WIN., xlab="eDIFF", ylab="WIN") #relevant
plot(X3P., WIN., xlab="3p%", ylab="WIN")
plot(X2P., WIN., xlab="2p%", ylab="WIN")
plot(X3PA, WIN., xlab="3pA", ylab="WIN") 
plot(X2PA, WIN., xlab="2pA", ylab="WIN") 
plot(AST, WIN., xlab="AST", ylab="WIN")
plot(TOV, WIN., xlab="TOV", ylab="WIN")
plot(ORB, WIN., xlab="ORB", ylab="WIN")
plot(DRB, WIN., xlab="DRB", ylab="WIN")
plot(FTA, WIN., xlab="DRB", ylab="WIN")
plot(DRB + ORB, WIN., xlab="RB", ylab="WIN")
plot(STL, WIN., xlab="STL", ylab="WIN")
plot(oEFF, WIN., xlab="oEFF", ylab="WIN") #relevant
plot(dEFF, WIN., xlab="dEFF", ylab="WIN") #relevant

plot(SOS, WIN., xlab = "SOS", ylab = "WIN", col = "blue", pch = 16, 
     main = "Scatter plot of SOS vs WIN")
legend("topright", legend = "Data Points", col = "blue", pch = 16)

plot(eDIFF, WIN., xlab = "eDIFF", ylab = "WIN", col = "purple", pch = 16, 
     main = "Scatter plot of eDIFF vs WIN")
legend("topright", legend = "Data Points", col = "purple", pch = 16)

cor(oEFF,WIN.) #check OK
cor(dEFF,WIN.) #check OK
cor(X3P.,WIN.)
cor(X2P.,WIN.)
cor(SOS, WIN.) #check OK
cor(eDIFF, WIN.) #check OK

#REGRESSIONS
modeltm = lm(WIN. ~ SOS + oEFF + dEFF + PACE, data=ts2122)
summary(modeltm) #oEFF and dEFF seem relevant
restm = residuals(modeltm)
plot(restm, col = ifelse(restm > 0, "blue", "red"), pch = 20, xlab = "Index", ylab = "Residuals")
legend("topright", legend = c("Positive Residuals", "Negative Residuals"), col = c("blue", "red"), pch = 20)
summary(restm) #mean OK
par(mfrow = c(1, 2)) # To display multiple plots in one row
qqnorm(restm, col = "blue", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")
qqline(restm, col = "red")
hist(restm, col = "lightblue", xlab = "Residuals", main = "Residuals Histogram")
print(shapiro.test(restm))

modelp = lm(WIN. ~ X2P. + X3P.,  data=ts2122)
summary(modelp)
resp = residuals(modelp)
summary(resp) #man OK
plot(resp) #homoscedasticity OK
hist(resp, col = "lightblue", xlab = "Residuals", main = "Residuals Histogram")
mean_res <- mean(resp)
sd_res <- sd(resp)
x <- seq(min(resp), max(resp), length.out = 100)
y <- dnorm(x, mean = mean_res, sd = sd_res) * length(resp) * diff(range(resp)) / length(x)
y <- y * ymax / max(y)
lines(x, y, col = "red", lwd = 2)

modeltpg =lm(WIN. ~ AST + TOV + STL + ORB + DRB + X3PA + X2PA + FTA, data=ts2122)
summary(modeltpg)
restpg = residuals(modeltpg)
plot(restpg) #homoscedasticity OK
summary(restpg) #mean OK
qqnorm(restpg)
qqline(restpg) 
hist(restpg, col = "pink", xlab = "Residuals", main = "Residuals Histogram")
print(shapiro.test(restpg))

#introduce distinction between tank and contenders
ts2122["status"] <- c("contender","contender","contender","contender",
                       "contender", "contender","contender","contender",
                       "tank","contender","tank","tank","contender","tank",
                       "contender","contender","contender","contender","contender",
                       "contender","tank","tank","contender","contender",
                       "tank","tank","contender","contender","contender","contender")
attach(ts2122)
names(ts2122)
View(ts2122)

plot(X3P., WIN., xlab = "X3P.", ylab = "WIN.")
points(X3P.[status == "contender"], WIN.[status == "contender"], pch = 16, col ='blue')
points(X3P.[status == "tank"], WIN.[status == "tank"], pch = 16, col = 'red')
legend("topright", legend = c("Contender", "Tank"), col = c("blue", "red"), pch = c(16, 16))

#test
contenders = X3P.[status=="contender"]
tank = X3P.[status=="tank"]
print(t.test(contenders, tank)) #p-value = 0.0004431 < alpha = 0.05

contenders = X2P.[status=="contender"]
tank = X2P.[status=="tank"]
print(t.test(contenders, tank)) #p-value = 0.1118 > alpha = 0.05

contenders = PACE[status=="contender"]
tank = PACE[status=="tank"]
print(t.test(contenders, tank)) #p-value = 0.02375 < alpha = 0.05

contenders = AST[status=="contender"]
tank = AST[status=="tank"]
print(t.test(contenders, tank)) #p-value = 0.01043 < alpha = 0.05

contenders = DRB[status=="contender"]
tank = dRB[status=="tank"]
print(t.test(contenders, tank)) #p-value < 2.2e-16 < alpha = 0.05

#step-up
View(ts2122)
ts2122 <- ts2122[-c(11, 22, 9, 21, 12, 25, 26, 14), ]
View(ts2122)

initial_model <- lm(WIN. ~ 1, data = ts2122)
final_model <- step(initial_model, direction = "forward",
                    scope = list(lower = ~1, upper = ~ PACE + SOS + oEFF+ dEFF + X2P. + 
                                   X3P. +AST+ TOV + STL + 
                                   ORB + DRB +X3PA + X2PA + FTA+FT.))
summary(final_model) #relevant: oEFF, dEFF, X3PA, X3P.

par()
plot(ts2122$X3P.,ts2122$WIN.,xlab = "X3P. contender", ylab = "WIN. contender", pch = 16, col = "green", main = "X3P.and WIN.")

#predictions
ts2223 <- read.csv("ts2223.R")
View(ts2223)
ts2223 <- ts2223[-c(9, 27, 11, 4, 25, 30, 12, 22), ]
View(ts2223)

predictions <- predict(final_model, newdata = ts2223)
predictions
R2 = R2(predictions, ts2223$WIN.)
mse = mean((predictions-ts2223$WIN.)^2)
mse
RMSE = RMSE(predictions, ts2223$WIN.)
n = 22
k = 4
adjusted_R2= 1- (1-R2^2)*(n-1)/(n-k-1)
R2
RMSE
print(adjusted_R2)

#further conclusion
seasons <- read.csv("seasons.R")
attach(seasons)
View(seasons)
par()
X3made = seasons$X3PA*seasons$X3P.

plot(seasons$Season, seasons$X3P., xlab = "Season", ylab = "X3P.", type = "l", col = "blue", main = "X3P. Over Seasons")
plot(seasons$Season, seasons$X3PA, xlab = "Season", ylab = "X3PA", type = "l", col = "blue", main = "X3PA Over Seasons")
plot(seasons$Season, seasons$PTS, xlab = "Season", ylab = "PPG", type = "l", col = "blue", main = "PPG Over Seasons")

X3PPG = (X3made*3)/seasons$PTS

plot(seasons$Season,X3PPG, xlab = "Season", ylab = "X3PPG", type = "l", col = "blue", main = "X3PPG Over Seasons")





