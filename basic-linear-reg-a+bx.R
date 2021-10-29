launch <- read.csv("https://raw.githubusercontent.com/PacktPublishing/Machine-Learning-with-R-Third-Edition/master/Chapter06/challenger.csv", stringsAsFactors = FALSE)

#finding slope, b = Cov(x, y) / Var(x)
b <- cov(launch$temperature, launch$distress_ct) / var(launch$temperature)
#finding intercept, a = ybar - b*xbar
a <- mean(launch$distress_ct) - b*mean(launch$temperature)
#finding corr coeff, r = cov(x, y)/(sd(x)sd(y))
r <- cov(launch$temperature, launch$distress_ct) / (sd(launch$temperature)*sd(launch$distress_ct))
cor(launch$temperature, launch$distress_ct) #gives same thing, much easier
print(a, b, r)
