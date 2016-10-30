# Author    : allqoow
# Contact   : allqoow@gmail.com
# Started on: 20161030(yyyymmdd)
# Project   : exerciseML(Exercise for Microeconometrics)

# cleaning the memory to make sure it's running on itself
remove(list=ls())

# setting a working directory:
setwd("C:/Users/user/OneDrive/2016Semester2_TUBerlin/Kurse/Microeconometrics/Tutorial_1_Introduction")

# installing the package "geometry"
install.packages("geometry")

# loading the package
library("geometry")

# 3.1
((2014 - 2011)/1992)*100

# 3.2
theYear = 2014
yearUniStart = 2011
yearBorn = 1992
((theYear - yearUniStart)/yearBorn)*100

# 3.3
inputVec = c(4,5,8,11)
sum(x=inputVec)

#3.4
#3.5
x = rnorm(100)
plot(x)

# 4
help(sqrt)

# 5 cf. "filescript.R"
# 6.1
# 6.2
dataVec = seq(from=31, to=60, by=1)
mat = matrix(data=dataVec, nrow=6)

# 6.3
x1 = rnorm(100)
x2 = rnorm(100)
x3 = rnorm(100)

t = data.frame(a=x1, b=x1+x2, c=x1+x2+x3)
plot(t)
sd(t$a)
sd(t$b)
sd(t$c)


# 7
plot(t$a, type="l", ylim=range(t), lwd=3, col=rgb(1,0,0,0.3))
lines(t$b, type="s", lwd=1, col=rgb(0.3,0.4,0.3,0.9))
points(t$c, pch=20, cex=10, col=rgb(0,0,1,0.3))
help(par)

# lwd: width of a line
# pch: type of points
# cex: size of points
# col: colour of the plot, if the arguement is given as rgb,
#      each element will be used as: (reg,green,blue,alpha)

# 8
d1 = data.frame(a=c(1,2,4,8,16,32),g=c(2,4,8,16,32,64),x=c(3,6,12,24,48,96))
d1
write.table(d1, file="tst1.txt", row.names=FALSE)
dRead = read.table(file="tst1.txt", header=TRUE)
d2 = dRead$g*5
write.table(d2, file="tst2.txt", row.names=FALSE)
dRead = read.table(file="tst2.txt", header=TRUE)
dRead

# 9
# 10
date1=strptime(c("20161030000000","20161206000000","20170505000000"),format="%Y%m%d%H%M%S")
expNPresents=c(10,15,20)
date1
expNPresents
plot(date1,expNPresents)

# 11.1
# 11.2
vec11 = seq(100)
vec11[1]
retVec11 = c()
for(i in 1:100){
  tryVal = vec11[i]
  if(tryVal<5 || tryVal>90){
    retVec11[i] = tryVal*10
  } else {
    retVec11[i] = tryVal*0.1
  }
}
retVec11

# 11.3
func11 = function(arg1){
  ret = c()
  for(i in 1:length(arg1)){
    tryVal = arg1[i]
    if(tryVal<5 || tryVal>90){
      ret[i] = tryVal*10
    } else {
      ret[i] = tryVal*0.1
    }
  }
  return(ret)
}

func11(seq(20))
func11(seq(100))

