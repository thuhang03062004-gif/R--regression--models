datasets :: cars
attach(cars)
x = speed 
y = dist
alpha= 0.05
n = length(x)
x.bar = mean(x)
y.bar = mean(y)
Sxx = sum((x-x.bar)^2)
Syy = sum((y-y.bar)^2)
Sxy = sum((x-x.bar)*(y-y.bar))
SSE = Syy-b1.hat*Sxy
b1.hat = Sxy/Sxx
b0.hat = y.bar - b1.hat*x.bar

#a) Vẽ đồ thị đám mây điểm. NX
par(mfrow = c(1,2))
plot(x,y,pch = 16, col = 'forestgreen',
     xlab = 'Biến độc lập x', 
     ylab = 'Biến phụ thuộc y', main = "a) Đồ thị đám mây điểm")

#b) Tìm đg HQTT của Y theo X. Vẽ đg hồi quy trên
cat("b) Đường hồi quy tuyến tính của Y theo X là \n")
cat("y =", b0.hat,"+",b1.hat,"* x\n")
#+đường HQTT
abline(b0.hat, b1.hat, lwd = 2, col ='red')

#c) Chẩn đoán chất lg mô hình hồi quy ( tính SSE, MSE, R^2, r)
SSE = Syy - b1.hat*Sxy
MSE = SSE/(n-2)
r = Sxy/sqrt(Sxx*Syy)
R2 = 1-SSE/Syy

##i) KTC cho các hệ số hồi quy, alpha = 5%
alpha = 0.05
#KTC cho b0
UB.b0 = b0.hat+sqrt(MSE*(sum(x^2)/(n*Sxx)))*qt(1-alpha/2,n-2)
LB.b0 = b0.hat-sqrt(MSE*(sum(x^2)/(n*Sxx)))*qt(1-alpha/2,n-2)
cat("(i) Khoảng tin cậy cho b0,b1:\n")
cat(" +Khoảng tin cậy cho b0:")
cat(" (",LB.b0,";",UB.b0,")\n")
#KTC cho b1
UB.b1 = b1.hat + sqrt(MSE/Sxx)*qt(1-alpha/2,n-2)
LB.b1 = b1.hat - sqrt(MSE/Sxx)*qt(1-alpha/2,n-2)
cat(" +Khoảng tin cậy cho b1:")
cat(" (",LB.b1,";",UB.b1,")\n")

##ii) KTC cho sigma^2
UB.sigma = SSE/qchisq(1-alpha/2,n-2, lower.tail = FALSE)
LB.sigma = SSE/qchisq(alpha/2,n-2, lower.tail = FALSE)
cat("(ii) Khoảng tin cậy cho sigma^2:")
cat(" (",LB.sigma,";",UB.sigma,")\n")

##iii) KTC đồng thời cho các hệ số hồi quy
#KTC đồng thời cho b1
LB.b1dt = b1.hat - sqrt(p*qf(1-alpha, p, n-p))*sqrt(MSE/Sxx)
UB.b1dt = b1.hat + sqrt(p*qf(1-alpha, p, n-p))*sqrt(MSE/Sxx)
#KTC đồng thời cho b0
LB.b0dt = b0.hat - sqrt(p*qf(1-alpha, p, n-p))*sqrt(MSE*(1/n + x.bar^2/Sxx))
UB.b0dt = b0.hat + sqrt(p*qf(1-alpha, p, n-p))*sqrt(MSE*(1/n + x.bar^2/Sxx))
cat("(iii) Khoảng tin cậy đồng thời cho b0,b1:\n")
cat(" +Khoảng tin cậy đồng thời cho b0:")
cat(" (",LB.b0dt,";",UB.b0dt,")\n")
cat(" +Khoảng tin cậy đồng thời cho b1:")
cat(" (",LB.b1dt,";",UB.b1dt,")\n")
#d) So sánh với các lệnh lm(), summary(), confint()
