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

b1.hat = Sxy/Sxx
b0.hat = y.bar - b1.hat*x.bar

SSE = Syy-b1.hat*Sxy
SSE = Syy - b1.hat*Sxy
MSE = SSE/(n-2)
r = Sxy/sqrt(Sxx*Syy)
R2 = 1-SSE/Syy
par(mfrow = c(2,3))
#a) Vẽ đồ thị đám mây điểm. 
plot(x,y,pch = 16, col = 'forestgreen',xlab = 'Biến độc lập x', ylab = 'Biến phụ thuộc y', main = "Đồ thị đám mây điểm")
#+đường HQTT
abline(b0.hat, b1.hat, lwd = 2, col ='red')

#b) Tìm đường HQTT của y theo x. Vẽ đg hồi quy, đồ thị biểu diễn các giá trị thặng dư ei
cat("b) Đường hồi quy tuyến tính của Y theo X là \n")
cat("y =", b0.hat,"+",b1.hat,"* x\n")
#vẽ đồ thị biểu diễn các giá trị thặng dư
Model = lm(y~x)
ei = resid(Model)
hii = ((x-x.bar)^2/Sxx)+(1/n)
std.ei = ei/sqrt((1-hii)*MSE)
y.hat = predict(Model)
plot(y.hat,std.ei,pch = 16, col ='forestgreen', main = 'Đồ thị biểu diễn các giá trị thặng dư')
abline(h = c(-2,2),lwd = 2, lty = 4, col = 'purple')

#c) Chẩn đoán chất lg mô hình hồi quy ( tính SSE, MSE, R^2, r)
##i) KTC cho các hệ số hồi quy, alpha = 5%
alpha = 0.05
p=2
F.value = qf(1-alpha, p, n-p)
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
cat("(ii) Khoảng tin cậy cho sigma^2:\n")
cat("       (",LB.sigma,";",UB.sigma,")\n")

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
##iv) Lập bảng tính anova cho bài toán hồi quy trên

##v) Kiểm tra giả thiết chuẩn của sai số dự báo bằng trực quan (histogram, QQ, boxplot..)
boxplot(ei,col ='salmon', main = 'Boxplot sai số dự báo')
QQ = qqnorm(ei, plot = FALSE)
plot(QQ, pch = 16, col = 'cornflowerblue', main = 'QQ của sai số dự báo')
qqline(QQ$y, probs = c(0.1,0.9), col = 'red')
ei_ct=ei/(sqrt(MSE*(1-hii)))
hist(std.ei,
     probability = FALSE,
     col = "forestgreen",
     border = "white",
     main = "Histogram sai số dự báo ")
