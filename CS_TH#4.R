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
NoP = 21
x.new = seq(min(x),max(x), length = NoP)
par(mfrow = c(2,3))
#a) Vẽ ellip tin cậy đồng thời cho các tham số 
X.mat = cbind(1,x)
alpha = 0.05
bankinh = sqrt(2*MSE*qf(1-alpha,2,n-2)) #qf(alpha,2,n-2, lowertail = false) (qf phân vị, pf hàm pp)
sigma = t(X.mat)%*%X.mat
#phân rã ma trận sigma
Eigen = eigen(sigma)
lamda = Eigen$values   #Eigen$value -> vector lamda, Eigen$vectors -> ma trận D
D = Eigen$vectors
phi = seq(0,2*pi,length =361)
z1 = bankinh * cos(phi)/sqrt(lamda[1])
z2 = bankinh * sin(phi)/sqrt(lamda[2])
z = rbind(z1,z2)
Y = D%*%z
x1 = Y[1,]+b0.hat
x2 = Y[2,]+b1.hat
plot(x1,x2,type='o', col = 2:7, xlab = 'b0', ylab = 'b1', 
     main = 'Ellipse tin cậy đồng thời cho các tham số của mô hình cũ')
points(b0.hat,b1.hat,pch =16, col = 'forestgreen')

#b) Vẽ đám mây điểm dữ liệu đường hồi quy tuyến tính, miền tin cậy cho gía trị tb dự báo và gtri dự báo mới
y.Pred = b0.hat+b1.hat*x.new
std.TB = sqrt(MSE*(1/n+(x.new-x.bar)^2/Sxx))
UB.TB = y.Pred + std.TB*qt(1-alpha/2,n-2)
LB.TB = y.Pred - std.TB*qt(1-alpha/2,n-2)
std.Pred = sqrt(MSE*(1+1/n+(x.new-x.bar)^2/Sxx))
UB.Pred = y.Pred + std.Pred*qt(1-alpha/2,n-2)
LB.Pred = y.Pred - std.Pred*qt(1-alpha/2,n-2)
#vẽ đám mây điểm
plot(x,y,pch = 16, col = 'salmon',xlab = 'b0', ylab = 'b1', ylim = c(min(LB.Pred,y),max(UB.Pred,y)),
     main = 'KTC cho giá trị trung bình dự báo mới và giá trị dự báo mới')
#vẽ đường hồi quy
abline(b0.hat, b1.hat, lwd = 2, col ='lightblue')
lines(x.new,UB.TB, lwd = 2 , lty =4, col = 'forestgreen')
lines(x.new,LB.TB, lwd = 2 , lty =4, col = 'forestgreen')
lines(x.new,UB.Pred, lwd = 2 , col = 'purple2')
lines(x.new,LB.Pred, lwd = 2 , col = 'purple2')

#c) Đánh dấu các điểm dữ liệu nằm ngoài miền tin cậy cho giá trị dự báo mới 

#d) chẩn đoán mô hình
##i) Vẽ boxplot cho các thặng dư ei, đánh dấu các điểm dị biệt
Model = lm(y~x)
ei = resid(Model)
boxplot(ei,col ='salmon', main = 'Boxplot mô hình cũ')
#identify(rep(1,n),ei,1:n,col='red')
##ii) Vẽ biểu đồ QQ, histogram cho thặng dư chuẩn tắc hoá
hii = (x-x.bar)^2/Sxx+1/n
std.ei = ei/sqrt((1-hii)*MSE)
QQ = qqnorm(std.ei, plot = FALSE)
plot(QQ,pch = 16, col = 'cornflowerblue',main = 'QQ mô hình cũ')
qqline(QQ$y, probs = c(0.1,0.9), col ='red')
hist(std.ei,probability = TRUE,col = "lightgreen",
     border = "white",main = "Histogram thặng dư chuẩn hóa",
     xlab = "Thặng dư chuẩn hóa", ylab = "Mật độ xác suất")


# điểm dị biệt
ID.out = which(abs(std.ei) >= 2)
ID.out
#loại bỏ các điểm dị biệt
x = x[-ID.out]
y = y[-ID.out]
n = length(x)
x.bar = mean(x)
y.bar = mean(y)
Sxx = sum((x-x.bar)^2)
Syy = sum((y-y.bar)^2)
Sxy = sum((x-x.bar)*(y-y.bar))

b1.hat = Sxy/Sxx
b0.hat = y.bar - b1.hat*x.bar

SSE = Syy-b1.hat*Sxy
MSE = SSE/(n-2)
r = Sxy/sqrt(Sxx*Syy)
R2 = 1-SSE/Syy
NoP = 21
x.new = seq(min(x),max(x), length = NoP)

#a) Vẽ ellip tin cậy đồng thời cho các tham số 
X.mat = cbind(1,x)
alpha = 0.05
bankinh = sqrt(2*MSE*qf(1-alpha,2,n-2)) #qf(alpha,2,n-2, lowertail = false) (qf phân vị, pf hàm pp)
sigma = t(X.mat)%*%X.mat
#phân rã ma trận sigma
Eigen = eigen(sigma)
lamda = Eigen$values   #Eigen$value -> vector lamda, Eigen$vectors -> ma trận D
D = Eigen$vectors
phi = seq(0,2*pi,length =361)
z1 = bankinh * cos(phi)/sqrt(lamda[1])
z2 = bankinh * sin(phi)/sqrt(lamda[2])
z = rbind(z1,z2)
Y = D%*%z
x1 = Y[1,]+b0.hat
x2 = Y[2,]+b1.hat
plot(x1,x2,type='o', col = 2:7, xlab = 'b0', ylab = 'b1', 
     main = 'Ellipse tin cậy đồng thời cho các tham số của mô hình mới')
points(b0.hat,b1.hat,pch =16, col = 'forestgreen')

#b) Vẽ đám mây điểm dữ liệu đường hồi quy tuyến tính, miền tin cậy cho gía trị tb dự báo và gtri dự báo mới
y.Pred = b0.hat+b1.hat*x.new
std.TB = sqrt(MSE*(1/n+(x.new-x.bar)^2/Sxx))
UB.TB = y.Pred + std.TB*qt(1-alpha/2,n-2)
LB.TB = y.Pred - std.TB*qt(1-alpha/2,n-2)
std.Pred = sqrt(MSE*(1+1/n+(x.new-x.bar)^2/Sxx))
UB.Pred = y.Pred + std.Pred*qt(1-alpha/2,n-2)
LB.Pred = y.Pred - std.Pred*qt(1-alpha/2,n-2)
#vẽ đám mây điểm
plot(x,y,pch = 16, col = 'salmon',xlab = 'b0', ylab = 'b1', ylim = c(min(LB.Pred,y),max(UB.Pred,y)),
     main = 'KTC cho giá trị trung bình dự báo mới và giá trị dự báo mới 
     của mô hình mới')
#vẽ đường hồi quy
abline(b0.hat, b1.hat, lwd = 2, col ='lightblue')
lines(x.new,UB.TB, lwd = 2 , lty =4, col = 'forestgreen')
lines(x.new,LB.TB, lwd = 2 , lty =4, col = 'forestgreen')
lines(x.new,UB.Pred, lwd = 2 , col = 'purple2')
lines(x.new,LB.Pred, lwd = 2 , col = 'purple2')

Model = lm(y~x)
ei = resid(Model)
boxplot(ei,col ='salmon', main ='Boxplot sau khi loại dị biệt')
##ii) Vẽ biểu đồ QQ, histogram cho thặng dư chuẩn tắc hoá
par(mfrow = c(1,2))
hii = (x-x.bar)^2/Sxx+1/n
std.ei = ei/sqrt((1-hii)*MSE)
QQ = qqnorm(ei, plot = FALSE)
plot(QQ,pch = 16, col = 'cornflowerblue', main = 'QQ mô hình mới')
qqline(QQ$y, probs = c(0.1,0.9), col ='red')
hist(std.ei,probability = TRUE,col = "forestgreen",
     border = "white",main = "Histogram thặng dư chuẩn hóa
     mô hình mới",
     xlab = "Thặng dư chuẩn hóa", ylab = "Mật độ xác suất")
