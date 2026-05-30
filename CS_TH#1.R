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
bang_dac_trung <- data.frame(
  Chiso = c("n", "x_bar", "y_bar", "Sigma x", "Sigma y", 
            "Sigma x^2", "Sigma y^2", "Sigma xy", "(Sigma x)(Sigma y)", 
            "Sxx", "Syy", "Sxy","r","R^2"),
  Giatri = c(n,x.bar,y.bar, sum(x), sum(y),sum(x^2), sum(y^2),sum(x*y),sum(x)*sum(y),Sxx,Syy,Sxy,Sxy/sqrt(Sxx*Syy),1-SSE/Syy)
)
cat("a) Thống kê mô tả \n")
print(bang_dac_trung)

#b) Vẽ đồ thị đám mây điểm. Nhận xét
par(mfrow = c(1,2))
plot(x,y,pch = 16, col = 'forestgreen',xlab = 'Biến độc lập x', ylab = 'Biến phụ thuộc y', main = "Đồ thị đám mây điểm")
#+đường HQTT
abline(b0.hat, b1.hat, lwd = 2, col ='red')

#c) Vẽ đồ thị hàm mục tiêu
b0 = seq(-18.5,-16.5, by = 0.1)
b1 = seq(3,5, by = 0.1)
f <- function(b0,b1) sum((y-b0-b1*x)^2)
Qb = Vectorize(f)
z = outer(b0,b1,Qb)
persp(b0,b1,z,phi = 10, theta = 45, col = 2:7, main = "Đồ thị hàm mục tiêu")

#d) Tìm đường HQTT của y theo x
cat("d) Đường hồi quy tuyến tính của Y theo X là \n")
cat("y =", b0.hat,"+",b1.hat,"* x\n")

#e) Sử dụng công thức nghiệm TQ để xây dựng đường HQTT với X là ma trận thiết kế
X.mat = cbind(1,x)
XtX = t(X.mat) %*% X.mat
XtX.inv = solve(XtX) #tính ma trận ngược ^-1
b.hat = XtX.inv%*%t(X.mat)%*%y
cat("e) Đường hồi quy tuyến tính của Y theo X (X là ma trận thiết kế)là \n")
cat("y =", b.hat[1,],"+",b.hat[2,],"* x\n")