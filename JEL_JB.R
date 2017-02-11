require(nleqslv)
require(normtest)

#### Jarque-Bera ####
u_est = function(Y) {
  m = length(Y)
  skew = 1/m * sum((Y - mean(Y))^3) / (1/m * sum((Y-mean(Y))^2))^1.5
  kurt = 1/m * sum((Y - mean(Y))^4) / (1/m * sum((Y-mean(Y))^2))^2
  m * (skew^2/6 + (kurt-3)^2/24)
}


#### Check ####
# B = 1000
# n = 500
# qchi = qchisq(0.95,1)

# # X = rnorm(n)
# # X = rt(n,3)
# X = rt(n,10)
# Tn = u_est(X)
# for (i in 1:n) {
#   Xi = X[-i]
#   Ti = u_est(Xi)
#   V[i] = n*Tn - (n-1)*Ti
# }

#### 计算置信区间 ####
L_theta = function(theta) {
  lamb = function(x) {
    for (i in 1:n) {
      numerator[i] = V[i] - theta
      denominator[i] = 1 + x*(V[i] - theta)
    }
    F1 = sum(numerator/denominator)
  }
  # (lambda = uniroot(lamb, c(-0.001,0.001), tol = 0.0000000001)$root)
  (lambda = nleqslv(0, lamb)$x)
  2*sum(log(1+lambda*(V-theta))) - qchi
}

# JEL = function(x0,theta) {
#   lambda = numeric(1)
# }

#### Check1 ####
# theta = 0
# lamb = function(x) {
#   for (i in 1:n) {
#     numerator[i] = V[i] - theta
#     denominator[i] = 1 + x*(V[i] - theta)
#   }
#   F1 = sum(numerator/denominator)
# }
# # (lambda = uniroot(lamb, c(-0.001,0.001), tol = 0.0000000001)$root)
# (lambda = nleqslv(0, lamb)$x)
# (2*sum(log(1+lambda*(V-theta))) - qchi)

#### Check2 ####
# THETA = seq(-5,5,by=0.1)
# L_THETA = vector(length=length(THETA))
# for (i in 1:length(THETA)) {
#   L_THETA[i] = L_theta(THETA[i])
# }
# 
# plot(THETA, L_THETA, type="l")
# abline(0,0)

#### 模拟结果 ####
set.seed(123)
B = 100
n = 500
qchi = qchisq(0.95,1)
reject = vector(length=B)

for (j in 1:B) {
  numerator = vector(length=n)
  denominator = vector(length=n)
  V = vector(length=n)
  X = rnorm(n)
  Tn = u_est(X)
  for (i in 1:n) {
    Xi = X[-i]
    Ti = u_est(Xi)
    V[i] = n*Tn - (n-1)*Ti
  }
  reject[j] = L_theta(0) > 0
}
JEL_JB = mean(reject,na.rm=T)
  
set.seed(123)
reject2 = vector(length=B)
for (j in 1:B) {
  X = rnorm(n)
  reject2[j] = jb.norm.test(X)$p.value < 0.05
}
JB = mean(reject2)

# 失败