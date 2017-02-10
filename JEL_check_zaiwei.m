function [pvalue, reject, stdvar] = JEL_check(dist,para,B,n,alpha)

[para1,para2] = size(para);
qchi2 = chi2inv(1-alpha,1);
reject = 0;

for i = 1:B
    if para2 == 1
        X = random(dist,para,1,n);
    else
        X = random(dist,para(1),para(2),1,n);
    end
    Tn = u_est(X);
    V = [];
    for i = 1:n
        Xi = X;
        Xi(:,i) = [];
        Ti = u_est(Xi);
        Vi = n*Tn-(n-1)*Ti;
        V = [V,Vi];
    end
    options = optimset('display','off');
    [theta_L, theta_U] = endpoint(0, 100, 0.001);
    reject = 1 - (theta_L <= 0 <= theta_U);
end
reject = reject/B;
end

% 用newtzero算endpoint
function [theta_L, theta_U] = endpoint(xr, m, tol)
[t_L, t_U] = NEWTZERO(@L_theta, xr, m, tol);

% 似然函数
function value = L_theta(t)
value(1) = 2*sum(1 + x.*(V-t));

% 求解lambda
function [chisq,x] = JEL(x0,options,V,theta)
[x,fval] = fsolve(@formula,x0,options,V,theta);
chisq=2*sum(log(1+x.*(V-theta)));
end

function fun = formula(lamda,V,theta)
fun(1) = (sum((V-theta)./(1+lamda.*(V-theta))))^2;
end

f
