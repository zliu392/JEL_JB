function [s,chi2,diff] = JEL_est(X,JB)
[~,n]=size(X);
Tn=u_est(X);
V=[];
for i=1:n
    Xi=X;
    Xi(:,i)=[];
    Ti=u_est(Xi);
    Vi=n*Tn-(n-1)*Ti;
    V=[V,Vi];
end

s = mean(V,2);
options = optimset('display','off');
[chi2,x] = JEL(0,options,V,JB);

diff = lengthen(V,JB);
end

function [chisq,x] = JEL(x0,options,V,theta)
[x,fval]=fsolve(@formula,x0,options,V,theta);
chisq=2*sum(log(1+x.*(V-theta)));
end

function fun = formula(lamda,V,theta)
fun(1) = (sum((V-theta)./(1+lamda.*(V-theta))))^2;
end

function [l_theta] = L_THETA(x,theta)
