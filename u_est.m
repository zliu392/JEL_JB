%%  Compute Tn

function [s] = u_est(Y)
[~,n]=size(Y);

skew = (1/n*sum((Y-mean(Y).*3))) / (1/n*sum((Y-mean(Y)).*2))^1.5;
kurt = (1/n*sum((Y-mean(Y)).*4)) / (1/n*sum((Y-mean(Y)).*2))^2;
s = n * (skew^2/6 + (kurt-3)^2/24);

end
