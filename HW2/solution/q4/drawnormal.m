function [Beta] = drawnormal(X,Y,W,invD,Hu,invBo,invBobo,m,K1)
%UNTITLED3 Summary of this function goes here
%   Detailed explanation goes here
T=7;
sum = zeros(K1,K1);
sum1 = zeros(K1,1);
for i=1:T+1:m
     Wj = W(i:i+T,:);
     Yj = Y(i:i+T);
     Xj = X(i:i+T,:);
     B1j = Wj*(pinv(invD))*(Wj') + pinv(Hu)*eye(T+1);
     invB1j = B1j\eye(8);
     sum = sum + Xj'*(invB1j)*Xj;
     sum1 = sum1 + Xj'*(invB1j)*Yj;
end
B_sur = pinv(sum + invBo);
Btilde = B_sur*(invBobo + sum1);
L =chol(B_sur,'lower');
Beta = Btilde + (L*mvnrnd(zeros(K1,1), eye(K1))');
end