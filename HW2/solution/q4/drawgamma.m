function [Hu] = drawgamma(alphao,m,deltao,Y,W,X,Beta,bis)
%UNTITLED Summary of this function goes here
%   Detailed explanation goes here
alpha1 = alphao + m;
T = 7;
sum = 0;
z=1;
for i=1:T+1:m
    Yj = Y(i:i+T);
    Xj = X(i:i+T,:);
    Wj = W(i:i+T,:);
    bisj = bis(:,z);
    sum = sum + (Yj - Xj*Beta - Wj*bisj)'*(Yj - Xj*Beta - Wj*bisj);
    z=z+1;
end
delta1 = deltao + sum;
Hu = gamrnd(alpha1/2,2/delta1);
end