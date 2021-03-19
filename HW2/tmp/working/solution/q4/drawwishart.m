function [invD] = drawwishart(vo,Do,n,bis,K2)
%UNTITLED2 Summary of this function goes here
%   Detailed explanation goes here
v = vo + K2;
sum=zeros(K2,K2);
for i=1:n
    sum = sum + bis(:,i)*(bis(:,i))';
end
D1 = pinv(Do + sum);
invD = wishrnd(D1,v);
end