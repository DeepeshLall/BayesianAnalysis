function [bis]= drawnormal1(Hu,W,invD,Y,X,Beta,m,K2)
%UNTITLED4 Summary of this function goes here
%   Detailed explanation goes here
T = 7;
z=1;
bis = zeros(K2,m/8);
for i=1:T+1:m
     Wj = W(i:i+T,:);
     Yj = Y(i:i+T);
     Xj = X(i:i+T,:);
     D1j = pinv(Hu*(Wj')*Wj + invD);
     bj = D1j*(Hu*(Wj')*(Yj - Xj*Beta));
     L = chol(D1j,'lower');
     bis(:,z) = bj + (L*mvnrnd(zeros(K2,1), eye(K2))');
     z=z+1;
end
end