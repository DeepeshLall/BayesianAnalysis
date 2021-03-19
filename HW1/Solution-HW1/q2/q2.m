clc;
clear all;

n = 100
x1 = 0 : 0.01 : 1;
y1 = betapdf(x1,21,91);


n = 1000
x = 0 : 0.01 : 1;
y = betapdf(x,192,820);
plot(x1,y1,'Color',[0.2,0.4,0.8]);
hold on
plot(x,y,'Color',[0.9,0.1,0.1]);
legend('Beta(21,91)','Beta(192,820)')
hold off
xlabel('x');
ylabel('f(x|a,b)');
title('Beta Distribution','Fontsize',12);

