clc;
clear all;

%% Varying with beta
hold on;
for b = 1:5
    n = 10;
    a = 1;
    y = zeros(1,10);
    x = linspace(10,19,10);
    for N = 10:19
        answer = factorial(N-1)*gamma(N-n+b);
        answer = answer/(N*factorial(N-n)*gamma(N+a+b));
        y(N-9) = answer;
    end
    plot(x,y);
    if b == 1
        str = {strcat('beta = ' , num2str(1))};
    else
        str = [str , strcat('beta = ' , num2str(b))];
    end
end
xlabel('N')
ylabel('f(N|n)')
title('Marginal Distribution (a=1,n=10) with varying beta','Fontsize',12);
legend(str{:});
saveas(gcf,'beta-hyperparameter.pdf')
hold off;

%% Varying with alpha
clf("reset")
hold on;
A = linspace(1,5,5);
for i = 1:5
    n = 10;
    b = 1;
    a = A(i);
    y = zeros(1,10);
    x = linspace(10,19,10);
    for N = 10:19
        answer = factorial(N-1)*gamma(N-n+b);
        answer = answer/(N*factorial(N-n)*gamma(N+a+b));
        y(N-9) = answer;
    end
    plot(x,y);
    if i == 1
        str = {strcat('Alpha = ' , num2str(a))};
    else
        str = [str , strcat('Alpha = ' , num2str(a))];
    end
end
xlabel('N')
ylabel('f(N|n)')
title('Marginal Distribution (b=1,n=10) with varying Alpha','Fontsize',12)
legend(str{:});
saveas(gcf,'alpha-hyperparameter.pdf')
hold off;

%% With Varying n
clf("reset")
hold on;
b = 0.1;
a = 0.1;
for i = 1:5
    y = zeros(1,10);
    x = linspace(10,19,10);
    for N = 10:19
        answer = factorial(N-1)*gamma(N-i+b);
        answer = answer/(N*factorial(N-i)*gamma(N+a+b));
        y(N-9) = answer;
    end
    plot(x,y);
    if i == 1
        str = {strcat('n = ' , num2str(i))};
    else
        str = [str , strcat('n = ' , num2str(i))];
    end
end
xlabel('N')
ylabel('f(N|n)')
title('Marginal Distribution (b=0.1,a=0.1) with varying n','Fontsize',12)
legend(str{:});
saveas(gcf,'n-parameter.pdf')
hold off;

