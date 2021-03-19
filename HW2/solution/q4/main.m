%% Data Summary and reading
data =xlsread('Vella-Verbeek-Data.xlsx','Data');
[m,n] = size(data);
ID = data(:,1);
Year = data(:,2);
AG = data(:,3);
Black = data(:,4);
Bus = data(:,5);
Con = data(:,6); 
Ent = data(:,7);
Exper = data(:,8);
Fin = data(:,9);
Hisp = data(:,10);
Hlth = data(:,11);
Hours = data(:,12);
Man = data(:,13);
Mar = data(:,14);
Min = data(:,15);
Nc = data(:,16);
Ne = data(:,17);
Occ1 = data(:,18);
Occ2 = data(:,19);
Occ3 = data(:,20);
Occ4 = data(:,21);
Occ5 = data(:,22);
Occ6 = data(:,23);
Occ7 = data(:,24);
Occ8 = data(:,25);
Occ9 = data(:,26);
Per = data(:,27);
Pro = data(:,28);
Pub = data(:,29);
Rur = data(:,30);
S = data(:,31);
School = data(:,32);
Tra = data(:,33);
Trad =  data(:,34);
Union = data(:,35);
wage = data(:,36);
all_means = [mean(Hours) mean(School) mean(Exper) mean(wage)];
all_std = [std(Hours) std(School) std(Exper) std(wage)];
all_count = [sum(AG) sum(Black) sum(Bus) sum(Con) sum(Ent)...
    sum(Fin) sum(Hisp) sum(Hlth) sum(Man) sum(Mar) sum(Min) sum(Nc) sum(Ne)...
    sum(Occ1) sum(Occ2) sum(Occ2) sum(Occ3) sum(Occ4) sum(Occ5) sum(Occ6)...
    sum(Occ7) sum(Occ8) sum(Occ9) sum(Per) sum(Pro) sum(Pub) sum(Rur) sum(S)...
    sum(Tra) sum(Trad) sum(Union)];
percentage = all_count./m;
rowlabels = char('Hours','School','Exper','Wages');
rowlabels1 = char('AG','Black','Bus','Con','Ent','Fin','Hisp','Hlth'...
    ,'Man','Mar','Nc','Ne','Occ1','Occ2','Occ3','Occ4','Occ5','Occ6','Occ7'...
    ,'Occ8','Occ9','Per','Pro','Pub','Rur','S','Tra','Trad','Union');
fprintf("          Data Summary                    \n")
fprintf("_______________________________________________\n")
fprintf("       Mean       Standard Deviation    \n")
fprintf("_______________________________________________\n")
for i=1:4
    fprintf('%s %4.2f       %4.2f \n', rowlabels(i,:), all_means(i), all_std(i))
end
fprintf("_______________________________________________\n")
fprintf("      Counts     Percentage              \n")
fprintf("_______________________________________________\n")
for i=1:29
    fprintf('%s %4.2f     %4.2f  \n', rowlabels1(i,:),all_count(i),percentage(i)*100)
end
%% Panel data Model
Y = wage;
W = [ones(m,1) Exper];
X = [AG Black Bus Con Ent Fin Hisp Hlth Hours Man Mar Min Nc Ne Occ1 Occ2...
    Occ3 Occ4 Occ5 Occ6 Occ7 Occ8 Per Pro Pub Rur S School Trad Union];
nsim = 10000;
burn = 2500;
K1 = size(X,2);
K2 = size(W,2);
Beta = zeros(K1,nsim);
bis = zeros(K2,m/8,nsim);
Hu = zeros(1,nsim);
invD = zeros(K2,K2,nsim);
Do = inv(eye(K2));
vo = 6;
bo = zeros(K1,1);
Bo = 10*eye(K1);
invBo = inv(Bo);
invBobo = Bo\bo;
alphao = 6;
deltao = 3;
b1 = zeros(K2,1);
Beta(:,1) = ones(K1,1);
bis(:,:,1) = ones(K2,m/8);
h = waitbar(0,'Simulation in Progress');
for i=2:nsim    
    Hu(1,i) = drawgamma(alphao,m,deltao,Y,W,X,Beta(:,i-1),bis(:,:,i-1));
    invD(:,:,i) = drawwishart(vo,Do,m/8,bis(:,:,i-1),K2);
    Beta(:,i) = drawnormal(X,Y,W,invD(:,:,i),Hu(1,i),invBo,invBobo,m,K1);
    bis(:,:,i) = drawnormal1(Hu(1,i),W,invD(:,:,i),Y,X,Beta(:,i),m,K2);
    waitbar(1/nsim);
end
close(h);
beta_means = mean(Beta(:,burn+1:nsim),2);
beta_std = std(Beta(:,burn+1:nsim),0,2);
bis_means = zeros(K2,m/8);
bis_std =zeros(K2,m/8);
for i=1:m/8
    bis_means(:,i) = [mean(bis(1,i,burn+1:nsim)); mean(bis(2,i,burn+1:nsim))];
    bis_std(:,i) = [std(bis(1,i,burn+1:nsim)); std(bis(2,i,burn+1:nsim))];
end
hu_means = mean(Hu,2);
D_means = [mean(invD(1,1,burn+1:nsim))...
    mean(invD(1,2,burn+1:nsim)); ...
    mean(invD(2,1,burn+1:nsim)) ...
    mean(invD(2,2,burn+1:nsim))];
D_std = [std(invD(1,1,burn+1:nsim))...
    std(invD(1,2,burn+1:nsim)); ...
    std(invD(2,1,burn+1:nsim))...
    std(invD(2,2,burn+1:nsim))];
hu_std = std(Hu,0,2);
mean_bis_mean = mean(bis_means,2);
mean_bis_std = mean(bis_std,2);
rowlabels2 = char('AG', 'Black', 'Bus','Con', 'Ent', 'Fin', 'Hisp', 'Hlth',...
    'Hours', 'Man', 'Mar', 'Min', 'Nc', 'Ne', 'Occ1', 'Occ2','Occ3','Occ4',...
    'Occ5', 'Occ6', 'Occ7', 'Occ8', 'Per', 'Pro', 'Pub', 'Rur', 'S', 'School'...
    ,'Trad', 'Union');
rowlabels3 =char('Intercept','Experience');
fprintf("               Bayesian Estimates                    \n")
fprintf("_______________________________________________\n")
fprintf("            Mean     Standard Deviation    \n")
fprintf("_______________________________________________\n")
for i=1:K2
    fprintf('%s  %4.2f     %4.2f \n',rowlabels3(i,:), mean_bis_mean(i),mean_bis_std(i));
end
for i=1:K1
    fprintf('%s  %4.2f     %4.2f \n',rowlabels2(i,:), beta_means(i), beta_std(i));
end
fprintf("__________________________________________________\n")
fprintf("Mean of invD Matrix = \n")
disp(D_means)
fprintf("Standard Deviation of invD Matrix = \n")
disp(D_std)
fprintf("__________________________________________________\n")
fprintf("Mean of Hu = \n")
disp(hu_means)
fprintf("Standard Deviation of Hu = \n")
disp(hu_std)
%% Interpretation of Coefficients
fprintf('As the coefficient for Union membership is positive and statistically\n')
fprintf(' significant, it implies that the logarithm of wages of the employee\n') 
fprintf(' increases by 0.08 if they are a part of Union as compared to when\n')
fprintf(' they are not.\n');