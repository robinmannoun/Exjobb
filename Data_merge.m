%% MERGE ALL DATA

all_data = [monthly_data, makro_data, firm_data];
all_data.ID = all_id; 

no_nan = rmmissing(all_data);

nbr_nan = groupcounts(no_nan,'ID');
shit = min(nbr_nan.GroupCount);

matrix_data = table2array(no_nan);
matrix_data(any(isinf(matrix_data),2),:) = [];

%% Dumies


%% Standardize liquidity measures

liq = matrix_data(:,2:7);

liq(:,1) = liq(:,1)./std(liq(:,1));
liq(:,2) = liq(:,2)./std(liq(:,2));
liq(:,3) = liq(:,3)./std(liq(:,3));
liq(:,4) = liq(:,4)./std(liq(:,4));
liq(:,5) = liq(:,5)./std(liq(:,5));
liq(:,6) = liq(:,6)./std(liq(:,6));

liq(liq>3) = [3];

%% PCA

[V,D] = eig(cov(liq));
%[Max, Ind] = max(diag(D));
[D,ind] = sort(diag(D),'descend');
V = (V(:,ind));
pc1 = liq*V(:,1);
pc2 = liq*V(:,2);
pc3 = liq*V(:,3);

orthognal= V(:,1)'*V(:,2);
test = sum(V.^2);

pc = [pc1, pc2 pc3];

u = [pc1 pc2 pc3 liq]; 
v = corr(u);

%% Macro matrix

macro  = matrix_data(:,8:17);

%% Firm matrix

firm = matrix_data(:,18:21);

firm(:,1) = firm(:,1)./std(firm(:,1));
firm(:,2) = firm(:,2)./std(firm(:,2));
firm(:,3) = firm(:,3)./std(firm(:,3));
firm(:,4) = firm(:,4)./std(firm(:,4));

firm(firm>3) = [3];

%% X-data
id = matrix_data(:,22);
time = matrix_data(:,1);

x= [macro, firm];
%% Regression

est1 = panel(id,time,pc1,x,'fe');

est2 = panel(id,time,pc2,x,'fe');

est3 = panel(id,time,pc3,x,'fe');

% plot(est.yhat(10000:13000),'r');
% hold on
% plot(pc1(10000:13000));

