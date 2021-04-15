%% Get data
data = readtable('Mega_merge.V3.xlsx');

data = data(:,1:18);
%% Convert data from char to numeric
nbr_row = height(data);

data.R = str2double(data.R);
data.PA = str2double(data.PA);
data.PB = str2double(data.PB);
data.Bid_ask = str2double(data.Bid_ask);
data.Bid_askNorm = str2double(data.Bid_askNormalized);
data.SpreadPercentage = str2double(data.SpreadPercentage);
data.PH = str2double(data.PH);
data.PL = str2double(data.PL);
data.PO = str2double(data.PO);
data.TbV = str2double(data.TbV);
data.TurnoverRatio = str2double(data.TurnoverRatio);
data.LHH = str2double(data.LHH);
data.LHHNormalized = str2double(data.LHHNormalized);

%% Get monthly average

monthly_LHH = grpstats(data,'DateNbr','mean','DataVars','LHH');
monthly_spreadPercentage = grpstats(data,'DateNbr','mean','DataVars','SpreadPercentage');
monthly_bid_ask = grpstats(data,'DateNbr','mean','DataVars','Bid_ask');

bid_ask = monthly_bid_ask.mean_Bid_ask;
bid_ask(bid_ask<0)= [0];
%% Zero trading days
nan_vec = isnan(data.PH);

data.nan=nan_vec;

nan_PH = grpstats(data,'DateNbr','sum','DataVars','nan');

zero_trading = nan_PH.sum_nan./nan_PH.GroupCount;

%% Monthly Return
data.DateNbr = data.DateNbr;
first = zeros(nbr_row,1);
first(1) = data.P(1);
last= zeros(nbr_row,1);
last(end)=data.P(end);

for i = 1:height(data)-1
   if(data.DateNbr(i)~= data.DateNbr(i+1))
       last(i) = data.P(i);
       first(i) = data.P(i+1);
   end 
end

first(first(:,1)==0,:) = [];
last(last(:,1)==0,:) = [];
ret = last./first-1;

%% Turnover Ratio
tbv_vec = grpstats(data,'DateNbr','sum','DataVars','TbV');
last_Tr= zeros(nbr_row,1);
last_Tr(end)=data.MV(end);

for i = 1:height(data)-1
   if(data.DateNbr(i)~= data.DateNbr(i+1))
       last_Tr(i) = data.MV(i);
   end 
end
last_Tr(last_Tr(:,1)==0,:) = [];
tbv_vec = table2array(tbv_vec);
tr = tbv_vec(:,3)./last_Tr;

%% Amihud Illiquidity

rt_abs = abs(data.R);
illiquidity_daily = rt_abs./data.TbV;

data.illiq = illiquidity_daily;

monthly_illiq = grpstats(data,'DateNbr','mean','DataVars','illiq');

%% Volatility

monthly_volatility = grpstats(data,'DateNbr','std','DataVars','P');

vol = monthly_volatility.std_P;

%% All data

monthly_data = table(tr,zero_trading,bid_ask,monthly_LHH.mean_LHH,monthly_spreadPercentage.mean_SpreadPercentage, monthly_illiq.mean_illiq,vol);

monthly_data.Properties.VariableNames = {'TurnoverRatio' 'Zero Trading Days' 'Bid-Ask' 'LHH' 'Spread Percentage' 'Illiquidity' 'Volatility'};

no_nan = rmmissing(monthly_data);
matrix_data = table2array(no_nan);
matrix_data(any(isinf(matrix_data),2),:) = [];

vol = matrix_data(:,7);

matrix_data(:,7)=[];

%matrix_data = table2array(monthly_data);
%% Normalize

matrix_data(:,1) = matrix_data(:,1)./std(matrix_data(:,1));
matrix_data(:,2) = matrix_data(:,2)./std(matrix_data(:,2));
matrix_data(:,3) = matrix_data(:,3)./std(matrix_data(:,3));
matrix_data(:,4) = matrix_data(:,4)./std(matrix_data(:,4));
matrix_data(:,5) = matrix_data(:,5)./std(matrix_data(:,5));
matrix_data(:,6) = matrix_data(:,6)./std(matrix_data(:,6));

matrix_data(matrix_data>3) = [3];

%% PCA

%[components, score] = pca(data);

[V,D] = eig(cov(matrix_data));
%[Max, Ind] = max(diag(D));
[D,ind] = sort(diag(D),'descend');
V = (V(:,ind));
pc1 = matrix_data*V(:,1);
pc2 = matrix_data*V(:,2);
pc3 = matrix_data*V(:,3);

plot(pc2);

% hold on
% plot(data(:,2));

orthognal= V(:,1)'*V(:,2);
test = sum(V.^2);

x = [pc1 pc2 pc3 matrix_data]; 
y = corr(x);


% plot(matrix_data(:,1),'r');
% hold on
% plot(matrix_data(:,2),'b');
% plot(matrix_data(:,3),'g');
% plot(matrix_data(:,4),'y');
% plot(matrix_data(:,5),'*');
% plot(matrix_data(:,6),'-');


%% Regression

makro_data = readtable('Makrovariabler_sammanställning.xlsx');

makro_data.GOVERNMENTBONDYIELD_10YEARMATURITIES = str2double(makro_data.GOVERNMENTBONDYIELD_10YEARMATURITIES);
