%% Get data
data = readtable('Mega_merge.V4.xlsx');

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

% Id per day
noCompanies = 91;
noDays = 4936;
id_d = linspace(1,noCompanies,noCompanies)';
all_id_d = zeros(noCompanies*noDays,1);

for k=1:noCompanies
    first = (k-1)*noDays+1;
    last = noDays*k;
    temp = k*ones(noDays,1);
    all_id_d(first:last) = temp;
end

% TR

tbv_vec = grpstats(data,'DateNbr','sum','DataVars','TbV');

I=data.DateNbr(1:end-1, 1) ~= data.DateNbr(2:end, 1);
last_Tr= zeros(21476,1);
last_Tr(end)=data.MV(end);

for i = 1:length(I)
   if(I(i)==1)
       last_Tr(i) = data.MV(i);
   end 
end

% for i = 1:height(data)-1
%    if(data.DateNbr(i)~= data.DateNbr(i+1)&& all_id_d(i)==all_id_d(i+1))
%        last_Tr(i) = data.MV(i);
%    end 
% end
last_Tr(last_Tr(:,1)==0,:) = [];
last_Tr(1027)=last_Tr(1026);
tbv_vec = table2array(tbv_vec);
tr = tbv_vec(:,3)./last_Tr;

%%
plot(tr(1:1000));
%ylim([0 0.3]);

%% LHH

ph_month = grpstats(data,'DateNbr','max','DataVars','PH');
pl_month = grpstats(data,'DateNbr','min','DataVars','PL');
ph = ph_month.max_PH;
pl= pl_month.min_PL;

LHH_ = (ph-pl)./pl;

LHH = LHH_./tr;

LHH(6609) = LHH(6610);


%% Amihud Illiquidity

rt_abs = abs(data.R);
illiquidity_daily = rt_abs./data.TbV;

data.illiq = illiquidity_daily;

monthly_illiq = grpstats(data,'DateNbr','mean','DataVars','illiq');

%% Volatility

monthly_volatility = grpstats(data,'DateNbr','std','DataVars','P');

vol = monthly_volatility.std_P;

%% Month Counter

nbr_months = linspace(1,236,236)';

new_col = nbr_months;
for i= 1:90

new_col = [new_col ;nbr_months];

end

%% Company id
noCompanies = 91;
noMonths = 236;
id = linspace(1,noCompanies,noCompanies)';
all_id = zeros(noCompanies*noMonths,1);

for k=1:noCompanies
    first = (k-1)*noMonths+1;
    last = noMonths*k;
    temp = k*ones(noMonths,1);
    all_id(first:last) = temp;
end


%% All data

monthly_data = table(new_col,tr,zero_trading,bid_ask,monthly_spreadPercentage.mean_SpreadPercentage,LHH, monthly_illiq.mean_illiq,vol);

monthly_data.Properties.VariableNames = {'Month_count' 'TurnoverRatio' 'Zero_Trading_Days' 'Bid_Ask' 'Spread_Percentage' 'LHH' 'Illiquidity' 'Volatility'};

monthly_data(21005:21240,:)= [];
monthly_data(17465:17700,:)= [];

no_nan = rmmissing(monthly_data);
matrix_data = table2array(no_nan);
matrix_data(any(isinf(matrix_data),2),:) = [];

vol = matrix_data(:,8);

matrix_data(:,8)=[];
matrix_data(:,1)=[];

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
%plot(matrix_data(:,2));

orthognal= V(:,1)'*V(:,2);
test = sum(V.^2);

pc = [pc1, pc2 pc3];

x = [pc1 pc2 pc3 matrix_data]; 
y = corr(x);


%plot(matrix_data(:,1),'r');
%hold on
plot(pc1);
hold on
plot(matrix_data(:,2),'b');
% plot(matrix_data(:,3),'g');
% plot(matrix_data(:,4),'y');
% plot(matrix_data(:,5),'*');
% plot(matrix_data(:,6),'-');


%% Macro variables

omxs_vol = readtable('OMXS med volla (1).xlsx');
omx = omxs_vol.Var7;
omx(omx ==0) = [];
omx = omx(2:237);

makro_omx = omx;

for i=1:90
    makro_omx = [makro_omx; omx];
end

makro_data = readtable('Makrovariabler_sammanställning_V2.xlsx');

makro_data.GOVERNMENTBONDYIELD_10YEARMATURITIES = str2double(makro_data.GOVERNMENTBONDYIELD_10YEARMATURITIES);
makro_data.UNEMPLOYMENTRATE = str2double(makro_data.UNEMPLOYMENTRATE);
makro_data.RATEOFINFLATION = str2double(makro_data.RATEOFINFLATION);

makro_data = makro_data(2:237,1:10);
makro_data.Date = nbr_months;
makro_matrix = table2array(makro_data);

stacked_x = makro_matrix;
for i= 1:90

stacked_x = [stacked_x ;makro_matrix];

end

stacked_x = [stacked_x,makro_omx];

stacked_x(:,3:10)= stacked_x(:,3:10)./100;
%% ALL DATA

monthly_data.makro = stacked_x;
monthly_data.id=all_id;

no_nan = rmmissing(monthly_data);
matrix_data = table2array(no_nan);
matrix_data(any(isinf(matrix_data),2),:) = [];

vol = matrix_data(:,8);

matrix_data(:,8)=[];
matrix_data(:,1)=[];

y = matrix_data(:,1:6);
x = matrix_data(:,8:17);
time = matrix_data(:,7);
pan_id = matrix_data(:,18);
%x=[x,vol];
spread = x(:,7)-x(:,9);
x = [x,spread];
x(:,9) = [];
x(:,7) = [];

y(:,1) = y(:,1)./std(y(:,1));
y(:,2) = y(:,2)./std(y(:,2));
y(:,3) = y(:,3)./std(y(:,3));
y(:,4) = y(:,4)./std(y(:,4));
y(:,5) = y(:,5)./std(y(:,5));
y(:,6) = y(:,6)./std(y(:,6));

y(y>3) = [3];

[V,D] = eig(cov(y));
%[Max, Ind] = max(diag(D));
[D,ind] = sort(diag(D),'descend');
V = (V(:,ind));
pc1 = y*V(:,1);
pc2 = y*V(:,2);
pc3 = y*V(:,3);

%x = [pc1 pc2 pc3 matrix_data(:,1:6)]; 
%y = corr(x);


%% Firm Characteristcs 

firm_ch = vol;

%% Estimation

est = panel(pan_id,time,pc1,x,'fe');

plot(est.yhat,'r');

plot(pc1);


