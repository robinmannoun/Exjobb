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

tbv_vec = grpstats(data,'DateNbr','sum','DataVars','TbV');

I=data.DateNbr(1:end-1, 1) ~= data.DateNbr(2:end, 1);
last_Tr= zeros(21476,1);
last_Tr(end)=data.MV(end);

for i = 1:length(I)
   if(I(i)==1)
       last_Tr(i) = data.MV(i);
   end 
end

last_Tr(last_Tr(:,1)==0,:) = [];
last_Tr(1027)=last_Tr(1026);
tbv_vec = table2array(tbv_vec);
tr = tbv_vec(:,3)./last_Tr;

%% LHH

ph_month = grpstats(data,'DateNbr','max','DataVars','PH');
pl_month = grpstats(data,'DateNbr','min','DataVars','PL');
ph = ph_month.max_PH;
pl= pl_month.min_PL;

LHH_ = (ph-pl)./pl;

LHH = LHH_./tr;


%% Amihud Illiquidity

rt_abs = abs(data.R);
illiquidity_daily = rt_abs./data.TbV;

data.illiq = illiquidity_daily;

monthly_illiq = grpstats(data,'DateNbr','mean','DataVars','illiq');

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

monthly_data = table(new_col,tr,zero_trading,bid_ask,LHH,monthly_spreadPercentage.mean_SpreadPercentage, monthly_illiq.mean_illiq);

monthly_data.Properties.VariableNames = {'Month_count' 'TurnoverRatio' 'Zero_Trading_Days' 'Bid_Ask' 'LHH' 'Spread_Percentage' 'Illiquidity'};
