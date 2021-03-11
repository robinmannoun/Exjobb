%% Get data
data = readtable('Mega_merge.xlsx');

data = data(:,1:18);
%% Convert data from char to numeric
nbr_row = height(data);

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

monthly_LHH = grpstats(data,'DateNbr','mean','DataVars','LHHNormalized');
monthly_TurnoverRatio = grpstats(data,'DateNbr','mean','DataVars','TurnoverRatio');
monthly_spreadPercentage = grpstats(data,'DateNbr','mean','DataVars','SpreadPercentage');
monthly_bid_ask = grpstats(data,'DateNbr','mean','DataVars','Bid_askNorm');


%% Zero trading days
nan_vec = isnan(data.PH);

data.nan=nan_vec;

nan_PH = grpstats(data,'DateNbr','sum','DataVars','nan');

zero_trading = nan_PH.sum_nan./nan_PH.GroupCount;

%% Create monthly dataset

monthly_data = table(monthly_TurnoverRatio.mean_TurnoverRatio,zero_trading,monthly_bid_ask.mean_Bid_askNorm,monthly_LHH.mean_LHHNormalized,monthly_spreadPercentage.mean_SpreadPercentage);

monthly_data.Properties.VariableNames = {'TurnoverRatio' 'Zero Trading Days' 'Bid-Ask' 'LHH' 'Spread Percentage'};
    