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

%% Compute averages

monthly_MV = grpstats(data,'DateNbr','mean','DataVars','MV');
monthly_P = grpstats(data,'DateNbr','mean','DataVars','P');

%% Volatility

daily_volatility = grpstats(data,'DateNbr','std','DataVars','R');
vol = sqrt(daily_volatility.GroupCount).*daily_volatility.std_R;

%% Firm characteristics

firm_data = readtable('Price to book, EPS all companies_final.xlsx');

firm_data.PTB = str2double(firm_data.PTB);
firm_data.EPS = str2double(firm_data.EPS);
firm_data = removevars(firm_data,{'Var1' 'Var4'});

firm_data.VOL = vol;

firm_data.MV = monthly_MV.mean_MV;
%firm_data.PE = monthly_P.mean_P./firm_data.EPS;


