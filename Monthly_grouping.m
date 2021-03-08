%% Get data
data = readtable('Small Cap normalized.xlsx')

data = data(:,1:17);
%% Convert all data to numeric
nbr_row = height(data);

data.PA = str2double(data.PA);
data.PB = str2double(data.PB);
data.Bid_ask = str2double(data.Bid_ask);
data.Bid_askNorm = str2double(data.Bid_askNorm);
data.SpreadPercentage = str2double(data.SpreadPercentage);
data.PH = str2double(data.PH);
data.PL = str2double(data.PL);
data.PO = str2double(data.PO);
data.TbV = str2double(data.TbV);
data.TurnoverRatio = str2double(data.TurnoverRatio);
data.LHH = str2double(data.LHH);
data.LHHNormalized = str2double(data.LHHNormalized);

%% Get monthly average

dv = datevec(data.Date);
data.Year = dv(:,1);
data.Month = dv(:,2);

monthly_avg = grpstats(data,{'Year','Month'},'mean','DataVars','R');

%nbr_months = height(monthly_avg);
%illiq = zeros(nbr_months,1);