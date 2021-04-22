%% Macro variables

omxs = readtable('OMXS_vol.xlsx');

omxs.Properties.VariableNames = {'Date' 'DateNbr' 'P' 'R'};

omx_daily = grpstats(omxs,'DateNbr','std','DataVars','R');

omxs_monthly_vol = sqrt(omx_daily.GroupCount).*omx_daily.std_R;

makro_omx = omxs_monthly_vol;

for i=1:90
    makro_omx = [makro_omx; omxs_monthly_vol];
end

makro_data = readtable('Makrovariabler_sammanställning_V3.xlsx');

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

stacked_x(:,1) =[];

makro_data = array2table(stacked_x); 

makro_data.Properties.VariableNames = { 'IP' 'UR' 'INF' 'SDEPOSIT' 'VSTOXX' 'CBOE' 'Y10' 'REPO' 'M3' 'OMXS_vol' };