%% Extracts BelowChest AOI from Tobii file for Cinderella2FW
% Adam Stone, October 2017
%
% The script, when run in Matlab, will output a table of AOI
% looking times for every trial. The exported Tobii file needs to be
% converted from .xlsx to .csv first. 


close all
clear all

%% Setting Up
% Get and import Cinderella 2 Forward file
disp('Get the .csv file')
[csvfile, pathname_csv] = uigetfile('.csv','Open the converted CSV file')
warning('off','MATLAB:table:ModifiedVarnames');
alldata = readtable(fullfile(pathname_csv,csvfile));


%% Chalk off each repetition 
% Each participant has 2 repetitions, we just need to break it down into
% those repetitions
breaks = zeros(height(alldata),1);
timestamp = alldata(:,{'RecordingTimestamp'});
timestamp = table2array(timestamp);
for i = 1:height(alldata)-1
    breaks(i) = timestamp(i+1) - timestamp(i);
end
breaks = [100;breaks];
breaks(end) = [];

% Find onsets
breaks2 = breaks > 20;
onsets = find(breaks2==1);

%% Create repetition column
rep = zeros(height(alldata),1);
j = 1;
for i = 1:length(onsets)-1
    rows = onsets(i+1) - onsets(i);
    rep(onsets(i):onsets(i)+rows-1) = j;
    j = j + 1;
end
j = j+1;
rep(onsets(end):end) = j;

rep2 = mod(rep,2);

for i = 1:length(rep2)
    if rep2(i) == 0
        rep2(i) = 2;
    end
end



%% Add Rep column
alldata = table2cell(alldata);
alldata = [alldata, num2cell(rep2)];
alldata(:,5) = [];
alldata(:,3) = [];
cinderella = repmat({'Cinderella_2'},length(rep),1);
direction = repmat({'forward'},length(rep),1);
alldata = [alldata, cinderella, direction];

rownames = {'participant','recording','BelowChest','repetition','story','direction'};
alldataT = cell2table(alldata,'VariableNames',rownames);

savefile = strcat('Cinderella2FW_BelowChest_aoidata.csv');
writetable(alldataT,fullfile(pathname_csv,savefile));
disp(['Saved ',savefile,'!']);
