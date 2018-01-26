%% Extracts trial information from Tobii file for Rain's Child Study
% Adam Stone, October 2017
%
% The script, when run in Matlab, will output a table of AOI
% looking times for every trial. The exported Tobii file needs to be
% converted from .xlsx to .csv first. 


close all
clear all

%% Setting Up
% Get and import file
disp('Get the .csv file')
[csvfile, pathname_csv] = uigetfile('.csv','Open the converted CSV file')
warning('off','MATLAB:table:ModifiedVarnames');
alldata = readtable(fullfile(pathname_csv,csvfile));

% Get ID, group, gender
ID = alldata.RecordingName{1};

if strfind(csvfile, 'Group 1')
    group = 1;
elseif strfind(csvfile, 'Group 2')
    group = 2;
else warning('Hello?')
end

gender = alldata.x_Gender_Value{1};
language = alldata.x_Language_Value{1};
analysis = alldata.x_Analysis_Value{1};
participant = alldata.x___ParticipantName{1};

% Replace all NA with '' 
%alldata = standardizeMissing(alldata,'NA');


%% Getting Events
% Get media column
media = alldata(:,'MediaName');
media.event = false(height(media),1);
media.onset = (1:height(media))';

% Find event onsets
for i = 1:(height(media)-1)
    if ~strcmp(media.MediaName(i),media.MediaName(i+1))
        media.event(i+1) = 1;
    end
end

% Collapse
events = media(find(media.event==1),:);
events = events(:,[1,3]);
offset = events{2:2:height(events),2};
events = events(find(~strcmp(events.MediaName,'')),:);
events.offset = offset-1;

% Delete Calibration
calib = false(height(events),1);
for i = 1:length(calib)
    if strfind(events.MediaName{i}, 'Calibration')
        calib(i) = 1;
    end
end
events.calib = calib;
events = events(find(events.calib==0),:);

% Delete Puppies
avi = false(height(events),1);
for i = 1:length(avi)
    if strfind(events.MediaName{i}, 'avi')
        avi(i) = 1;
    end
end
events.avi = avi;
events = events(find(events.avi==1),:);

%% Add Event Marks, Trialnum, Repetition, etc
marks = zeros(height(events),1);
for i = 1:length(marks)
    if strfind(events.MediaName{i}, 'Cinderella_1')
        marks(i) = 1;
    elseif strfind(events.MediaName{i}, 'Cinderella_2')
        marks(i) = 2;
    elseif strfind(events.MediaName{i}, 'ThreeBears_1')
        marks(i) = 3;
    elseif strfind(events.MediaName{i}, 'ThreeBears_2')
        marks(i) = 4;
    elseif strfind(events.MediaName{i}, 'KingMidas_1')
        marks(i) = 5;
    elseif strfind(events.MediaName{i}, 'KingMidas_2')
        marks(i) = 6;
    elseif strfind(events.MediaName{i}, 'RedRiding_1')
        marks(i) = 7;
    elseif strfind(events.MediaName{i}, 'RedRiding_2')
        marks(i) = 8;
    end
end

if group == 1
    events.mark = marks;
elseif group == 2
    events.mark = marks+8;
end

% if group == 1
%     events.rep = [repmat(1,8,1);repmat(2,8,1)]; 
% elseif group == 2
%     events.rep = [1,1,1,1,2,1,1,1,2,1,2,2,2,2,2,2]';
% end

rep = zeros(length(marks),1);
for i = 1:length(marks)
    found = find(marks(1:i)==marks(i));
    if length(found) == 1
        rep(i) = 1;
    elseif length(found) == 2
        rep(i) = 2;
    end
end

events.rep = rep;
events.trial = (1:height(events))';
events = events(:,{'MediaName','mark','trial','rep','onset','offset'});
eventscell = table2cell(events);


%% Get AOI columns
varnames = alldata.Properties.VariableNames';
for i = 1:length(varnames)
    if strfind(varnames{i},'AOI')
        varaoi(i) = 1;
    else varaoi(i) = 0;
    end
end
varaoi = varaoi';
firstaoi = min(find(varaoi==1));
lastaoi = max(find(varaoi==1));

aoi = alldata(:,firstaoi:lastaoi);
aoidata = [];


%% Loop through all trials
trialnum = height(events);

for i = 1:trialnum
    trialsection = aoi(events.onset(i):events.offset(i),:);
    aoisums = varfun(@sum,trialsection);
    aoisumscell = table2cell(aoisums);
    aoisumscell = [aoisums.Properties.VariableNames;aoisumscell]';
    aoisumscell(isnan(cell2mat(aoisumscell(:,2))),:) = [];
    
    repeat = length(aoisumscell);
    trialrow = eventscell(i,:);
    rows = repmat(trialrow,[repeat,1]);
    rows = [rows,aoisumscell];
    aoidata = [aoidata;rows];
end

%% Add info to aoidata, cell2table and save as csv

genders = repmat({gender},[length(aoidata),1]);
IDs = repmat({ID},[length(aoidata),1]);
languages = repmat({language},[length(aoidata),1]);
analyses = repmat({analysis},[length(aoidata),1]);
participants = repmat({participant},[length(aoidata),1]);
groups = repmat({group},[length(aoidata),1]);

aoidata = [IDs,participants,analyses,groups,genders,languages,aoidata];
rownames = {'recording','participant','analysis','group','gender','language','condition','mark','trial','repetition','onset','offset','aoi','hits'};
aoidataT = cell2table(aoidata,'VariableNames',rownames);

savefile = strcat(participant,'_',ID,'_aoidata.csv');
writetable(aoidataT,fullfile(pathname_csv,savefile));
disp(['Saved ',savefile,'!']);
