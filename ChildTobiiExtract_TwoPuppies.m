%% Extracts Frisbee/Chihuahua xy data from Tobii file - Rain's Child Study
% Adam Stone, October 2017
% The exported Tobii file needs to be converted from .xlsx to .csv first. 


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
participant = alldata.ParticipantName{1};

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

% Delete All But Frisbee and Chihuahua
avi = zeros(height(events),1);
for i = 1:length(avi)
    if strfind(events.MediaName{i}, '37c1fdb8')
        avi(i) = 1; % Chihuahua
    elseif strfind(events.MediaName{i}, 'd664cea0')
        avi(i) = 2; % Frisbee
    end
end
events.avi = avi;
events = events(find(events.avi==1 | events.avi==2),:);

%% Add Event Marks, Trialnum, Repetition, etc
marks = zeros(height(events),1);
for i = 1:length(marks)
    if strfind(events.MediaName{i}, '37c1fdb8')
        marks(i) = 1; % Chihuahua
    elseif strfind(events.MediaName{i}, 'd664cea0')
        marks(i) = 2; % Frisbee
    end
end

events.mark = marks;

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


%% Get X/Y columns
varnames = alldata.Properties.VariableNames';
xcol = find(ismember(varnames, 'GazePointX_MCSpx_'));
ycol = find(ismember(varnames, 'GazePointY_MCSpx_'));

xydata = [];


%% Loop through all puppy trials
trialnum = height(events);
% if strmatch(ID, 'kay 5y5m great')
%     trialnum = 3;
% end
% if strmatch(ID, 'Jelena CODA 4y,2m')
%     trialnum = 4;
% end
% if strmatch(ID, 'Julia CODA GOOD')
%     trialnum = 2;
% end
% if strmatch(ID, 'Lin11hy29_5m_NSE_GOOD')
%     trialnum = 4;
% end


for i = 1:trialnum
    xpos = alldata(events.onset(i):events.offset(i),xcol);
    ypos = alldata(events.onset(i):events.offset(i),ycol);
    xpos = table2cell(xpos);
    ypos = table2cell(ypos);
    xypos = cell2mat([xpos,ypos]);
    xypos = xypos(all(~isnan(xypos),2),:);
    sizexypos = size(xypos);
    markcol = repmat(events.mark(i),sizexypos(1),1);
    xypos = [markcol,xypos];
    xydata = [xydata;xypos];
end

%% Save xydata as csv
rownames = {'mark','xpos','ypos'};
xydataT = array2table(xydata,'VariableNames',rownames);
savefile = strcat(ID,'_TwoPuppiesData.csv');
writetable(xydataT,fullfile(pathname_csv,savefile));
disp(['Saved ',savefile,'!']);


%% Add info to aoidata, cell2table and save as csv
% 
% genders = repmat({gender},[length(aoidata),1]);
% IDs = repmat({ID},[length(aoidata),1]);
% languages = repmat({language},[length(aoidata),1]);
% analyses = repmat({analysis},[length(aoidata),1]);
% participants = repmat({participant},[length(aoidata),1]);
% groups = repmat({group},[length(aoidata),1]);
% 
% aoidata = [IDs,participants,analyses,groups,genders,languages,aoidata];
% rownames = {'recording','participant','analysis','group','gender','language','condition','mark','trial','repetition','onset','offset','aoi','hits'};
% aoidataT = cell2table(aoidata,'VariableNames',rownames);
% 
% savefile = strcat(ID,'_Frisbydata.csv');
% writetable(aoidataT,fullfile(pathname_csv,savefile));
% disp(['Saved ',savefile,'!']);
