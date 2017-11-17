%% Extracts XY information from Tobii file for Rain's Child Study
% Adam Stone, October 2017
%
% The script, when run in Matlab, will output a table of AOI
% looking times for every trial. The exported Tobii file needs to be
% converted from .xlsx to .csv first. 


% close all
% clear all

%% Setting Up
% Get and import file
% disp('Get the .csv file')
% [csvfile, pathname_csv] = uigetfile('.csv','Open the converted CSV file')
% warning('off','MATLAB:table:ModifiedVarnames');
alldata = readtable(fullfile(pathname_csv,csvfile));

% Get ID, group, gender, etc. 
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

% Find Participant Name (different for Group 1 v. Group 2
varnames = alldata.Properties.VariableNames';
for i = 1:length(varnames)
    if strfind(varnames{i},'ParticipantName')
        varaoi(i) = 1;
    else varaoi(i) = 0;
    end
end
varaoi = varaoi';
participantcol = min(find(varaoi==1));
participant = alldata{1,participantcol};
participant = participant{1};

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
    if strfind(varnames{i},'GazePointX_MCSpx_')
        varaoi(i) = 1;
    else varaoi(i) = 0;
    end
end
varaoi = varaoi';
xcol = min(find(varaoi==1));
ycol = xcol + 1; 

xy = alldata(:,xcol:ycol);
xydata = [];


%% Loop through all trials
trialnum = height(events);

for i = 1:trialnum
    trialsection = xy(events.onset(i):events.offset(i),:);
    rownum = height(trialsection);
    rowtorepeat = [events.MediaName(i), events.mark(i), events.trial(i),...
        events.rep(i), events.onset(i), events.offset(i)];
    repeatedrows = repmat(rowtorepeat, rownum, 1);
    xydatacell = table2cell(trialsection);
    xydatacell = [repeatedrows, xydatacell];
    xydata = [xydata; xydatacell];
end


%% Add info to aoidata, cell2table and save as csv

genders = repmat({gender},[length(xydata),1]);
IDs = repmat({ID},[length(xydata),1]);
languages = repmat({language},[length(xydata),1]);
analyses = repmat({analysis},[length(xydata),1]);
participants = repmat({participant},[length(xydata),1]);
groups = repmat({group},[length(xydata),1]);

xydata = [IDs,participants,analyses,groups,genders,languages,xydata];
rownames = {'recording','participant','analysis','group','gender','language','condition','mark','trial','repetition','onset','offset','x','y'};
xydataT = cell2table(xydata,'VariableNames',rownames);

savefile = strcat(ID,'_xydata.csv');
writetable(xydataT,fullfile(pathname_csv,savefile));
disp(['Saved ',savefile,'!']);
