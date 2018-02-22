%% Loops through csv files in a folder
% Adam Stone, October 2017

clear all
close all

% Select folder
disp('Select the folder');
pathname_csv = uigetdir();
csvfiles_all = dir(fullfile(pathname_csv,'*.csv'));

% Loop
filenum = length(csvfiles_all);
warning('off','MATLAB:table:ModifiedVarnames');
for i = 1:filenum
    csvfile = csvfiles_all(i).name;
    SignGesture_TobiiExtractloop; % Change this function for what you need
end