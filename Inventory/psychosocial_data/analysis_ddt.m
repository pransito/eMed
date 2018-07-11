%% Read in data from Delay Discounting Task and perform the following analyses:
% 1) Calculation of median switch point for every option of the standard item
% 2) Calculation of individual discount parameter
% 3) Performance of quality check based on two criteria (Johnson & Bickel, 2008):
%      - difference of switch points between option of particular delay and
%          following delay should not be higher than certain value
%      - switch point of longest delay should be below the switch point of
%          the shortest delay by a certain amount
% 4) Calculation of area under the curve as a theoretically neutral measure
%       measure of discounting (Myerson et al., 2001)



clear all;

%% Define variables

data_folder=...  % root folder containing log files
    'M:\eMed\Delay Discounting\Data';
sites={'Berlin';'Bonn';'Mannheim'};     % sites of data acquisition
res_folder='M:\eMed\Delay Discounting\Data';    % folder where DDT-structure should be saved
file_format=1;  % format of files to be read (1 - text-files; 2 - Excel files)

nr_trials=137;  % number of in trials in total
stand_opt_val=10;   % value of standard option
init_min=0.01;  % starting point for finding the minimum of the objective function

% options for quality check (see Johnson & Bickel 2008)
max_diff_delay=2;   % maximum difference between two successive delays
min_diff_delay=1;   % minimum difference between first and last delay

do_comp=1;          % 0 - reads in previously created DDT.mat-file; 1 - reads raw text files

do_plot=1;          % should data be plotted? (0 - no, 1 - yes)
plot_comp_app=2;    % based on which approach should switch points be plotted?
plotsize=[10 7];    % plot size in cm [width height]
plot_res=300;       % resolution of plot in dpi

%% Initialize structures

if do_comp
    
    for si=1:length(sites)
        DDT.site(si).name=sites{si};
    end
    
    
    %% Load data
    
    for si=1:length(sites)
        
        cd(fullfile(data_folder,sites{si}));
        file_dir=cd;
        
        % find Excel or text log files
        if file_format==1
            data_files=dir('*.txt');
        elseif file_format==2
            data_files=dir('*.xls');
        end
        
        for fi=1:length(data_files)
            
            % initialise variables
            DDT.site(si).subject(fi).filename=data_files(fi).name;
            DDT.site(si).subject(fi).delays_stand=zeros(nr_trials,1); % delay of the standard option
            DDT.site(si).subject(fi).value_alt=zeros(nr_trials,1);  % value of the alternative option
            DDT.site(si).subject(fi).response=zeros(nr_trials,1);   % option chosen by participant
            DDT.site(si).subject(fi).order=zeros(nr_trials,1);  % order of presentation of options
            DDT.site(si).subject(fi).RT=zeros(nr_trials,1); % reaction time
            
            % counting variables
            trial_count=1;  % counts number of trials
            
            % convert xls-file to txt-file as it otherwise cannot be read in
            % Matlab
            if file_format==2
                old_name=data_files(fi).name;
                new_name=[old_name(1:end-3) 'txt'];
                movefile(fullfile(file_dir,old_name),fullfile(file_dir, new_name));
            elseif file_format==1
                new_name=data_files(fi).name;
            end
            
            % read txt file line by line
            fileID=fopen(new_name);
            tline=fgetl(fileID);
            while ischar(tline)
                % stop if line contains the word 'Picture'
                if strfind(tline,'Picture')
                    linetext=strsplit(tline,'\t');
                    
                    % check if numbers follow the word 'Picture'
                    pic_ind=find(cellfun(@(x) strcmp(x,'Picture'),linetext));
                    if ~isempty(str2num(linetext{pic_ind+2}))
                        
                        % replace commas with dots (e.g. '10,00' -> '10.00')
                        linetext=cellfun(@(x) strrep(x,',','.'),linetext,...
                            'UniformOutput',false);
                        
                        % extract the relevant columns from this line
                        DDT.site(si).subject(fi).delays_stand(trial_count)=...
                            str2num(linetext{pic_ind+2});
                        DDT.site(si).subject(fi).value_alt(trial_count)=...
                            str2num(linetext{pic_ind+3});
                        DDT.site(si).subject(fi).order(trial_count)=...
                            str2num(linetext{pic_ind+4});
                        DDT.site(si).subject(fi).response(trial_count)=...
                            str2num(linetext{pic_ind+5});
                        DDT.site(si).subject(fi).RT(trial_count)=...
                            str2num(linetext{pic_ind+6});
                        
                        % increase trial count by 1
                        trial_count=trial_count+1;
                    end
                end
                
                % read next line
                tline=fgetl(fileID);
                
            end
            
            % close file
            fclose(fileID);
            
            % check if data are complete
            if trial_count==nr_trials+1
                data_complete=1;
            else data_complete=0;
                warning([DDT.site(si).subject(fi).filename ' is incomplete! Skipping further analysis.']);
                DDT.site(si).subject(fi).note='incomplete data';
            end
            
            
            % modify response vector such that 1 = selection of standard option
            % and 2 = selection of alternative option
            DDT.site(si).subject(fi).response=...
                DDT.site(si).subject(fi).response+1;
            
            
            %% Computation of median switch points
            
            if data_complete
                
                % arrange data for each standard option with 3 columns:
                % [standard option alternative option response RT]
                stand_opt=unique(DDT.site(si).subject(fi).delays_stand);
                
                ind_stand_opt=cellfun(@(x) ...
                    find(DDT.site(si).subject(fi).delays_stand==x),...
                    num2cell(stand_opt),'UniformOutput',false);
                DDT.site(si).subject(fi).blocks_stand_opt=...
                    cellfun(@(x) [DDT.site(si).subject(fi).delays_stand(x) ...
                    DDT.site(si).subject(fi).value_alt(x) ...
                    DDT.site(si).subject(fi).response(x) ...
                    DDT.site(si).subject(fi).RT(x)],ind_stand_opt,'UniformOutput',...
                    false);
                
                % sort rows by alternative option
                DDT.site(si).subject(fi).blocks_stand_opt=...
                    cellfun(@(x) sortrows(x,2), ...
                    DDT.site(si).subject(fi).blocks_stand_opt,'UniformOutput',false);
                
                % identify lower and upper switch points (i.e. the point above or
                % below which participant always or never decides for standard
                % option) and compute median switch point
                % lower switch point: first point at which participant prefers
                % alternative option
                % upper switch point: last point at which participant prefers
                % standard option
                lower_switch=cellfun(@(x) x(find(x(:,3)==2,1),2),...
                    DDT.site(si).subject(fi).blocks_stand_opt,'UniformOutput',false);
                emptycell=find(cellfun(@(x) isempty(x),lower_switch));
                if emptycell
                    for e=1:length(emptycell)
                        lower_switch{emptycell(e)}=...
                            max(DDT.site(si).subject(fi).blocks_stand_opt{emptycell(e)}(:,2));
                    end
                end
                lower_switch=cell2mat(lower_switch);
                
                upper_switch=cellfun(@(x) x(find(x(:,3)==1,1,'last'),2),...
                    DDT.site(si).subject(fi).blocks_stand_opt,'UniformOutput',false);
                emptycell=find(cellfun(@(x) isempty(x),upper_switch));
                if emptycell
                    for e=1:length(emptycell)
                        upper_switch{emptycell(e)}=...
                            min(DDT.site(si).subject(fi).blocks_stand_opt{emptycell(e)}(:,2));
                    end
                end
                upper_switch=cell2mat(upper_switch);
                
                DDT.site(si).subject(fi).approach(1).info=...
                    'Switch points based on first and last switches';
                DDT.site(si).subject(fi).approach(1).median_switch_points=...
                    [stand_opt median([upper_switch lower_switch],2)];
                
                
                % -------------------------------------------------
                % Additional/alternative approach:
                % Participants sometimes indicate a preference for the
                % alternative option at a single value and then switch back to
                % the standard option before ultimately and consistently
                % deciding for the alternative option at much higher values.
                % Such a single preference for the alternative option at low
                % values might be a mistake made by the participant, but they
                % can have dramatic influences on the calculation of the
                % switch points and often leads to implausible development of
                % switch points across delays. To compensate for this, in the
                % following switch points are calculated on the basis of the
                % second point at which the participant prefers the alternative
                % option and the second last point at which the participant the
                % standard option.
                % --------------------------------------------------
                lower_switch2=cellfun(@(x) x(find(x(:,3)==2,2),2),...
                    DDT.site(si).subject(fi).blocks_stand_opt,'UniformOutput',false);
                for c=1:length(lower_switch2)
                    if length(lower_switch2{c})<=1
                        lower_switch2{c}=...
                            max(DDT.site(si).subject(fi).blocks_stand_opt{c}(:,2));
                    else lower_switch2{c}=lower_switch2{c}(2);
                    end
                end
                lower_switch2=cell2mat(lower_switch2);
                
                upper_switch2=cellfun(@(x) x(find(x(:,3)==1,2,'last'),2),...
                    DDT.site(si).subject(fi).blocks_stand_opt,'UniformOutput',false);
                for c=1:length(upper_switch2)
                    if length(upper_switch2{c})<=1
                        upper_switch2{c}=...
                            min(DDT.site(si).subject(fi).blocks_stand_opt{c}(:,2));
                    else upper_switch2{c}=upper_switch2{c}(1);
                    end
                end
                upper_switch2=cell2mat(upper_switch2);
                
                DDT.site(si).subject(fi).approach(2).info=...
                    'Switch points based on second + second last switches';
                
                DDT.site(si).subject(fi).approach(2).median_switch_points=...
                    [stand_opt median([upper_switch2 lower_switch2],2)];
                
                
                % plot data
                if do_plot
                    figure;
                    plot(DDT.site(si).subject(fi).approach(plot_comp_app).median_switch_points(:,1),...
                        DDT.site(si).subject(fi).approach(plot_comp_app).median_switch_points(:,2),'ko');
                    xlabel('Length of delay (days)','FontWeight','bold')
                    ylabel('Value of alternative option (€)','FontWeight','bold');
                    set(gca,'XTick',stand_opt); hold on;
                    ylim([0 10.5]);
                end
                
                
                %% Computation of Impulsive Choice Ratio
                
                icr=sum(cellfun(@(x) length(find(x(:,3)==2)),...
                    DDT.site(si).subject(fi).blocks_stand_opt))/nr_trials;
                
                DDT.site(si).subject(fi).icr=icr;

                
                %% Computation of discount factor
                
                for ap=1:length(DDT.site(si).subject(fi).approach)
                    
                    x=DDT.site(si).subject(fi).approach(ap).median_switch_points(:,1);
                    y=DDT.site(si).subject(fi).approach(ap).median_switch_points(:,2);
                    
                    % hyperbolic function to be fitted to each participant's switch
                    % point data
                    % b = discount factor
                    % x = length of the delay of the standard option
                    % y = median switch points
                    F=@(b,x) stand_opt_val./(1+b(1).*x);
                    
                    % objective function (sum of squares of residuals)
                    FSS=@(b) sum((F(b,x)-y).^2);
                    
                    % find minimum of the objective function
                    [k,RSS]=fminsearch(FSS,init_min);
                    
                    % compute total sum of squares (TSS) to derive goodness of fit (R²)
                    % Note: for models without an intercept (as in this case), the
                    % total sum of squares is only the sum of y squared (and not the
                    % squared difference between y and mean of y)
                    TSS=sum(y.^2);
                    
                    % compute R²
                    R_squared=1-(RSS/TSS);
                    
                    % save data to DDT structure
                    DDT.site(si).subject(fi).approach(ap).k=k;
                    DDT.site(si).subject(fi).approach(ap).R_squared=R_squared;
                    
                    % add log-transformed k-parameter
                    DDT.site(si).subject(fi).approach(ap).log_k=log(k);
                    
                    
                    
                    %% Quality check
                    
                    % Compute the difference between two successive delays 
                    % (0 - failed quality check; 1 - passed quality check)
                    diff_delays=diff(DDT.site(si).subject(fi).approach(ap).median_switch_points(:,2));
                    if find(diff_delays>=max_diff_delay)
                        DDT.site(si).subject(fi).approach(ap).quality.max_delays=0;
                    else DDT.site(si).subject(fi).approach(ap).quality.max_delays=1;
                    end
                    
                    % compute difference between first and last delay
                    diff_firstlast=DDT.site(si).subject(fi).approach(ap).median_switch_points(1,2)-...
                        DDT.site(si).subject(fi).approach(ap).median_switch_points(end,2);
                    if diff_firstlast>=min_diff_delay
                        DDT.site(si).subject(fi).approach(ap).quality.min_delays=1;
                    else DDT.site(si).subject(fi).approach(ap).quality.min_delays=0;
                    end
                    
                    %% Computation of the area under the curve
                    
                    % normalization of delays
                    delay_norm=stand_opt./max(stand_opt);
                    
                    % normalization of values
                    value_norm=DDT.site(si).subject(fi).approach(ap).median_switch_points(:,2)./...
                        stand_opt_val;
                    
                    % compute area under the curve as the area under the
                    % trapezoids resulting from the area between the discount
                    % data and the x-axis
                    diff_delays=diff(delay_norm);
                    sum_values=(value_norm(1:end-1)+...
                        (value_norm(1:end-1)+diff(value_norm)))./2;
                    AUC=sum(diff_delays.*sum_values);
                    
                    DDT.site(si).subject(fi).approach(ap).AUC=AUC;
                    
                    % add fitted function to plot
                    if do_plot && plot_comp_app==ap
                        X=min(stand_opt):0.1:max(stand_opt);
                        plot(X,F(k,X),'k');
                        title(DDT.site(si).subject(fi).filename,'Interpreter','none');
                        text(300,8.5,['k = ' sprintf('%0.4f',k)]);
                        text(300,8,['R² = ' sprintf('%0.2f',R_squared)]);
                        text(300,7.5,['AUC = ' sprintf('%0.3f',AUC)]);
                        box off
                    end
                    
                    
                    
                end
                % save plot
                if do_plot
                    set(gcf,'PaperPositionMode','auto');
                    set(gcf,'PaperUnits','centimeters');
                    set(gcf,'PaperPosition',[1 1 plotsize]);
                    print(gcf,'-dtiff',['-r' num2str(plot_res)],...
                        DDT.site(si).subject(fi).filename(1:end-4));
                    
                    close(gcf);
                end
                
                disp(['Subject ' DDT.site(si).subject(fi).filename(1:end-4) ' done']);
                
            end
            
            
        end
    end
    
else cd(data_folder);
    load('DDT.mat');
end

%% Collect data across sites and participants

% define variable names
var_names{1}='site';
ct_vars=2;
var_names{ct_vars}='ICR';
ct_vars=ct_vars+1;
for ap=1:length(DDT.site(1).subject(1).approach)
    for sw=1:size(DDT.site(1).subject(1).approach(ap).median_switch_points,1)
        var_names{ct_vars}=['switch_point' ...
            num2str(DDT.site(1).subject(1).approach(ap).median_switch_points(sw,1)) ...
            '_approach' num2str(ap)];
        ct_vars=ct_vars+1;
    end
    var_names{ct_vars}=['k_approach' num2str(ap)];
    ct_vars=ct_vars+1;
    var_names{ct_vars}=['log_k_approach' num2str(ap)];
    ct_vars=ct_vars+1;
    var_names{ct_vars}=['R_squared_approach' num2str(ap)];
    ct_vars=ct_vars+1;
    var_names{ct_vars}=['quality_max_delays_approach' num2str(ap)];
    ct_vars=ct_vars+1;
    var_names{ct_vars}=['quality_min_delays_approach' num2str(ap)];
    ct_vars=ct_vars+1;
    var_names{ct_vars}=['AUC_approach' num2str(ap)];
    ct_vars=ct_vars+1;
end


% get total number of subjects
tot_subj=0;
for si=1:length(DDT.site)
    tot_subj=tot_subj+length(DDT.site(si).subject);
end

% prepare matrix for data storage
all_data=zeros(tot_subj,length(var_names));
all_data_id=cell(tot_subj,1);

row_ind=1;
nr_switch_points=size(DDT.site(1).subject(1).approach(1).median_switch_points,1);
for si=1:length(DDT.site)
    for su=1:length(DDT.site(si).subject)
        all_data_id{row_ind}=DDT.site(si).subject(su).filename;
        col_ind=1;
        all_data(row_ind,col_ind)=si;
        col_ind=col_ind+1;
        if ~isempty(DDT.site(si).subject(su).icr)
            all_data(row_ind,col_ind)=...
                DDT.site(si).subject(su).icr;
        else all_data(row_ind,col_ind)=NaN;
        end
        col_ind=col_ind+1;
        for ap=1:length(DDT.site(si).subject(su).approach)
            all_data(row_ind,col_ind:col_ind+nr_switch_points-1)=...
                DDT.site(si).subject(su).approach(ap).median_switch_points(:,2);
            col_ind=col_ind+nr_switch_points;
            all_data(row_ind,col_ind)=...
                DDT.site(si).subject(su).approach(ap).k;
            col_ind=col_ind+1;
            all_data(row_ind,col_ind)=...
                DDT.site(si).subject(su).approach(ap).log_k;
            col_ind=col_ind+1;
            all_data(row_ind,col_ind)=...
                DDT.site(si).subject(su).approach(ap).R_squared;
            col_ind=col_ind+1;
            all_data(row_ind,col_ind)=...
                DDT.site(si).subject(su).approach(ap).quality.max_delays;
            col_ind=col_ind+1;
            all_data(row_ind,col_ind)=...
                DDT.site(si).subject(su).approach(ap).quality.min_delays;
            col_ind=col_ind+1;
            all_data(row_ind,col_ind)=...
                DDT.site(si).subject(su).approach(ap).AUC;
            col_ind=col_ind+1;
        end
        row_ind=row_ind+1;
    end
end

% write data to Excel file
cd(res_folder);
var_names=[{'ID'} var_names];
xlsdata=[var_names; all_data_id num2cell(all_data)];
xlswrite('DDT.xls',xlsdata);

save(fullfile(res_folder,'DDT_matrix.mat'),'all_data','all_data_id','var_names');


%% Quality check of data

% provide a list of participants that do not fulfill quality criteria
% for ap=1:2






% save data
save(fullfile(res_folder,'DDT.mat'),'DDT');

