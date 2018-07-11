function data_comb

% Get data from DDT and combine them with data from quetionnaires and
% demographic data
% TODO: make explicit where the data lies; or change the location because
% now it is on a remote disk

clear

%% define variables

% data files
ddt_file='M:\eMed\Delay Discounting\Data\DDT.xls'; % output file from DDT-analysis
data_files={'M:\eMed\Delay Discounting\Data\NGFN_Berlin.xlsx';...
    'M:\eMed\Delay Discounting\Data\NGFN_Bonn.xlsx';...
    'M:\eMed\Delay Discounting\Data\NGFN_Mannheim.xlsx'};   % Excel files containing all other data from all three sites
invent_file='M:\eMed\Delay Discounting\Data\ngfn_inventory.xlsx';    % NGFN invenoty file

data_file_Bquest='M:\eMed\Delay Discounting\Data\NGFN_Bonn_new.xlsx'; % questionnaire data from Bonn


% output
output_name='Data_all';     % name of output file
output_path='M:\eMed\Delay Discounting\Data';   % path of output file

% headers indicating ID columns in SPSS files
id_header={'ADD_P','ID','ID_initials_ngfn'};
id_headers_B_match={'mp','ID'}; % headers to match IDs in the two SPSS files from Bonn

% headers indicating site in inventory file
id_header_invent={'ID'};
site_header={'site'};
site_order_ddt={'Berlin','Bonn','Mannheim'};

% variables from inventory file
invent_var{1}='ID';
invent_var{2}='Co.ID';
invent_var{3}='sex';
invent_var{4}='site';
invent_var{5}='scanner';
invent_var{6}='Drop.Out';
invent_var{7}='relapse';

% Other variables from SPSS files
% the following information has to be specified:
% 1) headers of other variables (note: variables can have different names in
%   the files coming from the different sites -> different variants for the
%   same variable should be defined in these cases)
%   var(1).name{1} -> variable 1
%   var(1).name{2} -> alternative name for variable 1
%   var(2).name{1} -> variable 2
%   ...
% 2) header for output file
%   var(1).header -> header for variable 1
%   ...
% 3) if necessary recode variable (e.g. to have the same type of coding across
%   sites)
%   Recode vector has to include three numbers: [site old new]
%   E.g.: var(1).recode=[2 1 0] -> for site 2, recode ones into zeros
var(1).name{1}='Notes';
var(1).name{2}='Drop_out_Grund';
var(1).header='Notes';
var(2).name{1}='Diagnose';
var(2).name{2}='group';
var(2).recode=[1 102 1; 2 1 0; 2 2 1];
var(2).header='Group';
var(3).name{1}='age';
var(3).name{2}='alter_berechnet';
var(3).header='Age';
var(4).name{1}='sex';
var(4).header='Sex';
var(4).recode=[3 2 1; 3 1 2];
var(5).name{1}='graduation';
var(5).name{2}='graduati';
var(5).header='Graduation';
var(6).name{1}='Raucherstatus';
var(6).name{2}='Smoking';
var(6).name{3}='FAG_ges';
var(6).header='Smoking';
var(6).recode=[2 2 0];
var(7).name{1}='AUDIT_ges';
var(7).name{2}='AUDIT_TS';
var(7).header='AUDIT';
var(8).name{1}='LDH_total';
var(8).header='LDH';
var(9).name{1}='BIS_ges';
var(9).name{2}='BIS_TOTAL';
var(9).name{3}='BIS_TS';
var(9).header='BIS';
var(10).name{1}='BIS_AI';
var(10).header='BIS_AI';
var(11).name{1}='BIS_MI';
var(11).header='BIS_MI';
var(12).name{1}='BIS_NI';
var(12).header='BIS_NI';
var(13).name{1}='NEO_N';
var(13).header='NEO_N';
var(14).name{1}='NEO_E';
var(14).header='NEO_E';
var(15).name{1}='NEO_O';
var(15).header='NEO_O';
var(16).name{1}='NEO_V';
var(16).header='NEO_V';
var(17).name{1}='NEO_G';
var(17).header='NEO_G';
var(18).name{1}='SSSV_Dis';
var(18).name{2}='SSSV_DIS';
var(18).header='SSSV_Dis';
var(19).name{1}='SSSV_TA';
var(19).header='SSSV_TA';
var(20).name{1}='SSSV_E';
var(20).header='SSSV_E';
var(21).name{1}='TCI_HA';
var(21).header='TCI_HA';
var(22).name{1}='TCI_NS';
var(22).header='TCI_NS';
var(23).name{1}='TCI_RD';
var(23).header='TCI_RD';




%% load DDT data

% load DDT-data
[data_ddt,header_ddt]=xlsread(ddt_file);

% get IDs from DDT-file (and remove header)
id_ddt=header_ddt(:,1);
id_ddt(1)=[];

% get site information 
site_ddt=data_ddt(:,1);

% remove the part of the filename following "DDT" to get subject IDs
id_ddt=cellfun(@(x) x(1:strfind(x,'_DDT')-1),id_ddt,'UniformOutput',false);


%% load data from inventory
[invent_data,invent_txt]=xlsread(invent_file);

% get IDs -> all data will be ordered and organized according to this
% variable
id_invent=cell2mat(cellfun(@(x) find(strcmp(invent_txt(1,:),x)),...
    id_header_invent,'UniformOutput',false));
id_invent=invent_txt(:,id_invent(1));


% get sites
site_invent=cell2mat(cellfun(@(x) find(strcmp(invent_txt(1,:),x)),...
    site_header,'UniformOutput',false));
site_invent=invent_txt(:,site_invent(1));

% remove headers
id_invent(1)=[]; 
site_invent(1)=[];

% transform site variable to numeric variable
site_invent=cellfun(@(x) find(strcmp(x,site_order_ddt)),site_invent);


%% preallocate variables

% all_data -> cell array to which data from DDT and other variables will be
% stored
all_data=cell(size(id_invent,1),length(var));
% all_headers -> cell array containing headers for the data stored in
% "all_data"
all_headers=cell(1,length(var));

%% match data from DDT file to data from inventory file  

% match IDs from DDT file to IDs from inventory file
id_match_ddt=match_ids(id_invent,id_ddt,site_invent,site_ddt);

% reorder DDT data such that they are in the same order as data from
% inventory file
ddt_data_match=zeros(length(id_match_ddt),size(data_ddt,2));
id_ddt_match=cell(length(id_match_ddt),1);

for s=1:length(id_match_ddt)
    
    if id_match_ddt(s)~=0
        ddt_data_match(s,:)=data_ddt(id_match_ddt(s),:);
        id_ddt_match(s)=id_ddt(id_match_ddt(s));
    end
end



%% get data from inventory file

data_col_invent=cellfun(@(x) find(strcmp(invent_txt(1,:),x)),invent_var);

%get values from each column
data_invent=cell(size(invent_data,1),length(data_col_invent));
for di=1:length(data_col_invent)
    data_invent(:,di)=invent_txt(2:end,data_col_invent(di));
end



%% load other data from the experiment

% load Excel file from each site
for f=1:length(data_files)
    [data_exp,txt_exp,raw_data]=xlsread(data_files{f});
    
    % check whether table starts with columns containing strings, in this
    % case the matrix "data_exp" has to be adjusted as this only starts
    % with the first column in which there are numbers. If the first (or
    % more) columns are strings, then the values in "data_exp" won't match 
    % the headers.
    raw_txt_match=cellfun(@(x,y) strcmp(num2str(x),num2str(y)),...
        raw_data,txt_exp);
    raw_txt_match_sum=sum(raw_txt_match);
    text_col=raw_txt_match_sum==size(data_exp,1)+1;
    col_to_add=find(text_col==0,1,'first')-1;
    
    % prepend zeros to "data_exp" when the table starts with string columns
    if col_to_add
        data_exp=[zeros(size(data_exp,1),col_to_add) data_exp];
    end
    
    % since there are two SPSS files from Bonn (one containing only
    % questionnaire data) combine them to one matrix
    if f==2
        % load questionnaire data
        [data_exp_quest,txt_exp_quest,raw_data_quest]=xlsread(data_file_Bquest);
        
        % get IDs from both files
        id_col_bonn1=cell2mat(cellfun(@(x) find(strcmp(txt_exp_quest(1,:),x)),...
            id_headers_B_match,'UniformOutput',false));
        id_bonn1=data_exp_quest(:,id_col_bonn1(1));
        id_col_bonn2=cell2mat(cellfun(@(x) find(strcmp(txt_exp(1,:),x)),...
            id_headers_B_match,'UniformOutput',false));
        id_bonn2=data_exp(:,id_col_bonn2(1));
        
        % match the IDs from both files
        id_match_bonn=match_ids(id_bonn1,id_bonn2);
        
        % If there are more columns in the text output from reading in the 
        % Excel files (i.e. "txt_exp" and "txt_exp_quest"), this is due to 
        % columns containing only strings. In these cases, append the 
        % numeric with NaNs, so that text and numeric ouput match (e.g. for 
        % headers).
        data_exp_quest=[data_exp_quest nan(size(data_exp_quest,1),...
            size(txt_exp_quest,2)-size(data_exp_quest,2))];
        data_exp=[data_exp nan(size(data_exp,1),size(txt_exp,2)-...
            size(data_exp,2))];
        
        % reorder the data from the second file to match the order from the
        % first file
        data_exp_reord=zeros(size(data_exp));
        txt_exp_reord=cellstr('');
        txt_exp_reord=repmat(txt_exp_reord,size(txt_exp,1)-1,size(txt_exp,2));
        for r=1:length(id_match_bonn)
            if id_match_bonn(r)~=0
            for c=1:size(data_exp_reord,2)
                data_exp_reord(r,c)=data_exp(id_match_bonn(r),c);
                txt_exp_reord{r,c}=txt_exp{id_match_bonn(r)+1,c};
            end
            end
        end
        
        data_exp=[data_exp_quest data_exp_reord];
        txt_exp_reord=[txt_exp(1,:); txt_exp_reord];
        txt_exp=[txt_exp_quest txt_exp_reord];
        
    end
        
    
    % find column containing participant IDs
    id_col=cell2mat(cellfun(@(x) find(strcmp(txt_exp(1,:),x)),id_header,...
        'UniformOutput',false));
    
    % extract participant IDs (and remove header)
    id_exp=txt_exp(:,id_col(1));
    id_exp(1)=[];
    
    % for site Bonn, there are multiple IDs in the SPSS files, construct
    % the ones that match the inventory file
    if f==2
        % since some IDs are numbers and others are strings, combine them
        % both into one string vector
        id_exp1=id_exp;
        for i=1:length(id_exp1)
            if ~isempty(id_exp1{i})
                length_code=length(id_exp1{i});
                if length_code==5
                    id_exp1{i}=['00' id_exp1{i}];
                elseif length_code==6
                    id_exp1{i}=['0' id_exp1{i}];
                end
            end
        end
        
        id_exp2=data_exp(:,id_col(1));
        
        miss_id_exp2=find(isnan(id_exp2));
        id_exp2=num2cell(id_exp2);
        id_exp2=cellfun(@(x) sprintf('%03.0f',x),id_exp2,'UniformOutput',false);
        id_exp2(miss_id_exp2)=id_exp1(miss_id_exp2);
        id_exp=id_exp2;
        
        id_exp=cellfun(@(x) ['ADD_P_' x],id_exp,'UniformOutput',false);
    end
    
    
    % get header
    header_exp=txt_exp(1,:);
    txt_exp(1,:)=[];
    
    % match IDs from SPSS file to inventory file
    id_match_exp=match_ids(id_invent,id_exp,site_invent,f*ones(size(id_exp)));

    
    
    % extract other variables from Excel file
    for v=1:length(var)
        % find column
        var_col=cell2mat(cellfun(@(x) find(strcmpi(header_exp,x)),...
            var(v).name,'UniformOutput',false));
        
        % if there is more than one match, choose the first one
        if length(var_col)>1
            var_col=min(var_col);
        end
        
        % get values from that column
        if sum(strcmp(txt_exp(:,var_col),'') | ...
                strcmp(txt_exp(:,var_col),' '))==length(txt_exp(:,var_col))
            val_var=data_exp(:,var_col);
        else val_var=txt_exp(:,var_col);
        end
        
        % recode variable if necessary
        if isfield(var(v),'recode') && ~isempty(var(v).recode) && ...
                ismember(f,var(v).recode(:,1))
            val_var_rec=NaN(size(val_var));
            recode_var=var(v).recode(var(v).recode(:,1)==f,:);
            % replace old with new values in temporary variable
            for rec=1:size(recode_var,1)
                val_var_rec(val_var==recode_var(rec,2))=recode_var(rec,3);
            end
            % change values accordingly in main vector
            val_var(~isnan(val_var_rec))=val_var_rec(~isnan(val_var_rec));
        end
        
        
        % assign values to participants
        for s=1:length(id_invent)
            if id_match_exp(s)>0
                if isnumeric(val_var)
                    all_data{s,v}=val_var(id_match_exp(s));
                else all_data{s,v}=val_var{id_match_exp(s)};
                end
            end
        end
            
        
        % add header
        if f==1
            all_headers{v}=var(v).header;
        end
        
    end
    
    
end

% combine DDT-data and data from SPSS-files as well as from inventory file 
all_data=[data_invent all_data id_ddt_match num2cell(ddt_data_match)];
% add headers
all_data=[invent_var all_headers header_ddt(1,:); all_data];

%% save as Excel file
xlswrite(fullfile(output_path,[output_name '.xls']),all_data);

end

function [id_match]=match_ids(id_ref,id_adjust,fmatch_ref,fmatch_adjust)

% match IDs between two files
% Inputs (both cell arrays):
% - id_ref -> ID list from reference file, which will be used for matching
% - id_adjust -> IDs from this list will be matched to "id_ref"
% - fmatch (optional) -> further variable used for matching


% for site Mannheim split IDs into strings with length of 4 (otherwise
% matching does not work), for other sites with length of 3
if exist('fmatch_adjust','var') && length(unique(fmatch_adjust))==1 && ...
        fmatch_adjust(1)==3
    id_splitsize_start=5;
else id_splitsize_start=3;
end

% convert ID lists to cells
if ~iscell(id_ref)
    id_ref=num2cell(id_ref);
end
if ~iscell(id_adjust)
    id_adjust=num2cell(id_adjust);
end

% convert numeric IDs to strings
if sum(cellfun(@isnumeric,id_ref))
    id_ref=cellfun(@(x) num2str(x),id_ref,'UniformOutput',false);
end
if sum(cellfun(@isnumeric,id_adjust))
    id_adjust=cellfun(@(x) num2str(x),id_adjust,'UniformOutput',false);
end

% predefine "id_match" vector
id_match=zeros(length(id_ref),1);

% For site Bonn, all IDs in the inventory file start with "ADD_P_", in the
% other files some participants are labeled with "ADD_P". Change all these
% instances to "ADD_P_".
rename_ind=find(cellfun(@(x) ~isempty(strfind(x,'ADD_P')) & ...
    isempty(strfind(x,'ADD_P_')),id_adjust));
for rn=1:length(rename_ind)
    id_adjust{rename_ind(rn)}=['ADD_P_' id_adjust{rename_ind(rn)}(6:end)];
end


% go through ID reference list and try to find occurrences of IDs in the 
% other file 
for s=1:length(id_match)
    id_ind=find(cellfun(@(x) ~isempty(strfind(x,id_ref{s})),id_adjust));
    
    % if possible, check whether IDs come from the same site
    if ~isempty(id_ind) && length(id_ind)==1 && ...
            exist('fmatch_ref','var') && exist('fmatch_adjust','var')
        if fmatch_ref(s)~=fmatch_adjust(id_ind)
            id_ind=[];
        end
    end
    
    % if the ID from the reference list has not been found in the other
    % list, split up the ID into subparts and search for them
    if isempty(id_ind)
        stayinloop=1; id_splitsize=id_splitsize_start;
        while stayinloop
            id_splitind=1;
            while stayinloop && id_splitsize<length(id_ref{s}) && ...
                    (id_splitind+id_splitsize-1)<=length(id_ref{s})
                id_searchstr=id_ref{s}(id_splitind:id_splitind+...
                    id_splitsize-1);
                id_ind=find(cellfun(@(x) ~isempty(strfind(x,...
                    id_searchstr)),id_adjust));
                
                % restrict matches to matches from same site
                if exist('fmatch_ref','var') && exist('fmatch_adjust','var')
                    id_ind=id_ind(fmatch_adjust(id_ind)==fmatch_ref(s));
                end
                
                if ~isempty(id_ind) && length(id_ind)==1
                    stayinloop=0;
                else id_splitind=id_splitind+1;
                end
            end
            
            % end loop if maximum number of searches has been applied and
            % no match has been found
            id_splitsize=id_splitsize+1;
            if id_splitsize==length(id_ref{s})
                stayinloop=0;
            end
        end
    end
    
    % write index of ID to "id_match" vector, otherwise throw out warning
    % message
    if ~isempty(id_ind) && length(id_ind)==1
        id_match(s)=id_ind;
    else warning([' No match found for ' num2str(id_ref{s})]);
    end
    
end


end
