% File:          recent.sl      -*- SLang -*-
% Provide easy access to recently opened/saved files.
%
% Version: 
% 1.0.1. by Guido Gonzato, <ggonza@tin.it>
% 
% 2.0    by Guenter Milde <g.milde@web.de>
%        *  Use a circular array -> no hidden recent buffer
%        *  Save to file only at exit -> less writing
%        *  Save last cursor position when saving a file
%        *  Support for "restore last session" (bug fixed)
%        *  Saving cursor position for open buffers at exit (report G. Gonzato)
% 2.1    15. Okt 2002   patch by   Paul Boekholt
%        *  correct linecounting in folds
%        *  goto_column -> goto_column_best_try
%        *  custom variable RECENT_FILES_EXCLUDE (by GM, based on PB)
%        *  if the line we jump to is in a fold, open it
% 2.2    12. 11. 2002
% 	 *  documentation error cleared (Joachim Schmitz)
% 	 *  local recent-files enabled (patch by Andree Borrmann)
% 	 *  restore_session moved to jed_startup_hooks so the custom settings
% 	    are valid when the last open files will be opened
% 	    (bug report Andree Borrmann)
% 	 *  auxiliary fun save_integer removed, as integer() is
% 	    already error save when feeded a string
% 2.2.1  *  if a file is already open, dont goto line (Paul Boekholt)
% 	    
% TODO: proper (tm) documentation of public functions and variables
%
% To activate the recent files feature, load recent.sl from your .jedrc,
% e.g. by including the following lines (of course uncommented):
%
% % Optionally set the custom variables, e.g. :
% variable RECENT_FILES_LIST = ".jedrecent";
% variable MAX_RECENT_FILES  = 10; 
% variable RECENT_FILES_EXCLUDE = ".*~"; % don't add backup files
% variable Local_Recent_List = 1; % store recent list in jed's working dir
% variable Restore_Last_Session = 1;    % reopen files from last session
% % (see also the defaults in the source code).
% 
% % Load recent.sl (assuming it is in the JED_LIBRARY path)
% require("recent");
%   
%   Warning: If you have a ".jedrecent" file from version 1.0.1 left,
%   versions 2.*  will give you errors. Deleting (or renaming) it will
%   solve the problem (reported by Joachim Schmitz).

% Last updated:  

% enable reevaluation without error for existing namespace: 
!if (_featurep("recent"))
  implements("recent");
else
  use_namespace("recent");

require("circle");  % "circular array" for the recent files list
autoload("fold_open_fold", "folding");

% --- custom variables: user-settable ----------------------------------

% The file to save the recent files list to.
% (Will be appended to Jed_Home_Directory if no absolute path is given.)
#ifdef IBMPC_SYSTEM
custom_variable("RECENT_FILES_LIST", "_jedrcnt");
#else
custom_variable("RECENT_FILES_LIST", ".jedrecent");
#endif

% do you want a local recent list? (stored in directory where jed was started)
% 0 no, 1 yes, -1 use, if already present. Toggle with recent_toggle_local().
custom_variable("Local_Recent_List", -1);

% The size of the Recent_Files array (won't change after loading recent.sl)
custom_variable("MAX_RECENT_FILES", 15);

% Which files shall not be added to the recent files list?
% Give a regexp pattern that is matched to the full filename.
custom_variable("RECENT_FILES_EXCLUDE", "^/tmp");

% Do you want to reopen the buffers that where open the last session?
custom_variable("Restore_Last_Session", 0);

% Deprecated: if recent-files-list is not wanted, don't evaluate this skript!
% custom_variable ("WANT_RECENT_FILES_LIST", 1);
if (__get_reference("WANT_RECENT_FILES_LIST") != NULL)
  if (@__get_reference("WANT_RECENT_FILES_LIST") == 0)
    error("Use RECENT_FILES_EXCLUDE to stop adding of files to the recent files list");

%Test _traceback=1;
%Test _debug_info=1;
%Test autoload("show", "diagnose"); % ! non standard jed mode !

% --- Variables -------------------------------------------------------

% Store filename and position
!if (is_defined("RecentMark_Type"))
typedef struct {
     file,    % full filename
     line,    % \_ Last editing point position
     column,  % /
     open,     % file was open when last session terminated (TRUE/FALSE)
} RecentMark_Type;

% a circular array of recent file marks
%    updated when loading or saving to a file
%    saved to RECENT_FILES_LIST on exit
%    restored from RECENT_FILES_LIST on startup
static variable Recent_Files =
  create_circ(RecentMark_Type, MAX_RECENT_FILES, "linear");

static variable Local_Recent_File = 
  dircat(getcwd(), extract_filename(RECENT_FILES_LIST));
% set local_session from -1 to 0, if no local recent file found
if (file_status(Local_Recent_File) != 1 and Local_Recent_List == -1 )
  Local_Recent_List = 0;

% --- Functions ----------------------------------------------------

% complete the recent-files file name with the path
static define get_recent_files_file_name ()
{
   % local session wanted:
   if (Local_Recent_List)
      return Local_Recent_File;

   variable file = RECENT_FILES_LIST;

   if (path_is_absolute (file))
     return file;
   
   variable dir = Jed_Home_Directory;
#ifdef IBMPC_SYSTEM
   if (dir == "" and getenv("TEMP") != NULL)
	dir = getenv("TEMP");
#endif
   file = dircat(dir,file);
   return file;
}

% Position opened file if it is found on the recent list
static define position_recent_file()
{
   variable n = 0, entry = @RecentMark_Type;
   foreach(circ_bget_values(Recent_Files))
     {
	entry = ();
	if (entry.file == buffer_filename()) {
	   goto_line(entry.line);
	   () = goto_column_best_try(entry.column);
	   loop(10)  % while (is_line_hidden) might cause an infinite loop!
	   {
	      !if(is_line_hidden)
		break;
	      fold_open_fold();
	   }
	   break;
	}
	n++;
     }
}

% reopen the files that were open in the last exited session of jed
static define restore_session()
{
   variable n = 0, entry = @RecentMark_Type;
   foreach(circ_bget_values(Recent_Files))
     {
	entry = ();
	if (entry.open)
	  find_file(entry.file);
	n++;
     }
}

% Load the list of recent files and initialize the Recent_Files array
static define load_recent_files_file ()
{
   variable file = get_recent_files_file_name ();
   variable str, entry = @RecentMark_Type;
   
   % reset the Recent Files
   Recent_Files = create_circ(RecentMark_Type, MAX_RECENT_FILES, "linear");

   () = read_file (file);
   variable curbuf = whatbuf();
   bob ();
   do
     {
	str = line_as_string();
	if (string_match(str, "^%", 1))
	  continue;
	str = strchop(str, ':', 0);
	if (str[0] == "")
	  continue;
	% show("processing line", str);
	% Backwards compatibility:
	if (length(str) < 4)
	  entry.file =   strjoin(str,":");
	else
	  entry.file =   strjoin(str[[0:-4]],":");
	entry.line =   integer(str[-3]); % third last element
	entry.column = integer(str[-2]); % last but one element
	entry.open =   integer(str[-1]); % last element
	circ_append(Recent_Files, @entry);
     }
   while (down_1);
   %Test show(".jedrecent evaluated:", Recent_Files);
   delbuf(whatbuf);
}

% Delete file from the list (if it is on)
static define delete_entry(file)
{
   variable entry = @RecentMark_Type, i = 0;

   foreach (circ_bget_values(Recent_Files))
     {
	entry = ();
	%Test show("Entry number:", Recent_Files.pointer, Entry);
	%Test show("Vergleich:", i, entry.file, file);
	if (file == entry.file)
	  {
	     circ_delete(Recent_Files, i);
	     return;
	  }
	i--;
     }
}

public define what_line_if_wide ()
{
  if (count_narrows ())
    {
      push_narrow ();
      widen_buffer ();
      what_line ();
      pop_narrow ();
    }
  else
    what_line ();
}

% This function will be called when loading or saving to a file
public define append_to_recent_files ()
{
   variable entry = @RecentMark_Type;
   % remove unwanted arguments from stack
   _pop_n (_NARGS);
   % find out the file name with full path
   entry.file = buffer_filename();
   % paranoia: abort, if there is no file associated with the current buffer
   !if (strlen(entry.file))
     return;
   % abort if file matches the exclude-pattern
   if (string_match(entry.file, RECENT_FILES_EXCLUDE, 1))
    return;
   entry.line =     what_line_if_wide();
   entry.column =   what_column();
   % delete entry, if it is already on the list,
   delete_entry(entry.file);
   % append the filename and position to the list
   circ_append(Recent_Files, entry, 1); % (1 = append at end)
   %Test show("file appended", Recent_Files);
}

% Save the list of recent files at exit
static define save_recent_files_file()
{
   variable file = get_recent_files_file_name();
   variable openfiles = Assoc_Type [String_Type];
   variable str, entry = @RecentMark_Type, is_open;

   setbuf("*recent_files*");
   insert("% List of files recently opened by JED,\n");
   insert("% do not edit manually (see recent.sl).\n");
   insert("% filename:line:col:lastsession\n");
   
   % generate associative array of open filenames with  matching buffer names
   loop (buffer_list())
   {
      variable buffile, bufdir, bufname, buf = ();
      (buffile, bufdir, bufname,) = getbuf_info(buf);
      if (buffile == "") % ignore buffers not connected to files
	continue;
      openfiles[path_concat(bufdir, buffile)] = bufname;
%      insert("% "+path_concat(bufdir, buffile)+" -> "+bufname+"\n");
   }
   
   foreach(circ_get_values(Recent_Files))
     {
	entry = ();
        %insert("% "+entry.file+"\n");
	
	% update the line/col info for the open buffers
	is_open = assoc_key_exists(openfiles, entry.file);
	if (is_open)
	  {
	     setbuf(openfiles[entry.file]);
	     entry.line =  what_line_if_wide();
	     entry.column =   what_column();
	     setbuf("*recent_files*");
	  }
	insert(sprintf("%s:%S:%S:%S\n",
		       entry.file, entry.line, entry.column, is_open));
     }
   () = write_buffer (file);
   delbuf(whatbuf);
   return 1;          % tell _jed_exit_hooks to continue
}

% Toggle the use of a local recent files file
public define recent_toggle_local()
{
   if (circ_length(Recent_Files))
     save_recent_files_file();                   % save the current state
   Local_Recent_List = not(Local_Recent_List);   % toggle
   variable dir; (, dir, , ) = getbuf_info();    % get current buffers dir
   Local_Recent_File = dircat(getcwd(), extract_filename(RECENT_FILES_LIST));
   load_recent_files_file();                     % load the new recent-file
   if (Local_Recent_List)
     message("Local recent list enabled");
   else
     message("Global recent list enabled");
}

% Build the menu of recent files.
public define recent_files_menu_callback (popup)
{
   variable i = '1'; % menu index: 1-9, then a-z, then A-Z, then restart
   variable menu, entry, name, n;

   for (n = 0; n < circ_length(Recent_Files); n++)
     {
	entry = circ_get(Recent_Files, -n);
	name = path_basename(entry.file);
	menu_append_item (popup, sprintf ("&%c %s", i, name),
			  &find_file, entry.file);
	% check - what should we use?
	switch (i)
	  { case '9': i = 'a'; }
	  { case 'z': i = 'A'; }
	  { case 'Z': i = '1'; }
	  {           i++;     }

     }
   menu_append_separator(popup);
   if (Local_Recent_List)
     menu_append_item (popup, "&Use global filelist" , "recent_toggle_local");
   else
     menu_append_item (popup, "&Use local filelist" , "recent_toggle_local");
}

static define add_recent_files_popup_hook (menubar)
{
   variable menu = "Global.&File";

   menu_append_separator (menu);
   menu_append_popup (menu, "&Recent Files");
   menu_set_select_popup_callback (strcat (menu, ".&Recent Files"),
				   &recent_files_menu_callback);
}


% debugging functions
static define sprint_recentmark(rm)  
{
   runhooks("sprint_struct", rm);
}
static define show_recent() 
{
   runhooks("show", Recent_Files);
}


% --- Code that gets executed when recent.sl is called -----------------

% Initialize the Recent_Files
load_recent_files_file();

% --- Hooks ---

% update Recent_Files when loading and saving a buffer
  % opening a file (no arguments, no return value)
append_to_hook("_jed_find_file_after_hooks", &position_recent_file);
append_to_hook("_jed_find_file_after_hooks", &append_to_recent_files);
  % saving to a file (one argument, no return value)
append_to_hook("_jed_save_buffer_after_hooks", &append_to_recent_files);

% Save the list of recent files at exit
append_to_hook("_jed_exit_hooks", &save_recent_files_file);

#ifexists _jed_run_hooks
append_to_hook ("load_popup_hooks", &add_recent_files_popup_hook);
#else
variable Menu_Load_Popups_Hook = &add_recent_files_popup_hook;
#endif

% Restore the last session
if (Restore_Last_Session and not(BATCH))
  add_to_hook("_jed_startup_hooks", &restore_session);

provide("recent");
% End of file recent.sl

