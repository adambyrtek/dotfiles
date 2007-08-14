%% help.sl
%% edited by Guenter Milde <g.milde@web.de>
%%   Version 1.0
%%   - added Guido Gonzatos function_help (renamed to help_for_word_at_point)
%%   - when no documentation is available, give message in minibuffer
%%     instead of popping up a help window (customizable)
%%   - help-mode (Return and 2click: help_for_word_at_point,
%%     		  Q                  close help,
%%     		  TAB                go to next defined object
%%     		  W                  where is command
%%     		  ...)
%%   Version 1.1
%%   - set variable
%%   - grep the definition of a library function
%%   - mini-help: a one-line help string for display in the minibuffer
%%   - help_for_help (aka help.hlp)
%%   - documentation of global functions
%%   Version 1.2
%%   - use blocal_hooks for mode-dependent context help
%%   - better formatting of apropos output (optional, with csvutils.sl)
%%   - showkey with Key_* Variables
%%   - Help_Topic type replaced by array
%%   - save version of set_variable: ask if type changed
%%   Version 1.2.1
%%   - bugfix for describe_bindings and describe_mode
%%   
%% ------------------------------------------------------------------------  
%% How to use it:
%% 
%% Place help.sl, txtutils.sl, bufutils.sl in the "jed_library_path"
%% (use get_jed_library_path() to see what this is)
%% Optionally, place grep.sl, filelist.sl, and circle.sl in the path too
%% 
%% (I recommend a separate directory for the local extensions --
%%  thus they won't be overwritten by upgrading to a new jed version.
%%  See the home-lib mode for an example how to do this)
%% ------------------------------------------------------------------------

% enable reevaluation without error for existing namespace: 
!if (_featurep("help"))
  implements("help");
else
  use_namespace("help");

% --- variables for user customization ----------------------

% How big shall the help window be maximal
% (set this to 0 if you don't want it to be fitted)
custom_variable("Help_max_window_size", 0.7);
% enable history (using circ.sl)
custom_variable("Help_with_history", 1);
% for one line help texts, just give a message instead of open up a buffer
custom_variable("Help_message_for_one_liners", 0);
% Do you want full- or mini-help with help_for_word_at_point?
custom_variable("Help_mini_help_for_word_at_point", 0);
% The standard help file to display with help().
custom_variable("Help_file", "generic.hlp");


% --- Requirements ---------------------------------------------------------

% distributed with jed but not loaded by default
autoload("add_keyword_n", "syntax.sl"); 
require("keydefs");
% needed auxiliary functions, not distributed with jed
autoload("bget_word", "txtutils");
autoload("run_function", "txtutils");
autoload("get_blocal", "txtutils");
autoload("popup_buffer", "bufutils");
autoload("close_buffer", "bufutils");
autoload("strread_file", "bufutils");
autoload("buffer_dirname", "bufutils");
autoload("autoload_if_present", "bufutils");
autoload("custom", "bufutils");
  
% --- Optional helpers (not really needed but nice to have)
% As we cannot be sure, that these functions are present, we must
% use runhooks("fun", [args]) whenn calling them

% nice formatting of apropos list
autoload_if_present("list2table", "csvutils.sl");   % also needs datutils
autoload_if_present("write_table_to_string", "csvutils.sl");
autoload_if_present("array_max", "datutils.sl");

% Interface to the grep command: grep for the source code of library functions
autoload_if_present("grep", "grep.sl");
autoload_if_present("filelist_open_file", "filelist.sl");

% Help History: walk for and backwards in the history of help items
if (strlen(expand_jedlib_file("circle.sl")))
{
   autoload("create_circ", "circle");
   autoload("circ_previous", "circle");
   autoload("circ_next", "circle");
   autoload("circ_get", "circle");
   autoload("circ_append", "circle");
}
else
    Help_with_history = 0;


% --- static variables  ----------------------------------------------

% valid chars in function and variable definitions
static variable Slang_word_chars = "A-Za-z0-9_";

% The symbolic names for keystrings defined in keydefs.sl
% filled when needed by expand_keystring()
static variable Keydef_Keys;

static variable current_topic; % ["fun", "subject"]

% --- auxiliary functions --------------------------------

% dummy definitions for recursive use (the real ones are at the end of file)
define help_mode() {}
define help_for_object() {}

static define read_object_from_mini(prompt, flags)
{
   variable objs;

   if (MINIBUFFER_ACTIVE) return;

   objs = _apropos("Global", "", flags);
   objs = strjoin(objs[array_sort (objs)], ",");

   return read_string_with_completion(prompt, "", objs);
}

public define read_function_from_mini(prompt)
{
   read_object_from_mini(prompt, 0x3);
}

public define read_variable_from_mini(prompt)
{
   read_object_from_mini(prompt, 0xC);
}

% --- History ---

% The history stack

if(Help_with_history)
  static variable Help_History = 
    runhooks("create_circ", Array_Type, 30, "linear");

public define help_previous_topic() 
{
   ()= run_function(push_array(runhooks("circ_previous", Help_History)));
}

public define help_next_topic() 
{
   ()= run_function(push_array(runhooks("circ_next", Help_History)));
}


% Open a help buffer, insert str, set to help mode, and add to history list
define help_display(str)
{
   %  if help_str is just one line, display in minibuffer
   if (Help_message_for_one_liners and is_substr(str, "\n") == 0)
     return message(str); 

   setbuf("*help*");
   set_readonly(0);
   erase_buffer();
   TAB = TAB_DEFAULT; % in case it is set differently by apropos...
   insert(str);
   bob();
   popup_buffer(whatbuf(), Help_max_window_size);
   help_mode();
   if (Help_with_history)
     if (length(where(current_topic != runhooks("circ_get", Help_History))))
       runhooks("circ_append", Help_History, @current_topic);
}

% --- basic help -----------------------------------------------------

%!%+
%\function{help}
%\synopsis{Pop up a window containing a help file.}
%\usage{Void help ([help_file])}
%\description
% Displays help_file in the help buffer.
% The file read in is given by the optional argument or the 
% (custom) variable \var{Help_File}.
%\seealso{help_for_help, help_mode, Help_File}
%!%-
public define help() % (help_file=Help_File)
{
   % optional argument with default
   variable help_file=Help_File;
   if (_NARGS)
     help_file = ();

   !if (path_is_absolute (help_file))
     help_file = expand_jedlib_file(help_file);
   
   if (file_status(help_file) != 1)
     verror ("Help error: File %S not found", help_file);
   % get the file and display in the help buffer
   current_topic = [_function_name, help_file];
   help_display(strread_file(help_file));
}


%!%+
%\function{help_for_help}
%\synopsis{Display the help for help.}
%\usage{Void help_for_help()}
%\description
% Displays help.hlp in the help buffer.
%\seealso{help, help_mode}
%!%-
public define help_for_help() {help("help.hlp");}

%!%+
%\function{apropos}
%\synopsis{List all defined objects that match a regular expression}
%\usage{Void apropos ([search_str])}
%\description
%   Apropos searches for defined functions and variables in the 
%   global namespace that match a given regular expression. If the 
%   optional search-string is missing, the user will be prompted for
%   a pattern in the minibuffer.
%\seealso{help_mode, describe_function, describe_variable }
%!%-
public define apropos () % ([search_str])
{
   % get search string
   variable search_str;
   if (_NARGS)
     search_str = ();
   else
     {
	if (MINIBUFFER_ACTIVE) return;
	search_str = read_mini("apropos:", "", "");
     }

   variable a = _apropos("Global", search_str, 0xF);
   vmessage ("Found %d matches.", length(a));
   !if (length(a))
     a = ["No results for \"" + search_str + "\""];
   
   current_topic = [_function_name, search_str];
   if (strlen(expand_jedlib_file("csvutils.sl")))
     {
	% sort array of hits and transform into a 2d array
	a = runhooks("list2table", a[array_sort(a)]);
	help_display(runhooks("write_table_to_string", a));
     }
   else
     {
	variable help_str = strjoin(a[array_sort(a)], "\t");
	help_display(help_str);
	set_readonly(0);
	TAB = runhooks("array_max", array_map(Int_Type, &strlen, a));
	call ("format_paragraph");
	set_buffer_modified_flag(0);
	set_readonly(1);
	fit_window(Help_max_window_size, get_blocal("is_popup", 0));

     }
   
}

%  Substitute control-characters by ^@ ... ^_
define strsub_control_chars(key)
{
   variable a = bstring_to_array(key);
   variable control_chars = where(a <32);
   a[control_chars] += 64;  % shift to start at '@'
   a = array_map(String_Type, &char, a);
   a[control_chars] = "^" + a[control_chars];    
   return strjoin(a, "");
}
     
%!%+
%\function{expand_keystring}
%\synopsis{Expand a keystring to easier readable form}
%\usage{String expand_keystring (String key)}
%\description
% This function takes a key string that is suitable for use in a 'setkey'
% definition and expands it to a easier readable form
%       For example, it expands ^I to the form "\t", ^[ to "\e",
%       ^[[A to Key_Up, etc...
%\seealso{setkey}
%!%-
public define expand_keystring (key)
{
   variable keyname, keystring;
   % initialize the Keydef_Keys dictionary
   !if (__is_initialized (&Keydef_Keys))
     {
	Keydef_Keys = Assoc_Type[String_Type, ""];
	foreach (_apropos("Global", "^Key_", 0xC))
	  {
	     keyname = ();
	     keystring = strsub_control_chars(@__get_reference(keyname));
	     Keydef_Keys[keystring] = keyname;
	  }
     }
   % substitute control chars by ^@ ... ^_
   key = strsub_control_chars(key);
   % check for a symbolic keyname and return it if defined
   if (strlen(Keydef_Keys[key]))
     return Keydef_Keys[key];
   % two more readability replacements
   (key, ) = strreplace(key, "^I", "\\t", strlen (key));
   (key, ) = strreplace(key, "^[", "\\e", strlen (key));
   return key;
}

%!%+
%\function{showkey}
%\synopsis{Show a keybinding.}
%\usage{Void showkey([keystring])}
%\description
%   Ask for a key to be pressed and display its binding(s)
%   in the minibuffer.
%\seealso{where_is, help_for_help}
%!%-
public define showkey() % ([keystring])
{
   variable ks, f, type;
   if (_NARGS)
     {
	ks = ();
	(type, f) = get_key_binding(ks);
     }
   else
     {
	flush("Show Key: ");
	(type, f) = get_key_binding();
	ks = expand_keystring(LASTKEY);
     }
   
   variable help_str;
   variable description = [" is undefined.",
			   " runs the S-Lang function ",
			   " runs the internal function ",
			   " runs the keyboard macro ",
			   " inserts ",
			   " runs the intrinsic function "];

   if (type == 0 and is_defined(f) == 1)
     type = 4;

   if(is_substr(ks, "Key_"))
     help_str = ks;
   else
     help_str = strcat("Key \"", ks, "\"");
   
   help_str += description[type+1];
   if (f != NULL)
     help_str += f;

   current_topic = [_function_name, ks];
   help_display(help_str);
}


%!%+
%\function{where_is}
%\synopsis{Show which key(s) a command is on}
%\usage{Void where_is([String cmd])}
%\description
%   If no argument is given, ask for a command.
%   Show the key that is bound to it.   
%\seealso{get_key_binding, help_for_help}
%!%-
public define where_is ()
{
   variable cmd = "";
   if(_NARGS)
     cmd = ();
     
   if (cmd == "") 
     {
	if (MINIBUFFER_ACTIVE) return;
	cmd = read_function_from_mini("Where is command:");
     }

   variable n, help_str = cmd + " is on ";
   
   n = which_key (cmd);
   !if (n)
     help_str = cmd + " is not on any keys.";
   loop(n)
     help_str += expand_keystring () + "  ";
   help_str += "   Keymap: " + what_keymap();

   current_topic = [_function_name, cmd];
   help_display(help_str);
}

% which key is the word under cursor on
public define where_is_word_at_point()
{
   variable obj = bget_word(Slang_word_chars);
   if (is_defined(obj) > 0)
     where_is(obj);
   else
     where_is();
}

% return a string with a variables name and value
static define variable_value_str(name)
{
   variable vref, type = "Any_Type", value = "<Uninitialized>";
   if (is_defined(name) == 0)
     return "variable " + name + " undefined";
   if (is_defined(name) > 0)
     return name + " is a function";
   vref = __get_reference(name);
   if (__is_initialized(vref))
     {
	value = @vref;
	if (typeof(value) == String_Type)
	  value = "\"" + value + "\"";
	type = typeof(value);
     }
   return sprintf("%S %s == %S", type, name, value);
}

% return a string with help for function/variable obj
define help_for_object(obj)
{
   variable help_str = "", doc_str, file, vref;
   variable function_types = [": <Undefined>",
                              ": intrinsic function",
                              ": library function"];

   if (is_internal(obj))
     {                  
	help_str += obj + ": internal function\n"
	  + "Use call(\"" + obj 
	  + "\") to access the internal function from slang.\n"
	  + "You might bind an internal function to a key "
	  + "using setkey or definekey\n\n";
     }

   if (is_defined (obj) < 0) % Variables
     help_str += variable_value_str(obj);
   else
     help_str += obj + function_types[is_defined(obj)];

   % get doc string
   foreach (strchop(Jed_Doc_Files, ',', 0))
     {
	file = ();
	doc_str = get_doc_string_from_file (file, obj);
	if (doc_str != NULL)
	  break;
     }

   if (doc_str == NULL)
     help_str += "  Undocumented";
   else
     help_str += doc_str[[strlen(obj):]];
     % help_str += sprintf("[Obtained from file %s]", file);
      
   return help_str;
}

%!%+
%\function{describe_function}
%\synopsis{Give help for a jed-function}
%\usage{Void describe_function ()}
%\description
%   Display the online help for \var{function} in the 
%   help buffer.
%\seealso{describe_variable, help_for_help, help_mode}
%!%-
public define describe_function () % ([fun])
{
   variable fun;
   if (_NARGS)
     fun = ();
   else
     fun = read_function_from_mini("Describe Function:");
   
   current_topic = [_function_name, fun];
   help_display(help_for_object(fun));
}

%!%+
%\function{describe_variable}
%\synopsis{Give help for a jed-variable}
%\usage{Void describe_variable({var])}
%\description
%   Display the online help for \var{variable} in the 
%   help buffer.
%\seealso{describe_function, help_for_help, help_mode}
%!%-
public define describe_variable() % ([var])
{
   variable var;
   if (_NARGS)
     var = ();
   else
     var = read_variable_from_mini("Describe Variable:");
   
   current_topic = [_function_name, var];
   help_display(help_for_object(var));
}

%!%+
%\function{describe_mode}
%\synopsis{Give help for the current mode}
%\usage{describe_mode ()}
%\description
%   Display the online help for the current editing mode
%   in the help buffer.
%\seealso{describe_function, help_for_help, help_mode}
%!%-
public define describe_mode ()
{
   variable modstr = extract_element (get_mode_name, 0, ' ');
   if (modstr == "") 
     modstr = "no";
   !if (is_defined (modstr))
     {
	modstr = strlow (modstr);
	!if (is_defined (modstr))
	  {
	     modstr += "_mode";
	     % !if (is_defined (modstr))
	     %   error ("Mode is not defined: " +  modstr);
	  }
     }
   current_topic = [_function_name, modstr];
   help_display(help_for_object (modstr));
}

%!%+
%\function{describe_bindings}
%\synopsis{Show a list of all keybindings}
%\usage{Void describe_bindings ()}
%\description
%   Show a list of all keybindings in the help buffer
%\seealso{showkey, where_is, help_mode}
%!%-
public define describe_bindings ()
{
   _pop_n (_NARGS); % remove unwanted argument
   flush("Building bindings..");
   variable map = what_keymap ();
   variable buf = whatbuf();
   current_topic = [_function_name, ""];
   help_display("");
   set_readonly(0);
   insert("Keymap: " + what_keymap() + "\n");
   dump_bindings (map);
   bob;
   % TODO:
   %    variable old_case_search = CASE_SEARCH;  
   %    CASE_SEARCH = 1;
   % do
   %   expand_keystring;
   % while (down(1))
   % bob;
   set_buffer_modified_flag(0);
   set_readonly(1);
   fit_window(Help_max_window_size, get_blocal("is_popup", 0));
}

%!%+
%\function{grep_definition}
%\synopsis{Grep source code of definition}
%\usage{Void grep_definition([function])}
%\description
%   If the util grep.sl is installed, grep_definition does a
%   grep for the function definition in all directories of the
%   jed_library_path.
%\notes
%   Needs the grep.sl mode and the grep system command
%   
%\seealso{describe_function, grep, get_jed_library_path}
%!%-
public define grep_definition() % ([fun])
{
   if (strlen(expand_jedlib_file("grep.sl")) == 0)
       error("grep_definition needs grep.sl");
   % optional argument, ask if not given
   variable fun;
   if (_NARGS)
     fun = ();
   else
     fun = read_function_from_mini("Grep Definition:");

   if (is_defined(fun) != 2)
     if (get_y_or_n(fun + ": not a library function. Grep anyway?") != 1)
       return;

   % build the search string and filename mask
   variable what = " \"define " + fun + "[ (]\" ", where = "";
   variable dir = buffer_dirname();
   variable lib, libs = strchop(get_jed_library_path, ',', 0);
   
   foreach (libs)
     {
	lib = ();
	if (lib == ".")
	  lib = dir;
	lib = path_concat(lib, "*.sl ");
	!if (is_substr(where, lib)) % don't add a path twice
	    where += lib;
     }
   % run the grep command
   runhooks("grep", what, where);

   % find number of output lines
   variable results;
   eob; results = what_line(); bob;
   if(looking_at("No results"))
     results = 0;
   
   if(results == 1)
     runhooks("filelist_open_file");
   if(results < 2)
     delbuf("*grep_output*");
   message(sprintf("Grep for \"%s\": %d definition(s) found", fun, results));
}

% grep a function definition,
% defaults to the word under cursor or the current help topic
public define help_grep_definition()
{
   variable obj = bget_word(Slang_word_chars);
   if (is_defined(obj) == 2) % library function
     grep_definition(obj);
   else if (current_topic[0] == "describe_function")
     grep_definition(current_topic[1]);
   else
     grep_definition();
}

%!%+
%\function{set_variable}
%\synopsis{Set a variable value}
%\usage{Void set_variable() % ([name])}
%\description
%   Set a variable to a new value, define the variable if undefined.
%   If the current word is no variable, ask for the variable name. 
%   The new value must be a valid slang expression that will be evaluated 
%   in the global namespace.
%   
%   WARNING: Setting variables to unsensible values might cause jed
%            to stop working
%\seealso{eval, read_variable_from_mini}
%!%-
public define set_variable()
{
   variable name = bget_word(Slang_word_chars);
   !if (is_defined(name) == -2) % user defined variable
     {
	if (andelse {whatbuf()=="*help*"}
	      {current_topic[0] == "describe_variable"})
	  name = current_topic[1];
	else
	  name = read_variable_from_mini("Set Variable:");
     }

   variable new = 0, var, value, def_string = "";
   
   % ensure var is globally defined
   if (is_defined (name) == 0)
     {
	new = 1;
	if (get_y_or_n("Variable "+name+" undefined, define: ") > 0)
	  eval("variable " + name);
	else
	  error("set_variable: Aborted");
     }
   % get pointer
   var = __get_reference(name);
   % evaluate value
   value = read_mini(variable_value_str(name) + " New value:","","");
   value = eval(value);
   % check for same datatype
   if (andelse {__is_initialized(var)} {typeof(@var) != typeof(value)})
     if ( get_y_or_n(
	  sprintf("Variable %s: change datatype from %S to %S?", 
		name, typeof(@var), typeof(value)) ) != 1)
       error("set_variable: Aborted");
   % now set the variable
   @var = value;
   % display new value
   message(variable_value_str(name));
}

%!%+
%\function{extract_mini_doc}
%\synopsis{extract the USAGE and SYNOPSIS lines of the current buffer}
%\usage{ Void extract_mini_doc()}
%\description
%   extract the USAGE and SYNOPSIS lines of the current buffer
%   (if the buffer contains a Jed Help Text of jedfuns.txt, say)
%   convert to a more concise format and return as string
%\seealso{make_mini_doc, help_for_function}
%!%-
public define extract_mini_doc()
{
   variable synopsis, usage, str = "";
   push_spot();
   bob;

   while (bol_fsearch(" SYNOPSIS"))
     {
	go_down_1;
	synopsis = strtrim(line_as_string ());
	go_down(2);
	if (looking_at(" USAGE"))
	  {
	     go_down_1;
	     usage = strtrim(line_as_string ());
	  }
	else
	  continue;
	% Replacements:
	!if (is_substr(usage, "="))
	  (usage, ) = strreplace (usage, " ", " = ", 1);
	% strip Void/void
	(usage, ) = strreplace (usage, "Void = ", "", 70);  % 70 <-> strlen (usage)
	(usage, ) = strreplace (usage, "void = ", "", 70);
	% simple types -> small letters
	(usage, ) = strreplace (usage, "Integer_Type", "n", 70);
	(usage, ) = strreplace (usage, "Int_Type", "i", 70);
	(usage, ) = strreplace (usage, "Integer", "i", 70);
	(usage, ) = strreplace (usage, "Double_Type", "x", 70);
	(usage, ) = strreplace (usage, "Double", "x", 70);
	% compound types -> capital letters
	(usage, ) = strreplace (usage, "String_Type", "Str", 70);
	(usage, ) = strreplace (usage, "String", "Str", 70);
	(usage, ) = strreplace (usage, "Array_Type", "Arr ", 70);
	(usage, ) = strreplace (usage, "Array", "Arr", 70);
	(usage, ) = strreplace (usage, "Assoc_Type", "Ass", 70);
	(usage, ) = strreplace (usage, "Assoc", "Ass", 70);
	% append ";" if not already there
	!if(usage[[strlen(usage)-1:]] == ";")
	  usage += ",";

	str += usage + "   " + synopsis;
     }
   pop_spot();
   return str;
}


%!%+
%\function{mini_help_for_object}
%\synopsis{Show concise help information in the minibuffer}
%\usage{Void mini_help_for_object(obj)}
%\description
%   Show the synopsis of the online help in the minibuffer.
%\seealso{describe_function, describe_variable}
%!%-
public define mini_help_for_object(obj)
{
   variable help_str = help_for_object(obj);
   if (is_substr(help_str, "\n") == 0 )
     return help_str;
   setbuf (make_tmp_buffer_name("*mini-help*"));
   insert (help_str);
   help_str = extract_mini_doc();
   set_buffer_modified_flag(0);
   delbuf(whatbuf());
   return help_str;
}


%!%+
%\function{help_for_word_at_point}
%\synopsis{Give (mode dependend) help for word under cursor}
%\usage{Void help_for_word_at_point ()}
%\description
%   Find the word under the cursor and give mode-dependend help.
%\seealso{describe_function, describe_variable}
%!%-
public define help_for_word_at_point ()
{
   variable word_at_point = bget_word(Slang_word_chars);
   % try blocal hook first
   if (run_function(get_blocal("help_for_word_hook", NULL), word_at_point))
     return;
   % default
   if (word_at_point == "")
     error("don't know what to give help for");
   describe_function(word_at_point);
}

public define help_2click_hook (line, col, but, shift)
{
   help_for_word_at_point();
   return (0);
}

% --- fast moving in the help buffer (link to link skipping)

% goto next word that is a defined function or variable
public define help_goto_next_object ()
{
   variable current_word;
   variable circled = 0;  % prevent infinite loops
   do
     {
	skip_chars(Slang_word_chars);
	skip_chars("^"+Slang_word_chars);
	skip_chars("\n");                    % "\n" is not part of "^a-z"????
	skip_chars("^"+Slang_word_chars);
	if (eobp)
	  if (circled == 0)
	    {bob; circled = 1;}
	else
	  return;
	  % error("no defined objects (other than current topic) in buffer");
	current_word = bget_word(Slang_word_chars);
     }
   while(0 == is_defined(current_word) or current_topic[1] == current_word);
}

% goto previous word that is a defined function or variable
public define help_goto_prev_object ()
{
   variable current_word;
   variable circled = 0;  % prevent infinite loops
   do
     {
	bskip_chars(Slang_word_chars);
	% Why is "\n" not part of "^a-z"????
	bskip_chars("^"+Slang_word_chars); bskip_chars("\n"); bskip_chars("^"+Slang_word_chars);
	bskip_chars(Slang_word_chars);
	if (bobp)
	  if (circled == 0)
	    {eob; circled = 1;}
	else
	  return;
	  % error("no defined objects (other than current topic) in buffer");
	current_word = bget_word(Slang_word_chars);
     }
   while(0 == is_defined(current_word) or current_topic[1] == current_word);
}

% --- "syntax" highlighting of "links" (defined objects)
create_syntax_table("Help");
set_syntax_flags("Help", 0);
define_syntax("0-9a-zA-Z_", 'w', "Help");       % Words
define_syntax('"', '"', "Help");                % Strings
% keywords will be added by the function help_mark_keywords()

% mark all words that match defined objects
static define help_mark_keywords()
{
   variable word;
   push_spot(); 
   bob();
   do
     {
 	word = bget_word(Slang_word_chars);
	if( is_defined(word) > 0)      % function
	  add_keyword("Help", word);
	if( is_defined(word) < 0)      % variable
	  add_keyword_n("Help", word, 2); 
	skip_chars(Slang_word_chars);
	skip_chars("^"+Slang_word_chars);  % skip all that is not a Slang_word_chars
	skip_chars("\n");
	skip_chars("^"+Slang_word_chars);
     }
   while(not(eobp));
   pop_spot();
}


% --- A dedicated mode for the help buffer -------------------------------

% Keybindings (customize with help_mode_hook)
$2 = "Help_Map";
!if (keymap_p ($2)) make_keymap ($2);
definekey ("help_for_word_at_point",  "^M",        $2);  % Return
definekey ("help_goto_next_object",   "^I",        $2);  % Tab
definekey ("close_buffer", 	      "\d155",     $2);  % "meta-escape"
definekey ("help_goto_prev_object", Key_Shift_Tab, $2);
definekey ("apropos", 		      "a",         $2);
definekey ("help_grep_definition",    "d",         $2);
% definekey ("toggle_readonly; text_mode", "e",         $2);  % "edit"
definekey ("describe_function",	      "f",         $2);
definekey ("help",	      	      "h",         $2);
definekey ("info_mode",	      	      "i",         $2);
definekey ("showkey",	      	      "k",         $2);
definekey ("unix_man",	      	      "u",         $2);
definekey ("close_buffer", 	      "q",         $2);  % quit
definekey ("set_variable",	      "s",         $2);
definekey ("describe_variable",	      "v",         $2);
definekey ("where_is_word_at_point",  "w",         $2);
definekey ("help_for_help",  	      "?",         $2);
if(Help_with_history)
{
   definekey ("help_next_topic",       Key_Alt_Right, $2); % Browser-like
   definekey ("help_previous_topic",   Key_Alt_Left,  $2); % Browser-like
}

help_for_help_string = 
"-> Apropos Fun Definition Key Help Info Set-variable Unix-man Var Where ?";

define help_mode()
{
   set_readonly(1);
   set_buffer_modified_flag(0);
   set_mode("Help", 0);
   use_keymap ("Help_Map");
   help_mark_keywords();
   use_syntax_table ("Help");
   set_buffer_hook ("mouse_2click", &help_2click_hook);
   define_blocal_var("help_for_word_hook", "describe_function");
   define_blocal_var("generating_function", current_topic);

   set_status_line(sprintf( "  %s   %s: %s  ", 
		   whatbuf, current_topic[0], current_topic[1]), 0);
   run_mode_hooks("help_mode_hook");
   message(help_for_help_string);
}

provide("help");
