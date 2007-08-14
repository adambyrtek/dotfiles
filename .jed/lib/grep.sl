% file grep.sl
% -*- mode: SLang; mode: fold -*-
%
% A function wich tries to get a file to open from the current line
% in current buffer. This was created to follow grep-output lines. Now does
% a lot more, and can recognise grep-like output (with
% and without line numbers, and if everything else fails, a simple file name.
%
% Guenter Milde contributed a separate mode for grep output, now you can
% even point and click with the mouse to choose a result! Futhermore, he
% also wrote a function to grep for something without leaving jed.
%
% a powerful example of how to use this is doing:
% fgrep -n "sometext" * | jed -f grep_mode
% go to a line looking promising and press ENTER. (or double click on
% the line)
% You should have grep_mode in your autoload list for this to work.
%
% If you are already running jed, start a search invoking the grep()
% function.
%
% You can customize the grep_mode by the use of "grep_mode_hook".
% E.g. if you have defined a function that closes the buffer and the  window
% it was in (if there was more than one window), the following snippet in
% your setup file (.jedrc, say) would set 'q' to call this function:
%
% define grep_mode_hook ()
% {
%    local_setkey ("close_buffer", "q");
% }
%
% _debug_info = 1;
% _traceback=1;

implements("Grep");

require("keydefs");

% Variables %{{{

% Directories: where to search.
custom_variable("Grep_Dirs", ".");
custom_variable("Include_Braket_Dirs", "/usr/include,.");
custom_variable("Include_Quote_Dirs", ".,./include,../include,./source,../source,./src,../src");
% the grep command
custom_variable ("GrepCommand", "egrep -n");


% remember the string to grep (as default for the next run) (cf. LAST_SEARCH)
static variable Grep_String = "";
% quasi constants
static variable JGMode = "Grep";
%}}}


% Follow commands. Those returns the filename and line number. (filename == NULL
% if match not found)
%{{{

% a follow_include helper.
static define find_include_match(match, Incl_Dirs)
{
        variable file, FG_Path;

        go_right_1();
        push_mark();
        () = ffind(match);
        file = bufsubstr();

        foreach (strchop(Incl_Dirs, ',', 0)) {
                FG_Path = expand_filename(path_concat((), file));
                if (file_status(FG_Path) == 1)
                        return FG_Path;
        }
        return NULL;
}

define follow_include()
{
        variable filename = NULL;

        push_spot();
        bol();

        skip_white();

        if (looking_at_char('#')) {
                go_right_1();
                skip_white();
                if (looking_at("include")) {
                        go_right(7);
                        skip_white();
                        if (looking_at_char('<'))
                                filename = find_include_match(">", Include_Braket_Dirs);
                        else if (looking_at_char('"'))
                                filename = find_include_match("\"", Include_Quote_Dirs);
                }
        }

        pop_spot();
        return (filename, 0);
}

define follow_grep()
{
        variable name;
        variable numline = 0;
        variable filename = NULL;

        push_spot();
        bol();

        push_mark();
        skip_chars("^:\n");

        if (looking_at_char(':')) {
                name = bufsubstr();
                go_right_1();
                push_mark();
                skip_chars("[0-9]");
                numline = integer(bufsubstr());

                foreach (strchop(","+Grep_Dirs, ',', 0)) {
                        filename = expand_filename(path_concat((), name));
                        if (file_status(filename) != 1)
                                filename = NULL;
                        else
                                break;
                }

        } else
                pop_mark(0);

        pop_spot();
        return (filename, numline);
}

define follow_list()
{
        variable filename;
        variable found = -1;

        push_spot();
        bol();
        push_mark();        % poppato da bufsubstr()
        skip_chars("^\t\n");
        filename = bufsubstr();
        pop_spot();
        if (strlen(filename) == 0)
                filename = NULL;
        else {
           filename = expand_filename(filename);
           if (file_status(filename) != 1)
                filename = NULL;
        }
        return (filename, 0);
}

define follow_file()
{
        variable goodchars = "^\t\n ()[]{}\"\'\$;:,";
        variable filename;

        push_spot();
        bskip_chars(goodchars);
        push_mark();
        skip_chars(goodchars);
        filename = bufsubstr();
        pop_spot();
        if (strlen(filename) == 0)
                filename = NULL;
        else if (file_status(expand_filename(filename)) != 1)
                filename = NULL;

        return (filename, 0);
}

%}}}

% Internal helpers %{{{
static define homedir_to_tilde()
{
        variable Home = getenv("HOME");
        variable HomeLen;
        if (Home == NULL)
                return;
        HomeLen = strlen(Home);
        if (HomeLen == 0)
                return;

%        bob();
        while (bol_fsearch(Home)) {
                replace_chars(HomeLen, "~");
        }
}

% Actions %{{{
% Forward reference...
public define grep_follow();

% Mouse bindings
define grep_mouse_2click_hook (line, col, but, shift)
{
   grep_follow();
   return (0);
}

% Update hook to move line mark.
static variable JGLine = NULL;
static define grep_update_hook()
{
   JGLine = create_line_mark(color_number("menu_selection"));
}

static variable Marks = @struct { mark, next };
Marks.mark = NULL; %  Sentinel
Marks.next = NULL;
static define _find_mark(n)
{
	variable m, l;
	
	foreach(Marks) {
		m = ();
		if (m.mark != NULL) {
			push_spot();    % Why I can access .line and .column of a Mark?
			goto_user_mark(m.mark);
			l = what_line();
			pop_spot();
			if (l == n)
				return m;
		}
	}
	return NULL;
}

public define grep_tag_line()
{
	variable t;
	
	t = _find_mark(what_line());
	if (t == NULL) {      
		t = @struct { mark, next };
		t.mark = create_line_mark(color_number("region"));
		t.next = Marks;
		Marks = t;
	} else {
		t.mark = t.next.mark;
		t.next = t.next.next;
	}      
}

define grep_remove_tagged()
{
	variable m, found = 0;
	% Assuming i'm already on the right buffer
	
	foreach(Marks) {
		m = ();
		if (m.mark != NULL) {
			goto_user_mark(m.mark);
			bol;
			call("kill_line");
			found = 1;
		}
	}
	Marks.mark = NULL;
	Marks.next = NULL;
	
	return found;
}

%%%  Debug ON
static define get_mark_info(m)
{
	variable b, l, c;
	
	push_spot();
	whatbuf();
	b = user_mark_buffer(m);
	setbuf(b);
	goto_user_mark(m);
	l = what_line();
	c = what_column();
	setbuf(());
	pop_spot();
	return (b, l, c);
}

define _dump_mark()
{
	variable m, b, l, c, i;
	
	whatbuf();
	setbuf("*mdump*");
	i = 1;
	foreach (Marks) {
		m = ();   
		if (m.mark != NULL) {
			(b, l, c) = get_mark_info(m.mark);
			vinsert("%02d: \"%s\" \t % 6d:% 3d\n", i, b, l, c);
		}
		i++;
	}
}
%%%% Debug OFF

public define grep_remove_selected_lines()
{
        set_readonly(0);
	ERROR_BLOCK {
		set_buffer_modified_flag(0);
		set_readonly(1);
		_clear_error();
	}
	if (grep_remove_tagged() == 0) {
		if (markp()) {
			narrow;
			mark_buffer;
			call("kill_region");
			widen;
		} else
			bol();
		% I should kill the void line left by kill_region...
		call("kill_line");
	}
	EXECUTE_ERROR_BLOCK;
}

%}}}

% Mode stuff %{{{
create_syntax_table(JGMode);
!if (keymap_p (JGMode)) make_keymap (JGMode);

definekey ("grep_tag_line",              "t",     JGMode);  % Tag
definekey ("grep_tag_line; go_down_1",   Key_Ins, JGMode);         % MC like
definekey ("grep_tag_line; go_down_1",   Key_Shift_Down, JGMode);  % CUA style
definekey ("grep_tag_line; go_up_1",     Key_Shift_Up,   JGMode);  % CUA style
definekey ("grep_follow",                "^M",    JGMode);  % Return
definekey ("delbuf(whatbuf)",               "q",     JGMode);
definekey ("delbuf(whatbuf)",               "\d155", JGMode);  % "meta-escape"
definekey ("grep_remove_selected_lines", "d",     JGMode);
definekey ("grep_remove_selected_lines", Key_Del, JGMode);  % CUA style

#ifdef HAS_DFA_SYNTAX
%%% DFA_CACHE_BEGIN %%%
static define setup_dfa_callback (name)
{
        dfa_enable_highlight_cache("grep.dfa", name);
        dfa_define_highlight_rule("^[^:]*:", "keyword", name);
        dfa_define_highlight_rule("[0-9]+:", "number", name);
        % Uhm, this matches numbers followed by ':' anywhere. If this really
        % annoys you, either:
        % 1 - disable dfa highlighting
        % 2 - comment the two dfa_define_highlight_rule above an use instead:
%       dfa_define_highlight_rule("^[^:]*:[0-9]+:", "keyword", name);

   dfa_build_highlight_table(name);
}
dfa_set_init_callback (&setup_dfa_callback, JGMode);
%%% DFA_CACHE_END %%%
enable_dfa_syntax_for_mode(JGMode);
#endif

%}}}

% Interface: %{{{
public define grep_follow()
{
        variable numline;
        variable filename;

        (filename, numline) = follow_include();

        if (filename == NULL)
                (filename, numline) = follow_grep();

        if (filename == NULL)
                (filename, numline) = follow_list();

        if (filename == NULL)
                (filename, numline) = follow_file();
        else {
                () = read_file(expand_filename(filename)); % create buffer but don't pop up window
                goto_line(numline);
                goto_column(0);
	   pop2buf(path_basename(filename));
        }
}

% a mode dedicated to the follow_grep command
public define grep_mode ()
{
   homedir_to_tilde();
   set_buffer_modified_flag (0); % so delbuf doesnot ask whether to save first
   set_readonly(1);
   set_mode(JGMode, 0);
   use_keymap (JGMode);
   use_syntax_table (JGMode);
   set_buffer_hook ( "mouse_2click", &grep_mouse_2click_hook);
   set_buffer_hook("update_hook", &grep_update_hook);
   run_mode_hooks("grep_mode_hook");
}

% Main function
public define grep()
{
   variable searchstr, search_where, bufDir, fullCmd;
   variable allfiles = "";
#ifdef UNIX
   allfiles = "*";
#elifdef IBMPC_SYSTEM
   allfiles = "*.*";
#endif
% TODO: What does gnu grep expect on DOS, What should this be on VMS and OS2 ?

   Grep_String = read_mini("(Flags and) String to grep: ", Grep_String, "");

   searchstr = read_with_completion ("Where to grep: ", "", allfiles, 'f');

   % append wildcard, if no filename (or pattern) is given
   if (path_basename(searchstr) == "")
     searchstr = path_concat(searchstr, allfiles);
   % To execute the command in the active buffer's path,
   % set the default dir to the active buffers dir
   (, bufDir, ,) = getbuf_info();
   () = change_default_dir(bufDir);
   % The output buffer will inherit the active buffer's path as well. 
   % So the path is redundant and we can strip it. 
   % (Note: this maight fail on case insensitive filesystems).
   if (is_substr(searchstr, bufDir))
     searchstr = searchstr[[strlen(bufDir):]];

   fullCmd = GrepCommand+" "+Grep_String+" "+searchstr;

   pop2buf("*grep_output*");
   set_readonly(0);
   erase_buffer();

   shell_perform_cmd (fullCmd, 1);
   
   bob;
   insert("% Command: " + fullCmd + "\n");
   grep_mode();
}

%}}}

provide("grep");

