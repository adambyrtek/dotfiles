% bufutils.sl  Tools for buffer and windows handling
% by Guenter Milde <g.milde@web.de>
% Version 1.0  first public version
% Version 1.1  bugfix: restore_buffer now resets the "changed on disk" flag
% Version 1.2  new: "blocal_hooks"
% Version 1.2.1 new: push_array

%!%+
%\function{push_defaults}
%\synopsis{Push the n last of the args to the stack}
%\usage{(a1, ..., an) = push_defaults(a1, ..., am, n)}
%\description
% Helps to define a slang function with optional arguments.
%\example
%#v+
% define fun() % (a1, a2, a3="d3", a4="d4")
% {
%    % get the arguments
%    variable a1, a2, a3, a4;
%    (a1, a2, a3, a4) = push_defaults( , , "d3", "d4", _NARGS);
%    % now use them
%    show(a1,a2,a3,a4);
% }
%#v-
%\seealso{__push_args, __pop_args, _NARGS }
%!%-
public define push_defaults() % args, n
{
   variable n = ();
   variable args = __pop_args(_NARGS-1);
   __push_args(args[[n:]]);
}

% error save implement of a namespace (just use if already existent)
define _implements(name)
{
   ERROR_BLOCK 
     { 
	if(get_y_or_n("Namespace " + name + " exists. Use it?") == 1)
	  _clear_error; use_namespace(name);
     }
     implements(name);
}
				 
define autoload_if_present(fun, file)
{
   if(strlen(expand_jedlib_file(file)))
     autoload(fun, file);
}

%!%+
%\function{get_blocal}
%\synopsis{return value of blocal variable or default value}
%\usage{Any get_blocal (String name, [Any default=NULL])}
%\description
% This function is similar to get_blocal_var, but if the local variable 
% "name" doesnot exist, it returns the default value instead of an error.
% Default defaults to NULL.
%\example
%#v+
%    if (get_blocal(foo), 0)
%      message("this buffer is fooish");  
%#v-
% will print the message if foo is a blocal variable with nonzero value.
%\seealso{get_blocal_var, blocal_var_exists}
%!%-
define get_blocal() % (name, default=NULL)
{
   variable name, default;
   (name, default) = push_defaults( , NULL, _NARGS);
     
   if (blocal_var_exists(name))
     return get_blocal_var(name);
   return default;
}

%!%+
%\function{buffer_dirname}
%\synopsis{Return the directory associated with the buffer}
%\usage{Str buffer_dirname()}
%\description
%   Return the directory associated with the buffer}
%\seealso{getbuf_info, buffer_filename}
%!%-
define buffer_dirname()
{
   variable dir, args = __pop_args(_NARGS);
   ( , dir, , ) = getbuf_info(__push_args(args));
   return dir;
}

% --- window operations ----------------------------------------------

%!%+
%\function{fit_window}
%\synopsis{fits the window size to the lenght of the buffer}
%\usage{ Void fit_window () % fit_window(max_rows = NULL)}
%\description
% the optional parameter max_rows gives the maximal size of the window,
% either as proportion of the total space or as fix number of lines.
% An argument of 0 means: don't fit
%\seealso{enlargewin, popup_buffer}
%!%-
define fit_window () % fit_window(max_rows = NULL)
{
   variable max_rows;
   max_rows = push_defaults(NULL, _NARGS);
   
   if (max_rows == 0)
     return;
   % convert max_rows from fraction to absolute if Double_Type:
   if (typeof(max_rows) == Double_Type)
	max_rows = int(SCREEN_HEIGHT-3 * max_rows);
   % get the desired number of rows (lines in the actual buffer or max_rows)
   push_spot();
   eob;
   variable wanted_rows = what_line;
   pop_spot();
   % limit to max_rows
   if(max_rows != NULL)
     if (wanted_rows > max_rows)
       wanted_rows = max_rows;
   % fit window
   if(wanted_rows >= SCREEN_HEIGHT-3)
     onewindow();
   variable misfit = wanted_rows - window_info('r');
   if (misfit > 0) { % window too small
      loop(misfit)
	enlargewin ();
   }
   if (misfit < 0) { % window too large
      variable curbuf = whatbuf();
      otherwindow;
      loop(-misfit)
	enlargewin ();
      pop2buf(curbuf);
   }
   recenter(what_line);
}

%!%+
%\function{close_buffer}
%\synopsis{close the current (or given) buffer}
%\usage{ Void close_buffer([close_window = 0, buf = whatbuf()])}
%\description
%   close the current (or given) buffer and also the containing window, if
%    blocal variable is_popup == 1 or
%    optional arg close_window != 0 
%\seealso{delbuf, close_window, popup_buffer}
%!%-
define close_buffer() % (close_window = 0, buf = whatbuf())
{
   % get optional argument(s)
   variable close_window, buf;
   (close_window, buf) = push_defaults(0, whatbuf(), _NARGS);
   if (typeof(close_window) == String_Type)
     {
	buf = close_window;
	close_window = 0;
     }
   % close also popup windows
   if (blocal_var_exists("is_popup"))
     close_window += 1;

   % close only, if the buffer is visible and active
   if (close_window > 0 and buffer_visible(buf) and buf == whatbuf())
     {
	call("delete_window");
     }
   delbuf(buf);
   % resize popup windows
   fit_window(get_blocal("is_popup", 0));
}

% close buffer in second window if there are two windows
define close_other_buffer ()
{
   if (nwindows () - MINIBUFFER_ACTIVE < 2)
     return;
   otherwindow();
   close_buffer();
}

%!%+
%\function{next_buffer}
%\synopsis{Goto next buffer}
%\usage{ Void next_buffer ()}
%\description
%   Cycle among the open buffers.
%   This function is taken from mouse.sl (mouse_next_buffer).
%\seealso{sw2buf, pop2buf}
%!%-
define next_buffer ()
{
   variable n, buf, cbuf = whatbuf ();

   n = buffer_list ();		       %/* buffers on stack */
   loop (n)
     {
	buf = ();
	n--;
	if (buf[0] == ' ') continue;
	sw2buf (buf);
	loop (n) pop ();
	return;
     }
}

% open buffer, preserve the number of windows currently open
define go2buf(buf)
{
   if(buffer_visible(buf))
     pop2buf(buf);   % open in other window
   else
     sw2buf(buf);    % open in current window
}

% --- "Popup Buffer" -----------------------------------------------------

custom_variable("Max_Popup_Size", 0.7);          % max size of one popup window
% Todo:
% custom_variable("Popup_max_popups", 2);        % max number of popup windows
% custom_variable("Popup_max_total_size", 0.7);  % max size of all popup windows

% a "popup buffer": 
%   - open in a new window, if there is enough space
%   - fit window, if there is only one window before 
%     or the new buffer replaces a popup_window
%     and if the buffer is not empty 
%     -> fill it with setbuf(buf), insert(str) or read_file(file)
%     	 before calling popup_buffer(buf) if you want to use this feature
%     	 or use fit_window explicitely
%   - closing with close_buffer closes the popup window as well.
% 
% The blocal variable is_popup marks a buffer as "popup buffer". 
% It contains the upper limit when fitting the window.
define popup_buffer() % (buf, max_rows = Max_Popup_Size)
{
   % get arguments
   variable buf, max_rows;
   (buf, max_rows) = push_defaults( , Max_Popup_Size, _NARGS);
   % currently open windows
   variable open_windows = nwindows() - MINIBUFFER_ACTIVE;
   % go to the buffer
   if (blocal_var_exists("is_popup"))
     sw2buf(buf);
   else
     pop2buf(buf);
   % fit window
   if (open_windows == 1 or 
       (open_windows == 2 and blocal_var_exists("is_popup")))
     {
	define_blocal_var("is_popup", max_rows);
	!if(bobp and eobp) % not empty
	  fit_window(max_rows);
     }
}

% --- push_keymap/pop_keymap ---------------------------------------------

static variable stack_name = "keymap_stack";

% temporarily push the keymap  
define push_keymap(new_keymap)
{
   !if (blocal_var_exists (stack_name))
     define_blocal_var (stack_name, "");

   set_blocal_var(what_keymap()+"|"+get_blocal_var(stack_name), stack_name);
   
   use_keymap(new_keymap);
   variable mode, flag;
   (mode, flag) = what_mode();
   set_mode(mode + " (" + new_keymap + ")", flag);
   %Test show("keymap stack is:", get_blocal_var(stack_name));
   %Test show("current keymap is:", what_keymap());
}

define pop_keymap ()
{
   variable kstack = get_blocal_var(stack_name);
   variable oldmap = extract_element (kstack, 0, '|');
   if (oldmap == "")
     error("keymap stack is empty.");
   
   variable mode, flag;
   (mode, flag) = what_mode();
   set_mode(mode[[0:-(strlen(what_keymap)+4)]], flag);
   use_keymap(oldmap);
   set_blocal_var(kstack[[strlen(oldmap)+1:]], stack_name);
  %Test show("keymap stack is:", get_blocal_var(stack_name));
  %Test	show("current keymap is:", what_keymap());
}

% rebind all keys that are bound to old_fun to new_fun
define rebind() % (old_fun, new_fun, keymap=what_keymap())
{
   variable old_fun, new_fun, keymap=what_keymap();
   (old_fun, new_fun, keymap) = push_defaults( , , what_keymap(), _NARGS);
   
   variable key;
   loop (which_key (old_fun))
   {
      key = ();
      definekey(new_fun, key, keymap);
   }
}

% -----------------------------------------------------------------------

% Read a file and return it as string
define strread_file(name)
{
   variable fp = fopen (name, "r");
   variable line, str = "";
   if (fp == NULL) verror ("File %s not found", name);
   while (-1 != fgets (&line, fp))
     str += line;
   () = fclose (fp);
   return str;
}

% restore (or update, if file changed on disk) a buffer to the file version
define restore_buffer()
{
   variable file = buffer_filename();
   variable col = what_column(), line = what_line();

   if(file_status(file) != 1) 
     return message("cannot open " + file);
   delbuf(whatbuf());
   () = find_file(file);
   goto_line(line);
   goto_column(col);
   % turn off the "changed on disk" bit
   % cf. example set_overwrite_mode () in the setbuf_info help
   setbuf_info (getbuf_info () & ~0x004);  
}


%!%+
%\function{run_function}
%\synopsis{Run a function if it exists.}
%\usage{Int_Type run_function(fun, [args])}
%\description
% Run a function if it exists. Return whether it exists or not
% The function can be given by name or by reference (this allows both:
% yet undefined function (as string) as well as static functions 
% (as reference)
% Any arguments following the function argument will be passed to the 
% function. 
%\example
%#v+
%
%    !if (run_function("foo"))
% 	message("\"foo\" is not defined");
% 	
%    !if (run_function(&foo))
% 	message("\"foo\" is not defined");
%#v-
%\notes
% If fun is an internal function, the optional arguments will
% be popped.
%\seealso{runhooks, run_blocal_hook}
%!%-
define run_function()  % (fun, [args])
{
   variable args = __pop_args(_NARGS-1);
   variable fun = ();
   if (typeof(fun) == String_Type)
     {
	if (is_defined(fun) > 0)
	  fun = __get_reference(fun);
	else if (is_internal(fun))
	  {
	     call(fun);
	     return 1;
	  }
     }
   if (typeof(fun) == Ref_Type)
     {
	@fun(__push_args(args));
	return 1;
     }
   return 0;
}

%!%+
%\function{push_array}
%\synopsis{Push an ordinary array on stack}
%\usage{(a[0], ..., a[-1])  push_array(Array a)}
%\description
% Push an ordinary array on the stack. This works like
% __push_args(args) but with an ordinary array of any type.
%\example
%#v+
%   variable hooka = [&message, "hello world"];
%   runhooks(push_array(hooka));
%#v-
%\seealso{pop_array, __push_args, __pop_args}
%!%-
public define push_array(a)
{
   for ($1=0; $1<length(a); $1++)
     a[$1];
}



% --- Buffer local hooks  -----------------------------------------------
% 
% Tools for the definition and use of buffer local hooks -- just like the
% indent_hook or the newline_and_indent_hook jed already provides.
% Extend this idea to additional hooks that can be set by a mode and used by 
% another. Allows customizatin to be split in the "language" mode that 
% provides functionality and the "emulation" mode that does the keybinding.
% 
% Implementation is done via blocal vars. The hook can either be given as
% a pointer (reference) to a function of as the function name as string.



%!%+
%\function{run_blocal_hook}
%\synopsis{Run a blocal hook if it exists}
%\usage{ Void run_blocal_hook(String hook, [args])}
%\description
%   Run a blocal hook if it exists
%\example
%#v+
% define run_buffer()
% {
%    run_blocal_hook("run_buffer_hook");
% }
%#v-
%\seealso{runhooks, run_function, get_blocal, get_blocal_var}
%!%-
define run_blocal_hook() % (hook, [args])       
{	
   variable args = __pop_args(_NARGS-1);
   variable hook = ();
   !if (run_function(get_blocal(hook, NULL), __push_args(args)))
     {
	_pop_n(length(args));
	message("\"" + hook + "\" not defined");
     }
}

% evaluate the current buffer as script
% use the blocal_hook "run_buffer_hook" to find out which function to use
define run_buffer()
{
   run_blocal_hook("run_buffer_hook");
}



provide("bufutils");
