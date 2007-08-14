% diagnose.sl   Diagnostic functions
% show the value of variables (nice for debugging)

autoload("popup_buffer", "bufutils");
require("sprint_var");

% where shall the output go?
custom_variable("Diagnose_Buffer", "*debug*");
% popup the diagnose buffer with n lines (0 for don't popup)
custom_variable("Diagnose_Popup", 0.3);


% define empty_stack()
% {
%    _pop_n(_stkdepth);
% }

public define show_string(str)
{
   variable curbuf= whatbuf();
   setbuf(Diagnose_Buffer);
   eob;
   insert(strcat("\n", str, "\n"));
   set_buffer_modified_flag (0);
   if (Diagnose_Popup != 0)
     popup_buffer(Diagnose_Buffer, Diagnose_Popup);
   pop2buf(curbuf);
}

% return a formatted string of the arguments
public define sprint_args() % args
{
   variable arg, n=_NARGS, sep=" ", strarray = String_Type[n];
   loop(n)
     {
	n--;
	arg = sprint_variable(());  % convert to a string
	if(is_substr(arg, "\n"))
	   sep = "\n";
	strarray[n] = arg;
     }
   return strjoin(strarray, sep) + sep;
}

% show the content of the arguments in the diagnose buffer
public define show() % args
{
   variable args = __pop_args(_NARGS);
   show_string(sprint_args(__push_args(args)));
}

% show the content of the arguments in the minibuffer
public define smessage() % args
{
   variable args = __pop_args(_NARGS);
   message(sprint_args(__push_args(args)));
}

%!%+
%\function{show_object}
%\synopsis{put debug information in a debug buffer }
%\usage{Void show_object (String item [, String hint]);}
%\usage{Void show_object (Ref_Type item [, String hint]);}
%\description
% Inserts the actual value of item (or "not defined") into a diagnose buffer.
% An optional argument hint is appended to the output.
% The hint string may contain valid sprint format specifiers 
% (like %s) with the appropriate variables as additional arguments. 
% If the first argument is "", only the hint is written.
%\example
% For example
%       variable CA;
%       variable CU = "kuckuck";
%       show_object ("CA");
%       show_object ("cu");
%       show_object ("CU", "a hint");
%       show_object ("", "I'm here at %s", time ());
%       show_object ("foo");
%       show_object ("bol");
%       show_object ("center_line");
% result in the following diagnose buffer
%       CA:   not initialized
%       cu:   not defined
%       CU == kuckuck	% a hint
%       % I'm here at 07:21
%       foo:	 user defined function
%       bol:	 intrinsic function
%       center_line:	 internal function
% TODO: Currently only globally defined variables and functions are detected 
%       if given as strings. Use the second format with reference (pointer) 
%       for static variables or functions and function-local variables.
%       blocal variables need to be given as strings.
%        
%\seealso{}
%!%-
public define show_object () % (object, [hint= ""])
{
   % get arguments
   variable hint = "";         % optional second argument
   variable object;              % name of object to output info about
   if (_NARGS > 1)
     {
	variable args = __pop_args (_NARGS-1);
	hint = sprintf (__push_args (args));
     }
   object = ();  % take 1st arg from stack (might be String or Reference)  
   
   variable ref = NULL;                  % reference to object
   variable str = "";	                 % output string
   variable value = " <Uninitialized>";	 % variable value
   
   EXIT_BLOCK 
     {
	if (hint != "")
	  str += "\t% " + hint;
	show_string(str);
     }

   % Get String_Type object and Ref_Type ref
   if (typeof (object) == Ref_Type) % Reference given, convert to a string
     {
	ref = object;
	object = string(object)[[1:]];  % without leading '&'
	if (object == "ocal Variable Reference") % for local Variables 
	  object = "Local variable ";
     }
   else % object is already a string
     {
	object = string(object);
	ref = __get_reference (object);
     }
   
   % determine type of object
   switch (is_defined(object))
     { case +1 :  str = object + ": intrinsic function"; return;}
     { case +2 :  str = object + ": library function"; return;}
     { case  0 :  % not globally defined
	if (is_internal(object))
	  {
	     str = object + ":\t internal function"; 
	     return;
	  }
	else if (blocal_var_exists(object))
	  {
	     str = "Blocal variable " + object
	       + " == " + sprint_variable(get_blocal_var(object));
	     return;
	  }
	else if (ref == NULL)
	  {
	     str = object + " <Undefined> (or local/static given as string)"; 
	     return;
	  }
	else
	  str = object;
     }
     { case -1 :  str = "Intrinsic variable " + object;}
     { case -2 :  str = "Library variable " + object;}
   
   % add variable value
   if (__is_initialized(ref))
     value = " == " + sprint_variable(@ref);
   str += value;
}

% print a listing of the stack to the diagnose buffer, emptying the stack 
public define show_stack()
{
   variable element;
   if (_stkdepth)
     {
	show_string("Stack listing: ");
	loop(_stkdepth)
	  {
	     element = ();
	     show(element);
	  }
     }
   else
     show_string("Stack empty");
}

% Show the (string) argument and the result of its evaluation
public define show_eval() % args
{
   variable arg;
   loop(_NARGS)
     {
	arg = ();
	show(arg, eval(arg));
     }
}
   
provide("diagnose");
