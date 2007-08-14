% textutils.sl: Tools for text processing (marking, string processing,
% formatting)

%--- marking and regions ---------------------------------------------
% mark a word (going forward if between two words)
define mark_word () % mark_word(word_chars = get_word_chars())
{
   variable word_chars = get_word_chars();
   if(_NARGS)
     word_chars = ();
   % go to begin of word after cursor, if not in a word
   skip_chars("^"+word_chars);
   % go to begin of word under cursor, stay when not in a word
   bskip_chars(word_chars);
   push_visible_mark();
   skip_chars(word_chars);  % goto end of word
}

% mark a word (going backward if between two words)
define bmark_word () % bmark_word(word_chars = get_word_chars())
{
   variable word_chars = get_word_chars();
   if(_NARGS)
     word_chars = ();
   % go to end of word under cursor, stay when not in a word
   skip_chars(word_chars);
   % go to end of word before cursor, if not in a word
   bskip_chars("^"+word_chars);
   push_visible_mark();
   bskip_chars(word_chars);  % goto beg of word
   check_region(0);
}



% return the word at point as string (going forward if between two words)
define get_word ()
{
   % pass on optional arguments
   variable args = __pop_args (_NARGS);
   push_spot;
   mark_word (__push_args (args));
   bufsubstr();
   pop_spot();
}

% return the word at point as string (going backward if between two words)
define bget_word ()
{
   % pass on optional arguments
   variable args = __pop_args (_NARGS);
   push_spot;
   bmark_word (__push_args (args));
   bufsubstr();
   pop_spot();
}


% return the current line as string (this one is already there:)
% define get_line() 
% {
%   line_as_string ()
% }


define indent_buffer ()
{  push_spot;
   bob;
   do
     indent_line;
   while (down_1);
   pop_spot;
}

define indent_region_or_line ()
{  
   !if(is_visible_mark)
     indent_line;
   else
     {
	check_region (1);                  % make sure the mark comes first
	variable End_Line = what_line;
	exchange_point_and_mark();         % now point is at start of region
	while (what_line <= End_Line)
	  {indent_line; down_1;}
	pop_mark (0);
	pop_spot;                          % return to where we were before
      }
}

define number_lines ()
{
   if(is_visible_mark)
     narrow;
   variable i = 1;
   bob;
   do
     { bol;
        insert(string(i) + " ");
	i++;
     }
   while (down_1);
   widen;
}

% reverse the order of characters in a string
define string_reverse (a)
{
   variable i = strlen (a) - 1;
   if (i < 1)
     return a;
   __tmp(a)[[i:0:-1]];
}

