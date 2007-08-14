% This module implements simple highlighting and indenting for makefiles. 
% The important functionality is the special highlighting of lines that
% start with a tab (using the 'preprocess' color), and the automatic
% indenting of rules using tabs. Orphaned tabs are displayed using the
% 'region' color.
% 
% 
% This is rather spartan, but should be better than using sh-mode.
% 
% Problems: 
%   * Very few keywords defined. But keyword highlighting isn't exactly
%     the point of this mode... :-)
%   * The handling for $()-expressions is rather obnoxious. Something
%     smarter and better looking would be nice.
%   * I'm not sure whether marking rules with the 'preprocess' color is
%     better than marking the tab with 'region', and highlighting the
%     rest of the line as normal. 
%  
% 2001-10-13 / Juho Snellman <jsnell@iki.fi>
%   * First public release.
%
% 2001-10-24 / Juho Snellman <jsnell@iki.fi>
%   * Added a enable_dfa_syntax_for_mode, since this mode is mostly
%     useless with standard highlighting. 
%   * Mark the tabs with region, as discussed above. 
%
% 2001-12-07 / Juho Snellman <jsnell@iki.fi>
%   * Added comment strings.
%   * Changed mode name to 'make'.
%   * Added a use_keymap()
%   * Fixed problem with colons in comments. 
%
% 2004-01-08 / Guenter Milde <g.milde@web.de>
%   * code optimization
%   
% 2004-01-09 / Juho Snellman <jsnell@iki.fi>
%   * Reverted the change to not using a keymap (textmode does a local_setkey
%     on \t -> text_indent_relative)


static variable mode = "make"; % less literal strings save memory

create_syntax_table (mode);
define_syntax ("#", "", '%', mode);
define_syntax ("([{", ")]}", '(', mode);

#ifdef HAS_DFA_SYNTAX
%%% DFA_CACHE_BEGIN %%%
static define setup_dfa_callback (name)
{
    % dfa_enable_highlight_cache ("makemode.dfa", name);
    dfa_define_highlight_rule ("\\\\.", "normal", name);
    dfa_define_highlight_rule ("#.*$", "comment", name);
    % dfa_define_highlight_rule ("^.*:", "keyword", name);    
    dfa_define_highlight_rule ("\\$\\(.*\\)", "keyword", name);
    dfa_define_highlight_rule ("\"([^\\\\\"]|\\\\.)*\"", "string", name);
    dfa_define_highlight_rule ("\"([^\\\\\"]|\\\\.)*$", "string", name);
    dfa_define_highlight_rule ("'[^']*'", "string", name);
    dfa_define_highlight_rule ("'[^']*$", "string", name);
    dfa_define_highlight_rule ("[\\|&;\\(\\)<>\\:]", "Qdelimiter", name);
    dfa_define_highlight_rule ("[\\[\\]\\*\\?=]", "Qoperator", name);
    dfa_define_highlight_rule ("[A-Za-z_]+",
                               "Knormal", name);
    dfa_define_highlight_rule ("^\t[ \t]*", "region", name);
    % dfa_define_highlight_rule ("^\t.*", "preprocess", name);
    dfa_define_highlight_rule (".", "normal", name);
    dfa_build_highlight_table (name);
}
dfa_set_init_callback (&setup_dfa_callback, mode);
enable_dfa_syntax_for_mode(mode); 
%%% DFA_CACHE_END %%%
#endif

() = define_keywords (mode, "shell", 5);
() = define_keywords (mode, "export", 6);

define is_rule_start_line ()
{
    push_spot();
    
    EXIT_BLOCK
    {
        pop_spot();
    }    
    
    bol_skip_white();
    
    % Disallow rules starting with whitespace. 
    !if (bolp)
      return 0;
    
    % Conditions on the ':' that is allowed to start a rule
    %  * Not in a comment or string
    %  * Followed by whitespace or eol
    
    while (ffind(":")) {
	if ( (parse_to_point() == 0) )
	{
	    () = right(1);
	    skip_white();
	    if (eolp())
	      return 1;
	}
	else 
	{
	    () = right(1);
	}
    }
    
    
    return 0;
}

% A line is in a rule if the previous line is in a rule (i.e, starts with
% a tab) or starts a rule (which we define to mean "contains a ':'").
define is_in_rule ()
{
    if ( is_rule_start_line )
      return 0;
    
    push_spot();
    
    EXIT_BLOCK
    {
        pop_spot();
    }    
    
    () = up(1);
    bol();
    
    % A tab at the start of line found -> this line is part of a rule.
    if (what_char()=='\t')
      return 1;
    
    return is_rule_start_line();
}

% Only indent lines that are inside a rule definition. Use hardcoded
% tabs instead of whitespace() due to make's moronic whitespace 
% dependencies.
define make_indent_line ()
{
    if (is_in_rule()) {
        bol();
        if (what_char != '\t') {        
            insert_char('\t');
        }
        bol_skip_white();
    }    
}

% We need to set our own tab handling, since textmode does a
% local_setkey("text_indent_relative", "\t").
!if (keymap_p (mode)) make_keymap (mode);
definekey( "indent_line", "\t", mode );

define make_mode ()
{
    set_mode(mode, 0);
    use_syntax_table (mode);
    use_keymap (mode);
    mode_set_mode_info (mode, "fold_info", "#{{{\r#}}}\r\r");
    set_buffer_hook ("indent_hook", &make_indent_line);
    set_comment_info (mode, "#", "", 0);
    run_mode_hooks("make_mode_hook");
}

