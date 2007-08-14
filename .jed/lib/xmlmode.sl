% A reasonable XML mode. Might be usable for SGML. Is certainly not 
% usable for ordinary HTML, but XHTML should be fine. 
% 
% 
% What it does:
%   * Robust indenting. None of the XML / XSL files i've worked with
%     have suffered from bad indentation (not only simple stuff).
%     The result is approximately: 
%       <root blah="foo">
%           <child bar="baz"
%                  blah="bar">
%               <foo bar="blah" />
%               text
%           </child>
%       </root>
%   * IMHO more usable syntax highlighting than the DocBook/HTML modes
%     have. I'm not totally happy with the result, but I suspect it's
%     the best that can be done until the DFA engine supports zero-
%     width assertions. :-)
% 
% Problems:
%   * Finding the indent level is O(n) in the number of lines in the
%     document. That's pretty much the price that has to be paid for
%     reliability :-/
%   * Lack of multi-line DFA really kills highlighting of PI's, CDATA, and
%     comments.
%   * " starts a string (i.e attribute value) also when not inside a tag.
%     This can lead to funny indentation effects in cases like below:
%       <foo> blah" </foo>
%     The solution is to split the ending tag to another line:
%       <foo> blah"
%       </foo>
%   * The automatic tag closing has problems, when there are multiple
%     closing tags on the same line. I.e:
%       <foo>
%         <bar>
%         </bar>X
%     Closing the tag at 'X' will produce another '</bar>' instead of
%     '</foo>'. The workaround is obvious ;-)
% 
% 2001-10-18 / Juho Snellman <jsnell@iki.fi>
%   * First public release 
%   
% 2001-10-22 / Juho Snellman <jsnell@iki.fi>
%   * Fixed the following problem: 
%       <foo>
%         "<bar/>"
%            blah
%   * Added a make_keymap(). (Why did it work without one?)
%  
% 2001-10-31 / Juho Snellman <jsnell@iki.fi>
%   * Removed the requirement that the file should start with the <?xml?>
%     pseudo-PI. 
%
% 2001-12-05 / Juho Snellman <jsnell@iki.fi>
%   * Better regexp for highlighting element names. Gets numbers and
%     capital letters too. 
%
% 2001-12-07 / Juho Snellman <jsnell@iki.fi>
%   * Fixed problem with '<>'.
%   * Added automatic closing of tags.   
%   * Added comment strings for comments.sl
%   
% 2002-04-17 / Juho Snellman <jsnell@iki.fi>
%   * '< foo' has no effect on the indent, while '<foo' has. I.e 
%     there may not be a space between the '<' and the name of
%     the element. 
%     
% 2002-07-21 / Juho Snellman <jsnell@iki.fi>
%   * Made most functions/variables static.

create_syntax_table ("xml");
define_syntax ("<", ">", '(', "xml");
define_syntax ('"', '"', "xml");
% define_syntax ('\'', '\'', "xml");
define_syntax ('\\', '\\', "xml");
define_syntax ("=/", '+', "xml");
define_syntax ("<!--", "-->", '%', "xml");
% define_syntax ("<![CDATA[", "]]>", '%', "xml");

set_syntax_flags ("xml", 0x04|0x80);

#ifdef HAS_DFA_SYNTAX
%%% DFA_CACHE_BEGIN %%%
static define setup_dfa_callback (name)
{
    % dfa_enable_highlight_cache ("xml.dfa", name);
    dfa_define_highlight_rule ("<!--.*-->", "comment", name);
    dfa_define_highlight_rule ("=", "operator", name);
    dfa_define_highlight_rule ("<|>|</|/>", "keyword", name);
    dfa_define_highlight_rule ("</?[A-Za-z0-9_\\-]+:?[A-Za-z0-9_\\-]*", "keyword", name);
    dfa_define_highlight_rule ("<!\\[CDATA\\[.*\\]\\]>", "preprocess", name);
    % Slightly flawed, the '>' could be inside an attribute
    dfa_define_highlight_rule ("<!.*>", "preprocess", name);
    dfa_define_highlight_rule ("<\\?.*\\?>", "preprocess", name);
    dfa_define_highlight_rule ("\&.*;", "preprocess", name);
    dfa_define_highlight_rule ("\"([^\\\\\"]|\\\\.)*\"", "string", name);
    % dfa_define_highlight_rule ("'([^\\\\\']|\\\\.)*'", "string", name);
    dfa_build_highlight_table (name);
}
dfa_set_init_callback (&setup_dfa_callback, "xml");
%%% DFA_CACHE_END %%%
#endif

variable XML_INDENT = 2;
variable XML_TAG_CONTENT_INDENT = 4;

static variable names = Assoc_Type [ String_Type ];

% Sets the name of the last tag on an indent level. 
static define xml_set_name(ind) 
{
    push_spot();        
    
    push_mark_eol();
    variable name = bufsubstr();
    
    if (string_match(name, "\\([A-Za-z0-9_\\-]+:?[A-Za-z0-9_\\-]*\\)", 1))
    {
	variable pos;
	variable len;
	
	(pos, len) = string_match_nth(1);
	
	name = substr(name, pos+1, len);
	
	names[sprintf("%d", ind)] = name;
    }    
    
    pop_spot();
}

static define xml_get_name(ind) 
{
    variable sind = sprintf("%d", ind);
    names[sind];
}

static define xml_looking_at(s) 
{
    return looking_at(s) and not looking_at(strcat(s," "));
}

% Returns the level of nested elements that the start of this line is
% inside of. (A negative number is returned if the start of the line 
% is inside a tag.)
static define xml_calculate_indent ()
{
    push_spot();
    
    variable row = what_line();
    variable ind = 0;
    
    EXIT_BLOCK {
	pop_spot();
    }
    
    bob();
        
    % 0 : Inside a <! ...  >
    % 1 : Inside a <? .. ?>
    % 2 : Inside a < ... >, </ ... >, or < ... />
    % 3 : Other
    variable state = 3;
    variable closing_tag = 0;
    % The line on which the current tag started on. 
    variable start_row = 0;
    
    variable i = 0;
    
    while (1) {
	% Just in case of bugs involving infinite loops.
	i++;
	if (i > 100000) {
	    error(sprintf("Gave up after 100000 tries. Document too large? State: %d Ind: %d Pos: %d/%d", state, ind,
			  what_column(), what_line() ));
	}
		
	switch (0)
	{ state == 0: 
	    switch (0) 
	    {   what_line() >= row : return 0; }
	    {   not fsearch(">") :  error("Unterminated processing instruction"); }
	    {   if (parse_to_point() == 0)
		  state = 3;
		() = right(2);
	    }	    
	}	
	{ state == 1: 
	    switch (0) 
	    { not fsearch("?>") :  error("Unterminated processing instruction"); }
	    { what_line() >= row : return 0; }
	    {   % Oops, make sure that the "?>" wasn't just an attribute
		% value. 
		if (parse_to_point() == 0)
		  state = 3;
		() = right(2);
	    }	    
	}	
	{ state == 2: 
	    switch (0)
	    {   looking_at("<") and what_line() == row :
		% error(sprintf("%d", ind-(1-closing_tag)));
		return ind-(1-closing_tag);;
	    }	    
	    { not fsearch(">")   : error("Unterminated element"); }
	    {   () = right(1);
		% If the '>' wasn't inside a string, we end the 
		% element
		if (parse_to_point() == 0) {
		    state = 3;
		    if (what_line() == row) {
			ind = ind-(1-closing_tag);
			% Multi-line tag
			if (what_line() != start_row) {
			    return -1;
			}	
			return ind;
		    }		
		    if (what_line() > row) {
			return -1;
		    }		
		    % If the element was immediately ended, decrement
		    % the indent level
		    () = left(2);
		    if (looking_at("/>")) {
			ind--;
		    }		    
		}		
	    }	    
	}	    
	{ state == 3:
	    switch (0) 
	      % We passed the point we were supposed to investigate.
	      % Just return what we have now. 
	    { what_line() > row : return ind; }
	    % Inside a comment (probably at the start). Move a little
	    % bit forward, and search for a tag start.
	    { parse_to_point() == -2 : () = right(1); () = fsearch("<"); }
	    % Oops, do the same thing for strings too.
	    { parse_to_point() == -1 : () = right(1); () = fsearch("<"); }
	    % Comments are handled magically by Jed, so we don't need to
	    % search for the end manually. Move the point to the right of
	    % the <!-- to make progress.
	    { looking_at("<!--") : () = right(4); }
	    % CDATA sections have no effect on indent. 
	    { looking_at("<![CDATA[") :  if (not fsearch("]]>")) return 0; }
	    % Nor do other PI's, but their ending condition is rather more
	    % difficult. Handle it in a different state.
	    { xml_looking_at("<!") :   state = 0; }
	    { xml_looking_at("<?") :   state = 1; }
	    % 
	    { looking_at("<>")  :  error(sprintf("Tag without a name on line %d.", what_line())); }
	    % Closing a tag decreases the indent. Change to "inside a tag"
	    % state. 
	    { xml_looking_at("</") :  
		state = 2; ind--;  closing_tag = 1;  start_row = what_line(); }
	    % The same, except opening a tag increases the indent.
	    { xml_looking_at("<")  :
		state = 2; ind++; closing_tag = 0; start_row = what_line(); xml_set_name(ind-1);}
	    % No more tags, return what we have now.
	    { (not right(1)) or (not (fsearch("<"))) : return ind; }
	}	    
    }    
}

define xml_start_of_next_word ()
{
    variable orig_col = what_column();
    push_spot();
    
    EXIT_BLOCK {
        pop_spot();
    }
    
    % First we find either eol, or some whitespace
    while ( (not eolp()) and
            (not (what_char() == ' ')) and
            (not (what_char() == '\t')) )
      () = right(1);
    
    % No interesting words found
    if (eolp()) return -orig_col;
    
    % Now we find either an eol, or the start of the word
    while ( (not eolp()) and
            ((what_char() == ' ') or
             (what_char() == '\t')) )
      () = right(1);
    
    % No interesting words found
    if (eolp()) return -orig_col;
    
    return what_column();
}

static define xml_parse_to_point() 
{
    push_spot();
    
    EXIT_BLOCK {
	pop_spot();
    }    
    
    variable row = what_line();
    
    % Should check that the start token isn't inside a string. 
    if (bsearch("<![CDATA[")) {
	% The CDATA hasn't been ended, so we're still in it... 
	if (not fsearch("]]>"))
	  return -2;
	% The CDATA is closed beyond the beginning point
	if (row <= what_line())
	  return -2;
    }    
    
    % Default to normal 
    pop_spot();
    push_spot();
    return parse_to_point();
}

define xml_indent_line ()
{
    variable col = 0;
    
    push_spot();
    
    EXIT_BLOCK {
        pop_spot();
        bol_trim();
        whitespace(col);
        bol_skip_white();
    }
    
    bol_skip_white();
    
    if (xml_parse_to_point == -2) {
	col = what_column() - 1;
	return;
    }    
    
    variable ind = xml_calculate_indent();
    
    % Inside a multi-line tag
    if (ind < 0) {
	() = find_matching_delimiter('>');
	col = xml_start_of_next_word();
	if (col < 0)
	  col = -col + XML_TAG_CONTENT_INDENT;
	col--;
    } else {
	col = ind * XML_INDENT;
    }
    
    return;
}

static define xml_close_tag_name() 
{
    variable ind = xml_calculate_indent();
    
    if (ind < 0)
      error("No tag to close");
    
    insert(xml_get_name(ind));
    
    !if (looking_at(">"))
      insert(">");
}

define xml_slash()
{
    insert("/");
    
    if (blooking_at("</")) 
    {
	if (xml_parse_to_point == 0) {
	    xml_close_tag_name();
	    push_spot();
	    xml_indent_line();
	    pop_spot();
	}	
    }    
}

define xml_close_tag()
{
    insert("<");
    xml_slash();
}

!if (keymap_p ("xml")) make_keymap ("xml");
% definekey( "xml_slash", "/", "xml" );
definekey( "indent_line", "\t", "xml" );
definekey_reserved( "xml_close_tag", "^C", "xml" );

%!%+
%\function{xml_mode}
%\synopsis{xml_mode}
%\usage{Void xml_mode ();}
%\description
%
% Functions that affect this mode include:
%#v+
%  function:             default binding:
%  xml_slash                  Not bound. Bind to '/' to automatically
%                             close tags when writing. 
%  xml_close_tag              Ctrl-C Ctrl-C
%  xml_indent_line            TAB
%#v-
% Variables affecting indentation include:
%#v+
% XML_INDENT
%#v-
%!%-
define xml_mode ()
{
    set_mode("xml", 4);
    set_buffer_hook ("indent_hook", "xml_indent_line");
    use_syntax_table ("xml");
    use_keymap ("xml");
    set_comment_info ("xml", "<!--", "-->", 0);
    run_mode_hooks ("xml_mode_hook");
}
