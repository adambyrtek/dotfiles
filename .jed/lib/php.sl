%   file     : php.sl
%   author   : Mikael Hultgren <mikael_hultgren@gmx.net>
%   
%   version 2.0.1 (20-Aug-2002)
%
%   $Id: php.sl,v 1.211 2002/08/20 02:28:53 child Exp $
%   $Revision: 1.211 $
%   
%   D e s c r i p t i o n
%   ---------------------
%   This is a mode for editing php files in jed, hence the name phpmode :)
%    
%   The reason for this mode is that the only mode i
%   could find for editing php files under jed was one 
%   i found on dotfiles made by Eric Thelin. 
%   But it didn't work as i wanted, so i grabbed what i
%   could from it, and started from cmode as a template.
%   
%   At the moment it does keyword highlighting and proper
%   indenting, plus a slew of other functionality.
%   
%   -------------------------------------------------------------------------------------------
%   PHP-mode variables:
%   -------------------------------------------------------------------------------------------
%   variable PHP_INDENT      = 4;       % Amount of space to indent within block.
%   variable PHP_BRACE       = 0;       % Amount of space to indent brace
%   variable PHP_BRA_NEWLINE = 0;       % If non-zero, insert a newline first before inserting 
%                                       % a '{'.
%   variable PHP_CONTINUED_OFFSET = 2;  % This variable controls the indentation of statements
%                                       % that are continued onto the next line.
%   variable PHP_COLON_OFFSET = 1;      % Controls the indentation of case inside switch statements.
%   variable PHP_CLASS_OFFSET = 4;      % Controls the amount of indenting inside the class,
%                                       % doesn't apply to the braces
%   variable PHP_SWITCH_OFFSET = 0      % Controls the ammount of extra indention inside switch statements.                                    
%   variable PHP_KET_NEWLINE = 0;       % If non-zero, insert a newline first before inserting 
%                                       % a '}'.
%   variable PHP_Autoinsert_Comments = 1;
%  --------------------------------------------------------------------------------------------
%   
%   T h a n k s  g o  o u t  t o
%   ----------------------------
%   
%    o Eric thelin <eric at thelin.org> for his phpmode that got me started.
%    o David <dstegbauer at post.cz>  who pointed out that php isn't in fact a
%      case sensitive language when it comes to
%      functions ;)
%    o Abraham vd Merwe <abz at blio.net> for his relentless bug-reporting,
%      feature suggestions and beta-tester.
%      Without him my to-do list would be
%      considerable shorter ;)
%    o cmode.sl in jed, without that this mode wouldn't be
%      nearly as feature rich as it is now.
%    o latex.sl for tips on how to do things.
%

% Set all variables to a default value so people who forget to add
% them to their .jedrc doesnt get a error.
custom_variable( "PHP_INDENT", 4 );
custom_variable( "PHP_BRACE", 0 );
custom_variable( "PHP_BRA_NEWLINE", 0 );
custom_variable( "PHP_CONTINUED_OFFSET", 2 );
custom_variable( "PHP_COLON_OFFSET", 1 );
custom_variable( "PHP_CLASS_OFFSET", 4 );
custom_variable( "PHP_SWITCH_OFFSET", 0 );
custom_variable( "PHP_KET_NEWLINE", 0 );
custom_variable( "PHP_Autoinsert_Comments", 0 );

% Set some variables that are used throughout the code.
private variable delim_PHP_start = "<?";
private variable delim_PHP_end   = "?>";
private variable delim_ASP_start = "<%";
private variable delim_ASP_end   = "%>";


define php_revision( )
{
	message("$Revision: 1.211 $");
}

static define php_is_comment( ) %{{{
{
	push_spot( );
	bol_skip_white( );
	0;
	if( orelse
	  { looking_at( "//" ) }
		{ looking_at( "#" ) }
		)	
	{
		pop( );
		what_column( );
	}
	pop_spot( );
}
%}}}
static define php_parse_to_point( ) %{{{
{
	parse_to_point( )
	  or php_is_comment( );
}
%}}}
static variable PHPmode_Fill_Chars = "";
define php_paragraph_sep( ) %{{{
{
	if( strlen( PHPmode_Fill_Chars )) 
	  return 0;
	push_spot( );
	bol_skip_white( );
	
	if( orelse
	  { looking_at( "* " ) }
		{ looking_at( "// " ) }
		{ looking_at( "# " ) }
		)	
	{
		go_right( 2 );
		skip_white( );
		if( looking_at( "@ " )) 
		  eol( );
	}
	
	eolp( ) or ( -2 != parse_to_point( ) );
	pop_spot( );
}
%}}}
define php_format_paragraph( ) %{{{
{
	variable n, dwrap;
	
	PHPmode_Fill_Chars = "";
	if( php_paragraph_sep( ) ) 
	  return;
	push_spot( ); 
	push_spot( ); 
	push_spot( );
	
	while( not( php_paragraph_sep( ) ))
	{
		!if( up_1( ) ) 
		  break;
	}
	if( php_paragraph_sep( ) ) 
	  go_down_1( );
	push_mark( );
	pop_spot( );
	
	while( not( php_paragraph_sep( ) ))
	{
		!if( down_1( ) ) 
		  break;
	}
	
	if( php_paragraph_sep( ) ) 
	  go_up_1( );
	
	narrow( );
	pop_spot( );
	bol( );
	push_mark( );
	skip_white( );
	if( looking_at( "* " )) 
	  go_right( 2 );
	else if( looking_at( "// " )) 
	  go_right( 3 );
	else if( looking_at( "# " )) 
	  go_right( 2 );
	
	PHPmode_Fill_Chars = bufsubstr( );
	dwrap = what_column( );
	bob( );
	do 
	{
		bol_trim( );
		if( looking_at( "* " )) 
		  deln( 2 );
		else if( looking_at( "// " )) 
		  deln( 3 );
		else if( looking_at( "# " )) 
		  deln( 2 );
	}
	
	while( down_1( ) );
	WRAP -= dwrap;
	call( "format_paragraph" );
	WRAP += dwrap;
	bob( );
	do 
	{
		insert( PHPmode_Fill_Chars );
	}
	while( down_1( ) );
	
	bol( );
	go_right( strlen( PHPmode_Fill_Chars ));
	
	skip_white( );
	if( looking_at( "*/" ))
	{
		push_mark( );
		bol_skip_white( );
		del_region( );
	}
	
	PHPmode_Fill_Chars = "";
	widen( );
	pop_spot( );
}
%}}}
define php_in_block( ) %{{{
{
	variable begin = 0, end = 0;	
	variable test;
	
	push_spot( );
	if( bolp( ) )
	{
		if( orelse
		  { looking_at( delim_PHP_start ) }
			{ looking_at( delim_PHP_end ) }
			{ looking_at( delim_ASP_start ) }
			{ looking_at( delim_ASP_end ) }
			)
		{
			pop_spot( );
			return( 1 );
		}
	}
	
	if( looking_at( ">" ))
	{
		go_left( 1 );
		if( orelse
		  { looking_at( "?" ) }
			{ looking_at( "%" ) }
			)
		{
			pop_spot( );
			return( 1 );
		}
	}
	
	forever
	{
		if( orelse
		  { bsearch( delim_PHP_start ) }
			{ bsearch( delim_ASP_start ) }
			)
		{
			if( php_parse_to_point( ) == 0 )
			{
				begin = what_line( );
				break;
			}
			continue;
		} else {
			break;
		}
	}
	
	pop_spot( );
	
	push_spot( );
	forever
	{
		if( orelse
		  { bsearch( delim_PHP_end ) }
			{ bsearch( delim_ASP_end ) }
			)
		{
			if( php_parse_to_point( ) == 0 )
			{
				end = what_line( );
				break;
			}
			continue;
		} else {
			break;
		}
	}
	
	pop_spot( );
	
	if( end < begin )
	{
		return( 1 );
	}
	return( 0 );
}
%}}}

define php_top_of_function( ) %{{{
{
	push_spot( );
	variable current,end,start_brace;
	current = what_line;
	!if( re_bsearch( "function[ \t]+[a-zA-Z_0-9]+[ \t]?\(.*\)") )
	{
		error( "Cant find top of function" );
	}
	
	% Check to see if were in a comment
	if( php_parse_to_point( ) )
	{
		pop_spot( );
		error( "Cant find top of function" );
	}
	
	!if( fsearch( "{") )
	{
		error( "Missing beginning brace of function." );
	}
	start_brace = what_line;
	if( start_brace > current )
	{
		pop_spot( );
		error( "Missing beginning brace of function." );
	}
	find_matching_delimiter( '{' );
	end = what_line;
	if( end < current )
	{
		pop_spot( );
		error( "Not in function" );
	}
	find_matching_delimiter( '}' );
}
%}}}
define php_end_of_function( ) %{{{
{
	!if( bolp( ) and looking_at_char( '{' ))
	  php_top_of_function( );
	call( "goto_match" );
}
%}}}
define php_mark_function( ) %{{{
{
	php_end_of_function( );
	push_visible_mark( );
	eol( );
	exchange_point_and_mark( );
	php_top_of_function( );
	bol_skip_white( );
	if( looking_at( "{") )
	{
		go_up( 1 );
	}
	bol( );
}
%}}}
define php_mark_matching( ) %{{{
{
	push_spot( );
	if( find_matching_delimiter( 0 ))
	{
		% Found one
		pop_spot( );
		push_visible_mark( );
		find_matching_delimiter( 0 );		
		exchange_point_and_mark( );
	} else {
		pop_spot( );
	}
}
%}}}
define php_bskip_over_comment( ) %{{{
{
	forever 
	{	
		bskip_chars (" \t\n");
		if( bobp( ) )
		  return;
		
		push_mark( );
		while( up_1( ) )
		{	
			go_down_1( );
			break;
		}
		
		bol_skip_white( );
		
		if( orelse
		  { looking_at( delim_PHP_start ) }
			{ looking_at( delim_PHP_end ) }
			{ looking_at( delim_ASP_start ) }
			{ looking_at( delim_ASP_end ) }
			)
		{
			pop_mark_0( );
			continue;
		}
		pop_mark_1( );
		
		!if( blooking_at( "*/" ))
		{
			push_mark( );
			variable ptp = -2;
			
			while( andelse
				 { ptp == -2 }
				   { bfind( "//" ) or bfind( "#" ) }
				   )
			  ptp = parse_to_point ();

			if (ptp == 0)
			{
				pop_mark_0( );
				continue;
			}
			
			bol( );
			!if( bobp( ) )
			{
				if( orelse
				  { looking_at( delim_PHP_start ) }
					{ looking_at( delim_PHP_end ) }
					{ looking_at( delim_ASP_start ) }
					{ looking_at( delim_ASP_end ) }
					)
				{
					pop_mark_0( );
					continue;
				}
			}			
			pop_mark_1( );
			break;
		}
		!if( bsearch( "/*" )) break;
	}
}
%}}}
static define php_looking_at( token ) %{{{
{
	variable cse = CASE_SEARCH, ret = 0;
	CASE_SEARCH = 1;
	
	if( looking_at( token ))
	{
		push_spot( );
		go_right( strlen( token ));
		_get_point( );
		skip_chars( "\t :({" );
		ret = ( _get_point( ) - ( )) or eolp( );
		pop_spot( );
	}
	CASE_SEARCH = cse;
	ret;
}
%}}}
static define php_indent_to( n ) %{{{
{
	bol( );
	% Force a reindent if the line does not contain tabs followed by spaces.
	skip_chars( "\t" );
	skip_chars( " " );
	
	if( ( what_column != n )
		or ( _get_point( ) != ( skip_white( ), _get_point( ))))
	{
		bol_trim( );
		n--;
		whitespace( n );
	}
}
%}}}
static define php_indent_continued_comment( col ) %{{{
{
	push_spot( );
	col++;			       %  add 1 so the we indent under * in /*
	php_indent_to( col );
	
	if( looking_at( "*" )
		or not( eolp( ) ))
	  pop_spot( );
	else
	{
		insert( "* " );
		pop_spot( );
		
		if( what_column( ) <= col )
		{
			goto_column( col + 2 );
		}
	}
}
%}}}
static define php_mode_if_bol_skip_white( ) %{{{
{
	push_mark( );
	bskip_white( );
	1;
	if( bolp( ) )
	{
		pop( );
		skip_white( );
		0;
	}
	pop_mark( ( ) );		       %  take argument from stack
}
%}}}
%#iftrue
% Return true if the spot is inside of a class definition
% Takes the opening brace of the enclosing block as an
% argument.
static define inside_class( bra ) %{{{
{
	push_spot( );
	EXIT_BLOCK
	{
		pop_spot( );
	}
	
	goto_user_mark( bra );
	
	% Assume that class is at the beginning of a line.  We may want to
	% change this assumption later.
	while( re_bsearch( "^\\c[ \t]*\\<class\\>" ))
	{
		if( 0 == parse_to_point( ) )
		{
			while( fsearch( "{" ))
			{
				if( 0 != parse_to_point( ) )
				{
					go_right_1( );
					continue;
				}
				
				if ( bra == create_user_mark( ) )
				  return 1;
				break;
			}
			return 0;
		}
		
		!if( left( 1 ))
		  break;
	}
	
	return 0;
} %}}}
%#endif
define php_indent_line( ) %{{{
{	
	variable val, col, extra_indent = 0;
	variable prep_line = 0;
	variable match_char, match_indent, this_char, match_line;
	variable match_mark;
	variable is_continuation = 0;
	
	% Check whetever we are in a php block or not
	if( php_in_block( ) )
	{
		push_spot( );
		bol_skip_white( );
		
		% Store the character we are standing on
		this_char = what_char( );
		if( -2 == parse_to_point( ) )
		{
			% In a c comment.  Indent it at level of matching /* string
			( ) = bsearch( "/*" );
			col = what_column( );
			pop_spot( );
			php_indent_continued_comment( col );
			php_mode_if_bol_skip_white( );
			return;
		}
		
		EXIT_BLOCK
		{
			php_mode_if_bol_skip_white( );
		}
		
		if( orelse
		  { php_looking_at( "case" ) }
			{ php_looking_at( "default" ) }
			)
		{
			if( ffind_char( ':' ))
			{
				extra_indent -= PHP_INDENT;
				extra_indent += PHP_COLON_OFFSET;
				%message(string(extra_indent));
			}
			bol( );
		} else {
			forever
			{
				php_bskip_over_comment( );
				!if( orelse
				   { blooking_at( ";" ) }
					 { blooking_at( "{" ) }
					 { blooking_at( "}" ) }
					 { blooking_at( ")," ) }
					 { blooking_at( "}," ) }
					 { blooking_at( ":" ) }
					 { bobp( ) }
					 )
				{	
					% This needs to be here to make sure were still in the phpblock
					if( php_in_block( ) )
					{
						% message("hej2");
						if( is_continuation )
						{
							% message("hej");
							extra_indent += PHP_CONTINUED_OFFSET;
						} 
						else 
						{
							% message("hej3");
							push_spot( );
							bol_skip_white( );
							% fsearch( "{" );
							% !if( blooking_at( ")" )
							extra_indent += PHP_CONTINUED_OFFSET;
							pop_spot( );
						}
					
						% extra_indent += PHP_CONTINUED_OFFSET;					
						is_continuation++;
						% is_continuation++;
					}
				}
				
				!if( blooking_at( ")" ))
				  break;
				push_mark( );
				go_left_1( );
				if( 1 != find_matching_delimiter( ')' ))
				{
					pop_mark_1( );
					break;
				}
				
				php_bskip_over_comment( );
				
				push_spot( );
				if( ( 1 == find_matching_delimiter( ')' )), pop_spot( ) )
				{
					pop_mark_1( );
					break;
				}
				
				pop_mark_0( );
				bol ( );
			}
		}
		
		val = find_matching_delimiter( ')' );
		match_mark = create_user_mark( );
		
		match_char = what_char( );
		match_line = what_line( );
		
		if( ( val < 0 ) and looking_at( "/*" ))
		  val = -2;
		else if( val == 1 )
		{
			go_right( 1 );
			skip_white( );
		}
		
		col = what_column( );

		bol_skip_white( );
		match_indent = what_column( );
		if( what_line( ) < prep_line )
		{
			match_char = 0;
		}
		
		pop_spot( );
		
		switch( val )
		{
		 case 0:			       %  mismatch
			if( match_char == '{' )
			{
				push_spot( );
				goto_user_mark( match_mark );
				
				bskip_chars( "\n\t " );				
				if( blooking_at( ")" ))
				{
					variable same_line = ( what_line == match_line );
					
					go_left_1( );
					if( 1 == find_matching_delimiter( ')' ))
					{
						bol_skip_white( );
						
						if( same_line )
						  match_indent = what_column( );
						
						% NOTE: This needs work.
						if( ( this_char != '}' )
							and looking_at( "switch" ))
						  match_indent += PHP_SWITCH_OFFSET;
					}
				}
				
				pop_spot( );
				col = match_indent;
#ifexists PHP_CLASS_OFFSET
				if( this_char == '}' )
				  col += PHP_INDENT;
				else if( inside_class( match_mark ))
				  col += PHP_CLASS_OFFSET;
				else
				  col += PHP_INDENT;
#else
				col += PHP_INDENT;
#endif
			} else if( match_char == '[' ) {
				push_spot( );
				php_indent_to( col + 1 );
				pop_spot( );
				return;
			} else {
				push_spot( );
				bol_skip_white( );
				if( looking_at_char( '{' ))
				  extra_indent = PHP_BRACE;
				extra_indent++;
				php_indent_to( extra_indent );
				pop_spot( );				
				return;
			}
		}
		{
		case 1:
			extra_indent = 0;	       %  match found
		}
		{
		case -2:			       %  inside comment
			if( this_char != '\\' ) 
			  col++;
			php_indent_continued_comment( col );
			return;
		}
		{
		 case 2:
			push_spot_bol( );
			trim( );
			pop_spot( );
			return;
		}
		
		switch( this_char )
		{
		 case '}':
			col -= PHP_INDENT;
		}
		{
		 case '{':			
			col += PHP_BRACE;
			if( is_continuation )
			  col -= PHP_CONTINUED_OFFSET;
			col += extra_indent;
		}
		{
			col += extra_indent;
		}
		
		push_spot( );
		php_indent_to( col );
		pop_spot( );
	} else {
		% Not in PHP block
		%insert( "\t" );
	}
	
}
%}}}
define php_indent_region_or_line( ) %{{{
{
	!if( is_visible_mark )
	  php_indent_line( );
	else
	{
		variable now,start,stop;
		check_region( 1 );
		stop = what_line( );
		pop_mark_1( );
		start = what_line( );
		push_mark( );
		forever
		{
			now = what_line( );
			if( now >= stop )
			  break;
			php_indent_line( );
			down_1( );
		}
		pop_spot( );
		flush( sprintf( "processed %d/%d lines.", start( ), stop ));		
	}
}
%}}}
define php_indent_buffer( ) %{{{
{
	variable col, max_line;
	push_spot( );
	eob( );
	max_line = what_line( );
	bob( );
	do
	{
		bol_skip_white( );
		indent_line( );
	} while( down_1( ) );
	
	trim_buffer( );
	flush( sprintf( "processed %d/%d lines.", what_line( ), max_line ));
	pop_spot( );
}
%}}}
define php_newline_and_indent( ) %{{{
{	
	variable PhpCcComment = "//";
	variable PhpBashComment = "#";
	
	if( bolp ( ) )
	{
		newline( );
		php_indent_line( );
		return;
	}
	
	variable col;
	variable PhpCcComment_len = strlen( PhpCcComment );
	variable PhpBashComment_len = strlen( PhpBashComment );
	
	if( PHP_Autoinsert_Comments )
	{
		col = what_column( );
		push_spot_bol( );
		if( looking_at( PhpCcComment ))
		{
			push_mark( );
			go_right( PhpCcComment_len );
			skip_white( );
			PhpCcComment = bufsubstr( );
			pop_spot( );
			newline( );
			if( col > PhpCcComment_len )
			  insert( PhpCcComment );
			return;
		} else if( looking_at( PhpBashComment )) {
			push_mark( );
			go_right( PhpBashComment_len );
			skip_white( );
			PhpBashComment = bufsubstr( );
			pop_spot( );
			newline( );
			if( col > PhpBashComment_len )
			  insert( PhpBashComment );
			return;
		}		  
		pop_spot( );
	}
	
	col = php_is_comment( );
	newline( );
	if( col )
	{
		php_indent_to( col );
		insert( "" );
	}
	else php_indent_line( );
}
%}}}
define php_insert_bra( ) %{{{
{
	if( php_parse_to_point( ) )
	  insert_char( '{' );
	else {
		push_spot( );
		php_bskip_over_comment( 0 );
		if( blooking_at( "," ), pop_spot( ) )
		{
			insert_char( '{' );
		} else { 
			push_spot( );
			skip_white( );
			if( eolp( ) )
			{
				bskip_white( );
				if( not( bolp( ) ) and PHP_BRA_NEWLINE, pop_spot( ) ) 
				  newline( );
				push_spot( );
				bskip_white( );
				bolp( );	       %  on stack
				pop_spot( );
				insert_char( '{' );
				if( ( ) ) 
				  php_indent_line( );   %  off stack
				eol( );
				if( PHP_BRA_NEWLINE ) 
				  php_newline_and_indent( );
			} else  {
				pop_spot( );
				insert_char( '{' );
			}
		}
	}
}
%}}}
define php_insert_ket( ) %{{{
{
	variable status = php_parse_to_point( );
	variable line = what_line( );
	
	push_spot( );
	skip_white( );
	push_spot( );
	if( status 
		or not( eolp( ) )
		or( 1 == find_matching_delimiter( '}' )) and( line == what_line( ) ))
		%or (bol_skip_white ( ), looking_at_char ('{')), pop_spot ( ))
	{
		pop_spot( );
		pop_spot( );
		if( PHP_KET_NEWLINE )
		{
			insert( "\n}" );
			php_indent_line( );
		}
		else
		  insert( "}" );
		blink_match( );
		return;
	}
	
	pop_spot( );
	bskip_white( );
	if( bolp( ), pop_spot( ) )
	{
		insert_char( '}' );
		trim( );
	} else {
		eol( );
		if( PHP_KET_NEWLINE )
		  insert( "\n}" );
		else
		  insert( "}" );
	}
	php_indent_line( );
	eol( ); 
	blink_match( );
	if( PHP_BRA_NEWLINE )
	  php_newline_and_indent( );
}
%}}}
define php_insert_colon( ) %{{{
{
	insert_char( ':' );
	!if( php_parse_to_point( ) )
	  php_indent_line( );
}
%}}}
define php_getname( tellstring ) %{{{
{
	variable gname = read_mini( tellstring, Null_String, Null_String );
	return gname;
}
%}}}
define php_ins_tn( str ) %{{{
{
	insert( str );
	php_indent_line( );
	insert( "\n" );
}
%}}}
define php_insert_function( ) %{{{
{
	variable name = php_getname( "function:" );
	php_ins_tn( sprintf( "function %s ( )", name ));
	php_ins_tn( "{" );
	php_ins_tn( "" );	
	php_ins_tn( "}" );
	bsearch( ")" );	
}
%}}}
define php_insert_class( ) %{{{
{
	variable name = php_getname( "class:" );
	php_ins_tn(sprintf( "class %s", name ));
	php_ins_tn( "{" );
	php_ins_tn( "" );	
	php_ins_tn( "}" );	
}
%}}}
define php_insert_tab( ) %{{{
{
	insert( "\t" );
}
%}}}
static define php_init_menu( menu ) %{{{
{
	menu_append_item( menu, "&Top of function", "php_top_of_function" );
	menu_append_item( menu, "&End of function", "php_end_of_function" );
	menu_append_item( menu, "&Mark function", "php_mark_function" );
	menu_append_item( menu, "&Mark matching", "php_mark_matching" );
	menu_append_separator( menu );
	menu_append_item( menu, "&Indent buffer", "php_indent_buffer" );
	menu_append_separator( menu );
	menu_append_item( menu, "&Insert class", "php_insert_class" );
	menu_append_item( menu, "&Insert function", "php_insert_function" );
	menu_append_item( menu, "&Insert brace", "php_insert_bra" );
	menu_append_item( menu, "&Insert ket", "php_insert_ket" );
	menu_append_item( menu, "&Insert colon", "php_insert_colon" );
	menu_append_separator( menu );
	menu_append_item( menu, "&Format paragraph", "php_format_paragraph" );
	menu_append_item( menu, "&Goto Match", "goto_match" );
	menu_append_item( menu, "&Insert TAB", "php_insert_tab" );
}
%}}}
$1 = "PHP";

!if( keymap_p( $1 )) 
  make_keymap( $1 ); %{{{
definekey( "indent_line", "\t", $1 );
definekey( "php_top_of_function", "\e^A", $1 );
definekey( "php_end_of_function", "\e^E", $1 );
definekey( "php_mark_function", "\e^H", $1 );
definekey( "php_mark_matching", "\e^M", $1 );
definekey( "php_insert_bra", "{", $1 );
definekey( "php_insert_ket", "}", $1 );
definekey( "php_insert_colon", ":", $1 );
definekey( "php_format_paragraph", "\eq", $1 );
definekey( "php_newline_and_indent", "\r", $1 );

definekey_reserved( "php_indent_region_or_line", "^R", $1 );
definekey_reserved( "php_indent_buffer", "^B", $1 );
definekey_reserved( "php_insert_class", "^C", $1 );
definekey_reserved( "php_insert_function", "^F", $1 );
definekey_reserved( "php_insert_tab","^I", $1 );
%}}}

% Now create and initialize the syntax tables. %{{{
create_syntax_table( $1 );
define_syntax( "/*", "*/", '%', $1 );          % comments
define_syntax( "#", "", '%', $1 );             % comments
define_syntax( "//", "", '%', $1 );            % comments
%define_syntax ("<>", '<', $1);
define_syntax( "([{", ")]}", '(', $1 );        % parentheses
define_syntax( '"', '"', $1 );                 % strings
define_syntax( '\'', '\'', $1 );               % strings
define_syntax( '\\', '\\', $1 );               % escape character
define_syntax( "0-9a-zA-Z_", 'w', $1 );        % words
define_syntax( "-+0-9a-fA-F.xXL", '0', $1 );   % numbers
define_syntax( ",;.:", ',', $1 );              % delimiters
define_syntax( "+-*/%=.&|^~<>!?@`", '+', $1 ); % operators
set_syntax_flags( $1, 0x05 );
%}}}

#ifdef HAS_DFA_SYNTAX %{{{
%%% DFA_CACHE_BEGIN %%%
static define setup_dfa_callback( name )
{
	dfa_enable_highlight_cache( "php.dfa", name );
	dfa_define_highlight_rule( "<%", "Qpreprocess", name );          % Asp style start tag
	dfa_define_highlight_rule( "%>", "Qpreprocess", name );          % Asp style end tag
	dfa_define_highlight_rule( "<\\?|<\\?php", "preprocess", name ); % Php style start tag 
	dfa_define_highlight_rule( "\\?>", "Qpreprocess", name ); % Php style end tag
	dfa_define_highlight_rule ("<!\\-\\-.*\\-\\-[ \t]*>", "Qcomment", name); % HTML comments
	dfa_define_highlight_rule ("<!\\-\\-", "comment", name); % HTML comments
	dfa_define_highlight_rule ("\\-\\-[ \t]*>", "comment", name); % HTML comments
	dfa_define_highlight_rule( "#.*", "comment", name );             % Shell style comment
	dfa_define_highlight_rule( "//.*", "comment", name );            % C++ style comment
	dfa_define_highlight_rule( "/\\*.*\\*/", "Qcomment", name );     % C style comment
	dfa_define_highlight_rule( "^([^/]|/[^\\*])*\\*/", "Qcomment", name ); % C style comment
	dfa_define_highlight_rule( "/\\*.*", "comment", name );          % C style comment
	dfa_define_highlight_rule( "^[ \t]*\\*+([ \t].*)?$", "comment", name ); % C style comment
	dfa_define_highlight_rule( "[A-Za-z_\\$][A-Za-z_0-9\\$]*", "Knormal", name );
	dfa_define_highlight_rule( "[ \t]+", "normal", name );
	dfa_define_highlight_rule( "[0-9]+(\\.[0-9][LlUu]*)?([Ee][\\+\\-]?[0-9]*)?","number", name );
	dfa_define_highlight_rule( "0[xX][0-9A-Fa-f]*[LlUu]*", "number", name );
	dfa_define_highlight_rule( "[\\(\\[{}\\]\\),;\\.:]", "delimiter", name ); % Delimiters:  ([{}]) 
	dfa_define_highlight_rule( "[%@\\?\\.\\-\\+/&\\*=<>\\|!~\\^]", "operator", name ); % Operators:  %@?.-+/&*=<>|!~^ 
	dfa_define_highlight_rule( "\"([^\"\\\\]|\\\\.)*\"", "string", name );
	dfa_define_highlight_rule( "\"([^\"\\\\]|\\\\.)*\\\\?$", "string", name );
	dfa_define_highlight_rule( "'([^'\\\\]|\\\\.)*'", "string", name );
	dfa_define_highlight_rule( "'([^'\\\\]|\\\\.)*\\\\?$", "string", name );
	dfa_build_highlight_table( name );
}
dfa_set_init_callback( &setup_dfa_callback, "PHP" );
%%% DFA_CACHE_END %%%
#endif
%}}}

% Type 0 keywords (keywords and constants) %{{{
() = define_keywords_n ($1,
"asdoifor",
2,0);

() = define_keywords_n ($1,
"andfornewvarxor",
3,0);

() = define_keywords_n ($1,
"argcargvcaseelsetrue",
4,0);

() = define_keywords_n ($1,
"breakclassfalsewhile",
5,0);

() = define_keywords_n ($1,
"elseifglobalphp_osswitch",
6,0);

() = define_keywords_n ($1,
"defaulte_errore_parseextendsforeach"
+ "globals",
7,0);

() = define_keywords_n ($1,
"continuee_notice__file__function"
+ "__line__php_self",
8,0);

() = define_keywords_n ($1,
"e_warning",
9,0);

() = define_keywords_n ($1,
"php_version",
11,0);

() = define_keywords_n ($1,
"php_errormsg",
12,0);

() = define_keywords_n ($1,
"http_get_vars",
13,0);

() = define_keywords_n ($1,
"http_post_vars",
14,0);

() = define_keywords_n ($1,
"http_cookie_vars",
16,0);
%}}}

% Type 1 keywords (functions) %{{{
() = define_keywords_n ($1,
"dlpi",
2,1);

() = define_keywords_n ($1,
"abschrcosdieendexpkeylogmaxminordpospow"
+ "sintan",
3,1);

() = define_keywords_n ($1,
"acosasinatanceilchopcopycoshdateeach"
+ "echoeregevalexecexitfeoffileftokglobis_a"
+ "joinleaklinklistmailmsqlnextpackprevrand"
+ "sinhsortsqrtstattanhtimetrim",
4,1);

() = define_keywords_n ($1,
"acosharrayasinhasortatanhbcaddbcdiv"
+ "bcmodbcmulbcpowbcsubchdirchgrpchmodchown"
+ "countcryptemptyeregifgetcfgetsflockfloor"
+ "flushfopenfputsfreadfseekfstatftellgzeof"
+ "hw_cphw_mvhypoticonvissetksortlstatltrim"
+ "mhashmkdirpopenprintrangeresetrmdirround"
+ "rsortrtrimsleepsplitsrandstrtrtouchumask"
+ "unsetusort",
5,1);

() = define_keywords_n ($1,
"arsortassertbccompbcsqrtbindecbzopen"
+ "bzreadchrootdblistdecbindechexdecoctdefine"
+ "deletefclosefflushfgetssfscanffwritegetcwd"
+ "getenvgmdategmp_orgzfilegzgetcgzgetsgzopen"
+ "gzputsgzreadgzseekgztellheaderhebrevhexdec"
+ "hw_whoifx_dointvalis_diris_intis_nankrsort"
+ "mktimeoctdecora_dopclosepg_ttyprintfputenv"
+ "recoderenamereturnrewindsizeofsplitisscanf"
+ "strchrstrcmpstrlenstrposstrrevstrspnstrstr"
+ "strtokstrvalsubstrsyslogsystemuasortuksort"
+ "uniqidunlinkunpackusleepyp_allyp_cat",
6,1);

() = define_keywords_n ($1,
"bcscalebzclosebzerrnobzerrorbzflush"
+ "bzwritecom_getcom_setcompactcurrentdbmopen"
+ "defineddirnameexplodeextractfgetcsvfilepro"
+ "ftp_getftp_putftp_pwdgetdategetmxrrgettext"
+ "gettypegmp_absgmp_addgmp_andgmp_cmpgmp_com"
+ "gmp_divgmp_gcdgmp_modgmp_mulgmp_neggmp_pow"
+ "gmp_subgmp_xorgzclosegzgetssgzwritehebrevc"
+ "hw_infohw_roothw_statimagegdimagesximagesy"
+ "implodeincludeini_getini_setis_boolis_file"
+ "is_linkis_longis_nullis_realmb_eregmt_rand"
+ "natsortodbc_doopendiropenlogpdf_arcpdf_new"
+ "pg_hostpg_portphpinfoprint_rreaddirrequire"
+ "sem_getsettypeshufflesnmpgetsnmpsetsoundex"
+ "sprintfstr_padstrcollstrcspnstristrstrncmp"
+ "strrchrstrrposswffillswffontswftextsymlink"
+ "tempnamtmpfileucfirstucwordsvirtualvprintf"
+ "yp_next",
7,1);

() = define_keywords_n ($1,
"basenamebzerrstrcal_infoccvs_add"
+ "ccvs_newclosedircloselogcom_loadconstant"
+ "cpdf_arcdba_opendba_syncdbmclosedbmfetch"
+ "dbx_sortdgettextdio_opendio_readdio_seek"
+ "dio_statfdf_openfdf_savefilesizefiletype"
+ "floatvalftp_cdupftp_execftp_fgetftp_fput"
+ "ftp_mdtmftp_pasvftp_quitftp_siteftp_size"
+ "getmygidgetmypidgetmyuidgmmktimegmp_fact"
+ "gmp_initgmp_powmgmp_signgmp_sqrtgzencode"
+ "gzrewindhw_closehw_dummyhw_errorhw_mapid"
+ "imagearcimagegifimagepngimap_uidin_array"
+ "ircg_msgis_arrayis_floatjdtounixldap_add"
+ "linkinfomb_eregimb_splitmsg_sendmt_srand"
+ "ngettextob_cleanob_flushob_startocierror"
+ "ocifetchocilogonociparseora_bindora_exec"
+ "ora_openoverloadpassthrupathinfopdf_arcn"
+ "pdf_clippdf_fillpdf_openpdf_rectpdf_save"
+ "pdf_showpdf_skewpg_closepg_querypg_trace"
+ "readfilereadlinereadlinkrealpathsnmpwalk"
+ "strftimeswfmorphswfmovieswfshapeudm_find"
+ "unixtojdvar_dumpvsprintfwordwrapyaz_hits"
+ "yaz_scanyaz_sortyaz_waityp_errnoyp_first"
+ "yp_matchyp_orderzip_openzip_read",
8,1);

() = define_keywords_n ($1,
"array_maparray_padarray_poparray_sum"
+ "cal_to_jdccvs_authccvs_doneccvs_initccvs_sale"
+ "ccvs_voidcheckdatecpdf_clipcpdf_fillcpdf_open"
+ "cpdf_rectcpdf_savecpdf_showcpdf_textcurl_exec"
+ "curl_initdba_closedba_fetchdba_popendbmdelete"
+ "dbmexistsdbminsertdbx_closedbx_errordbx_query"
+ "dcgettextdio_closedio_fcntldio_writedngettext"
+ "doublevalerror_logfdf_closefileatimefilectime"
+ "filegroupfileinodefilemtimefileownerfileperms"
+ "fpassthrufsockopenftp_chdirftp_closeftp_login"
+ "ftp_mkdirftp_nlistftp_rmdirftruncateget_class"
+ "getrusagegmp_div_qgmp_div_rgzdeflategzinflate"
+ "hw_insdochw_unlockicap_openifx_closeifx_error"
+ "ifx_queryimagecharimagecopyimagefillimagejpeg"
+ "imagelineimagewbmpimap_bodyimap_mailimap_open"
+ "imap_pingimap_sortini_alteriptcembediptcparse"
+ "ircg_joinircg_kickircg_nickircg_partis_double"
+ "is_finiteis_objectis_scalaris_stringlcg_value"
+ "ldap_bindldap_listldap_readldap_sortlocaltime"
+ "mb_strcutmb_strlenmb_strposmb_substrmcal_open"
+ "metaphonemicrotimeocicancelocicommitocilogoff"
+ "ocinlogonociplogonociresultodbc_execora_close"
+ "ora_errorora_fetchora_logonora_parseparse_str"
+ "parse_urlpdf_closepdf_scalepg_dbnamepg_delete"
+ "pg_insertpg_selectpg_updatephp_unamepreg_grep"
+ "proc_openqdom_treequotemetarewinddirserialize"
+ "setcookiesetlocalestrnatcmpstrtotimeswf_ortho"
+ "swf_scaleswfactionswfbitmapswfbuttonswfsprite"
+ "udm_errnoudm_errorurldecodeurlencodexml_parse"
+ "xptr_evalxslt_freeyaz_closeyaz_errnoyaz_error"
+ "yaz_rangeyp_masterzip_close",
9,1);

() = define_keywords_n ($1,
"addslashesarray_diffarray_fill"
+ "array_fliparray_keysarray_pusharray_rand"
+ "array_walkaspell_newbzcompressccvs_count"
+ "checkdnsrrcom_addrefcom_invokecom_isenum"
+ "cpdf_closecpdf_scalecurl_closecurl_errno"
+ "curl_errorcyrus_binddba_deletedba_exists"
+ "dba_insertdbase_opendbase_packdbmnextkey"
+ "dbmreplacedbplus_adddbplus_aqldbplus_sql"
+ "dbplus_tcldcngettextezmlm_hashfdf_create"
+ "fdf_set_apfrenchtojdftp_deleteftp_rename"
+ "getlastmodgetmyinodegetrandmaxgmp_clrbit"
+ "gmp_div_qrgmp_gcdextgmp_intvalgmp_invert"
+ "gmp_jacobigmp_randomgmp_setbitgmp_sqrtrm"
+ "gmp_strvalgmstrftimegzcompressgzpassthru"
+ "hw_connecthw_gettexthw_inscollicap_close"
+ "imagetypesimap_checkimap_closeimap_msgno"
+ "imap_popenircg_topicircg_whoisis_integer"
+ "is_numericjdtofrenchjdtojewishjdtojulian"
+ "jewishtojdjuliantojdldap_closeldap_errno"
+ "ldap_errorlocaleconvmb_strrposmcal_close"
+ "mcal_popenmcrypt_cbcmcrypt_cfbmcrypt_ecb"
+ "mcrypt_ofbmsql_closemsql_errormsql_query"
+ "mssql_bindmssql_initmuscat_getmysql_info"
+ "mysql_pingmysql_statncurses_nlnotes_body"
+ "ocicollmaxociexecuteociloadlobocinumcols"
+ "ocisavelobodbc_closeodbc_errorora_commit"
+ "ora_logoffora_plogonpcntl_execpcntl_fork"
+ "pdf_circlepdf_concatpdf_deletepdf_lineto"
+ "pdf_movetopdf_rotatepdf_strokepfpro_init"
+ "pfsockopenpg_connectpg_convertpg_copy_to"
+ "pg_lo_openpg_lo_readpg_lo_seekpg_lo_tell"
+ "pg_optionspg_untracephpcreditsphpversion"
+ "posix_killpreg_matchpreg_quotepreg_split"
+ "proc_closepspell_newqdom_errorreadgzfile"
+ "sem_removesession_idshell_execshm_attach"
+ "shm_detachshm_removeshmop_openshmop_read"
+ "shmop_sizestr_repeatstrcasecmpstrip_tags"
+ "strtolowerstrtoupperswf_lookatswf_nextid"
+ "swf_rotatetextdomainuser_errorvar_export"
+ "xpath_evalxslt_errnoxslt_erroryaz_record"
+ "yaz_searchyaz_syntax",
10,1);

() = define_keywords_n ($1,
"addcslashesapache_notearray_chunk"
+ "array_mergearray_shiftarray_slicecal_from_jd"
+ "ccvs_deleteccvs_lookupccvs_reportccvs_return"
+ "ccvs_statuschunk_splitcom_propgetcom_propput"
+ "com_propsetcom_releasecount_charscpdf_circle"
+ "cpdf_linetocpdf_movetocpdf_rotatecpdf_stroke"
+ "crack_checkctype_alnumctype_alphactype_cntrl"
+ "ctype_digitctype_graphctype_lowerctype_print"
+ "ctype_punctctype_spacectype_uppercurl_setopt"
+ "cyrus_closecyrus_querydba_nextkeydba_replace"
+ "dbase_closedbmfirstkeydbplus_currdbplus_find"
+ "dbplus_infodbplus_lastdbplus_nextdbplus_open"
+ "dbplus_prevdbplus_rzapdbplus_undodbx_compare"
+ "dbx_connectdebugger_ondotnet_loadeaster_date"
+ "easter_daysfbsql_closefbsql_errnofbsql_error"
+ "fbsql_queryfdf_set_optfile_existsftp_connect"
+ "ftp_rawlistftp_systypeget_browserget_cfg_var"
+ "gmp_hamdisthw_childrenhw_edittexthw_errormsg"
+ "hw_identifyhw_pconnecthwapi_hgcspibase_close"
+ "ibase_queryibase_transicap_reopenicap_snooze"
+ "ifx_connectifx_prepareimagecharupimagecreate"
+ "imageftbboximagefttextimagepsbboximagepstext"
+ "imagestringimap_alertsimap_appendimap_binary"
+ "imap_deleteimap_errorsimap_headerimap_qprint"
+ "imap_reopenimap_searchimap_setaclimap_status"
+ "imap_threadini_get_allini_restoreircg_notice"
+ "is_callableis_infiniteis_readableis_resource"
+ "is_writablejddayofweekjdmonthnameldap_delete"
+ "ldap_get_dnldap_modifyldap_renameldap_search"
+ "ldap_unbindlevenshteinmb_get_infomb_language"
+ "mb_strwidthmcal_reopenmcal_snoozemhash_count"
+ "msg_receivemsql_dbnamemsql_dropdbmsql_result"
+ "mssql_closemssql_querymuscat_givemysql_close"
+ "mysql_errnomysql_errormysql_querynatcasesort"
+ "ncurses_endncurses_rawnl_langinfoocicollsize"
+ "ocicolltrimocifreedescocirollbackocirowcount"
+ "odbc_commitodbc_cursorodbc_resultodbc_tables"
+ "ora_numcolsora_numrowspdf_curvetopdf_endpath"
+ "pdf_restorepdf_setdashpdf_setflatpdf_setfont"
+ "pdf_setgraypdf_show_xypg_end_copypg_last_oid"
+ "pg_lo_closepg_lo_writepg_metadatapg_num_rows"
+ "pg_pconnectpg_put_lineposix_timesposix_uname"
+ "recode_filesem_acquiresem_releasesesam_query"
+ "shm_get_varshm_put_varshmop_closeshmop_write"
+ "show_sourcesnmpwalkoidsocket_bindsocket_read"
+ "socket_recvsocket_sendsql_regcasestr_replace"
+ "strncasecmpswf_setfontswfgradientunserialize"
+ "xslt_createyaz_addinfoyaz_connectyaz_element"
+ "yaz_present",
11,1);

() = define_keywords_n ($1,
"array_filterarray_reducearray_search"
+ "array_splicearray_uniquearray_valuesaspell_check"
+ "base_convertbzdecompressccvs_commandccvs_reverse"
+ "class_existscpdf_curvetocpdf_newpathcpdf_restore"
+ "cpdf_rlinetocpdf_rmovetocpdf_setdashcpdf_setflat"
+ "cpdf_setgraycpdf_show_xyctype_xdigitcurl_getinfo"
+ "curl_versioncyrus_unbinddba_firstkeydba_optimize"
+ "dbase_createdbplus_chdirdbplus_closedbplus_errno"
+ "dbplus_firstdbplus_flushdbplus_rkeysdbplus_ropen"
+ "debugger_offdio_truncateereg_replacefbsql_commit"
+ "fbsql_resultfdf_get_filefdf_set_filefunc_get_arg"
+ "getimagesizegettimeofdaygmp_divexactgmp_legendre"
+ "gmp_popcountgzuncompressheaders_senthtmlentities"
+ "hw_getobjecthw_getremotehw_api->copyhw_api->find"
+ "hw_api->infohw_api->linkhw_api->lockhw_api->move"
+ "hw_api->useribase_commitibase_errmsgifx_errormsg"
+ "ifx_get_blobifx_get_charifx_getsqlcaifx_num_rows"
+ "ifx_pconnectimagecoloratimagedestroyimageellipse"
+ "imagepolygonimagesettileimagettfbboximagettftext"
+ "imap_expungeimap_headersimap_num_msginclude_once"
+ "ingres_closeingres_queryis_writeableldap_compare"
+ "ldap_connectldap_mod_addldap_mod_delmb_parse_str"
+ "mb_send_mailmcal_expungemsession_getmsession_inc"
+ "msession_setmsql_connectmsql_drop_dbmsql_listdbs"
+ "msql_numrowsmsql_regcasemssql_resultmuscat_close"
+ "muscat_setupmysql_resultncurses_beepncurses_bkgd"
+ "ncurses_echoncurses_inchncurses_initncurses_move"
+ "ncurses_nonlncurses_putpncurses_scrlnotes_search"
+ "notes_unreadob_end_cleanob_end_flushob_get_level"
+ "ob_gzhandlerocifetchintoocinewcursorodbc_binmode"
+ "odbc_columnsodbc_connectodbc_executeodbc_prepare"
+ "openssl_openopenssl_sealopenssl_signora_commiton"
+ "ora_rollbackovrimos_execpcntl_signalpdf_add_note"
+ "pdf_end_pagepdf_findfontpdf_get_fontpdf_open_gif"
+ "pdf_open_pdipdf_open_pngpdf_set_fontpdf_set_info"
+ "pdf_setcolorpg_copy_frompg_fetch_rowpg_field_num"
+ "pg_lo_createpg_lo_exportpg_lo_importpg_lo_unlink"
+ "posix_getcwdposix_getgidposix_getpidposix_getsid"
+ "posix_getuidposix_isattyposix_mkfifoposix_setgid"
+ "posix_setsidposix_setuidpreg_replaceprinter_list"
+ "printer_openpspell_checkrawurldecoderawurlencode"
+ "require_oncesesam_commitsession_nameshmop_delete"
+ "similar_textsnmprealwalksocket_closesocket_readv"
+ "socket_writestripslashessubstr_countswf_addcolor"
+ "swf_endshapeswf_fontsizeswf_getframeswf_mulcolor"
+ "swf_openfileswf_posroundswf_setframeswf_shapearc"
+ "swf_viewportswftextfieldsybase_closesybase_query"
+ "udm_cat_listudm_cat_pathudm_free_resxslt_process"
+ "xslt_set_logyaz_ccl_confyaz_databasezend_version",
12,1);

() = define_keywords_n ($1,
"apache_setenvarray_reversearray_unshift"
+ "cpdf_end_textcpdf_finalizecpdf_set_fontcyrus_connect"
+ "dbplus_rquerydbplus_updatediskfreespaceeregi_replace"
+ "fbsql_connectfbsql_drop_dbfbsql_stop_dbfdf_get_value"
+ "fdf_set_flagsfdf_set_valuefunc_get_argsfunc_num_args"
+ "get_meta_tagsgetallheadersgethostbyaddrgethostbyname"
+ "getservbynamegetservbyportgregoriantojdhw_getanchors"
+ "hw_getandlockhw_getparentshw_getrellinkhw_api_object"
+ "ibase_connectibase_executeibase_prepareibase_timefmt"
+ "ifx_copy_blobifx_fetch_rowifx_free_blobifx_free_char"
+ "imagecolorsetimageloadfontimagesetbrushimagesetpixel"
+ "imagesetstyleimagestringupimap_undeleteingres_commit"
+ "ircg_pconnectircg_set_fileis_executablejdtogregorian"
+ "mb_ereg_matchmb_http_inputmb_strimwidthmethod_exists"
+ "ming_setscalemsession_findmsession_listmsession_lock"
+ "msession_uniqmsg_get_queuemsg_set_queuemsql_createdb"
+ "msql_fieldlenmsql_list_dbsmsql_num_rowsmsql_pconnect"
+ "msql_selectdbmssql_connectmssql_executemt_getrandmax"
+ "mysql_connectmysql_db_namemysql_drop_dbncurses_addch"
+ "ncurses_clearncurses_delchncurses_erasencurses_flash"
+ "ncurses_getchncurses_hlinencurses_inschncurses_instr"
+ "ncurses_keyokncurses_mvcurncurses_napmsncurses_noraw"
+ "ncurses_vlinenotes_copy_dbnotes_drop_dbnotes_version"
+ "number_formatob_get_lengthocibindbynameocicollappend"
+ "ocicollassignocicolumnnameocicolumnsizeocicolumntype"
+ "ocifreecursorodbc_errormsgodbc_num_rowsodbc_pconnect"
+ "odbc_rollbackora_commitoffora_errorcodeora_getcolumn"
+ "ovrimos_closepcntl_waitpidpdf_close_pdipdf_closepath"
+ "pdf_get_valuepdf_open_filepdf_open_jpegpdf_open_tiff"
+ "pdf_set_valuepdf_setmatrixpdf_translatepfpro_cleanup"
+ "pfpro_processpfpro_versionpg_field_namepg_field_size"
+ "pg_field_typepg_get_resultpg_last_errorpg_num_fields"
+ "pg_send_queryphp_logo_guidphp_sapi_nameposix_ctermid"
+ "posix_getegidposix_geteuidposix_getpgidposix_getpgrp"
+ "posix_getppidposix_setegidposix_seteuidposix_setpgid"
+ "posix_ttynameprinter_abortprinter_closeprinter_write"
+ "readline_inforecode_stringsesam_connectsesam_execimm"
+ "session_startsession_unsetsocket_acceptsocket_create"
+ "socket_listensocket_selectsocket_sendtosocket_writev"
+ "stripcslashesstrnatcasecmpswf_closefileswf_endbutton"
+ "swf_endsymbolswf_fontslantswf_polarviewswf_popmatrix"
+ "swf_showframeswf_textwidthswf_translatesybase_result"
+ "trigger_errorwddx_add_varsxmlrpc_decodexmlrpc_encode"
+ "xslt_set_baseyaz_ccl_parseyaz_itemorderyp_err_string",
13,1);

() = define_keywords_n ($1,
"aspell_suggestassert_options"
+ "bindtextdomaincall_user_funcccvs_textvalue"
+ "clearstatcachecpdf_closepathcpdf_page_init"
+ "cpdf_set_titlecpdf_translatecrack_opendict"
+ "cybercash_decrcybercash_encrdbplus_errcode"
+ "dbplus_getlockdbplus_lockreldbplus_rchperm"
+ "dbplus_rcreatedbplus_resolvedbplus_rrename"
+ "dbplus_runlinkdbplus_saveposdbplus_tremove"
+ "domxml_new_docdomxml_versiondomxml_xmltree"
+ "escapeshellargescapeshellcmdexif_imagetype"
+ "exif_read_dataexif_thumbnailfbsql_database"
+ "fbsql_db_queryfbsql_hostnamefbsql_list_dbs"
+ "fbsql_num_rowsfbsql_passwordfbsql_pconnect"
+ "fbsql_rollbackfbsql_start_dbfbsql_username"
+ "fbsql_warningsfdf_get_statusfdf_set_status"
+ "ftp_get_optionftp_set_optionget_class_vars"
+ "gethostbynamelgetprotobynamegmp_prob_prime"
+ "highlight_filehw_childrenobjhw_docbyanchor"
+ "hw_getusernamehw_setlinkroothw_api->dbstat"
+ "hw_api->dcstathw_api->ftstathw_api->hwstat"
+ "hw_api->inserthw_api_contenthw_api->object"
+ "hw_api->removehw_api->unlockibase_blob_add"
+ "ibase_blob_getibase_pconnectibase_rollback"
+ "ifx_fieldtypesifx_nullformatifx_num_fields"
+ "imagecopymergeimagefilledarcimagefontwidth"
+ "imageinterlaceimagerectangleimap_fetchbody"
+ "imap_get_quotaimap_mail_copyimap_mail_move"
+ "imap_set_quotaimap_subscribeingres_connect"
+ "is_subclass_ofldap_start_tlsmb_ereg_search"
+ "mb_http_outputmcrypt_decryptmcrypt_encrypt"
+ "mcrypt_genericmsession_countmsg_stat_queue"
+ "msql_create_dbmsql_data_seekmsql_fetch_row"
+ "msql_fieldnamemsql_fieldtypemsql_numfields"
+ "msql_select_dbmsql_tablenamemssql_num_rows"
+ "mssql_pconnectmysql_db_querymysql_list_dbs"
+ "mysql_num_rowsmysql_pconnectncurses_addstr"
+ "ncurses_attronncurses_borderncurses_cbreak"
+ "ncurses_delwinncurses_filterncurses_has_ic"
+ "ncurses_has_ilncurses_insstrncurses_mvinch"
+ "ncurses_newwinncurses_noechoocicollgetelem"
+ "ocicolumnscaleocisavelobfileocisetprefetch"
+ "odbc_close_allodbc_fetch_rowodbc_field_len"
+ "odbc_field_numodbc_setoptionopenssl_verify"
+ "ora_columnnameora_columnsizeora_columntype"
+ "ora_fetch_intoovrimos_commitovrimos_cursor"
+ "ovrimos_resultparse_ini_filepcntl_wstopsig"
+ "pcntl_wtermsigpdf_begin_pagepdf_get_buffer"
+ "pdf_open_ccittpdf_open_imagepdf_setlinecap"
+ "pdf_show_boxedpg_fetch_arraypg_free_result"
+ "pg_last_noticepg_lo_read_allposix_getgrgid"
+ "posix_getgrnamposix_getloginposix_getpwnam"
+ "posix_getpwuidpreg_match_allpspell_suggest"
+ "read_exif_datasesam_errormsgsesam_rollback"
+ "sesam_seek_rowsession_decodesession_encode"
+ "set_time_limitshm_remove_varsocket_connect"
+ "socket_recvmsgsocket_sendmsgsubstr_replace"
+ "swf_actionplayswf_actionstopswf_definefont"
+ "swf_definelineswf_definepolyswf_definerect"
+ "swf_definetextswf_labelframeswf_pushmatrix"
+ "swf_startshapeswfdisplayitemsybase_connect"
+ "udm_free_agentvpopmail_errorxml_set_object"
+ "zend_logo_guidzip_entry_namezip_entry_open"
+ "zip_entry_read",
14,1);

() = define_keywords_n ($1,
"array_intersectarray_multisort"
+ "cpdf_begin_textcpdf_setlinecapcrack_closedict"
+ "create_functiondbase_numfieldsdbplus_freelock"
+ "dbplus_rcrtlikedbplus_setindexdbplus_unselect"
+ "dbplus_xlockreldisk_free_spacedomnode->prefix"
+ "domxml_open_memerror_reportingfbsql_create_db"
+ "fbsql_data_seekfbsql_db_statusfbsql_fetch_row"
+ "fbsql_field_lenfbsql_insert_idfbsql_read_blob"
+ "fbsql_read_clobfbsql_select_dbfbsql_tablename"
+ "function_existsget_object_varshw_changeobject"
+ "hw_deleteobjecthw_getchildcollhw_insertobject"
+ "hw_modifyobjecthw_new_documenthw_pipedocument"
+ "hw_api->checkinhw_api->contenthw_api->parents"
+ "hw_api->replaceibase_blob_echoibase_blob_info"
+ "ibase_blob_openibase_fetch_rowifx_create_blob"
+ "ifx_create_charifx_free_resultifx_update_blob"
+ "ifx_update_charifxus_free_slobifxus_open_slob"
+ "ifxus_read_slobifxus_seek_slobifxus_tell_slob"
+ "imagecolorexactimagedashedlineimagefontheight"
+ "imagepscopyfontimagepsfreefontimagepsloadfont"
+ "imap_bodystructimap_headerinfoimap_last_error"
+ "imap_num_recentingres_num_rowsingres_pconnect"
+ "ingres_rollbackircg_disconnectircg_ignore_add"
+ "ircg_ignore_delircg_set_on_dieldap_explode_dn"
+ "ldap_get_optionldap_get_valuesldap_next_entry"
+ "ldap_set_optionmb_convert_kanamb_detect_order"
+ "mb_ereg_replacemcal_date_validmcal_event_init"
+ "mcal_time_validmsession_createmsession_plugin"
+ "msession_unlockmsql_field_seekmsql_fieldflags"
+ "msql_fieldtablemsql_freeresultmsql_listfields"
+ "msql_listtablesmsql_num_fieldsmssql_data_seek"
+ "mssql_fetch_rowmssql_select_dbmysql_create_db"
+ "mysql_data_seekmysql_fetch_rowmysql_field_len"
+ "mysql_insert_idmysql_select_dbmysql_tablename"
+ "mysql_thread_idncurses_addnstrncurses_attroff"
+ "ncurses_attrsetncurses_bkgdsetncurses_has_key"
+ "ncurses_mvaddchncurses_mvdelchncurses_mvgetch"
+ "ncurses_mvhlinencurses_mvvlinencurses_qiflush"
+ "ncurses_refreshncurses_resettyncurses_savetty"
+ "ncurses_scr_setncurses_timeoutncurses_ungetch"
+ "ncurses_use_envncurses_vidattrnotes_create_db"
+ "notes_find_notenotes_list_msgsnotes_mark_read"
+ "ob_get_contentsocicolumnisnullocidefinebyname"
+ "odbc_autocommitodbc_fetch_intoodbc_field_name"
+ "odbc_field_typeodbc_num_fieldsodbc_procedures"
+ "odbc_result_allodbc_statisticsopenssl_csr_new"
+ "ovrimos_connectovrimos_executeovrimos_prepare"
+ "pcntl_wifexitedpdf_add_outlinepdf_add_pdflink"
+ "pdf_add_weblinkpdf_attach_filepdf_close_image"
+ "pdf_end_patternpdf_fill_strokepdf_place_image"
+ "pdf_set_leadingpdf_setlinejoinpdf_setpolydash"
+ "pdf_setrgbcolorpdf_stringwidthpg_cancel_query"
+ "pg_escape_byteapg_fetch_objectpg_fetch_result"
+ "pg_field_prtlenpg_result_errorposix_getgroups"
+ "posix_getrlimitprinter_end_docsesam_fetch_row"
+ "session_destroyset_file_buffersocket_recvfrom"
+ "socket_shutdownsocket_strerrorswf_enddoaction"
+ "swf_getfontinfoswf_onconditionswf_perspective"
+ "swf_placeobjectswf_shapelinetoswf_shapemoveto"
+ "swf_startbuttonswf_startsymbolsybase_num_rows"
+ "sybase_pconnectudm_alloc_agentudm_api_version"
+ "udm_open_storedversion_comparevpopmail_passwd"
+ "wddx_packet_endxml_parser_freexmlrpc_get_type"
+ "xmlrpc_set_typeyaz_scan_resultzip_entry_close",
15,1);

() = define_keywords_n ($1,
"array_key_existsaspell_check_raw"
+ "call_user_methodcom_load_typelibcpdf_add_outline"
+ "cpdf_fill_strokecpdf_import_jpegcpdf_rotate_text"
+ "cpdf_set_creatorcpdf_set_leadingcpdf_set_subject"
+ "cpdf_setlinejoincpdf_setrgbcolorcpdf_stringwidth"
+ "cybermut_testmacdbase_add_recorddbase_get_record"
+ "dbase_numrecordsdbplus_getuniquedbplus_rcrtexact"
+ "dbplus_rsecindexdbplus_unlockreldisk_total_space"
+ "domxml_open_fileextension_loadedfbsql_autocommit"
+ "fbsql_field_namefbsql_field_seekfbsql_field_type"
+ "fbsql_num_fieldsfdf_add_templatefdf_set_encoding"
+ "filepro_retrievefilepro_rowcountget_current_user"
+ "get_defined_varsget_parent_classgetprotobynumber"
+ "highlight_stringhtmlspecialcharshw_document_size"
+ "hw_free_documenthw_getanchorsobjhw_getparentsobj"
+ "hw_incollectionshw_insertanchorshw_api_attribute"
+ "hw_api->checkouthw_api->childrenhw_api->identify"
+ "hw_api->userlistibase_blob_closeibase_field_info"
+ "ibase_free_queryibase_num_fieldsicap_fetch_event"
+ "icap_list_alarmsicap_list_eventsicap_store_event"
+ "ifxus_close_slobifxus_write_slobimagecolorstotal"
+ "imagecopyresizedimagepalettecopyimagepsslantfont"
+ "imap_fetchheaderimap_listmailboximap_scanmailbox"
+ "imap_unsubscribeingres_fetch_rowircg_html_encode"
+ "ircg_set_currentis_uploaded_fileldap_first_entry"
+ "ldap_free_resultldap_get_entriesldap_mod_replace"
+ "mb_eregi_replacemcal_day_of_weekmcal_day_of_year"
+ "mcal_fetch_eventmcal_list_alarmsmcal_list_events"
+ "mcal_store_eventmcrypt_create_ivmdecrypt_generic"
+ "msession_connectmsession_destroymsession_getdata"
+ "msession_listvarmsession_randstrmsession_setdata"
+ "msession_timeoutmsg_remove_queuemsql_fetch_array"
+ "msql_fetch_fieldmsql_free_resultmsql_list_fields"
+ "msql_list_tablesmssql_field_namemssql_field_seek"
+ "mssql_field_typemssql_num_fieldsmuscat_setup_net"
+ "mysql_field_namemysql_field_seekmysql_field_type"
+ "mysql_num_fieldsncurses_addchstrncurses_baudrate"
+ "ncurses_clrtobotncurses_clrtoeolncurses_curs_set"
+ "ncurses_deletelnncurses_doupdatencurses_echochar"
+ "ncurses_flushinpncurses_getmousencurses_insdelln"
+ "ncurses_insertlnncurses_isendwinncurses_killchar"
+ "ncurses_longnamencurses_mvaddstrncurses_nocbreak"
+ "ncurses_scr_dumpncurses_scr_initncurses_slk_attr"
+ "ncurses_slk_initncurses_standendncurses_standout"
+ "ncurses_termnamencurses_wrefreshnotes_nav_create"
+ "ob_iconv_handlerocicolumntyperawocifreestatement"
+ "ociinternaldebugocinewcollectionocinewdescriptor"
+ "ociserverversionocistatementtypeodbc_fetch_array"
+ "odbc_field_scaleodbc_foreignkeysodbc_free_result"
+ "odbc_gettypeinfoodbc_longreadlenodbc_next_result"
+ "odbc_primarykeysopenssl_csr_signopenssl_free_key"
+ "openssl_pkey_newovrimos_num_rowsovrimos_rollback"
+ "pcntl_wifstoppedpdf_add_bookmarkpdf_end_template"
+ "pdf_get_fontnamepdf_get_fontsizepdf_initgraphics"
+ "pdf_set_durationpdf_set_text_pospdf_setgray_fill"
+ "pdf_setlinewidthpg_affected_rowspg_escape_string"
+ "pg_field_is_nullpg_result_statusprinter_draw_bmp"
+ "printer_draw_pieprinter_end_pagesesam_diagnostic"
+ "sesam_disconnectsesam_field_namesesam_num_fields"
+ "session_readonlysession_registersocket_iovec_add"
+ "socket_iovec_setswf_actiongeturlswf_definebitmap"
+ "swf_fonttrackingswf_modifyobjectswf_removeobject"
+ "swf_shapecurvetoswf_shapefilloffsybase_data_seek"
+ "sybase_fetch_rowsybase_select_dbudm_check_stored"
+ "udm_close_storedwddx_deserializexml_error_string"
+ "xptr_new_context",
16,1);

() = define_keywords_n ($1,
"apache_lookup_urical_days_in_month"
+ "connection_statuscpdf_save_to_filecpdf_set_keywords"
+ "cpdf_set_text_poscpdf_setgray_fillcpdf_setlinewidth"
+ "dbplus_freerlocksdbplus_restoreposdbplus_xunlockrel"
+ "domnode->set_namefbsql_change_userfbsql_create_blob"
+ "fbsql_create_clobfbsql_fetch_arrayfbsql_fetch_assoc"
+ "fbsql_fetch_fieldfbsql_field_flagsfbsql_field_table"
+ "fbsql_free_resultfbsql_list_fieldsfbsql_list_tables"
+ "fbsql_next_resultfile_get_contentsfilepro_fieldname"
+ "filepro_fieldtypeget_class_methodsget_resource_type"
+ "hw_docbyanchorobjhw_insertdocumenthw_api->srcsofdst"
+ "ibase_blob_cancelibase_blob_createibase_blob_import"
+ "ibase_free_resulticap_delete_eventifx_affected_rows"
+ "ifx_byteasvarcharifx_textasvarcharifxus_create_slob"
+ "ignore_user_abortimagecolorclosestimagecolorresolve"
+ "imagecreatefromgdimagefilltoborderimagegammacorrect"
+ "imagepsencodefontimagepsextendfontimagesetthickness"
+ "imap_getmailboxesimap_mail_composeimap_setflag_full"
+ "ingres_autocommitingres_field_nameingres_field_type"
+ "ingres_num_fieldsircg_channel_modeircg_get_username"
+ "ldap_parse_resultmb_output_handlermb_regex_encoding"
+ "mcal_append_eventmcal_date_comparemcal_delete_event"
+ "mcal_is_leap_yearmcal_week_of_yearmcrypt_list_modes"
+ "msql_fetch_objectmssql_fetch_arraymssql_fetch_assoc"
+ "mssql_fetch_batchmssql_fetch_fieldmssql_free_result"
+ "mssql_guid_stringmssql_next_resultmysql_change_user"
+ "mysql_fetch_arraymysql_fetch_assocmysql_fetch_field"
+ "mysql_field_flagsmysql_field_tablemysql_free_result"
+ "mysql_list_fieldsmysql_list_tablesncurses_addchnstr"
+ "ncurses_color_setncurses_erasecharncurses_halfdelay"
+ "ncurses_init_pairncurses_mousemaskncurses_mvaddnstr"
+ "ncurses_mvwaddstrncurses_noqiflushncurses_slk_clear"
+ "ncurses_slk_colorncurses_slk_touchncurses_termattrs"
+ "ncurses_typeaheadnotes_create_notenotes_header_info"
+ "notes_mark_unreadob_implicit_flushocicollassignelem"
+ "ocifetchstatementocifreecollectionociwritelobtofile"
+ "odbc_fetch_objectovrimos_fetch_rowovrimos_field_len"
+ "ovrimos_field_numpcntl_wexitstatuspcntl_wifsignaled"
+ "pdf_add_locallinkpdf_add_thumbnailpdf_begin_pattern"
+ "pdf_continue_textpdf_get_parameterpdf_get_pdi_value"
+ "pdf_makespotcolorpdf_open_pdi_pagepdf_set_parameter"
+ "pdf_set_text_risepdf_setmiterlimitpfpro_process_raw"
+ "printer_create_dcprinter_delete_dcprinter_draw_line"
+ "printer_draw_textprinter_start_docpspell_new_config"
+ "sesam_fetch_arraysesam_field_arraysesam_free_result"
+ "session_save_pathset_error_handlersocket_get_option"
+ "socket_get_statussocket_iovec_freesocket_last_error"
+ "socket_set_optionswf_getbitmapinfoswf_startdoaction"
+ "sybase_field_seeksybase_num_fieldsudm_check_charset"
+ "udm_get_doc_countudm_get_res_fieldudm_get_res_param"
+ "vpopmail_add_uservpopmail_del_userwddx_packet_start"
+ "xml_parser_createxpath_new_contextxslt_set_encoding",
17,1);

() = define_keywords_n ($1,
"array_count_valuesconnection_aborted"
+ "connection_timeoutconvert_cyr_stringcpdf_continue_text"
+ "cpdf_finalize_pagecpdf_output_buffercpdf_set_text_rise"
+ "cpdf_setmiterlimitcyrus_authenticatedbplus_undoprepare"
+ "domattribute->namedomnode->dump_nodedomnode->node_name"
+ "domnode->node_typefbsql_fetch_objectfbsql_set_lob_mode"
+ "filepro_fieldcountfilepro_fieldwidthget_included_files"
+ "get_required_filesgmp_perfect_squarehw_connection_info"
+ "hw_getchildcollobjhw_getchilddoccollhw_getsrcbydestobj"
+ "hw_output_documenthw_api->dstanchorshw_api->srcanchors"
+ "ibase_fetch_objecticonv_get_encodingiconv_set_encoding"
+ "ifx_htmltbl_resultimagealphablendingimagecolorallocate"
+ "imagecopymergegrayimagecopyresampledimagecreatefromgif"
+ "imagecreatefrompngimagecreatefromxbmimagecreatefromxpm"
+ "imagefilledellipseimagefilledpolygonimap_createmailbox"
+ "imap_deletemailboximap_getsubscribedimap_renamemailbox"
+ "ingres_fetch_arrayingres_field_scaleircg_is_conn_alive"
+ "ldap_count_entriesmailparse_msg_freemb_detect_encoding"
+ "mb_ereg_search_posmcal_days_in_monthmcal_event_set_end"
+ "mcrypt_generic_endmcrypt_get_iv_sizemcrypt_module_open"
+ "ming_useswfversionmove_uploaded_filemsession_get_array"
+ "msession_set_arraymsql_affected_rowsmssql_fetch_object"
+ "mssql_field_lengthmysql_fetch_objectncurses_define_key"
+ "ncurses_has_colorsncurses_init_colorncurses_mvaddchstr"
+ "ncurses_slk_attronncurses_ungetmouseocicolumnprecision"
+ "openssl_csr_exportovrimos_fetch_intoovrimos_field_name"
+ "ovrimos_field_typeovrimos_num_fieldsovrimos_result_all"
+ "pdf_add_annotationpdf_add_launchlinkpdf_begin_template"
+ "pdf_close_pdi_pagepdf_place_pdi_pagepdf_set_info_title"
+ "pdf_setgray_strokepg_client_encodingpg_connection_busy"
+ "printer_create_penprinter_delete_penprinter_draw_chord"
+ "printer_get_optionprinter_select_penprinter_set_option"
+ "printer_start_pagepspell_config_modepspell_config_repl"
+ "sesam_fetch_resultsession_unregistersocket_clear_error"
+ "socket_create_pairsocket_getpeernamesocket_getsockname"
+ "socket_iovec_allocsocket_iovec_fetchsocket_set_timeout"
+ "swf_shapefillsolidswf_shapelinesolidswfbutton_keypress"
+ "sybase_fetch_arraysybase_fetch_fieldsybase_free_result"
+ "vpopmail_alias_addvpopmail_alias_delvpopmail_alias_get"
+ "vpopmail_auth_userxml_get_error_codezip_entry_filesize",
18,1);

() = define_keywords_n ($1,
"cpdf_add_annotationcpdf_set_action_url"
+ "cpdf_setgray_strokedbase_delete_recorddbplus_freealllocks"
+ "domattribute->valuedomelement->tagnamedomnode->attributes"
+ "domnode->clone_nodedomnode->last_childdomnode->node_value"
+ "fbsql_affected_rowsfbsql_fetch_lengthsfdf_next_field_name"
+ "get_extension_funcshw_document_bodytaghw_document_content"
+ "hw_getobjectbyqueryhw_api_error->counthw_api_reason->type"
+ "ifx_blobinfile_modeifx_fieldpropertiesimagecolorsforindex"
+ "imagecreatefromjpegimagecreatefromwbmpimap_clearflag_full"
+ "imap_fetch_overviewimap_fetchstructureimap_listsubscribed"
+ "imap_mailboxmsginfoingres_fetch_objectingres_field_length"
+ "ldap_get_attributesldap_get_values_lenldap_next_attribute"
+ "ldap_next_referencemailparse_msg_parsemb_convert_encoding"
+ "mb_ereg_search_initmb_ereg_search_regsmcrypt_generic_init"
+ "mcrypt_get_key_sizemcrypt_module_closemhash_get_hash_name"
+ "msession_disconnectmssql_rows_affectedmysql_affected_rows"
+ "mysql_escape_stringmysql_fetch_lengthsmysql_get_host_info"
+ "ncurses_mvaddchnstrncurses_scr_restorencurses_slk_attroff"
+ "ncurses_slk_attrsetncurses_slk_refreshncurses_slk_restore"
+ "ncurses_start_colorodbc_specialcolumnsopenssl_pkey_export"
+ "ovrimos_free_resultovrimos_longreadlenpdf_get_image_width"
+ "pdf_open_image_filepdf_set_border_dashpdf_set_info_author"
+ "pdf_set_text_matrixpg_connection_resetprinter_create_font"
+ "printer_delete_fontprinter_draw_elipseprinter_select_font"
+ "pspell_new_personalsesam_affected_rowssession_module_name"
+ "session_write_closesocket_iovec_deletesocket_set_blocking"
+ "socket_set_nonblockswf_actiongotoframeswf_actiongotolabel"
+ "swf_actionnextframeswf_actionprevframeswf_actionsettarget"
+ "swf_addbuttonrecordsybase_fetch_objectudm_set_agent_param"
+ "vpopmail_add_domainvpopmail_del_domainwddx_serialize_vars",
19,1);

() = define_keywords_n ($1,
"call_user_func_array"
+ "cpdf_set_text_matrixcrack_getlastmessage"
+ "dbase_replace_recorddomdocument->doctype"
+ "domnode->child-nodesdomnode->first_child"
+ "domnode->get_contentdomnode->parent_node"
+ "domnode->set_contentdomnode->unlink_node"
+ "get_declared_classesget_magic_quotes_gpc"
+ "hw_getremotechildrenhw_api_content->read"
+ "hw_api_error->reasonhw_api->insertanchor"
+ "hw_api_object->counthw_api_object->title"
+ "hw_api_object->valueicap_create_calendar"
+ "icap_delete_calendaricap_rename_calendar"
+ "imagecolorclosesthwbimagecolordeallocate"
+ "imagecolorexactalphaimagecreatetruecolor"
+ "imagefilledrectangleircg_fetch_error_msg"
+ "ircg_nickname_escapeldap_first_attribute"
+ "ldap_first_referenceldap_parse_reference"
+ "ldap_set_rebind_procmailparse_msg_create"
+ "mb_convert_variablesmb_decode_mimeheader"
+ "mb_encode_mimeheadermb_internal_encoding"
+ "mcal_create_calendarmcal_delete_calendar"
+ "mcal_event_set_alarmmcal_event_set_class"
+ "mcal_event_set_startmcal_event_set_title"
+ "mcal_next_recurrencemcal_rename_calendar"
+ "mcrypt_enc_self_testmhash_get_block_size"
+ "mysql_get_proto_infomysql_list_processes"
+ "ncurses_delay_outputodbc_field_precision"
+ "odbc_tableprivilegesopenssl_error_string"
+ "pdf_closepath_strokepdf_get_image_height"
+ "pdf_get_majorversionpdf_get_minorversion"
+ "pdf_set_border_colorpdf_set_border_style"
+ "pdf_set_char_spacingpdf_set_info_creator"
+ "pdf_set_info_subjectpdf_set_word_spacing"
+ "pdf_setrgbcolor_fillpg_connection_status"
+ "printer_create_brushprinter_delete_brush"
+ "printer_select_brushpspell_clear_session"
+ "pspell_config_createpspell_config_ignore"
+ "pspell_save_wordlistreadline_add_history"
+ "sesam_settransactionsession_cache_expire"
+ "snmp_get_quick_printsnmp_set_quick_print"
+ "socket_create_listensybase_affected_rows"
+ "udm_add_search_limitudm_free_ispell_data"
+ "udm_load_ispell_datawddx_serialize_value"
+ "xml_parser_create_nsxmlrpc_server_create"
+ "xslt_set_sax_handler",
20,1);

() = define_keywords_n ($1,
"array_change_key_case"
+ "array_merge_recursivecpdf_closepath_stroke"
+ "cpdf_set_char_spacingcpdf_set_current_page"
+ "cpdf_set_word_spacingcpdf_setrgbcolor_fill"
+ "domdocument->dump_memdomdocumenttype->name"
+ "domnode->append_childdomnode->next_sibling"
+ "domnode->remove_childdomnode->replace_node"
+ "fbsql_set_transactionfile_register_wrapper"
+ "file_get_wrapper_dataget_defined_constants"
+ "get_defined_functionsget_loaded_extensions"
+ "hw_getchilddoccollobjhw_api_attribute->key"
+ "hw_api_object->assignhw_api_object->insert"
+ "hw_api_object->removeimagecolortransparent"
+ "imagecreatefromstringingres_field_nullable"
+ "mb_ereg_search_getposmb_ereg_search_setpos"
+ "mcrypt_generic_deinitmcrypt_get_block_size"
+ "mysql_get_client_infomysql_get_server_info"
+ "ncurses_def_prog_modencurses_mouseinterval"
+ "odbc_columnprivilegesodbc_procedurecolumns"
+ "openssl_get_publickeypdf_get_pdi_parameter"
+ "pdf_open_memory_imagepdf_set_horiz_scaling"
+ "pdf_set_info_keywordspreg_replace_callback"
+ "pspell_add_to_sessionreadline_list_history"
+ "readline_read_historyrestore_error_handler"
+ "session_cache_limitersession_is_registered"
+ "xml_parse_into_structxml_parser_get_option"
+ "xml_parser_set_optionxmlrpc_decode_request"
+ "xmlrpc_encode_requestxmlrpc_server_destroy"
+ "xpath_eval_expressionxslt_set_sax_handlers"
+ "yp_get_default_domain",
21,1);

() = define_keywords_n ($1,
"apache_child_terminate"
+ "call_user_method_arraycpdf_set_font_map_file"
+ "cpdf_set_horiz_scalingdomdocument->dump_file"
+ "domnode->insert_beforedomnode->is_blank_node"
+ "domnode->replace_childhw_document_attributes"
+ "hw_document_setcontenthw_getobjectbyqueryobj"
+ "hw_api->insertdocumenthw_api->objectbyanchor"
+ "imagecolorclosestalphaimagecolorresolvealpha"
+ "ingres_field_precisionircg_nickname_unescape"
+ "mailparse_msg_get_partmailparse_uudecode_all"
+ "mb_ereg_search_getregsmb_preferred_mime_name"
+ "mcrypt_enc_get_iv_sizemcrypt_get_cipher_name"
+ "mcrypt_list_algorithmsming_setcubicthreshold"
+ "mssql_get_last_messagemysql_unbuffered_query"
+ "ncurses_def_shell_modeopenssl_get_privatekey"
+ "openssl_public_decryptopenssl_public_encrypt"
+ "pdf_set_text_renderingpdf_setrgbcolor_stroke"
+ "pg_set_client_encodingprinter_draw_rectangle"
+ "printer_draw_roundrectpspell_add_to_personal"
+ "pspell_config_personalreadline_clear_history"
+ "readline_write_historyregister_tick_function"
+ "swf_actionwaitforframevpopmail_add_domain_ex"
+ "vpopmail_alias_get_allvpopmail_del_domain_ex"
+ "xslt_set_error_handler",
22,1);

() = define_keywords_n ($1,
"bind_textdomain_codeset"
+ "cpdf_place_inline_imagecpdf_set_page_animation"
+ "cpdf_set_text_renderingcpdf_setrgbcolor_stroke"
+ "cybermut_creerreponsecmdbplus_setindexbynumber"
+ "define_syslog_variablesdomattribute->specified"
+ "domnode->append_siblingdomnode->owner_document"
+ "fbsql_database_passwordhw_getobjectbyquerycoll"
+ "hw_api_attribute->valuehw_api->dstofsrcanchors"
+ "imagetruecolortopaletteimap_mime_header_decode"
+ "java_last_exception_getmailparse_stream_encode"
+ "mb_decode_numericentitymb_encode_numericentity"
+ "mb_substitute_charactermcal_event_set_category"
+ "mcrypt_enc_get_key_sizemcrypt_module_self_test"
+ "ncurses_slk_noutrefreshopenssl_private_decrypt"
+ "openssl_private_encryptpspell_config_save_repl"
+ "quoted_printable_decodeswf_actiontogglequality"
+ "swf_shapefillbitmapclipswf_shapefillbitmaptile"
+ "sybase_get_last_messageudm_clear_search_limits"
+ "vpopmail_set_user_quotaxml_set_default_handler"
+ "xml_set_element_handlerxslt_set_scheme_handler",
23,1);

() = define_keywords_n ($1,
"domnode->has_attributess"
+ "domnode->has_child_nodesfbsql_get_autostart_info"
+ "get_magic_quotes_runtimehw_api_attribute->values"
+ "hw_api_content->mimetypehw_api->insertcollection"
+ "import_request_variablesmailparse_msg_parse_file"
+ "mcal_event_add_attributemcrypt_enc_is_block_mode"
+ "mssql_min_error_severitymysql_character_set_name"
+ "mysql_real_escape_stringncurses_can_change_color"
+ "pspell_store_replacementsession_set_save_handler"
+ "set_magic_quotes_runtimeunregister_tick_function"
+ "xslt_set_scheme_handlerszip_entry_compressedsize",
24,1);

() = define_keywords_n ($1,
"cpdf_set_font_directories"
+ "domdocumenttype->entitiesdomelement->get_attribute"
+ "domelement->has_attributedomelement->set_attribute"
+ "domnode->previous_siblingfdf_set_javascript_action"
+ "java_last_exception_clearmcal_event_set_recur_none"
+ "mcrypt_enc_get_block_sizemcrypt_enc_get_modes_name"
+ "pdf_closepath_fill_strokepspell_config_runtogether"
+ "session_get_cookie_paramssession_set_cookie_params"
+ "sybase_min_error_severityvpopmail_add_alias_domain"
+ "vpopmail_alias_del_domainxmlrpc_server_call_method",
25,1);

() = define_keywords_n ($1,
"cpdf_closepath_fill_stroke"
+ "cybermut_creerformulairecmdomdocument->html_dump_mem"
+ "domdocumenttype->notationsdomdocumenttype->public_id"
+ "domdocumenttype->system_idfdf_set_submit_form_action"
+ "get_html_translation_tablehw_getobjectbyquerycollobj"
+ "hw_api_reason->descriptionhw_api->setcommitedversion"
+ "mailparse_msg_extract_partmcal_event_set_description"
+ "mcal_event_set_recur_dailymssql_min_message_severity"
+ "ncurses_use_default_colorsncurses_use_extended_names"
+ "openssl_csr_export_to_fileprinter_logical_fontheight"
+ "register_shutdown_functionsybase_min_client_severity"
+ "sybase_min_server_severityxml_get_current_byte_index",
26,1);

() = define_keywords_n ($1,
"cpdf_set_viewer_preferences"
+ "dbase_get_record_with_namesdomdocument->create_comment"
+ "domdocument->create_elementhw_api_object->attreditable"
+ "ircg_lookup_format_messagesmailparse_msg_get_part_data"
+ "mailparse_msg_get_structuremcal_event_set_recur_weekly"
+ "mcal_event_set_recur_yearlymcrypt_module_is_block_mode"
+ "openssl_pkey_export_to_filesybase_min_message_severity"
+ "xml_get_current_line_numberzip_entry_compressionmethod",
27,1);

() = define_keywords_n ($1,
"domelement->remove_attribute"
+ "readline_completion_functionvpopmail_add_alias_domain_ex",
28,1);

() = define_keywords_n ($1,
"domdocument->create_attribute"
+ "domdocument->create_text_nodedomdocument->document_element"
+ "ircg_register_format_messagesmcrypt_enc_is_block_algorithm"
+ "ncurses_assume_default_colorsxml_get_current_column_number"
+ "xml_set_notation_decl_handlerxmlrpc_server_register_method",
29,1);

() = define_keywords_n ($1,
"domdocument->get_element_by_id"
+ "domelement->get_attribute_nodedomelement->set_attribute_node"
+ "domprocessinginstruction->datahw_api_attribute->langdepvalue"
+ "mcrypt_enc_get_algorithms_namexml_set_character_data_handler",
30,1);

() = define_keywords_n ($1,
"cpdf_global_set_document_limits"
+ "mailparse_msg_extract_part_filemcal_fetch_current_stream_event"
+ "mcrypt_module_get_algo_key_size",
31,1);

() = define_keywords_n ($1,
"domdocumenttype->internal_subset"
+ "domprocessinginstruction->targetmcrypt_module_is_block_algorithm"
+ "xmlrpc_parse_method_descriptions",
32,1);

() = define_keywords_n ($1,
"domdocument->create_cdata_section"
+ "mcal_event_set_recur_monthly_mdaymcal_event_set_recur_monthly_wday"
+ "mcrypt_module_get_algo_block_size",
33,1);

() = define_keywords_n ($1,
"domdocument->add_root [deprecated]"
+ "mcrypt_enc_get_supported_key_sizesmcrypt_enc_is_block_algorithm_mode"
+ "xml_set_end_namespace_decl_handler",
34,1);

() = define_keywords_n ($1,
"domelement->get_elements_by_tagname"
+ "xml_set_external_entity_ref_handler",
35,1);

() = define_keywords_n ($1,
"domdocument->create_entity_reference"
+ "domdocument->get_elements_by_tagnamexml_set_start_namespace_decl_handler"
+ "xml_set_unparsed_entity_decl_handlerxmlrpc_server_add_introspection_data",
36,1);

() = define_keywords_n ($1,
"mcrypt_module_get_supported_key_sizes"
+ "mcrypt_module_is_block_algorithm_mode",
37,1);

() = define_keywords_n ($1,
"mailparse_determine_best_xfer_encoding"
+ "xml_set_processing_instruction_handler",
38,1);

() = define_keywords_n ($1,
"domdocument->create_processing_instruction",
42,1);

() = define_keywords_n ($1,
"xmlrpc_server_register_introspection_callback",
45,1);
%}}}

%!%+
%\function{php_mode}
%\synopsis{php_mode}
%\usage{Void php_mode ();}
%\description
% This is a mode that is dedicated to faciliate the editing of PHP language files.
% It calls the function \var{php_mode_hook} if it is defined. It also manages
% to recognice whetever it is in a php block or in a html block, for those people
% that doesnt seperate function from form ;)
%
% Functions that affect this mode include:
%#v+
%  function:             default binding:
%  php_top_of_function        ESC Ctrl-A
%  php_end_of_function        ESC Ctrl-E
%  php_mark_function          ESC Ctrl-H
%  php_mark_matching          ESC Ctrl-M
%  php_indent_buffer          Ctrl-C Ctrl-B
%  php_insert_class           Ctrl-C Ctrl-C
%  php_insert_function        Ctrl-C Ctrl-F
%  php_insert_bra             {
%  php_insert_ket             }
%  php_insert_colon           :
%  php_format_paragraph       ESC q
%  indent_line                TAB
%  newline_and_indent         RETURN
%  goto_match                 Ctrl-\
%  php_insert_tab             Ctrl-C Ctrl-I
%#v-
% Variables affecting indentation include:
%#v+
% PHP_INDENT
% PHP_BRACE
% PHP_BRA_NEWLINE
% PHP_KET_NEWLINE
% PHP_COLON_OFFSET
% PHP_CONTINUED_OFFSET
% PHP_CLASS_OFFSET
% PHP_Autoinsert_Comments
% PHP_SWITCH_OFFSET
%#v-
% Hooks: \var{php_mode_hook}
%!%-
define php_mode( )
{
	variable kmap = "PHP";
	set_mode( kmap, 2 );
	use_keymap( kmap );
	use_syntax_table( kmap );
	set_buffer_hook( "par_sep", "php_paragraph_sep" );
	set_buffer_hook( "indent_hook", "php_indent_region_or_line" );
	set_buffer_hook( "newline_indent_hook", "php_newline_and_indent" ); 
	
	mode_set_mode_info( "PHP", "fold_info", "//{{{\r//}}}\r\r" );
	mode_set_mode_info( "PHP", "init_mode_menu", &php_init_menu );	
	run_mode_hooks( "php_mode_hook" );
}

provide( "php_mode" );
