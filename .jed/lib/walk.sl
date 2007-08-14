% walk.sl       -*- mode: SLang; mode: fold -*-
% Save a stack of positions.
%
%_traceback = 1;
%_debug_info = 1;

%{{{ What is in this file?
% User Defined Options:
% Walk_Size              : How Big The Save Buffer should be.
%                          set This BEFORE Loading this File.
% Walk_Use_Current_Window: 1 = use sw2buf() when changing buffer (use
%                              same window of old buffer).
%                          0 = use pop2buf() when changing buffer (open
%                              a new window)
%
% Interface:
%       walk_mark_current_position()
%               Save Current position to a temporary variable for later use.
%       walk_return_to_marked_position()
%               Restore the temporary position, useful for error recovery.
%       walk_store_marked_position()
%               Store temporary position to stack.
%       walk_goto_current_position()
%               goto current position honoring Walk_Use_Current_Window.
%       walk_backward()
%               Turn backwards on position stack.
%       walk_forward()
%               Undo a BackTrace, effectively going forward.
%       _walk_dump_info()
%       	Dump Debug info...
%
% A normal usage would be:
% define my_func()
% {
%	ERROR_BLOCK {
%		...;
%		walk_return_to_marked_position();
%		...;
%	}
%	walk_mark_current_position();
%
%	% move as you like, using setbuf() to change buffer
%	% temporary and setbuf() + walk_goto_current_position()
%       % when you have reached the final destination.
%
%	walk_store_marked_position();
% }
%
% and bind walk_backward()/walk_forward() to some keys.
% I use <ESC-,> for the first, <ESC-M>  for the second. (<ESC-.>
% is bound to find_tag())
%}}}

implements("Walk");

%{{{ User-definable variables
% Yes, this can be done with a linked list, but I what to keep a limit on
% walk size, just in case.
custom_variable("Walk_Size", 32);
custom_variable("Walk_Use_Current_Window", 1);
%}}}

%{{{ Static variables

% Saurus Functions For walk
static variable BakBuffer_array = Mark_Type[Walk_Size];
static variable Temp_Pos = Mark_Type;
static variable push_pos = 0;
static variable last_pos = 0;
static variable forw_pos = 0;
%}}}

%{{{ "Stack Pointer" Handling
% Two unanswered questions about next two functions:
% 1) Is useful to code these in rpn?
% 2) how do I declare these as static?
% Moves Forward an index.
%static define walk_move_forward(pos)
%{
%        pos++;
%        if (pos >= Walk_Size)
%                pos = 0;
%        return pos;
%}
. ( 1 + dup Walk_Size >=
. { pop 0 } if
. ) walk_move_forward

% Moves backwards an index.
%static define walk_move_backward(pos)
%{
%        pos--;
%        if (pos < 0)
%                pos = Walk_Size - 1;
%        return pos;
%}
. ( dup 0 ==
. { pop Walk_Size } if
.   1 -
. ) walk_move_backward
%}}}

%{{{ internal Movement function
static define walk_goto_position(p)
{
	user_mark_buffer(p);  % On Stack.
        if (Walk_Use_Current_Window)
                sw2buf(());
        else
                pop2buf(());
	goto_user_mark(p);
}
%}}}

% ******* Interface *******

%{{{ Position Handling
% Put Data On Current Entry
public define walk_mark_current_position()
{
	Temp_Pos = create_user_mark();
}

% Set The Stack Pointers, this is done in another function to simplify
% Error Handling (in fact, you can avoid it at all, calling this when
% you know all went well).
public define walk_store_marked_position()
{
        BakBuffer_array[push_pos] = Temp_Pos;
        push_pos = walk_move_forward(push_pos);
        if (push_pos == last_pos)
                last_pos = walk_move_forward(last_pos);
        forw_pos = push_pos;
}
%}}}

%{{{ Movement Functions
% When you are happy with the position reached using setbuf(), call this
% to move to current position onoring the Walk_Use_Current_Window user
% defined option.
public define walk_goto_current_position()
{
        walk_goto_position(create_user_mark());
}

% Turn Back from a Tag lookup.
public define walk_backward()
{
        if ( push_pos == last_pos )
                error("walk: No BackTrace Data");
	% save current position for walk_forward()
        BakBuffer_array[push_pos] = create_user_mark();
        push_pos = walk_move_backward(push_pos);
        walk_goto_position(BakBuffer_array[push_pos]);
}

% Undo a BackTrace, effectively going forward.
public define walk_forward()
{
        if (push_pos == forw_pos)
                error("walk: No Forward Info.");
        push_pos = walk_move_forward(push_pos);
        walk_goto_position(BakBuffer_array[push_pos]);
}

% Return to position marked with walk_mark_current_position(), useful
% for error recovery.
public define walk_return_to_marked_position()
{
	setbuf(user_mark_buffer(Temp_Pos));
	goto_user_mark(Temp_Pos);
}
%}}}

%{{{ Debugging Functions
static define __ShowPos(p, i)
{
        variable a, b;

        a = "";
        if (push_pos == i)
                a = strcat(a, "p> ");
        if (last_pos == i)
                a = strcat(a, "l> ");
        if (forw_pos == i)
                a = strcat(a, "f> ");

        if (p != NULL) {
		% Oops, This Works only with a patch...
		variable buffer_name = "buf";
		%		if (p.buffer_name != NULL)
		%		     buffer_name = p.buffer_name;
		vinsert("%9s%d: %s[%d,%d]\n", a, i,
			buffer_name, p.line, p.column);
	}
}

define _walk_dump_info()
{
        variable i = 0;

        setbuf("*traceback*");
        insert("--- walk Dump:\n");
        vinsert("    Walk_Size = %d\n", Walk_Size);
        vinsert("    push_pos = %d\n", push_pos);
        vinsert("    last_pos = %d\n", last_pos);
        vinsert("    forw_pos = %d\n", forw_pos);
        if (Temp_Pos == NULL)
                insert("    Temp_Pos = **NULL**\n");
        else {
                insert("    Temp_Pos = \n");
                __ShowPos(Temp_Pos, -1);
        }
        insert("    Stack:\n");
        for (i=0; i<Walk_Size; i++)
        {
                __ShowPos(BakBuffer_array[i], i);
        }
}
%}}}

provide("walk");
