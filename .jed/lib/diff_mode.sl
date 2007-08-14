% diff_mode.sl -*- mode: Slang; mode: fold -*-
% Unified diff mode for jed
% Written by Dino Leonardo Sangoi <g1001863@univ.trieste.it>
% on a shiny day.
%
% It does highlighting a bit like mcedit (the midnight commander editor) does.
% (It uses dfa syntax highlighting).
%
% It helps also editing diffs, a normally unsafe operation (it can rebuild diff markers
% after editing). You can also remove a whole block.
%
% This works only on unified diffs right now, and probably forever.
%
%_debug_info = 1;
%_traceback = 1;

%%%% Static Functions
%%%%{{{
static define _diff_is_marker()
{
	orelse
	{ looking_at("+++ ") }
	{ looking_at("--- ") }
	{ looking_at("diff ") }
	{ looking_at("Only in ") };
}

static define _diff_parse_block_info(l)
{
	variable pos, len;

	% Uhmm, there's a better way to do this?
	if (string_match(l, "@@ \\-\\([0-9][0-9]\\)*,\\([0-9][0-9]*\\) \\+\\([0-9][0-9]\\)*,\\([0-9][0-9]*\\) @@", 1) == 0) {
		flush(l);
		sleep(2);
		error("malformed block header");
	}
	(pos, len) = string_match_nth(1);
	integer(l[[pos:pos+len]]);
	(pos, len) = string_match_nth(2);
	integer(l[[pos:pos+len]]);
	(pos, len) = string_match_nth(3);
	integer(l[[pos:pos+len]]);
	(pos, len) = string_match_nth(4);
	integer(l[[pos:pos+len]]);
	return;
}

static define _diff_count_block()
{
	variable countplus = 0;
	variable countminus = 0;
	variable countspace = 0;

	while (down_1) {
		bol();
		if (eobp)
			break;
		switch(what_char())
		{ case '+' : countplus++; }
		{ case '-' : countminus++; }
		{ countspace++; }
	}
	return (countplus, countminus, countspace);
}
%%%%}}}

%%%% Fast movement functions: these are a bit like c_{top,end}_of_function().
%%%%{{{
% Go to the top of diffed file
define diff_top_of_file()
{
	push_mark();
	if (bol_bsearch("--- ") == 0) {
		bob();
		if (bol_fsearch("--- ") == 0) {
			pop_mark(1);
			error("start of file not found.");
		}
	}
	if (up_1) {
		bol;
		if (looking_at("diff ") == 0)
			go_down_1;
	}
	pop_mark(0);
}

% Go to the end of diffed file
define diff_end_of_file()
{
	% Skip Junk AND first marker
	do {
		!if (down_1)
			break;
		bol();
	} while (_diff_is_marker());
	if (bol_fsearch("--- ") == 0) {
		eob();
		% I can only assume this is the last file-block.
	}
	while (_diff_is_marker()) {
		!if (up_1)
			break;
		bol;
	}
}

% Go to the top of diffed block
define diff_top_of_block()
{
	push_mark();
	if (bol_bsearch("@@ ") == 0) {
		bob();
		if (bol_fsearch("@@ ") == 0) {
			pop_mark(1);
			error("start of block not found.");
		}
	}
	pop_mark(0);
}

% Go to the end of diffed block
define diff_end_of_block()
{
	% Skip Junk
	do {
		!if (down_1)
			break;
		bol();
	} while (_diff_is_marker());
	if (bol_fsearch("@@ ") == 0) {
		eob();
		% I can only assume this is the last block.
	}
%	go_up_1();
}
%%%%}}}

%%%% Mark and narrow blocks
%%%%{{{
% mark the current file.
define diff_mark_file(skipheader)
{
	diff_top_of_file();
	if (skipheader) {
		do {
			!if (down_1)
				break;
			bol();
		} while (looking_at("+++ ") or looking_at("--- "));
	}

	push_mark();
	diff_end_of_file();
}

% mark the current block.
define diff_mark_block(skipheader)
{
	diff_top_of_block();
	if (skipheader) {
		go_down_1;
		bol();
	}

	push_mark();
	diff_end_of_block();
	bol();
	while (_diff_is_marker() or looking_at("@@ ")) {
		!if (up_1)
			break;
		bol();
	}
}

% narrows the current file.
define diff_narrow_to_file(skipheader)
{
	diff_mark_file(skipheader);
	narrow();
}

% narrows the current block.
define diff_narrow_to_block(skipheader)
{
	diff_mark_block(skipheader);
	narrow();
}
%%%%}}}

%%%% Redo diff markers after editing
%%%%{{{
% Rewrite a block header after editing it.
define diff_redo_block(old_off, new_off)
{
	variable countminus = 0;
	variable countplus = 0;
	variable countspace = 0;
	variable oldpos, oldsize, newpos, newsize;
	variable c;
	variable oldheader, newheader;

	push_spot();
	diff_narrow_to_block(0);
	bob();

	oldheader = line_as_string();
	(oldpos, oldsize, newpos, newsize) = _diff_parse_block_info(oldheader);

	(countplus, countminus, countspace) = _diff_count_block();
	countplus += countspace;
	countminus += countspace;
	newheader = sprintf("@@ -%d,%d +%d,%d @@", oldpos+old_off, countminus, newpos+new_off, countplus);
	flush(sprintf("@@ -%d,%d +%d,%d @@   -->   %s", oldpos, oldsize, newpos, newsize, newheader));

	if (strcmp(oldheader, newheader) != 0) {
		bob();
		call("kill_line");
		insert(newheader + "\n");
	}
	widen();
	pop_spot();
	return (old_off + countminus - oldsize, new_off + countplus - newsize);
}

static define _diff_redo_from_here(oldoff, newoff)
{
	% Hack to avoid parsing two times the first block
	diff_top_of_block();
	diff_end_of_block();
	variable done = eolp();
	while (1) {
		(oldoff, newoff) = diff_redo_block(oldoff, newoff);
		diff_end_of_block();
		if (eolp()) {
			if (done)
				break;
			done = 1;
		}
	}
}

define diff_redo_file()
{
	push_spot();
	diff_narrow_to_file(0);

	bob();
	_diff_redo_from_here(0, 0);
	widen();
	pop_spot();
}
%%%%}}}

%%%% Remove junk routines
%%%%{{{
% Remove "Only in..." lines (from diffs without -N)
define diff_remove_only_lines()
{
	push_spot();
	bob();
	while (bol_fsearch("Only in ")) {
		bol();
		call("kill_line");
	}
	pop_spot();
}
%%%%}}}

%%%% Remove block, rebuilding markers
%%%%{{{
define diff_remove_block(redo)
{
	% TODO: if no blocks left in this file, remove also the file markers
	variable countplus, countminus;

	push_spot();
	push_spot();
	diff_narrow_to_file(0); % Be safe
	pop_spot();
	diff_narrow_to_block(0);
	diff_top_of_block();
	(countplus, countminus,) = _diff_count_block();
	mark_buffer();
	call("kill_region");
	widen(); % block
	% This leaves an extra void line, kill it
	if (eobp())
		call("backward_delete_char_untabify");
	else
		call("kill_line");

	if (redo and not eobp()) {
		diff_end_of_block();
		_diff_redo_from_here(0, countminus-countplus);
	}

	widen(); % file
	pop_spot();

}
%%%%}}}

%%%% Standard mode things: keymap, syntax, highlighting
%%%%{{{
static variable Diff = "Diff";

require("keydefs");
define diff_add_saurus_bindings()
{
	definekey("diff_top_of_file", Key_Shift_F11, Diff);
	definekey("diff_end_of_file", Key_Shift_F12, Diff);
	definekey("diff_top_of_block", Key_Ctrl_F11, Diff);
	definekey("diff_end_of_block", Key_Ctrl_F12, Diff);
	definekey("(,) = diff_redo_block(0,0)", Key_F12, Diff);
	definekey("diff_redo_file()", Key_F11, Diff);
	definekey("diff_remove_block(1)", Key_F8, Diff);
	definekey("diff_remove_only_lines()", Key_F9, Diff);
	definekey("diff_jump_to_(1)", "", Diff);

	%%%% Other Functions
	% diff_mark_file(skipheader);
	% diff_mark_block(skipheader);
	% diff_narrow_to_file(skipheader);
	% diff_narrow_to_block(skipheader);
}

!if (keymap_p(Diff)) {
	make_keymap(Diff);
	diff_add_saurus_bindings();
}

create_syntax_table(Diff);
% set_syntax_flags (Diff, 5); % case insensitive + C-mode

#ifdef HAS_DFA_SYNTAX
%%% John!! We BADLY need a better set of color handling intrinsics !!!
set_color("keyword3",       "lightgray",   "blue");   % @@
set_color("keyword4",       "lightgray",   "red");  % Only
set_color("keyword5",       "red",         "black");  % -
set_color("keyword6",       "green",       "black");  % +
set_color("keyword7",       "red",   "lightgray");    % ---
set_color("keyword8",       "green",      "lightgray");    % +++
set_color("keyword9",       "black",       "lightgray");    % diff
%%% DFA_CACHE_BEGIN %%%
static define setup_dfa_callback (name)
{
	% for debugging
	dfa_enable_highlight_cache("diff.dfa", name);
	dfa_define_highlight_rule("^diff .*$", "keyword9", name);
	dfa_define_highlight_rule("^\\+\\+\\+ .*$", "keyword8", name);
	dfa_define_highlight_rule("^\\-\\-\\- .*$", "keyword7", name);
	dfa_define_highlight_rule("^\\+.*$", "keyword6", name);
	dfa_define_highlight_rule("^\\-.*$", "keyword5", name);
	dfa_define_highlight_rule("^Only .*$", "keyword4", name);
	dfa_define_highlight_rule("^Binary .*$", "keyword4", name);
	dfa_define_highlight_rule("^@@ .*$", "keyword3", name);
	dfa_build_highlight_table(name);
}
dfa_set_init_callback (&setup_dfa_callback, Diff);
%%% DFA_CACHE_END %%%
#endif
%%%%}}}

%%%% Jump to functions
%%%%{{{

require("walk");
define diff_jump_to_(new)
{
	variable name, pos;
	variable oldpos, newpos;
	variable marker;
	variable delta = what_line();
	% FIXME: Consider Added and removed lines while computing delta.
	walk_mark_current_position();
	diff_top_of_block();
	delta -= what_line();
	line_as_string(); % on stack
	(oldpos, , newpos, ) = _diff_parse_block_info();
	if (new) {
		marker = "+++ ";
		oldpos = newpos;
	} else
		marker = "--- ";
	oldpos += delta;
	diff_top_of_file();
	() = bol_fsearch(marker);
	name = line_as_string();
	newpos = is_substr(name, "\t") - 2;
	name = name[[4:newpos]];
	read_file(name);
	goto_line(oldpos);
	message(name+":"+string(oldpos));
	walk_goto_current_position();
	walk_store_marked_position();
}

%%%%}}}

define diff_mode()
{
	% Highlighting NEEDS dfa for this mode to work.
	enable_dfa_syntax_for_mode(Diff);

	set_mode(Diff, 0);
	use_syntax_table(Diff);
	use_keymap(Diff);
	run_mode_hooks("diff_mode_hook");
}
