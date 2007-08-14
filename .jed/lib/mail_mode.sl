% -*- SLang -*-

% mail_mode.sl
%
% This is an almost complete rewrite of 
% Ulli "Framstag" Horlacher's <framstag@belwue.de>
% mail_mode for jed.
%
% Thomas Roessler <roessler@guug.de>
% Wed Aug 12 15:53:49 MEST 1998
%
%
%
%
% Do as you wish with this code under the following conditions:
% 1) leave this notice intact
% 2) don't try to sell it
% 3) don't try to pretend it is your own code
%
%
% mail_mode.sl implements a mail mode that is useful for editing mail/news.
% To invoke, add this line to $JED_ROOT/lib/defaults.sl or $HOME/.jedrc :
%
%	autoload("mail_mode","mail_mode");
%
% Optionally, set your favourite quote colour by addiding this function to
% $JED_ROOT/lib/jed.rc or $HOME/.jedrc :
%
%	define mail_mode_hook() {
%	  if (USE_ANSI_COLORS) set_color("preprocess","green","black");
%	}
%
% to invoke mail_mode automatically, you can add the following
%   in $HOME/.elm/elmrc (if you use elm):
%	editor = jed %s -tmp -f mail_mode
%   in $HOME/.muttrc (if you use mutt):
%	set editor="jed %s -tmp -f mail_mode"
%   in $HOME/.tin/tinrc (if you use tin):
%	default_editor_format=jed %F -g %N -tmp -f mail_mode
%   in $HOME/.slrnrc (if you use slrn):
%	set editor_command "jed --score-arrange-score %s -g %d -tmp --mail-mode"


% Do we recognize mbox style "From " lines as headers?
variable mail_mode_have_mbox = 1;
variable mail_maybe_header = 1;

create_syntax_table("Mail");
define_syntax('>' ,'#', "Mail");

define bol_skip_tags(ntags)
{
  variable col = 0;
  variable n = 0;

  bol();
  
  while(looking_at_char('>') or looking_at_char(' ')
	or looking_at_char('\t'))
  {
    if(looking_at_char('>'))
    {
      if(n == ntags)
	break;
      
      n++;      
      col = what_column();
    }
    
    !if(right(1))
      break;
  }
  
  goto_column(col);
  if(looking_at_char('>'))
    right(1);
}

define count_tags()
{
  variable n = 0;
  
  push_spot();
  bol();
  while(looking_at_char('>')
	or looking_at_char(' ')
	or looking_at_char('\t'))
  {
    if(looking_at_char('>'))
      n++;
    
    !if(right(1))
      break;
  }
  pop_spot();
  return(n);
}

define bol_skip_all_tags()
{
  bol_skip_tags(count_tags());
}

define dequote_buffer(ntags)
{
  variable n = count_tags();

  push_spot();
  bob();
  
  while(1)
  {
    bol();
    push_mark();
    bol_skip_tags(ntags);
    if(ntags < n)
      skip_white();
    del_region();
    !if(down(1))
      break;
  }
  pop_spot();
}

define requote_buffer(ntags)
{
  variable i;
  
  push_spot();
  bob();
  
  while(1)
  {
    bol();
    for(i = 0; i < ntags; i++)
      insert(">");
	 
    !if(down(1))
      break;
  }

  pop_spot();
}
  
define empty_quoted_line()
{
  push_spot();

  bol();
  while(looking_at_char('>') or looking_at_char(' ') or 
	looking_at_char('\t'))
  {
    if(not(right(1)))
      break;
  }
  skip_white();
  eolp();
  
  pop_spot();
}


define mail_is_tag()
{
  push_spot();
  bol();
  (mail_mode_have_mbox and bobp() and looking_at("From ")) or
    (1 == re_looking_at("^[A-Za-z][^: ]*:"));
  pop_spot();
}

define mail_have_header()
{
  push_spot();
  bob();
  mail_is_tag();
  pop_spot();
}

define mail_is_body()
{
  !if(mail_maybe_header)
    return 1;
  
  !if(mail_have_header())
    return 1;

  push_spot();
  re_bsearch("^$");
  pop_spot();
}

define mail_is_header_tag()
{
  if(mail_is_body())
    return 0;
  
  return mail_is_tag();
  
}

define maybe_signature()
{
  variable a, b;
  
  push_spot();
  bol_skip_all_tags();
  skip_white();

  !if(looking_at("--"))
  {
    pop_spot();
    return 0;
  }

  right(2);
  skip_white();
  eolp();
  pop_spot();
}

define mail_parsep()
{
  push_spot();

  bol();
  if(not (mail_is_body()))
  {
    (mail_is_header_tag() or
     (skip_white(), eolp()));
  }
  else
  {
    (maybe_signature() or
     (skip_white(), eolp()) or
     empty_quoted_line()
     );
  }

  pop_spot();
}

define mail_backward_paragraph()
{
  variable n;
  
  if(mail_parsep())
    return;
  
  n = count_tags();
  while(not(mail_parsep()) and (count_tags() == n))
  {
    !if(up(1))
      break;
  }
  bol();
}

define mail_forward_paragraph()
{
  variable n;
  
  if(mail_parsep())
    return;
  
  n = count_tags();
  
  while(not(mail_parsep()) and (count_tags() == n))
  {
    !if(down(1))
      break;
  }
  bol();
}

define mail_begin_of_paragraph()
{
  mail_backward_paragraph();
  !if(bobp())
    down(1);
}

define mail_select_paragraph()
{
  if(mail_parsep())
  {
    push_mark();
    return;
  }
  
  mail_begin_of_paragraph();
  
  push_mark();
  
  mail_forward_paragraph();
  eol();
  !if(eobp())
    up(1);
  eol();
}
    
define dequote()
{
  push_spot();
  
  !if(markp())
    mail_select_paragraph();
  
  narrow();
  dequote_buffer(1);
  widen();
  
  pop_spot();
}
  
define requote()
{
  push_spot();
  
  !if(markp())
    mail_select_paragraph();

  narrow();
  requote_buffer(1);
  widen();
  pop_spot();
}

define mail_fix_quotes()
{
  variable l, m;

  push_spot();
  bob();
  while(not(mail_is_body()))
  {
    if(not(down(1)))
      break;
  }
  
  if(not(mail_is_body()))
  {
    pop_spot();
    return;
  }

  push_spot();

  %% pass 1: pull quote tags together.
  
  while(1)
  {
    bol();

    if(empty_quoted_line())
    {
      while(not(eolp()))
	del();
    }
    
    while(looking_at(">>"))
    {
      if(not(right(1)))
	break;
    }

    if(looking_at("> >"))
    {
      del(); del(); del();
      insert(">>");
    }
    else
    {
      if(not(down(1)))
	break;
    }
  }

  pop_spot();

  % pass 2: insert correct paragraph separators.
  
  m = -1;
  while(1)
  {
    l = m;
    if(mail_parsep())
      m = -1;
    else
      m = count_tags();

    !if(mail_parsep())
    {
      if((m != -1) and (l != -1) and (m != l))
      {
	bol();
	push_spot();
	insert("\n");
	pop_spot();
	down(1);
      }
    }
    !if(down(1))
      break;
  }
  
  pop_spot();
}
  
define reformat_header()
{
  push_spot();
  
  while(not(mail_is_header_tag()))
  {
    !if(up_1()) break;
  }
  
  if(not(mail_is_header_tag()))
  {
    pop_spot();
    return;
  }

  bol();
  while(not(looking_at(":"))) go_right(1);
  go_right(1);
  push_spot();
  insert("\n"); bol_trim(); bol(); insert(" ");
  call("format_paragraph");
  pop_spot(); del();
  
  pop_spot();
}

define reformat_quote()
{
  variable n;
  variable o;

  variable l1, l2;
  
  n = count_tags();
  o = mail_maybe_header;

  l1 = 0; l2 = 0;
  
  mail_maybe_header = 0;
  
  push_spot();
  
  !if(markp())
  {
    push_spot();
    mail_begin_of_paragraph();
    l1 = what_line();
    pop_spot();
    l2 = what_line();

    mail_select_paragraph();
  }

  narrow();
  dequote_buffer(n);
  bob();
  down(l2 - l1);
  call("format_paragraph");
  requote_buffer(n);
  widen();

  mail_maybe_header = o;
  
  pop_spot();
}

define mail_indent_calculate()
{
  variable col = 0;

  push_spot_bol();

  !if(re_bsearch ("[^ \t\n]"))
  {
    pop_spot();
    return col;
  }
  
  bol_skip_white();
  col = what_column() - 1;

  pop_spot();
  return col;
}
  
define mail_indent_line()
{
  variable col;

  push_spot();
  col = mail_indent_calculate();
  if(not(mail_is_body()))
  {
    if(mail_is_header_tag())
      col = 0;
    else
    {
      if(col == 0)
	col = 1;
    }
  }

  bol_trim();
  whitespace(col);
  pop_spot();
}

define mail_reformat()
{
  if(mail_is_body())
    reformat_quote();
  else
    reformat_header();
}

define mail_mode() 
{
  variable km = "mail_map";
  variable buf = "*mail*";
  
  no_mode();
  set_mode("Mail", 1);
  use_syntax_table("Mail");

  !if (strcmp(buf,whatbuf)) 
  {
    if (keymap_p(km)) 
      use_keymap(km);
  }

  local_setkey("mail_reformat", "\eq");

  % somewhat meaningful bindings
  local_setkey("dequote","^C<");
  local_setkey("requote","^C>");

  set_buffer_hook("par_sep", "mail_parsep");
  set_buffer_hook("indent_hook", "mail_indent_line");
  
  runhooks("text_mode_hook");
  runhooks("mail_mode_hook");

  save_buffers();
}

. "dequote" "requote" "reformat_quote"
. "mail_mode" "mail_fix_quotes"
loop (_stkdepth) add_completion(());
