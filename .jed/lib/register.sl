% Register facility.  This provides up to 95 pastebuffers.
%
% CHANGELOG
% =========
%
% v1.1 2001/07/15
%
%	o Added support to save/load registers (see 'regslave.sl' and compare
%	  with 'history.sl').
%
%	o Few changes to use 'colorin'. See 'colorin.sl' for more information.
%
% v1.0 2000/12/29
%
%	o Added register management from the menu bar.
%
%	o 'register' evolved from no-mode to 'quasimode'. That is,
%	  it behaves like 'minued' but without edition/update capabilities.
%	  (Francesc Rocher <f.rocher@computer.org>)
%
% Modified by JED
%

require( "colorin" );

static variable Max_Registers = 95;
static variable Register_Buffer_Arrays = String_Type[Max_Registers];
Register_Buffer_Arrays [*] = "";

define get_registers()
{
    return( Register_Buffer_Arrays[*] );
}

define set_registers( reg )
{
    variable i = 0;
    loop( Max_Registers )
    {
        if( strlen( reg[i] ))
            Register_Buffer_Arrays[i] = reg[i];
        ++i;
    }
}

static define reg_getkey ()
{
   variable key;
   forever
     {
	key = get_mini_response ("Enter Register: ") - 32;
	if ((key >= 0) and (key < 95))
	  return key;
	
	flush_input ();
	flush ("* Invalid Key *");
	usleep (250);
     }
}

define reg_copy_to_register ()
{
   !if (markp ())
      error ("No region defined.");
   Register_Buffer_Arrays [reg_getkey ()] = bufsubstr ();
}

define reg_insert_register ()
{
   variable i = reg_getkey ();
   variable str = Register_Buffer_Arrays [i];

   i += 32;
   if (str == "")
     {
	vmessage ("Register '%c' is empty.", i);
	return;
     }

   insert (str);
   vmessage ("Register '%c' inserted.", i);
}

static variable Reg_Mark;
static variable Reg_Line = 1;
static variable Reg_Nwindows;
static variable Reg_Cbuf;

static define reg_prev ()
{
   $0 = re_bsearch ("^ [^ ] ");
   !if ($0)
      $0 = re_bsearch ("^SPC");
   return $0;
}

static define reg_next ()
{
   return re_fsearch ("^ [^ ] ");
}

static define reg_update_hook ()
{
   bol ();
   if (looking_at ("   "))
     {
        if (what_line () > Reg_Line)
          {
             !if (reg_next ())
                () = reg_prev ();
          }
        else
          {
             !if (reg_prev ())
                () = reg_next ();
          }
     }
   goto_column (2);
   Reg_Line = what_line ();
   Reg_Mark = create_line_mark (Colorin_Line);
}

define reg_quit ()
{
   setbuf ("*registers*");
   set_buffer_modified_flag (0);

   sw2buf (Reg_Cbuf);

   delbuf ("*registers*");
   %menu_set_object_available ("Global.&Edit.Re&gisters.&Insert", 0);
   %menu_set_object_available ("Global.&Edit.Re&gisters.Ca&ncel", 0);
   if (Reg_Nwindows == 1)
      onewindow ();
   else
      otherwindow ();
}

define reg_insert ()
{
   $0 = what_char ();
   if (andelse
         {$0 == 'P'}
         {looking_at ("PC")})
      $0 = ' ';
   $0 -= 32;
   reg_quit ();
   insert (Register_Buffer_Arrays [$0]);
   recenter (0);
}

define reg_help ()
{
   message ("?: this help, q: quit mode, RET: insert register");
}


$1 = "register";
!if (keymap_p ($1))
{
   make_keymap ($1);
   definekey ("reg_help", "?", $1);
   definekey ("reg_quit", "q", $1);
   definekey ("reg_insert", "\r", $1);
}


define register_mode ()
{
   variable mode = "register";

   variable idx = where (Register_Buffer_Arrays != "");
   !if (length (idx))
     error ("There are no registers defined.");

   Reg_Nwindows = nwindows ();
   Reg_Cbuf = pop2buf_whatbuf ("*registers*");
   pop2buf ("*registers*");
   erase_buffer ();

   foreach (idx)
     {
	variable i = ();
	variable str = Register_Buffer_Arrays[i];
	variable c = i + 32;
	variable l = what_line ();

	l++;

	if (c == 32)
	  vinsert ("SPC %s\n", str);
	else
	  vinsert (" %c %s\n", c, str);

	while (what_line () > l)
	  {
	     go_up_1 ();
	     bol ();
	     whitespace (3);
	  }
	eob ();
     }

   call ("backward_delete_char_untabify");
   bob ();
   set_buffer_modified_flag (0);
   set_buffer_hook ("update_hook", &reg_update_hook);
   set_column_colors (Colorin_Left, 1, 3);
   toggle_readonly ();

   %menu_set_object_available ("Global.&Edit.Re&gisters.&Insert", 1);
   %menu_set_object_available ("Global.&Edit.Re&gisters.Ca&ncel", 1);

   use_keymap (mode);
   set_mode (mode, 0);
   run_mode_hooks ("register_mode_hook");
   reg_help ();
}
