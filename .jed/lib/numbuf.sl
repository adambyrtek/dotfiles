% number the buffers that have a file associated and bind
% Alt-# to goto_numbered_buffer

% comment these out once ready
%_traceback=1;
%_debug_info=1;

% Keybindings: Default is to bind Alt-1 to Alt-9 to goto_numbered_buffer.
% (With ALT_CHAR = 27) By default these are bound to digit_arg, make sure you
% bind another keyset to digit_arg.
% Use the following to change this. (Set to NULL if you don't want keybindings)
custom_variable("Numbuf_key_prefix", "\e");  % Escape (Alt/Meta)

% Set this to 1 to have only Alt-0 bound to open_buffer_list()
custom_variable("Numbuf_show_list_when_failing", 1);

#ifnexists array_value_exists
% Find out if the array contains "value" 
% (See cuamisc.sl for a varsion that works also for Arrays of Any_Type)
define array_value_exists (arr, value)
{
   if (_typeof (arr) != typeof(value))
     return 0;
   return length (where (arr == value));
}
#endif

% helper function:  (should be defined in a utility file)
% open buffer, preserve the number of windows currently open
define go2buf(buf)
{
   if(nwindows > 1)
     pop2buf(buf);   % open in other window
   else
     sw2buf(buf);    % open in current window
}

% now let's start:

static variable Current_Number = 0;
static variable Numbered_Buffer_List = String_Type[10];
% initialize:
for ($1=0; $1< length(Numbered_Buffer_List); $1++)
  Numbered_Buffer_List[$1] = "";

% number the buffer if not done
define number_buffer ()
{
   _pop_n (_NARGS);  % remove unwanted arguments from stack
   variable buf = whatbuf;
   % don't number hidden buffers and the ".jedrecent auxiliary buffer
   if (buf[0] == ' ' or buf == ".jedrecent")
     return;
   % find out if buffer is already numbered
   if(array_value_exists (Numbered_Buffer_List, buf))
     return;
   % find first free number
   variable i = 1, oldbuf;
   for (i=1; i < 10; i++)
     {
        oldbuf = Numbered_Buffer_List[i];
        if (orelse { oldbuf == NULL 
	} { bufferp(oldbuf) == 0 
	})
          break;
     }
   if (i < 10)
     {
        % add to list of numbered buffers and set status line
        Numbered_Buffer_List[i] = buf;
        set_status_line("["+ string(i)+ "]" + Status_Line_String, 0);
	Current_Number = i;
     }
   else
    {
        set_status_line("[*]" + Status_Line_String, 0);
       	Current_Number = 10;
    }
}

% number every buffer that is associated to a file:
% either when opening a file (no arguments, no return value)
append_to_hook("_jed_find_file_after_hooks", &number_buffer);
% or when saving a up to now "unfiled" one to a file (one argument, no return value)
append_to_hook("_jed_save_buffer_after_hooks", &number_buffer);


% Build the menu of numbered buffers
static define change_buffer_callback (popup)
{
   loop (buffer_list ())
     {
        variable b = ();
        if (b[0] == ' ')
          continue;

        menu_append_item (popup, b, &go2buf, b);
     }
}


public define numbered_buffers_menu_callback (popup)
{
   variable menu, buf, i;

   for (i = 1; i < 10; i++)
     {
        buf = Numbered_Buffer_List[i];
        if (andelse { buf != NULL 
	} { bufferp(buf) 
	})
          menu_append_item (popup, "&"+string(i)+" "+buf, "go2buf", buf);
     }
   % append the unnumbered buffers
   loop (buffer_list ())
     {
        buf = ();
        if (buf[0] == ' ' or array_value_exists (Numbered_Buffer_List, buf))
          continue;
        if (buf[0] == '*')
          menu_append_item (popup, "*&"+buf[[1:]], &go2buf, buf);
        else
          menu_append_item (popup, "&"+buf, &go2buf, buf);
     }

}


% Change the callback of the Change Buffer menu entry
static define numbered_buffers_popup_hook (menubar)
{
   menu_set_select_popup_callback ("Global.&Buffers.&Change Buffer",
                                   &numbered_buffers_menu_callback);
}
append_to_hook ("load_popup_hooks", &numbered_buffers_popup_hook);

% Open Buffers List.
define open_buffer_list()
{
   ungetkey('c');
   ungetkey('b');
   call("select_menubar");
}

define goto_numbered_buffer(n)
{
   variable buf = Numbered_Buffer_List[n];

   if (andelse {buf != NULL
   } { bufferp(buf) 
   })
     go2buf(buf);
   else if (Numbuf_show_list_when_failing)
     open_buffer_list();
   else
     message("Buffer "+string(n)+" doesn't exists.");
}

define goto_numbered_prev()
{
   variable buf;
   variable i;
   
   for (i = Current_Number-1; i > 0; i--) {
      buf = Numbered_Buffer_List[i];
      if (bufferp(buf)) {
	 go2buf(buf);
	 return;
      }
   }
   
   if (Numbuf_show_list_when_failing)
     open_buffer_list();
   else
     message("First numbered buffer.");
}

define goto_numbered_next()
{
   variable buf;
   variable i;
   
   for (i = Current_Number+1; i < 10; i++) {
      buf = Numbered_Buffer_List[i];
      if (bufferp(buf)) {
	 go2buf(buf);
	 return;
      }
   }
   
   if (Numbuf_show_list_when_failing)
     open_buffer_list();
   else
     message("Last numbered buffer.");
}

% Keybindings: Default is to bind Alt-1 to Alt-9 to goto_numbered_buffer
% See the custom variable Numbuf_key_prefix for changing this.
static define numbuf_bind_keys()
{
   variable i, digit;
   for(i=1; i<10; i++)
     {
        digit = string(i);
        setkey("goto_numbered_buffer("+ digit +")", Numbuf_key_prefix+digit);
     }
   setkey("goto_numbered_prev", Numbuf_key_prefix+"-");
   setkey("goto_numbered_next", Numbuf_key_prefix+"=");
}

if (Numbuf_key_prefix != NULL)
{
   setkey("open_buffer_list", Numbuf_key_prefix + "0");
   numbuf_bind_keys();
}

provide("numbuf");

