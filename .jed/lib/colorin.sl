%
% colorin.sl
% Color Interface: color values used by interfaces of some modes (e.g, minued)
% 
% $Id: colorin.sl,v 1.3 2001/07/16 00:25:52 rocher Exp $
% Francesc Rocher <f.rocher@computer.org>
%

custom_variable( "Colorin_Left", color_number( "menu_selection_char" ));
custom_variable( "Colorin_Line", color_number( "menu_selection" ));

%!%+
%\function{set_colorin}
%\synopsis{Select colors for some particular interfaces.}
%\usage{Void set_colorin (String_Type left, String_Type line);}
%\description
% With \var{set_colorin} you can customize, in a centralized way, what colors
% will be used by some modes. These modes are those that use a window to
% present you a list of options, and let you choose (or even edit) one of
% them by moving up and down across the list. To select one option, you
% usually only have to press return. For instance, \var{minued} mode and
% \var{Edit | Registers | View Registers} menu option use an interface of this
% sort.
%
% Parameters required by this function are strings representing valid color
% object names. See \var{set_color} to get the list of valid names. The first
% parameter specifies the color used to highlight the first columns (usually,
% the first 4/6 ones). The second one specifies the color used to highlight
% the selected line, or option.
%
% By default the color used on the left is \var{menu_selection}, and the color
% used for lines is \var{menu_selection_char}. To select another color set
% put, for example:
%#v+
%   autoload( "set_colorin", "colorin" );
%   set_colorin( "menu","string" );
%#v-
% in your jed initialization file.
% 
%\seealso{set_color}
%!%-
public define set_colorin( left, line )
{
    Colorin_Left = color_number( left );
    Colorin_Line = color_number( line );
}
