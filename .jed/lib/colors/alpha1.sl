% based on default1.sl
$1 = "default"; $2 = "default";

set_color("normal", $1, $2);
set_color("status", "yellow", "blue");
set_color("operator", $1, $2);      % +, -, etc..
set_color("number", "brightblue", $2);    % 10, 2.71, etc..
set_color("comment", "black", "brightcyan");% /* comment */
set_color("region", "yellow", "brightmagenta");
set_color("string", "brightblue", $2);    % "string" or 'char'
set_color("keyword", "brightred", $2);    % if, while, unsigned, ...
set_color("keyword1", "red", $2);    % if, while, unsigned, ...
set_color("keyword2", "yellow", $2);    % if, while, unsigned, ...
set_color("delimiter", $1, $2);     % {}[](),.;...
set_color("preprocess", "magenta", $2);
set_color("message", "blue", $2);
set_color("error", "brightred", $2);
set_color("dollar", "brightred", $2);
set_color("...", "red", $2);			  % folding indicator

set_color ("menu_char", "yellow", "blue");
set_color ("menu", "lightgray", "blue");
set_color ("menu_popup", "lightgray", "blue");
set_color ("menu_shadow", "blue", "black");
set_color ("menu_selection", "green", "red");
set_color ("menu_selection_char", "yellow", "red");

set_color ("cursor", "black", "red");
set_color ("cursorovr", "black", "red");
