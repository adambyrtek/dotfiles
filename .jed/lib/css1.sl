%{{{ Documentation
%
% Description
% 
%   CSS1 (Cascading Style Sheet - level 1) mode for JED containing a "Mode"
%   menu with a lot of items to help you write css files. It also defines
%   two syntax highlighting schemes; one based on the attributes and values
%   of CSS1 and one pattern based (DFA).
%   
%   N.B. It is not wise to use the DFA scheme, since it doesn't correctly
%   highlight one-line declarations and there's no visual feedback to whether
%   a typed attribute or value is correct spelled or not.
%   
% Usage
% 
%   Put this file in your JED_LIBRARY path and add the following lines to your
%   startup file (.jedrc or jed.rc):
%
%     autoload ("css1_mode", "css1");
%     add_mode_for_extension ("css1", "css");
%     add_mode_for_extension ("css1", "css1");
%
%   Every time you open a file called 'style.css', css1_mode will
%   automatically be loaded.
%
% Changelog
% 
%   1.6 - 2001/12/19 (JG):
%     - Moved and completed the syntax definitions and added attributes and 
%       values as keyword and keyword1 to be used without DFA highlighting.
%     - Added string and delimiter classes to the DFA rules.
%     - Updated the DFA functions to use the newer dfa_* names. 
%     - Corrected two typos in the attribute names in the "Mode" menu.
%     - Updated the documentation in this file.
%
%   1.5 - 2001/01/07 (FR): 
%     - First public release.
%
% Authors
% 
%   Johann Gerell <johann dot gerell at home dot se>
%   Francesc Rocher <f.rocher@computer.org>
%   
%}}}

$0 = "css1";

%{{{ Syntax definition
create_syntax_table($0);
define_syntax("/*", "*/", '%', $0);         % comment
define_syntax("//", "", '%', $0);           % comment
define_syntax(";:{},.", ',', $0);           % delimiters
define_syntax("{", "}", '(', $0);           % matched braces
define_syntax("#\\-0-9a-zA-Z_", 'w', $0);   % words
define_syntax('"', '"', $0);                % string
set_syntax_flags($0, 0x80);                 % strings don't span multiple lines
%}}}
%{{{ DFA highlighting
#ifdef HAS_DFA_SYNTAX
dfa_enable_highlight_cache("css1.dfa", $0);
dfa_define_highlight_rule("^.*//.*$", "Qcomment", $0);
dfa_define_highlight_rule("^.*/\\*.*\\*/$", "Qcomment", $0);
dfa_define_highlight_rule ("^[^;{}/\\*]*$", "keyword", $0);
dfa_define_highlight_rule ("^[^:]*:", "keyword1", $0);
dfa_define_highlight_rule("{|}|;", "delimiter", $0);
dfa_define_highlight_rule("\".*\"", "string", $0);
dfa_build_highlight_table($0);
#endif
%}}}
%{{{ Keywords: attributes
() = define_keywords_n($0, "clearcolorfloatwidth", 5, 0);
() = define_keywords_n($0, "bordermargin", 6, 0);
() = define_keywords_n($0, "displaypadding", 7, 0);
() = define_keywords_n($0, "font-size", 9, 0);
() = define_keywords_n($0, "font-stylemargin-toptext-align", 10, 0);
() = define_keywords_n($0, "font-familyfont-weightline-heightmargin-leftpadding-toptext-indentwhite-space", 11, 0);
() = define_keywords_n($0, "border-colorborder-styleborder-widthmargin-rightpadding-left", 12, 0);
() = define_keywords_n($0, "margin-bottompadding-right", 13, 0);
() = define_keywords_n($0, "padding-bottomtext-transform", 14, 0);
() = define_keywords_n($0, "list-style-typetext-decoration", 15, 0);
() = define_keywords_n($0, "background-colorbackground-imageborder-top-width", 16, 0);
() = define_keywords_n($0, "border-left-width", 17, 0);
() = define_keywords_n($0, "border-right-width", 18, 0);
() = define_keywords_n($0, "border-bottom-width", 19, 0);
%}}}
%{{{ Keywords: values
() = define_keywords_n($0, "pre", 3, 1);
() = define_keywords_n($0, "boldbothdiscleftnone", 4, 1);
() = define_keywords_n($0, "blinkblockinsetlargeserifsmallright", 5, 1);
() = define_keywords_n($0, "boldercentercircledoublegrooveinlineitaliclargermediumnormaloutsetsquare", 6, 1);
() = define_keywords_n($0, "cursivedecimalfantasyjustifylightersmallerx-largex-small", 7, 1);
() = define_keywords_n($0, "xx-largexx-small", 8, 1);
() = define_keywords_n($0, "list-itemlowercasemonospaceunderlineuppercase", 9, 1);
() = define_keywords_n($0, "capitalizesans-serif", 10, 1);
() = define_keywords_n($0, "lower-alphalower-romanupper-alphaupper-roman", 11, 1);
() = define_keywords_n($0, "line-through", 12, 1);
%}}}

private define css1_menu(menu) { %{{{
  menu_append_popup (menu, "&Font Properties");
  $0 = menu + ".&Font Properties";
  {
    menu_append_item ($0, "&Size", "insert (\"font-size: \")");
    menu_append_popup ($0, "&Absolute Size");
    $1 = $0 + ".&Absolute Size";
    {
      menu_append_item ($1, "xx-sm&all", "insert (\"font-size: xx-small;\")");
      menu_append_item ($1, "x-s&mall", "insert (\"font-size: x-small;\")");
      menu_append_item ($1, "&small", "insert (\"font-size: small;\")");
      menu_append_item ($1, "me&dium", "insert (\"font-size: medium;\")");
      menu_append_item ($1, "la&rge", "insert (\"font-size: large;\")");
      menu_append_item ($1, "x-lar&ge", "insert (\"font-size: x-large;\")");
      menu_append_item ($1, "xx-larg&e", "insert (\"font-size: xx-large;\")");
    }
    menu_append_popup ($0, "&Relative Size");
    $1 = $0 + ".&Relative Size";
    {
      menu_append_item ($1, "&smaller", "insert (\"font-size: smaller;\")");
      menu_append_item ($1, "&larger", "insert (\"font-size: larger;\")");
    }
    menu_append_separator ($0);
    menu_append_item ($0, "&Family", "insert (\"font-family: \")");
    menu_append_popup ($0, "Fon&tFamily");
    $1 = $0 + ".Fon&tFamily";
    {
      menu_append_item ($1, "&cursive", "insert (\"font-family: cursive;\")");
      menu_append_item ($1, "&fantasy", "insert (\"font-family: fantasy;\")");
      menu_append_item ($1, "&serif", "insert (\"font-family: serif;\")");
      menu_append_item ($1, "s&ans-serif", "insert (\"font-family: sans-serif;\")");
      menu_append_item ($1, "&monospace", "insert (\"font-family: monospace;\")");
    }
    menu_append_separator ($0);
    menu_append_popup ($0, "St&yle");
    $1 = $0 + ".St&yle";
    {
      menu_append_item ($1, "&normal", "insert (\"font-style: normal;\")");
      menu_append_item ($1, "&italic", "insert (\"font-style: italic;\")");
    }
    menu_append_popup ($0, "&Weight");
    $1 = $0 + ".&Weight";
    {
      menu_append_item ($1, "&normal", "insert (\"font-weight: normal;\")");
      menu_append_item ($1, "&bold", "insert (\"font-weight: bold;\")");
      menu_append_item ($1, "b&older", "insert (\"font-weight: bolder;\")");
      menu_append_item ($1, "&lighter", "insert (\"font-weight: lighter;\")");
      menu_append_item ($1, "&other", "insert (\"font-weight: \")");
    }
  }
  menu_append_popup (menu, "&Text Properties");
  $0 = menu + ".&Text Properties";
  {
    menu_append_item ($0, "&Line Height", "insert (\"line-height: \")");
    menu_append_popup ($0, "&Alignment");
    $1 = $0 + ".&Alignment";
    {
      menu_append_item ($1, "&left", "insert (\"text-align: left;\")");
      menu_append_item ($1, "&right", "insert (\"text-align: right;\")");
      menu_append_item ($1, "&center", "insert (\"text-align: center;\")");
      menu_append_item ($1, "&justify", "insert (\"text-align: justify;\")");
    }
    menu_append_popup ($0, "&Decoration");
    $1 = $0 + ".&Decoration";
    {
      menu_append_item ($1, "&none", "insert (\"text-decoration: none;\")");
      menu_append_item ($1, "&blink", "insert (\"text-decoration: blink;\")");
      menu_append_item ($1, "&line-through", "insert (\"text-decoration: line-through;\")");
      menu_append_item ($1, "&underline", "insert (\"text-decoration: underline;\")");
    }
    menu_append_item ($0, "&Indent", "insert (\"text-indent: \")");
    menu_append_popup ($0, "&Transform");
    $1 = $0 + ".&Transform";
    {
      menu_append_item ($1, "&none", "insert (\"text-transform: none;\")");
      menu_append_item ($1, "&capitalize", "insert (\"text-transform: capitalize;\")");
      menu_append_item ($1, "&lowercase", "insert (\"text-transform: lowercase;\")");
      menu_append_item ($1, "&uppercase", "insert (\"text-transform: uppercase;\")");
    }
  }
  menu_append_popup (menu, "&Block Formatting");
  $0 = menu + ".&Block Formatting";
  {
    menu_append_popup ($0, "&Margins");
    $1 = $0 + ".&Margins";
    {
      menu_append_item ($1, "&left", "insert (\"margin-left: \")");
      menu_append_item ($1, "&right", "insert (\"margin-right: \")");
      menu_append_item ($1, "&top", "insert (\"margin-top: \")");
      menu_append_item ($1, "&bottom", "insert (\"margin-bottom: \")");
      menu_append_item ($1, "&all", "insert (\"margin: \")");
    }
    menu_append_popup ($0, "&Padding");
    $1 = $0 + ".&Padding";
    {
      menu_append_item ($1, "&left", "insert (\"padding-left: \")");
      menu_append_item ($1, "&right", "insert (\"padding-right: \")");
      menu_append_item ($1, "&top", "insert (\"padding-top: \")");
      menu_append_item ($1, "&bottom", "insert (\"padding-bottom: \")");
      menu_append_item ($1, "&all", "insert (\"padding: \")");
    }
    menu_append_separator ($0);
    menu_append_popup ($0, "Border &Style");
    $1 = $0 + ".Border &Style";
    {
      menu_append_item ($1, "&none", "insert (\"border-style: none;\")");
      menu_append_item ($1, "&solid", "insert (\"border-style: solid;\")");
      menu_append_item ($1, "&double", "insert (\"border-style: double;\")");
      menu_append_item ($1, "&inset", "insert (\"border-style: inset;\")");
      menu_append_item ($1, "&outset", "insert (\"border-style: outset;\")");
      menu_append_item ($1, "&groove", "insert (\"border-style: groove;\")");
    }
    menu_append_popup ($0, "Border &Width");
    $1 = $0 + ".Border &Width";
    {
      menu_append_item ($1, "&left", "insert (\"border-left-width: \")");
      menu_append_item ($1, "&right", "insert (\"border-right-width: \")");
      menu_append_item ($1, "&top", "insert (\"border-top-width: \")");
      menu_append_item ($1, "&bottom", "insert (\"border-bottom-width: \")");
      menu_append_item ($1, "&all", "insert (\"border-width: \")");
    }
    menu_append_item ($0, "Border &Color", "insert (\"border-color: \")");
    menu_append_separator ($0);
    menu_append_popup ($0, "C&lear");
    $1 = $0 + ".C&lear";
    {
      menu_append_item ($1, "&none", "insert (\"clear: none;\")");
      menu_append_item ($1, "&left", "insert (\"clear: left;\")");
      menu_append_item ($1, "&right", "insert (\"clear: right;\")");
      menu_append_item ($1, "&both", "insert (\"clear: both;\")");
    }
    menu_append_popup ($0, "&Float");
    $1 = $0 + ".&Float";
    {
      menu_append_item ($1, "&none", "insert (\"float: none;\")");
      menu_append_item ($1, "&left", "insert (\"float: left;\")");
      menu_append_item ($1, "&right", "insert (\"float: right;\")");
    }
    menu_append_item ($0, "W&idth", "insert (\"width: \")");
  }
  menu_append_popup (menu, "&Color Properties");
  $0 = menu + ".&Color Properties";
  {
    menu_append_item ($0, "&color", "insert (\"color: \")");
    menu_append_item ($0, "&background-color", "insert (\"background-color: \")");
    menu_append_item ($0, "background-&image", "insert (\"background-image: \")");
  }
  menu_append_popup (menu, "C&lassification Properties");
  $0 = menu + ".C&lassification Properties";
  {
    menu_append_popup ($0, "&Display");
    $1 = $0 + ".&Display";
    {
      menu_append_item ($1, "&none", "insert (\"display: none;\")");
      menu_append_item ($1, "&block", "insert (\"display: block;\")");
      menu_append_item ($1, "&inline", "insert (\"display: inline;\")");
      menu_append_item ($1, "&list-item", "insert (\"display: list-item;\")");
    }
    menu_append_popup ($0, "&List Style Type");
    $1 = $0 + ".&List Style Type";
    {
      menu_append_item ($1, "&none", "insert (\"list-style-type: none;\")");
      menu_append_item ($1, "&disc", "insert (\"list-style-type: disc;\")");
      menu_append_item ($1, "&circle", "insert (\"list-style-type: circle;\")");
      menu_append_item ($1, "&square", "insert (\"list-style-type: square;\")");
      menu_append_item ($1, "&decimal", "insert (\"list-style-type: decimal;\")");
      menu_append_item ($1, "lower-&roman", "insert (\"list-style-type: lower-roman;\")");
      menu_append_item ($1, "upper-r&oman", "insert (\"list-style-type: upper-roman;\")");
      menu_append_item ($1, "lower-&alpha", "insert (\"list-style-type: lower-alpha;\")");
      menu_append_item ($1, "upper-a&lpha", "insert (\"list-style-type: upper-alpha;\")");
    }
    menu_append_popup ($0, "&White Space");
    $1 = $0 + ".&White Space";
    {
      menu_append_item ($1, "&normal", "insert (\"white-space: normal;\")");
      menu_append_item ($1, "&pre", "insert (\"white-space: pre;\")");
    }
  }
}
%}}}

public define css1_mode() {
  $0 = "css1";
  set_mode($0, 0);
  use_syntax_table($0);
  mode_set_mode_info($0, "init_mode_menu", &css1_menu);
}
