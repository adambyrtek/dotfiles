% --- Formatted info about variable values ---

autoload("array_product", "datutils");
autoload("array_repeat", "datutils");

% How much shall a listing indent?
% indendation per step (give as literal string of spaces!)
custom_variable("Sprint_Indent", "   ");

% newline + absolute indendation (used/set by sprint_...)
static variable Sprint_NL = "\n";

% a more verbose variant of string(var) that recurses into elements
% of compound data types
public define sprint_variable(var)
{
   variable str, type, sprint_hook;
   type = extract_element(string(typeof(var)), 0, '_');
   sprint_hook = __get_reference("sprint_"+strlow(type));
   if (sprint_hook != NULL)
     {
        % show_string("printing using sprint_"+strlow(type));
	str = @sprint_hook(var);
     }
   else
     str = string(var);
   return(str);
}

% Return a 1d array of indices for multidim-array with dimensions dims
define multidimensional_indices(dims)
{
   variable i, j, N = length(dims);          % dimensionality
   variable L = array_product(dims);
   variable index = Array_Type[L], a = Integer_Type[N,L];
   for (i=0; i<N; i++)
     {
	a[i,*] = array_repeat([0:array_product(dims[[i:]])-1]
				  /array_product(dims[[i+1:]]),
				  L/array_product(dims[[i:]])
				  );
     }
   for (j=0; j<L; j++)
     index[j] = a[*,j];
   return index;
}
  
% print to a string all elements of an array:
% for simple (atomic) values: "[1,2,3]",
% for compound values: "[0] 1,\n[1]2,\n,[2] 3"
define sprint_array(a)
{
   variable strarray, str, sep = ",";

   if (typeof(a) != Array_Type)
     return("sprint_array: " + string(a) + "is no array!");
   if (length(a) == 0)
     return("[]");

   variable dims, dimensionality, indices;
   (dims, dimensionality, ) = array_info(a);
   strarray = array_map(String_Type, &sprint_variable, a);
   % try simple oneliner
   str = "[" + strjoin(strarray, sep) + "]";
   % show(str);   
   % multidimensional, compound elements or too long
   if (orelse {dimensionality > 1} {strlen(str) > WRAP} {is_substr(str, "\n")})
     {
	indices = multidimensional_indices(dims);
	indices = array_map(String_Type, &sprint_variable, indices);
	reshape(strarray, length(strarray));
	strarray =  indices + " " + strarray;
	str = string(a) + Sprint_NL + strjoin(strarray, Sprint_NL);
	(str, ) = strreplace(str, "\n", "\n"+Sprint_Indent, strlen(str));
     }
   return (str);
}

% print to a string all keys and elements of an associative array:
define sprint_assoc(ass)
{
   if (typeof(ass) != Assoc_Type) 
     return(string(ass)+ "is no associative array");
   variable str = string(ass);
   % get and sort keys
   variable keys = assoc_get_keys(ass);
   variable I = array_sort(keys);   % returns the indices in sort order
   keys = keys[I];              % array as index returns array of values
   % get values and append key-value-pairs to str
   Sprint_NL += Sprint_Indent;
   variable key;
   foreach (keys)
     {
	key = ();
	str += sprintf("%s[%s]\t%s",
		       Sprint_NL, string(key), sprint_variable(ass[key]));
     }
   % the default value:
   ERROR_BLOCK {_clear_error; "not defined";} % in case no default is defined
   variable default = sprint_variable(ass["?_*_!"]); % (hopefully) never used key
   str += Sprint_NL + "DEFAULT\t " + default;
   Sprint_NL = Sprint_NL[[:-1-strlen(Sprint_Indent)]];
   return(str);
}

% print to a string all fields of a structure
define sprint_struct(s)
{
   variable field, value;
   variable str = string(s);
   Sprint_NL += Sprint_Indent;
   foreach (get_struct_field_names(s))
     {
	field = ();
	value = get_struct_field (s, field);
	str += Sprint_NL + "." + field + "\t" + sprint_variable(value);
     }
   Sprint_NL = Sprint_NL[[:-1-strlen(Sprint_Indent)]];
   return(str);
}

% Why is there no easy way to  access .line and .column of a Mark?
define mark_info(mark)
{
   variable b, l, c;
   variable buf = whatbuf();

   ERROR_BLOCK
     {
	_clear_error;
	pop;
	return("deleted buffer", NULL, NULL);
     }
   b = user_mark_buffer(mark);
   setbuf(b);
   push_spot();
   goto_user_mark(mark);
   l = what_line();
   c = what_column();
   pop_spot();
   return (b, l, c);
}

% print to a string nicely formatted info about a user_mark:
define sprint_mark(m)
{
   if (typeof(m) != Mark_Type) return(string(m) + "is no user mark");
   variable buf, line, column;
   (buf, line, column) = mark_info(m);
   variable str = "User_Mark";
   if(line != NULL)
     str += sprintf(" (%d,%d)", line, column);
   return (str + " in " + buf);
}

% surround with "" to make clear it is a string
define sprint_string(str)
{
   (str, ) = strreplace(str, "\n", "\\n", strlen(str));
   (str, ) = strreplace(str, "\t", "\\t", strlen(str));
   (str, ) = strreplace(str, "\e", "\\e", strlen(str));
   return ("\"" + str + "\"");
}

define sprint_char(ch)
{
   return ("'" + char(ch) + "'");
}

define sprint_any(any)
{
   return (sprint_variable(@any));
}
  
