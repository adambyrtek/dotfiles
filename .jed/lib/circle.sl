% circle.sl: a ring of variables/circular array/closed array

%!%+
%\variable{Circ_Type}
%\synopsis{A "circular" array with relational indexing via a set of functions.}
%\description
%
% "Circular" (or "cyclic", "looped", "closed") means that the first element 
% counts as successor of the last element. This can also be seen as a 
% periodic boundary condition (as known from crystal theory), 
% i.e. a circular array of size N looks like
%
%  .. N-2 N-1 0  1  2 .. N-2 N-1  0  1  2 .. N-2 N-1 0  1  2 .. N-2 N-1  0 ..
%
% The circuar array is implemented as a structure with the fields
%     Array_Type  .values:    a "normal" array holding the values
%     Int_Type    .pointer:   the actual position for relative indexing
%     Int_Type    .first:     the position of the first valid entry
%     Int_Type    .last:      the position of the last valid entry
%     String_Type .step_mode: the mode of relative indexing
%
% Indexing is done relative to the position of the pointer.
% The pointer can be moved using the functions circ_fstep(arr), 
% circ_bstep(arr), and circ_step(arr, n), where
% n might be negative - moving the pointer backwards (left in the above
% picture).
%
% There are three step-modes (for relative indexing and moving the pointer):
%
%    circle_all:     The pointer circles the whole array:
%
%    		     If the pointer is at element N-1, the next step
%    		     sets it to 0. If the pointer is at element 0, the
%    		     next back-step sets it to N-1
%
%    circle_values:  The pointer circles only the part of the array,
%    		     that is filled with valid values.
%
%    		     Valid values are the ones added with circ_append
%    		     (and their possible replacements by circ_set).
%    		     The range of valid values is stored in the fields
%    		     .first and .last and adjusted by circ_append(var, value)
%
%    		     If the pointer is at the last valid element, the next
%    		     step sets it to the first valid element. If the pointer
%    		     is at the first valid element, the next back-step sets
%    		     it to the last valid element.
%
%
%    linear:	     The pointer moves only in the part of the array,
%    		     that is filled with valid values.
%
%    		     If the pointer is at the last valid element, an attempt
%    		     to step further will not move it but just issue a
%    		     warning: 
%    		       "attempt to step past last/first valid entry"
%    		     If the pointer is at the first valid element, an attempt
%    		     to step further back will not move it but just issue a
%    		     warning: "First element reached".
%
% In pictures:
%  Pointers:        * pointer, f first, l last
%  Mode at boundry: <--> step/skip, | stay
%
% Full circle:
%
%  .. N-2 N-1 0  1  2 .. N-2 N-1  0  1  2 .. N-2 N-1 0  1  2 .. N-2 N-1  0 ..
%                             *<->*
% Limited_circle:
%
%  .. N-2 N-1 0  1  2  3  4 .. N-2 N-1 0  1  2  3  4 .. N-2 N-1 0  1  2  3 ..
%   --------l    f--valid-range--l        f---------------l        f--------
%                                *<------>*
% Linear:
%
%  .. N-2 N-1 0  1  2  3  4 .. N-2 N-1 0  1  2  3  4 .. N-2 N-1 0  1  2  3 ..
%   --------l    f--valid-range--l        f---------------l        f--------
%               |                *|
%
% The difference between a circular array in linear mode and the standard
% linear array is, that the "filling" is still circular, i.e. circ_append
% will not stop at N-1, but continue with 0, sacrifying the first
% value. This is similar to a stack with a limited number of levels, where
% the first item will fall of the stack if there are more pushes than levels.
% Therefore, a circular array in linear mode is e.g. best suited for a
% history stack with a limited number of stored positions/commands
% 
% The dereference operator @ may be used to create an instance of the
% circular array:
%    variable a = @Circ_Type;
% but typically one wants to initialize it as well, e.g.:   
%    variable a = create_circ(Integer_Type, 30, "circle_all");
%
% \seealso{cicr_append, circ_get, circ_set}
%!%-
%#ifnexists Circ_Type  % this gives problems with precompilation
typedef struct
{
   values,
   pointer,
   first,
   last,
   step_mode
} Circ_Type;
%#endif


%!%+
%\function{create_circ}
%\synopsis{Return an instance of a circular array}
%\usage{Circ_Type create_circ(data_type, N, step_mode)}
%\description
%   Create an circular array of size N with elements of Data_Type_Type 
%   data_type.
%   For an explanation of step_mode see Circ_Type.
%   
%\seealso{Circ_Type, create_circ, cicr_append, circ_set, circ_get, circ_fstep, circ_next}
%!%-
define create_circ(type, N, step_mode)
{
   variable circ = @Circ_Type;
   circ.values = type[N];
   circ.pointer = 0;
   % circ.first/last  % remain NULL until circ_append sets them
   circ.step_mode = step_mode;
   return(circ);
}

% --- "circular arithmetic" ---


%!%+
%\function{circ_add}
%\synopsis{Add m and n, "fold" the result back to [0,...,N-1]}
%\usage{Number circ_add(Number m, Number n, Integer N)}
%\description
%   circ_add does an "circular addition" of two numbers (Integer or Float). 
%   Use a negative number for b if you want a subtraction.
%\seealso{Circ_Type,  , }
%!%-
public define circ_add(m, n, N)
{
   variable sum;
   sum = (m + n) mod N;
   if (sum < 0)
     sum += N;
   return(sum);
}

% move pointer one step forward, using full circle
define circ_increment(var)
{
   var.pointer++;
   if (var.pointer == length(var.values))
     var.pointer = 0;
}

% move pointer one step backward, using full circle
define circ_decrement(var)
{
   var.pointer--;
   if (var.pointer < 0)
     var.pointer = length(var.values)-1;
}

% move the pointer one step forward, considering step_mode
define circ_fstep(var)
{
   % cannot step in valid range, if there is no valid range
   if (var.first == NULL and var.step_mode != "circle_all")
     error("no valid entries  [circular array]");

   if (var.step_mode == "circle_values")
     if (var.pointer == var.last)
       {
	  var.pointer = var.first;   % skip non-valid elements
	  return();
       }

   if (var.step_mode == "linear")
     if (var.pointer == var.last)
       error("attempt to step past last valid entry  [circular array]");
   circ_increment(var);
}

% move the pointer one step backward, considering step_mode
define circ_bstep(var)
{
   % we cannot step in valid range, if there is no valid range
   if (var.first == NULL and var.step_mode != "circle_all")
     error("no valid entries  [circular array]");

   if (var.step_mode == "circle_values")
     if (var.pointer == var.first)
       {
	  var.pointer = var.last;   % skip non-valid elements
	  return();
       }

   if (var.step_mode == "linear")
     if (var.pointer == var.first)
       error("attempt to bstep before first valid entry  [circular array]");
   circ_decrement(var);
}

% move the pointer n steps, considering step_mode
define circ_step(var, n)
{
   if (n > 0)
     loop (n)
       circ_fstep(var);
   if (n < 0)
     loop (-n)
       circ_bstep(var);
}

%!%+
%\function{circ_get}
%\synopsis{Return the value [pointer+offset] of the circular array var}
%\usage{Any_Type circ_get(Circ var, Int offset=0)}
%\description
%   Get the value at the actual position (var.pointer) or (if the 
%   optional argument is given) at pointer "+" offset 
%   ("+" means "circular addition" according to var.step_mode).
%   The return value is of course of the type defined by the creation of 
%   var.
%\seealso{Circ_Type, circ_set, circ_next, circ_previous}
%!%-
define circ_get() % (var, offset=0)
{
   % get arguments: last is top on stack
   variable offset = 0;
   if (_NARGS == 2)
     offset = ();
   variable var = ();
   
   variable value, p = var.pointer;    % save current position
   circ_step(var, offset);
   if (_typeof(var.values) == Any_Type)
     value = @var.values[var.pointer];
   else
     value = var.values[var.pointer];
   var.pointer = p;		% restore position
   return(value);
}


%!%+
%\function{circ_set}
%\synopsis{Set the value [pointer+offset] of the circular array var}
%\usage{Void circ_set(Circ var, Any_Type value, Int offset=0)}
%\description
%   Set the value at the actual position (var.pointer) or (if the 
%   optional argument is given) at pointer "+" offset 
%   ("+" means "circular addition" according to var.step_mode).
%\note
% circ_set does not use/affect the first-last pair, to change the range of
% "valid entries" as well, use the functions circ_append or circ_insert
%   
%\seealso{Circ_Type, circ_get, circ_append}
%!%-
define circ_set()  % circ_set(var, value, [offset = 0])
{
   % get arguments: last is top on stack
   variable offset = 0;
   if (_NARGS == 3)
     offset = ();
   variable value = ();
   variable var = ();
   
   variable p = var.pointer;    % save current position
   circ_step(var, offset);
   var.values[var.pointer] = value;
   var.pointer = p;		% restore position
}


% just for conveniency, some often used combinations:

%!%+
%\function{circ_step_and_get}
%\synopsis{Move the actual position n steps and return the value}
%\usage{Any_Type circ_step_and_get(Circ var, Int n)}
%\description
%   Move the pointer to the actual position n steps and get the value
%\seealso{Circ_Type, circ_step, circ_get, circ_next, circ_previous}
%!%-
define circ_step_and_get(var, n)
{
   circ_step(var, n);
   return(circ_get(var));
}

%!%+
%\function{circ_next}
%\synopsis{Move the actual position 1 step and return the value}
%\usage{Any_Type circ_next(Circ var)}
%\description
%   Move the pointer to the actual position one step and return 
%   the value of var[pointer]
%\seealso{Circ_Type, circ_step_and_get, circ_step, circ_get, circ_previous}
%!%-
define circ_next(var)
{
   circ_fstep(var);
   return(circ_get(var));
}

%!%+
%\function{circ_previous}
%\synopsis{Move the actual position 1 step back and return the value}
%\usage{Any_Type circ_previous(Circ var)}
%\description
%   Move the pointer to the actual position one step backwards and 
%   return the value of var[pointer]
%\seealso{Circ_Type, circ_step, circ_get, circ_previous}
%!%-
define circ_previous(var)
{
   circ_bstep(var);
   return(circ_get(var));
}


%!%+
%\function{circ_append}
%\synopsis{Append the value to the actual position}
%\usage{ public Void circ_append() % circ_append(var, value, [at_end = 0])}
%\description
% Set the value at the actual positions successor (circling round the full
% circle) and adjust the "range of valid entries" to end at this value.
% 
% If the array is already full, the first entry will be overwritten.
% 
% The optional argument "at_end" tells circ_append to append the 
% value not to the current position but at the end of the array.
% 
%\seealso{Circ_Type, circ_set, circ_get}
%!%-
public define circ_append() % circ_append(var, value, [at_end = 0])
{
   % get arguments: last is top on stack
   variable at_end = 0;
   if (_NARGS == 3)
     at_end = ();
   variable value = ();
   variable var = ();
   
   % if at_end is TRUE and list not empty: goto end of list 
   if (at_end and var.last != NULL)
     var.pointer = var.last;
   % append the value
   circ_increment(var); % move pointer one step, using full circle
   circ_set(var, value);
   % set .last
   var.last = var.pointer;
   % two cases where we need to set .first
   if(var.first == NULL)                     % array empty
     var.first = var.pointer;	             
   else if(var.first == var.pointer)         % array full 
     {		     		             
	var.first++;		             % -> increment .first
	if (var.first == length(var.values))
	  var.first = 0;
     }
}

% TODO circ_insert(var, value) insert value at position, "moving the others up"


%!%+
%\function{circ_delete}
%\synopsis{remove current entry and let rest "close the gap"}
%\usage{Void circ_delete(Circ var, offset=0)}
%\description
%   Remove the current entry from the range of valid entries.
%   
%   If step-mode == circle_all or no valid entries: do nothing
%   If pointer is at last position:
%   * the value is not deleted, but var.last is decrememted to mark it as
%     outside of the valid range. var.pointer is decremented as well
%\seealso{Circ_Type, circ_append, circ_set}
%!%-
public define circ_delete() % (var, offset=0)
{
   % get arguments
   variable offset = 0;
   if (_NARGS == 2)
     offset = ();
   variable var = ();
   
   % no valid entries
   if (var.first == NULL)  
     return;	     	
   % just one valid entry
   if (var.first == var.last) 
     {
	var.first = NULL; % \_ means: "NO VALID ENTRIES"
	var.last = NULL;  % /
	return;
     }
   % remember position
   variable p = var.pointer;  
   % consider offset (might abort due to errro with "linear" mode)
   circ_step(var, offset);
   % "delete": entries above the deleted slide one position down
   while (var.pointer != var.last)
     {
	circ_set(var, circ_get(var,1));
	circ_fstep(var);
     }
   % restore position
   var.pointer = p;
   % make sure pointer stays on a valid value
   if (var.pointer == var.last)               % at last valid entry
	circ_bstep(var); 
   % decrement .last 
   var.last = circ_add(var.last, -1,  length(var.values));
}


%!%+
%\function{circ_length}
%\synopsis{return the number of valid elements of a circular array}
%\usage{Int circ_length(Circ var)}
%\notes
% Returns the length of var.values, if var.step_mode is circle_all)
%\seealso{Circ_Type, create_circ, circ_append, circ_get_values }
%!%-
define circ_length(var)
{
   if (var.step_mode == "circle_all")
     return(length(var.values));
   if (var.first == NULL) % no valid entries
     return(0);
   variable ve_length = var.last - var.first + 1;
   if (ve_length < 1) ve_length += length(var.values);
   return(ve_length);
}


%!%+
%\function{circ_get_values}
%\synopsis{Return an ordinary array with the valid values of var}
%\usage{Array circ_get_values (Circ var)}
%\description
%   Return valid values (the ones added with circ_append)
%   (return all values if var.mode == circle_all).
%   if ("circle_all" or "circle_values"), element 0 is the current element
%   if ("linear"), element 0 is var.first and element N is var.last
%\notes
% the name is adopted to assoc_get_values(ass)
%\seealso{Circ_Type, circ_append, circ_get}
%!%-
define circ_get_values() % (var, direction = 1) % 1=forward, -1=backward
{
   % get arguments: last is top on stack
   variable direction = 1;
   if (_NARGS == 2)
     direction = ();
   variable var = ();

   variable i, arr = _typeof(var.values)[circ_length(var)];
   % No valid entries
   if (var.first == NULL and var.step_mode != "circle_all")
     return(arr);  % array of length 0
   % if "linear": return [first, ..., last]
   if (var.step_mode == "linear")
     {
	variable pointer = var.pointer; % save current position
	if (direction == 1)
	  var.pointer = var.first;
	else
	  var.pointer = var.last;
     }
   % fill array
   for (i=0; i<length(arr); i++)
     {
	  arr[i] = circ_get(var, direction * i);
     }
   if (var.step_mode == "linear")
     var.pointer = pointer;
   return(arr);
}

%!%+
%\function{circ_bget_values}
%\synopsis{return an array with the valid values of var in reverse order}
%\usage{Array circ_bget_values (Circ var)}
%\description
% return an ordinary array with the valid values 
% in reverse order
%\seealso{circ_get_values}
%!%-
define circ_bget_values (var)
{
   circ_get_values(var, -1);
}

% --------------- Testing ---------------------------------------------

% requires the non-standard module debug.sl
autoload("sprint_array", "debug");

% write info about circular array to debug buffer
define sprint_circ(var)
{
   variable str = sprint_array(circ_get_values(var));
   str += ", pointer == " + string(var.pointer);
   str += ", first == " + string(var.first);
   str += ", last == " + string(var.last);
   return(str);
}

provide("circle");
