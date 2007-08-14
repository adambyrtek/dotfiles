% home-lib.sl: 
% Support for a user-specific ~/.jed/ directory with private extensions.
% Prepend a user-specific "home-library" to the library path (On Unix also 
% a site-wide lib) and set also the Colors and Documentation paths.
% Together with make-ini.sl, this provides a nice way of extending your 
% jed with contributed or home-made scripts.
% 
% I put this in my defaults.sl, so it is read automatically at startup. 
% Another option would be to have a
% require("home-lib", "FULL_PATH_TO_HOME-LIB/home-lib.sl") in your .jedrc.
% (if you put home-lib.sl in the standard path, require("home-lib") suffices.)
% 
% TODO: * find a nice default for windows systems
% 	* think about "multiuser widows systems" (is there such a thing?)
% 	* find nice names for the private documentation files
% 	* jed or environment variables for the documentation files
% 	
% 	* make this a part of site.sl :-)

% --- Site Library --------------------------------------------------------

#ifdef UNIX   
%!%+
%\variable{Jed_Site_Library}
%\synopsis{The site's jed library (on UNIX)}
%\description
%  The value of this variable specifies the jed-site library
%  for site-wide jed-slang scripts.
%  This is set only under Unix, by default to the the directory 
%  JED_ROOT/site-lib (if this directory exists) unless the user has 
%  specified an alternate directory via the \var{JED_SITE_LIB} 
%  environment variable
%!%
variable Jed_Site_Library = (getenv("JED_SITE_LIB"));
if (Jed_Site_Library == NULL)
  Jed_Site_Library = path_concat(JED_ROOT, "/site-lib");
   
!if (2 == file_status(Jed_Site_Library)) % directory doesnot exist
  Jed_Site_Library = "";                %   give up

% declare the site-lib and the documentation file assuming 
% Jed_Site_Library has a structure similar to the standard jed library:
if (Jed_Site_Library != "")
{
   set_jed_library_path(Jed_Site_Library + "," 
			 + get_jed_library_path());
   $1 = path_concat(Jed_Site_Library, "colors");
   if(2 == file_status($1))
     Color_Scheme_Path = Color_Scheme_Path + "," + $1;
   
   % % documentation on library functions is under doc/txt/libfuns.txt
   % $1 = path_concat(Jed_Site_Library, "doc");
   % $1 = path_concat($1, "txt");
   % $1 = path_concat($1, "libfuns.txt");
   % if(1 == file_status($1))
   %   {
   % 	public variable Jed_Site_Doc_File = $1;
   % 	Jed_Doc_Files = Jed_Doc_Files + "," + Jed_Site_Doc_File;
   %   }
   
   % Declare the public functions to jed.
   % Assume a file ini.sl containing the initialization code
   % (a list of autoload declarations).
   if( 1 == file_status(path_concat(Jed_Site_Library, "ini.sl")) )
     () = evalfile(path_concat(Jed_Site_Library, "ini.sl"));
}
#endif  % UNIX

% --- jed home directory -----------------------------------------------
% 
% set the jed home directory to ~/.jed if this directory exits. 
% Append the lib subdirectory to the Library Path and lib/colors to the 
% Color_Scheme_Path + doc/txt/libfuns.txt to the Jed_Doc_Files

%!%+
%\variable{Jed_Home_Directory}
%\synopsis{User's jed home directory}
%\description
%  The value of this variable specifies the user's jed-home directory
%  where personal jed-related files are assumed to be found.  Under Unix, 
%  by default, this corresponds to the the directory ~/.jed/ (if this 
%  directory exists), otherwise it is set to the user's home 
%  directory unless the user has specified an alternate directory via the 
%  \var{JED_HOME} environment variable
%!%
variable Jed_Home_Directory;
#ifdef UNIX   
if (getenv("JED_HOME") == NULL)
{
   static variable HOME =  getenv("HOME");
   if (HOME == NULL) % HOME should be defined on Unix, but just be sure
     HOME = "";
   Jed_Home_Directory = path_concat(HOME, ".jed");
   
   !if(2 == file_status(Jed_Home_Directory)) % directory doesnot exist
     Jed_Home_Directory = HOME;              %   set to home-dir
   !if(2 == file_status(Jed_Home_Directory)) % directory doesnot exist
     Jed_Home_Directory = "";                %   give up
}
#endif  % UNIX

% declare the lib subdirectory and the documentation file assuming the 
% Jed_Home_Directory has a structure similar to JED_ROOT:
if (Jed_Home_Directory != "")
{
   $1 = path_concat(Jed_Home_Directory, "lib");
   if(2 == file_status($1))
     {
	public variable Jed_Home_Lib_Dir = $1;
	set_jed_library_path(Jed_Home_Lib_Dir + "," 
			      + get_jed_library_path());
	$1 = path_concat(Jed_Home_Lib_Dir, "colors");
	if(2 == file_status($1))
	  Color_Scheme_Path = Color_Scheme_Path + "," + $1;
     }
   
   % documentation on library functions is under doc/txt/libfuns.txt
   $1 = path_concat(Jed_Home_Directory, "doc");
   $1 = path_concat($1, "txt");
   $1 = path_concat($1, "libfuns.txt");
   if(1 == file_status($1))
     {
	public variable Jed_Home_Doc_File = $1;
	Jed_Doc_Files = Jed_Doc_Files + "," + Jed_Home_Doc_File;
     }
   % declare the public functions to jed
   % assume a file ini.sl containing the initialization code
   % (a list of autoload declarations)
   if( 1 == file_status(path_concat(Jed_Home_Directory, "ini.sl")) )
     () = evalfile(path_concat(Jed_Home_Directory, "ini.sl"));
}
