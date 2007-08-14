%_debug_info = 1;
%_traceback = 1;

variable ProjectFile =
#ifdef UNIX
        ".project.sl"
#else
        "wproject.sl"
#endif
        ;

variable ProjectDefault = "dfltprj.sl";

% This is better to have a name different from ProjectFile,
% so it isn't matched by the search algorithm.
custom_variable("Project_Global_File", NULL);

variable Project_File_Dir = NULL;

static define project_try_file(dir, file)
{
        variable File = path_concat(dir, file);

        if (file_status(File) == 1)
                return File;

        return NULL;
}

static define project_search_file()
{
        variable dir = getcwd();
        variable olddir = "";
        variable file;

        % First, try current dir and parents.
        do {
                file = project_try_file(dir, ProjectFile);
                if (file != NULL)
                        break;

                olddir = dir;
                dir = path_dirname(dir);
        } while (strcmp(dir, olddir) != 0);

        if (file == NULL) {
                % Not found, try default file in jed_library_path.
                foreach (strchop(get_jed_library_path(), ',', 0)) {
                        dir = ();
                        file = project_try_file(dir, ProjectDefault);
                        if (file != NULL)
                                break;
		}
        }

	if (file == NULL) {
                % Try Global File
                if (Project_Global_File != NULL) {
			dir = path_dirname(Project_Global_File);
                        if (file_status(Project_Global_File) == 1)
                                file = Project_Global_File;
                }
	}

        if (file != NULL) {
		Project_File_Dir = dir;
                () = evalfile(file);
	}
}

project_search_file();
