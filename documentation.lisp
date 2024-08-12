(in-package #:org.shirakumo.deploy)

;; deploy.lisp
(docs:define-docs
  (variable *foreign-libraries-to-reload*
    "This variable keeps a list of library instances to open on boot.

The variable is set during deployment and should not be
modified unless you know what you're doing.")

  (function warmly-boot
    "Perform a warm boot.

This updates the CFFI:*FOREIGN-LIBRARY-DIRECTORIES* variable
to a sensible value which should hopefully ensure that your
designated libraries are reloaded on boot.

This will also run all :BOOT hooks with the appropriate
arguments supplied.

See CFFI:*FOREIGN-LIBRARY-DIRECTORIES*
See RUN-HOOKS")

  (function quit
    "Runs the quit hooks and terminates the application.

If an error occurs during the execution of a quit hook, it
is ignored.

See RUN-HOOKS")

  (function call-entry-prepared
    "Sets up a basic environment to run the entry point in.

In particular, it sets up an EXIT restart to allow you to
quit the application, adds an error handler to handle the
top-level errors, potentially redirects all output streams,
executes the warm boot, and finally calls the entry point.

If an error should occur during boot or during the
execution of the entry point, a debugger is only invoked
if the environment variable DEPLOY_DEBUG_BOOT is set to a
non-empty value. Otherwise an error will lead to an exit
restart invocation.

If the environment variable DEPLOY_REDIRECT_OUTPUT is set
to a non-empty value, REDIRECT-OUTPUT is called with its
value.

See WARMLY-BOOT
See REDIRECT-OUTPUT")

  (function discover-entry-point
    "Attempt to resolve the given ASDF system's entry point to a function.

The entry point may be either a function or a class
designator. If a class, returned is a function that
simply instantiates the class.

See ASDF/SYSTEM:COMPONENT-ENTRY-POINT")

  (type deploy-op
    "An ASDF operation to perform a deployment.

When this operation is performed, the following steps
are taken:

1. The :LOAD hooks are run with the appropriate
   arguments.
2. The output files are determined, which should be
   a list of two paths, the first being the executable
   to dump to, and the second being a directory where
   all the resources should be stored.
3. The list of libraries to reload on boot is computed
   by removing all libraries that are either marked as
   dont-open, or aren't yet opened from LIST-LIBRARIES.
4. The deployment directories are created.
5. The *DATA-LOCATION* path is adapted to be relative
   to the binary file.
6. The :DEPLOY hooks are run with the appropriate
   arguments.
7. The :BUILD hooks are run with the appropriate
   arguments.
8. The image is dumped to an executable format, using
   core compression if available, and using the
   appropriate application type depending on the
   presence of the operating system and the
   :DEPLOY-CONSOLE feature flag.

See LIST-LIBRARIES
See *FOREIGN-LIBRARIES-TO-RELOAD*
See *DATA-LOCATION*
See FIND-RELATIVE-PATH-TO
See RUN-HOOKS")

  (function deployed-p
    "Returns T if the current Lisp environment has been booted from a deployed executable."))

;; hooks.lisp
(docs:define-docs
  (variable *hooks*
    "This variable holds a list of all hook instances.

The list should be sorted by priority of the hooks
in descending order.

See HOOK
See RUN-HOOKS")

  (type hook
    "This class encapsulates a hook.

Hooks are functions that are run during various
points of deployment and execution of the resulting
executable.

See HOOK-NAME
See HOOK-TYPE
See HOOK-FUNCTION
See HOOK-PRIORITY
See HOOK
See REMOVE-HOOK
See DEFINE-HOOK
See RUN-HOOKS")

  (function hook-name
    "Accessor to the name of the hook.

The name should be a symbol.

See HOOK")

  (function hook-type
    "Accessor to the type of the hook.

The type can be one of :LOAD :BUILD :DEPLOY :BOOT :QUIT.

See HOOK")

  (function hook-function
    "Accessor to the function of the hook.

This function is what is executed when RUN-HOOKS
is called.

See HOOK
See RUN-HOOKS")

  (function hook-priority
    "Accessor to the priority of the hook.

The priority should be an integer.
The higher the priority, the earlier the hook is
executed.

See HOOK")

  (function hook
    "Accessor to the hook instance of the given type and name.

See *HOOKS*
See HOOK
See REMOVE-HOOK")

  (function remove-hook
    "Remove the hook of the given type and name.

See *HOOKS*
See HOOK")

  (function define-hook
    "Define a new hook function.

The args list's arguments are automatically turned into
keyword arguments for the hook function. This allows you
to only specify the arguments that you are interested in.

The following arguments are available for all hook types:
- SYSTEM  The ASDF system object the application is built
          with.
- OP      The ASDF operation object used to build the
          application.

The following hook types are recognised:
- :load    These functions should perform the loading of
           systems or other kinds of things that will
           potentially change the set of libraries.

- :deploy  These functions are responsible for copying
           files into the deployment target directory.
           It supplies the following extra arguments:
  - DIRECTORY  The target directory into which resource
               files should be placed.

- :build   These functions should prepare the system for
           the dump to binary. Specifically you might
           want to shut down existing threads, close file
           handles, remove unneeded baggage, and remove
           potentially sensitive information about your
           system.

- :boot    These functions are run right before the
           primary entry point is executed. Thus they are
           responsible for preparing the runtime to
           continue by restarting threads, re-opening
           files, and so forth.
           It supplies the following extra arguments:
  - DIRECTORY  The directory in which the resource files
               now reside after boot.

- :quit    These functions are run right before the
           executable exits completely. They offer a
           last-minute opportunity to dump some
           information about the system, or to clean up
           vital resources.

See HOOK
See REMOVE-HOOK")

  (function run-hooks
    "Run the hooks of the given type, supplying the given arguments.

Refer to DEFINE-HOOKS for the recognised arguments for
each hook type.

This function simply iterates through *HOOKS*, checks the
HOOK-TYPE for compliance, establishes a REPORT-ERROR
restart, and then applies the HOOK-FUNCTION to the given
arguments.

The REPORT-ERROR restart simply prints out the error it
receives and is thus useful for ignoring errors that
might occur during the execution of a hook.

See *HOOKS*
See HOOK-FUNCTION
See DEFINE-HOOK")

  (function define-resource-directory
    "Shorthand to define a hook that simply deploys the given directory.

The directory has to be a form that evaluates to a
pathname to a directory that should be copied. The
path is merged with the system source directory of
the system being deployed. This means that relative
paths are relative to the source root of your system.

See DEFINE-HOOK
See COPY-DIRECTORY-TREE"))

;; library.lisp
(docs:define-docs
  (variable *system-source-directories*
    "This variable holds a list of paths to system library directories.

Deploy will search through these paths to attempt to
find the source of libraries if the more explicitly
provided paths should fail.")

  (function list-libraries
    "Return a fresh list of known foreign libraries.

All the returned libraries will be of type LIBRARY.

See LIBRARY
See CFFI:LIST-FOREIGN-LIBRARIES
See ENSURE-LIBRARY")

  (function ensure-library
    "Return the corresponding LIBRARY instance, if possible.

The following happens for the following type of
the argument:
- LIBRARY               The argument is returned.
- CFFI:FOREIGN-LIBRARY  The object is CHANGE-CLASSd
                        into a LIBRARY instance.
- SYMBOL                The library instance is
                        retrieved by name and then
                        passed back in.

See CFFI::GET-FOREIGN-LIBRARY
See LIBRARY")

  (type library
    "Class to represent a foreign library.

This is a subclass of CFFI:FOREIGN-LIBRARY with some
additional slots for information necessary for the
Deploy system.

Upon shared-initialize, if the LIBRARY-PATH is not
explicitly set, it is resolved through FIND-SOURCE-
FILE.

See LIBRARY-SYSTEM
See LIBRARY-SOURCES
See LIBRARY-PATH
See LIBRARY-DONT-OPEN-P
See LIBRARY-DONT-DEPLOY-P
See FIND-SOURCE-FILE")

  (function library-system
    "Accessor to the ASDF system associated with the library.

If there is a system associated with this library,
the system's source tree is used to search for the
library's source file.

See LIBRARY")

  (function library-sources
    "Accessor to the list of additional paths to search for source files.

See LIBRARY")

  (function library-path
    "Accessor to the definite path to the library's source file.

If this is NIL, the library could not be found.

See LIBRARY")

  (function library-dont-open-p
    "Accessor to whether the library should not be opened on boot.

See LIBRARY")

  (function library-dont-deploy-p
    "Accessor to whether to deploy (copy) the library to the resources directory on build.

See LIBRARY")

  (function possible-pathnames
    "Return a list of possible file pathnames that match the library.

By default this list includes:
- The CFFI:FOREIGN-LIBRARY-PATHNAME if present.
- Paths computed through the CFFI library's spec.
- A generic path after the LIBRARY-NAME.

See CFFI:FOREIGN-LIBRARY-PATHNAME
See CFFI:DEFINE-FOREIGN-LIBRARY
See LIBRARY-NAME
See LIBRARY
See RESOLVE-CFFI-SPEC")

  (function possible-directories
    "Return a list of possible directories to search for the library.

By default this list includes:
- The LIBRARY-SOURCES of the library.
- The directory tree of the LIBRARY-SYSTEM's source.
- The CFFI:*FOREIGN-LIBRARY-DIRECTORIES*
- The *SYSTEM-SOURCE-DIRECTORIES*
- The paths from the following environment variables:
  - PATH on Windows
  - LD_LIBRARY_PATH on Linux
  - DYLD_LIBRARY_PATH on Darwin

See LIBRARY-SOURCES
See LIBRARY-SYSTEM
See DISCOVER-SUBDIRECTORIES
See CFFI:*FOREIGN-LIBRARY-DIRECTORIES*
See *SYSTEM-SOURCE-DIRECTORIES*
See ENV-PATHS")

  (function find-source-file
    "Attempt to find the source file of the library on the system.

Uses the directories listed in POSSIBLE-DIRECTORIES
to look for the library source.

The directories are searched for pathnames that
match one of the POSSIBLE-PATHNAMES for the library.

See POSSIBLE-DIRECTORIES
See POSSIBLE-PATHNAMES")

  (function library-name
    "Return the library's name.

See CFFI:FOREIGN-LIBRARY-NAME")

  (function open-library
    "Open/load the library.

See CFFI:LOAD-FOREIGN-LIBRARY")

  (function close-library
    "Close/unload the library.

See CFFI:CLOSE-FOREIGN-LIBRARY")

  (function library-open-p
    "Returns whether the library is currently open.

See CFFI:FOREIGN-LIBRARY-LOADED-P")

  (function define-library
    "Define additional properties for a foreign library.

The NAME should be one of a valid CFFI foreign
library as defined by CFFI:DEFINE-FOREIGN-LIBRARY.

Valid properties are:
- :SYSTEM
- :SOURCES
- :PATH    
- :DONT-OPEN  
- :DONT-DEPLOY

See LIBRARY
See LIBRARY-SYSTEM
See LIBRARY-SOURCES
See LIBRARY-PATH
See LIBRARY-DONT-OPEN-P
See LIBRARY-DONT-DEPLOY-P"))

;; osx.lisp
(docs:define-docs
  (variable *info-plist-template*
    "This variable holds a pathname pointing to the Info.plist template to use for OS X app bundles.

See PARSE-INFO-PLIST")

  (variable *info-plist-readtable*
    "Custom readtable used to parse parts of the info plist.")

  (type osx-app-deploy-op
    "This deployment op generates an OS X app bundle.

See DEPLOY-OP
See PARSE-INFO-PLIST
See *INFO-PLIST-TEMPLATE*")

  (function parse-info-plist
    "Parses the Info.plist file and replaces values as appropriate.

Specifically, anything enclosed in [] is taken as a
list of symbols designating functions to call with
the system object. The resulting values are then
turned into a string by PRINC-TO-STRING, and escaped
for usage by XML-ESCAPED before writing them to
the returned string in place of the []. Note that you
can enclose multiple symbols. The value of the first
function call that returns non-NIL is used as the
value to splice into the file.

This allows to conveniently fill an Info.plist
template with values from the ASDF system.

Have a look at the standard Info.plist file shipped
with Deploy for what's possible.

See *INFO-PLIST-TEMPLATE*"))

;; toolkit.lisp
(docs:define-docs
  (variable *data-location*
    "Relative path designating the location of the resource directory.

This path should be relative to the location of
the executable.

You may set this variable before deployment to
influence where resource files are stored.

See DATA-DIRECTORY")

  (variable *status-output*
    "The output stream of status messages.

You can set this to NIL if you want to suppress status
messages on the console, or set them to some other
stream that won't bother the user.

See STATUS")

  (function data-directory
    "Return an absolute path to the resource directory.

See DATA-DIRECTORY")

  (function find-relative-path-to
    "Attempt to find a way to \"get to\" the target directory.

This tries to construct a relative path from the
source to the target pathname, using :UP transforms
in the pathname. If no relative path can be found,
an error is signalled.")

  (function make-lib-pathname
    "Create a pathname with the proper type for a library.

The type is OS-dependent.
Linux: \"so\"
Darwin: \"dylib\"
Windows: \"dll\"")

  (function pathname-filename
    "Return the full file name of the pathname as a string.")

  (function discover-subdirectories
    "Return a list of all directories contained in the path.

This includes the path itself.")

  (function status
    "Print a status message to *status-output*.

The level determines the granularity of the message.
Higher levels mean \"more detailed\".

See *STATUS-OUTPUT*")

  (function env-set-p
    "Returns the value of the given environment variable if it is set to a non-empty value.")

  (function redirect-output
    "Redirect all output to the given file.

This changes *STANDARD-OUTPUT*, *ERROR-OUTPUT*,
*TRACE-OUTPUT*, and *DEBUG-IO*. For the latter,
only its output stream is changed.

The stream to the given target file is returned.")

  (function runtime-directory
    "Returns a pathname to the directory where the executable is being run in.

See UIOP:ARGV0")

  (function directory-contents
    "Returns all files and subdirectories within the given pathname's directory.")

  (function copy-directory-tree
    "Copy the source directory to the target directory.

If COPY-ROOT is true, the source folder itself is
copied, otherwise only its contents are copied.

EXCLUDE may be a function of one argument, a source path, which
returns a boolean designating whether the source file should be
excluded or not.

IF-EXISTS may be one of the following values, governing what to do if
the destination file exists already:
  :REPLACE :SUPERSEDE :OVERWRITE
      --- Replaces the file regardless
  NIL :IGNORE
      --- Does nothing
  :ERROR
      --- Signals an error
  :UPDATE
      --- Replaces the file if its write date is older than the source
          file's")

  (function xml-escape
    "Escape the string for XML.

The following replacements are made:
 \"<\" => \"&lt;\"
 \">\" => \"&gt;\"
 \"&\" => \"&amp;")

  (function system-applicable-p
    "Returns T if the system-spec is applicable for the current system.

If system-spec is T, this returns T. Otherwise T is
returned if the system-spec is in *FEATURES*.")

  (function resolve-cffi-spec
    "Resolve the CFFI foreign library spec to a list of usable pathnames.

This is the spec as used in CFFI:DEFINE-FOREIGN-
LIBRARY.

See CFFI:DEFINE-FOREIGN-LIBRARY")

  (function split
    "Split the string on each split character.

Empty subsequences are not included.")

  (function env-paths
    "Returns the paths contained in the given environment variable.

For Windows systems, the split character is ;
otherwise it is :

See SPLIT
See UIOP:GETENV"))
