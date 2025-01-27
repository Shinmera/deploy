(in-package #:org.shirakumo.deploy)

;; asdf.lisp
(docs:define-docs
  (hook (:build clear-asdf)
    "Clears the ASDF configuration and freezes it in place.

This prevents issues with calling ASDF or UIOP functions
after the image has been dumped and resumed on another
setup that does not match the build host's."))

;; checksum.lisp
(docs:define-docs
  (variable *source-checksum*
    "If true pre-build, is set to the checksum of all source files on build.

See SOURCE-CHECKSUM")

  (variable *build-time*
    "If true pre-build, is set to the universal-time on build.")

  (function list-all-source-files
    "Returns a list of all source files of the current system state, if possible.

If ASDF is available this uses the list of loaded system and their
descriptions to compute the list.")

  (function source-checksum
    "Computes a checksum of the given source files.

The source files are sorted by their truenames in order to ensure
consistency regardless of order.

By default the output of LIST-ALL-SOURCE-FILES is used.

See LIST-ALL-SOURCE-FILES
See *SOURCE-CHECKSUM*"))

;; deploy.lisp
(docs:define-docs
  (variable *foreign-libraries-to-reload*
    "This variable keeps a list of library instances to open on boot.

The variable is set during deployment and should not be
modified unless you know what you're doing.")

  (function deployed-p
    "Returns T if the current Lisp environment has been booted from a deployed executable.")

  (function quit
    "Runs the quit hooks and terminates the application.

If an error occurs during the execution of a quit hook, it
is ignored.

See RUN-HOOKS")

  (function warmly-boot
    "Perform a warm boot.

This updates the CFFI:*FOREIGN-LIBRARY-DIRECTORIES* variable
to a sensible value which should hopefully ensure that your
designated libraries are reloaded on boot.

This will also run all :BOOT hooks with the appropriate
arguments supplied.

See CFFI:*FOREIGN-LIBRARY-DIRECTORIES*
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

  (type deploy-op
    "An operation to perform a deployment.

When ASDF is available, this is also an ASDF:OPERATION.

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
   appropriate application type.

See DEPLOY
See ENTRY-POINT
See OUTPUT-FILE
See LIST-LIBRARIES
See *FOREIGN-LIBRARIES-TO-RELOAD*
See *DATA-LOCATION*
See RUN-HOOKS")

  (type deploy-console-op
    "An operation to perform a console application deployment.

This is similar to DEPLOY-OP, but enforcing the DEPLOY-CONSOLE flag.

See DEPLOY-OP
See DEPLOY")

  (type deploy-image-op
    "An operation to perform an image deployment.

This is similar to DEPLOY-OP, but enforcing the DEPLOY-IMAGE flag.

See DEPLOY-OP
See DEPLOY")

  (function entry-point
    "Accesses the image entry point of the deployment operation.

If NIL the entry point will be the implementation's default, or in the
case of an ASDF operation, the entry point specified in the system
definition.

See DEPLOY
See DEPLOY-OP")

  (function output-file
    "Accesses the target output file of the deployment operation.

If NIL a file will be picked for you. In the case of an ASDF
operation, this will be a file named after the system being operated
on, and the directory will be a subdirectory called bin/ within the
system's source directory. Otherwise the bin/ directory within
Deploy's source directory will be used and the name will simply be
\"application\".

See DEPLOY
See DEPLOY-OP")

  (function deploy
    "Performs a deployment.

If the target is a pathname, a deployment is performed directly
without running any hooks.

If the target is a symbol, an operation object is created according to
that symbol's type and the extra arguments provided.

If the target is an operation object,  deployment is performed
according to that operation, running hooks as needed, ultimately
running DEPLOY with the intended target pathname.

If an ENTRY-POINT is given, that function will be invoked on boot. If
a TYPE is given, it designates the kind of deployment to perform. It
may be one of

  :EXECUTABLE -- A GUI application is deployed.
  :CONSOLE    -- A console application is deployed.
  :IMAGE      -- An image core file is deployed."))

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

See LIBRARY-SOURCES
See LIBRARY-PATH
See LIBRARY-DONT-OPEN-P
See LIBRARY-DONT-DEPLOY-P
See FIND-SOURCE-FILE")

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
See ENVVAR-DIRECTORIES")

  (function find-source-file
    "Attempt to find the source file of the library on the system.

Uses the directories listed in POSSIBLE-DIRECTORIES
to look for the library source.

The directories are searched for pathnames that
match one of the POSSIBLE-PATHNAMES for the library.

See LIBRARY
See POSSIBLE-DIRECTORIES
See POSSIBLE-PATHNAMES")

  (function library-name
    "Return the library's name.

See CFFI:FOREIGN-LIBRARY-NAME
See LIBRARY")

  (function library-soname
    "Return the library's encoded soname, if possible.

See LIBRARY")

  (function library-dependencies
    "Return the library's dependant libraries, if possible.

The returned value is a list of string names of the libraries.

See LIBRARY")

  (function open-library
    "Open/load the library.

See LIBRARY
See CFFI:LOAD-FOREIGN-LIBRARY")

  (function close-library
    "Close/unload the library.

See LIBRARY
See CFFI:CLOSE-FOREIGN-LIBRARY")

  (function library-open-p
    "Returns whether the library is currently open.

See LIBRARY
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
See LIBRARY-DONT-DEPLOY-P")

  (function patch-soname
    "Patch the library's encoded soname to match the file name.

Can be invoked with a LIBRARY, a LIBRARY designator, or a pathname of
the library file to patch.

See LIBRARY")

  (function patch-dependencies
    "Patch the library's encoded dependencies to match the given spec.

The spec should be a list of lists, each inner list having two
elements, the first being the name of the dependency to change, and
the second being the name to change it to.

Can be invoked with a LIBRARY, a LIBRARY designator, or a pathname of
the library file to patch.

See LIBRARY")

  (hook (:deploy foreign-libraries)
    "Gathers foreign library files and copies them to the deployment output path.

This will adjust the library spec to be fixed to the specific pathname
it has, to ensure it will be loaded when it is requested again later
on boot.

Deploy will do a best-effort scan to find the library file, but if
this fails, it will signal a continuable error with a USE-VALUE
restart active.")

  (hook (:build foreign-libraries)
    "Closes all foreign libraries and cleans out their registered paths.

This will also clear out CFFI's foreign library directory list.")

  (hook (:boot foreign-libraries)
    "Reloads all needed foreign libraries.

See *FOREIGN-LIBRARIES-TO-RELOAD*"))

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

;; shrinkwrap.lisp
(docs:define-docs
  (variable *sbcl-source-tree*
    "Path to the SBCL source tree's root.

This tree must have already been used to build the same SBCL you want
to deploy with.

Will try to auto-detect the path based on the logical pathname
translations of SYS.")

  (function shrinkwrap
    "Shrinkwrap the given ASDF system.

This is the same as
  (ASDF:OOS 'DEPLOY:SHRINKWRAP-OP system args...)

See SHRINKWRAP-OP")

  (function shrinkwrap-op
    "An ASDF operation to produce a shrinkwrapped executable.

Shrinkwrapping cannot be done in the same process, so this will start
foreign processes to drive the build.

It proceeds as follows:

1. Run DEPLOY-IMAGE-OP on the system to produce a full core.
2. Run the elftool to split the core into a symbol and object file.
3. Run the C compiler and linker to assemble a fully shrinkwrapped
   executable. This will use the same path as DEPLOY-OP.

See *SBCL-SOURCE-TREE*"))

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

  (function status
    "Print a status message to *status-output*.

The level determines the granularity of the message.
Higher levels mean \"more detailed\".

See *STATUS-OUTPUT*")

  (function envvar
    "Returns the value of the given environment variable.

May return NIL or an empty string if the variable is unset.")

  (function envvar-directory
    "Returns the environment variable as a pathname directory.

See ENVVAR")

  (function envvar-directories
    "Returns the paths contained in the given environment variable.

For Windows systems, the split character is ;
otherwise it is :

See ENVVAR")

  (function env-set-p
    "Returns the value of the given environment variable if it is set to a non-empty value.")

  (function redirect-output
    "Redirect all output to the given file.

This changes *STANDARD-OUTPUT*, *ERROR-OUTPUT*,
*TRACE-OUTPUT*, and *DEBUG-IO*. For the latter,
only its output stream is changed.

The stream to the given target file is returned.")

  (function featurep
    "Returns true if the given name names a feature.

See CL:*FEATURES*")

  (function runtime-directory
    "Returns a pathname to the directory where the executable is being run in.

See UIOP:ARGV0")

  (function copy-file
    "Copy the source to the target file.

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
          file's

See COPY-DIRECTORY-TREE")

  (function copy-directory-tree
    "Copy the source directory to the target directory.

If COPY-ROOT is true, the source folder itself is
copied, otherwise only its contents are copied.

EXCLUDE may be a function of one argument, a source path, which
returns a boolean designating whether the source file should be
excluded or not.

See COPY-FILE"))
