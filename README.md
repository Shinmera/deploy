## About Deploy
This is a system to help you easily and quickly deploy standalone common lisp applications as binaries. Specifically it is geared towards applications with foreign library dependencies that run some kind of GUI.

## How To
Probably the easiest way to use Deploy is with ASDF. To integrate it, change your system definition to include the following properties:

```lisp
:defsystem-depends-on (:deploy)
:build-operation "deploy-op"
:build-pathname "my-application-name"
:entry-point "my-package:my-start-function"
```

Once you have updated your system appropriately, all you need to do is start a fresh instance of your implementation from a terminal and run the following function:

```lisp
(asdf:make :my-system)
```

This will build your system, gather the necessary information, and deploy a standalone `bin` folder within your project's root directory. You can then ship this folder to your users.

If you want to deploy a console application rather than a GUI application, you should use ``deploy-console-op`` instead, which will activate the proper mode on Windows, and suppress the standard startup output.

You can also leverage deploy to "just" create an image of your implementation state rather than a standalone executable. In that case, you should use the ``deploy-image-op``.

With SBCL you can even leverage Deploy to create "shrinkwrapped" executables. These executables can statically or dynamically link C libraries and let Lisp stack frames integrate properly with foreign debuggers and profiling tools like perf. To produce a shrinkwrapped executable you must have the SBCL source tree available with which you already built your SBCL. Then you can create your executable by loading deploy and running

```lisp
(deploy:shrinkwrap "my-system")
```

It'll automatically invoke itself again to run ``deploy-image-op`` on your system, shrinkwrap that, and then compile it together with the rest of the dynamic libraries it depends on into the final executable.

## Without ASDF
Deploy can also be used entirely without depending on ASDF or UIOP. After prepping everything you want, you can simply call ``deploy`` with either the name of the operation type you want to perform the usual kind of deployment, or directly with a pathname to deploy your executable to. In the latter case, it will *not* run any of the usual hooks and instead perform as plain of a deployment as possible.

## Customising Foreign Libraries
Sometimes you might want to designate a specific library for deployment, rather than the one used during development. If this is the case, you have to help Deploy out a little by defining extra information about the library with `define-library`. If the foreign library is in the source tree of a lisp library, you can simply associate the CFFI library with the system that provides it, and Deploy will find it automatically:

```lisp
(deploy:define-library cffi-library-symbol
  :system :system-name-that-defines-the-library)
```

For example, the [cl-mpg123](https://github.com/Shirakumo/cl-mpg123) system provides a single library, which we would annotate like this:

```lisp
(deploy:define-library cl-mpg123-cffi:libmpg123
  :system :cl-mpg123)
```

If the file is not contained in the directory of the system that provides it, you can also designate specific source directories to scan:

```lisp
(deploy:define-library cffi-library-symbol
  :sources '("/some/path/where/the/library/is/stored/"))
```

Finally, you can also specify the path directly if you want Deploy to choose a particular file, rather than trying to find one on its own:

```lisp
(deploy:define-library cffi-library-symbol
  :path "/some/path/to/the/file.so")
```

Generally though these extra associations should not be necessary as Deploy will simply take the path that CFFI has already figured out to find the library.

Sometimes it might not be desired to deploy all the libraries, or reload them all upon boot. You can change this behaviour with `define-library`'s `:dont-deploy` and `:dont-open` properties respectively.

## Marking System Libraries as a Library Author
If you are a library author and want to make use of a system library, say Ole32 on Windows, or Foundation on MacOS, or FontConfig on Linux, those libraries should pretty much never be deployed. However, CFFI does not have a way to mark that status on its own, and depending on Deploy just to do that is silly.

In order to still be able to give that hint, you can add the name of your library to ``cl-user::*foreign-system-libraries*``. Deploy will pick up on that on its own and, unless specified directly in the ``define-library``, will make use of that. This automates the deployment exemption for users. An easy way to achieve this behaviour is:

```lisp
(set 'cl-user::*foreign-system-libraries*
     (union (when (boundp 'cl-user::*foreign-system-libraries*)
              (symbol-value 'cl-user::*foreign-system-libraries*))
            '(ole32 foundation fontconfig)))
```

## Extending Deployment and Boot Behaviour
Deploy also offers a hooks system with which you can easily extend the steps performed during deployment and during the boot process of the generated executable. With the `define-hook` macro you can add functions that are executed during various points of the process. Specifically, the following types are available:

* `:deploy` These functions are responsible for copying files into the deployment target directory.
* `:build` These functions should prepare the system for the dump to binary. Specifically you might want to shut down existing threads, close file handles, remove unneeded baggage, and remove potentially sensitive information about your system.
* `:boot` These functions are run right before the primary entry point is executed. Thus they are responsible for preparing the runtime to continue by restarting threads, re-opening files, and so forth.
* `:quit` These functions are run right before the executable exits completely. They offer a last-minute opportunity to dump some information about the system, or to clean up vital resources.

If you would simply like to include a data directory to bundle with the rest, use `define-resource-directory`. After boot, all of the resource files will be in the directory returned by `data-directory`.

## Deploying to an OS X App Bundle
If you would like a nicely bundled `.app` for OS X, you can simply change the `build-operation` in your ASDF system file to `osx-app-deploy-op`. If you would like to customise the `Info.plist` file that is generated for the app, you can change `*info-plist-template*` to point to a file that contains what you want.

## Debugging a Deployed Executable
If you're having trouble with an application that's already deployed, there's a few things you can do to debug it by setting environment variables. The following are recognised by Deploy:

* `DEPLOY_DEBUG_BOOT` if set to a non-empty value, on error the debugger is invoked rather than just exiting the application.
* `DEPLOY_REDIRECT_OUTPUT` if set to a file path, the output of all streams is redirected to this file.

Particularly on Windows and OS X debugging can be an issue, as a GUI application will not get a standard output to write to. In that case, the above redirect might help.

## Support
If you'd like to support the continued development of Deploy, please consider becoming a backer on Patreon:

<a href="https://patreon.com/shinmera">
  <img alt="Patreon" src="https://filebox.tymoon.eu//file/TWpjeU9RPT0=" />
</a>
