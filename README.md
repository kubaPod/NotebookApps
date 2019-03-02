# NotebookApps

Mini framework for easier creation of standalone or not CDFs/Notebooks.

- [x] source/dependencies parsed in notebook's local context

- [ ] injection of source files / dependencies (See `GetInjected`)

  - [x] single file packages and plain text WL code
  - [ ] multi file packages (*)
  - [ ] contexts

  (*) can be done by loading a set of single files, if package's setup allows

- [ ] localization of WL packages (e.g. ``MyPackage` ``  pushed to ``NotebookXYZ`MyPackage` ``)

  - [x] single file packages
  - [ ] multi file packages
  - [ ] contexts

- [ ] injection of resources (See `SetInjected`)

  - [x] given symbol initialized with given file's imported content
  - [x] given symbol initialized with nested association which resembles given directory structure of resources
  - [ ] add custom import rules

## Installation
 
### Manual
 
   Go to 'releases' tab and download appropriate .paclet file.
    
   Run `PacletInstall @ path/to/the.paclet` file
   
### Using ``MPM` ``
   
``MPM` `` is a small utility I wrote to install GitHub hosted paclets. If you don't have ``MPM` `` yet, run:
   
    Import["https://raw.githubusercontent.com/kubapod/mpm/master/install.m"]
   
and then:
   
    Needs @ "MPM`"    
    MPM`MPMInstall["kubapod", "notebookapps"]
    
## Quick start

    Needs @ "NotebookApps`"
    
    NewNotebookApp[ "appName", "your/Workspace/Directory"]
    
 Two files will be created
 
 - appName.nb will be your project notebook to build app / test etc
 - appNameSource.wl will be your main file with gui and other definitions. 
 

 ## Documentation

 For more details see [Wiki:Quick-Tutorial](https://github.com/kubaPod/NotebookApps/wiki/Quick-tutorial)