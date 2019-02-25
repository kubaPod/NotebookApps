# NotebookApps

Mini framework for easy creation of standalone or not CDFs/Notebooks, managing simple dependencies, session bookmarking etc.

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
 
   This file will be **read** withing generated notebook within a local notebook's context 
 so you don't need to worry about contexts.