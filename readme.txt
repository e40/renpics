
Table of Contents:

1. Introduction
2. Binary installation
3. Building from source code

===============================================================================
1. Introduction

"renpics" -- rename pictures from a digital camera

renpics can be used to rename `jpg' (and `avi' and `wav') files
produced by a digital camera based on exposure date and time the
pictures were taken.  The files are renamed to have the form:

   YYYYMMDD-hhmm-ssnn-xxxx.jpg

where YYYY is the year, MM is the month, DD is the day, hh is the
hour, mm is the minute, ss is the second, nn is a sequence number used
when more than one picture was taken in a given second, and xxxx
is the camera name.

renpics is a Windows console app.  That is, it is meant to be run from
a command shell (command.com or cmd.exe).

Here is a bat file that I use to move files from a CF (CompactFlash)
card onto my hard drive:

   @echo off
   renpics -m -o c:/pictures/_TMP/ j:/
   pause

-m means `move' instead of `copy'.  `-o c:/pictures/_TMP/' is the
destination directory and `j:/' is the source directory, which is the
CompactFlash reader.  renpics looks in all subdirectories of j:/ for
all .jpg's.

As mentioned above the name of the camera is added to the filename, at
the end.  For those that have different models of digital cameras, it
is often very useful to know from which camera a picture was taken.
If `renpics' does not know about your camera, then you can tell it
with the -c command line argument (see below).

The command line arguments are:

-c camera

  Set the name of the camera to `camera'.

-f

  Use the date `of' the file not the date `in' the file.  The date in
  the file is taken from the EXIF header created by the camera.

-o output-directory

  Set the output directory where the renamed files go.  It can be the
  same as the input directory, in which case the files are just
  renamed in that directory.

-m

  Move instead of copy the files.

-n

  Do not rename any files, just print what would have been done.

-q

  Be quiet, very quiet.  Normally renpics prints informative messages
  as to what it is doing.

===============================================================================
2. Binary installation

Copy the appropriate binary distribution:

ftp://ftp.franz.com/pub/examples/renpics/renpics-1.8-windows.zip

Unzip the binary and put the files somewhere in your PATH.  The
file in the ``system-dlls'' sub-directory, msvcrt.dll, will only be
needed in rare cases--if you get an error message about this DLL
being missing, copy it into the same directory as pubpics.exe.

===============================================================================
3. Building from source code

Start up a fully patched Allegro CL (6.2 or later) Enterprise Edition
and evaluate this:

      (load "buildit.cl")

It will make a renpics/ directory, which is how the renpics-<version>
directories above was created.
