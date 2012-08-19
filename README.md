SimpleMAC
=========

SimpleMAC is a marker and call method CFD (computational fluid dynamics) solver. It was originally developed for a graduate supercomputing course, and is now being released and updated for further use.

How to Develop
==============
Assuming you are familiar with git:
Either
  a) Fork the code
  b) Contact me and we can work together

An explanation of all the code may be found in the Doxygen documentation [here](http://jonkomperda.github.com/SimpleMAC).

License
=======
Prior to performing and research or publication with the use of this code you are required to contact me. You can not take this code and publish with it as your own, even with extensive changes, as this is considered plagiarism. Similarly do not use this code for classroom work, as it is also considered plagiarism and may be cause for dismissal from your university.

I require that I be informed and cited for any research/institution/publication/commercial use of this code.

How to Run
==========
1) To find a working version of the code (not currently in development and sure to compile/run as expected) go to the [Tags page](https://github.com/jonkomperda/SimpleMAC/tags).
2) Choose the version of the code you wish to download.
3) Extract the archive and 'cd' into the directory in your terminal.
4) type 'make' to build the code
5) type 'make run' to run the code
  a) 'make run' builds the correct directory structure for the data output. If you choose not to use this then be sure that you have the correct directory structure prior to running or you may encounter unexpected results.
6) open the results in the data directories using VisIt or any other visualization software that uses the VTK format.
7) modify 'size.f90' to change case specifics to your liking.
