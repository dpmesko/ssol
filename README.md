# SSOL

Welcome to the repository for the SSOL Programming Language! 


THE SSOL LANGUAGE COMPILER

Authors {Maddy Tipp mrt2148 (Manager/Tester), Jeevan Farias jtf2126 (Language Designer), Daniel Mesko dpm2153 (System Architect)}


To compile and run SSOL programs, first run 'make' to create the compiler executable 'ssol.native', along with object code for our SVG libraries. 
------------------------------
To run and test our hello world test program:

$ make
$ ./ssol tests/test-draw

The executable shell script 'ssol', compilesh the input file 'test/test-draw.ssol' into LLVM IR in a file called 'tests/test-draw.ll'.
Then 'llc' is used to compile the .ll into assembly code, saved in 'tests/test-draw.s'.
Finally, the assembly code is linekd with our relevant object code, 'printbig.o', 'draw.o', and 'svg.o', to produce and then run an executable called 'tests/test-draw'. 
The executable is automatically removed by the shell script after it is run. 
This program outputs a single svg file 'hello_world.svg', the name of which is hardcoded within this test program. The file will be written the same directory from which the compiler is run.
'hello_world.svg' can be opened in any text or image editor.
