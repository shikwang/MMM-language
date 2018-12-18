# MMM-language

“MMM” is a Matlab style matrix manipulation language that can be used to calculate matrix operations efficiently. Users are able to do matrix operations through both in-built and self-defined functions. Same operations can be applied to user defined struct to make code more concise and more efficient. Due to our implementation and optimization of basic matrix operations in Ocaml, the basic matrix operations will be fast. The application of this programming language ranges from image cropping, rotating, denoising, enhancement, edge detection, and color filtering. Users are able to define the struct and functions to process image more efficiently. 

download openCV
https://medium.com/@jaskaranvirdi/setting-up-opencv-and-c-development-environment-in-xcode-b6027728003

if error:
brew install glog


io.cpp is for external openCV library


io_compile.sh is to compile with io.cpp



To test image read:


make clean


make


./io_compile.sh ioTest.mmm


./main

