# Description
A modern Fortran library for reading and writing WAV (Wave Audio Format) files.

# Getting started
## Build with [fortran-lang/fpm](https://github.com/fortran-lang/fpm)
Fortran Package Manager (fpm) is a package manager and build system for Fortran.  
To use `wav-fortran` within your `fpm` project, add the following to your `fpm.toml` file:
```toml
[dependencies]
wav-fortran = { git="https://github.com/Murase-Hijiri/wav-fortran.git" }
```
## Get the source code
You can get the source code at [release](https://github.com/Murase-Hijiri/wav-fortran/releases).  
Move `./src/wav-fortran.f90` to your project directory, compile the source code and link to your main program. For example:
```bash
$ gfortran -c wav-fortran.f90
$ gfortran -o main.out main.f90 wav-fortran.o
```

# Documentation
See the documentation [here](https://github.com/Murase-Hijiri/wav-fortran/tree/main/doc).

# License
Licensed under [MIT License](https://github.com/Murase-Hijiri/wav-fortran/blob/main/LICENSE).
