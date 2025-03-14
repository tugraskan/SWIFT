[![Release](https://img.shields.io/github/release/tugraskan/SWIFT.svg?style=flat-square)](https://github.com/tugraskan/SWIFT/releases)
# SWIFT

The **Soil and Water Integrated Flow Tool** [SWIFT](https://swift.gitbook.io/docs) is an open source model jointly developed by the USDA Agricultural Research Service ([USDA-ARS](http://ars.usda.gov)) and Texas A&M AgriLife Research, part of The Texas A&M University System. Model contributions have been made by Colorado State University and others. SWIFT is a small watershed to river basin-scale model to simulate the quality and quantity of surface and ground water and predict the environmental impact of land use, land management practices, and climate change. SWIFT is widely used in assessing soil erosion prevention and control, non-point source pollution control and regional management in watersheds.

This repository contains the latest SWIFT source code and some test data to create and test the executable for various compiler and platforms. 

## Repository

Get the SWIFT sources by cloning the forked repository using `git`.  

```bash
$ git clone https://github.com/<user>/swift.git
```

Or, download the sources directly from the artifacts, unzip. Use a tagged version (preferred).

```bash
$ wget https://github.com/tugraskan/SWIFT/archive/refs/tags/61.0.zip
```

## Directory Structure

The directory structure is shown below. The `build` directory gets created and populated during the generation of the `cmake` files and the `cmake` build. 

```
swift
├── build
│   ├── ...
│   ├── *.mod
│   ├── Testing
│   └── CMakeFiles
│       ├── Makefile.cmake
│       ├── ...
│       └── swift-<ver>.dir
│           ├── *.mod.tstamp
│           ├── src
│           └── ...
├── data                      ---> contains all data sets for testing
│   ├── Ames_sub1
│   ├── <other>
│   └── ...
├── src                       ---> contains all swift Fortran source files
│   └── *.f90
├── test                      ---> contains all unit tests sources
│   ├── check.py
│   └── ...
├── doc                       ---> contains all hosted documentation
├── CMakeLists.txt            ---> cmake project file
├── ford.md.in                ---> FORD Documentation creation project
├── README.md                 ---> this file
└── ...
```

## Developing SWIFT

This GitHub repository is setup to build, test, and deploy SWIFT using the CMake tool. CMake is a cross-platform build tool that can be used at the command line but it is also supported through various IDEs, etc. More information can be found at [http://cmake.org](http://cmake.org). 

In addition to CMake, the following tools are also needed:

- `git` tool for version control
- `make` tool (for building)
- `gfortran` or `ifort/ifx` compiler and linker (for compiling/linking)
- `python3` (for testing, optional)
- `ford` (for documentation generation)

Use the operating system's preferred way of adding those tools to your installation. There is certainly more than one way of getting and installing them.

__The following sections are emphasizing various development aspects.__

* [Configuring, Building, Installing SWIFT using cmake](doc/Building.md)
- [Scenario Testing](doc/Testing.md)

- [Tagging and Versioning](doc/Tagging.md)

- [Developing in Visual Studio](doc/VS-Win.md)

- [FORTRAN Coding Conventions (alpha)](doc/coding_conventions.md)

## Documentation and References

[SWIFT Source Documentation on GitHub](https://tugraskan.github.io/SWIFT)

[SWIFT Input/Output Documentation on Gitbook](https://swift.gitbook.io/docs)

[SWIFT at TAMU](https://swift.tamu.edu)

[Older SWIFT versions on Bitbucket](https://bitbucket.org/blacklandgrasslandmodels/modular_swift/src/master)
