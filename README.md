[![Release](https://img.shields.io/github/release/tugraskan/SWIFT.svg?style=flat-square)](https://github.com/tugraskan/SWIFT/releases)
# SWIFT

The **Soil and Water Integrated Flow Tool** ([SWIFT](https://swift.gitbook.io/docs)) is an open-source model for rapidly estimating sediment and nutrient loads from small watersheds in the United States. Built upon the foundation of the SWAT+ framework, SWIFT leverages readily available data—such as watershed size, land use, and geographic location—to generate load estimates along with distributions that capture the inherent uncertainties of the estimation process. It employs export coefficients originally derived from the Soil and Water Assessment Tool (SWAT) and incorporates delivery ratios informed by the Conservation Effects and Assessment Project (CEAP). This approach makes SWIFT a practical tool for assessing non-point source pollution, soil erosion prevention, and the broader environmental impacts of land use and management practices.

This repository is based on the original SWAT+ repository and has been updated to incorporate the SWIFT framework. It includes the latest SWIFT source code along with test data to compile and validate the executable across various compilers and platforms.
 

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

! Planned [SWIFT Input/Output Documentation on Gitbook]()

[SWIFT at TAMU](https://blackland.tamu.edu/news/2023/acre-and-swift/)


