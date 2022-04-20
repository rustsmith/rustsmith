[![License](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)
[![Release CI](https://github.com/rustsmith/rustsmith/actions/workflows/release.yml/badge.svg)](https://github.com/rustsmith/rustsmith/actions/workflows/release.yml)

## RustSmith: A randomized program generator for Rust

RustSmith is a fuzzer built for the [Rust](https://github.com/3b1b/manim) programming language with the purpose of fuzzing the Rust compiler (rustc) and find compiler crashes and mis-compilations within the compiler. This repo contains the source code of the generator along with information on how to install and use it.

## Table of Contents:
- [Getting Started](#getting-started)
- [Building From Source](#building-from-source)
- [Usage](#usage)
- [Documentation](#documentation)
- [Contributing](#contributing)


## Getting Started

To get started with RustSmith, there are a couple of options as shown below:

### Recommended Method
To download and install RustSmith, the recommended method is to simply download the "RustSmith Executable" from the latest release from the "Releases" panel on the right.

RustSmith can then be invoked as below:

```shell
    ./rustsmith --help
```

### Running RustSmith using the JAR File

Alternatively, the JAR file in the releases can be downloaded and used instead. RustSmith would then be invoked as:

```shell
    java -jar RustSmith-1.0-SNAPSHOT-all.jar
```

## Building from Source

To build RustSmith from source, an installation of Java 15 is required. More information for installing OpenJDK 15 can be found on the [OpenJDK Website](https://openjdk.java.net).

Building and executing RustSmith can then be done as follows:

```shell
    git clone git@github.com:rustsmith/rustsmith.git
    cd rustsmith
    ./gradlew build
```

This will create both the standalone executable under `./run/rustsmith` and the packaged JAR file under `./build/libs/RustSmith-1.0-SNAPSHOT-all.jar` which can then be executed as described above.

## Usage

```shell
Usage: rustsmith [OPTIONS]

Options:
  -n, -count INT    No. of files to generate
  -p, -print        Print out program only
  -s, -seed INT     Optional Seed
  --directory TEXT  Directory to save files
  -h, --help        Show this message and exit

```

## Documentation
Documentation is in progress and will be released soon!

## Contributing
Contributions are all welcome! If you would like to contribute, please see the corresponding [guidelines][contributing]. By contributing, you are agreeing to our [code of conduct][code-of-conduct].

[contributing]: https://github.com/VAlgoLang/VAlgoLang/blob/master/CONTRIBUTING.md
[code-of-conduct]: https://github.com/VAlgoLang/VAlgoLang/blob/master/CODE_OF_CONDUCT.md