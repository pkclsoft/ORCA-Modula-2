# ORCA/Modula-2
Apple IIGS ORCA/Modula-2 Compiler, a Modula-2 compiler for the WD65C816 with libraries for the Apple IIGS.

If you would like to make changes to this compiler and distribute them to others, feel free to submit them here.

The general conditions that must be met before a change is released on master are:

1. The modified compiler must compile under the currently released version of ORCA/M and ORCA/Modula-2.

2. All samples from the original ORCA/Modula-2 distribution must compile and execute under the modified compiler, or the sample must be updated, too.

3. The compiler must work with the current ORCA/Modula-2 libraries, or the libraries must be modified, too.

A complete distribution of the ORCA/M languages, including installers and documentation, is available from the Juiced GS store at https://juiced.gs/store/category/software/. It is distributed as part of the Opus ][ package.

The other ORCA/M compilers, and libraries can be found on Github also at: https://github.com/byteworksinc

My thanks to Mike Westerfield of The Byte Works Inc. for his support and help to bring Modula-2 to the IIGS, and to Jim Merritt and Tim Swihart (of Apple) for encouraging me at the time.

## Line Endings
The text and source files in this repository originally used CR line endings, as usual for Apple II text files, but they have been converted to use LF line endings because that is the format expected by Git. If you wish to move them to a real or emulated Apple II and build them there, you will need to convert them back to CR line endings.

If you wish, you can configure Git to perform line ending conversions as files are checked in and out of the Git repository. With this configuration, the files in your local working copy will contain CR line endings suitable for use on an Apple II. To set this up, perform the following steps in your local copy of the Git repository (these should be done when your working copy has no uncommitted changes):

1. Add the following lines at the end of the `.git/config` file:
```
[filter "crtext"]
	clean = LC_CTYPE=C tr \\\\r \\\\n
	smudge = LC_CTYPE=C tr \\\\n \\\\r
```

2. Add the following line to the `.git/info/attributes` file, creating it if necessary:
```
* filter=crtext
```

3. Run the following commands to convert the existing files in your working copy:
```
rm .git/index
git checkout HEAD -- .
```

Alternatively, you can keep the LF line endings in your working copy of the Git repository, but convert them when you copy the files to an Apple II. There are various tools to do this.  One option is `udl`, which is [available][udl] both as a IIGS shell utility and as C code that can be built and used on modern systems.

Another option, if you are using the [GSPlus emulator](https://apple2.gs/plus/) is to host your local repository in a directory that is visible on both your host computer, and the emulator via the excellent [Host FST](https://github.com/ksherlock/host-fst).

[udl]: http://ftp.gno.org/pub/apple2/gs.specific/gno/file.convert/udl.114.shk

## File Types
In addition to converting the line endings, you will also have to set the files to the appropriate file types before building ORCA/Modula-2 on a IIGS.

For each of the different groups of code, there is a`fixtypes` script (for use under the ORCA/M shell) that modifies the file and aux type of all source and build scripts, *apart from the fixtures script itself!*

So, once you have the files from the repository on your IIGS (or emulator), within the ORCA/M shell, execute the following command on each `fixtypes` script:

    filetype fixtypes src 6

## Building the compiler
There are two main steps to building the compiler:
### 1. Build the libraries
The compiler depends upon a number of libraries.  The shipped application came with `m2lib` and it's associated DEF and SYM files.  To build the compiler, you need access to these from the released compiler (available via [Juiced GS](https://juiced.gs/store/category/software/)), or you need the ability to recompile them.  The full source to these libraries is located within the `libs` folder.

There are two stages to the building of the library.  The first compiles all of the Modula-2 code.  The second compiles the assembler components of the library.  Between the two phases, two assembly files, m2lib.asm and storage.asm need to be updated so that they align with the compiled Modula-2 code.

If you have installed the following three utilities into your ORCA environment, then the script that executes the first phase also automatically does this alignment and triggers the second phase.

* gsgrep, located at [gsgrep](https://github.com/pkclsoft/gsgrep) (AppleIIGS branch)
* minised, located at [minised](https://github.com/pkclsoft/minised) (AppleIIGS branch)
* assign, located at [assign](https://github.com/pkclsoft/assign)

If you don't have these tools installed, then the first `build` script will advise you to do the alignment yourself, and to start the second phase manually.

1. Within this folder, in the ORCA/M shell, after running the `libs/fixtypes` script, execute the `libs/build` script; this initiates the first phase.  That will compile all of the Modula-2 code that makes up the library.

2. When this is complete, if you don't have the above three tools installed, you'll be instructed to, on a macOS machine (typically), within the same folder (which is possible via the excellent [Host FST](https://github.com/ksherlock/host-fst)), execute the zsh script called `libs/fixasmkeys.sh`.  This script will update the `libs/asm/m2lib.asm` and `libs/asm/storage.asm` files so that they are aligned with the `libs/def/m2lib.def` and `libs/def/storage.def` definition files that were compiled in step 1.

3. With that command executed; again, only if the above three tools are not installed, you should now run the `libs/build2` script within the ORCA/M shell.

4. At the end of that script, the newly built `m2lib` will be moved to 13/ along with all of the generated SYM files so that when you next compile a Modula-2 source file, those newly compiled libraries will be referenced.  If you're making changes to the compiler that depend on changes in these libraries, this will all be needed.

### 2. Build the compiler
Once you have libraries in `m2lib` by either getting them with your original installation of ORCA/Modula-2 or compiling them (see step 1. above), you can compile the compiler.

#### History first
Yes, the ORCA/Modula-2 compiler is written in Modula-2.  The original code was licensed from ETH, but was then ported, and comprehensively modified to generate W65C816 code, and to work within the ORCA environment.  It was originally compiled on an intel 286 computer, generating assembly source code for ORCA/M.  Once the compiler was able to run on the IIGS well enough, it started compiling itself and the 286 was abandoned (with great satisfaction).

#### Getting it done
1. Within the top level folder of the repository, assuming you are within the ORCA/M environment, first execute the `fixtypes` script (don't forget to manually change it's type).

2. Next, execute the `build` script.  That will compile, and link all of the code that makes up the actual compiler. During this you'll be prompted to allow the file `compile.rez` to be copied down into the `obj` folder, and then at the end whether you want to copy the generated compiler (`m2c)` to the 16/ prefix, where the compilers all live in ORCA/M land.

## Tests
I never wrote a comprehensive set of tests for the compiler; I relied on the fact that it compiles itself, and works.  The code in the repo is able to compile itself, and then, using the generated binary, recompile itself and generate the same code.  As far as I know, with one exception, I've previously fixed the bugs I had on file.

Within the `tests` folder there are a number of simple, and not so simple tests.  There's even a benchmark application, but I have no recollection on how to use that effectively.

Building the tests follows the same pattern:
1. Run `fixtypes`
2. Run `buildtests`
3. Run `runtests` (to see if it's all OK)
4. Run `deltests` (to remove the artefacts)

## Historical Bug Reports
Within the `bugs` folder, I've included a number of old bug reports and applications that were written to test them and their corrections.  I note now in the buglist file, that there is at least one outstanding bug.
