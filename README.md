# codlic
A program for adding license text to code

## Synopsis
codlic is a program that is meant for quickly licensing code files. You can
write them all, then license them all at once. It is written in Common Lisp. I
started a C++ version once but wanted to have fun.

## Installation
codlic only runs on UNIX and Unix-like systems and building it requires a Common
Lisp implementation. The only one that I've ever tried is sbcl and don't expect
it to work with anything else. It can be installed in most distros.

To install, simply run `make install` or `make install INSTALL_PREFIX=` with
root privileges. Don't expect this makefile to behave like a normal one; it
doesn't have any of the options you expect. Even reinstalling doesn't actually
update the files in their destinations for some reason. If anyone knows how to
make a common lisp build system that can be installed with the only dependency
as the lisp implementation and without touching other files such as a making a
symlink in a local-projects folder, then let me know. This is the best I could
come up with.

### Updating Library Dependencies
The libraries bundled with the project may be out of date at some point. If you
ever need to run them, navigate into the directory and run `qlot update`. You
will have to install it with roswell or something, first.

## Usage
The basic usage is `codlic [options] [files]`.

Running codlic requires a license, a way to select files, and a way to select
the comment type to use.

You can specify the license with `--license-file` or `--license-name`. Using
`--license-name` you can specify one of the included licenses such as `gplv3` or
`mit`. Note, these are templates and you need to fill in things like copyright
holder, program name, and year yourself. This can be done with --license-search
and --license-replace. The license defaults to MIT.

To specify what files to license, you can pass no options to attempt to license
every file in a directory and will probably return an error when it cannot find
the comment type for a file. One of the easiest options to use is
`--auto-detect-comment-type` which does a lookup on some preset filetypes. You
can specify individual comment languages with the `--comment-language` option to
specify one of the presets, such as `c` or `lisp`.  You can also use custom
comment types, such as python style with `--single-comment-string '#'` or C with
`--opening-comment-string "/*"`, `--closing-comment-string " */"`, and
`--continuation-comment-string " *"`.  Notice the leading spaces in the closing
and continuation comment strings, this is so the formatting lines up.

To choose what files to license, simply pass files as arguments or directories
with some selection options. One way to filter out files is to use the
`--filetype-language` with a preset such as c or lisp. Another way is to use a
regex. To license files that end in both `.c` and `.cc`, you can use
`--filetype-regex '.*\.c|.*\.cc'` or `'.*\.cc?`.

See `man codlic` for information on every option.

## Examples
To license all C files in a directory with the MIT License, use

```bash
codlic --license-name mit --filetype-language c --comment-language c \
	--license-search "<copyright year>" --license-replace 2017 my_dir
```

To license the C++ files in a directory with the gplv3

```bash
cp /usr/local/share/codlic/license/gplv3 .
sed -i 's/Foobar/your-project/g' gplv3
codlic --license-file ./gplv3 --filetype-regex '.*\.cc' \
	--auto-detect-comment-type my_dir
```
