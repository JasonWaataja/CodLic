/*
 * Copyright (C) 2017 Jason Waataja

 * This file is part of CodLic.

 * CodLic is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.

 * CodLic is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.

 * You should have received a copy of the GNU General Public License
 * along with CodLic.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef CODLIC_OPTIONS_H
#define CODLIC_OPTIONS_H

#include <string>
#include <vector>

namespace codlic {

class Options {
public:
    Options(int argc, char *argv[]);
    /* Name of built-in licens name, such as "GPLv3". */
    bool has_license_name = false;
    std::string license_name;
    /* File to read license text from. */
    bool has_license_file = false;
    std::string license_file;
    /* Operate recursively on directories. */
    bool is_recursive = false;
    /* The name of the built-in filetype, such as "C" or "List". */
    bool has_filetype = false;
    std::string filetype;
    /* The regex to use for testing what source files to operate on. */
    bool has_filetype_regex;
    std::string filetype_regex;
    /* Name of built-in comment type such as "C", or "Lisp". */
    bool has_comment_type = false;
    std::string comment_type;
    /*
     * String for a line beginning comments. This is for filetypes such as
     * Python and Lisp that don't have both opening and closing comment string.
     */
    bool has_comment_string = false;
    std::string comment_string;
    /* String for the beginning of a block comment.*/
    bool has_opening_comment_string = false;
    std::string opening_comment_string;
    /* String for the end of a block comment. */
    bool has_closing_comment_string = false;
    std::string closing_comment_string;
    /* String for lines in the middle of a block comment. */
    bool has_continuation_comment_string = false;
    std::string continuation_comment_string;

    /* The non-option arguments. These represent the files to operate on. */
    std::vector<std::string> args;

private:
    void print_usage();
};
} /* namespace codlic */

#endif /* CODLIC_OPTIONS_H */
