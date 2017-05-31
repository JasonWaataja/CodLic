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

#include "config.h"

#include "options.h"

#include <getopt.h>

#include <iostream>
#include <stdexcept>

codlic::Options::Options(int argc, char *argv[])
{
    int option_index = 0;
    struct option long_options[]{
        {"license-name", required_argument, nullptr, 'l'},
        {"license-file", required_argument, nullptr, 'F'},
        {"recursive", no_argument, nullptr, 'r'},
        {"filetype", required_argument, nullptr, 'f'},
        {"filetype-regex", required_argument, nullptr, 'R'},
        {"comment-type", required_argument, nullptr, 't'},
        {"auto-determine-comment-type", no_argument, nullptr, 'a'},
        {"comment-string", required_argument, nullptr, 'c'},
        {"opening-comment-string", required_argument, nullptr, 'o'},
        {"closing-comment-string", required_argument, nullptr, 'C'},
        {"continuation-comment-string", required_argument, nullptr, 'm'},
        {nullptr, 0, nullptr, 0}
    };
    int c = getopt_long_only(argc, argv, "lFrfRtacoCm:", long_options,
        &option_index);
    while (c != -1) {
        switch (c) {
        case 0:
            break;
        case 'l':
            has_license_name = true;
            license_name = optarg;
            break;
        case 'F':
            has_license_file = true;
            license_file = optarg;
            break;
        case 'r':
            is_recursive = true;
            break;
        case 'f':
            has_filetype = true;
            filetype = optarg;
            break;
        case 'R':
            has_filetype_regex = true;
            filetype_regex = optarg;
            break;
        case 't':
            has_comment_type = true;
            comment_type = optarg;
            break;
        case 'a':
            should_auto_determine_comment_type = true;
            break;
        case 'c':
            has_comment_string = true;
            comment_string = optarg;
            break;
        case 'o':
            has_opening_comment_string = true;
            opening_comment_string = optarg;
            break;
        case 'C':
            has_closing_comment_string = true;
            closing_comment_string = optarg;
            break;
        case 'm':
            has_continuation_comment_string = true;
            continuation_comment_string = optarg;
            break;
        case ':':
            print_usage();
            throw std::runtime_error{"Failed to parse argument."};
        }
        c = getopt_long_only(argc, argv, "lFrfRcoCm:", long_options,
            &option_index);
    }
    for (int i = optind; i < argc; i++)
        args.push_back(std::string(argv[i]));
}

void
codlic::Options::print_usage()
{
    std::cout << "usage:" << std::endl;
}
