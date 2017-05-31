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

#ifndef CODLIC_COMMENT_TYPE_H
#define CODLIC_COMMENT_TYPE_H

#include "config.h"

#include <map>
#include <string>

namespace codlic {

class CommentType {
public:
    enum CommentFamily {
        SINGLE_DELIMITER,
        OPENING_CLOSING_DELIMITER
    };

    CommentFamily family;

    std::string single_delimiter = "//";
    std::string opening_delimiter = "/*";
    std::string closing_delimiter = " *";
    std::string continuation_delimiter = " */";

    CommentType();
    explicit CommentType(const std::string& single_delimiter);
    CommentType(const std::string& opening_delimiter,
        const std::string& closing_delimiter,
        const std::string& continuation_delimiter);
};

/* Get the map of language names to objects for their comment types. */
const std::map<std::string, CommentType>& comment_types();

/*
 * Don't call this. Use comment_types() instead, it stores a static instance of
 * an object create by this function.
 */
std::map<std::string, CommentType> create_comment_types();
} /* namespace codlic */

#endif /* CODLIC_COMMENT_TYPE_H */
