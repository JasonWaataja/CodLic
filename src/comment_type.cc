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

#include "comment_type.h"

codlic::CommentType::CommentType() : family{SINGLE_DELIMITER}
{
}

codlic::CommentType::CommentType(const std::string& single_delimiter)
    : family{SINGLE_DELIMITER}, single_delimiter{single_delimiter}
{
}

codlic::CommentType::CommentType(const std::string& opening_delimiter,
    const std::string& closing_delimiter,
    const std::string& continuation_delimiter)
    : family{OPENING_CLOSING_DELIMITER}
    , opening_delimiter{opening_delimiter}
    , closing_delimiter{closing_delimiter}
    , continuation_delimiter{continuation_delimiter}
{
}

const std::map<std::string, codlic::CommentType>&
codlic::comment_types()
{
    static std::map<std::string, CommentType> comment_data =
        create_comment_types();
    return comment_data;
}

std::map<std::string, codlic::CommentType>
codlic::create_comment_types()
{
    std::map<std::string, CommentType> comment_types_data;
    comment_types_data["c"] = CommentType{"/*", " *", " */"};
    comment_types_data["c++"] = CommentType{"//"};
    return comment_types_data;
}
