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

#ifndef CODLIC_FILETYPES_H
#define CODLIC_FILETYPES_H

#include <map>
#include <stdexcept>
#include <string>

namespace codlic {

/* Get the map between filetypes and the regex that matches it. */
const std::map<std::string, std::string>& filetype_map();

/*
 * Create the map between filetypes and the regex that matches it. Don't call
 * this function, call filetype_map() instead because it stores a static
 * instances of an object returned by this function.
 */
std::map<std::string, std::string> create_filetype_map();
} /* namespace codlic */

#endif /* CODLIC_FILES_H */
