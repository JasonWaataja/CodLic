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
#include <string>

namespace codlic {

const std::map<std::string, std::string>& filetype_map();

class FiletypeMap {
public:
    FiletypeMap();
    const std::map<std::string, std::string>& filetype_map();

private:
    std::map<std::string, std::string> map_data;
};
} /* namespace codlic */

#endif /* CODLIC_FILES_H */
