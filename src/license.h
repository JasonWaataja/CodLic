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

#ifndef CODLIC_LICENSE_H
#define CODLIC_LICENSE_H

#include "config.h"

#include <map>
#include <vector>

namespace codlic {

/* Get a map of license names to paths where they are contained. */
const std::map<std::string, std::string>& license_paths();

/*
 * Don't call this function. Call license_paths() instead. It returns a static
 * instance of an object created by this function.
 */
std::map<std::string, std::string> create_license_paths();

/*
 * Uses license_paths() and the installation path to get the correct file
 * location.
 */
std::string get_license_path(const std::string& license_name);

class License {
public:
    explicit License(const std::string& file_path);

    const std::string& get_license_file() const;
    std::vector<std::string> get_license_lines() const;

private:
    std::string license_file;
};
} /* namespace codlic */

#endif /* CODLIC_LICENSE_H */
