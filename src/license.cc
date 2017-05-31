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

#include "license.h"

const std::map<std::string, std::string>&
codlic::license_paths()
{
    static std::map<std::string, std::string> path_data =
        create_license_paths();
    return path_data;
}

std::map<std::string, std::string>
codlic::create_license_paths()
{
    std::map<std::string, std::string> path_data;
    path_data["gplv3"] = "gplv3";
    path_data["mit"] = "mit";
    return path_data;
}

std::string
codlic::get_license_path(const std::string& license_name)
{
    auto license_data = license_paths();
    auto license_pos = license_data.find(license_name);
    if (license_pos == license_data.end())
        throw std::runtime_error("Unknown license name.");
    std::string license_path = INSTALL_DIR;
    license_path += "/" + license_pos->second;
    return license_path;
}

codlic::License::License(const std::string& file_path)
    : license_file{file_path}
{
}

const std::string&
codlic::License::get_license_file() const
{
    return license_file;
}
