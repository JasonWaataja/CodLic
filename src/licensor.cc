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

#include "licensor.h"

#include <sys/stat.h>

codlic::Licensor::Licensor(std::shared_ptr<Options> options) : options{options}
{
}

void
codlic::Licensor::license()
{
}

std::vector<std::string>
codlic::Licensor::get_files(const std::string& file)
{
    std::vector<std::string> files;
    struct stat info;
    if (stat(file.c_str(), &info) != 0)
        return files;
    if (S_ISREG(info.st_mode))
        files.push_back(file);
    else {
        std::vector<std::string> dir_files = get_files_in_dir(file);
        files.insert(files.end(), dir_files.begin(), dir_files.end());
    }
    return files;
}

std::vector<std::string>
codlic::Licensor::get_files_in_dir(const std::string& dirpath)
{
    std::vector<std::string> files;
    return files;
}
