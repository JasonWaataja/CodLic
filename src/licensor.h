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

#ifndef CODLIC_LICENSOR_H
#define CODLIC_LICENSOR_H

#include <sys/stat.h>

#include <memory>
#include <stdexcept>
#include <string>
#include <vector>

#include "options.h"

namespace codlic {

/*
 * Modeled somewhat after the UNIX nftw function, but is designed to be used
 * with a functor object. T is a functor object that when called with a const
 * char* representing the path, and a stat* object for the path.
 */
template<typename T>
void nftw(const std::string& path, T func);

class Licensor {
public:
    Licensor(std::shared_ptr<Options> options);
    void license();


private:
    std::shared_ptr<Options> options;

    /* Gets the list of files to operate on, following directories. */
    std::vector<std::string> get_files(const std::string& file);
    std::vector<std::string> get_files_in_dir(const std::string& dirpath);
};
} /* namespace codlic */

template<typename T>
void
codlic::nftw(const std::string& path, T func)
{
    struct stat info;
    if (stat(path.c_str(), &info) != 0)
        throw std::runtime_error{"Failed to stat file or directory."};
    if (!S_ISDIR(info.st_mode))
        throw std::runtime_error{"Attempting to walk non-directory."};
}

#endif /* CODLIC_LICENSOR_H */
