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

#include <memory>
#include <string>
#include <vector>

#include "options.h"

namespace codlic {

class Licensor {
public:
    Licensor(std::shared_ptr<Options> options);
    void license();

private:
    std::shared_ptr<Options> options;

    /* Gets the list of files to operate on, following directories. */
    std::vector<std::string> get_files(const std::string& file);
    std::vector<std::string> get_files_in_dir(const std::string& dirpath);
    int nftw_func = [&files](const char *fpath, const struct stat*,
        int typeflag, struct FTW*) {
        if (typeflag == FTW_F)
            files.push_back(std::string{fpath});
    };
};
} /* namespace codlic */

#endif /* CODLIC_LICENSOR_H */
