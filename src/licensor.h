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

#include "config.h"

#include <sys/stat.h>

#include <dirent.h>
#include <stdlib.h>
#include <string.h>

#include <functional>
#include <memory>
#include <stdexcept>
#include <string>
#include <vector>

#include "comment_type.h"
#include "options.h"

namespace codlic {

/*
 * Modeled somewhat after the UNIX nftw function, but is designed to be used
 * with a functor object. T is a function object that when called with a const
 * char* representing the path, and a stat* object for the path. The function
 * object should return a boolean value. True indicates to continue processing
 * entries and false indicates stopping.
 *
 * This function may throw a NftwError for a variety of reasons.
 */
void nftw(const std::string& path, std::function<bool(const char*,
        const struct stat*)> func);

/*
 * Represents the object that does the licensing. There should be one of these
 * per program.
 */
class Licensor {
public:
    explicit Licensor(std::shared_ptr<Options> options);
    void license();

    CommentType get_comment_type_for_file(const std::string& path);

private:
    std::shared_ptr<Options> options;

    /* Gets the list of files to operate on, following directories. */
    std::vector<std::string> get_files(const std::string& file);
    /* Applies the correct operation to a file. */
    void perform_on(const std::string& file);
};

/*
 * A function object that finds files based on a set of options. This stores a
 * pointer to the vector being operated on, so that object's lifetime must not
 * end before this one.
 */
class FileFinder {
public:
    FileFinder(std::vector<std::string>& files,
        std::shared_ptr<Options> options);
    bool operator()(const char* path, const struct stat* info);

private:
    std::vector<std::string>* files;
    std::shared_ptr<Options> options;
};

/*
 * Wrapper for use with scandir. Its purpose is to use RAII to makes sure the
 * directory entries are always freed.
 */
class ScandirInfo {
public:
    /* Scans path. Throws std::runtime_error upon failure. */
    explicit ScandirInfo(const char* path);
    ~ScandirInfo();
    int size() const;
    struct dirent** entries() const;

private:
    int entry_count = 0;
    struct dirent** entry_data;
};


/* An error returned by the nftw function. */
class NftwError : public std::runtime_error {
public:
    /* Initializes the message with strerror(errno). */
    NftwError();
    explicit NftwError(const char* what_arg);
};
} /* namespace codlic */

#endif /* CODLIC_LICENSOR_H */
