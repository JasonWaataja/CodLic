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

#include <err.h>

#include <regex>

#include "filetypes.h"
#include "logging.h"

codlic::Licensor::Licensor(std::shared_ptr<Options> options) : options{options}
{
}

void
codlic::Licensor::license()
{
    std::vector<std::string> files;
    for (const auto& arg : options->args) {
        std::vector<std::string> arg_files = get_files(arg);
        files.insert(files.end(), arg_files.begin(), arg_files.end());
    }
}

void
codlic::Licensor::perform_on(const std::string& file)
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
    if (S_ISDIR(info.st_mode)) {
        try {
            FileFinder finder{files, options};
            nftw(file, finder);
        } catch (const NftwError& err) {
            warn(err.what());
        };
    }
    return files;
}

codlic::FileFinder::FileFinder(std::vector<std::string>& files,
    std::shared_ptr<Options> options) : files{&files}, options{options}
{
}

bool
codlic::FileFinder::operator()(const char* path, const struct stat* info)
{
    if (!S_ISREG(info->st_mode))
        return true;
    if (options->has_filetype_regex) {
        std::regex re{options->filetype_regex};
        if (std::regex_match(path, re)) {
            files->push_back(path);
            return true;
        }
    }
    if (options->has_filetype) {
        auto filetypes = filetype_map();
        auto filetype_pos = filetypes.find(options->filetype);
        if (filetype_pos != filetypes.end()) {
            files->push_back(path);
            return true;
        }
    }
    return true;
}

codlic::NftwError::NftwError() : std::runtime_error{strerror(errno)}
{
}

codlic::NftwError::NftwError(const char* what_arg)
    : std::runtime_error{what_arg}
{
}

codlic::ScandirInfo::ScandirInfo(const char* path)
{
    entry_count = scandir(path, &entry_data, nullptr, &alphasort);
    if (entry_count == -1)
        throw std::runtime_error{strerror(errno)};
}

codlic::ScandirInfo::~ScandirInfo() {
    free(entry_data);
}

int
codlic::ScandirInfo::size() const
{
    return entry_count;
}

struct dirent**
codlic::ScandirInfo::entries() const
{
    return entry_data;
}

void
codlic::nftw(const std::string& path, std::function<bool(const char*,
        const struct stat*)> func)
{
    /* Use this throughout the function. */
    struct stat info;
    if (stat(path.c_str(), &info) != 0)
        throw NftwError{};
    if (!S_ISDIR(info.st_mode))
        throw NftwError{"Attempting to walk non-directory."};
    try {
        ScandirInfo entries{path.c_str()};
        for (int i = 0; i < entries.size(); ++i) {
            std::string entry_path{path + "/" + entries.entries()[i]->d_name};
            if (stat(entry_path.c_str(), &info) != 0)
                throw NftwError{};
            bool should_continue = func(path.c_str(), &info);
            if (!should_continue)
                throw NftwError{"File tree walk stopped by function."};
            if (S_ISDIR(info.st_mode))
                nftw(entry_path, func);
        }
    } catch (const NftwError& err) {
        throw;
    } catch (const std::runtime_error& err) {
        throw NftwError(err.what());
    }
}
