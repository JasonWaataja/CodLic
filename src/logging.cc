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

#include "logging.h"

#include <err.h>
#include <stdarg.h>

void
codlic::warn(const char *message, ...)
{
    va_list arglist;
    va_start(arglist, message);
    vwarnx(message, arglist);
    va_end(arglist);
};

void
codlic::warn(const std::string& message)
{
    warnx("%s", message.c_str());
}
