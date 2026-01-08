/*
   Copyright 2025 e-soul.org
   All rights reserved.
   Redistribution and use in source and binary forms, with or without modification, are permitted
   provided that the following conditions are met:
   1. Redistributions of source code must retain the above copyright notice, this list of conditions
      and the following disclaimer.
   2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions
      and the following disclaimer in the documentation and/or other materials provided with the distribution.
   THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND ANY EXPRESS OR IMPLIED
   WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
   FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
   FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
   BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
   BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
   LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
#pragma once

#include <string>
#include <string_view>
#include <variant>

namespace sisyl {

enum class Primitive {
    Int64,
    Bool,
    Str,
    Void,
};

using Type = std::variant<Primitive, std::string>; // string holds class name

[[nodiscard]] inline constexpr bool isPrimitive(const Type &type) {
    return std::holds_alternative<Primitive>(type);
}

[[nodiscard]] inline constexpr bool isVoid(const Type &type) {
    if (const auto *prim = std::get_if<Primitive>(&type)) {
        return *prim == Primitive::Void;
    }
    return false;
}

[[nodiscard]] inline constexpr bool isInt(const Type &type) {
    if (const auto *prim = std::get_if<Primitive>(&type)) {
        return *prim == Primitive::Int64;
    }
    return false;
}

[[nodiscard]] inline constexpr bool isInt64(const Type &type) {
    return isInt(type);
}

[[nodiscard]] inline constexpr bool isBool(const Type &type) {
    if (const auto *prim = std::get_if<Primitive>(&type)) {
        return *prim == Primitive::Bool;
    }
    return false;
}

[[nodiscard]] inline constexpr bool isStr(const Type &type) {
    if (const auto *prim = std::get_if<Primitive>(&type)) {
        return *prim == Primitive::Str;
    }
    return false;
}

[[nodiscard]] inline bool isSameType(const Type &lhs, const Type &rhs) {
    if (lhs.index() != rhs.index()) {
        return false;
    }
    if (const auto *lp = std::get_if<Primitive>(&lhs)) {
        return *lp == std::get<Primitive>(rhs);
    }
    return std::get<std::string>(lhs) == std::get<std::string>(rhs);
}

[[nodiscard]] inline std::string toString(const Type &type) {
    if (const auto *prim = std::get_if<Primitive>(&type)) {
        switch (*prim) {
        case Primitive::Int64:
            return "Int64";
        case Primitive::Bool:
            return "Bool";
        case Primitive::Str:
            return "Str";
        case Primitive::Void:
            return "Void";
        }
    }
    return std::get<std::string>(type);
}

[[nodiscard]] inline Type parseType(std::string_view text) {
    if (text == "Int64") {
        return Primitive::Int64;
    }
    if (text == "Bool") {
        return Primitive::Bool;
    }
    if (text == "Str") {
        return Primitive::Str;
    }
    if (text == "Void" || text == "void") {
        return Primitive::Void;
    }
    return std::string{text};
}

} // namespace sisyl
