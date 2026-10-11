case 'a': {
    if (ident == "alignas") {
        tok = KEYWORD;
    } else if (ident == "alignof") {
        tok = KEYWORD;
    } else if (ident == "and") {
        tok = KEYWORD;
    } else if (ident == "and_eq") {
        tok = KEYWORD;
    } else if (ident == "asm") {
        tok = KEYWORD;
    } else if (ident == "atomic_cancel") {
        tok = KEYWORD;
    } else if (ident == "atomic_commit") {
        tok = KEYWORD;
    } else if (ident == "atomic_noexcept") {
        tok = KEYWORD;
    } else if (ident == "auto") {
        tok = KEYWORD;
    }
    break;
}
case 'b': {
    if (ident == "bitand") {
        tok = KEYWORD;
    } else if (ident == "bitor") {
        tok = KEYWORD;
    } else if (ident == "bool") {
        tok = KEYWORD;
    } else if (ident == "break") {
        tok = KEYWORD;
    }
    break;
}
case 'c': {
    if (ident == "case") {
        tok = KEYWORD;
    } else if (ident == "catch") {
        tok = KEYWORD;
    } else if (ident == "char") {
        tok = KEYWORD;
    } else if (ident == "char16_t") {
        tok = KEYWORD;
    } else if (ident == "char32_t") {
        tok = KEYWORD;
    } else if (ident == "char8_t") {
        tok = KEYWORD;
    } else if (ident == "class") {
        tok = KEYWORD;
    } else if (ident == "co_await") {
        tok = KEYWORD;
    } else if (ident == "co_return") {
        tok = KEYWORD;
    } else if (ident == "co_yield") {
        tok = KEYWORD;
    } else if (ident == "compl") {
        tok = KEYWORD;
    } else if (ident == "concept") {
        tok = KEYWORD;
    } else if (ident == "const") {
        tok = KEYWORD;
    } else if (ident == "const_cast") {
        tok = KEYWORD;
    } else if (ident == "consteval") {
        tok = KEYWORD;
    } else if (ident == "constexpr") {
        tok = KEYWORD;
    } else if (ident == "constinit") {
        tok = KEYWORD;
    } else if (ident == "continue") {
        tok = KEYWORD;
    } else if (ident == "contract_assert") {
        tok = KEYWORD;
    }
    break;
}
case 'd': {
    if (ident == "decltype") {
        tok = KEYWORD;
    } else if (ident == "default") {
        tok = KEYWORD;
    } else if (ident == "delete") {
        tok = KEYWORD;
    } else if (ident == "do") {
        tok = KEYWORD;
    } else if (ident == "double") {
        tok = KEYWORD;
    } else if (ident == "dynamic_cast") {
        tok = KEYWORD;
    }
    break;
}
case 'e': {
    if (ident == "else") {
        tok = KEYWORD;
    } else if (ident == "enum") {
        tok = KEYWORD;
    } else if (ident == "explicit") {
        tok = KEYWORD;
    } else if (ident == "export") {
        tok = KEYWORD;
    } else if (ident == "extern") {
        tok = KEYWORD;
    }
    break;
}
case 'f': {
    if (ident == "false") {
        tok = KEYWORD;
    } else if (ident == "float") {
        tok = KEYWORD;
    } else if (ident == "for") {
        tok = KEYWORD;
    } else if (ident == "friend") {
        tok = KEYWORD;
    }
    break;
}
case 'g': {
    if (ident == "goto") {
        tok = KEYWORD;
    }
    break;
}
case 'i': {
    if (ident == "if") {
        tok = KEYWORD;
    } else if (ident == "inline") {
        tok = KEYWORD;
    } else if (ident == "int") {
        tok = KEYWORD;
    }
    break;
}
case 'l': {
    if (ident == "long") {
        tok = KEYWORD;
    }
    break;
}
case 'm': {
    if (ident == "mutable") {
        tok = KEYWORD;
    }
    break;
}
case 'n': {
    if (ident == "namespace") {
        tok = KEYWORD;
    } else if (ident == "new") {
        tok = KEYWORD;
    } else if (ident == "noexcept") {
        tok = KEYWORD;
    } else if (ident == "not") {
        tok = KEYWORD;
    } else if (ident == "not_eq") {
        tok = KEYWORD;
    } else if (ident == "nullptr") {
        tok = KEYWORD;
    }
    break;
}
case 'o': {
    if (ident == "operator") {
        tok = KEYWORD;
    } else if (ident == "or") {
        tok = KEYWORD;
    } else if (ident == "or_eq") {
        tok = KEYWORD;
    }
    break;
}
case 'p': {
    if (ident == "private") {
        tok = KEYWORD;
    } else if (ident == "protected") {
        tok = KEYWORD;
    } else if (ident == "public") {
        tok = KEYWORD;
    }
    break;
}
case 'r': {
    if (ident == "reflexpr") {
        tok = KEYWORD;
    } else if (ident == "register") {
        tok = KEYWORD;
    } else if (ident == "reinterpret_cast") {
        tok = KEYWORD;
    } else if (ident == "requires") {
        tok = KEYWORD;
    } else if (ident == "return") {
        tok = KEYWORD;
    }
    break;
}
case 's': {
    if (ident == "short") {
        tok = KEYWORD;
    } else if (ident == "signed") {
        tok = KEYWORD;
    } else if (ident == "sizeof") {
        tok = KEYWORD;
    } else if (ident == "static") {
        tok = KEYWORD;
    } else if (ident == "static_assert") {
        tok = KEYWORD;
    } else if (ident == "static_cast") {
        tok = KEYWORD;
    } else if (ident == "struct") {
        tok = KEYWORD;
    } else if (ident == "switch") {
        tok = KEYWORD;
    } else if (ident == "synchronized") {
        tok = KEYWORD;
    }
    break;
}
case 't': {
    if (ident == "template") {
        tok = KEYWORD;
    } else if (ident == "this") {
        tok = KEYWORD;
    } else if (ident == "thread_local") {
        tok = KEYWORD;
    } else if (ident == "throw") {
        tok = KEYWORD;
    } else if (ident == "true") {
        tok = KEYWORD;
    } else if (ident == "try") {
        tok = KEYWORD;
    } else if (ident == "typedef") {
        tok = KEYWORD;
    } else if (ident == "typeid") {
        tok = KEYWORD;
    } else if (ident == "typename") {
        tok = KEYWORD;
    }
    break;
}
case 'u': {
    if (ident == "union") {
        tok = KEYWORD;
    } else if (ident == "unsigned") {
        tok = KEYWORD;
    } else if (ident == "using") {
        tok = KEYWORD;
    }
    break;
}
case 'v': {
    if (ident == "virtual") {
        tok = KEYWORD;
    } else if (ident == "void") {
        tok = KEYWORD;
    } else if (ident == "volatile") {
        tok = KEYWORD;
    }
    break;
}
case 'w': {
    if (ident == "wchar_t") {
        tok = KEYWORD;
    } else if (ident == "while") {
        tok = KEYWORD;
    }
    break;
}
case 'x': {
    if (ident == "xor") {
        tok = KEYWORD;
    } else if (ident == "xor_eq") {
        tok = KEYWORD;
    }
    break;
}
