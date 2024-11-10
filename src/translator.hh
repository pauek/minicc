#ifndef TRANSLATOR_HH
#define TRANSLATOR_HH
#include <cassert>
#include <cstdarg>
#include <cstdio>
#include <iostream>
#include <map>
#include <string>
const int          MAX_NUMERAL = 9;
extern const char *numeral[MAX_NUMERAL + 1];

enum Language { en = 0, es = 1, ca = 2 };

class Translator {
    Language                   language;
    std::map<std::string, int> _index;

    void build_index() {
        int i = 0;
        while (std::string(_translations[i][0]) != "END") {
            _index[_translations[i][0]] = i;
            i++;
        }
    }

    static const int   NUM_LANGS = 3;
    static const char *_translations[1000][Translator::NUM_LANGS];

   public:
    Translator() : language(es) { build_index(); }

    void set_language(Language lang) {
        assert(lang >= 0 and lang < NUM_LANGS);
        language = lang;
    }

    std::string translate(std::string message) const {
        auto it = _index.find(message);
        return (it == _index.end() ? message : _translations[it->second][language]);
    }

    static Translator translator;
};

inline std::string _T(std::string message) {
    std::string t = Translator::translator.translate(message);
    return (t != "" ? t : message);
}

template <typename T1>
inline std::string _T(const char *format, const T1& t1) {
    static char buffer[200];  // FIXME: buffer overflow
    std::string f = Translator::translator.translate(format);
    std::sprintf(buffer, f.c_str(), t1);
    return std::string(buffer);
}

template <typename T1, typename T2>
inline std::string _T(const char *format, const T1& t1, const T2& t2) {
    static char buffer[200];  // FIXME: buffer overflow
    std::string f = Translator::translator.translate(format);
    std::sprintf(buffer, f.c_str(), t1, t2);
    return std::string(buffer);
}

template <typename T1, typename T2, typename T3>
inline std::string _T(const char *format, const T1& t1, const T2& t2, const T3& t3) {
    static char buffer[200];  // FIXME: buffer overflow
    std::string f = Translator::translator.translate(format);
    std::sprintf(buffer, f.c_str(), t1, t2, t3);
    return std::string(buffer);
}

template <typename T1, typename T2, typename T3, typename T4>
inline std::string _T(const char *format, const T1& t1, const T2& t2, const T3& t3, const T4& t4) {
    static char buffer[200];  // FIXME: buffer overflow
    std::string f = Translator::translator.translate(format);
    std::sprintf(buffer, f.c_str(), t1, t2, t3, t4);
    return std::string(buffer);
}

template <typename T1, typename T2, typename T3, typename T4, typename T5>
inline std::string
_T(const char *format, const T1& t1, const T2& t2, const T3& t3, const T4& t4, const T5& t5) {
    static char buffer[200];  // FIXME: buffer overflow
    std::string f = Translator::translator.translate(format);
    std::sprintf(buffer, f.c_str(), t1, t2, t3, t4, t5);
    return std::string(buffer);
}
#endif
