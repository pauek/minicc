#ifndef I18N_HH
#define I18N_HH

#include <cassert>
#include <cstdarg>
#include <cstdio>
#include <cstring>
#include <iostream>
#include <map>
#include <string>

const int          MAX_NUMERAL = 9;
extern const char *numeral[MAX_NUMERAL + 1];

enum class Language {
    en = 0,
    es = 1,
    ca = 2,
};

class Translator {
    Language                    language;
    std::map<const char *, int> _index;

    void build_index();

    static const int   NUM_LANGS = 3;
    static const char *_translations[1000][Translator::NUM_LANGS];

   public:
    Translator() : language(Language::es) { build_index(); }

    void set_language(Language lang) { language = lang; }

    const char *translate(const char *message) const {
        auto it = _index.find(message);
        if (it == _index.end()) {
            return message;
        }
        auto i = static_cast<int>(language);
        return _translations[it->second][i];
    }

    static Translator translator;
};

inline std::string _T(const char *message) {
    const char *t = Translator::translator.translate(message);
    return (t != nullptr ? t : message);
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
