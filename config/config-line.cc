#include <string>
using namespace std;

#include "config-line.hh"
#include "../utils.hh"

namespace Config {

ConfigLine split_config_line(string line, int lin) {
    size_t col = 1;

    // Parse indentation
    while (col <= line.size() && isspace(line[col - 1])) {
        col++;
    }
    int indent = col - 1;
    if (indent % 4 != 0) {
        return ConfigLine{
            .error = Error("Indentation levels have to be multiples of 4 spaces!", lin, col)
        };
    }

    // Parse name
    ConfigLine result = {
        .name = "",
        .level = uint8_t(indent / 4),
    };
    while (col <= line.size() && line[col - 1] != ':') {
        char ch = line[col - 1];
        if (isspace(ch)) {
            return ConfigLine{
                .error = Error("Spaces are not allowed config names.", lin, col),
            };
        }
        if (not allowed_char(ch)) {
            string message = std::format("Character '{}' not allowed in config names.", ch);
            return ConfigLine{
                .error = Error(message, lin, col),
            };
        }
        result.name += ch;
        col++;
    }

    // If we are at ':' we expect a value afterwards
    if (col <= line.size() && line[col - 1] == ':') {
        col++;
        // Skip spaces
        while (col <= line.size() && isspace(line[col - 1])) {
            col++;
        }

        if (col >= line.size()) {
            // if we are at the end of the line, there is no value
            result.value = None;
        } else if (line.substr(col - 1, 3) == "yes" || line.substr(col - 1, 2) == "no") {
            // There is 'yes' or 'no' (possibly followed by other things)
            bool is_yes = line[col - 1] == 'y';

            result.value = is_yes ? Yes : No;
            col += (is_yes ? 3 : 2);

            // Ensure everything else is space
            while (col <= line.size()) {
                if (not isspace(line[col - 1])) {
                    return ConfigLine{
                        .error = Error("Spurious characters after value", lin, col),
                    };
                }
            }
        } else {
            return ConfigLine{
                .error = Error("Value has to be 'yes' or 'no'", lin, col),
            };
        }
    }

    return result;
}

}
