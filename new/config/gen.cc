#include <fstream>
#include <vector>
using namespace std;

#include "../config.hh"
#include "../utils.hh"

struct Node {
    Config::ID  id;
    int         parent;
    string      name;
    vector<int> children;
};

void update_children_and_parent(vector<vector<Node>>& levels, int curr) {
    // Update children and parent
    auto& upper_level = levels[curr - 1];
    auto& level = levels[curr];
    int   i = level.size() - 1;
    while (i >= 0 && level[i].parent == -1) {
        auto& parent = upper_level.back();
        assert(parent.id.index == upper_level.size() - 1);
        level[i].parent = parent.id.index;
        parent.children.push_back(level[i].id.index);
        i--;
    }
}

void read_levels(string filename, vector<vector<Node>>& levels) {
    ifstream fin(filename);

    uint8_t curr_level = 0, last_level = 0;
    int     lin = 1;
    string  line;
    while (getline(fin, line)) {
        if (all_spaces(line)) {
            error("../config.options:{}:1: Empty lines are not allowed", lin);
        }
        auto [name, level] = split_config_line(filename, line, lin);

        // Manage changes in level
        auto level_change = level - last_level;

        // Error if jump +2 or more
        if (level_change > 1) {
            error(".minicc:{}:1: Config level should change by 1!.", lin);
        }

        // If the level change is positive, we already pushed the node
        // We only need to update parent and children for the level changes
        // that are negative
        if (level_change < 0) {
            // Exiting one *or more* branches
            for (int curr = last_level; curr >= level; curr--) {
                update_children_and_parent(levels, curr);
            }
        }

        // Create new node
        if (level >= levels.size()) {
            levels.resize(level + 1, {});
        }
        auto index = levels[level].size();
        auto id = Config::ID(level, index);
        levels[level].push_back({id, -1, name, {}});

        lin++;
        last_level = level;
    }

    // Don't forget last level change
    for (int curr = last_level; curr > 0; curr--) {
        update_children_and_parent(levels, curr);
    }
}

int main() {
    vector<vector<Node>> levels;
    read_levels("../config.options", levels);

    
}
