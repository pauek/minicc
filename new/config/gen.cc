#include <fstream>
#include <vector>
using namespace std;

#include "../config.hh"
#include "../utils.hh"

struct Node {
    Config::ID  id;
    int         parent = -1;
    string      name;
    pair<int, int> children = {-1, -1};
};

void update_children_and_parent(vector<vector<Node>>& levels, int curr) {
    // Update children and parent
    auto& upper_level = levels[curr - 1];
    auto& level = levels[curr];
    int   i = level.size() - 1;
    auto& parent = upper_level.back();
    while (i >= 0 && level[i].parent == -1) {
        assert(parent.id.index == upper_level.size() - 1);
        level[i].parent = parent.id.index;
        int child = level[i].id.index;
        // begin, end
        if (parent.children.first == -1) {
        	parent.children.first = child;
        } else {
        	parent.children.first = min(parent.children.first, child);
        }
        parent.children.second = max(parent.children.second, child + 1);
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
        levels[level].push_back({id, -1, name, {-1, -1}});

        lin++;
        last_level = level;
    }

    // Don't forget last level change
    for (int curr = last_level; curr > 0; curr--) {
        update_children_and_parent(levels, curr);
    }
}

void generate_level_vector(string filename, const vector<vector<Node>>& levels) {
    ofstream fout(filename);

    fout << "std::vector<Level> levels = {" << endl;
    for (const auto& level : levels) {
        fout << "    {";

        // Names
        fout << "{";
        for (const auto& node : level) {
            fout << '"' << node.name << '"' << ',';
        }
        fout << "},";

        // Children
        fout << "{";
        for (const auto& node : level) {
        	auto [begin, end] = node.children;
         	begin = max(begin, 0);
          	end = max(end, 0);
            fout << "{" << begin << ", " << end << "},";
        }
        fout << "},";

        // Parent
        fout << "{";
        for (const auto& node : level) {
            fout << node.parent << ",";
        }
        fout << "},";

        // State
        fout << "{";
        for (const auto& node : level) {
            fout << "No,";
        }
        fout << "},";

        fout << "}, " << endl;
    }
    fout << "};" << endl;
}

struct StructGen {
    ostream&                    out;
    const vector<vector<Node>>& levels;

    StructGen(ostream& out_, const vector<vector<Node>>& levels_) : out(out_), levels(levels_) {}

    const Node& node(Config::ID id) { return levels[id.level][id.index]; }

    void generate(Config::ID id) {
        const auto& n = node(id);
        if (id.level > 0) {
            out << string((id.level - 1) * 4, ' ') << "struct " << n.name << " : Option<"
                << id.level << ", " << id.index << "> {";
        }
        if (n.children.first == n.children.second) {
            out << "};" << endl;
        } else {
            out << endl;
            auto [begin, end] = n.children;
            for (auto i = begin; i < end; i++) {
                generate(Config::ID(id.level + 1, i));
            }
            if (id.level > 0) {
                out << string((id.level - 1) * 4, ' ') << "};" << endl;
            }
        }
    }
};

void generate_structs(string filename, const vector<vector<Node>>& levels) {
    ofstream fout(filename);
    StructGen(fout, levels).generate(Config::ID(0, 0));
}

int main() {
    vector<vector<Node>> levels;
    read_levels("../config.options", levels);

    generate_level_vector("../config.levels.cc.inc", levels);
    cout << "Generated \"../config.levels.cc.inc\"" << endl;

    generate_structs("../config.types.cc.inc", levels);
    cout << "Generate \"../config.types.cc.inc\"" << endl;
}
