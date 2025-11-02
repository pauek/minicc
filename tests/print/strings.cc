#include <iostream>
using namespace std;

int main() {
    string line;
    getline(cin, line);
    int pos1, pos2 = 0;
    pos1 = line.find('"', pos2 + 1);
    pos2 = line.find('"', pos1 + 1);
    while (pos1 != string::npos && pos2 != string::npos) {
        cout << line.substr(pos1 + 1, pos2 - pos1 - 1) << endl;
        pos1 = line.find('"', pos2 + 1);
        pos2 = line.find('"', pos1 + 1);
    }
    cout << endl;
}
[[out]]--------------------------------------------------
#include <iostream>
using namespace std;

int main() {
    string line;
    getline(cin, line);
    int pos1, pos2 = 0;
    pos1 = line.find('"', pos2 + 1);
    pos2 = line.find('"', pos1 + 1);
    while (pos1 != string::npos && pos2 != string::npos) {
        cout << line.substr(pos1 + 1, pos2 - pos1 - 1) << endl;
        pos1 = line.find('"', pos2 + 1);
        pos2 = line.find('"', pos1 + 1);
    }
    cout << endl;
}
