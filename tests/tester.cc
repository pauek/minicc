#include <filesystem>
#include <iostream>
#include <string>

using namespace std;
namespace fs = filesystem;

int main(int argc, char *argv[]) {
	for (int i = 1; i < argc; i++) {
		string type = argv[i];
		string path = string("./tests/") + type;
		for (const auto& entry : fs::directory_iterator(path)) {
			cout << entry.path() << endl;
			auto command = "./minicc test " + type + " " + path;
			system(command.c_str());
		}
	}
}