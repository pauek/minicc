#include <iostream>
#include <string>
#include <filesystem>
#include <fstream>
using namespace std;

string read_file(string filename) {
	auto size = filesystem::file_size(filename);
	string content(size, '\0');
	ifstream file(filename);
	if (file.bad()) {
		cerr << "Bad!" << endl;
	}
	file.read(&content[0], size);
	return content;
}

string read_stdin() {
	string input, line;
	while (getline(cin, line)) {
		// FIXME: This is VERY inefficient!!
		input += line + '\n';
	}
	return input;
}
