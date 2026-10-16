int main() {
	if (a) {
		a = 1;
		b = true || false && false;
	}

	if (b || d) {
		a = 2;
	} else {
		a = 3;
	}

	if (a) {
		a = 1;
	} else if (b) {
		b = 1;
	} else if (c) {
		c = 1;
	} else {
		d = 1;
	}
}
