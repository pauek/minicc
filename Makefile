
all: minicc

minicc:
	@make -j8 -C src
	@cp src/minicc .

release:
	@make -j8 -C src release
	@cp src/minicc .

clean:
	@make -C src clean

.PHONY: clean minicc