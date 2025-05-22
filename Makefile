BUILD_TARGETS := minicc release debug
OTHER_TARGETS := format clean

all: minicc

$(BUILD_TARGETS):
	@make -j8 -C src $@
	@cp src/minicc .

$(OTHER_TARGETS):
	@make -C src $@

test: debug
	./test.sh

.PHONY: $(BUILD_TARGETS) $(OTHER_TARGETS)